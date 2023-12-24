/// The compiler and VM use the `Arena` defined here to store values.
use std::marker::PhantomData;

/// The arena itself.
#[derive(Debug)]
pub struct Arena<T> {
    heap: Vec<Value<T>>,
}

#[derive(Debug)]
enum Value<T> {
    Live(T),

    /// During garbage collection, after a value is moved from one `Arena` to
    /// another, a `Ref` to the value in the new `Arena` is stored in its place,
    /// so that other references to the initial value can be updated.
    Moved(Ref<T>),
}

impl<T> Value<T> {
    fn unwrap_value(self) -> T {
        match self {
            Value::Live(v) => v,
            _ => panic!("unwrapped moved value"),
        }
    }

    fn unwrap_value_ref(&self) -> &T {
        match self {
            Value::Live(v) => v,
            _ => panic!("unwrapped moved value"),
        }
    }

    fn unwrap_mut_value_ref(&mut self) -> &mut T {
        match self {
            Value::Live(v) => v,
            _ => panic!("unwrapped moved value"),
        }
    }
}

/// Storing a `T` into an arena returns a `Ref<T>`.
/// This `Ref<T>` can later be used to retrieve the `T`, set a new value, etc.
#[derive(Debug)]
pub struct Ref<T> {
    /// Position of the item in the heap of the `Arena`.
    idx: usize,

    /// Needed for type-checking.
    phantom: PhantomData<T>,
}

impl<T> PartialEq for Ref<T> {
    fn eq(&self, other: &Self) -> bool {
        self.idx == other.idx
    }
}

impl<T> Eq for Ref<T> {}

// we need to implement it manually, otherwise Ref<T> won't be Copy if T isn't.
impl<T> Clone for Ref<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Ref<T> {
}

impl<T> Arena<T> {
    /// Insert a new value in the arena.
    pub fn insert(&mut self, val: T) -> Ref<T> {
        self.heap.push(Value::Live(val));
        Ref {
            idx: self.heap.len() - 1,
            phantom: PhantomData,
        }
    }

    /// Update the value pointed to by the Ref in the arena.
    pub fn update(&mut self, key: Ref<T>, val: T) {
        self.heap[key.idx] = Value::Live(val);
    }

    /// Get a reference to the value pointed to by the Ref in the arena.
    /// Should not be called while the values are being moved to an other arena,
    /// as a panic could occur.
    pub fn get(&self, key: Ref<T>) -> &T {
        self.heap[key.idx].unwrap_value_ref()
    }

    /// Get a mutable reference to the value pointed to by the Ref in the arena.
    /// Should not be called while the values are being moved to an other arena,
    /// as a panic could occur.
    pub fn get_mut(&mut self, key: Ref<T>) -> &mut T {
        self.heap[key.idx].unwrap_mut_value_ref()
    }

    /// Returns a `Ref` that will point to the next inserted element.
    /// Is not valid and should not be used before that element is inserted,
    /// and a panic could occur otherwise.
    fn next_ref(&self) -> Ref<T> {
        Ref {
            idx: self.heap.len() - 1,
            phantom: PhantomData,
        }
    }
}

impl <T> Arena<T> {
    /// Moves a `Ref` from `self` to `to`.
    pub fn move_ref(&mut self, to: &mut Arena<T>, ptr: Ref<T>) -> Ref<T> {
        match self.heap.get(ptr.idx) {
            Some(Value::Live(_)) => {
                let new_ptr = to.next_ref();
                let val = std::mem::replace(&mut self.heap[ptr.idx], Value::Moved(new_ptr)).unwrap_value();
                to.insert(val);
                new_ptr
            },
            Some(Value::Moved(new_ptr)) => *new_ptr,
            None => panic!("tried to move invalid ref"),
        }
    }
}

impl <T: Clone> Arena<T> {
    /// Stores a new value, equal to the one in the passed `Ref` in the `Arena`
    /// and returns its `Ref`.
    pub fn clone_in_arena(&mut self, key: Ref<T>) -> Ref<T> {
        let og = self.get(key);
        self.insert(og.clone())
    }
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Arena {
            heap: Vec::new(),
        }
    }
}

#[macro_export]
/// Concisely check if the values pointed-to by two `Ref` are equal.
macro_rules! refs_eql {
    ($arena: expr, $lhs: expr, $rhs: expr) => {
        {
            let l = $arena.get($lhs);
            let r = $arena.get($rhs);
            l == r
        }
    }
}

#[macro_export]
/// Concisely check if the value pointed-to by a `Ref` is equal to the passe-in literal value.
macro_rules! ref_eql_lit {
    ($arena: expr, $lhs: ident, $lit: literal) => {
        {
            let l = $arena.get($lhs);
            l == lit
        }
    }
}
