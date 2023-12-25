/// The compiler and VM use the `Arena` defined here to store values.
/// The `Arena` type implements methods used in a moving GC approach.
use std::marker::PhantomData;

/// The arena itself.
#[derive(Debug)]
pub struct Arena<T, GCflag: GCFlag> {
    heap: Vec<Value<T>>,

    /// Controls whether the values in the arena can be moved to another
    /// for garbage collection, and whether it can be locked.
    flag: GCflag,
}

pub trait GCFlag: std::fmt::Debug + Default {}

/// Must be implemented by a GCFlag to allow inserting values into the `Arena`
/// holding it.
pub trait InsertableArena {}

/// `GCFlag` for arena holding values constructed during program execution or
/// compilation.
#[derive(Clone, Debug, Default)]
pub struct Dynamic {
    /// Some types, such as strings, can be created both during compilation and
    /// execution. Having a single `Arena` for both cases prevents errors where a
    /// `Ref` is used with the wrong `Arena`.
    ///
    /// During garbage collection, values created statically will not get collected,
    /// as it would require iterating over every instruction in the body of every
    /// function that could currently be called. Instead, they will be directly
    /// moved to the new `Arena`.
    /// This could be problematic if we couldn't tell whether each arena-allocated
    /// value comes from a static or dynamic context. Luckily, since compilation
    /// precedes execution, values allocated during compilation will always
    /// be stored before those created during program execution.
    ///
    /// As a result, during garbage collection, if the number of compile-time
    /// allocated values is known, these values can simply be moved from one `Arena`
    /// to the other in the same position, and every existing `Ref` to it will
    /// remain valid.
    num_statics: usize,
}

/// Flag for `Arena`s that can store run-time values, as well as compilation-time.
/// Supports garbage collection and value insertion/mutation.
impl GCFlag for Dynamic {}
impl InsertableArena for Dynamic {}

/// Flag for `Arena`s that can only store compilation-time values.
/// Supports insertion/mutation, but not garbage-collection.
#[derive(Debug, Default)]
pub struct StaticOpen {}
impl GCFlag for StaticOpen {}
impl InsertableArena for StaticOpen {}

/// Flag for `Arena`s that can only store compilation-time values.
/// Does not support mutation or insertion.
#[derive(Debug, Default)]
pub struct StaticLocked {}
impl GCFlag for StaticLocked {}

/// Holds values inside of an `Arena`.
#[derive(Clone, Debug)]
enum Value<T> {
    Live(T),

    /// During GC, after a value is moved from one `Arena` to another,
    /// a `Ref` to the value in the new `Arena` is stored in its place,
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
impl<T> Copy for Ref<T> {}

// we need to implement it manually, otherwise Ref<T> won't be Copy if T isn't.
impl<T> Clone for Ref<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl <T, U: GCFlag + InsertableArena> Arena<T, U> {
    /// Insert a new value in the arena.
    pub fn insert(&mut self, val: T) -> Ref<T> {
        self.heap.push(Value::Live(val));
        Ref {
            idx: self.heap.len() - 1,
            phantom: PhantomData,
        }
    }
}

impl<T, U: GCFlag> Arena<T, U> {
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

    /// Returns the number of objects currently stored in the arena.
    pub fn len(&self) -> usize {
        self.heap.len()
    }

    /// Returns a `Ref` that will point to the next inserted element.
    /// Is not valid and should not be used before that element is inserted,
    /// and a panic could occur otherwise.
    fn next_ref(&self) -> Ref<T> {
        Ref {
            idx: self.heap.len(),
            phantom: PhantomData,
        }
    }
}

impl<T: Clone> Arena<T, Dynamic> {
    /// Creates a new heap for GC.
    pub fn next_heap(&mut self) -> Self {
        let heap = self.heap.drain(..self.flag.num_statics).collect();
        Arena {
            heap,
            flag: self.flag.clone(),
        }
    }

    /// Moves a `Ref` from `self` to `to`, and returns the new `Ref` to it.
    /// If the `Ref` points to a value that was already moved, simply returns
    /// the new valid `Ref`.
    ///
    /// Also returns a bool: whether that `Ref` was just moved, or moved by a
    /// previous call.
    pub fn move_ref(&mut self, to: &mut Arena<T, Dynamic>, ptr: Ref<T>) -> (Ref<T>, bool) {
        if ptr.idx < self.flag.num_statics {
            return (ptr, true);
        }
        match self.heap.get(ptr.idx - self.flag.num_statics) {
            Some(Value::Live(_)) => {
                let new_ptr = to.next_ref();
                let val = std::mem::replace(
                    &mut self.heap[ptr.idx - self.flag.num_statics],
                    Value::Moved(new_ptr)
                ).unwrap_value();
                to.insert(val);
                (new_ptr, false)
            },
            Some(Value::Moved(new_ptr)) => (*new_ptr, true),
            None => panic!("tried to move invalid ref"),
        }
    }
}

impl<T: Clone, U: GCFlag + InsertableArena> Arena<T, U> {
    /// Stores a new value, equal to the one in the passed `Ref` in the `Arena`
    /// and returns its `Ref`.
    pub fn clone_in_arena(&mut self, key: Ref<T>) -> Ref<T> {
        let og = self.get(key);
        self.insert(og.clone())
    }
}

impl<T> Arena<T, StaticOpen> {
    /// Turns `self` into a `Arena<T, StaticLocked>`.
    pub fn lock(self) -> Arena<T, StaticLocked> {
        Arena {
            heap: self.heap,
            flag: StaticLocked::default(),
        }
    }

    /// Turns `self` into a `Arena<T, Dynamic>`.
    pub fn as_dynamic(self) -> Arena<T, Dynamic> {
        Arena {
            flag: Dynamic {
                num_statics: self.heap.len(),
            },
            heap: self.heap,
        }
    }
}

impl<T, U: GCFlag> Default for Arena<T, U> {
    fn default() -> Self {
        Arena {
            heap: Vec::new(),
            flag: U::default(),
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
