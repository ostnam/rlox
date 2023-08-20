/// The compiler and VM use the `Arena` defined here to store values.
use std::marker::PhantomData;

/// The arena itself.
pub struct Arena<T> {
    /// Where the value are stored.
    heap: Vec<T>,
}

/// Storing a `T` into an arena returns a `Ref<T>`.
/// This `Ref<T>` can later be used to retrieve the `T`, set a new value, etc.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Ref<T> {
    /// Position of the item in the heap of the `Arena`.
    idx: usize,

    /// Needed for type-checking.
    phantom: PhantomData<T>,
}

impl<T> Arena<T> {
    /// Create a new, empty arena.
    pub fn new() -> Self {
        Arena { heap: Vec::new() }
    }

    /// Insert a new value in the arena.
    pub fn insert(&mut self, val: T) -> Ref<T> {
        self.heap.push(val);
        Ref {
            idx: self.heap.len() - 1,
            phantom: PhantomData,
        }
    }

    /// Update the value pointed to by the Ref in the arena.
    pub fn update(&mut self, key: &Ref<T>, val: T) {
        self.heap[key.idx] = val;
    }

    /// Get a reference to the value pointed to by the Ref in the arena.
    pub fn get(&self, key: &Ref<T>) -> &T {
        &self.heap[key.idx]
    }

    /// Get a mutable reference to the value pointed to by the Ref in the arena.
    pub fn get_mut(&mut self, key: &Ref<T>) -> &mut T {
        &mut self.heap[key.idx]
    }
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self::new()
    }
}
