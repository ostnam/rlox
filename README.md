My implementation of the bytecode VM based interpreter presented in the second half of [Crafting Interpreters](https://craftinginterpreters.com/).
The language it runs, Lox, is a dynamically-typed, object-oriented and has a syntax similar to Javascript.

Some notes on my implementation are included below.

# Compiler
The compiler presented in the book works in a single pass, emitting instructions as it parses tokens.
Although it is slower and less memory-efficient, I chose to use the more common architecture where
parsing tokens into an AST and emitting bytecode from the AST are separate.
This will allow me to use this project to try implementing optimizations (for educational purposes,
since it doesn't make that much sense to optimize heavily an interpreted language).

# VM
The VM described in the book is written in C, and wouldn't pass the borrow checker if directly translated to Rust.
I chose to use an `Arena` type, which will store every object in an inner vector, similar to arena allocators.
Instead of holding a `&T` or `&mut T`, objects will hold a `Ref<T>` instead, which is just a struct wrapping the index of the
object in the `Arena`'s vector.

The VM implemented in the book uses a mark-and-sweep garbage collector. I implemented a moving GC instead, since
it is straight-forward to implement with arenas.
