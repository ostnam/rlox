use rlox::{chunk::LoxVal, vm::{VMError, VM}, compiler::Compiler};

pub fn run_program(src: &str) -> Result<LoxVal, VMError> {
    let chunk = Compiler::new(src)
        .unwrap()
        .compile()
        .unwrap();

    VM::from(&chunk).interpret()
}
