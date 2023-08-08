use crate::chunk::LoxVal;
use crate::compiler::Compiler;
use crate::vm::{VMError, VM};

pub fn run_program(src: &str) -> Result<LoxVal, VMError> {
    let chunk = Compiler::new(src)
        .unwrap()
        .compile()
        .unwrap();

    VM::from(&chunk).interpret()
}
