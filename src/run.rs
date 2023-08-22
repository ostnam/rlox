use crate::chunk::LoxVal;
use crate::parser::Parser;
use crate::vm::{VMError, VM};

pub fn run_program(src: &str) -> Result<LoxVal, VMError> {
    let chunk = Parser::new(src)
        .unwrap()
        .compile()
        .unwrap();

    VM::from(chunk).interpret()
}
