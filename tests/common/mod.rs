use rlox::chunk::LoxVal;
use rlox::parser::Parser;
use rlox::vm::{VMError, VM};

pub fn run_expr(src: &str) -> Result<LoxVal, VMError> {
    let chunk = Parser::new(src)
        .unwrap()
        .compile_expr()
        .unwrap();

    VM::from(chunk).interpret()
}

pub fn run_program(src: &str) -> Result<LoxVal, VMError> {
    let chunk = Parser::new(src)
        .unwrap()
        .compile()
        .unwrap();

    VM::from(chunk).interpret()
}
