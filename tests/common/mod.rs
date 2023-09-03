use rlox::chunk::OwnedLoxVal;
use rlox::compiler::Compiler;
use rlox::parser::Parser;
use rlox::vm::{VMError, VM};

pub fn run_program(src: &str) -> Result<OwnedLoxVal, VMError> {
    let parsed = Parser::new(src).unwrap().parse().unwrap();
    let compiled = Compiler::compile(parsed).unwrap();
    VM::from(compiled).interpret()
}
