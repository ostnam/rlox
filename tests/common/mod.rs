use rlox::chunk::OwnedLoxVal;
use rlox::compiler::{Compiler, CompilationResult};
use rlox::parser::Parser;
use rlox::vm::{VMError, VM};

pub fn run_program(src: &str) -> Result<OwnedLoxVal, VMError> {
    let parsed = Parser::new(src).unwrap().parse().unwrap();
    let compiled = Compiler::compile(parsed).unwrap();
    VM::from(compiled).interpret()
}

pub fn compile(src: &str) -> Option<CompilationResult> {
    let parsed = Parser::new(src).unwrap().parse().unwrap();
    Compiler::compile(parsed)
}
