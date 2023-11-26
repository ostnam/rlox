use crate::chunk::OwnedLoxVal;
use crate::compiler::Compiler;
use crate::parser::Parser;
use crate::vm::{VMError, VM};

pub fn run_program(src: &str) -> Result<OwnedLoxVal, VMError> {
    let ast = Parser::new(src)
        .unwrap()
        .parse()
        .unwrap();
    let compiled = Compiler::compile(ast).unwrap();

    VM::from(compiled).interpret()
}
