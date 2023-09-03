use crate::chunk::LoxVal;
use crate::vm::{VMError, VM};

pub fn run_program(src: &str) -> Result<LoxVal, VMError> {
    /*
    let chunk = Parser::new(src)
        .unwrap()
        .parse()
        .unwrap();

    VM::from(chunk).interpret()
    */
    Err(VMError::Bug("not implemented yet".to_string()))
}
