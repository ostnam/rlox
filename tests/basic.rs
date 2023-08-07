use rlox::{chunk::LoxVal, vm::VMError};

mod common;

#[test]
fn test_constants() {
    assert_eq!(
        common::run_program("10"),
        Ok(LoxVal::Num(10.0)),
    );
    assert_eq!(
        common::run_program("11.0"),
        Ok(LoxVal::Num(11.0)),
    );
    assert_eq!(
        common::run_program("true"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("false"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("nil"),
        Ok(LoxVal::Nil),
    );
}

#[test]
fn test_add_num() {
    assert_eq!(
        common::run_program("10 + 21"),
        Ok(LoxVal::Num(31.0)),
    );
    assert_eq!(
        common::run_program("10 + 21 + 100"),
        Ok(LoxVal::Num(131.0)),
    );
}

#[test]
fn test_add_str() {
    assert_eq!(
        common::run_program(r#""hello" + " " + "lox""#),
        Ok(LoxVal::Str("hello lox".to_string())),
    );
}

#[test]
fn test_add_type_error() {
    assert!(matches!(
        common::run_program(r#""hello " + 10"#),
        Err(VMError::TypeError {
            line: 1,
            ..
        }),
    ));
}

#[test]
fn test_sub_num() {
    assert_eq!(
        common::run_program("10 - 21"),
        Ok(LoxVal::Num(-11.0)),
    );
    assert_eq!(
        common::run_program("10 - 21 - 32"),
        Ok(LoxVal::Num(-43.0)),
    );
}

#[test]
fn test_mult_num() {
    assert_eq!(
        common::run_program("10 * 21"),
        Ok(LoxVal::Num(210.0)),
    )
}

#[test]
fn test_div_num() {
    assert_eq!(
        common::run_program("10 / 20"),
        Ok(LoxVal::Num(0.5)),
    );
    assert_eq!(
        common::run_program("8 / 4 / 2"),
        Ok(LoxVal::Num(1.0)),
    );
}

#[test]
fn test_unary_op() {
    assert_eq!(
        common::run_program("-38"),
        Ok(LoxVal::Num(-38.0)),
    );
    assert_eq!(
        common::run_program("----1000"),
        Ok(LoxVal::Num(1000.0)),
    );
}

#[test]
fn test_parens() {
    assert_eq!(
        common::run_program("(1 + 2) * 3"),
        Ok(LoxVal::Num(9.0)),
    );
    assert_eq!(
        common::run_program("(1 + 2) * (3 + 2)"),
        Ok(LoxVal::Num(15.0)),
    );
}

#[test]
fn test_mixed_arithmetic() {
    assert_eq!(
        common::run_program("(10 + 30 * --20 / 100 + 4 - -20) / (2 + 8)"),
        Ok(LoxVal::Num(4.0)),
    );
}

#[test]
fn test_not() {
    assert_eq!(
        common::run_program("!true"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("!false"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("!nil"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("!10"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("!0"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("!-1"),
        Ok(LoxVal::Bool(false)),
    );
}

// < <= >= >
#[test]
fn test_cmp() {
    assert_eq!(
        common::run_program("1 == 1"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("1 == 2"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("1.0 == 1"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("1 == nil"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("1 == true"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("1 == false"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("true == true"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("false == false"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("nil == nil"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("1 != 1"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("1 != 2"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("1.0 != 1"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("1 != nil"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("1 != true"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("1 != false"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("true != true"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("false != false"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("nil != nil"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("1 > 2"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("1 < 2"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("1 <= 1"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("1 >= 1"),
        Ok(LoxVal::Bool(true)),
    );
    assert!(matches!(
        common::run_program("1 >= true"),
        Err(VMError::TypeError { .. }),
    ));
    assert!(matches!(
        common::run_program("1 < nil"),
        Err(VMError::TypeError { .. }),
    ));
    assert!(matches!(
        common::run_program("true < nil"),
        Err(VMError::TypeError { .. }),
    ));
    assert!(matches!(
        common::run_program("true > false"),
        Err(VMError::TypeError { .. }),
    ));
}
