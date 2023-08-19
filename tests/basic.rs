use rlox::chunk::{LoxVal, Function, Chunk, Instruction, OpCode, LocalVarRef, Class};
use rlox::vm::VMError;

mod common;

#[test]
fn test_constants() {
    assert_eq!(
        common::run_expr("10"),
        Ok(LoxVal::Num(10.0)),
    );
    assert_eq!(
        common::run_expr("11.0"),
        Ok(LoxVal::Num(11.0)),
    );
    assert_eq!(
        common::run_expr("true"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_expr("false"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_expr("nil"),
        Ok(LoxVal::Nil),
    );
}

#[test]
fn test_add_num() {
    assert_eq!(
        common::run_expr("10 + 21"),
        Ok(LoxVal::Num(31.0)),
    );
    assert_eq!(
        common::run_expr("10 + 21 + 100"),
        Ok(LoxVal::Num(131.0)),
    );
}

#[test]
fn test_add_str() {
    assert_eq!(
        common::run_expr(r#""hello" + " " + "lox""#),
        Ok(LoxVal::Str("hello lox".to_string())),
    );
}

#[test]
fn test_add_type_error() {
    assert!(matches!(
        common::run_expr(r#""hello " + 10"#),
        Err(VMError::TypeError {
            line: 1,
            ..
        }),
    ));
}

#[test]
fn test_sub_num() {
    assert_eq!(
        common::run_expr("10 - 21"),
        Ok(LoxVal::Num(-11.0)),
    );
    assert_eq!(
        common::run_expr("10 - 21 - 32"),
        Ok(LoxVal::Num(-43.0)),
    );
}

#[test]
fn test_mult_num() {
    assert_eq!(
        common::run_expr("10 * 21"),
        Ok(LoxVal::Num(210.0)),
    )
}

#[test]
fn test_div_num() {
    assert_eq!(
        common::run_expr("10 / 20"),
        Ok(LoxVal::Num(0.5)),
    );
    assert_eq!(
        common::run_expr("8 / 4 / 2"),
        Ok(LoxVal::Num(1.0)),
    );
}

#[test]
fn test_unary_op() {
    assert_eq!(
        common::run_expr("-38"),
        Ok(LoxVal::Num(-38.0)),
    );
    assert_eq!(
        common::run_expr("----1000"),
        Ok(LoxVal::Num(1000.0)),
    );
}

#[test]
fn test_parens() {
    assert_eq!(
        common::run_expr("(1 + 2) * 3"),
        Ok(LoxVal::Num(9.0)),
    );
    assert_eq!(
        common::run_expr("(1 + 2) * (3 + 2)"),
        Ok(LoxVal::Num(15.0)),
    );
}

#[test]
fn test_mixed_arithmetic() {
    assert_eq!(
        common::run_expr("(10 + 30 * --20 / 100 + 4 - -20) / (2 + 8)"),
        Ok(LoxVal::Num(4.0)),
    );
}

#[test]
fn test_not() {
    assert_eq!(
        common::run_expr("!true"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_expr("!false"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_expr("!nil"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_expr("!10"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_expr("!0"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_expr("!-1"),
        Ok(LoxVal::Bool(false)),
    );
}

// < <= >= >
#[test]
fn test_cmp() {
    assert_eq!(
        common::run_expr("1 == 1"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_expr("1 == 2"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_expr("1.0 == 1"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_expr("1 == nil"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_expr("1 == true"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_expr("1 == false"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_expr("true == true"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_expr("false == false"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_expr("nil == nil"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_expr("1 != 1"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_expr("1 != 2"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_expr("1.0 != 1"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_expr("1 != nil"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_expr("1 != true"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_expr("1 != false"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_expr("true != true"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_expr("false != false"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_expr("nil != nil"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_expr("1 > 2"),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_expr("1 < 2"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_expr("1 <= 1"),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_expr("1 >= 1"),
        Ok(LoxVal::Bool(true)),
    );
    assert!(matches!(
        common::run_expr("1 >= true"),
        Err(VMError::TypeError { .. }),
    ));
    assert!(matches!(
        common::run_expr("1 < nil"),
        Err(VMError::TypeError { .. }),
    ));
    assert!(matches!(
        common::run_expr("true < nil"),
        Err(VMError::TypeError { .. }),
    ));
    assert!(matches!(
        common::run_expr("true > false"),
        Err(VMError::TypeError { .. }),
    ));
}

#[test]
fn test_expr_stmt() {
    assert_eq!(
        common::run_program("20 + 20;"),
        Ok(LoxVal::Num(40.0)),
    );
}

#[test]
fn test_print_stmt() {
    assert_eq!(
        common::run_program("print 20;"),
        Ok(LoxVal::Num(20.0)),
        // should be the last value pushed to the stack
    );
    assert_eq!(
        common::run_program("print 20 + 30;"),
        Ok(LoxVal::Num(50.0)),
    );
}

#[test]
fn test_global_var() {
    assert_eq!(
        common::run_program("var x; x;"),
        Ok(LoxVal::Nil),
    );
    assert_eq!(
        common::run_program("var x = 1; x;"),
        Ok(LoxVal::Num(1.0)),
    );
    assert_eq!(
        common::run_program("var x = 10 * 3 + 3; x;"),
        Ok(LoxVal::Num(33.0)),
    );
    assert_eq!(
        common::run_program("var x = 1; x = 2; x;"),
        Ok(LoxVal::Num(2.0)),
    );
    assert_eq!(
        common::run_program("x;"),
        Err(VMError::UndefinedVariable { line: 1, name: "x".to_string()}),
    );
    assert_eq!(
        common::run_program("x = 10;"),
        Err(VMError::UndefinedVariable { line: 0, name: "x".to_string()}),
    );
}

#[test]
fn test_local_var() {
    assert_eq!(
        common::run_program(r#"
            {
                var x = 10;
                x;
            }
        "#),
        Ok(LoxVal::Num(10.0)),
    );
    assert_eq!(
        common::run_program(r#"
            {
                var x = 10;
                20;
                {
                    x;
                }
            }
        "#),
        Ok(LoxVal::Num(10.0)),
    );
    assert_eq!(
        common::run_program(r#"
            {
                var x = 10;
                {
                    var x = 20;
                }
                x;
            }
        "#),
        Ok(LoxVal::Num(10.0)),
    );
    assert_eq!(
        common::run_program(r#"
            var x = 10;
            {
                var y = 20;
                x = y;
                x = y;
            }
            x;
        "#),
        Ok(LoxVal::Num(20.0)),
    );
}

#[test]
fn test_if_stmt() {
    assert_eq!(
        common::run_program(r#"
            if (1 > 0) {
                10;
            }
        "#),
        Ok(LoxVal::Num(10.0)),
    );
    assert_eq!(
        common::run_program(r#"
            if (1 < 0) {
                1;
            } else {
                2;
            }
        "#),
        Ok(LoxVal::Num(2.0)),
    );
    assert_eq!(
        common::run_program(r#"
            if (1 < 0) {
                1;
            } else if (false) {
                2;
            } else {
                3;
            }
        "#),
        Ok(LoxVal::Num(3.0)),
    );
    assert_eq!(
        common::run_program(r#"
            var x = 1;
            if (true) {
                var x = 2;
            }
            x;
        "#),
        Ok(LoxVal::Num(1.0)),
    );
}

#[test]
fn test_bool_operator() {
    assert_eq!(
        common::run_program(r#"
            true and false;
        "#),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program(r#"
            false and true;
        "#),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program(r#"
            1 and 0;
        "#),
        Ok(LoxVal::Num(0.0)),
    );
    assert_eq!(
        common::run_program(r#"
            true and true and false and 10;
        "#),
        Ok(LoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program(r#"
            10 or false;
        "#),
        Ok(LoxVal::Num(10.0)),
    );
    assert_eq!(
        common::run_program(r#"
            false or true;
        "#),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program(r#"
            false or nil;
        "#),
        Ok(LoxVal::Nil),
    );
    assert_eq!(
        common::run_program(r#"
            true or true and false and 10;
        "#),
        Ok(LoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program(r#"
            false or true and false or 20;
        "#),
        Ok(LoxVal::Num(20.0)),
    );
}

#[test]
fn test_while() {
    assert_eq!(
        common::run_program(r#"
            var x = 0;
            while (x < 10) {
                x = x + 1;
            }
            x;
        "#),
        Ok(LoxVal::Num(10.0)),
    );
    assert_eq!(
        common::run_program(r#"
            var x = 0;
            while (false) {
                x = x + 1;
            }
            x;
        "#),
        Ok(LoxVal::Num(0.0)),
    );
}

#[test]
fn test_for() {
    assert_eq!(
        common::run_program(r#"
            var x = 0;
            for (; x < 10; x = x + 1) {
            }
            x;
        "#),
        Ok(LoxVal::Num(10.0)),
    );
    assert_eq!(
        common::run_program(r#"
            var x = 0;
            for (var y = 0; y < 10; y = y + 1) {
                x = y;
            }
            x;
        "#),
        Ok(LoxVal::Num(9.0)),
    );
    assert_eq!(
        common::run_program(r#"
            var x = 0;
            for (var y = 0; false; y = y + 1) {
                x = y;
            }
            x;
        "#),
        Ok(LoxVal::Num(0.0)),
    );
    assert_eq!(
        common::run_program(r#"
            var x = 0;
            var y;
            for (y = 0; y < 10; y = y + 1) {
                x = y;
            }
            x;
        "#),
        Ok(LoxVal::Num(9.0)),
    );
}

#[test]
fn test_functions() {
    assert_eq!(
        common::run_program(r#"
            fun f(arg) {
                arg + 10;
            }

            print f;
        "#),
        Ok(LoxVal::Function(Function {
            arity: 1,
            chunk: Chunk(vec![
                Instruction { op: OpCode::GetLocal(LocalVarRef { frame: 1, pos: 0 }), line: 3},
                Instruction { op: OpCode::Constant(LoxVal::Num(10.0)), line: 3},
                Instruction { op: OpCode::Add, line: 3},
                Instruction { op: OpCode::Pop, line: 3},
                Instruction { op: OpCode::Constant(LoxVal::Nil), line: 4},
                Instruction { op: OpCode::Return, line: 4},
            ]),
            name: "f".to_string(),
        })),
    );
    assert_eq!(
        common::run_program(r#"
            fun math(a, b, c) {
                return a - b + c;
            }

            math(1, 2, 4);
        "#),
        Ok(LoxVal::Num(3.0)),
    );
    assert_eq!(
        common::run_program(r#"
            fun fib(n) {
                if (n < 2) return n;
                return fib(n - 2) + fib(n - 1);
            }

            fib(3);
        "#),
        Ok(LoxVal::Num(2.0)),
    );
}

#[test]
fn test_closures() {
    assert_eq!(
        common::run_program(r#"
            var x = "global";
            fun outer() {
                var x = "outer";
                    fun inner() {
                        return x;
                    }
                return inner();
            }
            outer();
        "#),
        Ok(LoxVal::Str("outer".to_string())),
    );
    assert_eq!(
        common::run_program(r#"
            var x = "global";
            fun outer() {
                var x = "not dropped";
                    fun inner() {
                        return x;
                    }
                return inner;
            }
            var z = outer();
            z();
        "#),
        Ok(LoxVal::Str("not dropped".to_string())),
    );
    assert_eq!(
        common::run_program(r#"
            fun makeClosure(value) {
                fun closure() {
                    return value;
                }
                return closure;
            }
            var a = makeClosure("separate ");
            var b = makeClosure("variables");
            a() + b();
        "#),
        Ok(LoxVal::Str("separate variables".to_string())),
    );
    assert_eq!(
        common::run_program(r#"
            fun outer() {
                var x = "value";
                fun middle() {
                    fun inner() {
                        return x;
                    }
                    return inner;
                }
                return middle;
            }
            var mid = outer();
            var in = mid();
            in();
        "#),
        Ok(LoxVal::Str("value".to_string())),
    );
    assert_eq!(
        common::run_program(r#"
        fun outer() {
            var x = "before";
            fun inner() {
                x = "assigned";
            }
            inner();
            return x;
            }
        outer();
        "#),
        Ok(LoxVal::Str("assigned".to_string())),
    );
}

#[test]
fn test_classes() {
    assert_eq!(
        common::run_program(r#"
            class C {}
            C;
        "#),
        Ok(LoxVal::Class(Class { name: "C".to_string()})),
    );
}
