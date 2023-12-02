#![feature(assert_matches)]
use std::assert_matches::assert_matches;

use rlox::{vm::VMError, chunk::OwnedLoxVal};

mod common;

#[test]
fn test_constants() {
    assert_eq!(
        common::run_program("10;"),
        Ok(OwnedLoxVal::Num(10.0)),
    );
    assert_eq!(
        common::run_program("11.0;"),
        Ok(OwnedLoxVal::Num(11.0)),
    );
    assert_eq!(
        common::run_program("true;"),
        Ok(OwnedLoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("false;"),
        Ok(OwnedLoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("nil;"),
        Ok(OwnedLoxVal::Nil),
    );
}

#[test]
fn test_add_num() {
    assert_eq!(
        common::run_program("10 + 21;"),
        Ok(OwnedLoxVal::Num(31.0)),
    );
    assert_eq!(
        common::run_program("10 + 21 + 100;"),
        Ok(OwnedLoxVal::Num(131.0)),
    );
}

#[test]
fn test_add_str() {
    assert_eq!(
        common::run_program(r#""hello" + " " + "lox";"#),
        Ok(OwnedLoxVal::Str("hello lox".to_string())),
    );
}

#[test]
fn test_add_type_error() {
    assert_matches!(
        common::run_program(r#""hello " + 10;"#),
        Err(VMError::TypeError { .. }),
    );
}

#[test]
fn test_sub_num() {
    assert_eq!(
        common::run_program("10 - 21;"),
        Ok(OwnedLoxVal::Num(-11.0)),
    );
    assert_eq!(
        common::run_program("10 - 21 - 32;"),
        Ok(OwnedLoxVal::Num(-43.0)),
    );
}

#[test]
fn test_mult_num() {
    assert_eq!(
        common::run_program("10 * 21;"),
        Ok(OwnedLoxVal::Num(210.0)),
    )
}

#[test]
fn test_div_num() {
    assert_eq!(
        common::run_program("10 / 20;"),
        Ok(OwnedLoxVal::Num(0.5)),
    );
    assert_eq!(
        common::run_program("8 / 4 / 2;"),
        Ok(OwnedLoxVal::Num(1.0)),
    );
}

#[test]
fn test_unary_op() {
    assert_eq!(
        common::run_program("-38;"),
        Ok(OwnedLoxVal::Num(-38.0)),
    );
    assert_eq!(
        common::run_program("----1000;"),
        Ok(OwnedLoxVal::Num(1000.0)),
    );
}

#[test]
fn test_parens() {
    assert_eq!(
        common::run_program("(1 + 2) * 3;"),
        Ok(OwnedLoxVal::Num(9.0)),
    );
    assert_eq!(
        common::run_program("(1 + 2) * (3 + 2);"),
        Ok(OwnedLoxVal::Num(15.0)),
    );
}

#[test]
fn test_mixed_arithmetic() {
    assert_eq!(
        common::run_program("(10 + 30 * --20 / 100 + 4 - -20) / (2 + 8);"),
        Ok(OwnedLoxVal::Num(4.0)),
    );
}

#[test]
fn test_not() {
    assert_eq!(
        common::run_program("!true;"),
        Ok(OwnedLoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("!false;"),
        Ok(OwnedLoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("!nil;"),
        Ok(OwnedLoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("!10;"),
        Ok(OwnedLoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("!0;"),
        Ok(OwnedLoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("!-1;"),
        Ok(OwnedLoxVal::Bool(false)),
    );
}

#[test]
fn test_cmp() {
    assert_eq!(
        common::run_program("1 == 1;"),
        Ok(OwnedLoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("1 == 2;"),
        Ok(OwnedLoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("1.0 == 1;"),
        Ok(OwnedLoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("1 == nil;"),
        Ok(OwnedLoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("1 == true;"),
        Ok(OwnedLoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("1 == false;"),
        Ok(OwnedLoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("true == true;"),
        Ok(OwnedLoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("false == false;"),
        Ok(OwnedLoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("nil == nil;"),
        Ok(OwnedLoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("1 != 1;"),
        Ok(OwnedLoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("1 != 2;"),
        Ok(OwnedLoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("1.0 != 1;"),
        Ok(OwnedLoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("1 != nil;"),
        Ok(OwnedLoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("1 != true;"),
        Ok(OwnedLoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("1 != false;"),
        Ok(OwnedLoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("true != true;"),
        Ok(OwnedLoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("false != false;"),
        Ok(OwnedLoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("nil != nil;"),
        Ok(OwnedLoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("1 > 2;"),
        Ok(OwnedLoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program("1 < 2;"),
        Ok(OwnedLoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("1 <= 1;"),
        Ok(OwnedLoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program("1 >= 1;"),
        Ok(OwnedLoxVal::Bool(true)),
    );
    assert_matches!(
        common::run_program("1 >= true;"),
        Err(VMError::TypeError { .. }),
    );
    assert_matches!(
        common::run_program("1 < nil;"),
        Err(VMError::TypeError { .. }),
    );
    assert_matches!(
        common::run_program("true < nil;"),
        Err(VMError::TypeError { .. }),
    );
    assert_matches!(
        common::run_program("true > false;"),
        Err(VMError::TypeError { .. }),
    );
}

#[test]
fn test_print_stmt() {
    assert_eq!(
        common::run_program("print 20;"),
        Ok(OwnedLoxVal::Num(20.0)),
        // should be the last value pushed to the stack
    );
    assert_eq!(
        common::run_program("print 20 + 30;"),
        Ok(OwnedLoxVal::Num(50.0)),
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
        Ok(OwnedLoxVal::Num(10.0)),
    );
    assert_eq!(
        common::run_program(r#"
            if (1 < 0) {
                1;
            } else {
                2;
            }
        "#),
        Ok(OwnedLoxVal::Num(2.0)),
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
        Ok(OwnedLoxVal::Num(3.0)),
    );
    /*
    assert_eq!(
        common::run_program(r#"
            var x = 1;
            if (true) {
                var x = 2;
            }
            x;
        "#),
        Ok(OwnedLoxVal::Num(1.0)),
    );
    */
}

#[test]
fn test_global_var() {
    assert_eq!(
        common::run_program("var x; x;"),
        Ok(OwnedLoxVal::Nil),
    );
    assert_eq!(
        common::run_program("var x = 1; x;"),
        Ok(OwnedLoxVal::Num(1.0)),
    );
    assert_eq!(
        common::run_program("var x = 10 * 3 + 3; x;"),
        Ok(OwnedLoxVal::Num(33.0)),
    );
    assert_eq!(
        common::run_program("var x = 1; x = 2 ; x ;"),
        Ok(OwnedLoxVal::Num(2.0)),
    );
    assert_eq!(
        common::run_program("x;"),
        Err(VMError::UndefinedVariable { line: 0, name: "x".to_string()}),
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
        Ok(OwnedLoxVal::Num(10.0)),
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
        Ok(OwnedLoxVal::Num(10.0)),
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
        Ok(OwnedLoxVal::Num(10.0)),
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
        Ok(OwnedLoxVal::Num(20.0)),
    );
    assert_eq!(
        common::run_program(r#"
            { var a = 10;
                {
                    var a = 20;
                }
            }
        "#),
        Ok(OwnedLoxVal::Num(20.0)),
    );
    assert_matches!(
        common::compile(r#"
            {
                var a = 10;
                var a = 20;
            }
        "#),
        None,
    );
}

#[test]
fn test_bool_operator() {
    assert_eq!(
        common::run_program(r#"
            true and false;
        "#),
        Ok(OwnedLoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program(r#"
            false and true;
        "#),
        Ok(OwnedLoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program(r#"
            1 and 0;
        "#),
        Ok(OwnedLoxVal::Num(0.0)),
    );
    assert_eq!(
        common::run_program(r#"
            true and true and false and 10;
        "#),
        Ok(OwnedLoxVal::Bool(false)),
    );
    assert_eq!(
        common::run_program(r#"
            10 or false;
        "#),
        Ok(OwnedLoxVal::Num(10.0)),
    );
    assert_eq!(
        common::run_program(r#"
            false or true;
        "#),
        Ok(OwnedLoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program(r#"
            false or nil;
        "#),
        Ok(OwnedLoxVal::Nil),
    );
    assert_eq!(
        common::run_program(r#"
            true or true and false and 10;
        "#),
        Ok(OwnedLoxVal::Bool(true)),
    );
    assert_eq!(
        common::run_program(r#"
            false or true and false or 20;
        "#),
        Ok(OwnedLoxVal::Num(20.0)),
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
        Ok(OwnedLoxVal::Num(10.0)),
    );
    assert_eq!(
        common::run_program(r#"
            var x = 0;
            while (false) {
                x = x + 1;
            }
            x;
        "#),
        Ok(OwnedLoxVal::Num(0.0)),
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
        Ok(OwnedLoxVal::Num(10.0)),
    );
    assert_eq!(
        common::run_program(r#"
            var x = 0;
            for (var y = 0; y < 10; y = y + 1) {
                x = y;
            }
            x;
        "#),
        Ok(OwnedLoxVal::Num(9.0)),
    );
    assert_eq!(
        common::run_program(r#"
            var x = 0;
            for (var y = 0; false; y = y + 1) {
                x = y;
            }
            x;
        "#),
        Ok(OwnedLoxVal::Num(0.0)),
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
        Ok(OwnedLoxVal::Num(9.0)),
    );
}

#[test]
fn test_functions() {
    assert_eq!(
        common::run_program(r#"
            fun f(arg) {
                arg + 10;
            }

            f(20);
        "#),
        Ok(OwnedLoxVal::Nil),
    );
    assert_eq!(
        common::run_program(r#"
            fun math(a, b, c) {
                return a - b + c;
            }

            math(1, 2, 4);
        "#),
        Ok(OwnedLoxVal::Num(3.0)),
    );
    assert_eq!(
        common::run_program(r#"
            fun fib(n) {
                if (n < 2) return n;
                return fib(n - 2) + fib(n - 1);
            }

            fib(3);
        "#),
        Ok(OwnedLoxVal::Num(2.0)),
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
        Ok(OwnedLoxVal::Str("outer".to_string())),
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
        Ok(OwnedLoxVal::Str("not dropped".to_string())),
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
        Ok(OwnedLoxVal::Str("separate variables".to_string())),
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
        Ok(OwnedLoxVal::Str("value".to_string())),
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
        Ok(OwnedLoxVal::Str("assigned".to_string())),
    );
}

#[test]
fn test_classes() {
    assert_matches!(
        common::run_program(r#"
            class C {}
            C;
        "#),
        Ok(OwnedLoxVal::Class(_)),
    );
    assert_matches!(
        common::run_program(r#"
            class C {}
            var x = C();
        "#),
        Ok(OwnedLoxVal::Instance(_)),
    );
    assert_eq!(
        common::run_program(r#"
            class C {}
            var x = C();
            x.a = 1;
            x.b = 2;
            x.a + x.b;
        "#),
        Ok(OwnedLoxVal::Num(3.0)),
    );
    assert_eq!(
        common::run_program(r#"
            class C {}
            var x = C();
            var y = C();
            y.b = 2;
            x.a = y;
            x.a.b = 3;
            x.a.b - 2;
        "#),
        Ok(OwnedLoxVal::Num(1.0)),
    );
    assert_matches!(
        common::run_program(r#"
            class C {
                f() {}
            }
            var x = C();
            x.f;
        "#),
        Ok(OwnedLoxVal::Closure(_)),
    );
    assert_eq!(
        common::run_program(r#"
            class C {
                f(a, b) {
                    return a - b;
                }
            }
            var x = C();
            x.f(1, 2) + 10;
        "#),
        Ok(OwnedLoxVal::Num(9.0)),
    );
    assert_eq!(
        common::run_program(r#"
            class C {
                init() {
                    this.x = 10;
                }
            }
            var c = C();
            c.x + 5;
        "#),
        Ok(OwnedLoxVal::Num(15.0)),
    );
    assert_eq!(
        common::run_program(r#"
            class C {
                init (x) {
                    this.x = x;
                }
                f() {
                    var old = this.x;
                    return old;
                }
            }
            var c = C(10);
            c.f() + 5;
        "#),
        Ok(OwnedLoxVal::Num(15.0)),
    );
    assert_eq!(
        common::run_program(r#"
            class Nested {
                method() {
                    fun function() {
                        return this.x;
                    }
                    return function;
                }
            }
            var i = Nested();
            i.x = 10;
            var m = i.method();
            m() + 5.0;
        "#),
        Ok(OwnedLoxVal::Num(15.0)),
    );
    assert_eq!(
        common::run_program(r#"
            fun f(x) {
                class C {
                    method() {
                        return this.x + x;
                    }
                }
                var c = C();
                c.x = 2;
                return c;
            }
            var a = f(1);
            a.method();
        "#),
        Ok(OwnedLoxVal::Num(3.0)),
    );
    assert_eq!(
        common::run_program(r#"
            class C {
                init(x) {
                    this.x = x;
                }
                method(y) {
                    fun f() {
                        return this.x + y;
                    }
                    return f;
                }
            }
            var c = C(1);
            var f = c.method(2);
            f();
        "#),
        Ok(OwnedLoxVal::Num(3.0)),
    );
    assert_eq!(
        common::run_program(r#"
            class C {
                init(x) {
                    this.x = x;
                }
                method(y) {
                    fun f() {
                        return this.x + y;
                    }
                    return f;
                }
            }
            var c = C(1);
            var f = c.method(2);
            f();
        "#),
        Ok(OwnedLoxVal::Num(3.0)),
    );
    assert_eq!(
        common::run_program(r#"
            class A {
                f() {
                    this.x = 10;
                }
            }
            class C < A{
                g() {
                    return this.x + 20;
                }
            }
            var c = C();
            c.f();
            c.g() + 5;
        "#),
        Ok(OwnedLoxVal::Num(35.0)),
    );
    assert_eq!(
        common::run_program(r#"
            class A {
                init() {
                    this.x = 10;
                    this.f();
                }
                f() {
                    this.x = this.x + 10;
                }
            }
            class C < A {
                init() {
                    super.init();
                }
                g() {
                    super.f();
                    return this.x + 10;
                }
            }
            var c = C();
            c.f();
            c.g() + 5;
        "#),
        Ok(OwnedLoxVal::Num(55.0)),
    );
}
