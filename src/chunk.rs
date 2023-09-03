use std::collections::HashMap;
use std::fmt::Debug;

use crate::arena::Ref;
use crate::vm::VMError;

#[derive(Clone, PartialEq)]
pub enum LoxVal {
    Bool(bool),
    Nil,
    Num(f64),
    Str(String),
    CompiledFn(CompiledFn),
    NativeFn(NativeFn),
    Class(Ref<Class>),
    Instance(Ref<ClassInstance>),
    BoundMethod(Ref<BoundMethod>),
}

#[derive(Clone, PartialEq)]
pub struct CompiledFn {
    pub arity: u8,
    pub chunk: Chunk,
    pub name: Ref<String>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Class {
    pub name: String,
    pub methods: HashMap<String, CompiledFn>,
    pub sup: Option<Ref<Class>>,
}

impl Class {
    pub fn new_instance(&self) -> ClassInstance {
        ClassInstance {
            class: self.clone(),
            fields: HashMap::new(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClassInstance {
    pub class: Class,
    pub fields: HashMap<String, LoxVal>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BoundMethod {
    pub this: Ref<ClassInstance>,
    pub method: CompiledFn,
    pub sup: Option<Ref<Class>>,
}

type NativeFn = fn(&[LoxVal]) -> Result<LoxVal, VMError>;

pub enum Callable {
    CompiledFn(CompiledFn),
    NativeFn(NativeFn),
    Class(Ref<Class>),
    Method(Ref<BoundMethod>),
}

impl LoxVal {
    pub fn type_name(&self) -> String {
        match self {
            LoxVal::Bool(_) => "bool".to_string(),
            LoxVal::Nil     => "nil".to_string(),
            LoxVal::Num(_)  => "number".to_string(),
            LoxVal::Str(_)  => "string".to_string(),
            LoxVal::CompiledFn(_)  => "function".to_string(),
            LoxVal::NativeFn(_)  => "function (builtin)".to_string(),
            LoxVal::Class(_)  => "class".to_string(),
            LoxVal::Instance(_)  => format!("instance"),
            LoxVal::BoundMethod(_)  => format!("method"),
        }
    }

    pub fn cast_to_bool(&self) -> LoxVal {
        match self {
            LoxVal::Nil | LoxVal::Bool(false) => LoxVal::Bool(false),
            _ => LoxVal::Bool(true),
        }
    }

    pub fn cast_to_not_bool(&self) -> LoxVal {
        match self {
            LoxVal::Nil | LoxVal::Bool(false) => LoxVal::Bool(true),
            _ => LoxVal::Bool(false),
        }
    }
}

impl std::fmt::Debug for LoxVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxVal::Bool(b) => write!(f, "Bool: {b}"),
            LoxVal::Nil     => write!(f, "nil"),
            LoxVal::Num(n)  => write!(f, "Num: {n}"),
            LoxVal::Str(s)  => write!(f, "Str: \"{s}\""),
            LoxVal::CompiledFn(fun)  => write!(f, "Function: \"{fun:?}\""),
            LoxVal::NativeFn(fun)  => write!(f, "Native function: \"{fun:?}\""),
            LoxVal::Class(cls)  => write!(f, "Class: \"{cls:?}\""),
            LoxVal::Instance(val)  => write!(f, "Class instance: \"{val:?}\""),
            LoxVal::BoundMethod(m) => write!(f, "Bound method: {m:?}"),
        }
    }
}

impl std::fmt::Display for LoxVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxVal::Bool(b) => write!(f, "{b}"),
            LoxVal::Nil     => write!(f, "nil"),
            LoxVal::Num(n)  => write!(f, "{n}"),
            LoxVal::Str(s)  => write!(f, "{s}"),
            LoxVal::CompiledFn(fun)  => write!(f, "<function: {}>", fun.name),
            LoxVal::NativeFn(_)  => write!(f, "<builtin function>"),
            LoxVal::Class(_)  => write!(f, "<class>"),
            LoxVal::Instance(_)  => write!(f, "<class instance>"),
            LoxVal::BoundMethod(_)  => write!(f, "<method>"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum OpCode {
    Add,
    Call(u8),
    Constant(LoxVal),
    Class(Ref<String>),
    Closure(Ref<CompiledFn>),
    DefineClass(Ref<String>),
    DefineGlobal(Ref<String>),
    Divide,
    Equal,
    GetProperty(Ref<String>),
    GetGlobal(Ref<String>),
    GetLocal(LocalVarRef),
    GetSuperMethod(LocalVarRef, Ref<String>),
    GetUpval(usize),
    Greater,
    Inherit,
    Jump(usize),
    JumpIfTrue(usize),
    JumpIfFalse(usize),
    Less,
    Method(Ref<String>),
    Multiply,
    Negate,
    Not,
    Pop,
    Print,
    Return,
    SetProperty(Ref<String>),
    SetGlobal(Ref<String>),
    SetLocal(LocalVarRef),
    SetUpval(usize),
    Substract,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LocalVarRef {
    pub frame: usize,
    pub pos: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Instruction {
    pub op: OpCode,
    pub line: u64,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Chunk (pub Vec<Instruction>);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum FnType {
    Regular,
    Main,
    Method,
    Ctor,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn loxval_display() {
        assert_eq!(
            format!("{}", LoxVal::Bool(true)),
            "true".to_string(),
        );
        assert_eq!(
            format!("{}", LoxVal::Bool(false)),
            "false".to_string(),
        );
        assert_eq!(
            format!("{}", LoxVal::Nil),
            "nil".to_string(),
        );
        assert_eq!(
            format!("{}", LoxVal::Num(3.0)),
            "3".to_string(),
        );
        assert_eq!(
            format!("{}", LoxVal::Num(3.14)),
            "3.14".to_string(),
        );
        assert_eq!(
            format!("{}", LoxVal::Num(-3.14)),
            "-3.14".to_string(),
        );
        assert_eq!(
            format!("{}", LoxVal::Str("hello world!".to_string())),
            "hello world!".to_string(),
        );
    }

    #[test]
    fn loxval_debug() {
        assert_eq!(
            format!("{:?}", LoxVal::Bool(true)),
            "Bool: true".to_string(),
        );
        assert_eq!(
            format!("{:?}", LoxVal::Bool(false)),
            "Bool: false".to_string(),
        );
        assert_eq!(
            format!("{:?}", LoxVal::Nil),
            "nil".to_string(),
        );
        assert_eq!(
            format!("{:?}", LoxVal::Num(3.0)),
            "Num: 3".to_string(),
        );
        assert_eq!(
            format!("{:?}", LoxVal::Num(3.14)),
            "Num: 3.14".to_string(),
        );
        assert_eq!(
            format!("{:?}", LoxVal::Num(-3.14)),
            "Num: -3.14".to_string(),
        );
        assert_eq!(
            format!("{:?}", LoxVal::Str("hello world!".to_string())),
            r#"Str: "hello world!""#.to_string(),
        );
    }
}
