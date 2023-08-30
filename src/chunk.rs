use std::collections::HashMap;
use std::fmt::{Debug, Display};

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

#[derive(Clone, PartialEq)]
pub enum OpCode {
    Add,
    Call(u8),
    Constant(LoxVal),
    Class(String),
    Closure(Function),
    DefineClass(String),
    DefineGlobal(String),
    Divide,
    Equal,
    GetProperty(String),
    GetGlobal(String),
    GetLocal(LocalVarRef),
    GetSuperMethod(LocalVarRef, String),
    GetUpval(usize),
    Greater,
    Inherit,
    Jump(usize),
    JumpIfTrue(usize),
    JumpIfFalse(usize),
    Less,
    Method(String),
    Multiply,
    Negate,
    Not,
    Pop,
    Print,
    Return,
    SetProperty(String),
    SetGlobal(String),
    SetLocal(LocalVarRef),
    SetUpval(usize),
    Substract,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LocalVarRef {
    pub frame: usize,
    pub pos: usize,
}

#[derive(Clone, PartialEq)]
pub struct Instruction {
    pub op: OpCode,
    pub line: u64,
}

impl Debug for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Instr, line {}: {}", self.line, self.op)
    }
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            OpCode::Add => "ADD".to_string(),
            OpCode::Call(args) => format!("CALL WITH {args} args"),
            OpCode::Class(cls) => format!("CLASS: {}", cls),
            OpCode::Closure(f) => format!("CLOSURE: {}", f.name),
            OpCode::Constant(idx) => format!("CONSTANT {idx}"),
            OpCode::DefineClass(name) => format!("DEFCLASS {name}"),
            OpCode::DefineGlobal(name) => format!("DEFGLOBAL {name}"),
            OpCode::Divide => "DIVIDE".to_string(),
            OpCode::Equal => "EQUAL".to_string(),
            OpCode::GetProperty(name) => format!("GET PROPERTY: {name}"),
            OpCode::GetGlobal(name) => format!("GET GLOBAL: {name}"),
            OpCode::GetLocal(pos) => format!("GET LOCAL FRAME: {} POS: {}", pos.frame, pos.pos),
            OpCode::GetSuperMethod(pos, name) => format!("GET SUPER POS: {}, {}, NAME: {}", pos.frame, pos.pos, name),
            OpCode::GetUpval(pos) => format!("GET UPVAL IDX: {pos}"),
            OpCode::Greater => "GREATER".to_string(),
            OpCode::Inherit => "INHERIT".to_string(),
            OpCode::Jump(jmp_size) => format!("JUMP: {jmp_size} instructions"),
            OpCode::JumpIfFalse(jmp_size) => format!("JUMP IF FALSE: {jmp_size} instructions"),
            OpCode::JumpIfTrue(jmp_size) => format!("JUMP IF TRUE: {jmp_size} instructions"),
            OpCode::Less => "LESS".to_string(),
            OpCode::Method(name) => format!("METHOD: {name}"),
            OpCode::Multiply => "MULTIPLY".to_string(),
            OpCode::Negate => "NEGATE".to_string(),
            OpCode::Not => "NOT".to_string(),
            OpCode::Pop => "POP".to_string(),
            OpCode::Print => "PRINT".to_string(),
            OpCode::Return => "RETURN".to_string(),
            OpCode::SetProperty(name) => format!("SET PROPERTY: {name}"),
            OpCode::SetGlobal(name) => format!("SET GLOBAL: {name}"),
            OpCode::SetLocal(pos) => format!("SET LOCAL FRAME: {} POS: {}", pos.frame, pos.pos),
            OpCode::SetUpval(pos) => format!("SET UPVAL IDX: {pos}"),
            OpCode::Substract => "SUBSTRACT".to_string(),
        };
        write!(f, "{}", name)
    }
}

impl std::fmt::Debug for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}


#[derive(Clone, Debug, Default, PartialEq)]
pub struct Chunk (pub Vec<Instruction>);

impl Chunk {
    pub fn disassemble(&self, name: &str) {
        println!("== {name} ==");
        let mut previous = 0;
        for chunk in &self.0 {
            let line_marker = if chunk.line == previous {
                "|".to_string()
            } else {
                previous = chunk.line;
                previous.to_string()
            };
            println!("{line_marker}:{}", chunk.op);
        }
    }
}

impl std::fmt::Debug for CompiledFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "function {}:", self.name)?;
        writeln!(f, "arity {}:", self.arity)?;
        for instr in &self.chunk.0 {
            writeln!(f, "{instr:?}")?;
        }
        write!(f, "")
    }
}

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
