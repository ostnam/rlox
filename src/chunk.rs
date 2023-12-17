use std::collections::HashMap;
use std::fmt::Debug;

use crate::arena::Ref;
use crate::vm::VMError;

/// Every value Lox programs can manipulate
#[derive(Clone, Debug, PartialEq)]
pub enum LoxVal {
    Bool(bool),
    Nil,
    Num(f64),
    Str(Ref<String>),
    /// Used for functions, closures and methods
    Closure(Ref<Closure>),
    NativeFn(NativeFn),
    Class(Ref<Class>),
    Instance(Ref<ClassInstance>),
}

/// Like a `LoxVal`, but every `Ref<T>` field is owned instead.
/// Used to return values from the VM.
#[derive(Clone, Debug, PartialEq)]
pub enum OwnedLoxVal {
    Bool(bool),
    Nil,
    Num(f64),
    Str(String),
    Closure(Closure),
    NativeFn(NativeFn),
    Class(Class),
    Instance(ClassInstance),
}

#[derive(Clone, Debug, PartialEq)]
pub struct CompiledFn {
    pub arity: u8,
    pub chunk: Chunk,
    pub name: Ref<String>,
}

/// Represents functions, closures and methods.
#[derive(Clone, Debug, PartialEq)]
pub struct Closure {
    pub function: Ref<CompiledFn>,

    /// idx of every instruction that closes over a local variable
    pub upvalues: Vec<Ref<LoxVal>>,

    /// `None` unless it's a method.
    pub this: Option<Ref<ClassInstance>>,

    /// `None` unless it's a method from a class with a superclass.
    pub sup: Option<Ref<Class>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Upvalue {
    Local(RelativeStackIdx),
    Parent(usize),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Class {
    pub name: Ref<String>,
    pub methods: HashMap<String, Ref<Closure>>,
    pub sup: Option<Ref<Class>>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClassInstance {
    pub class: Ref<Class>,
    pub fields: HashMap<String, LoxVal>,
}

/// Instantiate a new, empty `ClassInstance` of the passed-in `class`.
/// Doesn't evaluate the constructor.
pub fn new_class_instance(class: Ref<Class>) -> ClassInstance {
    ClassInstance {
        class,
        fields: HashMap::new(),
    }
}

type NativeFn = fn(&[LoxVal]) -> Result<LoxVal, VMError>;

pub enum Callable {
    Closure(Ref<Closure>),
    NativeFn(NativeFn),
    Class(Ref<Class>),
}

impl LoxVal {
    pub fn type_name(&self) -> String {
        match self {
            LoxVal::Bool(_) => "bool".to_string(),
            LoxVal::Nil     => "nil".to_string(),
            LoxVal::Num(_)  => "number".to_string(),
            LoxVal::Str(_)  => "string".to_string(),
            LoxVal::Closure(_)  => "function".to_string(),
            LoxVal::NativeFn(_)  => "function (builtin)".to_string(),
            LoxVal::Class(_)  => "class".to_string(),
            LoxVal::Instance(_)  => format!("instance"),
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RelativeStackIdx(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ChunkIndex(pub usize);

#[derive(Clone, Debug, PartialEq)]
pub enum OpCode {
    Add,

    /// Stores the number of arguments stored.
    /// When it is called, the top of the stack will be the function to call,
    /// and the values before that the rest of the arguments.
    /// The function (top of stack) will be popped by this instruction.
    Call(u8),

    CaptureUpvalue(Upvalue),
    Constant(LoxVal),
    Class(Ref<String>),
    Closure(Ref<CompiledFn>),
    DefineClass(Ref<String>),
    DefineGlobal(Ref<String>),
    Divide,
    Equal,
    GetProperty(Ref<String>),
    GetGlobal(Ref<String>),
    GetLocal(RelativeStackIdx),
    GetSuperMethod(Ref<String>),
    GetUpval(usize),
    GE,
    GT,
    Inherit,
    Jump(usize),
    JumpIfTrue(usize),
    JumpIfFalse(usize),
    LE,
    LT,
    Method(Ref<String>),
    Multiply,
    Negate,
    Not,
    NotEqual,
    Pop,
    PopN(u8),
    Print,
    Return,
    SetProperty(Ref<String>),
    SetGlobal(Ref<String>),
    SetLocal(RelativeStackIdx),
    SetUpval(usize),
    Substract,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Instruction {
    pub op: OpCode,
    pub line: u64,
}

pub type Chunk = Vec<Instruction>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum FnType {
    Regular,
    Main,
    Method,
    Ctor,
}
