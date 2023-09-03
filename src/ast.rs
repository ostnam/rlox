use crate::{arena::Ref, chunk::OpCode};

pub type Program = Vec<Declaration>;

#[derive(PartialEq, Debug)]
pub enum Declaration {
    Class {
        name: Ref<String>,
        super_name: Option<Ref<String>>,
        methods: Vec<Ref<Function>>,
    },
    Fun(Ref<Function>),
    Var(VarDecl),
    Stmt(Stmt),
}

#[derive(PartialEq, Debug)]
pub struct VarDecl {
    pub name: Ref<String>,
    pub val: Option<Expr>,
}

#[derive(PartialEq, Debug)]
pub struct Function {
    pub arity: u8,
    pub body: Block,
    pub closed_over: Vec<StackRef>,
    pub sub_closures: Vec<Ref<Function>>,
}

#[derive(PartialEq, Debug)]
pub struct StackRef {
    frame: usize,
    index: usize,
}

pub type Block = Vec<Declaration>;

#[derive(PartialEq, Debug)]
pub enum Stmt {
    Expr(Expr),
    For {
        init: Option<ForInit>,
        cond: Option<Expr>,
        update: Option<Expr>,
        body: Box<Stmt>,
    },
    If {
        cond: Expr,
        body: Box<Stmt>,
        else_cond: Option<Box<Stmt>>,
    },
    Print(Expr),
    Return(Option<Expr>),
    While {
        cond: Expr,
        body: Box<Stmt>,
    },
    Block(Block),
}

#[derive(PartialEq, Debug)]
pub enum ForInit {
    Decl(VarDecl),
    Expr(Expr),
}

#[derive(PartialEq, Debug)]
pub enum Expr {
    Assignment {
        tgt: Box<Expr>,
        val: Box<Expr>,
    },
    Unop {
        op: UnaryOperator,
        val: Box<Expr>,
    },
    Binop {
        op: BinaryOperator,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Primary(Primary),
    Call {
        lhs: Box<Expr>,
        args: Vec<Expr>,
    },
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Dot(Box<Expr>, Ref<String>),
}

#[derive(PartialEq, Debug)]
pub enum UnaryOperator {
    Not,
    Neg,
}

#[derive(PartialEq, Debug)]
pub enum BinaryOperator {
    Eql,
    NotEql,
    Minus,
    Plus,
    Mult,
    Div,
    GT,
    GE,
    LT,
    LE,
}

#[derive(PartialEq, Debug)]
pub enum Primary {
    Bool(bool),
    Nil,
    This,
    Num(f64),
    Str(Ref<String>),
    Name(Ref<String>),
    Super,
}

pub trait AsOpcode {
    fn as_opcode(&self) -> OpCode;
}

impl AsOpcode for UnaryOperator {
    fn as_opcode(&self) -> OpCode {
        match self {
            UnaryOperator::Not => OpCode::Not,
            UnaryOperator::Neg => OpCode::Negate,
        }
    }
}

impl AsOpcode for BinaryOperator {
    fn as_opcode(&self) -> OpCode {
        match self {
            BinaryOperator::Eql => OpCode::Equal,
            BinaryOperator::NotEql => OpCode::NotEqual,
            BinaryOperator::Minus => OpCode::Substract,
            BinaryOperator::Plus => OpCode::Add,
            BinaryOperator::Mult => OpCode::Multiply,
            BinaryOperator::Div => OpCode::Divide,
            BinaryOperator::GT => OpCode::GT,
            BinaryOperator::GE => OpCode::GE,
            BinaryOperator::LT => OpCode::LT,
            BinaryOperator::LE => OpCode::LE,
        }
    }
}
