use crate::{arena::Ref, chunk::OpCode};

pub type Program = Vec<Declaration>;

#[derive(PartialEq, Debug)]
pub enum Declaration {
    Class {
        name: Ref<String>,
        super_name: Option<Ref<String>>,
        methods: Vec<Function>,
    },
    Fun(Function),
    Var(VarDecl),
    Stmt(Stmt),
}

#[derive(PartialEq, Debug)]
pub struct VarDecl {
    pub name: Ref<String>,
    pub val: Option<Expr>,
}

/// `Functions` are AST nodes for a function/method declaration.
/// In the bytecode and at runtime, functions are represented by `Closures`
#[derive(PartialEq, Debug)]
pub struct Function {
    pub name: Ref<String>,
    pub args: Vec<Ref<String>>,
    pub body: Block,
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
    Name(Ref<String>),
    Nil,
    Num(f64),
    Str(Ref<String>),
    Super,
    This,
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
