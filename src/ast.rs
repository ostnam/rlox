use crate::arena::Ref;

pub type Program = Vec<Declaration>;

pub enum Declaration {
    Class {
        name: Ref<String>,
        super_name: Ref<String>,
        methods: Vec<Function>,
    },
    Fun(Function),
    Var {
        name: Ref<String>,
        val: Option<Expr>,
    },
    Stmt(Stmt),
}

pub struct Function {
    pub params: Vec<Ref<String>>,
    pub body: Block,
}

pub type Block = Vec<Declaration>;

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
    Block,
}

pub enum ForInit {
    Decl {
        name: Ref<String>,
        val: Option<Expr>,
    },
    Expr(Expr),
}

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
    Call(Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Dot(Box<Expr>, Box<Expr>),
}

pub enum UnaryOperator {
    Not,
    Neg,
}

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

pub enum Primary {
    Bool(bool),
    Nil,
    This,
    Num(f64),
    Str(Ref<String>),
    Name(Ref<String>),
    Super(Option<String>),
}
