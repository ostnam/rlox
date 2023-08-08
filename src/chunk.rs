use std::fmt::Display;

#[derive(Clone, PartialEq)]
pub enum LoxVal {
    Bool(bool),
    Nil,
    Num(f64),
    Str(String),
}

impl LoxVal {
    pub fn type_name(&self) -> String {
    match self {
            LoxVal::Bool(_) => "bool",
            LoxVal::Nil     => "nil",
            LoxVal::Num(_)  => "number",
            LoxVal::Str(_)  => "string",
        }.to_string()
    }

    pub fn cast_to_bool(self) -> LoxVal {
        match self {
            LoxVal::Nil | LoxVal::Bool(false) => LoxVal::Bool(false),
            _ => LoxVal::Bool(true),
        }
    }

    pub fn cast_to_not_bool(self) -> LoxVal {
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
            LoxVal::Str(s)  => write!(f, "Str: {s}"),
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
        }
    }
}


#[derive(Debug, PartialEq)]
pub enum OpCode {
    Add,
    Constant(LoxVal),
    DefineGlobal(String),
    Divide,
    Equal,
    Greater,
    Less,
    Multiply,
    Negate,
    Not,
    Pop,
    Print,
    Return,
    Substract,
}

#[derive(Debug, PartialEq)]
pub struct Instruction {
    pub op: OpCode,
    pub line: u64,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = match self {
            OpCode::Add => "ADD".to_string(),
            OpCode::Constant(idx) => format!("CONSTANT {idx}"),
            OpCode::DefineGlobal(name) => format!("DEFGLOBAL {name}"),
            OpCode::Divide => "DIVIDE".to_string(),
            OpCode::Equal => "EQUAL".to_string(),
            OpCode::Greater => "GREATER".to_string(),
            OpCode::Less => "LESS".to_string(),
            OpCode::Multiply => "MULTIPLY".to_string(),
            OpCode::Negate => "NEGATE".to_string(),
            OpCode::Not => "NOT".to_string(),
            OpCode::Pop => "POP".to_string(),
            OpCode::Print => "PRINT".to_string(),
            OpCode::Return => "RETURN".to_string(),
            OpCode::Substract => "SUBSTRACT".to_string(),
        };
        write!(f, "{}", name)
    }
}

#[derive(Debug, PartialEq)]
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
