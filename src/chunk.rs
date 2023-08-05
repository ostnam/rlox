use std::fmt::Display;

#[derive(Clone, Debug, PartialEq)]
pub enum LoxVal {
    Num(f64),
    Str(String),
}

impl LoxVal {
    pub fn type_name(&self) -> String {
    match self {
            LoxVal::Num(_) => "number",
            LoxVal::Str(_) => "string",
        }.to_string()
    }
}

impl Display for LoxVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxVal::Num(n) => write!(f, "Num: {}", n),
            LoxVal::Str(s) => write!(f, "Str: {}", s),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum OpCode {
    Add,
    Constant(LoxVal),
    Divide,
    Multiply,
    Negate,
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
            OpCode::Divide => "DIVIDE".to_string(),
            OpCode::Multiply => "MULTIPLY".to_string(),
            OpCode::Negate => "NEGATE".to_string(),
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
