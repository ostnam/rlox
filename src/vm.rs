use std::collections::HashMap;

use crate::chunk::{Chunk, Instruction, LoxVal::{self, Num, Str}, OpCode};

pub struct VM<'a> {
    chunk: &'a Chunk,
    ip: usize,
    stack: Vec<LoxVal>,
    globals: HashMap<&'a str, LoxVal>,
    last_val: LoxVal,
}

impl<'a> From<&'a Chunk> for VM<'a> {
    fn from(chunk: &'a Chunk) -> Self {
        VM {
            chunk,
            ip: 0,
            stack: Vec::new(),
            globals: HashMap::new(),
            last_val: LoxVal::Nil,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum VMError {
    EndedWithNoReturn,
    StackExhausted {
        line: u64,
        details: String, 
    },
    UndefinedVariable {
        line: u64,
        name: String,
    },
    TypeError {
        line: u64,
        expected: String,
        got: String,
        details: String,
    },
}

impl VMError {
    fn stack_exhausted(instruction: &Instruction) -> Self {
        Self::StackExhausted{
            line: instruction.line,
            details: format!("for instruction: {}", instruction.op),
        }
    }
}

impl<'a> VM<'a> {
    pub fn interpret(&mut self) -> Result<LoxVal, VMError> {
        for instr in &self.chunk.0 {
            match &instr.op {
                OpCode::Add => match (self.pop_val(), self.pop_val()) {
                    (Some(Num(r)), Some(Num(l))) => self.push_val(Num(l+r)),
                    (Some(Str(r)), Some(Str(l))) => self.push_val(Str(l + &r)),
                    (Some(Num(_)), Some(other))  => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "The + operator can either be used to add number or concatenate strings".to_string(),
                    }),
                    (Some(Str(_)), Some(other))  => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "string".to_string(),
                        got: other.type_name(),
                        details: "The + operator can either be used to add number or concatenate strings".to_string(),
                    }),
                    (Some(other), Some(_)) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number or string".to_string(),
                        got: other.type_name(),
                        details: "The + operator can either be used to add number or concatenate strings".to_string(),
                    }),
                    (None, None) | (None, Some(_)) | (Some(_), None) => return Err(VMError::stack_exhausted(instr)),
                }

                OpCode::Constant(c) => self.push_val(c.clone()),

                OpCode::DefineGlobal(name) => match self.pop_val() {
                    Some(val) => {
                        self.globals.insert(name, val);
                    },
                    None => return Err(VMError::stack_exhausted(instr)),
                },

                OpCode::Divide => match (self.pop_val(), self.pop_val()) {
                    (Some(Num(r)), Some(Num(l))) => self.push_val(Num(l/r)),
                    (Some(Num(_)), Some(other)) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the right side of the / operator".to_string(),
                    }),
                    (Some(other), Some(_)) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the left side of the / operator".to_string(),
                    }),
                    (None, None) | (None, Some(_)) | (Some(_), None) => return Err(VMError::stack_exhausted(instr)),
                },

                OpCode::Equal => match (self.pop_val(), self.pop_val()) {
                    (Some(rhs), Some(lhs)) => self.push_val(LoxVal::Bool(lhs == rhs)),
                    (None, None) | (None, Some(_)) | (Some(_), None) => return Err(VMError::stack_exhausted(instr)),
                }

                OpCode::GetGlobal(var) => {
                    match self.globals.get(var.as_str()) {
                        Some(val) => self.push_val(val.clone()),
                        None => return Err(VMError::UndefinedVariable {
                            name: var.clone(),
                            line: instr.line,
                        }),
                    }
                }

                OpCode::Greater => match (self.pop_val(), self.pop_val()) {
                    (Some(Num(r)), Some(Num(l))) => self.push_val(LoxVal::Bool(l>r)),
                    (Some(Num(_)), Some(other)) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the right side of the > operator".to_string(),
                    }),
                    (Some(other), Some(_)) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the left side of the > operator".to_string(),
                    }),
                    (None, None) | (None, Some(_)) | (Some(_), None) => return Err(VMError::stack_exhausted(instr)),
                },
                OpCode::Less => match (self.pop_val(), self.pop_val()) {
                    (Some(Num(r)), Some(Num(l))) => self.push_val(LoxVal::Bool(l<r)),
                    (Some(Num(_)), Some(other)) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the right side of the < operator".to_string(),
                    }),
                    (Some(other), Some(_)) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the left side of the < operator".to_string(),
                    }),
                    (None, None) | (None, Some(_)) | (Some(_), None) => return Err(VMError::stack_exhausted(instr)),
                },

                OpCode::Multiply => match (self.pop_val(), self.pop_val()) {
                    (Some(Num(r)), Some(Num(l))) => self.push_val(Num(l*r)),
                    (Some(Num(_)), Some(other)) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the right side of the * operator".to_string(),
                    }),
                    (Some(other), Some(_)) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the left side of the * operator".to_string(),
                    }),
                    (None, None) | (None, Some(_)) | (Some(_), None) => return Err(VMError::stack_exhausted(instr)),
                },


                OpCode::Negate => match self.pop_val() {
                    None => return Err(VMError::stack_exhausted(instr)),
                    Some(Num(n)) => self.push_val(Num(-n)),
                    Some(other) => return Err(VMError::TypeError{
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "the negate operator only takes numbers".to_string(),
                    }),
                }

                OpCode::Not => match self.pop_val() {
                    Some(val) => self.push_val(val.cast_to_not_bool()),
                    _ => return Err(VMError::stack_exhausted(instr)),
                }

                OpCode::Pop => {
                    self.pop_val();
                },

                OpCode::Print => match self.pop_val() {
                    Some(val) => println!("{val}"),
                    _ => return Err(VMError::stack_exhausted(instr)),
                }

                OpCode::SetGlobal(var) => match self.pop_val() {
                    Some(val) if self.globals.contains_key(var.as_str()) => {
                        self.globals.insert(var.as_str(), val);
                    },
                    Some(_) => return Err(VMError::UndefinedVariable {
                        line: 0,
                        name: var.clone(),
                    }),
                    None => return Err(VMError::stack_exhausted(instr)),
                }

                OpCode::Substract => match (self.pop_val(), self.pop_val()) {
                    (Some(Num(r)), Some(Num(l))) => self.push_val(Num(l-r)),
                    (Some(Num(_)), Some(other)) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the right side of the - operator".to_string(),
                    }),
                    (Some(other), Some(_)) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the left side of the - operator".to_string(),
                    }),
                    (None, None) | (None, Some(_)) | (Some(_), None) => return Err(VMError::stack_exhausted(instr)),
                },


                OpCode::Return => return Ok(self.stack.pop().unwrap_or(self.last_val.clone())),
            }
        };

        Err(VMError::EndedWithNoReturn)
    }

    fn advance(&mut self) -> Option<&Instruction> {
        let res = self.chunk.0.get(self.ip);
        self.ip += 1;
        res
    }

    fn push_val(&mut self, val: LoxVal) {
        self.last_val = val.clone();
        self.stack.push(val);
    }

    fn pop_val(&mut self) -> Option<LoxVal> {
        self.stack.pop()
    }

    fn peek(&mut self, depth: usize) -> Option<&LoxVal> {
        self.stack.get(self.stack.len() - 1 - depth)
    }
}

