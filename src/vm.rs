use crate::chunk::{Chunk, Instruction, LoxVal::{self, Num, Str}, OpCode};

type Value = ();

pub struct VM<'a> {
    chunk: &'a Chunk,
    ip: usize,
    stack: Vec<LoxVal>,
}

pub enum VMError {
    EndedWithNoReturn,
    StackExhausted {
        line: u64,
        details: String, 
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
    fn interpret(&mut self, chunk: &Chunk) -> Result<(), VMError> {
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

                OpCode::Substract => todo!(),

                OpCode::Return => {
                    if let Some(_) = self.pop_val() {
                        return Ok(())
                    }
                    return Err(VMError::stack_exhausted(instr));
                },
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
        self.stack.push(val);
    }

    fn pop_val(&mut self) -> Option<LoxVal> {
        self.stack.pop()
    }
}

