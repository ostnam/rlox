use std::collections::HashMap;

use crate::chunk::{Instruction, LoxVal::{self, Num, Str}, OpCode, Function};

pub struct VM {
    functions: Vec<Function>,
    // current_function: usize,
    ip: usize,
    stack: Vec<LoxVal>,
    globals: HashMap<String, LoxVal>,
    last_val: LoxVal,
    // call_frames: Vec<CallFrame>,
}

struct CallFrame {
    function: usize,
    ip: usize,
    slots: Vec<LoxVal>,
}

impl From<Vec<Function>> for VM {
    fn from(functions: Vec<Function>) -> Self {
        VM {
            functions,
            ip: 0,
            stack: Vec::new(),
            globals: HashMap::new(),
            last_val: LoxVal::Nil,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum VMError {
    LocalResolutionBug {
        depth: usize,
    },
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

impl VM {
    pub fn interpret(&mut self) -> Result<LoxVal, VMError> {
        loop {
            let instr = match self.functions[0].chunk.0.get(self.ip) {
                Some(i) => i,
                None => break,
            }.clone();
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
                    (None, None) | (None, Some(_)) | (Some(_), None) => return Err(VMError::stack_exhausted(&instr)),
                }

                OpCode::Constant(c) => self.push_val(c.clone()),

                OpCode::DefineGlobal(name) => match self.pop_val() {
                    Some(val) => {
                        self.globals.insert(name.clone(), val);
                    },
                    None => return Err(VMError::stack_exhausted(&instr)),
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
                    (None, None) | (None, Some(_)) | (Some(_), None) => return Err(VMError::stack_exhausted(&instr)),
                },

                OpCode::Equal => match (self.pop_val(), self.pop_val()) {
                    (Some(rhs), Some(lhs)) => self.push_val(LoxVal::Bool(lhs == rhs)),
                    (None, None) | (None, Some(_)) | (Some(_), None) => return Err(VMError::stack_exhausted(&instr)),
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

                OpCode::GetLocal(depth) => {
                    match self.stack.get(*depth) {
                        Some(val) => self.push_val(val.clone()),
                        None => return Err(VMError::LocalResolutionBug {
                            depth: *depth,
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
                    (None, None) | (None, Some(_)) | (Some(_), None) => return Err(VMError::stack_exhausted(&instr)),
                },

                OpCode::Jump(tgt) => {
                    self.ip = *tgt;
                    continue;  // to avoid ip += 1 at the end of the match
                }

                OpCode::JumpIfFalse(tgt) => {
                    match self.peek(0).map(|v| v.clone().cast_to_bool()) {
                        Some(LoxVal::Bool(false)) => {
                            self.ip = *tgt;
                            continue;
                        }
                        _ => (),
                    }
                },

                OpCode::JumpIfTrue(tgt) => {
                    match self.peek(0).map(|v| v.clone().cast_to_bool()) {
                        Some(LoxVal::Bool(true)) => {
                            self.ip = *tgt;
                            continue;
                        }
                        _ => (),
                    }
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
                    (None, None) | (None, Some(_)) | (Some(_), None) => return Err(VMError::stack_exhausted(&instr)),
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
                    (None, None) | (None, Some(_)) | (Some(_), None) => return Err(VMError::stack_exhausted(&instr)),
                },


                OpCode::Negate => match self.pop_val() {
                    None => return Err(VMError::stack_exhausted(&instr)),
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
                    _ => return Err(VMError::stack_exhausted(&instr)),
                }

                OpCode::Pop => {
                    self.pop_val();
                },

                OpCode::Print => match self.pop_val() {
                    Some(val) => println!("{val}"),
                    _ => return Err(VMError::stack_exhausted(&instr)),
                }

                OpCode::SetGlobal(var) => match self.peek(0) {
                    Some(val) if self.globals.contains_key(var.as_str()) => {
                        self.globals.insert(var.clone(), val.clone());
                    },
                    Some(_) => return Err(VMError::UndefinedVariable {
                        line: 0,
                        name: var.clone(),
                    }),
                    None => return Err(VMError::stack_exhausted(&instr)),
                }

                OpCode::SetLocal(pos) => {
                    if *pos >= self.stack.len() {
                        return Err(
                            VMError::LocalResolutionBug { depth: *pos }
                        );
                    }
                    match self.peek(0) {
                        Some(val) => self.stack[*pos] = val.clone(),
                        None => return Err(VMError::StackExhausted {
                                line: 0,
                                details: format!("Tried to set variable at pos {pos}, but peek returned None."),
                        }),
                    }
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
                    (None, None) | (None, Some(_)) | (Some(_), None) => return Err(VMError::stack_exhausted(&instr)),
                },


                OpCode::Return => return Ok(self.stack.pop().unwrap_or(self.last_val.clone())),
            }

            self.ip += 1;
        };

        Err(VMError::EndedWithNoReturn)
    }

    fn push_val(&mut self, val: LoxVal) {
        self.last_val = val.clone();
        self.stack.push(val);
    }

    fn pop_val(&mut self) -> Option<LoxVal> {
        self.stack.pop()
    }

    fn peek(&self, depth: usize) -> Option<&LoxVal> {
        self.stack.get(self.stack.len() - 1 - depth)
    }
}

