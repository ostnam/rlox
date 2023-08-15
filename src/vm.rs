use std::collections::HashMap;

use crate::chunk::{Instruction, LoxVal::{self, Num, Str}, OpCode, Function, Callable};

pub struct VM {
    main: Function,
    stack: Vec<LoxVal>,
    globals: HashMap<String, LoxVal>,
    last_val: LoxVal,
    call_frames: Vec<CallFrame>,
}

struct CallFrame {
    /// Index of the function in the VM functions field.
    function: Function,
    ip: usize,
    /// Index of the stack where the frame starts
    /// when calling a function, the value of the function being called is
    /// placed immediately before this index.
    offset: usize,
}

impl From<Function> for VM {
    fn from(main: Function) -> Self {
        VM {
            main: main.clone(),
            stack: Vec::new(),
            globals: HashMap::new(),
            last_val: LoxVal::Nil,
            call_frames: vec![
                CallFrame { function: main.clone(), ip: 0, offset: 0 }
            ],
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
    IncorrectCurrentFunction,
    IncorrectArgCount {
        expected: u8,
        got: u8,
        line: u64,
    }
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
    fn get_current_instr(&self) -> Result<Option<Instruction>, VMError> {
        let current_fn = self.call_frames[self.call_frames.len() - 1].function.clone();
        let ip = self.get_current_frame()?.ip;
        Ok(current_fn.chunk.0.get(ip).map(|instr| instr.clone()))
    }

    fn set_ip(&mut self, tgt: usize) -> Result<(), VMError> {
        let last_frame_idx = self.call_frames.len() - 1;
        match self.call_frames.get_mut(last_frame_idx) {
            Some(frame) => {
                frame.ip = tgt;
                Ok(())
            }
            None => return Err(VMError::IncorrectCurrentFunction),
        }
    }

    fn increment_ip(&mut self) -> Result<(), VMError> {
        let last_frame_idx = self.call_frames.len() - 1;
        match self.call_frames.get_mut(last_frame_idx) {
            Some(frame) => {
                frame.ip += 1;
                Ok(())
            }
            None => return Err(VMError::IncorrectCurrentFunction),
        }
    }

    fn get_current_frame(&self) -> Result<&CallFrame, VMError> {
        match self.call_frames.get(self.call_frames.len() - 1) {
            Some(frame) => Ok(frame),
            None => Err(VMError::IncorrectCurrentFunction),
        }
    }

    fn get_current_frame_mut(&mut self) -> Result<&mut CallFrame, VMError> {
        let last_frame_idx = self.call_frames.len() - 1;
        match self.call_frames.get_mut(last_frame_idx) {
            Some(frame) => Ok(frame),
            None => Err(VMError::IncorrectCurrentFunction),
        }
    }

    fn get_local(&self, idx: usize) -> Result<Option<&LoxVal>, VMError> {
        let current_frame = self.get_current_frame()?;
        Ok(self.stack.get(current_frame.offset + idx))
    }

    fn set_local(&mut self, idx: usize, val: LoxVal) -> Result<(), VMError> {
        let current_frame_offset = self.get_current_frame_mut()?.offset;
        self.stack[current_frame_offset + idx] = val;
        Ok(())
    }

    fn get_called_fn(&self, n_args: u8) -> Result<Callable, VMError> {
        let last_arg_idx = self.stack.len() - 1;
        let fn_idx = last_arg_idx - n_args as usize;
        match self.stack.get(fn_idx) {
            Some(LoxVal::Function(f)) => Ok(Callable::Function(f.clone())),
            Some(LoxVal::NativeFunction(f)) => Ok(Callable::NativeFunction(*f)),
            Some(other) => Err(VMError::TypeError {
                line: 0,
                expected: "callable".to_string(),
                got: other.type_name(),
                details: "can't call non-function".to_string(),
            }),
            None => Err(VMError::StackExhausted {
                line: 0,
                details: format!("stack exhausted trying to get the function being called."),
            }),
        }
    }

    pub fn interpret(&mut self) -> Result<LoxVal, VMError> {
        loop {
            let instr = match self.get_current_instr()? {
                Some(instr) => instr,
                None => break,
            };
            match instr.op {
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

                OpCode::Call(n_args) => {
                    let mut native_called = false;
                    match self.get_called_fn(n_args)? {
                        Callable::Function(f) => {
                            if f.arity != n_args {
                                return Err(VMError::IncorrectArgCount {
                                    expected: f.arity,
                                    got: n_args,
                                    line: 0
                                });
                            }
                            self.call_frames.push(CallFrame {
                                function: f.clone(),
                                ip: 0,
                                offset: self.stack.len() - n_args as usize,
                            });
                        },
                        Callable::NativeFunction(f) => {
                            native_called = true;
                            self.apply_native(f, n_args)?;
                        }
                    };
                    let prev_fn_idx = if native_called {
                        self.call_frames.len() - 1
                    } else {
                        self.call_frames.len() - 1
                    };
                    match self.call_frames.get_mut(prev_fn_idx) {
                        Some(frame) => frame.ip += 1,
                        None => (),
                    };
                    continue;
                },

                OpCode::Constant(c) => self.push_val(c.clone()),

                OpCode::DefineGlobal(ref name) => match self.pop_val() {
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
                        details: "on the left side of the / operator".to_string(),
                    }),
                    (Some(other), Some(_)) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the right side of the / operator".to_string(),
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
                        None => match get_native(&var) {
                            Some(f) => self.push_val(LoxVal::NativeFunction(f)),
                            None => return Err(VMError::UndefinedVariable {
                                name: var.clone(),
                                line: instr.line,
                            })
                        },
                    }
                }

                OpCode::GetLocal(depth) => {
                    match self.get_local(depth)? {
                        Some(val) => self.push_val(val.clone()),
                        None => return Err(VMError::LocalResolutionBug {
                            depth,
                        }),
                    }
                }

                OpCode::Greater => match (self.pop_val(), self.pop_val()) {
                    (Some(Num(r)), Some(Num(l))) => self.push_val(LoxVal::Bool(l>r)),
                    (Some(Num(_)), Some(other)) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the left side of the > operator".to_string(),
                    }),
                    (Some(other), Some(_)) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the right side of the > operator".to_string(),
                    }),
                    (None, None) | (None, Some(_)) | (Some(_), None) => return Err(VMError::stack_exhausted(&instr)),
                },

                OpCode::Jump(tgt) => {
                    self.set_ip(tgt)?;
                    continue;  // to avoid ip += 1 at the end of the match
                }

                OpCode::JumpIfFalse(tgt) => {
                    match self.peek(0).map(|v| v.clone().cast_to_bool()) {
                        Some(LoxVal::Bool(false)) => {
                            self.set_ip(tgt)?;
                            continue;
                        }
                        _ => (),
                    }
                },

                OpCode::JumpIfTrue(tgt) => {
                    match self.peek(0).map(|v| v.clone().cast_to_bool()) {
                        Some(LoxVal::Bool(true)) => {
                            self.set_ip(tgt);
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
                        details: "on the left side of the < operator".to_string(),
                    }),
                    (Some(other), Some(_)) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the right side of the < operator".to_string(),
                    }),
                    (None, None) | (None, Some(_)) | (Some(_), None) => return Err(VMError::stack_exhausted(&instr)),
                },

                OpCode::Multiply => match (self.pop_val(), self.pop_val()) {
                    (Some(Num(r)), Some(Num(l))) => self.push_val(Num(l*r)),
                    (Some(Num(_)), Some(other)) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the left side of the * operator".to_string(),
                    }),
                    (Some(other), Some(_)) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the right side of the * operator".to_string(),
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

                OpCode::SetGlobal(ref var) => match self.peek(0) {
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
                    match self.peek(0) {
                        Some(val) => self.set_local(pos, val.clone())?,
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
                        details: "on the left side of the - operator".to_string(),
                    }),
                    (Some(other), Some(_)) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the right side of the - operator".to_string(),
                    }),
                    (None, None) | (None, Some(_)) | (Some(_), None) => return Err(VMError::stack_exhausted(&instr)),
                },


                OpCode::Return => {
                    if self.call_frames.len() == 1 {
                        return Ok(self.stack.pop().unwrap_or(self.last_val.clone()));
                    }
                    let result = self.pop_val().unwrap();
                    let old_frame = self.call_frames.pop().unwrap();
                    let frame_start_idx = old_frame.offset - 1;
                    self.stack.truncate(frame_start_idx);
                    self.stack.push(result);
                }
            }

            self.increment_ip()?;
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

    fn apply_native(
        &mut self,
        f: fn(&[LoxVal]) -> Result<LoxVal, VMError>,
        n_args: u8
    ) -> Result<(), VMError> {
        let frame_end = self.stack.len();
        let frame_start = frame_end - n_args as usize;
        let result = f(&self.stack[frame_start..frame_end])?;
        self.stack.truncate(frame_start);
        self.push_val(result);
        Ok(())
    }
}

fn native_clock(_args: &[LoxVal]) -> Result<LoxVal, VMError> {
    let now = std::time::SystemTime::now();
    let since_unix = now.duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_millis();

    Ok(LoxVal::Num(since_unix as f64))
}

fn get_native(name: &str) -> Option<fn(&[LoxVal]) -> Result<LoxVal, VMError>> {
    match name {
        "clock" => Some(native_clock),
        _ => None
    }
}
