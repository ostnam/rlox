use std::collections::HashMap;

use crate::arena::Arena;
use crate::chunk::Class;
use crate::chunk::{Instruction, LoxVal::{self, Num, Str}, OpCode, Function, Callable, LocalVarRef, ClassInstance};

pub struct VM {
    stack: Vec<LocalVar>,
    globals: HashMap<String, LoxVal>,
    last_val: LoxVal,
    call_frames: Vec<CallFrame>,
    ref_resolver: Vec<RefStatus>,
    classes: Arena<Class>,
    instances: Arena<ClassInstance>,
}

struct LocalVar {
    val: LoxVal,
    upval_idx: Option<usize>,
}

enum RefStatus {
    OnStack(usize),
    OnHeap(LoxVal),
}

impl From<LoxVal> for LocalVar {
    fn from(val: LoxVal) -> Self {
        LocalVar { val, upval_idx: None }
    }
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
            stack: Vec::new(),
            globals: HashMap::new(),
            last_val: LoxVal::Nil,
            call_frames: vec![
                CallFrame { function: main, ip: 0, offset: 0 }
            ],
            ref_resolver: Vec::new(),
            classes: Arena::new(),
            instances: Arena::new(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum VMError {
    LocalResolutionBug {
        var_ref: LocalVarRef,
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
    },
    UndefinedProperty {
        prop_name: String,
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
    fn get_current_instr(&self) -> Result<Option<Instruction>, VMError> {
        let current_fn = self.call_frames[self.call_frames.len() - 1].function.clone();
        let ip = self.get_current_frame()?.ip;
        Ok(current_fn.chunk.0.get(ip).cloned())
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
            None => Err(VMError::IncorrectCurrentFunction),
        }
    }

    fn get_current_frame(&self) -> Result<&CallFrame, VMError> {
        match self.call_frames.last() {
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

    fn get_local(&self, var_ref: &LocalVarRef) -> Result<Option<&LoxVal>, VMError> {
        let current_frame = self.get_current_frame()?;
        Ok(self.stack.get(current_frame.offset + var_ref.pos).map(|x| &x.val))
    }

    fn set_local(&mut self, var_ref: &LocalVarRef, val: LoxVal) -> Result<(), VMError> {
        let current_frame_offset = self.get_current_frame_mut()?.offset;
        self.stack[current_frame_offset + var_ref.pos].val = val;
        Ok(())
    }

    /// Reads every instruction of a function, to modify accesses to
    /// closed-over variables, into a Get/SetUpval instruction.
    fn resolve_closure(&mut self, closure: &mut Function) {
        let current_frame = self.call_frames.len();
        for instr in closure.chunk.0.iter_mut() {
            match &mut instr.op {
                OpCode::GetLocal(var) if var.frame != current_frame => {
                    let upval_idx = self.register_upval(var);
                    instr.op = OpCode::GetUpval(upval_idx);
                }
                OpCode::SetLocal(var) if var.frame != current_frame  => {
                    let upval_idx = self.register_upval(var);
                    instr.op = OpCode::SetUpval(upval_idx);
                }
                OpCode::Closure(f) => {
                    self.resolve_closure(f);
                    instr.op = OpCode::Constant(LoxVal::Function(f.clone()));
                }
                _ => continue,
            }
        }
    }

    /// Registers a new UpVal.
    /// The pointed-to value's `is_closed_over` field will be set to `true`,
    /// and the index of the upval returned.
    fn register_upval(&mut self, var_ref: &LocalVarRef) -> usize {
        let var_frame_offset = self.call_frames[var_ref.frame].offset;
        let var_stack_idx = var_frame_offset + var_ref.pos;
        self.ref_resolver.push(
            RefStatus::OnStack(var_stack_idx)
        );
        let upval_idx = self.ref_resolver.len() - 1;
        self.stack[var_stack_idx].upval_idx = Some(upval_idx);
        upval_idx
    }

    /// Reads the variable with the upval index, from the stack or the heap.
    fn read_upval(&self, ref_idx: usize) -> LoxVal {
        match &self.ref_resolver[ref_idx] {
            RefStatus::OnStack(idx) => self.stack[*idx].val.clone(),
            RefStatus::OnHeap(value) => value.clone(),
        }
    }

    /// Sets the variable with the upval index, on the stack or the heap.
    fn set_upval(&mut self, ref_idx: usize, val: LoxVal) {
        match self.ref_resolver.get_mut(ref_idx).unwrap() {
            RefStatus::OnStack(idx) => self.stack[*idx].val = val,
            v@RefStatus::OnHeap(_) => *v = RefStatus::OnHeap(val),
        }
    }

    fn get_called_fn(&self, n_args: u8) -> Result<Callable, VMError> {
        let last_arg_idx = self.stack.len() - 1;
        let fn_idx = last_arg_idx - n_args as usize;
        match self.stack.get(fn_idx).map(|x| x.val.clone()) {
            Some(LoxVal::Function(f)) => Ok(Callable::Function(f.clone())),
            Some(LoxVal::NativeFunction(f)) => Ok(Callable::NativeFunction(f)),
            Some(LoxVal::Class(cls)) => Ok(Callable::Class(cls)),
            Some(other) => Err(VMError::TypeError {
                line: 0,
                expected: "callable".to_string(),
                got: other.type_name(),
                details: "can't call non-function".to_string(),
            }),
            None => Err(VMError::StackExhausted {
                line: 0,
                details: String::from("stack exhausted trying to get the function being called."),
            }),
        }
    }

    pub fn interpret(&mut self) -> Result<LoxVal, VMError> {
        while let Some(instr) = self.get_current_instr()? {
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
                    let mut frame_added = false;
                    match self.get_called_fn(n_args)? {
                        Callable::Function(f) => {
                            frame_added = true;
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
                        Callable::Class(cls) => {
                            self.pop_val();
                            let class = self.classes.get(cls);
                            let inst_ref = self.instances.insert(class.new_instance());
                            self.push_val(
                                LoxVal::Instance(inst_ref)
                            );
                        },
                        Callable::NativeFunction(f) => {
                            self.apply_native(f, n_args)?;
                        }
                    };
                    let prev_fn_idx = if frame_added {
                        self.call_frames.len() - 2
                    } else {
                        self.call_frames.len() - 1
                    };
                    match self.call_frames.get_mut(prev_fn_idx) {
                        Some(frame) => frame.ip += 1,
                        None => (),
                    };
                    continue;
                },

                OpCode::Class(name) => {
                    let class = Class {
                        name,
                        methods: HashMap::new(),
                    };
                    let class_ref = self.classes.insert(class);
                    self.push_val(LoxVal::Class(class_ref));
                }

                OpCode::Closure(mut f) => {
                    self.resolve_closure(&mut f);
                    self.push_val(LoxVal::Function(f));
                }

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

                OpCode::GetLocal(var_ref) => {
                    match self.get_local(&var_ref)? {
                        Some(val) => self.push_val(val.clone()),
                        None => return Err(VMError::LocalResolutionBug {
                            var_ref,
                        }),
                    }
                }

                OpCode::GetProperty(prop_name) => {
                    let inst_ref = match self.peek(0)? {
                        LoxVal::Instance(r) => r.clone(),
                        other => return Err(VMError::TypeError {
                            line: 0,
                            expected: "instance".to_string(),
                            got: other.type_name(),
                            details: "can only read properties of class instances".to_string(),
                        }),
                    };
                    self.pop_var();
                    match self.instances.get(inst_ref).fields.get(&prop_name) {
                        Some(val) => {
                            self.push_val(val.clone());
                        }
                        None => return Err(VMError::UndefinedProperty { prop_name }),
                    };
                }

                OpCode::GetUpval(upval_idx) => {
                    self.push_val(self.read_upval(upval_idx))
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
                    if let LoxVal::Bool(false) = self.peek(0)?.cast_to_bool() {
                        self.set_ip(tgt)?;
                        continue;
                    }
                },

                OpCode::JumpIfTrue(tgt) => {
                    if let LoxVal::Bool(true) = self.peek(0)?.cast_to_bool() {
                        self.set_ip(tgt)?;
                        continue;
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

                OpCode::Pop => self.pop_var(),

                OpCode::Print => match self.pop_val() {
                    Some(val) => println!("{val}"),
                    _ => return Err(VMError::stack_exhausted(&instr)),
                }

                OpCode::SetGlobal(ref var) => {
                    match self.globals.insert(var.clone(), self.peek(0)?.clone()) {
                        Some(_) => (),
                        None => return Err(VMError::UndefinedVariable {
                            line: 0,
                            name: var.clone(),
                        }),
                    }
                }

                OpCode::SetLocal(var_ref) => {
                    let val = self.peek(0)?;
                    self.set_local(&var_ref, val.clone())?;
                }

                OpCode::SetProperty(prop_name) => {
                    let inst = match self.peek(1)? {
                        LoxVal::Instance(v) => v.clone(),
                        other => return Err(VMError::TypeError {
                            line: 0,
                            expected: "instance".to_string(),
                            got: other.type_name(),
                            details: "can only set properties on class instances".to_string(),
                        }),
                    };
                    let val = match self.pop_val() {
                        Some(v) => v,
                        None => return Err(VMError::StackExhausted {
                                line: 0,
                                details: String::new(),
                        }),
                    };
                    self.pop_val();
                    self.instances.get_mut(inst).fields.insert(prop_name, val.clone());
                    self.push_val(val);
                }

                OpCode::SetUpval(upval_idx) => self.set_upval(upval_idx, self.peek(0)?.clone()),

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
                        return Ok(self.stack.pop().map(|x| x.val).unwrap_or(self.last_val.clone()));
                    }
                    let result = self.pop_val().unwrap();
                    let old_frame = self.call_frames.pop().unwrap();
                    for _ in old_frame.offset..self.stack.len() {
                        self.pop_var();
                    }
                    self.pop_val();
                    self.push_val(result);
                    continue;
                }
            }

            self.increment_ip()?;
        };

        Err(VMError::EndedWithNoReturn)
    }

    fn push_val(&mut self, val: LoxVal) {
        self.last_val = val.clone();
        self.stack.push(val.into());
    }

    fn pop_val(&mut self) -> Option<LoxVal> {
        self.stack.pop().map(|x| x.val)
    }

    fn pop_var(&mut self) {
        match self.stack.pop() {
            Some(LocalVar { upval_idx: Some(idx), val }) => {
                self.ref_resolver[idx] = RefStatus::OnHeap(val);
            }
            _ => (),
        }
    }

    fn peek(&self, depth: usize) -> Result<&LoxVal, VMError> {
        match self.stack.get(self.stack.len() - 1 - depth) {
            Some(v) => Ok(&v.val),
            None => Err(VMError::StackExhausted {
                line: 0,
                details: format!(
                    "tried to peek {depth} values deep but stack.len() == {}",
                    self.stack.len(),
                ),
            }),
        }
    }

    fn apply_native(
        &mut self,
        f: fn(&[LoxVal]) -> Result<LoxVal, VMError>,
        n_args: u8
    ) -> Result<(), VMError> {
        let frame_end = self.stack.len();
        let frame_start = frame_end - n_args as usize;
        let args: Vec<LoxVal> = self.stack[frame_start..frame_end]
            .iter()
            .map(|x| x.val.clone())
            .collect();
        let result = f(args.as_slice())?;
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
