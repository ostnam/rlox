use std::collections::HashMap;

use crate::arena::{Arena, Ref};
use crate::chunk::{Class,  new_class_instance, OwnedLoxVal};
use crate::chunk::{Instruction, LoxVal, LoxVal::*, OpCode, Callable, ClassInstance, Closure};
use crate::compiler::CompilationResult;

pub struct VM {
    stack: Vec<LocalVar>,
    globals: HashMap<String, LoxVal>,
    last_val: LoxVal,
    call_frames: Vec<CallFrame>,
    ref_resolver: Vec<RefStatus>,
    closures: Arena<Closure>,
    strings: Arena<String>,
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
    function: Ref<Closure>,
    ip: usize,
    /// Index of the stack where the frame starts
    /// when calling a function, the value of the function being called is
    /// placed immediately before this index.
    offset: usize,
}

impl From<CompilationResult> for VM {
    fn from(mut main: CompilationResult) -> Self {
        let main_ref = main.closures.insert(main.main);
        VM {
            stack: Vec::new(),
            globals: HashMap::new(),
            last_val: LoxVal::Nil,
            call_frames: vec![
                CallFrame { function: main_ref, ip: 0, offset: 0 }
            ],
            ref_resolver: Vec::new(),
            closures: main.closures,
            strings: main.strings,
            classes: Arena::new(),
            instances: Arena::new(),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum VMError {
    Bug(String),
    LocalResolutionBug {
        var_ref: usize,
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
    SuperNoSuper,
}

impl VM {
    fn get_current_instr(&self) -> Result<Option<Instruction>, VMError> {
        let current_fn = self.call_frames[self.call_frames.len() - 1].function.clone();
        let ip = self.get_current_frame()?.ip;
        Ok(self.closures.get(current_fn).chunk.get(ip).cloned())
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

    fn get_local(&self, var_ref: usize) -> Result<Option<&LoxVal>, VMError> {
        let current_frame = self.get_current_frame()?;
        Ok(self.stack.get(current_frame.offset + var_ref).map(|x| &x.val))

    }

    fn set_local(&mut self, var_ref: usize, val: LoxVal) -> Result<(), VMError> {
        self.stack[var_ref].val = val;
        Ok(())
    }

    /// Reads every instruction of a function, to modify accesses to
    /// closed-over variables, into a Get/SetUpval instruction.
    fn resolve_closure(&mut self, closure_ref: Ref<Closure>) {
        /*
        let mut closure = self.functions.get(closure_ref);
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
                    self.resolve_closure(*f);
                    instr.op = OpCode::Closure(f.clone());
                }
                _ => continue,
            }
        }
        */
        todo!()
    }

    /// Registers a new UpVal, the index of the upval returned.
    fn register_upval(&mut self, var_ref: usize) -> usize {
        self.ref_resolver.push(
            RefStatus::OnStack(var_ref)
        );
        let upval_idx = self.ref_resolver.len() - 1;
        self.stack[var_ref].upval_idx = Some(upval_idx);
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
            Some(LoxVal::Closure(f)) => Ok(Callable::Closure(f)),
            Some(LoxVal::NativeFn(f)) => Ok(Callable::NativeFn(f)),
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

    pub fn interpret(&mut self) -> Result<OwnedLoxVal, VMError> {
        while let Some(instr) = self.get_current_instr()? {
            match instr.op {
                OpCode::Add => match (self.pop_val()?, self.pop_val()?) {
                    (Num(r), Num(l)) => self.push_val(Num(l+r)),
                    (Str(r), Str(l)) => {
                        let mut res = self.strings.get(l).clone();
                        res.push_str(self.strings.get(r));
                        let res_ref = self.strings.insert(res);
                        self.push_val(Str(res_ref));
                    }
                    (Num(_), other)  => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "The + operator can either be used to add number or concatenate strings".to_string(),
                    }),
                    (Str(_), other)  => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "string".to_string(),
                        got: other.type_name(),
                        details: "The + operator can either be used to add number or concatenate strings".to_string(),
                    }),
                    (other, _) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number or string".to_string(),
                        got: other.type_name(),
                        details: "The + operator can either be used to add number or concatenate strings".to_string(),
                    }),
                }

                OpCode::Call(n_args) => {
                    match self.call_frames.last_mut() {
                        Some(frame) => frame.ip += 1,
                        None => (),
                    };
                    match self.pop_val()? {
                        LoxVal::Closure(closure_ref) => {
                            let f = self.closures.get(closure_ref);
                            if f.arity != n_args {
                                return Err(VMError::IncorrectArgCount {
                                    expected: f.arity,
                                    got: n_args,
                                    line: 0
                                });
                            }
                            self.call_frames.push(CallFrame {
                                function: closure_ref,
                                ip: 0,
                                offset: self.stack.len() - n_args as usize,
                            });
                        }
                        _ => unreachable!("aaa"),
                    };
                    continue;
                },

                OpCode::Class(name) => {
                    let class = Class {
                        name,
                        methods: HashMap::new(),
                        sup: None,
                    };
                    let class_ref = self.classes.insert(class);
                    self.push_val(LoxVal::Class(class_ref));
                }

                OpCode::Closure(f) => {
                    self.resolve_closure(f);
                    self.push_val(LoxVal::Closure(f));
                }

                OpCode::Constant(c) => self.push_val(c.clone()),

                OpCode::DefineClass(name) => {
                    let val = self.peek(0)?.clone();
                    self.globals.insert(self.strings.get(name).clone(), val);
                },

                OpCode::DefineGlobal(name) => {
                    let val = self.pop_val()?.clone();
                    self.globals.insert(self.strings.get(name).clone(), val);
                },

                OpCode::Divide => match (self.pop_val()?, self.pop_val()?) {
                    (Num(r), Num(l)) => self.push_val(Num(l/r)),
                    (Num(_), other) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the left side of the / operator".to_string(),
                    }),
                    (other, _) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the right side of the / operator".to_string(),
                    }),
                },

                OpCode::Equal => {
                    let lhs = self.pop_val()?;
                    let rhs = self.pop_val()?;
                    self.push_val(LoxVal::Bool(lhs == rhs));
                },

                OpCode::NotEqual => {
                    let lhs = self.pop_val()?;
                    let rhs = self.pop_val()?;
                    self.push_val(LoxVal::Bool(!(lhs == rhs)));
                },

                OpCode::GetGlobal(var) => {
                    let var_str = self.strings.get(var);
                    match self.globals.get(var_str) {
                        Some(val) => self.push_val(val.clone()),
                        None => match get_native(var_str) {
                            Some(f) => self.push_val(LoxVal::NativeFn(f)),
                            None => return Err(VMError::UndefinedVariable {
                                name: var_str.clone(),
                                line: instr.line,
                            })
                        },
                    }
                }

                OpCode::GetLocal(var_ref) => {
                    match self.get_local(var_ref)? {
                        Some(val) => self.push_val(val.clone()),
                        None => return Err(VMError::LocalResolutionBug {
                            var_ref,
                        }),
                    }
                }
                OpCode::GetSuperMethod(var_ref, name) => {
                    let supercls_ref = match self.get_local(var_ref)? {
                        Some(LoxVal::Class(val)) => val,
                        Some(LoxVal::Nil) => return Err(VMError::SuperNoSuper),
                        Some(_) => return Err(VMError::Bug(
                            "error getting super".to_string()
                        )),
                        None => return Err(VMError::LocalResolutionBug {
                            var_ref,
                        }),
                    };
                    let supercls = self.classes.get(*supercls_ref);
                    let this_pos = var_ref - 1;
                    let this = match self.get_local(this_pos)? {
                        Some(LoxVal::Instance(r)) => r,
                        _ => return Err(VMError::Bug(
                            "Error getting this".to_string(),
                        )),
                    };
                    let meth_name = self.strings.get(name);
                    match supercls.methods.get(meth_name) {
                        Some(f) => {
                            let f_closure = self.closures.get(*f).clone();
                            let method = Closure {
                                this: Some(*this),
                                sup: supercls.sup,
                                ..f_closure
                            };
                            let meth_ref = self.closures.insert(method);
                            self.push_val(LoxVal::Closure(meth_ref));
                        }
                        None => return Err(VMError::UndefinedProperty {
                            prop_name: meth_name.clone(),
                        }),
                    }
                }

                OpCode::GetProperty(prop_name_ref) => {
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
                    let inst = self.instances.get(inst_ref);
                    let cls = self.classes.get(inst.class);
                    let prop_name = self.strings.get(prop_name_ref);
                    match inst.fields.get(prop_name) {
                        Some(val) => {
                            self.push_val(val.clone());
                        }
                        None => match cls.methods.get(prop_name) {
                            Some(f) => {
                                let f_closure = self.closures.get(*f).clone();
                                let method = Closure {
                                    this: Some(inst_ref),
                                    sup: cls.sup,
                                    ..f_closure
                                };
                                let meth_ref = self.closures.insert(method);
                                self.push_val(LoxVal::Closure(meth_ref));
                            },
                            None => return Err(VMError::UndefinedProperty {
                                prop_name: prop_name.clone()
                            }),
                        }
                    };
                }

                OpCode::GetUpval(upval_idx) => {
                    self.push_val(self.read_upval(upval_idx))
                }

                OpCode::GT => match (self.pop_val()?, self.pop_val()?) {
                    (Num(r), Num(l)) => self.push_val(LoxVal::Bool(l>r)),
                    (Num(_), other) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the left side of the > operator".to_string(),
                    }),
                    (other, _) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the right side of the > operator".to_string(),
                    }),
                },

                OpCode::GE => match (self.pop_val()?, self.pop_val()?) {
                    (Num(r), Num(l)) => self.push_val(LoxVal::Bool(l >= r)),
                    (Num(_), other) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the left side of the >= operator".to_string(),
                    }),
                    (other, _) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the right side of the >= operator".to_string(),
                    }),
                },


                // stack: |  super  |  <--- top
                //        |   sub   |
                //  As we are in a class definition, the superclass will be
                //  popped so that the subclass remains on top of the stack.
                OpCode::Inherit => {
                    match (self.pop_val()?, self.peek(0)?) {
                        (LoxVal::Class(sup), &LoxVal::Class(sub)) => {
                            self.classes.get_mut(sub).sup = Some(sup);
                            let inherited = self.classes.get(sup).methods.clone();
                            self.classes.get_mut(sub).methods = inherited;
                        }
                        (other, _) => return Err(VMError::TypeError {
                            line: 0,
                            expected: "class".to_string(),
                            got: other.type_name(),
                            details: "can only inherit from a class".to_string(),
                        }),
                    }
                }

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

                OpCode::LT => match (self.pop_val()?, self.pop_val()?) {
                    (Num(r), Num(l)) => self.push_val(LoxVal::Bool(l < r)),
                    (Num(_), other) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the left side of the < operator".to_string(),
                    }),
                    (other, _) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the right side of the < operator".to_string(),
                    }),
                },

                OpCode::LE => match (self.pop_val()?, self.pop_val()?) {
                    (Num(r), Num(l)) => self.push_val(LoxVal::Bool(l <= r)),
                    (Num(_), other) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the left side of the <= operator".to_string(),
                    }),
                    (other, _) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the right side of the <= operator".to_string(),
                    }),
                },

                OpCode::Method(name_ref) => {
                    let name = self.strings.get(name_ref).clone();
                    let meth = match self.pop_val()? {
                        LoxVal::Closure(f) => f.clone(),
                        other => return Err(VMError::Bug(
                            format!("non-function: {other:?} was on stack in method position during methods declarations"),
                        )),
                    };
                    let cls = match self.peek(0)? {
                        LoxVal::Class(cls) => cls,
                        other => return Err(VMError::Bug(
                            format!("non-class: {other:?} was on stack in class position during methods declarations"),
                        )),
                    };
                    let class = self.classes.get_mut(*cls);
                    class.methods.insert(name, meth);
                },

                OpCode::Multiply => match (self.pop_val()?, self.pop_val()?) {
                    (Num(r), Num(l)) => self.push_val(Num(l*r)),
                    (Num(_), other) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the left side of the * operator".to_string(),
                    }),
                    (other, _) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the right side of the * operator".to_string(),
                    }),
                },

                OpCode::Negate => match self.pop_val()? {
                    Num(n) => self.push_val(Num(-n)),
                    other => return Err(VMError::TypeError{
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "the negate operator only takes numbers".to_string(),
                    }),
                }

                OpCode::Not => {
                    let val = self.pop_val()?.cast_to_not_bool();
                    self.push_val(val);
                }

                OpCode::Pop => self.pop_var(),

                OpCode::Print => println!("{:?}", self.pop_val()?),

                OpCode::SetGlobal(name_ref) => {
                    let name = self.strings.get(name_ref).clone();
                    match self.globals.insert(name.clone(), self.peek(0)?.clone()) {
                        Some(_) => (),
                        None => return Err(VMError::UndefinedVariable {
                            line: 0,
                            name,
                        }),
                    }
                }

                OpCode::SetLocal(var_ref) => {
                    let val = self.peek(0)?;
                    self.set_local(var_ref, val.clone())?;
                }

                OpCode::SetProperty(prop_name_ref) => {
                    let inst = match self.peek(1)? {
                        LoxVal::Instance(v) => v.clone(),
                        other => return Err(VMError::TypeError {
                            line: 0,
                            expected: "instance".to_string(),
                            got: other.type_name(),
                            details: "can only set properties on class instances".to_string(),
                        }),
                    };
                    let val = self.pop_val()?;
                    self.pop_val()?;
                    let prop_name = self.strings.get(prop_name_ref);
                    self.instances.get_mut(inst).fields.insert(prop_name.clone(), val.clone());
                    self.push_val(val);
                }

                OpCode::SetUpval(upval_idx) => self.set_upval(upval_idx, self.peek(0)?.clone()),

                OpCode::Substract => match (self.pop_val()?, self.pop_val()?) {
                    (Num(r), Num(l)) => self.push_val(Num(l-r)),
                    (Num(_), other) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the left side of the - operator".to_string(),
                    }),
                    (other, _) => return Err(VMError::TypeError {
                        line: instr.line,
                        expected: "number".to_string(),
                        got: other.type_name(),
                        details: "on the right side of the - operator".to_string(),
                    }),
                },


                OpCode::Return => {
                    if self.call_frames.len() == 1 {
                        let last = self.stack.pop().map(|x| x.val).unwrap_or(self.last_val.clone());
                        return Ok(match last {
                            Bool(b) => OwnedLoxVal::Bool(b),
                            Nil => OwnedLoxVal::Nil,
                            Num(n) => OwnedLoxVal::Num(n),
                            Str(s) => OwnedLoxVal::Str(
                                self.strings.get(s).clone()
                            ),
                            LoxVal::Closure(c) => OwnedLoxVal::Closure(
                                self.closures.get(c).clone(),
                            ),
                            NativeFn(f) => OwnedLoxVal::NativeFn(f),
                            LoxVal::Class(c) => OwnedLoxVal::Class(
                                self.classes.get(c).clone()
                            ),
                            Instance(i) => OwnedLoxVal::Instance(
                                self.instances.get(i).clone()
                            ),
                        });
                    }
                    let result = self.pop_val().unwrap();
                    let old_frame = self.call_frames.pop().unwrap();
                    self.stack.truncate(old_frame.offset);
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

    fn pop_val(&mut self) -> Result<LoxVal, VMError> {
        match self.stack.pop() {
            Some(v) => Ok(v.val),
            _ => Err(VMError::StackExhausted {
                line: 0,
                details: "tried to pop_val() but stack is empty".to_string(),
            }),
        }
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
