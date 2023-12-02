use std::collections::HashMap;

use crate::arena::{Arena, Ref};
use crate::chunk::{Class, OwnedLoxVal, RelativeStackIdx, CompiledFn, Upvalue};
use crate::chunk::{Instruction, LoxVal, LoxVal::*, OpCode, ClassInstance, Closure};
use crate::compiler::CompilationResult;

pub struct VM {
    stack: Vec<LocalVar>,
    globals: HashMap<String, LoxVal>,
    last_val: LoxVal,
    call_frames: Vec<CallFrame>,
    heap: Arena<LoxVal>,
    closures: Arena<Closure>,
    functions: Arena<CompiledFn>,
    strings: Arena<String>,
    classes: Arena<Class>,
    instances: Arena<ClassInstance>,
}

enum LocalVar {
    OnStack(LoxVal),
    OnHeap(Ref<LoxVal>),
}

impl From<LoxVal> for LocalVar {
    fn from(val: LoxVal) -> Self {
        LocalVar::OnStack(val)
    }
}

struct CallFrame {
    /// Index of the function in the VM functions field.
    closure: Ref<Closure>,
    ip: usize,
    /// Index of the stack where the frame starts
    /// when calling a function, the value of the function being called is
    /// placed immediately before this index.
    offset: usize,
}

impl From<CompilationResult> for VM {
    fn from(main: CompilationResult) -> Self {
        let main_closure = Closure {
            function: main.main,
            sup: None,
            this: None,
            upvalues: Vec::new(),
        };
        let mut closures = Arena::new();
        let main_closure_ref = closures.insert(main_closure);
        VM {
            stack: Vec::new(),
            globals: HashMap::new(),
            last_val: LoxVal::Nil,
            call_frames: vec![
                CallFrame { closure: main_closure_ref, ip: 0, offset: 0 }
            ],
            heap: Arena::new(),
            functions: main.closures,
            closures,
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
        var_ref: RelativeStackIdx,
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
    UpvalueResolutionBug,
    UpvalueCaptureBug,
    UpvalueRegistrationBug,
}

impl VM {
    fn get_current_instr(&self) -> Result<Option<Instruction>, VMError> {
        let current_fn = self.call_frames[self.call_frames.len() - 1].closure.clone();
        let ip = self.get_current_frame()?.ip;
        let fn_ref = self.closures.get(current_fn).function;
        let chunk = &self.functions.get(fn_ref).chunk;
        Ok(chunk.get(ip).cloned())
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

    fn get_local(&self, var_ref: RelativeStackIdx) -> Result<&LoxVal, VMError> {
        let current_frame = self.get_current_frame()?;
        match self.stack.get(current_frame.offset + var_ref.0) {
            Some(LocalVar::OnStack(val)) => Ok(val),
            Some(LocalVar::OnHeap(heap_ref)) => Ok(self.heap.get(*heap_ref)),
            None => Err(VMError::LocalResolutionBug { var_ref }),
        }

    }

    fn set_local(&mut self, var_ref: RelativeStackIdx, val: LoxVal) -> Result<(), VMError> {
        let current_frame_offset = self.get_current_frame()?.offset;
        match self.stack.get_mut(current_frame_offset + var_ref.0) {
            Some(var@LocalVar::OnStack(_)) => {
                *var = LocalVar::OnStack(val);
                Ok(())
            }
            Some(LocalVar::OnHeap(heap_ref)) => {
                *self.heap.get_mut(*heap_ref) = val;
                Ok(())
            }
            None => Err(VMError::LocalResolutionBug { var_ref }),
        }
    }

    fn register_upval(&mut self, heap_ref: Ref<LoxVal>) -> Result<(), VMError> {
        match self.pop_val()? {
            LoxVal::Closure(c) => {
                self.closures.get_mut(c).upvalues.push(heap_ref);
                self.push_val(LoxVal::Closure(c));
                Ok(())
            }
            LoxVal::Bool(_)
            | LoxVal::Nil
            | LoxVal::Num(_)
            | LoxVal::Str(_)
            | LoxVal::NativeFn(_)
            | LoxVal::Class(_)
            | LoxVal::Instance(_) => Err(VMError::UpvalueRegistrationBug),
        }
    }

    fn move_to_heap(&mut self, stack_pos: RelativeStackIdx) -> Result<Ref<LoxVal>, VMError> {
        let val = self.get_local(stack_pos)?.clone();
        let heap_ref = self.heap.insert(val);
        let current_frame_offset = self.get_current_frame()?.offset;
        match self.stack.get_mut(current_frame_offset + stack_pos.0) {
            Some(x) => {
                *x = LocalVar::OnHeap(heap_ref);
                Ok(heap_ref)
            },
            None => Err(VMError::LocalResolutionBug { var_ref: stack_pos })
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
                            let closure = self.closures.get(closure_ref);
                            let function = self.functions.get(closure.function);
                            if function.arity != n_args {
                                return Err(VMError::IncorrectArgCount {
                                    expected: function.arity,
                                    got: n_args,
                                    line: 0
                                });
                            }
                            self.call_frames.push(CallFrame {
                                closure: closure_ref,
                                ip: 0,
                                offset: self.stack.len() - n_args as usize,
                            });
                        }
                        LoxVal::NativeFn(f) => self.apply_native(f, n_args)?,
                        _ => unreachable!("aaa"),
                    };
                    continue;
                },

                OpCode::CaptureUpvalue(upvalue) => {
                    match upvalue {
                        Upvalue::Local(stack_pos) => {
                            let heap_ref = self.move_to_heap(stack_pos)?;
                            self.register_upval(heap_ref)?;
                        },
                        Upvalue::Parent(upval_idx) => {
                            let closure_ref = self.get_current_frame()?.closure;
                            let closure = self.closures.get(closure_ref);
                            let heap_ref = match closure.upvalues.get(upval_idx) {
                                Some(heap_ref) => heap_ref,
                                None => return Err(VMError::UpvalueCaptureBug),
                            };
                            self.register_upval(*heap_ref)?;
                        }
                    }
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

                OpCode::Closure(function) => {
                    let closure = Closure {
                        function,
                        sup: None,
                        this: None,
                        upvalues: Vec::new(),
                    };
                    let closure_ref = self.closures.insert(closure);
                    self.push_val(LoxVal::Closure(closure_ref));
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
                    let val = self.get_local(var_ref)?;
                    self.push_val(val.clone());
                }
                OpCode::GetSuperMethod(var_ref, name) => {
                    todo!();
                }

                OpCode::GetProperty(prop_name_ref) => {
                    let inst_ref = match self.pop_val()? {
                        LoxVal::Instance(r) => r.clone(),
                        other => return Err(VMError::TypeError {
                            line: 0,
                            expected: "instance".to_string(),
                            got: other.type_name(),
                            details: "can only read properties of class instances".to_string(),
                        }),
                    };
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
                    let current_frame = self.get_current_frame()?;
                    let current_closure = self.closures.get(current_frame.closure);
                    match current_closure.upvalues.get(upval_idx).map(|upval| self.heap.get(*upval)) {
                        Some(val) => self.push_val(val.clone()),
                        None => return Err(VMError::UpvalueResolutionBug),
                    };
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

                OpCode::PopN(n) => {
                    for _ in 0..n {
                        self.pop_val()?;
                    }
                },

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
                    let inst = match self.pop_val()? {
                        LoxVal::Instance(v) => v.clone(),
                        other => return Err(VMError::TypeError {
                            line: 0,
                            expected: "instance".to_string(),
                            got: other.type_name(),
                            details: "can only set properties on class instances".to_string(),
                        }),
                    };
                    let val = self.pop_val()?;
                    let prop_name = self.strings.get(prop_name_ref);
                    self.instances.get_mut(inst).fields.insert(prop_name.clone(), val.clone());
                    self.push_val(val);
                }

                OpCode::SetUpval(upval_idx) => {
                    let new_val = self.pop_val()?;
                    let current_frame = self.get_current_frame()?;
                    let current_closure = self.closures.get(current_frame.closure);
                    match current_closure.upvalues.get(upval_idx).map(|upval| self.heap.get_mut(*upval)) {
                        Some(var) => {
                            *var = new_val;
                        },
                        None => return Err(VMError::UpvalueResolutionBug),
                    };
                }

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
                        let last = match self.stack.pop() {
                            Some(LocalVar::OnStack(v)) => v,
                            Some(LocalVar::OnHeap(heap_ref)) => self.heap.get(heap_ref).clone(),
                            None => self.last_val.clone(),
                        };
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
            Some(LocalVar::OnStack(v)) => Ok(v),
            Some(LocalVar::OnHeap(heap_ref)) => Ok(self.heap.get(heap_ref).clone()),
            _ => Err(VMError::StackExhausted {
                line: 0,
                details: "tried to pop_val() but stack is empty".to_string(),
            }),
        }
    }

    fn pop_var(&mut self) {
        self.stack.pop();
    }

    fn peek(&self, depth: usize) -> Result<&LoxVal, VMError> {
        match self.stack.last() {
            Some(LocalVar::OnStack(v)) => Ok(&v),
            Some(LocalVar::OnHeap(heap_ref)) => Ok(self.heap.get(*heap_ref)),
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
        let mut args = Vec::new();
        for _ in 0..n_args {
            args.push(self.pop_val()?);
        }
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
