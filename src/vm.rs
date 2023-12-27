use fnv::FnvHashMap;

use crate::arena::{Arena, Ref, Dynamic, StaticLocked};
use crate::chunk::{Class, OwnedLoxVal, RelativeStackIdx, CompiledFn, Upvalue, new_class_instance, FnType, ClassInstance};
use crate::chunk::{Instruction, LoxVal, LoxVal::*, OpCode, Closure};
use crate::compiler::CompilationResult;

pub struct VM {
    stack: Vec<LocalVar>,
    globals: FnvHashMap<String, LoxVal>,
    last_val: LoxVal,
    call_frames: Vec<CallFrame>,
    heap: Arena<LoxVal, Dynamic>,
    classes: Arena<Class, Dynamic>,
    instances: Arena<ClassInstance, Dynamic>,
    closures: Arena<Closure, Dynamic>,
    strings: Arena<String, Dynamic>,
    functions: Arena<CompiledFn, StaticLocked>,

    /// The first GC cycle will be triggered when the estimated size
    /// of objects exceeds this value.
    gc_threshold: usize,

    /// Estimate of the average size of an object (string, class, instance or
    /// closure), in bytes. Used to trigger GC.
    avg_object_size: usize,
}

#[derive(Clone)]
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

    /// Index of the stack where the frame starts, == index
    /// where the first arg should be.
    offset: usize,
    kind: FnType,
}

impl From<CompilationResult> for VM {
    fn from(main: CompilationResult) -> Self {
        let main_closure = Closure {
            function: main.main,
            sup: None,
            this: None,
            upvalues: Vec::new(),
        };
        let mut closures = Arena::default();
        let main_closure_ref = closures.insert(main_closure);
        VM {
            stack: Vec::new(),
            globals: FnvHashMap::default(),
            last_val: LoxVal::Nil,
            call_frames: vec![
                CallFrame {
                    closure: main_closure_ref,
                    ip: 0,
                    offset: 0,
                    kind: FnType::Main,
                }
            ],
            heap: Arena::default(),
            instances: Arena::default(),
            functions: main.closures.lock(),
            closures,
            strings: main.strings.as_dynamic(),
            classes: Arena::default(),
            gc_threshold: 50_000_000,
            avg_object_size: 500,
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
    ArgsButNoInit,
}

type LoxBuiltinFn = fn(&[LoxVal]) -> Result<LoxVal, VMError>;

impl VM {
    fn get_current_instr(&self) -> Result<Instruction, VMError> {
        let current_frame = self.get_current_frame()?;
        let current_closure = self.closures.get(current_frame.closure);
        let chunk = &self.functions.get(current_closure.function).chunk;
        chunk.get(current_frame.ip).cloned().ok_or(VMError::EndedWithNoReturn)
    }

    fn set_ip(&mut self, tgt: usize) -> Result<(), VMError> {
        let last_frame_idx = self.call_frames.len() - 1;
        match self.call_frames.get_mut(last_frame_idx) {
            Some(frame) => {
                frame.ip = tgt;
                Ok(())
            }
            None => Err(VMError::IncorrectCurrentFunction),
        }
    }

    fn increment_ip(&mut self) -> Result<(), VMError> {
        match self.call_frames.last_mut() {
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
        loop {
            let instr = self.get_current_instr()?;
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

                OpCode::Call(mut n_args) => {
                    self.increment_ip()?;
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
                            let mut this = None;
                            if let Some(t) = closure.this {
                                this = Some(LoxVal::Instance(t));
                                n_args += 1;
                            }
                            let mut super_ = None;
                            if let Some(s) = closure.sup {
                                super_ = Some(LoxVal::Class(s));
                                n_args += 1;
                            }
                            if let Some(v) = this {
                                self.push_val(v);
                            }
                            if let Some(v) = super_ {
                                self.push_val(v);
                            }
                            self.call_frames.push(CallFrame {
                                closure: closure_ref,
                                ip: 0,
                                offset: self.stack.len() - n_args as usize,
                                kind: FnType::Regular,
                            });
                            self.run_gc_if_needed();
                        }
                        LoxVal::NativeFn(f) => self.apply_native(f, n_args)?,
                        LoxVal::Class(class_ref) => {
                            let class = self.classes.get(class_ref);
                            let sup = class.sup;
                            let inst = new_class_instance(class_ref);
                            let inst_ref = self.instances.insert(inst);
                            let init = class.methods.get(&"init".to_string());
                            if init.is_none() && n_args > 0 {
                                return Err(VMError::ArgsButNoInit);
                            }
                            if let Some(init) = init {
                                self.call_frames.push(CallFrame {
                                    closure: *init,
                                    ip: 0,
                                    offset: self.stack.len() - n_args as usize,
                                    kind: FnType::Ctor,
                                });
                                self.push_val(LoxVal::Instance(inst_ref));
                                if let Some(sup) = sup {
                                    self.push_val(LoxVal::Class(sup));
                                }
                            } else {
                                self.push_val(LoxVal::Instance(inst_ref));
                            }
                        },
                        other => return Err(VMError::TypeError {
                            line: instr.line,
                            expected: "callable".to_string(),
                            got: other.type_name(),
                            details: "tried to call non callable value".to_string(),
                        }),
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
                        methods: FnvHashMap::default(),
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
                    let val = self.peek()?.clone();
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
                OpCode::GetSuperMethod(meth_name) => {
                    let cls_ref = match self.pop_val()? {
                        LoxVal::Class(r) => r,
                        other => return Err(VMError::TypeError {
                            line: 0,
                            expected: "class".to_string(),
                            got: other.type_name(),
                            details: "'super' isn't a class".to_string(),
                        }),
                    };
                    let this = match self.pop_val()? {
                        LoxVal::Instance(i) => i,
                        other => return Err(VMError::TypeError {
                            line: 0,
                            expected: "instance".to_string(),
                            got: other.type_name(),
                            details: "BUG".to_string(),
                        }),
                    };
                    let mut super_cls = Some(self.classes.get(cls_ref));
                    let mut found = None;
                    let meth_name = self.strings.get(meth_name);
                    while let Some(cls) = super_cls {
                        if let Some(meth) = cls.methods.get(meth_name) {
                            let og = self.closures.get(*meth).clone();
                            let bound = Closure {
                                this: Some(this),
                                ..og
                            };
                            let bound_ref = self.closures.insert(bound);
                            found = Some(LoxVal::Closure(bound_ref));
                            break;
                        }
                        super_cls = cls.sup.map(|r| self.classes.get(r));
                    }
                    match found {
                        Some(val) => self.push_val(val),
                        None => return Err(VMError::UndefinedProperty {
                            prop_name: meth_name.clone()
                        }),
                    };
                }

                OpCode::GetProperty(prop_name_ref) => {
                    let inst_ref = match self.pop_val()? {
                        LoxVal::Instance(r) => r,
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
                    match (self.pop_val()?, self.peek()?) {
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
                    if let LoxVal::Bool(false) = self.peek()?.cast_to_bool() {
                        self.set_ip(tgt)?;
                        continue;
                    }
                },

                OpCode::JumpIfTrue(tgt) => {
                    if let LoxVal::Bool(true) = self.peek()?.cast_to_bool() {
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
                        LoxVal::Closure(f) => f,
                        other => return Err(VMError::Bug(
                            format!("non-function: {other:?} was on stack in method position during methods declarations"),
                        )),
                    };
                    let cls = match self.peek()? {
                        LoxVal::Class(cls) => *cls,
                        other => return Err(VMError::Bug(
                            format!("non-class: {other:?} was on stack in class position during methods declarations"),
                        )),
                    };
                    let sup = self.classes.get(cls).sup;
                    let closure = self.closures.get_mut(meth);
                    closure.sup = sup;
                    let class = self.classes.get_mut(cls);
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

                OpCode::Pop => {
                    self.pop_val()?;
                },

                OpCode::PopN(n) => {
                    for _ in 0..n {
                        self.pop_val()?;
                    }
                },

                OpCode::Print => println!("{:?}", self.pop_val()?),

                OpCode::SetGlobal(name_ref) => {
                    let name = self.strings.get(name_ref).clone();
                    match self.globals.insert(name.clone(), self.peek()?.clone()) {
                        Some(_) => (),
                        None => return Err(VMError::UndefinedVariable {
                            line: 0,
                            name,
                        }),
                    }
                }

                OpCode::SetLocal(var_ref) => {
                    let val = self.peek()?;
                    self.set_local(var_ref, val.clone())?;
                }

                OpCode::SetProperty(prop_name_ref) => {
                    let val = self.pop_val()?;
                    let inst = match self.pop_val()? {
                        LoxVal::Instance(v) => v,
                        other => return Err(VMError::TypeError {
                            line: 0,
                            expected: "instance".to_string(),
                            got: other.type_name(),
                            details: "can only set properties on class instances".to_string(),
                        }),
                    };
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
                    let old_frame = self.call_frames.pop().unwrap();
                    match old_frame.kind {
                        FnType::Main => {
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
                        },
                        FnType::Regular
                        | FnType::Ctor => {
                            let result = self.pop_val()?;
                            self.stack.truncate(old_frame.offset);
                            self.push_val(result);
                        }
                    }
                    continue; // to not increment the IP twice
                }
            }

            self.increment_ip()?;
        };
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

    fn peek(&self) -> Result<&LoxVal, VMError> {
        match self.stack.last() {
            Some(LocalVar::OnStack(v)) => Ok(v),
            Some(LocalVar::OnHeap(heap_ref)) => Ok(self.heap.get(*heap_ref)),
            None => Err(VMError::StackExhausted {
                line: 0,
                details: format!(
                    "tried to peek but stack.len() == {}",
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

    /// Takes a `LoxVal` and `Arena`s, and moves the value to one of the new `Arena`s.
    /// Calling `move_val` ensures that the value is still valid after GC is finished.
    fn move_val(
        &mut self,
        val: LoxVal,
        new_strings: &mut Arena<String, Dynamic>,
        new_classes: &mut Arena<Class, Dynamic>,
        new_instances: &mut Arena<ClassInstance, Dynamic>,
        new_closures: &mut Arena<Closure, Dynamic>,
        new_heap: &mut Arena<LoxVal, Dynamic>,
    ) -> LoxVal {
        match val {
            Str(ptr) => {
                let (new_ref, _) = self.strings.move_ref(new_strings, ptr);
                LoxVal::Str(new_ref)
            },
            LoxVal::Closure(ptr) => LoxVal::Closure(self.move_closure(
                ptr,
                new_strings,
                new_classes,
                new_instances,
                new_closures,
                new_heap,
            )),
            LoxVal::Class(ptr) => LoxVal::Class(self.move_class(
                ptr,
                new_strings,
                new_classes,
                new_instances,
                new_closures,
                new_heap,
            )),

            Instance(ptr) => LoxVal::Instance(self.move_instance(
                ptr,
                new_strings,
                new_classes,
                new_instances,
                new_closures,
                new_heap,
            )),
            v@Bool(_)
            | v@Nil
            | v@Num(_)
            | v@NativeFn(_) => v,
        }
    }

    /// Move a `ClassInstance` to `new_heap` and moves the value of its fields
    /// so that they are still valid after the arena is dropped.
    fn move_instance(
        &mut self,
        instance: Ref<ClassInstance>,
        new_strings: &mut Arena<String, Dynamic>,
        new_classes: &mut Arena<Class, Dynamic>,
        new_instances: &mut Arena<ClassInstance, Dynamic>,
        new_closures: &mut Arena<Closure, Dynamic>,
        new_heap: &mut Arena<LoxVal, Dynamic>,
    ) -> Ref<ClassInstance> {
        let (new_ref, already_moved) = self.instances.move_ref(new_instances, instance);
        if already_moved {
            return new_ref;
        }
        let instance = new_instances.get(new_ref);
        let new_class = self.move_class(
            instance.class,
            new_strings,
            new_classes,
            new_instances,
            new_closures,
            new_heap
        );

        let mut new_fields = std::mem::take(&mut new_instances.get_mut(new_ref).fields);
        for (_, field) in new_fields.iter_mut() {
            let new_val = self.move_val(
                field.clone(),
                new_strings,
                new_classes,
                new_instances,
                new_closures,
                new_heap,
            );
            *field = new_val;
        }
        let inst = new_instances.get_mut(new_ref);
        inst.class = new_class;
        inst.fields = new_fields;

        new_ref
    }

    /// Move a `Class` to `new_heap` so that it is still valid after the arena
    /// is dropped.
    fn move_class(
        &mut self,
        class: Ref<Class>,
        new_strings: &mut Arena<String, Dynamic>,
        new_classes: &mut Arena<Class, Dynamic>,
        new_instances: &mut Arena<ClassInstance, Dynamic>,
        new_closures: &mut Arena<Closure, Dynamic>,
        new_heap: &mut Arena<LoxVal, Dynamic>,
    ) -> Ref<Class> {
        let (new_ref, already_moved) = self.classes.move_ref(new_classes, class);
        if already_moved {
            return new_ref;
        }
        let cls = new_classes.get(new_ref);
        let (new_name, _) = self.strings.move_ref(new_strings, cls.name);

        let new_sup = cls.sup.map(|sup|
            self.move_class(
                sup,
                new_strings,
                new_classes,
                new_instances,
                new_closures,
                new_heap,
            )
        );

        let mut new_methods = std::mem::take(&mut new_classes.get_mut(new_ref).methods);
        for (_, method) in new_methods.iter_mut() {
            *method = self.move_closure(
                *method,
                new_strings,
                new_classes,
                new_instances,
                new_closures,
                new_heap,
            );
        }
        let cls = new_classes.get_mut(new_ref);
        cls.name = new_name;
        cls.methods = new_methods;
        cls.sup = new_sup;

        new_ref
    }

    /// Move a `Closure` to `new_heap` so that it is still valid after the arena
    /// is dropped.
    fn move_closure(
        &mut self,
        closure: Ref<Closure>,
        new_strings: &mut Arena<String, Dynamic>,
        new_classes: &mut Arena<Class, Dynamic>,
        new_instances: &mut Arena<ClassInstance, Dynamic>,
        new_closures: &mut Arena<Closure, Dynamic>,
        new_heap: &mut Arena<LoxVal, Dynamic>,
    ) -> Ref<Closure> {
        let (new_ref, already_moved) = self.closures.move_ref(new_closures, closure);
        if already_moved {
            return new_ref;
        }

        let closure = new_closures.get(new_ref);
        let new_this = closure.this.map(|ptr|
            self.move_instance(
                ptr,
                new_strings,
                new_classes,
                new_instances,
                new_closures,
                new_heap
            )
        );

        let closure = new_closures.get(new_ref);
        let new_sup = closure.sup.map(|ptr|
            self.move_class(
                ptr,
                new_strings,
                new_classes,
                new_instances,
                new_closures,
                new_heap
            )
        );

        let mut new_upvalues = std::mem::take(&mut new_closures.get_mut(new_ref).upvalues);
        for upval in new_upvalues.iter_mut() {
            let (new_upval, already_moved) = self.heap.move_ref(new_heap, *upval);
            *upval = new_upval;
            if !already_moved {
                let mut upval_val = new_heap.get(new_upval).clone();
                upval_val = self.move_val(
                    upval_val,
                    new_strings,
                    new_classes,
                    new_instances,
                    new_closures,
                    new_heap,
                );
                *new_heap.get_mut(new_upval) = upval_val;
            } 
        }

        let closure = new_closures.get_mut(new_ref);
        closure.this = new_this;
        closure.sup = new_sup;
        closure.upvalues = new_upvalues;

        new_ref
    }


    fn run_gc(&mut self) {
        // Create new arenas
        let mut new_strings = self.strings.next_heap();
        let mut new_classes = self.classes.next_heap();
        let mut new_instances = self.instances.next_heap();
        let mut new_closures = self.closures.next_heap();
        let mut new_heap = self.heap.next_heap();
        let mut new_stack = self.stack.clone();

        // Move values on the stack.
        for val in new_stack.iter_mut() {
            match val {
                LocalVar::OnStack(val) => {
                    let new_val = self.move_val(
                        val.clone(),
                        &mut new_strings,
                        &mut new_classes,
                        &mut new_instances,
                        &mut new_closures,
                        &mut new_heap
                    );
                    *val = new_val;
                }
                LocalVar::OnHeap(heap_ref) => {
                    let (new_heap_ref, already_moved) = self.heap.move_ref(&mut new_heap, *heap_ref);
                    *heap_ref = new_heap_ref;
                    if !already_moved {
                        let mut val = new_heap.get(new_heap_ref).clone();
                        val = self.move_val(
                            val,
                            &mut new_strings,
                            &mut new_classes,
                            &mut new_instances,
                            &mut new_closures,
                            &mut new_heap,
                        );
                        *new_heap.get_mut(new_heap_ref) = val;
                    }
                }
            }
        }

        // Move global variables
        let mut temp = std::mem::take(&mut self.globals);
        for (_, val) in temp.iter_mut() {
            *val = self.move_val(
                val.clone(),
                &mut new_strings,
                &mut new_classes,
                &mut new_instances,
                &mut new_closures,
                &mut new_heap,
            );
        }
        std::mem::swap(&mut self.globals, &mut temp);

        // Move closures on the callstack
        let mut temp = std::mem::take(&mut self.call_frames);
        for frame in temp.iter_mut() {
            frame.closure = self.move_closure(
                frame.closure,
                &mut new_strings,
                &mut new_classes,
                &mut new_instances,
                &mut new_closures,
                &mut new_heap,
            );
        }
        std::mem::swap(&mut self.call_frames, &mut temp);

        // Set the new arena
        self.strings = new_strings;
        self.classes = new_classes;
        self.instances = new_instances;
        self.closures = new_closures;
        self.heap = new_heap;
        self.stack = new_stack;

        // update gc_threshold
        self.gc_threshold = self.heap_size() * 2;
    }

    /// Checks whether a garbage collection is needed, and triggers it
    /// if so.
    /// Can only be safely called at the beginning of a LoxVal function call,
    /// just after the new callframe has been pushed.
    fn run_gc_if_needed(&mut self) {
        if self.heap_size() > self.gc_threshold {
            self.run_gc();
        }
    }

    /// Returns an estimate of the number of the cumulative size of every
    /// heap-allocated value.
    fn heap_size(&self) -> usize {
        ( self.strings.len()
        + self.classes.len()
        + self.instances.len()
        + self.closures.len()
        + self.heap.len()
        + self.globals.len()
        ) * self.avg_object_size
    }
}

fn native_clock(_args: &[LoxVal]) -> Result<LoxVal, VMError> {
    let now = std::time::SystemTime::now();
    let since_unix = now.duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_millis();

    Ok(LoxVal::Num(since_unix as f64))
}

fn get_native(name: &str) -> Option<LoxBuiltinFn> {
    match name {
        "clock" => Some(native_clock),
        _ => None
    }
}
