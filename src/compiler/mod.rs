use crate::arena::{Ref, Arena};
use crate::ast::{Function, Declaration, Expr, Stmt, AsOpcode, Primary, ForInit, VarDecl};
use crate::chunk::{Closure, FnType, OpCode, Chunk, Instruction, LoxVal};
use crate::parser::Parse;
use crate::refs_eql;

mod resolver;
use resolver::Resolver;

use self::resolver::StackRef;

pub struct Compiler {
    // Arenas for different objects
    strings: Arena<String>,
    closures: Arena<Closure>,

    resolver: Resolver,
    current_closure: Closure,
    current_closure_type: FnType,
    current_closure_level: ClosureLevel,

    current_line: u64,
}

pub struct CompilationResult {
    pub strings: Arena<String>,
    pub closures: Arena<Closure>,
    pub main: Closure,
}

enum ClosureLevel {
    Nothing,
    Root,
    Children(Closure),
}

/// Wrapper type that represents the target of a jump instruction.
/// The `tgt` field is the index of the instruction in the `Chunk`,
/// so it can be used as the value stored in the OpCode.
#[repr(transparent)]
#[derive(Clone, Copy)]
struct JumpTgt {
    tgt: usize,
}

/// Useful when we add a jump that we'll patch later.
impl Default for JumpTgt {
    fn default() -> Self {
        Self { tgt: 0 }
    }
}

/// Reference to a jump instruction in the current function chunk.
/// `idx` is the direct index in the `Chunk`.
/// Obviously, changing the current function makes all `JumpRef` invalid.
#[repr(transparent)]
#[derive(Clone, Copy)]
struct JumpRef {
    idx: usize,
}

/// Every possible jump type, directly matches to an `OpCode`.
#[derive(Clone, Copy)]
enum JumpKind {
    Inconditional,
    IfTrue,
    IfFalse,
}

impl Compiler {
    pub fn compile(mut input: Parse) -> Option<CompilationResult> {
        let mut closures = Arena::new();
        let name = input.strings.insert("main".to_string());
        let main = Closure {
            arity: 0,
            chunk: Chunk::default(),
            name,
            sup: None,
            this: None,
            upval_idx: Vec::new(),
            child_closures: None,
        };
        let mut compiler = Self {
            strings: input.strings,
            closures,
            resolver: Resolver::default(),
            current_closure: main,
            current_closure_type: FnType::Main,
            current_closure_level: ClosureLevel::Nothing,
            current_line: 0,
        };
        compiler.run_compilation(input.program)
    }

    fn run_compilation(mut self, ast: Vec<Declaration>) -> Option<CompilationResult> {
        self.pass(ast).unwrap();
        Some(CompilationResult {
            strings: self.strings,
            closures: self.closures,
            main: self.current_closure,
        })
    }

    /// Writes an `Instruction` corresponding to the given `OpCode` at
    /// the end of the current function.
    fn emit_instr(&mut self, op: OpCode) {
        let line = self.current_line;
        self.current_closure
            .chunk
            .push(Instruction { op, line });
    }
    /// Writes an error to stdout.
    fn emit_err(&self, err: &str) {
        println!("[{}]: {}", self.current_line, err);
    }

    /// The main compilation pass.
    fn pass(&mut self, nodes: Vec<Declaration>) -> Result<(), ()> {
        for node in &nodes {
            self.compile_decl(node);
        }
        self.emit_instr(OpCode::Return);
        Ok(())
    }

    fn compile_decl(&mut self, decl: &Declaration) {
        match decl {
            Declaration::Class { name, super_name, methods } => {
                self.emit_instr(OpCode::Class(*name));
                if self.resolver.current_scope_depth() > 0 {
                    self.resolver.declare_local(*name);
                    self.resolver.init_last_local();
                } else {
                    self.emit_instr(OpCode::DefineClass(*name));
                }
                if let Some(sup_name_ref) = super_name {
                    if refs_eql!(self.strings, *sup_name_ref, *name) {
                        self.emit_err("class can't inherit from itself");
                    }
                    self.compile_read_var(*sup_name_ref);
                    self.emit_instr(OpCode::Inherit);
                }
                for method in methods {
                    self.compile_method(method);
                }
                if self.resolver.current_scope_depth() == 0 {
                    self.emit_instr(OpCode::Pop);
                }
            },
            Declaration::Fun(f) => self.compile_function(&f),
            Declaration::Stmt(stmt) => self.compile_stmt(&stmt),
            Declaration::Var(var_decl) => self.compile_var_decl(var_decl),
        }
    }

    fn compile_var_decl(&mut self, var_decl: &VarDecl) {
        if self.resolver.current_scope_depth() > 0 {
            self.resolver.declare_local(var_decl.name);
        }
        if let Some(expr) = &var_decl.val {
            self.compile_expr(expr);
        } else {
            self.emit_instr(OpCode::Constant(LoxVal::Nil));
        }
        if self.resolver.current_scope_depth() > 0 {
            self.resolver.init_last_local();
        } else {
            self.emit_instr(OpCode::DefineGlobal(var_decl.name));
        }
    }

    fn compile_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                self.compile_expr(&expr);
                self.emit_instr(OpCode::Pop);
            },

            Stmt::For { init, cond, update, body } =>  {
                self.resolver.begin_scope();
                match init {
                    Some(ForInit::Decl(var_decl)) =>
                        self.compile_var_decl(&var_decl),
                    Some(ForInit::Expr(expr)) =>
                        self.compile_expr(&expr),
                    None => (),
                };
                let mut end_jmp = None;
                let before_cond = self.get_jump_tgt();
                if let Some(expr) = cond {
                    self.compile_expr(&expr);
                    end_jmp = Some(
                        self.emit_jump(JumpKind::IfFalse, JumpTgt::default())
                    );
                    self.emit_instr(OpCode::Pop);
                }
                self.compile_stmt(body);
                if let Some(expr) = update {
                    self.compile_expr(expr);
                    self.emit_jump(JumpKind::Inconditional, before_cond);
                }
                if let Some(jmp) = end_jmp {
                    self.set_jump_tgt(jmp);
                }
                self.emit_instr(OpCode::Pop);
                self.end_scope();
            },

            Stmt::If { cond, body, else_cond } => {
                self.compile_expr(cond);
                let false_jmp = self.emit_jump(JumpKind::IfFalse, JumpTgt::default());
                self.emit_instr(OpCode::Pop);
                self.compile_stmt(body);
                let body_jmp = self.emit_jump(JumpKind::Inconditional, JumpTgt::default());
                self.set_jump_tgt(false_jmp);
                self.emit_instr(OpCode::Pop);
                if let Some(stmt) = else_cond {
                    self.compile_stmt(stmt);
                }
                self.set_jump_tgt(body_jmp);
            },
            Stmt::Print(expr) => {
                self.compile_expr(&expr);
                self.emit_instr(OpCode::Print);
                // The VM pops the value that is printed.
            },
            Stmt::Return(expr) => {
                if let FnType::Main | FnType::Ctor = self.current_closure_type {
                    self.emit_err("can't use return statement in this context");
                }
                match expr {
                    Some(expr) => self.compile_expr(expr),
                    None => self.emit_instr(OpCode::Constant(LoxVal::Nil)),
                }
                self.emit_instr(OpCode::Return);
            },
            Stmt::While { cond, body } => {
                let loop_start = self.get_jump_tgt();
                self.compile_expr(cond);
                let exit_jmp = self.emit_jump(JumpKind::IfFalse, JumpTgt::default());
                self.emit_instr(OpCode::Pop);
                self.compile_stmt(body);
                self.emit_jump(JumpKind::Inconditional, loop_start);
                self.set_jump_tgt(exit_jmp);
                self.emit_instr(OpCode::Pop);
            },
            Stmt::Block(block) => {
                self.resolver.begin_scope();
                for decl in block {
                    self.compile_decl(decl);
                }
                self.end_scope();
            },
        }
    }

    fn compile_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Unop { op, val } => {
                self.compile_expr(val);
                self.emit_instr(op.as_opcode());
            },
            Expr::Binop { op, lhs, rhs } => {
                self.compile_expr(lhs);
                self.compile_expr(rhs);
                self.emit_instr(op.as_opcode());
            },
            Expr::Assignment { tgt, val } => self.compile_assignment(tgt, val),
            Expr::Primary(Primary::Bool(b)) =>
                self.emit_instr(OpCode::Constant(LoxVal::Bool(*b))),
            Expr::Primary(Primary::Nil) =>
                self.emit_instr(OpCode::Constant(LoxVal::Nil)),
            Expr::Primary(Primary::Num(n)) =>
                self.emit_instr(OpCode::Constant(LoxVal::Num(*n))),
            Expr::Primary(Primary::Str(s)) =>
                self.emit_instr(OpCode::Constant(LoxVal::Str(*s))),
            Expr::Primary(Primary::Name(name)) => self.compile_read_var(*name),
            Expr::Primary(Primary::This)
            | Expr::Primary(Primary::Super) => todo!(),
            Expr::Call { lhs, args } => todo!(),
            Expr::And(lhs, rhs) => self.compile_and(lhs, rhs),
            Expr::Or(lhs, rhs) => self.compile_or(lhs, rhs),
            Expr::Dot(_, _) => todo!(),
        }
    }

    /// Compiles Expr::Assignment
    fn compile_assignment(&mut self, tgt: &Expr, val: &Expr) {
        match tgt {
            Expr::Primary(Primary::Name(n)) => {
                self.compile_expr(val);
                match self.resolver.resolve_local(&self.strings, *n) {
                    Some(StackRef::Local(idx)) => self.emit_instr(OpCode::SetLocal(idx)),
                    Some(StackRef::ClosedOver(idx)) => {
                        self.register_upval(idx);
                        self.emit_instr(OpCode::SetUpval(0));
                    },
                    None => self.emit_instr(OpCode::SetGlobal(*n)),
                }
            },
            Expr::Dot(_, _) => todo!(),
            Expr::Primary(_)
            | Expr::Assignment { .. }
            | Expr::Unop { .. }
            | Expr::Binop { .. }
            | Expr::Call { .. }
            | Expr::And(_, _)
            | Expr::Or(_, _) => self.emit_err("invalid assignment target"),
        }
    }

    /// Adds the index of the next instruction of the current function.
    /// As a result, a `{Get,Set}Upval` instr should be emitted next.
    fn register_upval(&mut self, upval_tgt: usize) {
        todo!();
        self.current_closure.upval_idx.push(self.current_closure.chunk.len());
    }

    /// Returns a `JumpTgt` that corresponds to the next instruction.
    fn get_jump_tgt(&self) -> JumpTgt {
        JumpTgt { tgt: self.get_next_instr_idx() }
    }

    /// Emits an instruction corresponding to a jump to the specified target.
    fn emit_jump(&mut self, kind: JumpKind, tgt: JumpTgt) -> JumpRef {
        let res = JumpRef {
            idx: self.get_next_instr_idx()
        };
        match kind {
            JumpKind::Inconditional => self.emit_instr(OpCode::Jump(tgt.tgt)),
            JumpKind::IfTrue => self.emit_instr(OpCode::JumpIfTrue(tgt.tgt)),
            JumpKind::IfFalse => self.emit_instr(OpCode::JumpIfFalse(tgt.tgt)),
        }
        res
    }

    /// Updates the target of the passed-in jump to point to the next
    /// instruction.
    fn set_jump_tgt(&mut self, jmp: JumpRef) {
        let tgt = self.get_next_instr_idx();
        match self.current_closure.chunk.get_mut(jmp.idx) {
            Some(x@Instruction { op: OpCode::Jump(_), .. }) => {
                x.op = OpCode::Jump(tgt);
            },
            Some(x@Instruction { op: OpCode::JumpIfTrue(_), .. }) => {
                x.op = OpCode::JumpIfTrue(tgt);
            },
            Some(x@Instruction { op: OpCode::JumpIfFalse(_), .. }) => {
                x.op = OpCode::JumpIfFalse(tgt);
            },
            _ => self.emit_err("BUG: incorrect jump retargeting"),
        }
    }

    /// Returns the index of the next instruction to be compiled.
    fn get_next_instr_idx(&self) -> usize {
        self.current_closure.chunk.len()
    }

    /// Compiles the instructions to read the variable
    /// (== push it to the stack) with the given `name`,
    /// in the current scope.
    fn compile_read_var(&mut self, name: Ref<String>) {
        match self.resolver.resolve_local(&self.strings, name) {
            Some(StackRef::Local(idx)) => self.emit_instr(OpCode::GetLocal(idx)),
            Some(StackRef::ClosedOver(idx)) => self.emit_instr(OpCode::GetUpval(idx)),
            None => self.emit_instr(OpCode::GetGlobal(name)),
        }
    }

    /// Compiles a method.
    fn compile_method(&mut self, method: &Function) {
        /*
        let meth = self.functions.get(method);
        let compiled_fn = self.compile_function(meth);
        self.emit_instr(OpCode::Method(compiled_fn));
        */
    }

    /// Compiles a function.
    fn compile_function(&mut self, f: &Function) {
        // if we're in a local scope, we need to add the function name
        // as a local variable so that when we compile the body,
        // the name resolves properly.
        if self.resolver.current_scope_depth() > 0 {
            self.resolver.declare_local(f.name);
            self.resolver.init_last_local();
        };
        let new_closure = Closure {
            arity: f.arity,
            chunk: Vec::new(),
            name: f.name,
            this: None,
            sup: None,
            upval_idx: Vec::new(),
            child_closures: None,
        };
        let old_closure = Some(std::mem::replace(&mut self.current_closure, new_closure));
        let (old_closure, old_closure_level) = match &self.current_closure_level {
                ClosureLevel::Nothing => {
                    self.current_closure_level = ClosureLevel::Root;
                    self.current_closure.child_closures = Some(Vec::new());
                    (old_closure, Some(ClosureLevel::Nothing))
                },
                ClosureLevel::Root => {
                    self.current_closure_level = ClosureLevel::Children(old_closure.unwrap());
                    (None, Some(ClosureLevel::Root))

                },
                ClosureLevel::Children(_) => (old_closure, None)
        };
        self.resolver.begin_fn_scope(f);
        for decl in &f.body {
            self.compile_decl(decl);
        }
        self.emit_implicit_return();
        self.resolver.end_fn_scope();
        let old_closure = match (old_closure, old_closure_level) {
            (Some(c), Some(ClosureLevel::Nothing)) => {
                self.current_closure_level = ClosureLevel::Nothing;
                c
            }
            (None, Some(ClosureLevel::Root)) => {
                if let ClosureLevel::Children(c) = std::mem::replace(&mut self.current_closure_level, ClosureLevel::Root) {
                    c
                } else {
                    unreachable!("invariant violated")
                }
            }
            (Some(c), None) => c,
            _ => unreachable!("invariant violated"),
        };
        let new_closure = std::mem::replace(&mut self.current_closure, old_closure);
        let new_closure_name = new_closure.name;
        let new_closure_ref = self.closures.insert(new_closure);
        if self.resolver.current_scope_depth() > 0 {
            self.emit_instr(OpCode::Closure(new_closure_ref));
            self.resolver.declare_local(new_closure_name);
            self.resolver.init_last_local();
        } else {
            self.emit_instr(OpCode::Constant(LoxVal::Closure(new_closure_ref)));
            self.emit_instr(OpCode::DefineGlobal(new_closure_name));
        }
    }

    fn emit_implicit_return(&mut self) {
        self.emit_instr(OpCode::Constant(LoxVal::Nil));
        self.emit_instr(OpCode::Return);
    }

    /// Compiles the "and" operator
    fn compile_and(&mut self, lhs: &Expr, rhs: &Expr) {
        self.compile_expr(lhs);
        let jump = self.emit_jump(JumpKind::IfFalse, JumpTgt::default());
        self.emit_instr(OpCode::Pop);
        self.compile_expr(rhs);
        self.set_jump_tgt(jump);
    }

    /// Compiles the "or" operator
    fn compile_or(&mut self, lhs: &Expr, rhs: &Expr) {
        self.compile_expr(lhs);
        let jump = self.emit_jump(JumpKind::IfTrue, JumpTgt::default());
        self.emit_instr(OpCode::Pop);
        self.compile_expr(rhs);
        self.set_jump_tgt(jump);
    }

    fn end_scope(&mut self) {
        let num_to_pop = self.resolver.end_scope();
        for _ in 0..num_to_pop {
            self.emit_instr(OpCode::Pop);
        }
    }
}
