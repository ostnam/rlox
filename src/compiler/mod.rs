use crate::arena::{Ref, Arena};
use crate::ast::{Function, Declaration, Expr, Stmt, AsOpcode, Primary, ForInit, VarDecl};
use crate::chunk::{CompiledFn, FnType, OpCode, Chunk, Instruction, LoxVal};
use crate::parser::Parse;
use crate::refs_eql;

mod resolver;
use resolver::Resolver;

use self::resolver::{StackRef, ScopeId};

pub struct Compiler {
    // Arenas for different objects
    strings: Arena<String>,
    closures: Arena<CompiledFn>,

    resolver: Resolver,
    current_closure: Ref<CompiledFn>,
    current_closure_type: FnType,
    current_parent_closure: Option<Ref<CompiledFn>>,

    current_line: u64,
    had_error: bool,

    this: Ref<String>,
    super_: Ref<String>,
}

#[derive(Debug)]
pub struct CompilationResult {
    pub strings: Arena<String>,
    pub closures: Arena<CompiledFn>,
    pub main: Ref<CompiledFn>,
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
        let main = CompiledFn {
            arity: 0,
            chunk: Chunk::default(),
            name,
        };
        let main_ref = closures.insert(main);
        let mut strings = input.strings;
        let this = strings.insert("this".to_string());
        let super_ = strings.insert("super".to_string());
        let compiler = Self {
            strings,
            closures,
            resolver: Resolver::default(),
            current_closure: main_ref,
            current_closure_type: FnType::Main,
            current_parent_closure: None,
            current_line: 0,
            had_error: false,
            this,
            super_,
        };
        compiler.run_compilation(input.program)
    }

    fn run_compilation(mut self, ast: Vec<Declaration>) -> Option<CompilationResult> {
        self.pass(ast).unwrap();
        if self.had_error {
            None
        } else {
            Some(CompilationResult {
                strings: self.strings,
                closures: self.closures,
                main: self.current_closure,
            })
        }
    }

    /// Writes an `Instruction` corresponding to the given `OpCode` at
    /// the end of the current function.
    fn emit_instr(&mut self, op: OpCode) {
        let line = self.current_line;
        self.closures.get_mut(self.current_closure)
            .chunk
            .push(Instruction { op, line });
    }
    /// Writes an error to stdout.
    fn emit_err(&mut self, err: &str) {
        self.had_error = true;
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
                    if let Err(e) = self.resolver.declare_local(&self.strings, *name) {
                        self.emit_err(&e.to_string());
                    }
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
                    self.compile_function(method, true);
                }
                if self.resolver.current_scope_depth() == 0 {
                    self.emit_instr(OpCode::Pop);
                }
            },
            Declaration::Fun(f) => self.compile_function(&f, false),
            Declaration::Stmt(stmt) => self.compile_stmt(&stmt),
            Declaration::Var(var_decl) => self.compile_var_decl(var_decl),
        }
    }

    fn compile_var_decl(&mut self, var_decl: &VarDecl) {
        if self.resolver.current_scope_depth() > 0 {
            if let Err(e) = self.resolver.declare_local(&self.strings, var_decl.name) {
                self.emit_err(&e.to_string());
            }
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
                let scope = self.resolver.begin_scope();
                match init {
                    Some(ForInit::Decl(var_decl)) =>
                        self.compile_var_decl(&var_decl),
                    Some(ForInit::Expr(expr)) => {
                        self.compile_expr(&expr);
                        self.emit_instr(OpCode::Pop);
                    }
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
                    self.emit_instr(OpCode::Pop);
                    self.emit_jump(JumpKind::Inconditional, before_cond);
                }
                if let Some(jmp) = end_jmp {
                    self.set_jump_tgt(jmp);
                }
                self.emit_instr(OpCode::Pop);
                self.end_scope(scope);
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
            Stmt::Return(Some(_)) if self.current_closure_type == FnType::Ctor => self.emit_err("can't return value from init"),
            Stmt::Return(None) if self.current_closure_type == FnType::Ctor => self.emit_implicit_return(),
            Stmt::Return(expr) => {
                match self.current_closure_type {
                    FnType::Main => self.emit_err("can't use return statement in this context"),
                    FnType::Method
                    | FnType::Regular => (),
                    FnType::Ctor => unreachable!("BUG: improper pattern matching for Stmt::Return in constructors"),
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
                let scope = self.resolver.begin_scope();
                for decl in block {
                    self.compile_decl(decl);
                }
                self.end_scope(scope);
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
            Expr::Primary(Primary::This) => match self.resolver.resolve(&self.strings, self.this) {
                    Some(StackRef::Local(idx)) => self.emit_instr(OpCode::GetLocal(idx)),
                    Some(StackRef::Upval(idx)) => self.emit_instr(OpCode::GetUpval(idx)),
                    None => self.emit_err("used keyword 'this' outside of method"),
            },
            Expr::Primary(Primary::Super) =>
                match self.resolver.resolve(&self.strings, self.this) {
                    Some(StackRef::Local(idx)) => self.emit_instr(OpCode::GetLocal(idx)),
                    Some(StackRef::Upval(idx)) => self.emit_instr(OpCode::GetUpval(idx)),
                    None => self.emit_err("used keyword 'super' outside of method"),
                },
            Expr::Call { lhs, args } => {
                for arg in args {
                    self.compile_expr(arg);
                }
                self.compile_expr(lhs);
                self.emit_instr(OpCode::Call(args.len() as u8));
            }
            Expr::And(lhs, rhs) => self.compile_and(lhs, rhs),
            Expr::Or(lhs, rhs) => self.compile_or(lhs, rhs),
            Expr::Dot(lhs, rhs) => {
                self.compile_expr(lhs);
                self.emit_instr(OpCode::GetProperty(*rhs));
            }
        }
    }

    /// Compiles Expr::Assignment
    fn compile_assignment(&mut self, tgt: &Expr, val: &Expr) {
        match tgt {
            Expr::Primary(Primary::Name(n)) => {
                self.compile_expr(val);
                match self.resolver.resolve(&self.strings, *n) {
                    Some(StackRef::Local(idx)) => self.emit_instr(OpCode::SetLocal(idx)),
                    Some(StackRef::Upval(upval)) => {
                        self.emit_instr(OpCode::SetUpval(upval));
                    },
                    None => self.emit_instr(OpCode::SetGlobal(*n)),
                }
            },
            Expr::Dot(lhs, prop) => {
                self.compile_expr(lhs);
                self.compile_expr(val);
                self.emit_instr(OpCode::SetProperty(*prop));
            },
            Expr::Primary(_)
            | Expr::Assignment { .. }
            | Expr::Unop { .. }
            | Expr::Binop { .. }
            | Expr::Call { .. }
            | Expr::And(_, _)
            | Expr::Or(_, _) => self.emit_err("invalid assignment target"),
        }
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
        match self.closures.get_mut(self.current_closure).chunk.get_mut(jmp.idx) {
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
        self.closures.get(self.current_closure).chunk.len()
    }

    /// Compiles the instructions to read the variable
    /// (== push it to the stack) with the given `name`,
    /// in the current scope.
    fn compile_read_var(&mut self, name: Ref<String>) {
        match self.resolver.resolve(&self.strings, name) {
            Some(StackRef::Local(idx)) => self.emit_instr(OpCode::GetLocal(idx)),
            Some(StackRef::Upval(idx)) => {
                self.emit_instr(OpCode::GetUpval(idx));
            },
            None => self.emit_instr(OpCode::GetGlobal(name)),
        }
    }

    /// Compiles a function.
    fn compile_function(
        &mut self,
        f: &Function,
        is_method: bool,
    ) {
        // if we're in a local scope, we need to add the function name
        // as a local variable so that when we compile the body,
        // the name resolves properly.
        if !is_method && self.resolver.current_scope_depth() > 0 {
            if let Err(e) = self.resolver.declare_local(&self.strings, f.name) {
                self.emit_err(&e.to_string());
            }
            self.resolver.init_last_local();
        };
        let arity = match f.args.len().try_into() {
            Ok(a) => a,
            _ => {
                self.emit_err("Function takes {} parameters (maximum: 255).");
                0
            },
        };

        let new_closure = CompiledFn {
            arity,
            chunk: Vec::new(),
            name: f.name,
        };
        let new_closure_name = new_closure.name;
        let new_closure_ref = self.closures.insert(new_closure);
        let parent_closure = std::mem::replace(&mut self.current_closure, new_closure_ref);
        let grandparent_closure = std::mem::replace(&mut self.current_parent_closure, Some(parent_closure));
        let scope = self.resolver.begin_fn_scope(&self.strings, &f.args);
        if is_method {
            if let Err(e) = self.resolver.declare_local(&self.strings, self.this) {
                self.emit_err(&format!("BUG when compiling method declaration, {e}"));
            }
            self.resolver.init_last_local();
        }
        let new_closure_type = match self.strings.get(f.name).as_str() {
            "init" if is_method => FnType::Ctor,
            _ if is_method => FnType::Method,
            _ => FnType::Regular,
        };
        let old_fn_type = std::mem::replace(&mut self.current_closure_type, new_closure_type);
        for decl in &f.body {
            self.compile_decl(decl);
        }
        self.emit_implicit_return();
        let captured = match self.resolver.end_fn_scope(scope) {
            Ok(upvalues) => upvalues,
            Err(e) => {
                self.emit_err(&e.to_string());
                Vec::new()
            }
        };
        let parent_closure = std::mem::replace(&mut self.current_parent_closure, grandparent_closure).unwrap();
        self.current_closure = parent_closure;
        self.current_closure_type = old_fn_type;
        if is_method {
            self.emit_instr(OpCode::Closure(new_closure_ref));
            for upvalue in captured {
                self.emit_instr(OpCode::CaptureUpvalue(upvalue));
            }
            self.emit_instr(OpCode::Method(f.name));
        } else if self.resolver.current_scope_depth() > 0 {
            self.emit_instr(OpCode::Closure(new_closure_ref));
            for upvalue in captured {
                self.emit_instr(OpCode::CaptureUpvalue(upvalue));
            }
            self.resolver.init_last_local();
        } else {
            self.emit_instr(OpCode::Closure(new_closure_ref));
            self.emit_instr(OpCode::DefineGlobal(new_closure_name));
        }
    }

    fn emit_implicit_return(&mut self) {
        match self.current_closure_type {
            FnType::Regular
            | FnType::Method => self.emit_instr(OpCode::Constant(LoxVal::Nil)),
            FnType::Ctor => {
                if let Some(StackRef::Local(idx)) = self.resolver.resolve(&self.strings, self.this) {
                self.emit_instr(OpCode::GetLocal(idx));
                } else {
                self.emit_err("BUG: this is not a local var");
                }
            }

            FnType::Main => self.emit_err("BUG: emitting implicit return for main"),
        };
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

    fn end_scope(&mut self, id: ScopeId) {
        match self.resolver.end_scope(id) {
            Ok(num_to_pop) => {
                if let Ok(u) = num_to_pop.try_into() {
                    self.emit_instr(OpCode::PopN(u));
                } else {
                    for _ in 0..num_to_pop {
                        self.emit_instr(OpCode::Pop);
                    }
                }
            }
            Err(e) => self.emit_err(&e.to_string()),
        }
    }
}
