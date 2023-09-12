use crate::arena::{Ref, Arena};
use crate::ast::{Function, Declaration, Expr, Stmt, AsOpcode, Primary, ForInit, VarDecl};
use crate::chunk::{Closure, FnType, OpCode, Chunk, Instruction, LoxVal, LocalVarRef};
use crate::parser::Parse;
use crate::refs_eql;

pub struct Compiler {
    // Arenas for different objects
    functions: Arena<Function>,
    strings: Arena<String>,
    closures: Arena<Closure>,

    // for resolving variables
    locals: Vec<Vec<Local>>,
    current_closure: Ref<Closure>,
    current_closure_type: FnType,

    executable: Vec<Ref<Closure>>,
    current_line: u64,
}

#[derive(Clone, Debug)]
struct Local {
    name: Ref<String>,
    depth: usize,
    initialized: bool,
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

pub struct CompilationResult {
    pub strings: Arena<String>,
    pub functions: Arena<Closure>,
    pub executable: Vec<Ref<Closure>>,
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
            child_closures: Vec::new(),
        };
        let main_ref = closures.insert(main);
        let mut compiler = Self {
            functions: input.functions,
            strings: input.strings,
            closures,
            locals: Vec::new(),
            current_closure: main_ref,
            current_closure_type: FnType::Main,
            executable: Vec::new(),
            current_line: 0,
        };
        compiler.executable.push(main_ref);
        compiler.run_compilation(input.program)
    }

    fn run_compilation(mut self, ast: Vec<Declaration>) -> Option<CompilationResult> {
        self.optimize_ast();
        self.pass(ast).unwrap();
        Some(CompilationResult {
            strings: self.strings,
            functions: self.closures,
            executable: self.executable,
        })
    }

    fn optimize_ast(&mut self) {
    }

    /// Returns a reference to the function currently being compiled.
    fn get_current_fn(&self) -> &Closure {
        self.closures.get(self.current_closure)
    }

    /// Returns a `&mut` to the function currently being compiled.
    fn get_current_fn_mut(&mut self) -> &mut Closure {
        self.closures.get_mut(self.current_closure)
    }

    /// Writes an `Instruction` corresponding to the given `OpCode` at
    /// the end of the current function.
    fn emit_instr(&mut self, op: OpCode) {
        let line = self.current_line;
        self.get_current_fn_mut()
            .chunk.push(Instruction { op, line });
    }

    /// Registers a new local in the current scope.
    /// If there are no valid scopes currently, does nothing.
    fn declare_local(&mut self, name: Ref<String>) {
        let depth = self.current_scope_depth();
        self.locals.last_mut().map_or_else(
            || {
                dbg!("Failed to declare local");
            },
            |frame|
                frame.push(Local {
                    name,
                    depth,
                    initialized: false,
        }))
    }

    /// The last declared local's `initialized` value is set to `true`.
    fn init_last_local(&mut self) {
        self.locals
            .last_mut()
            .and_then(|frame| frame.last_mut())
            .map_or_else(
                || {
                    dbg!("Couldn't init_last_local: no local available");
                },
                |local| local.initialized = true
        );
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
                if self.current_scope_depth() > 0 {
                    self.declare_local(*name);
                    self.init_last_local();
                } else {
                    self.emit_instr(OpCode::DefineClass(*name));
                }
                if let Some(sup_name_ref) = super_name {
                    if refs_eql!(self.strings, *sup_name_ref, *name) {
                        self.emit_err("class can't inherit from itself");
                    }
                    self.read_variable(*sup_name_ref);
                    self.emit_instr(OpCode::Inherit);
                }
                for method in methods {
                    self.compile_method(*method);
                }
                if self.current_scope_depth == 0 {
                    self.emit_instr(OpCode::Pop);
                }
            },
            Declaration::Fun(_) => todo!(),
            Declaration::Stmt(stmt) => self.compile_stmt(&stmt),
            Declaration::Var(_) => todo!(),
        }
    }

    fn compile_var_decl(&mut self, var_decl: &VarDecl) {
        todo!()
    }

    fn compile_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                self.compile_expr(&expr);
                self.emit_instr(OpCode::Pop);
            },
            Stmt::For { init, cond, update, body } =>  {
                self.begin_scope();
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
                }
                self.compile_stmt(body);
                if let Some(expr) = update {
                    self.compile_expr(expr);
                    self.emit_jump(JumpKind::Inconditional, before_cond);
                }
                if let Some(jmp) = end_jmp {
                    self.set_jump_tgt(jmp);
                }
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
                self.begin_scope();
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
            _ => todo!(),
            Expr::Call { lhs, args } => todo!(),
            Expr::And(_, _) => todo!(),
            Expr::Or(_, _) => todo!(),
            Expr::Dot(_, _) => todo!(),
        }
    }

    /// Compiles Expr::Assignment
    fn compile_assignment(&mut self, tgt: &Expr, val: &Expr) {
        match tgt {
            Expr::Primary(Primary::Name(n)) => {
                self.compile_expr(val);
                match self.resolve_local(*n) {
                    Some(local_var_ref) if local_var_ref.is_closed_over(self.current_scope_depth()) => {
                        self.register_upval(local_var_ref);
                        self.emit_instr(OpCode::SetUpval(0));
                    },
                    Some(local_var_ref) => self.emit_instr(OpCode::SetLocal(local_var_ref)),
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

    /// Adds the index of the next instruction in the body of the current
    /// function. As a result, a `{Get,Set}Upval` instr should be emitted next.
    fn register_upval(&mut self, upval_tgt: LocalVarRef) {
        let current_fn = self.get_current_fn_mut();
        current_fn.upval_idx.push(current_fn.chunk.len());
    }

    fn resolve_local(&mut self, name: Ref<String>) -> Option<LocalVarRef> {
        None
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
        let current_fn = self.get_current_fn_mut();
        match current_fn.chunk.get_mut(jmp.idx) {
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
        self.get_current_fn().chunk.len()
    }

    fn current_scope_depth(&self) -> usize {
        self.locals.len()
    }

    fn begin_scope(&mut self) {
    }

    fn end_scope(&mut self) {
    }

    /// Compiles the instructions to read the variable
    /// (== push it to the stack) with the given `name`,
    /// in the current scope.
    fn read_variable(&mut self, name: Ref<String>) {
        todo!()
    }

    /// Compiles a method.
    fn compile_method(&mut self, method: Ref<Function>) {
        /*
        let meth = self.functions.get(method);
        let compiled_fn = self.compile_function(meth);
        self.emit_instr(OpCode::Method(compiled_fn));
        */
    }
    ///
    /// Compiles a method.
    fn compile_function(&mut self, f: Ref<Function>) -> Ref<Closure> {
        todo!()
    }
}
