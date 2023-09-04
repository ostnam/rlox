use crate::arena::{Ref, Arena};
use crate::ast::{Function, Declaration, Expr, Stmt, AsOpcode, Primary};
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
    current_scope_depth: usize,
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
            current_scope_depth: 0,
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
    fn get_current_fn(&mut self) -> &Closure {
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
        self.locals.last_mut().map_or_else(
            || {
                dbg!("Failed to declare local");
            },
            |frame|
                frame.push(Local {
                    name,
                    depth: self.current_scope_depth,
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
        for node in nodes {
            match node {
                Declaration::Class { name, super_name, methods } => {
                    self.emit_instr(OpCode::Class(name));
                    if self.current_scope_depth > 0 {
                        self.declare_local(name);
                        self.init_last_local();
                    } else {
                        self.emit_instr(OpCode::DefineClass(name));
                    }
                    if let Some(sup_name_ref) = super_name {
                        if refs_eql!(self.strings, sup_name_ref, name) {
                            self.emit_err("class can't inherit from itself");
                            return Err(());
                        }
                        self.read_variable(sup_name_ref);
                        self.emit_instr(OpCode::Inherit);
                    }

                    for method in methods {
                        self.compile_method(method);
                    }

                    if self.current_scope_depth == 0 {
                        self.emit_instr(OpCode::Pop);
                    }

                },
                Declaration::Fun(_) => todo!(),
                Declaration::Stmt(Stmt::Expr(expr)) => {
                    self.compile_expr(&expr);
                    self.emit_instr(OpCode::Pop);
                },
                Declaration::Stmt(_) => {
                    todo!()
                },
                Declaration::Var(_) => todo!(),
            }
        }
        self.emit_instr(OpCode::Return);
        Ok(())
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
                    Some(local_var_ref) if local_var_ref.is_closed_over(self.current_scope_depth) => {
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
