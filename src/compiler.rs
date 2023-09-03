use crate::arena::{Ref, Arena};
use crate::ast::{Function, Declaration, Expr, Stmt, AsOpcode, Primary};
use crate::chunk::{Closure, FnType, OpCode, Chunk, Instruction, LoxVal};
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
    current_function: usize,
    current_function_type: FnType,

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
    pub fn compile(input: Parse) -> Option<CompilationResult> {
        let compiler = Self {
            functions: input.functions,
            strings: input.strings,
            closures: Arena::new(),
            locals: Vec::new(),
            current_scope_depth: 0,
            current_function: 0,
            current_function_type: FnType::Main,
            executable: Vec::new(),
            current_line: 0,
        };
        compiler.run_compilation(input.program)
    }

    fn run_compilation(mut self, ast: Vec<Declaration>) -> Option<CompilationResult> {
        let name = self.strings.insert("main".to_string());
        let main = Closure {
            arity: 0,
            chunk: Chunk::default(),
            name,
            sup: None,
            this: None,
        };
        let main_ref = self.closures.insert(main);
        self.executable.push(main_ref);
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

    /// Writes an `Instruction` corresponding to the given `OpCode` at
    /// the end of the current function.
    fn emit_instr(&mut self, op: OpCode) {
        let current_fn_ref = self.executable.get(self.current_function)
            .unwrap_or_else(|| panic!(
                "BUG: getting current function in emit_instr: current function idx is {} but len of functions is {}",
                self.current_function,
                self.executable.len()
        ));
        let compiled_fn = self.closures.get_mut(*current_fn_ref);
        compiled_fn.chunk.0.push(Instruction { op, line: self.current_line });
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
            Expr::Assignment { tgt, val } => todo!(),
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
