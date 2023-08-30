use crate::arena::{Ref, Arena};
use crate::ast::{Function, Declaration};
use crate::chunk::{CompiledFn, FnType, OpCode, Chunk, Instruction};
use crate::parser::Parse;
use crate::refs_eql;

pub struct Compiler {
    // Arenas for different objects
    functions: Arena<Function>,
    strings: Arena<String>,
    compiled_fns: Arena<CompiledFn>,

    // for resolving variables
    locals: Vec<Vec<Local>>,
    current_scope_depth: usize,
    current_function: usize,
    current_function_type: FnType,

    ast_roots: Vec<Declaration>,
    executable: Vec<Ref<CompiledFn>>,
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
    pub functions: Arena<CompiledFn>,
    pub executable: Vec<Ref<CompiledFn>>,
}

impl Compiler {
    pub fn compile(input: Parse) -> Option<CompilationResult> {
        let compiler = Self {
            functions: input.functions,
            strings: input.strings,
            compiled_fns: Arena::new(),
            locals: Vec::new(),
            current_scope_depth: 0,
            current_function: 0,
            current_function_type: FnType::Main,
            ast_roots: input.program,
            executable: Vec::new(),
            current_line: 0,
        };
        compiler.run_compilation()
    }

    fn run_compilation(mut self) -> Option<CompilationResult> {
        let name = self.strings.insert("main".to_string());
        let main = CompiledFn { arity: 0, chunk: Chunk::default(), name };
        let main_ref = self.compiled_fns.insert(main);
        self.executable.push(main_ref);
        self.optimize_ast();
        self.pass();
        None
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
        let compiled_fn = self.compiled_fns.get_mut(*current_fn_ref);
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
    fn pass(&mut self) -> Result<(), ()> {
        for node in &self.ast_roots {
            match node {
                Declaration::Class { name, super_name, methods } => {
                    self.emit_instr(OpCode::Class(*name));
                    if self.current_scope_depth > 0 {
                        self.declare_local(*name);
                        self.init_last_local();
                    } else {
                        self.emit_instr(OpCode::DefineClass(*name));
                    }
                    if let Some(sup_name_ref) = super_name {
                        if refs_eql!(self.strings, *sup_name_ref, *name) {
                            self.emit_err("class can't inherit from itself");
                            return Err(());
                        }
                        self.read_variable(*sup_name_ref);
                        self.emit_instr(OpCode::Inherit);
                    }

                    for &method in methods {
                        self.compile_method(method);
                    }

                    if self.current_scope_depth == 0 {
                        self.emit_instr(OpCode::Pop);
                    }

                },
                Declaration::Fun(_) => todo!(),
                Declaration::Stmt(_) => todo!(),
                Declaration::Var(_) => todo!(),
            }
        }
        Ok(())
    }

    /// Compiles the instructions to read the variable
    /// (== push it to the stack) with the given `name`,
    /// in the current scope.
    fn read_variable(&mut self, name: Ref<String>) {
        todo!()
    }

    /// Compiles a method.
    fn compile_method(&mut self, method: Ref<Function>) {
        todo!()
    }
}
