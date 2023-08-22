use crate::arena::Arena;
use crate::chunk::{Chunk, Instruction, OpCode, LoxVal, Function, FunctionType, LocalVarRef};
use crate::scanner::{Scanner, Token, ScanError, self, ScanResult};

pub struct Parser {
    scanner: Box<dyn Iterator<Item=Token>>,
    strings: Arena<String>,
    previous: Token,
    current: Option<Token>,
    had_error: bool,
    panic_mode: bool,
    current_line: u64,
    locals: Vec<Vec<Local>>,
    current_scope_depth: i32,
    functions: Vec<Function>,
    current_function: usize,
    current_function_type: FunctionType,
}

#[derive(Clone, Debug)]
struct Local {
    name: String,
    depth: i32,
    initialized: bool,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum PrecedenceLvl {
    Null,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl From<Token> for PrecedenceLvl {
    fn from(op: Token) -> Self {
        match op {
            Token::RParen { .. }
            | Token::LBrace { .. }
            | Token::RBrace { .. }
            | Token::Comma { .. }
            | Token::Semicolon { .. }
            | Token::Bang { .. }
            | Token::Eql { .. }
            | Token::Identifier { .. }
            | Token::NumLit { .. }
            | Token::StrLit { .. }
            | Token::Class { .. }
            | Token::Else { .. }
            | Token::False { .. }
            | Token::For { .. }
            | Token::Fun { .. }
            | Token::If { .. }
            | Token::Nil { .. }
            | Token::Print { .. }
            | Token::Return { .. }
            | Token::Super { .. }
            | Token::This { .. }
            | Token::True { .. }
            | Token::Var { .. }
            | Token::While { .. } => PrecedenceLvl::Null,

            Token::Or { .. } => PrecedenceLvl::Or,
            Token::And { .. } => PrecedenceLvl::And,

            Token::BangEql { .. }
            | Token::EqlEql { .. } => PrecedenceLvl::Equality,

            Token::Greater { .. }
            | Token::GreaterEql { .. }
            | Token::Less { .. }
            | Token::LessEql { .. } => PrecedenceLvl::Comparison,

            Token::Minus { .. }
            | Token::Plus { .. } => PrecedenceLvl::Term,

            Token::Slash { .. }
            | Token::Star { .. } => PrecedenceLvl::Factor,

            Token::LParen { .. }
            | Token::Dot { .. } => PrecedenceLvl::Call,
        }
    }
}

impl From<&Token> for PrecedenceLvl {
    fn from(op: &Token) -> Self {
        match op {
            Token::RParen { .. }
            | Token::LBrace { .. }
            | Token::RBrace { .. }
            | Token::Comma { .. }
            | Token::Semicolon { .. }
            | Token::Bang { .. }
            | Token::Eql { .. }
            | Token::Identifier { .. }
            | Token::NumLit { .. }
            | Token::StrLit { .. }
            | Token::Class { .. }
            | Token::Else { .. }
            | Token::False { .. }
            | Token::For { .. }
            | Token::Fun { .. }
            | Token::If { .. }
            | Token::Nil { .. }
            | Token::Print { .. }
            | Token::Return { .. }
            | Token::Super { .. }
            | Token::This { .. }
            | Token::True { .. }
            | Token::Var { .. }
            | Token::While { .. } => PrecedenceLvl::Null,

            Token::Or { .. } => PrecedenceLvl::Or,
            Token::And { .. } => PrecedenceLvl::And,

            Token::BangEql { .. }
            | Token::EqlEql { .. } => PrecedenceLvl::Equality,

            Token::Greater { .. }
            | Token::GreaterEql { .. }
            | Token::Less { .. }
            | Token::LessEql { .. } => PrecedenceLvl::Comparison,

            Token::Minus { .. }
            | Token::Plus { .. } => PrecedenceLvl::Term,

            Token::Slash { .. }
            | Token::Star { .. } => PrecedenceLvl::Factor,

            Token::LParen { .. }
            | Token::Dot { .. } => PrecedenceLvl::Call,
        }
    }
}


impl PrecedenceLvl {
    fn next(&self) -> PrecedenceLvl {
        match self {
            PrecedenceLvl::Null => PrecedenceLvl::Assignment,
            PrecedenceLvl::Assignment => PrecedenceLvl::Or,
            PrecedenceLvl::Or => PrecedenceLvl::And,
            PrecedenceLvl::And => PrecedenceLvl::Equality,
            PrecedenceLvl::Equality => PrecedenceLvl::Comparison,
            PrecedenceLvl::Comparison => PrecedenceLvl::Term,
            PrecedenceLvl::Term => PrecedenceLvl::Factor,
            PrecedenceLvl::Factor => PrecedenceLvl::Unary,
            PrecedenceLvl::Unary => PrecedenceLvl::Call,
            PrecedenceLvl::Call => PrecedenceLvl::Primary,
            PrecedenceLvl::Primary => PrecedenceLvl::Primary,
        }
    }
}

enum CompilationError {
    Raw {
        text: String,
    },
    ScanError(scanner::ScanError),
    TokensLeft,
    VariableUsedWhileInit {
        var_name: String,
        line: u64,
    },
}

/// More concise way to call `Compiler::consume`.
macro_rules! consume {
    ($self:ident, $tok_type:path, $msg:literal) => {
        $self.consume(
            |t| matches!(t, $tok_type { .. }),
            &CompilationError::Raw {
                text: format!("[{}]: {}", $self.current_line, $msg),
            }
        )
    }
}

/// More concise way to call `Compiler::matches`.
macro_rules! tok_matches {
    ($self:ident, $tok_type:path) => {
        $self.matches(
            |t| matches!(t, $tok_type { .. }),
        )
    }
}

impl Parser {
    pub fn new(src: &str) -> Result<Self, ScanError> {
        let ScanResult { toks: tokens, strings } = Scanner::new(src).scan()?;
        let scanner = tokens.into_iter();
        let main = Function {
            arity: 0,
            chunk: Chunk::default(),
            name: "main".to_string(),
        };
        Ok(Parser {
            scanner: Box::new(scanner),
            strings,
            previous: Token::LParen { line: 0 },
            current: None,
            had_error: false,
            panic_mode: false,
            current_line: 0,
            locals: vec![vec![]],
            current_scope_depth: 0,
            functions: vec![main],
            current_function: 0,
            current_function_type: FunctionType::Script,
        })
    }

    pub fn compile(mut self) -> Result<Function, ()> {
        self.advance();
        while self.current.is_some() {
            self.declaration();
        }
        if let Err(e) = self.end_compilation() {
            self.emit_error(&e);
        }

        match self.had_error {
            false => Ok(self.functions[0].clone()),
            true => Err(()),
        }
    }

    pub fn compile_expr(mut self) -> Result<Function, ()> {
        self.advance();
        self.expression();
        if let Err(e) = self.end_compilation() {
            self.emit_error(&e);
        }

        match self.had_error {
            false => Ok(self.functions[0].clone()),
            true => Err(()),
        }
    }

    fn end_compilation(&mut self) -> Result<(), CompilationError> {
        match self.scanner.next() {
            None => {
                self.emit_instr(OpCode::Return);
                Ok(())
            }
            Some(_) => Err(CompilationError::TokensLeft),
        }
    }

    fn emit_error(&mut self, err: &CompilationError) {
        if self.panic_mode {
            return
        }
        self.panic_mode = true;
        self.had_error = true;

        match err {
            CompilationError::TokensLeft => 
                println!("BUG: tokens left over"),
            CompilationError::Raw { text } => println!("{text}"),
            CompilationError::ScanError(e) => match e {
                ScanError::Bug { details, line } =>
                    println!("[{line}] BUG: {details}"),
                ScanError::UnclosedStringLiteral =>
                    println!("Unclosed string literal"),
                ScanError::UnknownCharacter =>
                    println!("Unknown character"),
            }
            CompilationError::VariableUsedWhileInit { var_name, line } => {
                println!("[{line}]: Variable {var_name} used during its initialization.");
            }
        }
    }

    fn advance(&mut self) {
        if let Some(t) = &self.current {
            self.previous = t.to_owned();
        }
        self.current_line = self.previous.line();
        loop {
            match self.scanner.next() {
                Some(tok) => {
                    self.current = Some(tok);
                    break;
                },
                None => {
                    self.current = None;
                    break;
                },
            }
        }
    }

    /// Generally, you should use this method through the `consume!()`
    /// macro, as it requires less boilerplate.
    fn consume<F: Fn(&Token) -> bool>(
        &mut self,
        f: F,
        err: &CompilationError
    ) {
        match &self.current {
            Some(t) if f(t) => {
                self.current_line = t.line();
                self.advance();
            }
            _ => self.emit_error(err)
        }
    }

    /// Generally, you should use this method through the `tok_matches!()`
    /// macro, as it requires less boilerplate.
    fn matches<F: Fn(&Token) -> bool>(
        &mut self,
        f: F,
    ) -> bool {
        match &self.current {
            Some(t) if f(t) => {
                self.current_line = t.line();
                self.advance();
                true
            }
            _ => false,
        }
    }

    fn begin_scope(&mut self) {
        self.current_scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.current_scope_depth -= 1;
        let current_frame = self.locals[self.locals.len() - 1].clone();
        let mut num_valid_locals = current_frame.len();
        for idx in (0..current_frame.len()).rev() {
            match current_frame.get(idx) {
                None => continue,
                Some(var) => {
                    if var.depth <= self.current_scope_depth {
                        break;
                    }
                    num_valid_locals -= 1;
                    self.emit_instr(OpCode::Pop);
                }
            }
        }
        if let Some(v) = self.locals.last_mut() {
            v.truncate(num_valid_locals);
        };
    }

    fn begin_fn_scope(&mut self) {
        self.current_scope_depth += 1;
        self.locals.push(Vec::new());
    }

    fn end_fn_scope(&mut self) {
        self.current_scope_depth -= 1;
        self.locals.pop();
    }

    fn declare_local(&mut self, name: &str) {
        let last_frame = self.locals.last_mut().unwrap();
        last_frame.push(Local {
            name: name.to_string(),
            depth: self.current_scope_depth,
            initialized: false,
        });
    }

    fn init_last_local(&mut self) {
        match self.locals.last_mut().and_then(|f| f.last_mut()) {
            Some(var) => var.initialized = true,
            None => (),
        }
    }

    /// Takes the name of a local variable, and returns `Some` of the index
    /// on the stack of that variable at runtime if it is a declared local
    /// variable, and `None` otherwise.
    fn resolve_local(&mut self, name: &str) -> Option<LocalVarRef> {
        // to avoid an underflow when we reach the for loop
        if self.locals.is_empty() {
            return None;
        }
        for frame in (0..self.locals.len()).rev() {
            let current_frame = self.locals[frame].clone();
            for pos in (0..current_frame.len()).rev() {
                match current_frame.get(pos) {
                    None => continue,
                    Some(var) if var.name == name => {
                        if var.initialized {
                            return Some(LocalVarRef { frame, pos });
                        } else {
                            self.emit_error(&CompilationError::VariableUsedWhileInit {
                                var_name: name.to_string(),
                                line: self.current_line,
                            });
                        }
                    }
                    Some(_) => continue,
                }
            }
        }
        None
    }

    fn get_next_instr_idx(&self) -> usize {
        self.functions
            .get(self.current_function)
            .expect(
                &format!(
                    "BUG: self.current_function has value {} but only {} functions are listed.",
                    self.current_function,
                    self.functions.len(),
            ))
            .chunk.0.len()
    }

    fn emit_instr(&mut self, instr: OpCode) {
        if let Some(f) = self.functions.get_mut(self.current_function) {
            f.chunk.0.push(Instruction { op: instr, line: self.current_line });
        }
    }

    fn parse_precedence(&mut self, precedence: PrecedenceLvl) {
        self.advance();
        let can_assign = precedence <= PrecedenceLvl::Assignment;
        match self.prefix_rule(&self.previous.clone(), can_assign) {
            Ok(_) => (),
            Err(_) => self.emit_error(&CompilationError::Raw{
                text: String::from("Expected expression.")
            }),
        };
        if can_assign && tok_matches!(self, Token::Eql) {
            self.emit_error(&CompilationError::Raw {
                text: String::from("Invalid assignment target"),
            });
        }

        let mut current = match &self.current {
            Some(t) => t.clone(),
            None => return,
        };
        while precedence <= PrecedenceLvl::from(current) {
            self.advance();
            self.infix_rule(&self.previous.clone(), can_assign);
            current = match &self.current {
                Some(t) => t.clone(),
                None => return,
            };
        }
    }

    fn number(&mut self, _can_assign: bool) {
        if let Token::NumLit { value, .. } = self.previous {
            self.emit_instr(OpCode::Constant(LoxVal::Num(value)));
        }
    }

    fn string(&mut self, _can_assign: bool) {
        if let Token::StrLit { content, .. } = &self.previous {
            // TODO self.emit_instr(OpCode::Constant(LoxVal::Str(content.clone())));
        }
    }

    // assumes the leading '(' has already been consumed
    fn grouping(&mut self, _can_assign: bool) {
        self.expression();
        consume!(self, Token::RParen, "unclosed parens");
    }

    fn unary(&mut self, _can_assign: bool) {
        let op = self.previous.clone();
        self.parse_precedence(PrecedenceLvl::Unary);
        match op {
            Token::Minus { .. } => self.emit_instr(OpCode::Negate),
            Token::Bang { .. } => self.emit_instr(OpCode::Not),
            _ => (),
        }
    }

    fn declaration(&mut self) {
        if tok_matches!(self, Token::Var) {
            self.var_declaration();
        } else if tok_matches!(self, Token::Fun) {
            self.function_declaration();
        } else if tok_matches!(self, Token::Class) {
            self.class_declaration();
        } else {
            self.statement();
        }
        if self.panic_mode {
            self.synchronize();
        }
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;
        while self.current.is_some() {
            if matches!(self.previous, Token::Semicolon { .. }) {
                return;
            }
            match self.current {
                Some( Token::Class  { .. }
                    | Token::Fun    { .. }
                    | Token::Var    { .. }
                    | Token::For    { .. }
                    | Token::If     { .. }
                    | Token::While  { .. }
                    | Token::Print  { .. }
                    | Token::Return { .. }
                    ) => return,
                _ => (),
            }
            self.advance();
        }
    }

    // The var keyword must already have been matched.
    fn var_declaration(&mut self) {
        let var_name = match self.identifier("after var keyword") {
            Some(s) => s,
            None => return,
        };
        if self.current_scope_depth > 0 {
            self.declare_local(&var_name);
        }
        if tok_matches!(self, Token::Eql) {
            self.expression();
        } else {
            self.emit_instr(OpCode::Constant(LoxVal::Nil));
        }
        consume!(self, Token::Semicolon, "missing semicolon after variable declaration");
        if self.current_scope_depth == 0 {
            self.emit_instr(OpCode::DefineGlobal(var_name));
        } else {
            self.init_last_local();
        }
    }

    fn function_declaration(&mut self) {
        let fn_name = match self.identifier("after fun") {
            Some(s) => s,
            _ => return,
        };
        self.function(fn_name, FunctionType::Regular);
    }

    fn function(
        &mut self,
        name: String,
        fn_type: FunctionType,
    ) {
        let old_fn_type = self.current_function_type;
        self.current_function_type = fn_type;
        // if we're in a local scope, we need to add the function name
        // as a local variable so that when we compile the body,
        // the name resolves properly.
        if self.current_scope_depth > 0 && fn_type == FunctionType::Regular {
            self.declare_local(&name);
            self.init_last_local();
        };
        self.functions.push(Function {
            arity: 0,
            chunk: Chunk::default(),
            name: name.clone(),
        });
        let old_fn_idx = self.current_function;
        let new_fn_idx = self.functions.len() - 1;
        self.current_function = new_fn_idx;
        self.begin_fn_scope();
        consume!(self, Token::LParen, "missing ( after function name");
        if !matches!(self.current, Some(Token::RParen { .. })) {
            let mut first = true;
            while first || tok_matches!(self, Token::Comma) {
                first = false;
                if let Some(f) = self.functions.get_mut(self.current_function) {
                    f.arity += 1;
                }
                let arg_name = match self.identifier("in parameters list") {
                    Some(s) => s,
                    None => return,
                };
                self.declare_local(&arg_name);
                self.init_last_local();
            }
        }
        if fn_type == FunctionType::Method || fn_type == FunctionType::Ctor {
            self.declare_local("this");
            self.init_last_local();
            self.declare_local("super");
            self.init_last_local();
        }
        consume!(self, Token::RParen, "missing ) after function args");
        consume!(self, Token::LBrace, "missing { after function args");
        self.block();
        self.emit_implicit_return();
        self.end_fn_scope();
        self.current_function = old_fn_idx;
        if self.current_scope_depth > 0 {
            self.emit_instr(OpCode::Closure(self.functions[new_fn_idx].clone()));
        } else if fn_type == FunctionType::Method || fn_type == FunctionType::Ctor {
            self.emit_instr(
                OpCode::Closure(self.functions[new_fn_idx].clone()));
        } else {
            self.emit_instr(OpCode::Constant(
                LoxVal::Function(self.functions[new_fn_idx].clone())
            ));
            self.emit_instr(OpCode::DefineGlobal(name));
        }
        self.current_function_type = old_fn_type;
    }

    fn class_declaration(&mut self) {
        let class_name = match self.identifier("after 'class'") {
            Some(s) => s,
            None => return,
        };
        self.emit_instr(OpCode::Class(class_name.clone()));
        if self.current_scope_depth > 0 {
            self.declare_local(&class_name);
            self.init_last_local();
        } else {
            self.emit_instr(OpCode::DefineClass(class_name.clone()));
        }
        if tok_matches!(self, Token::Less) {
            match self.identifier("after 'class'") {
                Some(s) if s != class_name => s,
                Some(_) => {
                    self.emit_error(&CompilationError::Raw {
                        text: "class can't inherit from itself".to_string(),
                    });
                    return;
                }
                None => return,
            };
            self.variable(false);
            self.emit_instr(OpCode::Inherit)
        }
        consume!(self, Token::LBrace, "missing { after class name");
        while !tok_matches!(self, Token::RBrace) {
            if let None = self.current {
                self.emit_error(&CompilationError::Raw {
                    text: format!("[{}]: missing }} after class declaration", self.current_line),
                });
            }
            self.method();
        }
        if self.current_scope_depth == 0 {
            self.emit_instr(OpCode::Pop);
        }
    }

    fn method(&mut self) {
        let name = match self.identifier("for method name") {
            Some(s) => s,
            None => return,
        };
        let fn_type = if name == "init" {
            FunctionType::Ctor
        } else {
            FunctionType::Method
        };
        self.function(name.clone(), fn_type);
        self.emit_instr(OpCode::Method(name));
    }


    fn return_statement(&mut self) {
        match self.current_function_type {
            FunctionType::Script | FunctionType::Ctor => {
                self.emit_error(&CompilationError::Raw {
                    text: format!("[{}]: return statement not allowed in this context", self.current_line),
                });
                return;
            },
            _ => (),
        }

        if tok_matches!(self, Token::Semicolon) {
            self.emit_implicit_return();
        } else {
            self.expression();
            consume!(self, Token::Semicolon, "missing ; after return keyword");
            self.emit_instr(OpCode::Return);
        }
    }

    fn emit_implicit_return(&mut self) {
        match self.current_function_type {
            FunctionType::Ctor => {
                let pos = self.resolve_local("this").unwrap();
                self.emit_instr(OpCode::GetLocal(pos));
                self.emit_instr(OpCode::Return);
            }
            _ => {
                self.emit_instr(OpCode::Constant(LoxVal::Nil));
                self.emit_instr(OpCode::Return);
            }
        }
    }

    fn call(&mut self, _can_assign: bool) {
        let mut argcount: u8 = 0;
        if !matches!(self.current, Some(Token::RParen { .. })) {
            let mut fst_iter = true;
            while fst_iter || tok_matches!(self, Token::Comma) {
                fst_iter = false;
                self.expression();
                argcount = match argcount.checked_add(1) {
                    Some(x) => x,
                    None => {
                        self.emit_error(&CompilationError::Raw {
                            text: format!("[{}]: can't pass more than 255 args to a function.", self.current_line),
                        });
                        return;
                    },
                }
            }
        }
        consume!(self, Token::RParen, "expected ) after args list");
        self.emit_instr(OpCode::Call(argcount));
    }

    fn statement(&mut self) {
        if tok_matches!(self, Token::Print) {
            self.print_statement();
        } else if tok_matches!(self, Token::LBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else if tok_matches!(self, Token::If) {
            self.if_statement();
        } else if tok_matches!(self, Token::While) {
            self.while_statement();
        } else if tok_matches!(self, Token::For) {
            self.for_statement();
        } else if tok_matches!(self, Token::Return) {
            self.return_statement();
        } else {
            self.expr_statement();
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        consume!(self, Token::Semicolon, "missing ; after print statement");
        self.emit_instr(OpCode::Print);
    }

    fn block(&mut self) {
        loop {
            match self.current {
                None | Some(Token::RBrace { .. }) => break,
                _ => self.declaration(),
            }
        }
        consume!(self, Token::RBrace, "unclosed block");
    }

    fn if_statement(&mut self) {
        consume!(self, Token::LParen, "missing ( after 'if'");
        self.expression();
        consume!(self, Token::RParen, "missing ) after 'if' condition");

        let false_jmp = self.emit_jump(OpCode::JumpIfFalse(0));
        self.emit_instr(OpCode::Pop);
        self.statement();
        let body_jmp = self.emit_jump(OpCode::Jump(0));

        // condition evaluating to false jumps here
        self.patch_jump(false_jmp);
        self.emit_instr(OpCode::Pop);
        if tok_matches!(self, Token::Else) {
            self.statement();
        }
        self.patch_jump(body_jmp);
    }

    fn while_statement(&mut self) {
        let loop_start = self.get_next_instr_idx();
        consume!(self, Token::LParen, "missing ( after 'while'");
        self.expression();
        consume!(self, Token::RParen, "missing ) after 'while'");
        let exit_jmp = self.emit_jump(OpCode::JumpIfFalse(0));
        self.emit_instr(OpCode::Pop);
        self.statement();
        self.emit_loop_jump(loop_start);
        self.patch_jump(exit_jmp);
        self.emit_instr(OpCode::Pop);
    }

    fn for_statement(&mut self) {
        self.begin_scope();
        consume!(self, Token::LParen, "missing ( after 'for'");
        // initializer
        if tok_matches!(self, Token::Var) {
            self.var_declaration();
        } else if !tok_matches!(self, Token::Semicolon) {
            self.expr_statement();
        }

        // condition
        let mut loop_start = self.get_next_instr_idx();
        let mut exit_jmp = None;
        if !tok_matches!(self, Token::Semicolon) {
            self.expression();
            consume!(self, Token::Semicolon, "missing ; after for condition");
            exit_jmp = Some(self.emit_jump(OpCode::JumpIfFalse(0)));
            self.emit_instr(OpCode::Pop);
        }

        // update
        if !tok_matches!(self, Token::RParen) {
            let body_jump = self.emit_jump(OpCode::Jump(0));
            let update_start = self.get_next_instr_idx();
            self.expression();
            self.emit_instr(OpCode::Pop);
            consume!(self, Token::RParen, "missing ) after 'for' update statement");
            self.emit_loop_jump(loop_start);
            loop_start = update_start;
            self.patch_jump(body_jump);
        }

        self.statement();
        self.emit_loop_jump(loop_start);
        if let Some(idx) = exit_jmp {
            self.patch_jump(idx);
            self.emit_instr(OpCode::Pop);
        }
        self.end_scope();
    }

    fn emit_jump(&mut self, jmp: OpCode) -> usize {
        self.emit_instr(jmp);
        self.get_next_instr_idx() - 1
    }

    fn emit_loop_jump(&mut self, tgt: usize) {
        self.emit_instr(OpCode::Jump(tgt));
    }

    fn patch_jump(&mut self, jmp_idx: usize) {
        let tgt = self.get_next_instr_idx();
        let instr = self
            .functions
            .get_mut(self.current_function)
            .and_then(|f| f.chunk.0.get_mut(jmp_idx));
        match instr {
            Some(x@Instruction { op: OpCode::JumpIfFalse(_), .. }) => {
                x.op = OpCode::JumpIfFalse(tgt);
            },
            Some(x@Instruction { op: OpCode::Jump(_), .. }) => {
                x.op = OpCode::Jump(tgt);
            },
            Some(x@Instruction { op: OpCode::JumpIfTrue(_), .. }) => {
                x.op = OpCode::JumpIfTrue(tgt);
            },
            Some(other) => {
                let copy = other.clone();
                self.emit_error(&CompilationError::Raw {
                    text: format!(
                        "[{}]: BUG: error patching jump, unhandled jump kind: {:?} ",
                        self.current_line,
                        copy,
                    ),
                })
            },
            None => self.emit_error(&CompilationError::Raw {
                text: format!("[{}]: BUG: error patching jump, incorrect jump index: {jmp_idx}", self.current_line),
            })

        }
    }

    fn dot(&mut self, can_assign: bool) {
        let field_name = match self.identifier("after .") {
            Some(n) => n,
            None => return,
        };
        if can_assign && tok_matches!(self, Token::Eql) {
            self.expression();
            self.emit_instr(OpCode::SetProperty(field_name));
        } else {
            self.emit_instr(OpCode::GetProperty(field_name));
        }
    }

    fn expr_statement(&mut self) {
        self.expression();
        consume!(self, Token::Semicolon, "expected ; after expression");
        self.emit_instr(OpCode::Pop);
    }

    fn expression(&mut self) {
        self.parse_precedence(PrecedenceLvl::Assignment);
    }

    fn binary(&mut self, _can_assign: bool) {
        let op = self.previous.clone();
        self.parse_precedence(PrecedenceLvl::from(&self.previous).next());

        match op {
            Token::Plus { .. } => self.emit_instr(OpCode::Add),
            Token::Minus { .. } => self.emit_instr(OpCode::Substract),
            Token::Star { .. } => self.emit_instr(OpCode::Multiply),
            Token::Slash { .. } => self.emit_instr(OpCode::Divide),
            Token::BangEql { .. } => {
                self.emit_instr(OpCode::Equal);
                self.emit_instr(OpCode::Not);
            },
            Token::EqlEql { .. } => self.emit_instr(OpCode::Equal),
            Token::Greater { .. } => self.emit_instr(OpCode::Greater),
            Token::GreaterEql { .. } => {
                self.emit_instr(OpCode::Less);
                self.emit_instr(OpCode::Not);
            }
            Token::Less { .. } => self.emit_instr(OpCode::Less),
            Token::LessEql { .. } => {
                self.emit_instr(OpCode::Greater);
                self.emit_instr(OpCode::Not);
            },
            _ => (),
        }
    }

    fn and(&mut self, _can_assign: bool) {
        let false_jmp = self.emit_jump(OpCode::JumpIfFalse(0));
        self.emit_instr(OpCode::Pop);
        self.parse_precedence(PrecedenceLvl::And);
        self.patch_jump(false_jmp);
    }

    fn or(&mut self, _can_assign: bool) {
        let true_jmp = self.emit_jump(OpCode::JumpIfTrue(0));
        self.emit_instr(OpCode::Pop);
        self.parse_precedence(PrecedenceLvl::Or);
        self.patch_jump(true_jmp);
    }

    fn literal(&mut self, _can_assign: bool) {
        match self.previous {
            Token::True { .. } => self.emit_instr(OpCode::Constant(LoxVal::Bool(true))),
            Token::False { .. } => self.emit_instr(OpCode::Constant(LoxVal::Bool(false))),
            Token::Nil { .. } => self.emit_instr(OpCode::Constant(LoxVal::Nil)),
            _ => (),
        }
    }

    fn variable(&mut self, can_assign: bool) {
        /* TODO
        if let Token::Identifier { name, .. } = self.previous.clone() {
            match self.resolve_local(&name) {
                Some(pos) => {
                    if can_assign && tok_matches!(self, Token::Eql) {
                        self.expression();
                        self.emit_instr(OpCode::SetLocal(pos));
                    } else {
                        self.emit_instr(OpCode::GetLocal(pos));
                    }
                },
                None => {
                    if can_assign && tok_matches!(self, Token::Eql) {
                        self.expression();
                        self.emit_instr(OpCode::SetGlobal(name.clone()));
                    } else {
                        self.emit_instr(OpCode::GetGlobal(name));
                    }
                }
            }
        }
        */
    }

    fn this(&mut self) {
        match self.resolve_local("this") {
            Some(pos) => self.emit_instr(OpCode::GetLocal(pos)),
            None => self.emit_error(&CompilationError::Raw {
                text: format!(
                    "[{}]: 'this' used outside of method",
                    self.current_line,
                )
            }),
        }
    }

    fn super_(&mut self) {
        let pos = match self.resolve_local("super") {
            Some(pos) => pos,
            None => {
                self.emit_error(&CompilationError::Raw {
                    text: format!(
                        "[{}]: 'super' used outside of method",
                        self.current_line,
                    )
                });
                return;
            }
        };
        consume!(self, Token::Dot, "expected . after 'super'");
        match self.identifier("after 'super.'") {
            Some(s) => self.emit_instr(OpCode::GetSuperMethod(pos, s)),
            None => {
                self.emit_error(&CompilationError::Raw {
                    text: format!(
                        "[{}]: 'super.' without method name",
                        self.current_line,
                    )
                });
                return;
            },
        }
    }

    /// If the next token is a `Token::Identifier`, returns its `String`.
    /// Otherwise, return `None`, after emitting an error about a missing
    /// identifier in the given `ctx`.
    fn identifier(&mut self, ctx: &str) -> Option<String> {
        /* TODO
        match &self.current {
            Some(Token::Identifier { name, .. }) => {
                let name = name.clone();
                self.advance();
                Some(name)
            },
            _ => {
                self.emit_error(&CompilationError::Raw {
                    text: format!(
                        "[{}]: expected identifier {ctx}.",
                        self.current_line,
                )});

                None
            },
        }
        */
        None
    }

    fn prefix_rule(&mut self, token: &Token, can_assign: bool) -> Result<(), ()> {
        match token {
            Token::LParen { .. } => {
                self.grouping(can_assign);
                Ok(())
            },
            Token::RParen { .. } => Err(()),
            Token::LBrace { .. } => Err(()),
            Token::RBrace { .. } => Err(()),
            Token::Comma { .. } => Err(()),
            Token::Dot { .. } => Err(()),
            Token::Minus { .. } => {
                self.unary(can_assign);
                Ok(())
            },
            Token::Plus { .. } => Err(()),
            Token::Semicolon { .. } => Err(()),
            Token::Slash { .. } => Err(()),
            Token::Star { .. } => Err(()),
            Token::Bang { .. } => {
                self.unary(can_assign);
                Ok(())
            },
            Token::BangEql { .. } => Err(()),
            Token::Eql { .. } => Err(()),
            Token::EqlEql { .. } => Err(()),
            Token::Greater { .. } => Err(()),
            Token::GreaterEql { .. } => Err(()),
            Token::Less { .. } => Err(()),
            Token::LessEql { .. } => Err(()),
            Token::Identifier { .. } => {
                self.variable(can_assign);
                Ok(())
            },
            Token::NumLit { .. } => {
                self.number(can_assign);
                Ok(())
            },
            Token::StrLit { .. } => {
                self.string(can_assign);
                Ok(())
            },
            Token::And { .. } => Err(()),
            Token::Class { .. } => Err(()),
            Token::Else { .. } => Err(()),
            Token::False { .. } => {
                self.literal(can_assign);
                Ok(())
            },
            Token::For { .. } => Err(()),
            Token::Fun { .. } => Err(()),
            Token::If { .. } => Err(()),
            Token::Nil { .. } => {
                self.literal(can_assign);
                Ok(())
            },
            Token::Or { .. } => Err(()),
            Token::Print { .. } => Err(()),
            Token::Return { .. } => Err(()),
            Token::Super { .. } => {
                self.super_();
                Ok(())
            },
            Token::This { .. } => {
                self.this();
                Ok(())
            },
            Token::True { .. } => {
                self.literal(can_assign);
                Ok(())
            },
            Token::Var { .. } => Err(()),
            Token::While { .. } => Err(()),
        }
    }

    fn infix_rule(&mut self, token: &Token, can_assign: bool) {
        match token {
            Token::RParen { .. } => (),
            Token::LBrace { .. } => (),
            Token::RBrace { .. } => (),
            Token::Comma { .. } => (),
            Token::Semicolon { .. } => (),
            Token::Bang { .. } => (),
            Token::Eql { .. } => (),
            Token::Identifier { .. } => (),
            Token::NumLit { .. } => (),
            Token::StrLit { .. } => (),
            Token::Class { .. } => (),
            Token::Else { .. } => (),
            Token::False { .. } => (),
            Token::For { .. } => (),
            Token::Fun { .. } => (),
            Token::If { .. } => (),
            Token::Nil { .. } => (),
            Token::Print { .. } => (),
            Token::Return { .. } => (),
            Token::Super { .. } => (),
            Token::This { .. } => (),
            Token::True { .. } => (),
            Token::Var { .. } => (),
            Token::While { .. } => (),
            Token::And { .. } => self.and(can_assign),
            Token::Or { .. } => self.or(can_assign),
            Token::Plus { .. }
            | Token::Minus { .. }
            | Token::Slash { .. }
            | Token::Star { .. }
            | Token::BangEql { .. }
            | Token::EqlEql { .. }
            | Token::Greater { .. }
            | Token::GreaterEql { .. }
            | Token::Less { .. }
            | Token::LessEql { .. } => self.binary(can_assign),
            Token::LParen { .. } => self.call(can_assign),
            Token::Dot { .. } => self.dot(can_assign),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn run_compiler_expr(program: &str) -> Function {
        Parser::new(program).unwrap().compile_expr().unwrap()
    }

    fn run_compiler(program: &str) -> Result<Function, ()> {
        Parser::new(program).unwrap().compile()
    }

    #[test]
    fn compile_number() {
        assert_eq!(
            run_compiler_expr("10").chunk,
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Num(10.0)), line: 1 },
                Instruction { op: OpCode::Return, line: 1 },
            ])
        )
    }

    #[test]
    fn compile_string() {
        assert_eq!(
            run_compiler_expr(r#""hello lox""#).chunk,
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Str("hello lox".to_string())), line: 1 },
                Instruction { op: OpCode::Return, line: 1 },
            ])
        )
    }

    #[test]
    fn compile_add() {
        assert_eq!(
            run_compiler_expr("10 + 20 + 30").chunk,
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Num(10.0)), line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Num(20.0)), line: 1 },
                Instruction { op: OpCode::Add, line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Num(30.0)), line: 1 },
                Instruction { op: OpCode::Add, line: 1 },
                Instruction { op: OpCode::Return, line: 1 },
            ])
        );

        assert_eq!(
            run_compiler_expr(r#""hello" + " " + "lox""#).chunk,
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Str("hello".to_string())), line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Str(" ".to_string())), line: 1 },
                Instruction { op: OpCode::Add, line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Str("lox".to_string())), line: 1 },
                Instruction { op: OpCode::Add, line: 1 },
                Instruction { op: OpCode::Return, line: 1 },
            ])
        );
    }

    #[test]
    fn compile_sub() {
        assert_eq!(
            run_compiler_expr("10 - 20 - 30").chunk,
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Num(10.0)), line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Num(20.0)), line: 1 },
                Instruction { op: OpCode::Substract, line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Num(30.0)), line: 1 },
                Instruction { op: OpCode::Substract, line: 1 },
                Instruction { op: OpCode::Return, line: 1 },
            ])
        )
    }

    #[test]
    fn compile_add_mult() {
        assert_eq!(
            run_compiler_expr("1 + 2 * 3 + 4").chunk,
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Num(1.0)), line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Num(2.0)), line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Num(3.0)), line: 1 },
                Instruction { op: OpCode::Multiply, line: 1 },
                Instruction { op: OpCode::Add, line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Num(4.0)), line: 1 },
                Instruction { op: OpCode::Add, line: 1 },
                Instruction { op: OpCode::Return, line: 1 },
            ])
        )
    }

    #[test]
    fn compile_minus() {
        assert_eq!(
            run_compiler_expr("-1").chunk,
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Num(1.0)), line: 1 },
                Instruction { op: OpCode::Negate, line: 1 },
                Instruction { op: OpCode::Return, line: 1 },
            ])
        )
    }

    #[test]
    fn compile_multiple_prefix() {
        assert_eq!(
            run_compiler_expr("---1").chunk,
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Num(1.0)), line: 1 },
                Instruction { op: OpCode::Negate, line: 1 },
                Instruction { op: OpCode::Negate, line: 1 },
                Instruction { op: OpCode::Negate, line: 1 },
                Instruction { op: OpCode::Return, line: 1 },
            ])
        )
    }

    #[test]
    fn compile_prefix_and_infix() {
        assert_eq!(
            run_compiler_expr("-1 + 2 * 3").chunk,
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Num(1.0)), line: 1 },
                Instruction { op: OpCode::Negate, line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Num(2.0)), line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Num(3.0)), line: 1 },
                Instruction { op: OpCode::Multiply, line: 1 },
                Instruction { op: OpCode::Add, line: 1 },
                Instruction { op: OpCode::Return, line: 1 },
            ])
        )
    }

    #[test]
    fn compile_expr_stmt() {
        assert_eq!(
            run_compiler("-1 + 2 * 3;").unwrap().chunk,
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Num(1.0)), line: 1 },
                Instruction { op: OpCode::Negate, line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Num(2.0)), line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Num(3.0)), line: 1 },
                Instruction { op: OpCode::Multiply, line: 1 },
                Instruction { op: OpCode::Add, line: 1 },
                Instruction { op: OpCode::Pop, line: 1 },
                Instruction { op: OpCode::Return, line: 1 },
            ])
        )
    }

    #[test]
    fn compile_print_stmt() {
        assert_eq!(
            run_compiler("print -1 + 2 * 3;").unwrap().chunk,
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Num(1.0)), line: 1 },
                Instruction { op: OpCode::Negate, line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Num(2.0)), line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Num(3.0)), line: 1 },
                Instruction { op: OpCode::Multiply, line: 1 },
                Instruction { op: OpCode::Add, line: 1 },
                Instruction { op: OpCode::Print, line: 1 },
                Instruction { op: OpCode::Return, line: 1 },
            ])
        )
    }

    #[test]
    fn compile_global_set() {
        assert_eq!(
            run_compiler("var x;").unwrap().chunk,
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Nil), line: 1 },
                Instruction { op: OpCode::DefineGlobal("x".to_string()), line: 1 },
                Instruction { op: OpCode::Return, line: 1 },
            ])
        );
        assert_eq!(
            run_compiler("a * b = 10;"),
            Err(()),
        );
    }

    #[test]
    fn compile_local_var() {
        assert_eq!(
            run_compiler(r#"
                {
                    var x = 10;
                    {
                        var y = x;
                        y + 1;
                    }
                }
            "#).unwrap().chunk,
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Num(10.0)), line: 3 },
                Instruction { op: OpCode::GetLocal(LocalVarRef { frame: 0, pos: 0 }), line: 5 },
                Instruction { op: OpCode::GetLocal(LocalVarRef { frame: 0, pos: 1 }), line: 6 },
                Instruction { op: OpCode::Constant(LoxVal::Num(1.0)), line: 6 },
                Instruction { op: OpCode::Add, line: 6 },
                Instruction { op: OpCode::Pop, line: 6 },
                Instruction { op: OpCode::Pop, line: 7 },  // pop y
                Instruction { op: OpCode::Pop, line: 8 },  // pop x
                Instruction { op: OpCode::Return, line: 8 },
            ]),
        );
        assert!(matches!(
            run_compiler(r#"
                {
                    var x = 10;
                    {
                        var x = x;
                    }
                }
            "#),
            Err(_),
        ));
    }

    #[test]
    fn compile_if_stmt() {
        assert_eq!(
            run_compiler(r#"
                if (true) {
                    10;
                }
            "#).unwrap().chunk,
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Bool(true)), line: 2 },
                Instruction { op: OpCode::JumpIfFalse(6), line: 2 },
                Instruction { op: OpCode::Pop, line: 2 },
                Instruction { op: OpCode::Constant(LoxVal::Num(10.0)), line: 3 },
                Instruction { op: OpCode::Pop, line: 3 },
                Instruction { op: OpCode::Jump(7), line: 4 },
                Instruction { op: OpCode::Pop, line: 4 },
                Instruction { op: OpCode::Return, line: 4 },
            ]),
        );
    }

    #[test]
    fn compile_for_stmt() {
        assert_eq!(
            run_compiler(r#"
                var x = 0;
                for (var y = 0; y < 10; y = y + 1) {
                    x = y;
                }
            "#).unwrap().chunk,
            Chunk(vec![
                // line 1
                Instruction { op: OpCode::Constant(LoxVal::Num(0.0)), line: 2 },
                Instruction { op: OpCode::DefineGlobal("x".to_string()), line: 2 },
                // var y = 0;
                Instruction { op: OpCode::Constant(LoxVal::Num(0.0)), line: 3 },
                // loop condition
                Instruction { op: OpCode::GetLocal(LocalVarRef { frame: 0, pos: 0 }), line: 3 },
                Instruction { op: OpCode::Constant(LoxVal::Num(10.0)), line: 3 },
                Instruction { op: OpCode::Less, line: 3 },
                Instruction { op: OpCode::JumpIfFalse(19), line: 3 },
                // update
                Instruction { op: OpCode::Pop, line: 3 },
                // jump to body
                Instruction { op: OpCode::Jump(15), line: 3 },

                // update
                Instruction { op: OpCode::GetLocal(LocalVarRef { frame: 0, pos: 0 }), line: 3 },
                Instruction { op: OpCode::Constant(LoxVal::Num(1.0)), line: 3 },
                Instruction { op: OpCode::Add, line: 3 },
                Instruction { op: OpCode::SetLocal(LocalVarRef { frame: 0, pos: 0 }), line: 3 },
                Instruction { op: OpCode::Pop, line: 3 },
                // jump to cond
                Instruction { op: OpCode::Jump(3), line: 3 },

                // body
                Instruction { op: OpCode::GetLocal(LocalVarRef { frame: 0, pos: 0 }), line: 4 },
                Instruction { op: OpCode::SetGlobal("x".to_string()), line: 4 },
                Instruction { op: OpCode::Pop, line: 4 },
                Instruction { op: OpCode::Jump(9), line: 5 },
                Instruction { op: OpCode::Pop, line: 5 },
                Instruction { op: OpCode::Pop, line: 5 },
                Instruction { op: OpCode::Return, line: 5 },
            ]),
        );
    }

    #[test]
    fn compile_function() {
        let function = Function {
            arity: 0,
            chunk: Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Num(2.0)), line: 3},
                Instruction { op: OpCode::Print, line: 3},
                Instruction { op: OpCode::Constant(LoxVal::Nil), line: 4},
                Instruction { op: OpCode::Return, line: 4},
            ]),
            name: "function".to_string(),
        };
        assert_eq!(
            run_compiler(r#"
                fun function() {
                    print 2;
                }
            "#).unwrap().chunk,
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Function(function)), line: 4 },
                Instruction { op: OpCode::DefineGlobal("function".to_string()), line: 4 },
                Instruction { op: OpCode::Return, line: 4 },
            ]),
        );
    }
}
