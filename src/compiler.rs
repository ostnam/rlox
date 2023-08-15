use crate::chunk::{Chunk, Instruction, OpCode, LoxVal, Function, FunctionType};
use crate::scanner::{Scanner, ScannerInitError, Token, ScanError, self};

#[derive(Debug)]
pub struct Compiler<'a> {
    scanner: Scanner<'a>,
    previous: Token,
    current: Option<Token>,
    had_error: bool,
    panic_mode: bool,
    current_line: u64,
    locals: Vec<Local>,
    current_scope_depth: i32,
    functions: Vec<Function>,
    current_function: usize,
    current_function_type: FunctionType,
}

#[derive(Debug)]
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

impl From<&Token> for PrecedenceLvl {
    fn from(op: &Token) -> Self {
        match op {
            Token::LParen { .. }
            | Token::RParen { .. }
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
            | Token::While { .. }
            | Token::Dot { .. } => PrecedenceLvl::Null,

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
    UnclosedBlock {
        line: u64
    },
    UnclosedParens {
        line: u64
    },
    VariableUsedWhileInit {
        var_name: String,
        line: u64,
    },
    IfStmtMissingParens {
        line: u64,
    },
    WhileStmtMissingParens {
        line: u64,
    },
    ForStmtMissingParens {
        line: u64,
    },
}

impl<'a> Compiler<'a> {
    pub fn new(src: &'a str) -> Result<Self, ScannerInitError> {
        let scanner = Scanner::new(src)?;
        let main = Function {
            arity: 0,
            chunk: Chunk::default(),
            name: "main".to_string(),
        };
        Ok(Compiler {
            scanner,
            previous: Token::LParen { line: 0 },
            current: None,
            had_error: false,
            panic_mode: false,
            current_line: 0,
            locals: vec![],
            current_scope_depth: 0,
            functions: vec![main],
            current_function: 0,
            current_function_type: FunctionType::Script,
        })
    }

    pub fn compile(mut self) -> Result<Vec<Function>, ()> {
        self.advance();
        while self.current.is_some() {
            self.declaration();
        }
        if let Err(e) = self.end_compilation() {
            self.emit_error(&e);
        }

        match self.had_error {
            false => Ok(self.functions),
            true => Err(()),
        }
    }

    pub fn compile_expr(mut self) -> Result<Vec<Function>, ()> {
        self.advance();
        self.expression();
        if let Err(e) = self.end_compilation() {
            self.emit_error(&e);
        }

        match self.had_error {
            false => Ok(self.functions),
            true => Err(()),
        }
    }

    fn end_compilation(&mut self) -> Result<(), CompilationError> {
        match self.scanner.next() {
            None => {
                self.emit_instr(Instruction  {
                    op: OpCode::Return,
                    line: self.current_line,
                });
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
            CompilationError::UnclosedBlock { line } =>
                println!("[{line}]: unclosed block"),
            CompilationError::UnclosedParens { line } =>
                println!("[{line}]: unclosed parens"),
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
            CompilationError::IfStmtMissingParens { line } => {
                println!("[{line}]: Missing parens around the condition of an if statement.");
            },
            CompilationError::WhileStmtMissingParens { line } => {
                println!("[{line}]: Missing parens around the condition of a while statement.");
            },
            CompilationError::ForStmtMissingParens { line } => {
                println!("[{line}]: Missing parens around the condition of a for statement.");
            },
        }
    }

    fn advance(&mut self) {
        if let Some(t) = &self.current {
            self.previous = t.clone();
        }
        self.current_line = self.previous.line();
        loop {
            match self.scanner.next() {
                Some(Ok(tok)) => {
                    self.current = Some(tok);
                    break;
                },
                Some(Err(e)) if !self.panic_mode =>
                    self.emit_error(&CompilationError::ScanError(e)),
                Some(Err(_)) => (),
                None => {
                    self.current = None;
                    break;
                },
            }
        }
    }

    // Return whether the consumption was successful.
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
        let mut num_valid_locals = self.locals.len();
        for idx in (0..self.locals.len()).rev() {
            match self.locals.get(idx) {
                None => continue,
                Some(var) => {
                    if var.depth <= self.current_scope_depth {
                        break;
                    }
                    num_valid_locals -= 1;
                    self.emit_instr(Instruction {
                        op: OpCode::Pop,
                        line: self.current_line,
                    });
                }
            }
        }
        self.locals.truncate(num_valid_locals);
    }

    /// Takes the name of a local variable, and returns `Some` of the index
    /// on the stack of that variable at runtime if it is a declared local
    /// variable, and `None` otherwise.
    fn resolve_local(&mut self, name: &str) -> Option<usize> {
        // to avoid an underflow when we reach the for loop
        if self.locals.len() == 0 {
            return None;
        }
        for idx in (0..self.locals.len()).rev() {
            match self.locals.get(idx) {
                None => continue,
                Some(var) if var.name == name => {
                    if var.initialized {
                        return Some(idx);
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

    fn emit_instr(&mut self, instr: Instruction) {
        self.functions.get_mut(self.current_function).map(|f| f.chunk.0.push(instr));
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
        if can_assign && self.matches(|t| matches!(t, Token::Eql { .. })) {
            self.emit_error(&CompilationError::Raw {
                text: String::from("Invalid assignment target"),
            });
        }

        let mut current = match &self.current {
            Some(t) => t.clone(),
            None => return,
        };
        while precedence <= PrecedenceLvl::from(&current) {
            self.advance();
            self.infix_rule(&self.previous.clone(), can_assign);
            current = match &self.current {
                Some(t) => t.clone(),
                None => return,
            };
        }
    }

    fn number(&mut self, _can_assign: bool) {
        if let Token::NumLit { value, line } = self.previous {
            self.emit_instr(Instruction {
                op: OpCode::Constant(LoxVal::Num(value)),
                line,
            })
        }
    }

    fn string(&mut self, _can_assign: bool) {
        if let Token::StrLit { content, line } = &self.previous {
            self.emit_instr(Instruction {
                op: OpCode::Constant(LoxVal::Str(content.clone())),
                line: *line,
            });
        }
    }

    // assumes the leading '(' has already been consumed
    fn grouping(&mut self, _can_assign: bool) {
        self.expression();
        self.consume(
            |t| matches!(t, Token::RParen { .. }),
            &CompilationError::UnclosedParens { line: self.current_line },
        )
    }

    fn unary(&mut self, _can_assign: bool) {
        let op = self.previous.clone();
        self.parse_precedence(PrecedenceLvl::Unary);
        match op {
            Token::Minus { line } => self.emit_instr(Instruction {
                op: OpCode::Negate,
                line,
            }),
            Token::Bang { line } => self.emit_instr(Instruction {
                op: OpCode::Not,
                line,
            }),
            _ => (),
        }
    }

    fn declaration(&mut self) {
        if self.matches(|t| matches!(t, Token::Var { .. })) {
            self.var_declaration();
        } else if self.matches(|t| matches!(t, Token::Fun { .. })) {
            self.function_declaration();
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
        let (var_name, line) = match &self.current {
            Some(Token::Identifier { name, line }) => (name.clone(), *line),
            other => {
                self.emit_error(&CompilationError::Raw {
                    text: format!(
                      "[{}]: Expected variable name after keyword var but got: {:?}",
                      self.current_line,
                      other,
                  )
                });
                return;
            }
        };
        self.advance();
        if self.current_scope_depth > 0 {
            self.locals.push(Local {
                    name: var_name.clone(),
                    depth: self.current_scope_depth,
                    initialized: false,
            });
        }
        if self.matches(|t| matches!(t, Token::Eql { .. })) {
            self.expression();
        } else {
            self.emit_instr(Instruction {
                op: OpCode::Constant(LoxVal::Nil),
                line: self.current_line
            })
        }
        self.consume(
            |t| matches!(t, Token::Semicolon { .. }),
            &CompilationError::Raw{
                text: format!("[{}]: Missing semicolon after variable declaration.", self.current_line),
            },
        );
        if self.current_scope_depth == 0 {
            self.emit_instr(Instruction {
                op: OpCode::DefineGlobal(var_name),
                line,
            })
        } else {
            let prev = self.locals.pop().expect("BUG: local was removed from the local vec before the end of its declaration");
            self.locals.push(Local { initialized: true, ..prev });
        }
    }

    fn function_declaration(&mut self) {
        let fn_name = match &self.current {
            Some(Token::Identifier { name, .. }) => {
                let name = name.clone();
                self.advance();
                name
            },
            _ => {
                self.emit_error(&CompilationError::Raw {
                    text: format!(
                        "[{}]: function with no name",
                        self.current_line,
                    ),
                });
                return;
            },
        };
        self.function(fn_name, FunctionType::Regular);
    }

    fn function(
        &mut self,
        name: String,
        _fn_type: FunctionType,
    ) {
        // if we're in a local scope, we need to add the function name
        // as a local variable so that when we compile the body,
        // the name resolves properly.
        if self.current_scope_depth > 0 {
            self.locals.push(Local {
                name: name.clone(),
                depth: self.current_scope_depth,
                initialized: true,
            });
        };
        self.functions.push(Function {
            arity: 0,
            chunk: Chunk::default(),
            name: name.clone(),
        });
        let old_fn_idx = self.current_function;
        let new_fn_idx = self.functions.len() - 1;
        self.current_function = new_fn_idx;
        self.begin_scope();
        self.consume(
            |t| matches!(t, Token::LParen { .. }),
            &CompilationError::Raw {
                text: format!(
                    "[{}] missing ( after function name",
                    self.current_line,
                ),
            },
        );
        if !matches!(self.current, Some(Token::RParen { .. })) {
            let mut first = true;
            while first || self.matches(|t| matches!(t, Token::Comma { .. })) {
                first = false;
                self.functions
                    .get_mut(self.current_function)
                    .map(|f| f.arity += 1);
                let arg_name = match &self.current {
                    Some(Token::Identifier { name, .. }) => {
                        let name = name.clone();
                        self.advance();
                        name
                    },
                    _ => {
                        self.emit_error(&CompilationError::Raw {
                            text: format!(
                                "[{}] expected parameter name",
                                self.current_line,
                            ),
                        });
                        return;
                    },
                };
                self.locals.push(Local {
                    name: arg_name,
                    depth: self.current_scope_depth,
                    initialized: true,
                });
            }
        }
        self.consume(
            |t| matches!(t, Token::RParen { .. }),
            &CompilationError::Raw {
                text: format!(
                    "[{}] missing ) after function args",
                    self.current_line,
                ),
            },
        );
        self.consume(
            |t| matches!(t, Token::LBrace { .. }),
            &CompilationError::Raw {
                text: format!(
                    "[{}] missing {{ after function args",
                    self.current_line,
                ),
            },
        );
        self.block();
        self.end_scope();
        self.current_function = old_fn_idx;
        self.emit_instr(Instruction {
            op: OpCode::Constant(LoxVal::Function(self.functions[new_fn_idx].clone())),
            line: self.current_line,
        });

        if self.current_scope_depth == 0 {
            self.emit_instr(Instruction {
                op: OpCode::DefineGlobal(name),
                line: self.current_line,
            });
        }
    }

    fn statement(&mut self) {
        if self.matches(|t| matches!(t, Token::Print { .. })) {
            self.print_statement();
        } else if self.matches(|t| matches!(t, Token::LBrace { .. })) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else if self.matches(|t| matches!(t, Token::If { .. })) {
            self.if_statement();
        } else if self.matches(|t| matches!(t, Token::While { .. })) {
            self.while_statement();
        } else if self.matches(|t| matches!(t, Token::For { .. })) {
            self.for_statement();
        } else {
            self.expr_statement();
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(
            |t| matches!(t, Token::Semicolon { .. }),
            &CompilationError::Raw {
                text: format!(
                    "[{}]: print statement semicolon is missing at line",
                    self.current_line,
                )
            }
        );
        self.emit_instr(
            Instruction {
                op: OpCode::Print,
                line: self.current_line,
            }
        )
    }

    fn block(&mut self) {
        loop {
            match self.current {
                None | Some(Token::RBrace { .. }) => break,
                _ => self.declaration(),
            }
        }
        self.consume(
            |t| matches!(t, Token::RBrace { .. }),
            &CompilationError::UnclosedBlock { line: self.current_line },
        );
    }

    fn if_statement(&mut self) {
        self.consume(
            |t| matches!(t, Token::LParen { .. }),
            &CompilationError::IfStmtMissingParens {
                line: self.current_line,
            }
        );
        self.expression();
        self.consume(
            |t| matches!(t, Token::RParen { .. }),
            &CompilationError::IfStmtMissingParens {
                line: self.current_line,
            }
        );

        let false_jmp = self.emit_jump(OpCode::JumpIfFalse(0));
        self.emit_instr(Instruction {
            op: OpCode::Pop,
            line: self.current_line,
        });
        self.statement();
        let body_jmp = self.emit_jump(OpCode::Jump(0));

        // condition evaluating to false jumps here
        self.patch_jump(false_jmp);
        self.emit_instr(Instruction {
            op: OpCode::Pop,
            line: self.current_line,
        });
        if self.matches(|t| matches!(t, Token::Else { .. })) {
            self.statement();
        }
        self.patch_jump(body_jmp);
    }

    fn while_statement(&mut self) {
        let loop_start = self.get_next_instr_idx();
        self.consume(
            |t| matches!(t, Token::LParen { .. }),
            &CompilationError::WhileStmtMissingParens { line: self.current_line }
        );
        self.expression();
        self.consume(
            |t| matches!(t, Token::RParen { .. }),
            &CompilationError::WhileStmtMissingParens { line: self.current_line }
        );
        let exit_jmp = self.emit_jump(OpCode::JumpIfFalse(0));
        self.emit_instr(Instruction { op: OpCode::Pop, line: self.current_line });
        self.statement();
        self.emit_loop_jump(loop_start);
        self.patch_jump(exit_jmp);
        self.emit_instr(Instruction { op: OpCode::Pop, line: self.current_line });
    }

    fn for_statement(&mut self) {
        self.begin_scope();
        self.consume(
            |t| matches!(t, Token::LParen { .. }),
            &CompilationError::ForStmtMissingParens { line: self.current_line }
        );
        // initializer
        if self.matches(|t| matches!(t, Token::Var { .. })) {
            self.var_declaration();
        } else if !self.matches(|t| matches!(t, Token::Semicolon { .. })) {
            self.expr_statement();
        }

        // condition
        let mut loop_start = self.get_next_instr_idx();
        let mut exit_jmp = None;
        if !self.matches(|t| matches!(t, Token::Semicolon { .. })) {
            self.expression();
            self.consume(
                |t| matches!(t, Token::Semicolon { .. }),
                &CompilationError::Raw {
                    text: format!("[{}]: Missing ; after condition of the for loop.", self.current_line),
                },
            );
            exit_jmp = Some(self.emit_jump(OpCode::JumpIfFalse(0)));
            self.emit_instr(Instruction {
                op: OpCode::Pop,
                line: self.current_line,
            });
        }

        // update
        if !self.matches(|t| matches!(t, Token::RParen { .. })) {
            let body_jump = self.emit_jump(OpCode::Jump(0));
            let update_start = self.get_next_instr_idx();
            self.expression();
            self.emit_instr(Instruction {
                op: OpCode::Pop,
                line: self.current_line
            });
            self.consume(
                |t| matches!(t, Token::RParen { .. }),
                &CompilationError::ForStmtMissingParens { line: self.current_line },
            );
            self.emit_loop_jump(loop_start);
            loop_start = update_start;
            self.patch_jump(body_jump);
        }

        self.statement();
        self.emit_loop_jump(loop_start);
        if let Some(idx) = exit_jmp {
            self.patch_jump(idx);
            self.emit_instr(Instruction {
                op: OpCode::Pop,
                line: self.current_line,
            });
        }
        self.end_scope();
    }

    fn emit_jump(&mut self, jmp: OpCode) -> usize {
        self.emit_instr(Instruction { op: jmp, line: self.current_line });
        self.get_next_instr_idx() - 1
    }

    fn emit_loop_jump(&mut self, tgt: usize) {
        self.emit_instr(Instruction { op: OpCode::Jump(tgt), line: self.current_line });
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

    fn expr_statement(&mut self) {
        self.expression();
        self.consume(
            |t| matches!(t, Token::Semicolon { .. }),
            &CompilationError::Raw {
                text: format!(
                    "[{}]: expected ; after expression",
                    self.current_line,
                )
            }
        );
        self.emit_instr(
            Instruction {
                op: OpCode::Pop,
                line: self.current_line,
            }
        )
    }

    fn expression(&mut self) {
        self.parse_precedence(PrecedenceLvl::Assignment);
    }

    fn binary(&mut self, _can_assign: bool) {
        let op = self.previous.clone();
        self.parse_precedence(PrecedenceLvl::from(&self.previous).next());

        match op {
            Token::Plus { line } => 
                self.emit_instr(Instruction {
                    op: OpCode::Add,
                    line,
                }),
            Token::Minus { line } => 
                self.emit_instr(Instruction {
                    op: OpCode::Substract,
                    line,
                }),
            Token::Star { line } => 
                self.emit_instr(Instruction {
                    op: OpCode::Multiply,
                    line,
                }),
            Token::Slash { line } => 
                self.emit_instr(Instruction {
                    op: OpCode::Divide,
                    line,
                }),
            Token::BangEql { line } => {
                self.emit_instr(Instruction {
                    op: OpCode::Equal,
                    line,
                });
                self.emit_instr(Instruction {
                    op: OpCode::Not,
                    line,
                });
            },
            Token::EqlEql { line } =>
                self.emit_instr(Instruction {
                    op: OpCode::Equal,
                    line,
                }),
            Token::Greater { line } => 
                self.emit_instr(Instruction {
                    op: OpCode::Greater,
                    line,
                }),
            Token::GreaterEql { line } => {
                self.emit_instr(Instruction {
                    op: OpCode::Less,
                    line,
                });
                self.emit_instr(Instruction {
                    op: OpCode::Not,
                    line,
                });
            }
            Token::Less { line } => 
                self.emit_instr(Instruction {
                    op: OpCode::Less,
                    line,
                }),
            Token::LessEql { line } => {
                self.emit_instr(Instruction {
                    op: OpCode::Greater,
                    line,
                });
                self.emit_instr(Instruction {
                    op: OpCode::Not,
                    line,
                });
            },
            _ => (),
        }
    }

    fn and(&mut self, _can_assign: bool) {
        let false_jmp = self.emit_jump(OpCode::JumpIfFalse(0));
        self.emit_instr(Instruction { op: OpCode::Pop, line: self.current_line });
        self.parse_precedence(PrecedenceLvl::And);
        self.patch_jump(false_jmp);
    }

    fn or(&mut self, _can_assign: bool) {
        let true_jmp = self.emit_jump(OpCode::JumpIfTrue(0));
        self.emit_instr(Instruction { op: OpCode::Pop, line: self.current_line });
        self.parse_precedence(PrecedenceLvl::Or);
        self.patch_jump(true_jmp);
    }

    fn literal(&mut self, _can_assign: bool) {
        match self.previous {
            Token::True { line } =>
                self.emit_instr(Instruction {
                    op: OpCode::Constant(LoxVal::Bool(true)),
                    line,
                }),
            Token::False { line } =>
                self.emit_instr(Instruction {
                    op: OpCode::Constant(LoxVal::Bool(false)),
                    line,
                }),
            Token::Nil { line } =>
                self.emit_instr(Instruction {
                    op: OpCode::Constant(LoxVal::Nil),
                    line,
                }),
            _ => (),
        }
    }

    fn variable(&mut self, can_assign: bool) {
        if let Token::Identifier { name, line } = self.previous.clone() {
            match self.resolve_local(&name) {
                Some(pos) => {
                    if can_assign && self.matches(|t| matches!(t, Token::Eql { .. })) {
                        self.expression();
                        self.emit_instr(Instruction {
                            op: OpCode::SetLocal(pos),
                            line,
                        })
                    } else {
                        self.emit_instr(Instruction {
                            op: OpCode::GetLocal(pos),
                            line
                        })
                    }
                },
                None => {
                    if can_assign && self.matches(|t| matches!(t, Token::Eql { .. })) {
                        self.expression();
                        self.emit_instr(Instruction {
                            op: OpCode::SetGlobal(name.clone()),
                            line,
                        })
                    } else {
                        self.emit_instr(Instruction {
                            op: OpCode::GetGlobal(name),
                            line
                        })
                    }
                }
            }
        }
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
            Token::Super { .. } => Err(()),
            Token::This { .. } => Err(()),
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
            Token::LParen { .. } => (),
            Token::RParen { .. } => (),
            Token::LBrace { .. } => (),
            Token::RBrace { .. } => (),
            Token::Comma { .. } => (),
            Token::Dot { .. } => (),
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
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn run_compiler_expr(program: &str) -> Vec<Function> {
        Compiler::new(program).unwrap().compile_expr().unwrap()
    }

    fn run_compiler(program: &str) -> Result<Vec<Function>, ()> {
        Compiler::new(program).unwrap().compile()
    }

    #[test]
    fn compile_number() {
        assert_eq!(
            run_compiler_expr("10")[0].chunk,
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Num(10.0)), line: 1 },
                Instruction { op: OpCode::Return, line: 1 },
            ])
        )
    }

    #[test]
    fn compile_string() {
        assert_eq!(
            run_compiler_expr(r#""hello lox""#)[0].chunk,
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Str("hello lox".to_string())), line: 1 },
                Instruction { op: OpCode::Return, line: 1 },
            ])
        )
    }

    #[test]
    fn compile_add() {
        assert_eq!(
            run_compiler_expr("10 + 20 + 30")[0].chunk,
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
            run_compiler_expr(r#""hello" + " " + "lox""#)[0].chunk,
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
            run_compiler_expr("10 - 20 - 30")[0].chunk,
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
            run_compiler_expr("1 + 2 * 3 + 4")[0].chunk,
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
            run_compiler_expr("-1")[0].chunk,
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
            run_compiler_expr("---1")[0].chunk,
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
            run_compiler_expr("-1 + 2 * 3")[0].chunk,
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
            run_compiler("-1 + 2 * 3;").unwrap()[0].chunk,
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
            run_compiler("print -1 + 2 * 3;").unwrap()[0].chunk,
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
            run_compiler("var x;").unwrap()[0].chunk,
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
            "#).unwrap()[0].chunk,
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Num(10.0)), line: 3 },
                Instruction { op: OpCode::GetLocal(0), line: 5 },
                Instruction { op: OpCode::GetLocal(1), line: 6 },
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
            "#).unwrap()[0].chunk,
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
            "#).unwrap()[0].chunk,
            Chunk(vec![
                // line 1
                Instruction { op: OpCode::Constant(LoxVal::Num(0.0)), line: 2 },
                Instruction { op: OpCode::DefineGlobal("x".to_string()), line: 2 },
                // var y = 0;
                Instruction { op: OpCode::Constant(LoxVal::Num(0.0)), line: 3 },
                // loop condition
                Instruction { op: OpCode::GetLocal(0), line: 3 },
                Instruction { op: OpCode::Constant(LoxVal::Num(10.0)), line: 3 },
                Instruction { op: OpCode::Less, line: 3 },
                Instruction { op: OpCode::JumpIfFalse(19), line: 3 },
                // update
                Instruction { op: OpCode::Pop, line: 3 },
                // jump to body
                Instruction { op: OpCode::Jump(15), line: 3 },

                // update
                Instruction { op: OpCode::GetLocal(0), line: 3 },
                Instruction { op: OpCode::Constant(LoxVal::Num(1.0)), line: 3 },
                Instruction { op: OpCode::Add, line: 3 },
                Instruction { op: OpCode::SetLocal(0), line: 3 },
                Instruction { op: OpCode::Pop, line: 3 },
                // jump to cond
                Instruction { op: OpCode::Jump(3), line: 3 },

                // body
                Instruction { op: OpCode::GetLocal(0), line: 4 },
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
            ]),
            name: "function".to_string(),
        };
        assert_eq!(
            run_compiler(r#"
                fun function() {
                    print 2;
                }
            "#).unwrap()[0].chunk,
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Function(function)), line: 4 },
                Instruction { op: OpCode::DefineGlobal("function".to_string()), line: 4 },
                Instruction { op: OpCode::Return, line: 4 },
            ]),
        );
    }
}
