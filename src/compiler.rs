use crate::chunk::{Chunk, Instruction, OpCode, LoxVal};
use crate::scanner::{Scanner, ScannerInitError, Token, ScanError, self};

pub struct Compiler<'a> {
    scanner: Scanner<'a>,
    result: Chunk,
    previous: Option<Token>,
    current: Option<Token>,
    had_error: bool,
    panic_mode: bool,
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
            | Token::And { .. }
            | Token::Class { .. }
            | Token::Else { .. }
            | Token::False { .. }
            | Token::For { .. }
            | Token::Fun { .. }
            | Token::If { .. }
            | Token::Nil { .. }
            | Token::Or { .. }
            | Token::Print { .. }
            | Token::Return { .. }
            | Token::Super { .. }
            | Token::This { .. }
            | Token::True { .. }
            | Token::Var { .. }
            | Token::While { .. }
            | Token::Dot { .. } => PrecedenceLvl::Null,

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
    TokensLeft,
    UnclosedParens {
        line: u64
    },
    Raw {
        text: String,
    },
    ScanError(scanner::ScanError),
}

impl<'a> Compiler<'a> {
    pub fn new(src: &'a str) -> Result<Self, ScannerInitError> {
        let scanner = Scanner::new(src)?;
        Ok(Compiler {
            scanner,
            result: Chunk(Vec::new()),
            previous: None,
            current: None,
            had_error: false,
            panic_mode: false,
        })
    }

    pub fn compile(mut self) -> Result<Chunk, ()> {
        self.advance();
        self.expression();
        if let Err(e) = self.end_compilation() {
            self.emit_error(&e);
        }

        match self.had_error {
            false => Ok(self.result),
            true => Err(()),
        }
    }

    fn end_compilation(&mut self) -> Result<(), CompilationError> {
        match self.scanner.next() {
            None => {
                self.result.0.push(Instruction {
                    op: OpCode::Return,
                    line: 0,
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
        }
    }

    fn advance(&mut self) {
        self.previous = self.current.clone();
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
            Some(t) if f(t) => self.advance(),
            _ => self.emit_error(err)
        }
    }

    fn emit_instr(&mut self, instr: Instruction) {
        self.result.0.push(instr)
    }

    fn parse_precedence(&mut self, precedence: PrecedenceLvl) {
        self.advance();
        let mut previous = match &self.previous {
            Some(t) => t.clone(),
            None => return,
        };
        match self.prefix_rule(&previous) {
            Ok(_) => (),
            Err(_) => self.emit_error(&CompilationError::Raw{
                text: String::from("Expected expression.")
            }),
        };

        let mut current = match &self.current {
            Some(t) => t.clone(),
            None => return,
        };
        while precedence <= PrecedenceLvl::from(&current) {
            self.advance();
            previous = match &self.previous {
                Some(t) => t.clone(),
                None => return,
            };
            self.infix_rule(&previous);
            current = match &self.current {
                Some(t) => t.clone(),
                None => return,
            };
        }
    }

    fn number(&mut self) {
        if let Some(Token::NumLit { value, line }) = self.previous {
            self.emit_instr(Instruction {
                op: OpCode::Constant(LoxVal::Num(value)),
                line,
            })
        }
    }

    fn string(&mut self) {
        if let Some(Token::StrLit { content, line }) = &self.previous {
            self.emit_instr(Instruction {
                op: OpCode::Constant(LoxVal::Str(content.clone())),
                line: *line,
            });
        }
    }

    // assumes the leading '(' has already been consumed
    fn grouping(&mut self) {
        self.expression();
        self.consume(
            |t| matches!(t, Token::RParen { .. }),
            &CompilationError::TokensLeft,
        )
    }

    fn unary(&mut self) {
        let op = self.previous.clone();
        self.parse_precedence(PrecedenceLvl::Unary);
        match op {
            Some(Token::Minus { line }) => self.emit_instr(Instruction {
                op: OpCode::Negate,
                line,
            }),
            Some(Token::Bang { line }) => self.emit_instr(Instruction {
                op: OpCode::Not,
                line,
            }),
            _ => (),
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(PrecedenceLvl::Assignment);
    }

    fn binary(&mut self) {
        let previous = match &self.previous {
            Some(a) => a.clone(),
            None => return,
        };
        self.parse_precedence(PrecedenceLvl::from(&previous).next());

        match previous {
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

    fn literal(&mut self) {
        match self.previous {
            Some(Token::True { line }) =>
                self.emit_instr(Instruction {
                    op: OpCode::Constant(LoxVal::Bool(true)),
                    line,
                }),
            Some(Token::False { line }) =>
                self.emit_instr(Instruction {
                    op: OpCode::Constant(LoxVal::Bool(false)),
                    line,
                }),
            Some(Token::Nil { line }) =>
                self.emit_instr(Instruction {
                    op: OpCode::Constant(LoxVal::Nil),
                    line,
                }),
            _ => (),
        }
    }

    fn prefix_rule(&mut self, token: &Token) -> Result<(), ()> {
        match token {
            Token::LParen { .. } => {
                self.grouping();
                Ok(())
            },
            Token::RParen { .. } => Err(()),
            Token::LBrace { .. } => Err(()),
            Token::RBrace { .. } => Err(()),
            Token::Comma { .. } => Err(()),
            Token::Dot { .. } => Err(()),
            Token::Minus { .. } => {
                self.unary();
                Ok(())
            },
            Token::Plus { .. } => Err(()),
            Token::Semicolon { .. } => Err(()),
            Token::Slash { .. } => Err(()),
            Token::Star { .. } => Err(()),
            Token::Bang { .. } => {
                self.unary();
                Ok(())
            },
            Token::BangEql { .. } => Err(()),
            Token::Eql { .. } => Err(()),
            Token::EqlEql { .. } => Err(()),
            Token::Greater { .. } => Err(()),
            Token::GreaterEql { .. } => Err(()),
            Token::Less { .. } => Err(()),
            Token::LessEql { .. } => Err(()),
            Token::Identifier { .. } => Err(()),
            Token::NumLit { .. } => {
                self.number();
                Ok(())
            },
            Token::StrLit { .. } => {
                self.string();
                Ok(())
            },
            Token::And { .. } => Err(()),
            Token::Class { .. } => Err(()),
            Token::Else { .. } => Err(()),
            Token::False { .. } => {
                self.literal();
                Ok(())
            },
            Token::For { .. } => Err(()),
            Token::Fun { .. } => Err(()),
            Token::If { .. } => Err(()),
            Token::Nil { .. } => {
                self.literal();
                Ok(())
            },
            Token::Or { .. } => Err(()),
            Token::Print { .. } => Err(()),
            Token::Return { .. } => Err(()),
            Token::Super { .. } => Err(()),
            Token::This { .. } => Err(()),
            Token::True { .. } => {
                self.literal();
                Ok(())
            },
            Token::Var { .. } => Err(()),
            Token::While { .. } => Err(()),
        }
    }

    fn infix_rule(&mut self, token: &Token) {
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
            Token::And { .. } => (),
            Token::Class { .. } => (),
            Token::Else { .. } => (),
            Token::False { .. } => (),
            Token::For { .. } => (),
            Token::Fun { .. } => (),
            Token::If { .. } => (),
            Token::Nil { .. } => (),
            Token::Or { .. } => (),
            Token::Print { .. } => (),
            Token::Return { .. } => (),
            Token::Super { .. } => (),
            Token::This { .. } => (),
            Token::True { .. } => (),
            Token::Var { .. } => (),
            Token::While { .. } => (),
            Token::Plus { .. }
            | Token::Minus { .. }
            | Token::Slash { .. }
            | Token::Star { .. }
            | Token::BangEql { .. }
            | Token::EqlEql { .. }
            | Token::Greater { .. }
            | Token::GreaterEql { .. }
            | Token::Less { .. }
            | Token::LessEql { .. } => self.binary(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn run_compiler(program: &str) -> Chunk {
        Compiler::new(program).unwrap().compile().unwrap()
    }

    #[test]
    fn compile_number() {
        assert_eq!(
            run_compiler("10"),
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Num(10.0)), line: 1 },
                Instruction { op: OpCode::Return, line: 0 },
            ])
        )
    }

    #[test]
    fn compile_string() {
        assert_eq!(
            run_compiler(r#""hello lox""#),
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Str("hello lox".to_string())), line: 1 },
                Instruction { op: OpCode::Return, line: 0 },
            ])
        )
    }

    #[test]
    fn compile_add() {
        assert_eq!(
            run_compiler("10 + 20 + 30"),
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Num(10.0)), line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Num(20.0)), line: 1 },
                Instruction { op: OpCode::Add, line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Num(30.0)), line: 1 },
                Instruction { op: OpCode::Add, line: 1 },
                Instruction { op: OpCode::Return, line: 0 },
            ])
        );

        assert_eq!(
            run_compiler(r#""hello" + " " + "lox""#),
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Str("hello".to_string())), line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Str(" ".to_string())), line: 1 },
                Instruction { op: OpCode::Add, line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Str("lox".to_string())), line: 1 },
                Instruction { op: OpCode::Add, line: 1 },
                Instruction { op: OpCode::Return, line: 0 },
            ])
        );
    }

    #[test]
    fn compile_sub() {
        assert_eq!(
            run_compiler("10 - 20 - 30"),
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Num(10.0)), line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Num(20.0)), line: 1 },
                Instruction { op: OpCode::Substract, line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Num(30.0)), line: 1 },
                Instruction { op: OpCode::Substract, line: 1 },
                Instruction { op: OpCode::Return, line: 0 },
            ])
        )
    }

    #[test]
    fn compile_add_mult() {
        assert_eq!(
            run_compiler("1 + 2 * 3 + 4"),
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Num(1.0)), line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Num(2.0)), line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Num(3.0)), line: 1 },
                Instruction { op: OpCode::Multiply, line: 1 },
                Instruction { op: OpCode::Add, line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Num(4.0)), line: 1 },
                Instruction { op: OpCode::Add, line: 1 },
                Instruction { op: OpCode::Return, line: 0 },
            ])
        )
    }

    #[test]
    fn compile_minus() {
        assert_eq!(
            run_compiler("-1"),
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Num(1.0)), line: 1 },
                Instruction { op: OpCode::Negate, line: 1 },
                Instruction { op: OpCode::Return, line: 0 },
            ])
        )
    }

    #[test]
    fn compile_multiple_prefix() {
        assert_eq!(
            run_compiler("---1"),
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Num(1.0)), line: 1 },
                Instruction { op: OpCode::Negate, line: 1 },
                Instruction { op: OpCode::Negate, line: 1 },
                Instruction { op: OpCode::Negate, line: 1 },
                Instruction { op: OpCode::Return, line: 0 },
            ])
        )
    }

    #[test]
    fn compile_prefix_and_infix() {
        assert_eq!(
            run_compiler("-1 + 2 * 3"),
            Chunk(vec![
                Instruction { op: OpCode::Constant(LoxVal::Num(1.0)), line: 1 },
                Instruction { op: OpCode::Negate, line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Num(2.0)), line: 1 },
                Instruction { op: OpCode::Constant(LoxVal::Num(3.0)), line: 1 },
                Instruction { op: OpCode::Multiply, line: 1 },
                Instruction { op: OpCode::Add, line: 1 },
                Instruction { op: OpCode::Return, line: 0 },
            ])
        )
    }
}
