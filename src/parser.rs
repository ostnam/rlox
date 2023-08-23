use crate::arena::{Arena, Ref};
use crate::ast::*;
use crate::refs_eql;
use crate::scanner::{Scanner, Token, ScanError, ScanResult};

pub struct Parser {
    scanner: Box<dyn Iterator<Item=Token>>,
    strings: Arena<String>,
    previous: Token,
    current: Option<Token>,
    had_error: bool,
    panic_mode: bool,
    current_line: u64,
    parsed: Program,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
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

/// More concise way to call `Compiler::consume`.
macro_rules! consume {
    ($self:ident, $tok_type:path, $msg:literal) => {
        $self.consume(
            |t| matches!(t, $tok_type { .. }),
            $msg,
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
        Ok(Parser {
            scanner: Box::new(scanner),
            strings,
            previous: Token::LParen { line: 0 },
            current: None,
            had_error: false,
            panic_mode: false,
            current_line: 0,
            parsed: Vec::new(),
        })
    }

    /// Main parsing method.
    pub fn parse(mut self) -> Option<Program> {
        self.advance();
        while self.current.is_some() {
            let decl = self.declaration()?;
            self.parsed.push(decl);
        }
        self.end_parsing();
        if self.had_error {
            None
        } else {
            Some(self.parsed)
        }
    }

    /// Only parses an expression: used for tests.
    pub fn parse_expr(mut self) -> Option<Expr> {
        self.advance();
        let expr = self.expression()?;
        self.end_parsing();
        if self.had_error {
            None
        } else {
            Some(expr)
        }
    }

    fn end_parsing(&mut self) {
        if let Some(_) = self.scanner.next() {
            self.emit_err("tokens left at the end of parsing");
        }
    }

    fn emit_err(&mut self, err: &str) {
        if self.panic_mode {
            return
        }
        self.panic_mode = true;
        self.had_error = true;
        println!("[{}]: {}", self.current_line, err);
    }

    fn advance(&mut self) {
        if let Some(t) = &self.current {
            self.previous = t.clone();
        }
        self.current_line = self.previous.line();
        self.current = self.scanner.next();
    }

    /// Generally, you should use this method through the `consume!()`
    /// macro, as it requires less boilerplate.
    fn consume<F: Fn(&Token) -> bool>(
        &mut self,
        f: F,
        err: &str,
    ) {
        match &self.current {
            Some(t) if f(t) => {
                self.advance();
            }
            _ => self.emit_err(err)
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

    fn parse_precedence(&mut self, precedence: PrecedenceLvl) -> Option<Expr> {
        self.advance();
        let mut expr = self.prefix_rule()?;
        while self.continue_pratt_loop(precedence) {
            self.advance();
            expr = self.infix_rule(expr)?;
        }
        Some(expr)
    }

    fn continue_pratt_loop(&self, precedence: PrecedenceLvl) -> bool {
        let next = match &self.current {
            Some(a) => a,
            None => return false,
        };
        precedence <= PrecedenceLvl::from(next)
    }

    fn number(&mut self) -> Option<Expr> {
        if let Token::NumLit { value, .. } = self.previous {
            Some(Expr::Primary(Primary::Num(value)))
        } else {
            None
        }
    }

    fn string(&mut self) -> Option<Expr> {
        if let Token::StrLit { content, .. } = self.previous {
            Some(Expr::Primary(Primary::Str(content)))
        } else {
            None
        }
    }

    // assumes the leading '(' has already been consumed
    fn grouping(&mut self) -> Option<Expr> {
        let res = self.expression()?;
        consume!(self, Token::RParen, "unclosed parens");
        Some(res)
    }

    fn unary(&mut self, op: UnaryOperator) -> Option<Expr> {
        let val = self.parse_precedence(PrecedenceLvl::Unary)?;
        Some(Expr::Unop {
            op,
            val: Box::new(val),
        })
    }

    fn declaration(&mut self) -> Option<Declaration> {
        let res = if tok_matches!(self, Token::Var) {
            Some(Declaration::Var(self.var_declaration()?))
        } else if tok_matches!(self, Token::Fun) {
            self.function_declaration()
        } else if tok_matches!(self, Token::Class) {
            self.class_declaration()
        } else {
            Some(Declaration::Stmt(self.statement()?))
        };
        if self.panic_mode {
            self.synchronize();
        }
        res
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;
        self.advance();
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
    fn var_declaration(&mut self) -> Option<VarDecl> {
        let name = self.identifier("after var keyword")?;
        let val = if tok_matches!(self, Token::Eql) {
            Some(self.expression()?)
        } else {
            None
        };
        consume!(self, Token::Semicolon, "missing semicolon after variable declaration");
        Some(VarDecl {
            name,
            val
        })
    }

    fn function_declaration(&mut self) -> Option<Declaration> {
        let _ = self.identifier("after fun")?;
        consume!(self, Token::LParen, "missing ( after function name");
        let mut first = true;
        let mut arity = 0;
        if !tok_matches!(self, Token::RParen) {
            while first || tok_matches!(self, Token::Comma) {
                first = false;
                self.identifier("in parameters list")?;
                arity += 1;
            }
            consume!(self, Token::RParen, "missing ) after function args");
        }
        consume!(self, Token::LBrace, "missing { after function args");
        let block = self.block()?;
        Some(Declaration::Fun(Function {
            arity,
            body: block,
        }))
    }

    fn class_declaration(&mut self) -> Option<Declaration> {
        let name = self.identifier("after 'class'")?;
        let super_name = if tok_matches!(self, Token::Less) {
            let super_name = self.identifier("for superclass name")?;
            let is_same_class = refs_eql!(self.strings, name, super_name);
            if is_same_class {
                self.emit_err("class can't inherit from itself");
                return None;
            }
            Some(super_name)
        } else {
            None
        };
        consume!(self, Token::LBrace, "missing { after class name");
        let mut methods = Vec::new();
        while !tok_matches!(self, Token::RBrace) {
            if let None = self.current {
                self.emit_err("missing } after class declaration");
            }
            let meth = match self.function_declaration()? {
                Declaration::Fun(f) => f,
                _ => return None,
            };
            methods.push(meth);
        }
        Some(Declaration::Class { name, super_name, methods, })
    }

    fn return_statement(&mut self) -> Option<Stmt> {
        Some(Stmt::Return(
            if tok_matches!(self, Token::Semicolon) {
                None
            } else {
                let val = self.expression()?;
                consume!(self, Token::Semicolon, "missing ; after return keyword");
                Some(val)
            }
        ))
    }

    fn call(&mut self, lhs: Expr) -> Option<Expr> {
        let mut args = Vec::new();
        if !tok_matches!(self, Token::RParen) {
            let mut fst_iter = true;
            while fst_iter || tok_matches!(self, Token::Comma) {
                fst_iter = false;
                args.push(self.expression()?);
            }
            consume!(self, Token::RParen, "expected ) after args list");
        }
        Some(Expr::Call { lhs: Box::new(lhs), args })
    }

    fn statement(&mut self) -> Option<Stmt> {
        if tok_matches!(self, Token::Print) {
            self.print_statement()
        } else if tok_matches!(self, Token::LBrace) {
            Some(Stmt::Block(self.block()?))
        } else if tok_matches!(self, Token::If) {
            self.if_statement()
        } else if tok_matches!(self, Token::While) {
            self.while_statement()
        } else if tok_matches!(self, Token::For) {
            self.for_statement()
        } else if tok_matches!(self, Token::Return) {
            self.return_statement()
        } else {
            self.expr_statement()
        }
    }

    fn print_statement(&mut self) -> Option<Stmt> {
        let val = self.expression()?;
        consume!(self, Token::Semicolon, "missing ; after print statement");
        Some(Stmt::Print(val))
    }

    /// The opening { must have been matched.
    fn block(&mut self) -> Option<Block> {
        let mut res = Vec::new();
        loop {
            match self.current {
                None | Some(Token::RBrace { .. }) => break,
                _ => res.push(self.declaration()?),
            }
        }
        consume!(self, Token::RBrace, "unclosed block");
        Some(res)
    }

    fn if_statement(&mut self) -> Option<Stmt> {
        consume!(self, Token::LParen, "missing ( after 'if'");
        let cond = self.expression()?;
        consume!(self, Token::RParen, "missing ) after 'if' condition");

        let body = Box::new(self.statement()?);
        let else_cond = if tok_matches!(self, Token::Else) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };
        Some(Stmt::If { cond, body, else_cond })
    }

    fn while_statement(&mut self) -> Option<Stmt> {
        consume!(self, Token::LParen, "missing ( after 'while'");
        let cond = self.expression()?;
        consume!(self, Token::RParen, "missing ( after 'while'");
        let body = Box::new(self.statement()?);
        Some(Stmt::While { cond, body })
    }

    fn for_statement(&mut self) -> Option<Stmt> {
        consume!(self, Token::LParen, "missing ( after 'for'");
        let init = if tok_matches!(self, Token::Var) {
            Some(ForInit::Decl(self.var_declaration()?))
        } else if !tok_matches!(self, Token::Semicolon) {
            let i = ForInit::Expr(self.expression()?);
            consume!(self, Token::Semicolon, "expected ; after init of for");
            Some(i)
        } else {
            None
        };

        let cond = if !tok_matches!(self, Token::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };

        let update = if !tok_matches!(self, Token::RParen) {
            let u = self.expression()?;
            consume!(self, Token::RParen, "missing ) after 'for' update statement");
            Some(u)
        } else {
            None
        };

        let body = Box::new(self.statement()?);
        Some(Stmt::For { init, cond, update, body })
    }

    fn dot(&mut self, lhs: Expr) -> Option<Expr> {
        let field_name = self.identifier("after .")?;
        Some(Expr::Dot(Box::new(lhs), field_name))
    }

    fn expr_statement(&mut self) -> Option<Stmt> {
        let expr = self.expression()?;
        consume!(self, Token::Semicolon, "expected ; after expression");
        Some(Stmt::Expr(expr))
    }

    fn expression(&mut self) -> Option<Expr> {
        self.parse_precedence(PrecedenceLvl::Assignment)
    }

    fn binary(&mut self, lhs: Expr, op: BinaryOperator) -> Option<Expr> {
        let rhs = self.parse_precedence(PrecedenceLvl::from(&self.previous).next())?;
        Some(Expr::Binop { op, lhs: Box::new(lhs), rhs: Box::new(rhs), })
    }

    fn and(&mut self, lhs: Expr) -> Option<Expr> {
        let rhs = self.parse_precedence(PrecedenceLvl::And)?;
        Some(Expr::And(Box::new(lhs), Box::new(rhs)))
    }

    fn or(&mut self, lhs: Expr) -> Option<Expr> {
        let rhs = self.parse_precedence(PrecedenceLvl::Or)?;
        Some(Expr::Or(Box::new(lhs), Box::new(rhs)))
    }

    /// If the next token is a `Token::Identifier`, returns its `String`.
    /// Otherwise, return `None`, after emitting an error about a missing
    /// identifier in the given `ctx`.
    fn identifier(&mut self, ctx: &str) -> Option<Ref<String>> {
        match self.current {
            Some(Token::Identifier { name, .. }) => {
                self.advance();
                Some(name)
            },
            _ => {
                self.emit_err(&format!("expected identifier {ctx}."));
                None
            },
        }
    }

    fn prefix_rule(&mut self) -> Option<Expr> {
        match self.previous {
            Token::Bang { .. } => self.unary(UnaryOperator::Not),
            Token::False { .. } => Some(Expr::Primary(Primary::Bool(false))),
            Token::Identifier { name, .. } => Some(Expr::Primary(Primary::Name(name))),
            Token::LParen { .. } => self.grouping(),
            Token::Minus { .. } => self.unary(UnaryOperator::Neg),
            Token::Nil { .. } => Some(Expr::Primary(Primary::Nil)),
            Token::NumLit { .. } => self.number(),
            Token::StrLit { .. } => self.string(),
            Token::Super { .. } => Some(Expr::Primary(Primary::Super)),
            Token::This { .. } => Some(Expr::Primary(Primary::This)),
            Token::True { .. } => Some(Expr::Primary(Primary::Bool(true))),
            _ => None,
        }
    }

    fn infix_rule(&mut self, lhs: Expr) -> Option<Expr> {
        match self.previous {
            Token::And { .. } => self.and(lhs),
            Token::BangEql { .. } => self.binary(lhs, BinaryOperator::NotEql),
            Token::Dot { .. } => self.dot(lhs),
            Token::EqlEql { .. } => self.binary(lhs, BinaryOperator::Eql),
            Token::Greater { .. } => self.binary(lhs, BinaryOperator::GT),
            Token::GreaterEql { .. } => self.binary(lhs, BinaryOperator::GE),
            Token::Less { .. } => self.binary(lhs, BinaryOperator::LT),
            Token::LessEql { .. } => self.binary(lhs, BinaryOperator::LE),
            Token::LParen { .. } => self.call(lhs),
            Token::Minus { .. } => self.binary(lhs, BinaryOperator::Minus),
            Token::Or { .. } => self.or(lhs),
            Token::Plus { .. } => self.binary(lhs, BinaryOperator::Plus),
            Token::Slash { .. } => self.binary(lhs, BinaryOperator::Div),
            Token::Star { .. } => self.binary(lhs, BinaryOperator::Mult),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn run_parser_expr(program: &str) -> Expr {
        Parser::new(program).unwrap().parse_expr().unwrap()
    }

    fn run_parser(program: &str) -> Option<Vec<Declaration>> {
        Parser::new(program).unwrap().parse()
    }

    #[test]
    fn parse_number() {
        assert_eq!(
            run_parser_expr("10"),
            Expr::Primary(Primary::Num(10.0)),
        );
        assert_eq!(
            run_parser_expr("10.0"),
            Expr::Primary(Primary::Num(10.0)),
        );
    }
    #[test]
    fn parse_string() {
        assert!(matches!(
            run_parser_expr(r#""hello lox""#),
            Expr::Primary(Primary::Str(_)),
        ));
    }

    #[test]
    fn parse_add() {
        assert_eq!(
            run_parser_expr("10 + 20 + 30"),
            Expr::Binop {
                op: BinaryOperator::Plus,
                lhs: Box::new(Expr::Binop {
                    op: BinaryOperator::Plus,
                    lhs: Box::new(Expr::Primary(Primary::Num(10.0))),
                    rhs: Box::new(Expr::Primary(Primary::Num(20.0))),
                }),
                rhs: Box::new(Expr::Primary(Primary::Num(30.0))),
            },
        );

        assert!(matches!(
            run_parser_expr(r#""hello" + " " + "lox""#),
            Expr::Binop { op: BinaryOperator::Plus, .. },
        ));
    }

    #[test]
    fn parse_sub() {
        assert_eq!(
            run_parser_expr("10 - 20 - 30"),
            Expr::Binop {
                op: BinaryOperator::Minus,
                lhs: Box::new(Expr::Binop {
                    op: BinaryOperator::Minus,
                    lhs: Box::new(Expr::Primary(Primary::Num(10.0))),
                    rhs: Box::new(Expr::Primary(Primary::Num(20.0))),
                }),
                rhs: Box::new(Expr::Primary(Primary::Num(30.0))),
            },
        )
    }

    #[test]
    fn parse_add_mult() {
        assert_eq!(
            run_parser_expr("1 + 2 * 3 + 4"),
            Expr::Binop {
                op: BinaryOperator::Plus,
                lhs: Box::new(Expr::Binop {
                    op: BinaryOperator::Plus,
                    lhs: Box::new(Expr::Primary(Primary::Num(1.0))),
                    rhs: Box::new(Expr::Binop {
                        op: BinaryOperator::Mult,
                        lhs: Box::new(Expr::Primary(Primary::Num(2.0))),
                        rhs: Box::new(Expr::Primary(Primary::Num(3.0))),
                    }),
                }),
                rhs: Box::new(Expr::Primary(Primary::Num(4.0))),
            }
        )
    }
    #[test]
    fn parse_minus() {
        assert_eq!(
            run_parser_expr("-1"),
            Expr::Unop {
                op: UnaryOperator::Neg,
                val: Box::new(Expr::Primary(Primary::Num(1.0)))
            }
        )
    }

    #[test]
    fn parse_multiple_prefix() {
        assert_eq!(
            run_parser_expr("--!1"),
            Expr::Unop {
                op: UnaryOperator::Neg,
                val: Box::new(Expr::Unop {
                    op: UnaryOperator::Neg,
                    val: Box::new(Expr::Unop {
                        op: UnaryOperator::Not,
                        val: Box::new(Expr::Primary(Primary::Num(1.0))),
                    })
                })
            }
        )
    }

    #[test]
    fn parse_prefix_and_infix() {
        assert_eq!(
            run_parser_expr("-1 + 2 * 3"),
            Expr::Binop {
                op: BinaryOperator::Plus,
                lhs: Box::new(Expr::Unop {
                    op: UnaryOperator::Neg,
                    val: Box::new(Expr::Primary(Primary::Num(1.0)))
                }),
                rhs: Box::new(Expr::Binop {
                    op: BinaryOperator::Mult,
                    lhs: Box::new(Expr::Primary(Primary::Num(2.0))),
                    rhs: Box::new(Expr::Primary(Primary::Num(3.0))),
                })
            }
        )
    }

    #[test]
    fn parse_expr_stmt() {
        assert_eq!(
            run_parser("-1 + 2 * 3;-1 + 2 * 3;").unwrap(),
            vec![
                Declaration::Stmt(Stmt::Expr(Expr::Binop {
                    op: BinaryOperator::Plus,
                    lhs: Box::new(Expr::Unop {
                        op: UnaryOperator::Neg,
                        val: Box::new(Expr::Primary(Primary::Num(1.0)))
                    }),
                    rhs: Box::new(Expr::Binop {
                        op: BinaryOperator::Mult,
                        lhs: Box::new(Expr::Primary(Primary::Num(2.0))),
                        rhs: Box::new(Expr::Primary(Primary::Num(3.0))),
                    })
                })),
                Declaration::Stmt(Stmt::Expr(Expr::Binop {
                    op: BinaryOperator::Plus,
                    lhs: Box::new(Expr::Unop {
                        op: UnaryOperator::Neg,
                        val: Box::new(Expr::Primary(Primary::Num(1.0)))
                    }),
                    rhs: Box::new(Expr::Binop {
                        op: BinaryOperator::Mult,
                        lhs: Box::new(Expr::Primary(Primary::Num(2.0))),
                        rhs: Box::new(Expr::Primary(Primary::Num(3.0))),
                    })
                }))
            ]
        )
    }

    #[test]
    fn parse_print_stmt() {
        assert_eq!(
            run_parser("print -1 + 2 * 3;").unwrap(),
            vec![
                Declaration::Stmt(Stmt::Print(Expr::Binop {
                    op: BinaryOperator::Plus,
                    lhs: Box::new(Expr::Unop {
                        op: UnaryOperator::Neg,
                        val: Box::new(Expr::Primary(Primary::Num(1.0)))
                    }),
                    rhs: Box::new(Expr::Binop {
                        op: BinaryOperator::Mult,
                        lhs: Box::new(Expr::Primary(Primary::Num(2.0))),
                        rhs: Box::new(Expr::Primary(Primary::Num(3.0))),
                    })
                })),
            ]
        )
    }

    #[test]
    fn parse_global_set() {
        assert!(matches!(
            run_parser("var x;").unwrap()[..],
            [
                Declaration::Var(VarDecl {
                    name: _,
                    val: None,
                })
            ]
        ));
    }

    #[test]
    fn parse_local_var() {
        assert!(matches!(
            run_parser(r#"
                {
                    var x = 10;
                }
            "#).unwrap()[0],
            Declaration::Stmt(Stmt::Block(_))
        ));
    }

    #[test]
    fn parse_if_stmt() {
        assert_eq!(
            run_parser(r#"
                if (true) {
                    10;
                }
            "#).unwrap(),
            vec![
                Declaration::Stmt(Stmt::If {
                    cond: Expr::Primary(Primary::Bool(true)),
                    body: Box::new(Stmt::Block(vec![
                        Declaration::Stmt(Stmt::Expr(Expr::Primary(Primary::Num(10.0))))
                    ])),
                    else_cond: None,
                })
            ],
        );
    }
}
