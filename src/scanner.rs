use std::{str::Chars, iter::Peekable};

/// Identifiers and strings store a &str of the source string, hence the lifetime.
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    // single-char tokens
    LParen     { line: u64 },
    RParen     { line: u64 },
    LBrace     { line: u64 },
    RBrace     { line: u64 },
    Comma      { line: u64 },
    Dot        { line: u64 },
    Minus      { line: u64 },
    Plus       { line: u64 },
    Semicolon  { line: u64 },
    Slash      { line: u64 },
    Star       { line: u64 },

    // one or 2 chars long
    Bang       { line: u64 },
    BangEql    { line: u64 },
    Eql        { line: u64 },
    EqlEql     { line: u64 },
    Greater    { line: u64 },
    GreaterEql { line: u64 },
    Less       { line: u64 },
    LessEql    { line: u64 },

    //  literals
    Identifier { name: String, line: u64 },
    NumLit     { value: f64, line: u64 },
    /// The quotes aren't included in `content`
    StrLit     { content: String, line: u64 },

    // keywords
    And        { line: u64 },
    Class      { line: u64 },
    Else       { line: u64 },
    False      { line: u64 },
    For        { line: u64 },
    Fun        { line: u64 },
    If         { line: u64 },
    Nil        { line: u64 },
    Or         { line: u64 },
    Print      { line: u64 },
    Return     { line: u64 },
    Super      { line: u64 },
    This       { line: u64 },
    True       { line: u64 },
    Var        { line: u64 },
    While      { line: u64 },
}

impl Token {
    pub fn line(&self) -> u64 {
        match self {
            Token::LParen { line } => *line,
            Token::RParen { line } => *line,
            Token::LBrace { line } => *line,
            Token::RBrace { line } => *line,
            Token::Comma { line } => *line,
            Token::Dot { line } => *line,
            Token::Minus { line } => *line,
            Token::Plus { line } => *line,
            Token::Semicolon { line } => *line,
            Token::Slash { line } => *line,
            Token::Star { line } => *line,
            Token::Bang { line } => *line,
            Token::BangEql { line } => *line,
            Token::Eql { line } => *line,
            Token::EqlEql { line } => *line,
            Token::Greater { line } => *line,
            Token::GreaterEql { line } => *line,
            Token::Less { line } => *line,
            Token::LessEql { line } => *line,
            Token::Identifier { name: _, line } => *line,
            Token::NumLit { value: _, line } => *line,
            Token::StrLit { content: _, line } => *line,
            Token::And { line } => *line,
            Token::Class { line } => *line,
            Token::Else { line } => *line,
            Token::False { line } => *line,
            Token::For { line } => *line,
            Token::Fun { line } => *line,
            Token::If { line } => *line,
            Token::Nil { line } => *line,
            Token::Or { line } => *line,
            Token::Print { line } => *line,
            Token::Return { line } => *line,
            Token::Super { line } => *line,
            Token::This { line } => *line,
            Token::True { line } => *line,
            Token::Var { line } => *line,
            Token::While { line } => *line,
        }
    }
}

/// Every error that can occur while scanning.
#[derive(Debug)]
pub enum ScanError {
    /// For internal errors that shouldn't occur unless there's something
    /// wrong with the implementation of the scanner.
    Bug {
        details: String,
        line: u64,
    },
    UnclosedStringLiteral,
    UnknownCharacter,
}

#[derive(Debug)]
pub struct Scanner<'a> {
    chars: Peekable<Chars<'a>>,
    current_line: u64,
}

#[derive(Debug)]
pub enum ScannerInitError {
    InputTooLong
}

impl<'a> Scanner<'a> {
    /// Creates a new scanner for the `src`.
    ///
    /// Can only fail if `src.len() == usize::MAX()`, in which case
    /// `ScannerInitError::InputTooLong` will be returned (as `advance`
    /// increments `self.pos` by 1, if `src.len() == usize::MAX`,
    /// we risk overflowing back to 0, potentially causing the scanner to loop
    /// forever).
    pub fn new(src: &'a str) -> Result<Scanner<'a>, ScannerInitError> {
        if src.len() == usize::MAX {
            Err(ScannerInitError::InputTooLong)
        } else {
            Ok(Scanner { chars: src.chars().peekable(), current_line: 1 })
        }
    }

    /// Returns the next `char` from the `src`, or `None` if we `advance`d past
    /// the last `char`.
    /// The next time `advance` is called, the following `char` will be returned.
    fn advance(&mut self) -> Option<char> {
        self.chars.next()
    }

    /// Returns the next `char` from the `src`, or `None` if we `advance`d past
    /// the last `char`.
    /// Unlike `advance`, it doesn't increment the internal counter, and calling
    /// `peek` repeatedly returns the same character (or `None`) at every call.
    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|x| *x)
    }

    /// Returns the next `char` from the `src`, or `None` if we `advance`d past
    /// the last `char`.
    /// Like `peek`, it doesn't increment the internal counter.
    fn peek_next(&self) -> Option<char> {
        let mut copy = self.chars.clone();
        copy.next();
        copy.peek().map(|x| *x)
    }

    /// If the next char is equal to the one passed, returns true and consumes
    /// it, if not do nothing.
    fn matches(&mut self, c: char) -> bool {
        match self.peek() {
            Some(d) if d == c => {
                self.chars.next();
                true
            }
            _ => false
        }
    }

    /// Also skips comments.
    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                None => break,
                Some(' ' | '\t' | '\r') => {
                    self.advance();
                }
                Some('\n') => {
                    self.current_line += 1;
                    self.advance();
                }
                Some('/') if matches!(self.peek_next(), Some('/')) => {
                    self.advance();
                    self.advance();
                    loop {
                        match self.peek() {
                            Some('\n') => break,
                            Some(_) => {
                                self.advance();
                            }
                            None => break,
                        }
                    }
                }
                _ => break,
            };
        }
    }

    /// Called to scan a string literal.
    /// The leading " must already have been matched.
    /// Only returns `None` if the end of the source has been reached.
    fn scan_str_literal(&mut self) -> Option<Result<Token, ScanError>> {
        let start_line = self.current_line;
        let mut res = String::new();
        loop {
            match self.advance() {
                None => return Some(Err(ScanError::UnclosedStringLiteral)),
                Some('\n') => {
                    self.current_line += 1;
                    res.push('\n');
                }
                Some('"') => {
                    break;
                },
                Some(c) => {
                    res.push(c)
                },
            }
        }
        Some(Ok(Token::StrLit {
            content: res,
            line: start_line,
        }))
    }

    /// Called to scan a number literal.
    /// Only returns `None` if the end of the source has been reached.
    fn scan_num_literal(&mut self) -> Option<Result<Token, ScanError>> {
        let mut collected = String::new();
        loop {
            match self.peek() {
                None if collected.len() == 0 => return None,
                None => break,
                Some(c) if c.is_ascii_digit() => {
                    collected.push(c);
                    self.advance();
                }
                Some(_) => break,
            }
        }
        match (self.peek(), self.peek_next()) {
            (Some('.'), Some(c)) if c.is_ascii_digit() => {
                self.advance();
                self.advance();
                collected.push('.');
                collected.push(c);
                loop {
                    match self.peek() {
                        None => break,
                        Some(c) if c.is_ascii_digit() => {
                            self.advance();
                            collected.push(c);
                        }
                        Some(_) => break,
                    }
                }
            }
            _ => (),
        }

        Some(match collected.parse::<f64>() {
            Ok(num) => Ok(Token::NumLit {
                value: num,
                line: self.current_line,
            }),
            Err(e) => Err(ScanError::Bug {
                details: format!(
                    "Error parsing slice representing number: {} to f64: {}",
                    collected,
                    e,
                ),
                line: self.current_line,
            }),
        })
    }

    fn scan_identifier_or_keyword(&mut self) -> Option<Result<Token, ScanError>> {
        let mut collected = String::new();
        match self.peek() {
            None => return None,
            Some(c) if c.is_ascii_alphabetic() || c == '_' => {
                self.advance();
                collected.push(c);
            },
            _ => return Some(Err(ScanError::Bug {
                details: format!(
                    "Error scanning identifier at line {}",
                    self.current_line
                ),
                line: self.current_line,
            })),
        }
        loop {
            match self.peek() {
                Some(c) if c.is_ascii_alphanumeric() || c == '_' => {
                    self.advance();
                    collected.push(c);
                },
                Some(_) | None => break,
            }
        }
        match collected.as_str() {
            "and"    => Some(Ok(Token::And    { line: self.current_line })),
            "class"  => Some(Ok(Token::Class  { line: self.current_line })),
            "else"   => Some(Ok(Token::Else   { line: self.current_line })),
            "false"  => Some(Ok(Token::False  { line: self.current_line })),
            "for"    => Some(Ok(Token::For    { line: self.current_line })),
            "fun"    => Some(Ok(Token::Fun    { line: self.current_line })),
            "if"     => Some(Ok(Token::If     { line: self.current_line })),
            "nil"    => Some(Ok(Token::Nil    { line: self.current_line })),
            "or"     => Some(Ok(Token::Or     { line: self.current_line })),
            "print"  => Some(Ok(Token::Print  { line: self.current_line })),
            "return" => Some(Ok(Token::Return { line: self.current_line })),
            "super"  => Some(Ok(Token::Super  { line: self.current_line })),
            "this"   => Some(Ok(Token::This   { line: self.current_line })),
            "true"   => Some(Ok(Token::True   { line: self.current_line })),
            "var"    => Some(Ok(Token::Var    { line: self.current_line })),
            "while"  => Some(Ok(Token::While  { line: self.current_line })),
            _ => Some(Ok(Token::Identifier {
                name: collected,
                line: self.current_line
            })),
        }
    }
}

impl<'a> std::iter::Iterator for Scanner<'a> {
    type Item = Result<Token, ScanError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        match self.peek()? {
            c if c.is_ascii_alphabetic() || c == '_'
                => return self.scan_identifier_or_keyword(),
            c if c.is_ascii_digit()
                => return self.scan_num_literal(),
            _   => (),
        };
        match self.advance()? {
            '(' => Some(Ok(Token::LParen { line: self.current_line })),
            ')' => Some(Ok(Token::RParen { line: self.current_line })),
            '{' => Some(Ok(Token::LBrace { line: self.current_line })),
            '}' => Some(Ok(Token::RBrace { line: self.current_line })),
            ';' => Some(Ok(Token::Semicolon { line: self.current_line })),
            ',' => Some(Ok(Token::Comma { line: self.current_line })),
            '.' => Some(Ok(Token::Dot { line: self.current_line })),
            '-' => Some(Ok(Token::Minus { line: self.current_line })),
            '+' => Some(Ok(Token::Plus { line: self.current_line })),
            '/' => Some(Ok(Token::Slash { line: self.current_line })),
            '*' => Some(Ok(Token::Star { line: self.current_line })),
            '!' if self.matches('=') =>
                Some(Ok(Token::BangEql { line: self.current_line })),
            '!' => Some(Ok(Token::Bang { line: self.current_line })),
            '=' if self.matches('=') =>
                Some(Ok(Token::EqlEql { line: self.current_line })),
            '=' => Some(Ok(Token::Eql { line: self.current_line })),
            '<' if self.matches('=') =>
                Some(Ok(Token::LessEql { line: self.current_line })),
            '<' => Some(Ok(Token::Less { line: self.current_line })),
            '>' if self.matches('=') =>
                Some(Ok(Token::GreaterEql { line: self.current_line })),
            '>' => Some(Ok(Token::Greater { line: self.current_line })),
            '"' => {
                self.scan_str_literal()
            }
            _ => Some(Err(ScanError::UnknownCharacter))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn scan<'a>(src: &'a str) -> Vec<Token> {
        Scanner::new(src)
            .unwrap()
            .map(|r| r.unwrap())
            .collect()
    }

    #[test]
    fn scan_single_token() {
        let toks = scan("(");
        assert_eq!(toks[..], [Token::LParen { line: 1 }]);
        let toks = scan(")");
        assert_eq!(toks[..], [Token::RParen { line: 1 }]);
        let toks = scan("{");
        assert_eq!(toks[..], [Token::LBrace { line: 1 }]);
        let toks = scan("}");
        assert_eq!(toks[..], [Token::RBrace { line: 1 }]);
        let toks = scan(",");
        assert_eq!(toks[..], [Token::Comma { line: 1 }]);
        let toks = scan(".");
        assert_eq!(toks[..], [Token::Dot { line: 1 }]);
        let toks = scan("-");
        assert_eq!(toks[..], [Token::Minus { line: 1 }]);
        let toks = scan("+");
        assert_eq!(toks[..], [Token::Plus { line: 1 }]);
        let toks = scan(";");
        assert_eq!(toks[..], [Token::Semicolon { line: 1 }]);
        let toks = scan("/");
        assert_eq!(toks[..], [Token::Slash { line: 1 }]);
        let toks = scan("*");
        assert_eq!(toks[..], [Token::Star { line: 1 }]);
        let toks = scan("!");
        assert_eq!(toks[..], [Token::Bang { line: 1 }]);
        let toks = scan("!=");
        assert_eq!(toks[..], [Token::BangEql { line: 1 }]);
        let toks = scan("=");
        assert_eq!(toks[..], [Token::Eql { line: 1 }]);
        let toks = scan("==");
        assert_eq!(toks[..], [Token::EqlEql { line: 1 }]);
        let toks = scan(">");
        assert_eq!(toks[..], [Token::Greater { line: 1 }]);
        let toks = scan(">=");
        assert_eq!(toks[..], [Token::GreaterEql { line: 1 }]);
        let toks = scan("<");
        assert_eq!(toks[..], [Token::Less { line: 1 }]);
        let toks = scan("<=");
        assert_eq!(toks[..], [Token::LessEql { line: 1 }]);
        let toks = scan("and");
        assert_eq!(toks[..], [Token::And { line: 1 }]);
        let toks = scan("class");
        assert_eq!(toks[..], [Token::Class { line: 1 }]);
        let toks = scan("else");
        assert_eq!(toks[..], [Token::Else { line: 1 }]);
        let toks = scan("false");
        assert_eq!(toks[..], [Token::False { line: 1 }]);
        let toks = scan("for");
        assert_eq!(toks[..], [Token::For { line: 1 }]);
        let toks = scan("fun");
        assert_eq!(toks[..], [Token::Fun { line: 1 }]);
        let toks = scan("if");
        assert_eq!(toks[..], [Token::If { line: 1 }]);
        let toks = scan("nil");
        assert_eq!(toks[..], [Token::Nil { line: 1 }]);
        let toks = scan("or");
        assert_eq!(toks[..], [Token::Or { line: 1 }]);
        let toks = scan("print");
        assert_eq!(toks[..], [Token::Print { line: 1 }]);
        let toks = scan("return");
        assert_eq!(toks[..], [Token::Return { line: 1 }]);
        let toks = scan("super");
        assert_eq!(toks[..], [Token::Super { line: 1 }]);
        let toks = scan("this");
        assert_eq!(toks[..], [Token::This { line: 1 }]);
        let toks = scan("true");
        assert_eq!(toks[..], [Token::True { line: 1 }]);
        let toks = scan("var");
        assert_eq!(toks[..], [Token::Var { line: 1 }]);
        let toks = scan("while");
        assert_eq!(toks[..], [Token::While { line: 1 }]);
    }

    #[test]
    fn scan_identifier() {
        let toks = scan("first _snd _t_r2d_3 a");
        assert_eq!(toks.len(), 4);
        assert_eq!(toks[0], Token::Identifier { name: String::from("first"), line: 1, });

        assert_eq!(toks[1], Token::Identifier { name: String::from("_snd"), line: 1, });

        assert_eq!(toks[2], Token::Identifier { name: String::from("_t_r2d_3"), line: 1, });

        assert_eq!(toks[3], Token::Identifier { name: String::from("a"), line: 1, });
    }

    #[test]
    fn scan_num_literal() {
        let toks = scan("0");
        assert_eq!(toks[..], [Token::NumLit { value: 0_f64, line: 1 }]);

        let toks = scan("128");
        assert_eq!(toks[..], [Token::NumLit { value: 128_f64, line: 1 }]);

        let toks = scan("1+2");
        assert_eq!(
            toks[..],
            [
                Token::NumLit { value: 1_f64, line: 1 },
                Token::Plus   { line: 1 },
                Token::NumLit { value: 2_f64, line: 1 },
            ]
            );

        let toks = scan("1.0");
        assert_eq!(toks[..], [Token::NumLit { value: 1.0, line: 1 }]);

        let toks = scan("-1.0");
        assert_eq!(
            toks[..],
            [
                Token::Minus  { line: 1 },
                Token::NumLit { value: 1.0_f64, line: 1 },
            ]
        );

        let toks = scan("- 2.0");
        assert_eq!(
            toks[..],
            [
                Token::Minus  { line: 1 },
                Token::NumLit { value: 2.0_f64, line: 1 },
            ]
        );
    }

    #[test]
    fn scan_str_literal() {
        let toks = scan(&r#""hello rust""#);
        assert_eq!(toks[..], [Token::StrLit { content: String::from("hello rust"), line: 1 } ]);

        let toks = scan(r#""""#);
        assert_eq!(toks[..], [Token::StrLit { content: String::from(""), line: 1 } ]);

        let toks = scan(r#""🤔""#);
        assert_eq!(toks[..], [Token::StrLit { content: String::from("🤔"), line: 1 } ]);
    }
}
