use std::{str::Chars, iter::Peekable};

use crate::arena::{Ref, Arena};

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
    Identifier { name: Ref<String>, line: u64 },
    NumLit     { value: f64, line: u64 },
    /// The quotes aren't included in `content`
    StrLit     { content: Ref<String>, line: u64 },

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
    strings: Arena<String>,
}

pub struct ScanResult {
    pub toks: Vec<Token>,
    pub strings: Arena<String>,
}

impl<'a> Scanner<'a> {
    /// Creates a new scanner for the `src`.
    pub fn new(src: &'a str) -> Scanner<'a> {
        Scanner {
            chars: src.chars().peekable(),
            current_line: 1,
            strings: Arena::default()
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
        self.chars.peek().copied()
    }

    /// Returns the next `char` from the `src`, or `None` if we `advance`d past
    /// the last `char`.
    /// Like `peek`, it doesn't increment the internal counter.
    fn peek_next(&self) -> Option<char> {
        let mut copy = self.chars.clone();
        copy.next();
        copy.peek().copied()
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
    fn scan_str_literal(&mut self) -> Result<Token, ScanError> {
        let start_line = self.current_line;
        let mut res = String::new();
        loop {
            match self.advance() {
                None => return Err(ScanError::UnclosedStringLiteral),
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
        let str_ref = self.strings.insert(res);
        Ok(Token::StrLit {
            content: str_ref,
            line: start_line,
        })
    }

    /// Called to scan a number literal.
    /// Only returns `None` if the end of the source has been reached.
    fn scan_num_literal(&mut self, c: char) -> Result<Token, ScanError> {
        let mut collected = c.to_string();
        self.advance();
        loop {
            match self.peek() {
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

        match collected.parse::<f64>() {
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
        }
    }

    fn scan_identifier_or_keyword(&mut self, fst: char) -> Result<Token, ScanError> {
        let mut collected = fst.to_string();
        self.advance();
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
            "and"    => Ok(Token::And    { line: self.current_line }),
            "class"  => Ok(Token::Class  { line: self.current_line }),
            "else"   => Ok(Token::Else   { line: self.current_line }),
            "false"  => Ok(Token::False  { line: self.current_line }),
            "for"    => Ok(Token::For    { line: self.current_line }),
            "fun"    => Ok(Token::Fun    { line: self.current_line }),
            "if"     => Ok(Token::If     { line: self.current_line }),
            "nil"    => Ok(Token::Nil    { line: self.current_line }),
            "or"     => Ok(Token::Or     { line: self.current_line }),
            "print"  => Ok(Token::Print  { line: self.current_line }),
            "return" => Ok(Token::Return { line: self.current_line }),
            "super"  => Ok(Token::Super  { line: self.current_line }),
            "this"   => Ok(Token::This   { line: self.current_line }),
            "true"   => Ok(Token::True   { line: self.current_line }),
            "var"    => Ok(Token::Var    { line: self.current_line }),
            "while"  => Ok(Token::While  { line: self.current_line }),
            _ => {
                let name = self.strings.insert(collected);
                Ok(Token::Identifier {
                    name,
                    line: self.current_line
                })
            }
        }
    }

    pub fn scan(mut self) -> Result<ScanResult, ScanError> {
        let mut res = Vec::new();
        loop {
            self.skip_whitespace();
            let c = match self.peek() {
                Some(c) => c,
                None => break,
            };
            if c.is_ascii_alphabetic() || c == '_' {
                res.push(self.scan_identifier_or_keyword(c)?);
                continue;
            };
            if c.is_ascii_digit() {
               res.push(self.scan_num_literal(c)?);
               continue;
            };
            self.advance();
            match c {
                '(' => res.push(Token::LParen { line: self.current_line }),
                ')' => res.push(Token::RParen { line: self.current_line }),
                '{' => res.push(Token::LBrace { line: self.current_line }),
                '}' => res.push(Token::RBrace { line: self.current_line }),
                ';' => res.push(Token::Semicolon { line: self.current_line }),
                ',' => res.push(Token::Comma { line: self.current_line }),
                '.' => res.push(Token::Dot { line: self.current_line }),
                '-' => res.push(Token::Minus { line: self.current_line }),
                '+' => res.push(Token::Plus { line: self.current_line }),
                '/' => res.push(Token::Slash { line: self.current_line }),
                '*' => res.push(Token::Star { line: self.current_line }),
                '!' if self.matches('=') =>
                    res.push(Token::BangEql { line: self.current_line }),
                '!' => res.push(Token::Bang { line: self.current_line }),
                '=' if self.matches('=') =>
                    res.push(Token::EqlEql { line: self.current_line }),
                '=' => res.push(Token::Eql { line: self.current_line }),
                '<' if self.matches('=') =>
                    res.push(Token::LessEql { line: self.current_line }),
                '<' => res.push(Token::Less { line: self.current_line }),
                '>' if self.matches('=') =>
                    res.push(Token::GreaterEql { line: self.current_line }),
                '>' => res.push(Token::Greater { line: self.current_line }),
                '"' => res.push(self.scan_str_literal()?),
                _ => return Err(ScanError::UnknownCharacter),
            };
        }

        Ok(ScanResult {
            toks: res,
            strings: self.strings,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn scan<'a>(src: &'a str) -> ScanResult {
        Scanner::new(src).scan().unwrap()
    }

    #[test]
    fn scan_single_token() {
        let toks = scan("(");
        assert_eq!(toks.toks[..], [Token::LParen { line: 1 }]);
        let toks = scan(")");
        assert_eq!(toks.toks[..], [Token::RParen { line: 1 }]);
        let toks = scan("{");
        assert_eq!(toks.toks[..], [Token::LBrace { line: 1 }]);
        let toks = scan("}");
        assert_eq!(toks.toks[..], [Token::RBrace { line: 1 }]);
        let toks = scan(",");
        assert_eq!(toks.toks[..], [Token::Comma { line: 1 }]);
        let toks = scan(".");
        assert_eq!(toks.toks[..], [Token::Dot { line: 1 }]);
        let toks = scan("-");
        assert_eq!(toks.toks[..], [Token::Minus { line: 1 }]);
        let toks = scan("+");
        assert_eq!(toks.toks[..], [Token::Plus { line: 1 }]);
        let toks = scan(";");
        assert_eq!(toks.toks[..], [Token::Semicolon { line: 1 }]);
        let toks = scan("/");
        assert_eq!(toks.toks[..], [Token::Slash { line: 1 }]);
        let toks = scan("*");
        assert_eq!(toks.toks[..], [Token::Star { line: 1 }]);
        let toks = scan("!");
        assert_eq!(toks.toks[..], [Token::Bang { line: 1 }]);
        let toks = scan("!=");
        assert_eq!(toks.toks[..], [Token::BangEql { line: 1 }]);
        let toks = scan("=");
        assert_eq!(toks.toks[..], [Token::Eql { line: 1 }]);
        let toks = scan("==");
        assert_eq!(toks.toks[..], [Token::EqlEql { line: 1 }]);
        let toks = scan(">");
        assert_eq!(toks.toks[..], [Token::Greater { line: 1 }]);
        let toks = scan(">=");
        assert_eq!(toks.toks[..], [Token::GreaterEql { line: 1 }]);
        let toks = scan("<");
        assert_eq!(toks.toks[..], [Token::Less { line: 1 }]);
        let toks = scan("<=");
        assert_eq!(toks.toks[..], [Token::LessEql { line: 1 }]);
        let toks = scan("and");
        assert_eq!(toks.toks[..], [Token::And { line: 1 }]);
        let toks = scan("class");
        assert_eq!(toks.toks[..], [Token::Class { line: 1 }]);
        let toks = scan("else");
        assert_eq!(toks.toks[..], [Token::Else { line: 1 }]);
        let toks = scan("false");
        assert_eq!(toks.toks[..], [Token::False { line: 1 }]);
        let toks = scan("for");
        assert_eq!(toks.toks[..], [Token::For { line: 1 }]);
        let toks = scan("fun");
        assert_eq!(toks.toks[..], [Token::Fun { line: 1 }]);
        let toks = scan("if");
        assert_eq!(toks.toks[..], [Token::If { line: 1 }]);
        let toks = scan("nil");
        assert_eq!(toks.toks[..], [Token::Nil { line: 1 }]);
        let toks = scan("or");
        assert_eq!(toks.toks[..], [Token::Or { line: 1 }]);
        let toks = scan("print");
        assert_eq!(toks.toks[..], [Token::Print { line: 1 }]);
        let toks = scan("return");
        assert_eq!(toks.toks[..], [Token::Return { line: 1 }]);
        let toks = scan("super");
        assert_eq!(toks.toks[..], [Token::Super { line: 1 }]);
        let toks = scan("this");
        assert_eq!(toks.toks[..], [Token::This { line: 1 }]);
        let toks = scan("true");
        assert_eq!(toks.toks[..], [Token::True { line: 1 }]);
        let toks = scan("var");
        assert_eq!(toks.toks[..], [Token::Var { line: 1 }]);
        let toks = scan("while");
        assert_eq!(toks.toks[..], [Token::While { line: 1 }]);
    }

    #[test]
    fn scan_identifier() {
        let toks = scan("first _snd _t_r2d_3 a");
        assert_eq!(toks.toks.len(), 4);
        assert!(
            match toks.toks[0] {
                Token::Identifier { name, line: 1 } => {
                    toks.strings.get(name) == "first"
                },
                _ => false,
            }
        );
        assert!(
            match toks.toks[1] {
                Token::Identifier { name, line: 1 } => {
                    toks.strings.get(name) == "_snd"
                },
                _ => false,
            }
        );
        assert!(
            match toks.toks[2] {
                Token::Identifier { name, line: 1 } => {
                    toks.strings.get(name) == "_t_r2d_3"
                },
                _ => false,
            }
        );
        assert!(
            match toks.toks[3] {
                Token::Identifier { name, line: 1 } => {
                    toks.strings.get(name) == "a"
                },
                _ => false,
            }
        );
    }

    #[test]
    fn scan_num_literal() {
        let toks = scan("0");
        assert_eq!(toks.toks[..], [Token::NumLit { value: 0_f64, line: 1 }]);

        let toks = scan("128");
        assert_eq!(toks.toks[..], [Token::NumLit { value: 128_f64, line: 1 }]);

        let toks = scan("1+2");
        assert_eq!(
            toks.toks[..],
            [
                Token::NumLit { value: 1_f64, line: 1 },
                Token::Plus   { line: 1 },
                Token::NumLit { value: 2_f64, line: 1 },
            ]
            );

        let toks = scan("1.0");
        assert_eq!(toks.toks[..], [Token::NumLit { value: 1.0, line: 1 }]);

        let toks = scan("-1.0");
        assert_eq!(
            toks.toks[..],
            [
                Token::Minus  { line: 1 },
                Token::NumLit { value: 1.0_f64, line: 1 },
            ]
        );

        let toks = scan("- 2.0");
        assert_eq!(
            toks.toks[..],
            [
                Token::Minus  { line: 1 },
                Token::NumLit { value: 2.0_f64, line: 1 },
            ]
        );
    }

    #[test]
    fn scan_str_literal() {
        let toks = scan(&r#""hello rust""#);
        assert!(
            match toks.toks[0] {
                Token::StrLit { content, line: 1 } => {
                    toks.strings.get(content) == "hello rust"
                },
                _ => false,
            }
        );
        let toks = scan(r#""""#);
        assert!(
            match toks.toks[0] {
                Token::StrLit { content, line: 1 } => {
                    toks.strings.get(content) == ""
                },
                _ => false,
            }
        );
        let toks = scan(r#""🤔""#);
        assert!(
            match toks.toks[0] {
                Token::StrLit { content, line: 1 } => {
                    toks.strings.get(content) == "🤔"
                },
                _ => false,
            }
        );
    }
}
