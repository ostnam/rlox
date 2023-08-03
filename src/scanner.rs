/// Identifiers and strings store a &str of the source string, hence the lifetime.
#[derive(Debug, PartialEq)]
pub enum Token<'a> {
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
    Identifier { name: &'a [char], line: u64 },
    NumLit     { value: f64, line: u64 },
    /// The quotes aren't included in `content`
    StrLit     { content: &'a [char], line: u64 },

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

pub struct Scanner<'a> {
    src: &'a [char],
    pos: usize,
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
    fn new(src: &'a [char]) -> Result<Scanner<'a>, ScannerInitError> {
        if src.len() == usize::MAX {
            Err(ScannerInitError::InputTooLong)
        } else {
            Ok(Scanner { src, pos: 0, current_line: 1 })
        }
    }

    /// Returns the next `char` from the `src`, or `None` if we `advance`d past
    /// the last `char`.
    /// The next time `advance` is called, the following `char` will be returned.
    fn advance(&mut self) -> Option<&'a char> {
        self.pos += 1;
        self.src.get(self.pos - 1)
    }

    /// Returns the next `char` from the `src`, or `None` if we `advance`d past
    /// the last `char`.
    /// Unlike `advance`, it doesn't increment the internal counter, and calling
    /// `peek` repeatedly returns the same character (or `None`) at every call.
    fn peek(&self) -> Option<&'a char> {
        self.src.get(self.pos)
    }

    /// Returns the next `char` from the `src`, or `None` if we `advance`d past
    /// the last `char`.
    /// Like `peek`, it doesn't increment the internal counter.
    fn peek_next(&self) -> Option<&'a char> {
        self.src.get(self.pos + 1)
    }

    /// Decrements the internal counter: calling `advance` will return the same
    /// char as the previous call.
    /// If we're at the first char, do nothing.
    fn unwind(&mut self) {
        if self.pos > 0 {
            self.pos -= 1
        }
    }

    /// If the next char is equal to the one passed, returns true and consumes
    /// it, if not do nothing.
    fn matches(&mut self, c: char) -> bool {
        match self.src.get(self.pos) {
            Some(d) if *d == c => {
                self.pos += 1;
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
    /// The leading " must not have been matched.
    /// Only returns `None` if the end of the source has been reached.
    fn scan_str_literal(&mut self) -> Option<Result<Token<'a>, ScanError>> {
        let start_line = self.current_line;
        match self.advance() {
            None => return None,
            Some('"') => (),
            Some(_) => return Some(Err(ScanError::Bug{
                details: "Error parsing string literal.".to_string(),
                line: self.current_line,
            })),
        }
        let start_pos = self.pos;
        loop {
            match self.peek() {
                None => return Some(Err(ScanError::UnclosedStringLiteral)),
                Some('\n') => {
                    self.current_line += 1;
                    self.advance();
                }
                Some('"') => {
                    self.advance();
                    break;
                },
                Some(_) => {
                    self.advance();
                },
            }
        }
        Some(Ok(Token::StrLit {
            content: &self.src[start_pos..self.pos-1],
            line: start_line,
        }))
    }

    /// Called to scan a number literal.
    /// Only returns `None` if the end of the source has been reached.
    fn scan_num_literal(&mut self) -> Option<Result<Token<'a>, ScanError>> {
        let starting_pos = self.pos;
        loop {
            match self.peek() {
                None if starting_pos == self.pos => 
                    return Some(Err(ScanError::Bug{
                        details: "Error scanning int".to_string(),
                        line: self.current_line,
                    })),
                None => break,
                Some(c) if c.is_ascii_digit() => {
                    self.advance();
                }
                Some(_) => break,
            }
        }
        match (self.peek(), self.peek_next()) {
            (Some('.'), Some(c)) if c.is_ascii_digit() => {
                self.advance();
                self.advance();
                loop {
                    match self.peek() {
                        None => break,
                        Some(c) if c.is_ascii_digit() => {
                            self.advance();
                        }
                        Some(_) => break,
                    }
                }
            }
            _ => (),
        }

        let num_str: String = self.src[starting_pos..self.pos].iter().collect();
        Some(match num_str.parse::<f64>() {
            Ok(num) => Ok(Token::NumLit {
                value: num,
                line: self.current_line,
            }),
            Err(e) => Err(ScanError::Bug {
                details: format!(
                    "Error parsing slice representing number: {} to f64: {}",
                    num_str,
                    e,
                ),
                line: self.current_line,
            }),
        })
    }

    fn scan_identifier_or_keyword(&mut self) -> Option<Result<Token<'a>, ScanError>> {
        let starting_pos = self.pos;
        match self.peek() {
            None => return None,
            Some(c) if c.is_ascii_alphabetic() || *c == '_' => {
                self.advance();
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
                Some(c) if c.is_ascii_alphanumeric() || *c == '_' => {
                    self.advance();
                },
                Some(_) | None => break,
            }
        }
        match self.src[starting_pos..self.pos].iter().collect::<String>().as_str() {
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
                name: &self.src[starting_pos..self.pos],
                line: self.current_line
            })),
        }
    }
}

impl<'a> std::iter::Iterator for Scanner<'a> {
    type Item = Result<Token<'a>, ScanError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
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
                self.unwind();
                self.scan_str_literal()
            }
            c if c.is_ascii_alphabetic() || *c == '_' => {
                self.unwind();
                self.scan_identifier_or_keyword()
            }
            c if c.is_ascii_digit() => {
                self.unwind();
                self.scan_num_literal()
            }
            _ => Some(Err(ScanError::UnknownCharacter))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn scan<'a>(src: &'a [char]) -> Vec<Token<'a>> {
        Scanner::new(src)
            .unwrap()
            .map(|r| r.unwrap())
            .collect()
    }

    #[test]
    fn scan_lparen() {
        let src: Vec<_> = "(".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::LParen { line: 1 }])
    }

    #[test]
    fn scan_rparen() {
        let src: Vec<_> = ")".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::RParen { line: 1 }])
    }

    #[test]
    fn scan_lbrace() {
        let src: Vec<_> = "{".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::LBrace { line: 1 }])
    }

    #[test]
    fn scan_rbrace() {
        let src: Vec<_> = "}".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::RBrace { line: 1 }])
    }

    #[test]
    fn scan_comma() {
        let src: Vec<_> = ",".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::Comma { line: 1 }])
    }

    #[test]
    fn scan_dot() {
        let src: Vec<_> = ".".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::Dot { line: 1 }])
    }

    #[test]
    fn scan_minus() {
        let src: Vec<_> = "-".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::Minus { line: 1 }])
    }

    #[test]
    fn scan_plus() {
        let src: Vec<_> = "+".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::Plus { line: 1 }])
    }

    #[test]
    fn scan_semicolon() {
        let src: Vec<_> = ";".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::Semicolon { line: 1 }])
    }

    #[test]
    fn scan_slash() {
        let src: Vec<_> = "/".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::Slash { line: 1 }])
    }

    #[test]
    fn scan_star() {
        let src: Vec<_> = "*".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::Star { line: 1 }])
    }

    #[test]
    fn scan_bang() {
        let src: Vec<_> = "!".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::Bang { line: 1 }])
    }

    #[test]
    fn scan_bangeql() {
        let src: Vec<_> = "!=".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::BangEql { line: 1 }])
    }

    #[test]
    fn scan_eql() {
        let src: Vec<_> = "=".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::Eql { line: 1 }])
    }

    #[test]
    fn scan_eqleql() {
        let src: Vec<_> = "==".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::EqlEql { line: 1 }])
    }

    #[test]
    fn scan_greater() {
        let src: Vec<_> = ">".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::Greater { line: 1 }])
    }

    #[test]
    fn scan_greatereql() {
        let src: Vec<_> = ">=".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::GreaterEql { line: 1 }])
    }

    #[test]
    fn scan_less() {
        let src: Vec<_> = "<".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::Less { line: 1 }])
    }

    #[test]
    fn scan_lesseql() {
        let src: Vec<_> = "<=".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::LessEql { line: 1 }])
    }

    #[test]
    fn scan_identifier() {
        let src: Vec<_> = "first _snd _t_r2d_3 a".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks.len(), 4);
        let fst: Vec<_> = "first".chars().collect();
        let fst_chars = &fst;
        assert_eq!(toks[0], Token::Identifier { name: fst_chars, line: 1, });

        let snd: Vec<_> = "_snd".chars().collect();
        let snd_chars = &snd;
        assert_eq!(toks[1], Token::Identifier { name: snd_chars, line: 1, });

        let trd: Vec<_> = "_t_r2d_3".chars().collect();
        let trd_chars = &trd;
        assert_eq!(toks[2], Token::Identifier { name: trd_chars, line: 1, });

        let a: Vec<_> = "a".chars().collect();
        let a_chars = &a;
        assert_eq!(toks[3], Token::Identifier { name: a_chars, line: 1, });
    }

    #[test]
    fn scan_num_literal() {
        let src: Vec<_> = "0".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::NumLit { value: 0_f64, line: 1 }]);

        let src: Vec<_> = "128".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::NumLit { value: 128_f64, line: 1 }]);

        let src: Vec<_> = "1+2".chars().collect();
        let toks = scan(&src);
        assert_eq!(
            toks[..],
            [
                Token::NumLit { value: 1_f64, line: 1 },
                Token::Plus   { line: 1 },
                Token::NumLit { value: 2_f64, line: 1 },
            ]
            );

        let src: Vec<_> = "1.0".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::NumLit { value: 1.0, line: 1 }]);

        let src: Vec<_> = "-1.0".chars().collect();
        let toks = scan(&src);
        assert_eq!(
            toks[..],
            [
                Token::Minus  { line: 1 },
                Token::NumLit { value: 1.0_f64, line: 1 },
            ]
        );

        let src: Vec<_> = "- 2.0".chars().collect();
        let toks = scan(&src);
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
        let src: Vec<_> = r#""hello rust""#.chars().collect();
        let toks = scan(&src);
        let expected_str_vec: Vec<_> = "hello rust".chars().collect();
        assert_eq!(toks[..], [Token::StrLit { content: &expected_str_vec, line: 1 } ]);

        let src: Vec<_> = r#""""#.chars().collect();
        let toks = scan(&src);
        let expected_str_vec: Vec<_> = "".chars().collect();
        assert_eq!(toks[..], [Token::StrLit { content: &expected_str_vec, line: 1 } ]);

        let src: Vec<_> = r#""🤔""#.chars().collect();
        let toks = scan(&src);
        let expected_str_vec: Vec<_> = "🤔".chars().collect();
        assert_eq!(toks[..], [Token::StrLit { content: &expected_str_vec, line: 1 } ]);
    }

    #[test]
    fn scan_and() {
        let src: Vec<_> = "and".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::And { line: 1 }])
    }

    #[test]
    fn scan_class() {
        let src: Vec<_> = "class".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::Class { line: 1 }])
    }

    #[test]
    fn scan_else() {
        let src: Vec<_> = "else".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::Else { line: 1 }])
    }

    #[test]
    fn scan_false() {
        let src: Vec<_> = "false".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::False { line: 1 }])
    }

    #[test]
    fn scan_for() {
        let src: Vec<_> = "for".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::For { line: 1 }])
    }

    #[test]
    fn scan_fun() {
        let src: Vec<_> = "fun".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::Fun { line: 1 }])
    }

    #[test]
    fn scan_if() {
        let src: Vec<_> = "if".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::If { line: 1 }])
    }

    #[test]
    fn scan_nil() {
        let src: Vec<_> = "nil".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::Nil { line: 1 }])
    }

    #[test]
    fn scan_or() {
        let src: Vec<_> = "or".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::Or { line: 1 }])
    }

    #[test]
    fn scan_print() {
        let src: Vec<_> = "print".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::Print { line: 1 }])
    }

    #[test]
    fn scan_return() {
        let src: Vec<_> = "return".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::Return { line: 1 }])
    }

    #[test]
    fn scan_super() {
        let src: Vec<_> = "super".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::Super { line: 1 }])
    }

    #[test]
    fn scan_this() {
        let src: Vec<_> = "this".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::This { line: 1 }])
    }

    #[test]
    fn scan_true() {
        let src: Vec<_> = "true".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::True { line: 1 }])
    }

    #[test]
    fn scan_var() {
        let src: Vec<_> = "var".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::Var { line: 1 }])
    }

    #[test]
    fn scan_while() {
        let src: Vec<_> = "while".chars().collect();
        let toks = scan(&src);
        assert_eq!(toks[..], [Token::While { line: 1 }])
    }
}
