use crate::error_handler::error;
use std::collections::HashMap;

#[derive(PartialEq)]
#[derive(Clone)]
#[derive(Copy)]
#[derive(Hash)]
#[derive(Eq)]
pub enum TokenType {
    LeftParen, 
    RightParen,
    LeftBrace,
    RightBrace,
    LeftSquareBrace,
    RightSquareBrace,
    Comma, 
    Dot,
    Minus, Plus, Semicolon, Slash, Star, Percent,

    Bang, 
    BangEqual,
    Equal, 
    EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,
    AndEqual, OrEqual,
    PlusEqual, MinusEqual,
    StarEqual, SlashEqual,
    XorEqual,
    PercentEqual,
    LeftShiftEqual, RightShiftEqual,

    Identifier, String, Number,
    
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    Tilde,
    LeftShift,
    RightShift,
    And, 
    Else,
    True,
    False,
    For, 
    If,
    Or,
    Return, 
    While,
    Int, Float, Double, Char, Signed, Long, Printf,

    Eof,
}

pub(super) fn tok_type_string(tok_type: TokenType) -> String {
    match tok_type {
        TokenType::BitwiseAnd => String::from("&"),
        TokenType::BitwiseOr => String::from("|"),
        TokenType::Plus => String::from("+"),
        TokenType::Minus => String::from("-"),
        TokenType::Star => String::from("*"),
        TokenType::Slash => String::from("/"),
        TokenType::Percent => String::from("%"),
        TokenType::BitwiseXor => String::from("^"),
        TokenType::RightShiftEqual => String::from("[]"),
        _ => String::from("")
    }
}

#[derive(Clone)]
pub struct Token {
    pub tok_type: TokenType,
    pub lexeme: String,
    pub literal: String,
    pub line_no: usize
}

impl Token {
    fn to_string(&self) -> String {
        return format!("[{}]: {} {}", self.line_no, self.literal, self.lexeme);
    }

    pub(super) fn generate_token(tok_type: TokenType, line_no: usize) -> Token {
        let tok_str = tok_type_string(tok_type.clone());
        Token { tok_type, lexeme: tok_str.clone(), literal: tok_str.clone(), line_no }
    }

    pub(super) fn is_bitwise_operator(&self) -> bool {
        match self.tok_type {
            TokenType::BitwiseAnd |
            TokenType::BitwiseOr |
            TokenType::BitwiseXor |
            TokenType::And |
            TokenType::Or => true,
            _ => false
        }
    }

    pub(super) fn is_float_returning_operator(&self) -> bool {
        match self.tok_type {
            TokenType::Plus |
            TokenType::Star |
            TokenType::Minus |
            TokenType::Slash => true,
            _ => false 
        }
    }
}

struct Scanner {
    start: usize,
    current: usize,
    line_no: usize,
    tokens: Vec<Token>,
    source: String,
    keywords: HashMap<String, TokenType>
}

impl Scanner {
    fn scan_token(&mut self) {
        let c = self.next_char();
        match c {
            '#' => while self.peek() != '\n' && !self.is_at_eof() { self.next_char(); },
            '(' => self.add_token(TokenType::LeftParen, c.to_string()),
            ')' => self.add_token(TokenType::RightParen, c.to_string()),
            '{' => self.add_token(TokenType::LeftBrace, c.to_string()),
            '}' => self.add_token(TokenType::RightBrace, c.to_string()),
            '[' => self.add_token(TokenType::LeftSquareBrace, c.to_string()),
            ']' => self.add_token(TokenType::RightSquareBrace, c.to_string()),
            ',' => self.add_token(TokenType::Comma, c.to_string()),
            '.' => self.add_token(TokenType::Dot, c.to_string()),
            ';' => self.add_token(TokenType::Semicolon, c.to_string()),
            '~' => self.add_token(TokenType::Tilde, c.to_string()),
            '&' => { 
                let next = self.peek();
                match next {
                    '&' => {
                        self.add_token(TokenType::And, c.to_string());
                        self.next_char();
                    },
                    '=' => {
                        self.add_token(TokenType::AndEqual, c.to_string());
                        self.next_char();
                    },
                    _ => self.add_token(TokenType::BitwiseAnd, c.to_string())
                }
            },
            '|' => { 
                let next = self.peek();
                match next {
                    '|' => {
                        self.add_token(TokenType::Or, c.to_string());
                        self.next_char();
                    },
                    '=' => {
                        self.add_token(TokenType::OrEqual, c.to_string());
                        self.next_char();
                    },
                    _ => self.add_token(TokenType::BitwiseOr, c.to_string()),
                }
            },
            '^' => { 
                let tok_type = if self.match_char('=') { TokenType::XorEqual } else { TokenType::BitwiseXor };
                self.add_token(tok_type, c.to_string()) 
            },
            '%' => { 
                let tok_type = if self.match_char('=') { TokenType::PercentEqual } else { TokenType::Percent };
                self.add_token(tok_type, c.to_string()) 
            },
            '*' => { 
                let tok_type = if self.match_char('=') { TokenType::StarEqual } else { TokenType::Star };
                self.add_token(tok_type, c.to_string()) 
            },
            '+' => { 
                let tok_type = if self.match_char('=') { TokenType::PlusEqual } else { TokenType::Plus };
                self.add_token(tok_type, c.to_string()) 
            },
            '-' => { 
                let tok_type = if self.match_char('=') { TokenType::MinusEqual } else { TokenType::Minus };
                self.add_token(tok_type, c.to_string())
            },
            '!' => { 
                let tok_type = if self.match_char('=') { TokenType::BangEqual } else { TokenType::Bang };
                self.add_token(tok_type, c.to_string());
            },
            '=' => { 
                let tok_type = if self.match_char('=') { TokenType::EqualEqual } else { TokenType::Equal };
                self.add_token(tok_type, c.to_string());
            },
            '<' => { 
                let next = self.peek();
                match next {
                    '=' => { 
                        self.add_token(TokenType::LessEqual, c.to_string());
                        self.next_char();
                    }
                    '<' => { 
                        self.next_char();
                        let tok_type = if self.match_char('=') { TokenType::LeftShiftEqual } else { TokenType::LeftShift };
                        self.add_token(tok_type, c.to_string());
                    }
                    _ => self.add_token(TokenType::Less, c.to_string())
                }
            },
            '>' => { 
                let next = self.peek();
                match next {
                    '=' => { 
                        self.add_token(TokenType::GreaterEqual, c.to_string());
                        self.next_char();
                    }
                    '>' => { 
                        self.next_char();
                        let tok_type = if self.match_char('=') { TokenType::RightShiftEqual } else { TokenType::RightShift };
                        self.add_token(tok_type, c.to_string());
                    }
                    _ => self.add_token(TokenType::Greater, c.to_string())
                }
            },
            '/' => {

                let next = self.peek();
                // Advance through comment
                match next {
                    '/' => while self.peek() != '\n' && !self.is_at_eof() { self.next_char(); },
                    '=' => {
                        self.add_token(TokenType::SlashEqual, c.to_string());
                        self.next_char();
                    }, 
                    _ => self.add_token(TokenType::Slash, c.to_string())
                }
            }
            '"' => self.parse_string(),
            '\n' => self.line_no += 1,
            ' ' | '\r' | '\t' => (),
            _ => {
                if self.is_digit(c) {
                    self.parse_number();
                }
                else if self.is_alpha(c) {
                    self.parse_identifier();
                }
                else {
                    error(self.line_no, format!("Unkown character {}", c));
                }
            }
        }
    } 

    fn parse_identifier(&mut self) {
        while self.is_alphaneumeric(self.peek()) { self.next_char(); }

        let text = String::from(&self.source[self.start..self.current]);
        let tok_type = self.keywords.get(&text);
        match tok_type {
            Some(t) => {
                if *t == TokenType::Printf {
                    while self.peek() != '\n' && !self.is_at_eof() { self.next_char(); }
                }
                else {
                    self.add_token(t.clone(), text);
                }
            },
            None => self.add_token(TokenType::Identifier, text),
        }
    }

    fn is_alphaneumeric(&self, c: char) -> bool {
        return self.is_alpha(c) || self.is_digit(c);
    }

    fn is_alpha(&self, c: char) -> bool {
        return (c >= 'a' && c <= 'z') ||
               (c >= 'A' && c <= 'Z') ||
                c == '_';
    }

    fn is_digit(&self, c: char) -> bool {
        return c >= '0' && c <= '9';
    }

    fn parse_number(&mut self) {
        // First part of decimal
        while self.is_digit(self.peek()) { self.next_char(); }
        if self.peek() == '.' && self.is_digit(self.peek_next()) {
            self.next_char();

            // Second part of decimal
            while self.is_digit(self.peek()) { self.next_char(); }
        }
        self.add_token(TokenType::Number, String::from(&self.source[self.start..self.current]))
    }

    fn parse_string(&mut self) {
        while self.peek() != '"' && !self.is_at_eof() {
            if self.peek() == '\n' { self.line_no += 1 }
            self.next_char();
        }

        if self.is_at_eof() {
            error(self.line_no, String::from("Unterminated string."))
        }

        self.next_char();
        let value = &self.source[self.start + 1..self.current - 1];
        self.add_token(TokenType::String, String::from(value))
    }

    fn next_char(&mut self) -> char {
        let c = self.source.chars().nth(self.current).unwrap();
        self.current += 1;
        return c;
    }

    fn add_token(&mut self, tok_type: TokenType, literal: String) {
        let new_token = Token {
            tok_type,
            lexeme: String::from(&(self.source)[self.start..self.current]),
            literal,
            line_no: self.line_no
        };
        self.tokens.push(new_token);
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_eof() { return false };
        if self.source.chars().nth(self.current).unwrap() != expected { return false };

        self.current += 1;
        return true; 
    }

    fn peek(&self) -> char {
        if self.is_at_eof() { return '\0' };
        return self.source.chars().nth(self.current).unwrap();
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() { return '\0'; }
        return self.source.chars().nth(self.current + 1).unwrap();
    }

    fn is_at_eof(&self) -> bool {
        return self.current == self.source.len();
    }

    #[allow(dead_code)]
    fn print_tokens(&self) {
        for token in self.tokens.iter() {
            println!("{}", token.to_string());
        }
    }
}

fn init_keywords() -> HashMap<String, TokenType> {
    return HashMap::from([
        (String::from("if"), TokenType::If),
        (String::from("else"), TokenType::Else),
        (String::from("while"), TokenType::While),
        (String::from("for"), TokenType::For),
        (String::from("return"), TokenType::Return),
        (String::from("true"), TokenType::True),
        (String::from("false"), TokenType::False),
        (String::from("int"), TokenType::Int),
        (String::from("float"), TokenType::Float),
        (String::from("double"), TokenType::Double),
        (String::from("signed"), TokenType::Signed),
        (String::from("char"), TokenType::Char),
        (String::from("long"), TokenType::Long),
        (String::from("printf"), TokenType::Printf)
    ]);
}

pub fn scan_tokens(source: String) -> Vec<Token> {
    let mut scanner = Scanner {
        start: 0, 
        current: 0,
        line_no: 1,
        tokens: Vec::new(),
        source,
        keywords: init_keywords()
    };
    while !scanner.is_at_eof() {
        scanner.start = scanner.current;
        scanner.scan_token();
    }
    scanner.tokens.push(Token {tok_type: TokenType::Eof, lexeme: String::from(""), literal: String::from(""), line_no: scanner.line_no});
    scanner.print_tokens();
    return scanner.tokens;
}