use crate::error_handler::error;
use core::fmt;
use std::collections::HashMap;

use super::library_constructs::SpecialConstants;

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

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}]: {} {}", self.line_no, self.literal, self.lexeme)
    }
}

impl Token {

    pub(super) fn generate_token(tok_type: TokenType, line_no: usize) -> Token {
        let tok_str = tok_type_string(tok_type);
        Token { tok_type, lexeme: tok_str.clone(), literal: tok_str.clone(), line_no }
    }

    pub(super) fn is_bitwise_operator(&self) -> bool {
        matches!(self.tok_type, 
            TokenType::BitwiseAnd |
            TokenType::BitwiseOr |
            TokenType::BitwiseXor |
            TokenType::And |
            TokenType::Or
        )
    }

    pub(super) fn is_float_returning_operator(&self) -> bool {
        matches!(self.tok_type,
            TokenType::Plus |
            TokenType::Star |
            TokenType::Minus |
            TokenType::Slash
        )
    }

    pub(super) fn is_alphaneumeric(&self) -> bool {
        self.lexeme.chars().all(Scanner::is_alphaneumeric)
    }

}

struct Scanner {
    start: usize,
    current: usize,
    line_no: usize,
    tokens: Vec<Token>,
    source: String,
    keywords: HashMap<String, TokenType>,
}

impl Scanner {
    fn scan_token(&mut self) {
        let c = self.next_char();
        match c {
            '#' => while self.peek() != '\n' && !self.is_at_eof() { self.next_char(); },
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            '[' => self.add_token(TokenType::LeftSquareBrace),
            ']' => self.add_token(TokenType::RightSquareBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            ';' => self.add_token(TokenType::Semicolon),
            '~' => self.add_token(TokenType::Tilde),
            '&' => { 
                let next = self.peek();
                match next {
                    '&' => {
                        self.add_token(TokenType::And);
                        self.next_char();
                    },
                    '=' => {
                        self.add_token(TokenType::AndEqual);
                        self.next_char();
                    },
                    _ => self.add_token(TokenType::BitwiseAnd)
                }
            },
            '|' => { 
                let next = self.peek();
                match next {
                    '|' => {
                        self.add_token(TokenType::Or);
                        self.next_char();
                    },
                    '=' => {
                        self.add_token(TokenType::OrEqual);
                        self.next_char();
                    },
                    _ => self.add_token(TokenType::BitwiseOr),
                }
            },
            '^' => { 
                let tok_type = if self.match_char('=') { TokenType::XorEqual } else { TokenType::BitwiseXor };
                self.add_token(tok_type) 
            },
            '%' => { 
                let tok_type = if self.match_char('=') { TokenType::PercentEqual } else { TokenType::Percent };
                self.add_token(tok_type) 
            },
            '*' => { 
                let tok_type = if self.match_char('=') { TokenType::StarEqual } else { TokenType::Star };
                self.add_token(tok_type) 
            },
            '+' => { 
                let tok_type = if self.match_char('=') { TokenType::PlusEqual } else { TokenType::Plus };
                self.add_token(tok_type) 
            },
            '-' => { 
                let tok_type = if self.match_char('=') { TokenType::MinusEqual } else { TokenType::Minus };
                self.add_token(tok_type)
            },
            '!' => { 
                let tok_type = if self.match_char('=') { TokenType::BangEqual } else { TokenType::Bang };
                self.add_token(tok_type);
            },
            '=' => { 
                let tok_type = if self.match_char('=') { TokenType::EqualEqual } else { TokenType::Equal };
                self.add_token(tok_type);
            },
            '<' => { 
                let next = self.peek();
                match next {
                    '=' => { 
                        self.add_token(TokenType::LessEqual);
                        self.next_char();
                    }
                    '<' => { 
                        self.next_char();
                        let tok_type = if self.match_char('=') { TokenType::LeftShiftEqual } else { TokenType::LeftShift };
                        self.add_token(tok_type);
                    }
                    _ => self.add_token(TokenType::Less)
                }
            },
            '>' => { 
                let next = self.peek();
                match next {
                    '=' => { 
                        self.add_token(TokenType::GreaterEqual);
                        self.next_char();
                    }
                    '>' => { 
                        self.next_char();
                        let tok_type = if self.match_char('=') { TokenType::RightShiftEqual } else { TokenType::RightShift };
                        self.add_token(tok_type);
                    }
                    _ => self.add_token(TokenType::Greater)
                }
            },
            '/' => {

                let next = self.peek();
                // Advance through comment
                match next {
                    '/' => while self.peek() != '\n' && !self.is_at_eof() { self.next_char(); },
                    '=' => {
                        self.add_token(TokenType::SlashEqual);
                        self.next_char();
                    }, 
                    _ => self.add_token(TokenType::Slash)
                }
            }
            '"' => self.parse_string(),
            '\n' => self.line_no += 1,
            ' ' | '\r' | '\t' => (),
            _ => {
                if Scanner::is_digit(c) {
                    self.parse_number();
                }
                else if Scanner::is_alpha(c) {
                    self.parse_identifier();
                }
                else {
                    error(self.line_no, format!("Unkown character {}", c));
                }
            }
        }
    } 

    fn parse_identifier(&mut self) {
        while Scanner::is_alphaneumeric(self.peek()) { self.next_char(); }

        let text = String::from(&self.source[self.start..self.current]);
        let tok_type = self.keywords.get(&text);
        match tok_type {
            Some(t) => {
                if *t == TokenType::Printf {
                    while self.peek() != '\n' && !self.is_at_eof() { self.next_char(); }
                }
                else {
                    self.add_token(*t);
                }
            },
            None =>  {
                if SpecialConstants::is_mips_item_variant(&text) || SpecialConstants::is_mips_type_variant(&text) {
                    self.add_token(TokenType::String)
                }
                else {
                    self.add_token(TokenType::Identifier)
                }
            },
        }
    }

    fn is_alphaneumeric(c: char) -> bool {
        Scanner::is_alpha(c) || Scanner::is_digit(c)
    }

    fn is_alpha(c: char) -> bool {
        c.is_ascii_lowercase() ||
               c.is_ascii_uppercase() ||
                c == '_'
    }

    fn is_digit(c: char) -> bool {
        c.is_ascii_digit()
    }

    fn parse_number(&mut self) {
        // First part of decimal
        while Scanner::is_digit(self.peek()) { self.next_char(); }
        if self.peek() == '.' && Scanner::is_digit(self.peek_next()) {
            self.next_char();

            // Second part of decimal
            while Scanner::is_digit(self.peek()) { self.next_char(); }
        }
        self.add_token(TokenType::Number)
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
        self.add_token(TokenType::String)
    }

    fn next_char(&mut self) -> char {
        let c = self.source.chars().nth(self.current).unwrap();
        self.current += 1;
        c
    }

    fn add_token(&mut self, tok_type: TokenType) {
        let new_token = Token {
            tok_type,
            lexeme: String::from(&(self.source)[self.start..self.current]),
            literal: String::from(&(self.source)[self.start..self.current]),
            line_no: self.line_no
        };
        self.tokens.push(new_token);
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_eof() { return false };
        if self.source.chars().nth(self.current).unwrap() != expected { return false };

        self.current += 1;
        true
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
        self.current == self.source.len()
    }

    #[allow(dead_code)]
    fn print_tokens(&self) {
        for token in self.tokens.iter() {
            println!("{}", token);
        }
    }
}

fn init_keywords() -> HashMap<String, TokenType> {
    HashMap::from([
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
    ])
}

pub fn scan_tokens(source: String) -> Vec<Token> {
    let mut scanner = Scanner {
        start: 0, 
        current: 0,
        line_no: 1,
        tokens: Vec::new(),
        source,
        keywords: init_keywords(),
    };
    while !scanner.is_at_eof() {
        scanner.start = scanner.current;
        scanner.scan_token();
    }
    scanner.tokens.push(Token {tok_type: TokenType::Eof, lexeme: String::from(""), literal: String::from(""), line_no: scanner.line_no});
    scanner.print_tokens();
    scanner.tokens
}