#[derive(Debug, Clone, PartialEq, Eq)]
#[repr(u8)]
pub enum TokenType {
    // Basic character tokens
    Letter(char),      // Single alphabetic character (a-z, A-Z)
    Digit(char),       // Single numeric character (0-9)
    Number(String),    // Consecutive digits (for convenience)
    
    // Symbol tokens
    Sharp,             // # or ♯
    Flat,              // b or ♭ (context determines if 'b' is flat or note B)
    Slash,             // /
    Plus,              // +
    Minus,             // -
    Apostrophe,        // '
    Exclamation,       // !
    Underscore,        // _
    
    // Special symbols
    Triangle,          // △
    Circle,            // ° or ø
    LParen,
    RParen,
    Comma,
    LBracket,
    RBracket,
    
    // Whitespace
    Whitespace,
    
    // End markers
    Illegal,
    Eof,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Letter(c) => write!(f, "{}", c)?,
            TokenType::Digit(c) => write!(f, "{}", c)?,
            TokenType::Number(num) => f.write_str(num)?,
            TokenType::Sharp => f.write_str("#")?,
            TokenType::Flat => f.write_str("b")?,
            TokenType::Slash => f.write_str("/")?,
            TokenType::Plus => f.write_str("+")?,
            TokenType::Minus => f.write_str("-")?,
            TokenType::Apostrophe => f.write_str("'")?,
            TokenType::Exclamation => f.write_str("!")?,
            TokenType::Underscore => f.write_str("_")?,
            TokenType::Triangle => f.write_str("△")?,
            TokenType::Circle => f.write_str("°")?,
            TokenType::LParen => f.write_str("(")?,
            TokenType::RParen => f.write_str(")")?,
            TokenType::Comma => f.write_str(",")?,
            TokenType::LBracket => f.write_str("[")?,
            TokenType::RBracket => f.write_str("]")?,
            TokenType::Whitespace => f.write_str(" ")?,
            TokenType::Illegal => f.write_str("ILLEGAL")?,
            TokenType::Eof => f.write_str("EOF")?,
        }
        Ok(())
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub token_type: TokenType,
    pub pos: usize,
    pub len: usize,
}

impl Token {
    pub fn new(token_type: TokenType, pos: usize, len: usize) -> Token {
        Token {
            token_type,
            pos,
            len,
        }
    }
}

use std::fmt::Display;

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}", self.token_type))?;
        Ok(())
    }
}
