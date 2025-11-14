//! Parsing System
//!
//! Token-based parsing with specialized mini-parsers

pub mod token;
pub mod lexer;

pub use token::{Token, TokenType};
pub use lexer::Lexer;

/// Simple base parse error for generic parsing failures
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    EmptyInput,
    NoValidParser { context: String },
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::EmptyInput => write!(f, "Empty input"),
            ParseError::NoValidParser { context } => write!(f, "No valid parser found: {}", context),
        }
    }
}

impl std::error::Error for ParseError {}

