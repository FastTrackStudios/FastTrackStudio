//! Chord parsing module
//! 
//! Parses chord symbols like "Cmaj7", "Dmin7b5", "G7sus4", etc.

pub mod ast;
pub mod expressions;
pub mod expression;
pub mod parser_error;
pub mod mini_parser;

// Old files removed: lexer.rs, parser.rs, token_translator.rs
// They have been replaced by mini_parser.rs

pub use parser_error::ChordParserError;
pub use mini_parser::ChordMiniParser;
