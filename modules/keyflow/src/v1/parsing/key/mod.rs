//! Key signature parsing module
//! 
//! Parses key signatures like "C Major", "A Minor", "D Dorian", "C [0,2,4,5,7,9,11]"

pub mod ast;
pub mod expressions;
pub mod lexer;
pub mod parser;
pub mod parser_error;
pub mod signature_parser;

pub use parser::KeyParser;
pub use parser_error::KeyParserError;
pub use signature_parser::KeySignatureMiniParser;

#[cfg(test)]
mod test_parser;
