//! Scale parsing module
//! 
//! Parses scale definitions like "[0,2,4,5,7,9,11]", "Major", "Dorian", etc.

pub mod ast;
pub mod expressions;
pub mod parser;
pub mod parser_error;

pub use parser::ScaleParser;
pub use parser_error::ScaleParserError;
