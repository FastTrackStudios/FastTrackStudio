//! Base parser trait and common functionality

use crate::parsing::common::{Lexer, ParserError, ParserErrors, Token};

/// Base trait for all musical parsers
pub trait MusicalParser<T> {
    /// Parse input string into the target type
    fn parse(&mut self, input: &str) -> Result<T, ParserErrors>;
    
    /// Get the current errors
    fn errors(&self) -> &[ParserError];
    
    /// Clear errors
    fn clear_errors(&mut self);
}

/// Base parser struct with common functionality
pub struct BaseParser {
    pub lexer: Lexer,
    pub errors: Vec<ParserError>,
}

impl BaseParser {
    pub fn new() -> Self {
        Self {
            lexer: Lexer::new(),
            errors: Vec::new(),
        }
    }
    
    /// Add an error to the error list
    pub fn add_error(&mut self, error: ParserError) {
        self.errors.push(error);
    }
    
    /// Check if there are any errors
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
    
    /// Get all errors as ParserErrors
    pub fn get_errors(&self) -> ParserErrors {
        ParserErrors::new(self.errors.clone())
    }
    
    /// Clear all errors
    pub fn clear_errors(&mut self) {
        self.errors.clear();
    }
    
    /// Tokenize input string
    pub fn tokenize(&mut self, input: &str) -> Vec<Token> {
        self.lexer.scan_tokens(input)
    }
}
