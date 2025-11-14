//! Common parsing errors shared across all musical parsers

use std::fmt;
use serde::Serialize;

/// Common errors that can occur in any musical parser
#[derive(Debug, Serialize, PartialEq, Eq, Clone)]
pub enum CommonParserError {
    /// Illegal token at position
    IllegalToken(usize),
    /// Missing root note
    MissingRootNote,
    /// Invalid note name
    InvalidNoteName(String),
    /// Unexpected modifier at position
    UnexpectedModifier(usize),
    /// Unexpected closing parenthesis at position
    UnexpectedClosingParenthesis(usize),
    /// Missing closing parenthesis at position
    MissingClosingParenthesis(usize),
    /// Nested parenthesis at position
    NestedParenthesis(usize),
}

impl CommonParserError {
    /// Get the position where the error occurred (1-based)
    pub fn position(&self) -> Option<usize> {
        match self {
            CommonParserError::IllegalToken(pos) 
            | CommonParserError::UnexpectedModifier(pos)
            | CommonParserError::UnexpectedClosingParenthesis(pos)
            | CommonParserError::NestedParenthesis(pos)
            | CommonParserError::MissingClosingParenthesis(pos) => Some(*pos),
            CommonParserError::MissingRootNote => Some(1),
            CommonParserError::InvalidNoteName(_) => None,
        }
    }
}

impl fmt::Display for CommonParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CommonParserError::IllegalToken(pos) => write!(f, "Illegal token at position {}", pos),
            CommonParserError::MissingRootNote => write!(f, "Missing root note"),
            CommonParserError::InvalidNoteName(name) => write!(f, "Invalid note name: {}", name),
            CommonParserError::UnexpectedModifier(pos) => write!(f, "Unexpected modifier at position {}", pos),
            CommonParserError::UnexpectedClosingParenthesis(pos) => write!(f, "Unexpected closing parenthesis at position {}", pos),
            CommonParserError::MissingClosingParenthesis(pos) => write!(f, "Missing closing parenthesis at position {}", pos),
            CommonParserError::NestedParenthesis(pos) => write!(f, "Nested parenthesis at position {}", pos),
        }
    }
}

/// Generic parser errors container that can hold any type of parser error
#[derive(Debug, Serialize, PartialEq, Eq, Clone)]
pub enum ParserError {
    /// Common errors shared across parsers
    Common(CommonParserError),
    /// Chord-specific errors
    Chord(crate::parsing::chord::ChordParserError),
    /// Key-specific errors  
    Key(crate::parsing::key::KeyParserError),
    /// Scale-specific errors
    Scale(crate::parsing::scale::ScaleParserError),
}

impl ParserError {
    /// Get the position where the error occurred (1-based)
    pub fn position(&self) -> Option<usize> {
        match self {
            ParserError::Common(err) => err.position(),
            ParserError::Chord(err) => err.position(),
            ParserError::Key(err) => err.position(),
            ParserError::Scale(err) => err.position(),
        }
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParserError::Common(err) => write!(f, "{}", err),
            ParserError::Chord(err) => write!(f, "{}", err),
            ParserError::Key(err) => write!(f, "{}", err),
            ParserError::Scale(err) => write!(f, "{}", err),
        }
    }
}

/// Error returned when multiple errors occur during parsing
#[derive(Debug, Serialize, PartialEq, Eq, Clone)]
pub struct ParserErrors {
    pub errors: Vec<ParserError>,
}

impl ParserErrors {
    pub fn new(messages: Vec<ParserError>) -> ParserErrors {
        ParserErrors { errors: messages }
    }
    
    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }
    
    pub fn len(&self) -> usize {
        self.errors.len()
    }
}

impl fmt::Display for ParserErrors {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.errors.is_empty() {
            write!(f, "No errors")
        } else if self.errors.len() == 1 {
            write!(f, "{}", self.errors[0])
        } else {
            write!(f, "Multiple errors: ")?;
            for (i, error) in self.errors.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", error)?;
            }
            Ok(())
        }
    }
}