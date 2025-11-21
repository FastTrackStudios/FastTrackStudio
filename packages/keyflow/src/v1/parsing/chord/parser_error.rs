//! Chord-specific parsing errors

use std::fmt;
use serde::Serialize;

/// Errors specific to chord parsing
#[derive(Debug, Serialize, PartialEq, Eq, Clone)]
pub enum ChordParserError {
    /// Unexpected note at position
    UnexpectedNote(usize),
    /// Duplicate modifier
    DuplicateModifier(String),
    /// Inconsistent extension
    InconsistentExtension(String),
    /// Duplicate extension at position
    DuplicateExtension(usize),
    /// Invalid extension at position
    InvalidExtension(usize),
    /// Wrong expression target at position
    WrongExpressionTarget(usize),
    /// Three consecutive semitones
    ThreeConsecutiveSemitones(Vec<String>),
    /// Missing add target
    MissingAddTarget((usize, usize)),
    /// Illegal or missing omit target
    IllegalOrMissingOmitTarget((usize, usize)),
    /// Illegal add target
    IllegalAddTarget((usize, usize)),
    /// Illegal slash notation at position
    IllegalSlashNotation(usize),
    /// Invalid power expression
    InvalidPowerExpression,
}

impl ChordParserError {
    /// Get the position where the error occurred (1-based)
    pub fn position(&self) -> Option<usize> {
        match self {
            ChordParserError::UnexpectedNote(pos) 
            | ChordParserError::DuplicateExtension(pos) 
            | ChordParserError::InvalidExtension(pos)
            | ChordParserError::WrongExpressionTarget(pos)
            | ChordParserError::IllegalSlashNotation(pos) => Some(*pos),
            ChordParserError::MissingAddTarget((pos, len))
            | ChordParserError::IllegalOrMissingOmitTarget((pos, len))
            | ChordParserError::IllegalAddTarget((pos, len)) => Some(*pos + *len),
            ChordParserError::DuplicateModifier(_)
            | ChordParserError::InconsistentExtension(_)
            | ChordParserError::ThreeConsecutiveSemitones(_)
            | ChordParserError::InvalidPowerExpression => None,
        }
    }
}

impl fmt::Display for ChordParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ChordParserError::UnexpectedNote(pos) => write!(f, "Unexpected note at position {}", pos),
            ChordParserError::DuplicateModifier(modifier) => write!(f, "Duplicate modifier: {}", modifier),
            ChordParserError::InconsistentExtension(extension) => write!(f, "Inconsistent extension: {}", extension),
            ChordParserError::DuplicateExtension(pos) => write!(f, "Duplicate extension at position {}", pos),
            ChordParserError::InvalidExtension(pos) => write!(f, "Invalid extension at position {}", pos),
            ChordParserError::WrongExpressionTarget(pos) => write!(f, "Invalid expression target at position: {}", pos),
            ChordParserError::ThreeConsecutiveSemitones(notes) => write!(f, "Three consecutive semitones: {:?}", notes),
            ChordParserError::MissingAddTarget((pos, len)) => write!(f, "Missing add target at position {}", pos + len),
            ChordParserError::IllegalOrMissingOmitTarget((pos, len)) => write!(f, "Illegal or missing omit target at position {}", pos + len),
            ChordParserError::IllegalAddTarget((pos, len)) => write!(f, "Illegal add target at position {}", pos + len),
            ChordParserError::IllegalSlashNotation(pos) => write!(f, "Illegal slash notation at position {}", pos),
            ChordParserError::InvalidPowerExpression => write!(f, "A power chord should only contain a 5"),
        }
    }
}
