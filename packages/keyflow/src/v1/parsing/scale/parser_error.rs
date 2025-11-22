//! Scale-specific parsing errors

use std::fmt;
use serde::Serialize;

/// Errors specific to scale parsing
#[derive(Debug, Serialize, PartialEq, Eq, Clone)]
pub enum ScaleParserError {
    /// Invalid semitone value
    InvalidSemitone(u8),
    /// Scale too short (minimum 2 notes)
    ScaleTooShort,
    /// Scale too long (maximum 12 notes)
    ScaleTooLong,
    /// Duplicate semitone in scale
    DuplicateSemitone(u8),
    /// Invalid scale pattern
    InvalidScalePattern(String),
    /// Missing scale definition
    MissingScaleDefinition,
    /// Invalid interval in scale
    InvalidInterval(String),
}

impl ScaleParserError {
    /// Get the position where the error occurred (1-based)
    pub fn position(&self) -> Option<usize> {
        // Most scale errors don't have specific positions
        None
    }
}

impl fmt::Display for ScaleParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ScaleParserError::InvalidSemitone(semitone) => write!(f, "Invalid semitone value: {} (must be 0-11)", semitone),
            ScaleParserError::ScaleTooShort => write!(f, "Scale too short (minimum 2 notes required)"),
            ScaleParserError::ScaleTooLong => write!(f, "Scale too long (maximum 12 notes allowed)"),
            ScaleParserError::DuplicateSemitone(semitone) => write!(f, "Duplicate semitone in scale: {}", semitone),
            ScaleParserError::InvalidScalePattern(pattern) => write!(f, "Invalid scale pattern: {}", pattern),
            ScaleParserError::MissingScaleDefinition => write!(f, "Missing scale definition"),
            ScaleParserError::InvalidInterval(interval) => write!(f, "Invalid interval in scale: {}", interval),
        }
    }
}
