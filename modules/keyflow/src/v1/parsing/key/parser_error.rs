//! Key-specific parsing errors

use std::fmt;
use serde::Serialize;

/// Errors specific to key parsing
#[derive(Debug, Serialize, PartialEq, Eq, Clone)]
pub enum KeyParserError {
    /// Missing scale type
    MissingScaleType,
    /// Invalid scale name
    InvalidScaleName(String),
    /// Empty custom scale
    EmptyCustomScale,
    /// Invalid custom scale format
    InvalidCustomScale(String),
    /// Invalid key signature format
    InvalidKeySignature(String),
    /// Conflicting scale information
    ConflictingScaleInfo,
    /// Invalid mode for scale type
    InvalidModeForScaleType(String, String),
}

impl KeyParserError {
    /// Get the position where the error occurred (1-based)
    pub fn position(&self) -> Option<usize> {
        // Most key errors don't have specific positions
        None
    }
}

impl fmt::Display for KeyParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            KeyParserError::MissingScaleType => write!(f, "Missing scale type"),
            KeyParserError::InvalidScaleName(name) => write!(f, "Invalid scale name: {}", name),
            KeyParserError::EmptyCustomScale => write!(f, "Custom scale cannot be empty"),
            KeyParserError::InvalidCustomScale(input) => write!(f, "Invalid custom scale format: {}", input),
            KeyParserError::InvalidKeySignature(input) => write!(f, "Invalid key signature format: {}", input),
            KeyParserError::ConflictingScaleInfo => write!(f, "Conflicting scale information provided"),
            KeyParserError::InvalidModeForScaleType(mode, scale_type) => {
                write!(f, "Mode '{}' is not valid for scale type '{}'", mode, scale_type)
            }
        }
    }
}
