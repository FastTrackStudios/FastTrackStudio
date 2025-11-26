//! Marker Region Error Types
//!
//! This module defines error types specific to marker and region operations.

use std::fmt;

/// Errors that can occur during marker and region operations
#[derive(Debug, Clone, PartialEq)]
pub enum MarkerRegionError {
    /// Invalid marker position
    InvalidMarkerPosition(f64),

    /// Invalid region - start position is after end position
    InvalidRegion { start: f64, end: f64 },

    /// Marker not found with the given ID
    MarkerNotFound(u32),

    /// Region not found with the given ID
    RegionNotFound(u32),

    /// Invalid marker name (empty or invalid characters)
    InvalidMarkerName(String),

    /// Invalid region name (empty or invalid characters)
    InvalidRegionName(String),

    /// Position out of bounds for the project
    PositionOutOfBounds { position: f64, max_position: f64 },

    /// Overlapping region conflict
    OverlappingRegion {
        existing_id: u32,
        new_start: f64,
        new_end: f64,
    },

    /// Source implementation error
    SourceError(String),

    /// Time conversion error
    TimeConversionError(String),

    /// Parsing error for marker/region data
    ParseError(String),

    /// IO error when reading/writing marker/region data
    IoError(String),

    /// Feature not supported by the current implementation
    NotSupported(String),

    /// Generic operation error
    OperationError(String),
}

impl fmt::Display for MarkerRegionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidMarkerPosition(pos) => {
                write!(f, "Invalid marker position: {} seconds", pos)
            }
            Self::InvalidRegion { start, end } => {
                write!(
                    f,
                    "Invalid region: start ({}) must be before end ({})",
                    start, end
                )
            }
            Self::MarkerNotFound(id) => {
                write!(f, "Marker with ID {} not found", id)
            }
            Self::RegionNotFound(id) => {
                write!(f, "Region with ID {} not found", id)
            }
            Self::InvalidMarkerName(name) => {
                write!(f, "Invalid marker name: '{}'", name)
            }
            Self::InvalidRegionName(name) => {
                write!(f, "Invalid region name: '{}'", name)
            }
            Self::PositionOutOfBounds {
                position,
                max_position,
            } => {
                write!(
                    f,
                    "Position {} is out of bounds (max: {})",
                    position, max_position
                )
            }
            Self::OverlappingRegion {
                existing_id,
                new_start,
                new_end,
            } => {
                write!(
                    f,
                    "New region ({}-{}) overlaps with existing region ID {}",
                    new_start, new_end, existing_id
                )
            }
            Self::SourceError(msg) => {
                write!(f, "Source error: {}", msg)
            }
            Self::TimeConversionError(msg) => {
                write!(f, "Time conversion error: {}", msg)
            }
            Self::ParseError(msg) => {
                write!(f, "Parse error: {}", msg)
            }
            Self::IoError(msg) => {
                write!(f, "IO error: {}", msg)
            }
            Self::NotSupported(feature) => {
                write!(f, "Feature not supported: {}", feature)
            }
            Self::OperationError(msg) => {
                write!(f, "Operation error: {}", msg)
            }
        }
    }
}

impl std::error::Error for MarkerRegionError {}

// Conversions from standard error types
impl From<std::io::Error> for MarkerRegionError {
    fn from(error: std::io::Error) -> Self {
        Self::IoError(error.to_string())
    }
}

impl From<Box<dyn std::error::Error>> for MarkerRegionError {
    fn from(error: Box<dyn std::error::Error>) -> Self {
        Self::SourceError(error.to_string())
    }
}

impl From<String> for MarkerRegionError {
    fn from(msg: String) -> Self {
        Self::OperationError(msg)
    }
}

impl From<&str> for MarkerRegionError {
    fn from(msg: &str) -> Self {
        Self::OperationError(msg.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        let error = MarkerRegionError::InvalidMarkerPosition(-1.0);
        assert_eq!(error.to_string(), "Invalid marker position: -1 seconds");

        let error = MarkerRegionError::InvalidRegion {
            start: 10.0,
            end: 5.0,
        };
        assert_eq!(
            error.to_string(),
            "Invalid region: start (10) must be before end (5)"
        );
    }

    #[test]
    fn test_error_conversions() {
        let io_error = std::io::Error::new(std::io::ErrorKind::NotFound, "File not found");
        let marker_error: MarkerRegionError = io_error.into();

        match marker_error {
            MarkerRegionError::IoError(_) => (),
            _ => panic!("Expected IoError variant"),
        }
    }
}
