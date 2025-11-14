//! Error types for the setlist module
//!
//! Provides comprehensive error handling for all setlist operations including
//! validation, parsing, and source-specific errors.

use thiserror::Error;

/// Main error type for setlist operations
#[derive(Error, Debug, Clone, PartialEq)]
pub enum SetlistError {
    /// Section validation errors
    #[error("Invalid section: {message}")]
    InvalidSection { message: String },

    /// Song validation errors
    #[error("Invalid song: {message}")]
    InvalidSong { message: String },

    /// Setlist validation errors
    #[error("Invalid setlist: {message}")]
    InvalidSetlist { message: String },

    /// Section parsing errors
    #[error("Failed to parse section type from '{input}': {reason}")]
    SectionParseError { input: String, reason: String },

    /// Song parsing errors
    #[error("Failed to parse song from markers/regions: {reason}")]
    SongParseError { reason: String },

    /// Time range validation errors
    #[error("Invalid time range: start {start} must be before end {end}")]
    InvalidTimeRange { start: f64, end: f64 },

    /// Marker/region source errors
    #[error("Marker/region source error: {message}")]
    SourceError { message: String },

    /// REAPER integration errors
    #[cfg(feature = "reaper")]
    #[error("REAPER error: {message}")]
    ReaperError { message: String },

    /// RPP parsing errors
    #[cfg(feature = "rpp")]
    #[error("RPP parsing error: {message}")]
    RppError { message: String },

    /// I/O errors (file operations, etc.)
    #[error("I/O error: {message}")]
    IoError { message: String },

    /// JSON serialization/deserialization errors
    #[error("JSON error: {message}")]
    JsonError { message: String },

    /// Generic validation error
    #[error("Validation error: {message}")]
    ValidationError { message: String },

    /// Not found errors
    #[error("Not found: {item_type} with identifier '{identifier}'")]
    NotFound {
        item_type: String,
        identifier: String,
    },

    /// Duplicate item errors
    #[error("Duplicate {item_type}: '{identifier}' already exists")]
    Duplicate {
        item_type: String,
        identifier: String,
    },

    /// Configuration errors
    #[error("Configuration error: {message}")]
    ConfigError { message: String },
}

impl SetlistError {
    /// Create a section validation error
    pub fn invalid_section<S: Into<String>>(message: S) -> Self {
        Self::InvalidSection {
            message: message.into(),
        }
    }

    /// Create a song validation error
    pub fn invalid_song<S: Into<String>>(message: S) -> Self {
        Self::InvalidSong {
            message: message.into(),
        }
    }

    /// Create a setlist validation error
    pub fn invalid_setlist<S: Into<String>>(message: S) -> Self {
        Self::InvalidSetlist {
            message: message.into(),
        }
    }

    /// Create a section parsing error
    pub fn section_parse_error<S1: Into<String>, S2: Into<String>>(
        input: S1,
        reason: S2,
    ) -> Self {
        Self::SectionParseError {
            input: input.into(),
            reason: reason.into(),
        }
    }

    /// Create a song parsing error
    pub fn song_parse_error<S: Into<String>>(reason: S) -> Self {
        Self::SongParseError {
            reason: reason.into(),
        }
    }

    /// Create an invalid time range error
    pub fn invalid_time_range(start: f64, end: f64) -> Self {
        Self::InvalidTimeRange { start, end }
    }

    /// Create a source error
    pub fn source_error<S: Into<String>>(message: S) -> Self {
        Self::SourceError {
            message: message.into(),
        }
    }

    /// Create an I/O error
    pub fn io_error<S: Into<String>>(message: S) -> Self {
        Self::IoError {
            message: message.into(),
        }
    }

    /// Create a JSON error
    pub fn json_error<S: Into<String>>(message: S) -> Self {
        Self::JsonError {
            message: message.into(),
        }
    }

    /// Create a validation error
    pub fn validation_error<S: Into<String>>(message: S) -> Self {
        Self::ValidationError {
            message: message.into(),
        }
    }

    /// Create a not found error
    pub fn not_found<S1: Into<String>, S2: Into<String>>(
        item_type: S1,
        identifier: S2,
    ) -> Self {
        Self::NotFound {
            item_type: item_type.into(),
            identifier: identifier.into(),
        }
    }

    /// Create a duplicate error
    pub fn duplicate<S1: Into<String>, S2: Into<String>>(
        item_type: S1,
        identifier: S2,
    ) -> Self {
        Self::Duplicate {
            item_type: item_type.into(),
            identifier: identifier.into(),
        }
    }

    /// Create a configuration error
    pub fn config_error<S: Into<String>>(message: S) -> Self {
        Self::ConfigError {
            message: message.into(),
        }
    }

    /// Check if this error is recoverable
    pub fn is_recoverable(&self) -> bool {
        match self {
            Self::InvalidSection { .. }
            | Self::InvalidSong { .. }
            | Self::InvalidSetlist { .. }
            | Self::SectionParseError { .. }
            | Self::InvalidTimeRange { .. }
            | Self::ValidationError { .. }
            | Self::Duplicate { .. } => true,

            Self::SourceError { .. }
            | Self::IoError { .. }
            | Self::JsonError { .. }
            | Self::NotFound { .. }
            | Self::ConfigError { .. } => false,

            #[cfg(feature = "reaper")]
            Self::ReaperError { .. } => false,

            #[cfg(feature = "rpp")]
            Self::RppError { .. } => false,

            Self::SongParseError { .. } => true,
        }
    }

    /// Get error category for logging/monitoring
    pub fn category(&self) -> &'static str {
        match self {
            Self::InvalidSection { .. }
            | Self::InvalidSong { .. }
            | Self::InvalidSetlist { .. }
            | Self::InvalidTimeRange { .. }
            | Self::ValidationError { .. } => "validation",

            Self::SectionParseError { .. }
            | Self::SongParseError { .. } => "parsing",

            Self::SourceError { .. } => "source",

            Self::IoError { .. } => "io",
            Self::JsonError { .. } => "json",
            Self::NotFound { .. } => "not_found",
            Self::Duplicate { .. } => "duplicate",
            Self::ConfigError { .. } => "config",

            #[cfg(feature = "reaper")]
            Self::ReaperError { .. } => "reaper",

            #[cfg(feature = "rpp")]
            Self::RppError { .. } => "rpp",
        }
    }
}

// Conversions from standard library errors
impl From<std::io::Error> for SetlistError {
    fn from(err: std::io::Error) -> Self {
        Self::io_error(err.to_string())
    }
}

impl From<serde_json::Error> for SetlistError {
    fn from(err: serde_json::Error) -> Self {
        Self::json_error(err.to_string())
    }
}

impl From<uuid::Error> for SetlistError {
    fn from(err: uuid::Error) -> Self {
        Self::validation_error(format!("UUID error: {}", err))
    }
}

// Conversion from marker-region errors
impl From<marker_region::MarkerRegionError> for SetlistError {
    fn from(err: marker_region::MarkerRegionError) -> Self {
        Self::source_error(err.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_creation() {
        let err = SetlistError::invalid_section("Section has no name");
        assert_eq!(err.to_string(), "Invalid section: Section has no name");
        assert!(err.is_recoverable());
        assert_eq!(err.category(), "validation");
    }

    #[test]
    fn test_invalid_time_range() {
        let err = SetlistError::invalid_time_range(100.0, 50.0);
        assert_eq!(
            err.to_string(),
            "Invalid time range: start 100 must be before end 50"
        );
        assert!(err.is_recoverable());
    }

    #[test]
    fn test_not_found_error() {
        let err = SetlistError::not_found("song", "Bohemian Rhapsody");
        assert_eq!(
            err.to_string(),
            "Not found: song with identifier 'Bohemian Rhapsody'"
        );
        assert!(!err.is_recoverable());
        assert_eq!(err.category(), "not_found");
    }

    #[test]
    fn test_error_categories() {
        assert_eq!(SetlistError::invalid_section("test").category(), "validation");
        assert_eq!(SetlistError::section_parse_error("test", "reason").category(), "parsing");
        assert_eq!(SetlistError::source_error("test").category(), "source");
        assert_eq!(SetlistError::io_error("test").category(), "io");
        assert_eq!(SetlistError::not_found("type", "id").category(), "not_found");
        assert_eq!(SetlistError::duplicate("type", "id").category(), "duplicate");
    }

    #[test]
    fn test_recoverable_errors() {
        assert!(SetlistError::invalid_section("test").is_recoverable());
        assert!(SetlistError::validation_error("test").is_recoverable());
        assert!(SetlistError::duplicate("type", "id").is_recoverable());

        assert!(!SetlistError::io_error("test").is_recoverable());
        assert!(!SetlistError::source_error("test").is_recoverable());
        assert!(!SetlistError::not_found("type", "id").is_recoverable());
    }

    #[test]
    fn test_conversions() {
        // Test IO error conversion
        let io_err = std::io::Error::new(std::io::ErrorKind::NotFound, "file not found");
        let setlist_err = SetlistError::from(io_err);
        assert!(matches!(setlist_err, SetlistError::IoError { .. }));

        // Test JSON error conversion
        let json_result: Result<i32, serde_json::Error> = serde_json::from_str("invalid json");
        let json_err = json_result.unwrap_err();
        let setlist_err = SetlistError::from(json_err);
        assert!(matches!(setlist_err, SetlistError::JsonError { .. }));
    }
}
