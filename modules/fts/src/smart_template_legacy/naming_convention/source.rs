//! Source trait for parsing naming conventions
//! 
//! This trait allows different implementations to parse track names:
//! - String parser (from fts-naming-parser crate)
//! - UI input forms
//! - API endpoints
//! - Other future sources

use super::TrackName;

/// Trait for sources that can parse track names according to the FTS naming convention.
/// 
/// Implementations can parse from different sources:
/// - String input (via fts-naming-parser)
/// - Structured data (JSON, forms, etc.)
/// - Other formats
pub trait NamingConventionSource {
    /// Parse a single track name from the input.
    /// 
    /// # Errors
    /// 
    /// Returns an error if the input cannot be parsed or if parsing fails.
    fn parse(&self, input: &str) -> Result<TrackName, Box<dyn std::error::Error>>;
    
    /// Parse multiple track names in batch (may be optimized).
    /// 
    /// # Errors
    /// 
    /// Returns an error if batch parsing fails. Individual items may fail silently
    /// depending on implementation.
    fn parse_batch(&self, inputs: &[String]) -> Result<Vec<TrackName>, Box<dyn std::error::Error>>;
    
    /// Get the name of this source (for debugging/logging).
    fn source_name(&self) -> &'static str;
}

