//! Parser trait
//!
//! Trait for parsing track names.

use crate::smart_template::naming::track_name::TrackNameLike;

/// Trait for parsing track names
///
/// Implementations of this trait convert string track names into structured
/// data that can be used for matching and template generation.
pub trait Parser: Send + Sync {
    /// The output type (must implement TrackNameLike)
    type Output: TrackNameLike;
    
    /// Error type for parsing operations
    type Error: std::error::Error + Send + Sync;
    
    /// Parse a track name string into structured data
    fn parse(&self, name: &str) -> Result<Self::Output, Self::Error>;
    
    /// Get the name/identifier of this parser
    fn name(&self) -> &str;
}
