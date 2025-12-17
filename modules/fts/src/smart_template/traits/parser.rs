//! Parser trait
//!
//! Defines the interface for parsing track names into structured data.

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

/// Trait for types that represent parsed track names
///
/// This allows different track name representations to work with the parser system.
pub trait TrackNameLike: Clone + Send + Sync {
    /// Get the group prefix if available
    fn group_prefix(&self) -> Option<&str>;
    
    /// Get the sub-type if available
    fn sub_type(&self) -> Option<&[String]>;
    
    /// Get the original name
    fn original_name(&self) -> Option<&str>;
}
