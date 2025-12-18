//! Track name trait
//!
//! Trait for types that represent parsed track names.

/// Trait for types that represent parsed track names
///
/// This allows different track name representations to work with the parser system.
pub trait TrackNameLike: Send + Sync {
    /// Get the group prefix if available
    fn group_prefix(&self) -> Option<&str>;
    
    /// Get the sub-type if available
    fn sub_type(&self) -> Option<&[String]>;
    
    /// Get the original name
    fn original_name(&self) -> Option<&str>;
}
