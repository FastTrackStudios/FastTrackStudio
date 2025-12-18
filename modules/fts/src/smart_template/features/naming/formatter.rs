//! Formatter trait
//!
//! Trait for formatting track names.

use crate::smart_template::features::naming::track_name::TrackNameLike;

/// Trait for formatting track names
///
/// Implementations of this trait convert structured track name data back into
/// string representations for display or storage.
pub trait Formatter: Send + Sync {
    /// The track name type this formatter works with
    type TrackName: TrackNameLike;
    
    /// Format a track name from structured data
    fn format(&self, track_name: &Self::TrackName) -> String;
    
    /// Get the name/identifier of this formatter
    fn name(&self) -> &str;
}
