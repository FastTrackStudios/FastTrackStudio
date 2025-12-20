use serde::{Deserialize, Serialize};
use std::fmt::Debug;

// Re-export the derive macro
pub use monarchy_derive::Metadata;

/// Core trait that defines metadata for sortable items
pub trait Metadata: Clone + Default + Send + Sync + 'static {
    /// The field type used as keys for this metadata
    type Field: Clone
        + Debug
        + PartialEq
        + Serialize
        + for<'de> Deserialize<'de>
        + Send
        + Sync
        + 'static;

    /// The value type that can be stored in this metadata
    type Value: Clone + Debug + Send + Sync + 'static;

    /// Get a value for a given field
    fn get(&self, field: &Self::Field) -> Option<Self::Value>;

    /// Set a value for a given field
    fn set(&mut self, field: Self::Field, value: Self::Value);

    /// Get all available fields for this metadata type
    fn fields() -> Vec<Self::Field>;

    /// Get fields marked as variants (for differentiating similar items)
    fn variant_fields() -> Vec<Self::Field> {
        Vec::new() // Default: no variant fields
    }
}
