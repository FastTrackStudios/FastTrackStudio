use serde::{Deserialize, Serialize};
use std::fmt::Debug;

/// Core trait that defines metadata for sortable items
/// 
/// The derive macro is re-exported at the crate root with the same name.
/// This follows the standard Rust pattern (like serde::Serialize).
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

    /// Create a Value from a string for a given field
    /// 
    /// This is used by the parser to create Value enum variants from extracted strings.
    /// The default implementation returns None - specific Metadata implementations
    /// should override this if they need string-based value creation.
    fn create_string_value(_field: &Self::Field, _value: String) -> Option<Self::Value> {
        None
    }

    /// Create a Value from a vector of strings for a given field
    /// 
    /// This is used by the parser to create Value enum variants for Vec<String> fields.
    /// The default implementation returns None - specific Metadata implementations
    /// should override this if they need vector-based value creation.
    fn create_vec_string_value(_field: &Self::Field, _values: Vec<String>) -> Option<Self::Value> {
        None
    }
}
