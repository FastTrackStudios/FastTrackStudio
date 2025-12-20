//! Field types and traits for metadata fields
//! 
//! This module provides the infrastructure for treating metadata fields
//! as Groups, allowing them to have patterns, negative patterns, prefixes, etc.

use crate::{Group, Metadata};

/// Trait for types that can be converted into a Field
/// 
/// This allows metadata fields to be represented as Groups, enabling
/// pattern matching, prefixes, nested groups, etc. for each field.
pub trait IntoField<M: Metadata> {
    /// Convert this type into a Group that represents a metadata field
    fn into_field(self) -> Group<M>;
}

/// A metadata field represented as a Group
/// 
/// This allows each metadata field to have its own:
/// - Patterns for matching
/// - Negative patterns for exclusion
/// - Prefixes
/// - Nested groups
/// - Priority
/// - etc.
#[derive(Clone, Debug)]
pub struct MetadataField<M: Metadata> {
    /// The field this represents
    pub field: M::Field,
    
    /// The group configuration for this field
    pub group: Group<M>,
}

impl<M: Metadata> MetadataField<M> {
    /// Create a new metadata field from a field identifier
    pub fn new(field: M::Field) -> Self {
        Self {
            field: field.clone(),
            group: Group::builder(format!("{:?}", field))
                .field(field)
                .build(),
        }
    }
    
    /// Create a metadata field from a Group
    pub fn from_group(field: M::Field, group: Group<M>) -> Self {
        Self { field, group }
    }
}

impl<M: Metadata> IntoField<M> for MetadataField<M> {
    fn into_field(self) -> Group<M> {
        self.group
    }
}

impl<M: Metadata> IntoField<M> for Group<M> {
    fn into_field(self) -> Group<M> {
        self
    }
}

