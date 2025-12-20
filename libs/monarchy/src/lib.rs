//! Monarchy: A metadata-based hierarchical sorter
//!
//! Type-safe, config-compatible organization system.

pub mod config;
pub mod error;
pub mod group;
pub mod metadata;
pub mod organizer;
pub mod parser;
pub mod structure;
pub mod test_utils;

// Core exports
pub use config::{Config, ConfigBuilder, FallbackStrategy, ParserRules};
pub use error::{MonarchyError, Result};
pub use group::{Group, GroupBuilder};
pub use metadata::Metadata;
pub use organizer::Organizer;
pub use parser::Parser;
pub use structure::Structure;
pub use test_utils::StructureAssertions;

// Re-export the derive macro
pub use monarchy_derive::Metadata as MetadataDerive;

// Re-export serde traits for derive usage
pub use serde::{Deserialize, Serialize};

/// Represents a parsed item with metadata
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Item<M: Metadata> {
    /// Unique identifier for this item
    pub id: String,

    /// Original input string
    pub original: String,

    /// Parsed metadata
    pub metadata: M,

    /// Which group this item matched
    pub matched_group: Option<String>,
}

/// Trait for converting various input types into sortable strings
pub trait IntoInputs {
    fn into_inputs(self) -> Vec<String>;
}

// Blanket implementation for any iterator that yields items convertible to String
impl<I, T> IntoInputs for I
where
    I: IntoIterator<Item = T>,
    T: Into<String>,
{
    fn into_inputs(self) -> Vec<String> {
        self.into_iter().map(|item| item.into()).collect()
    }
}

/// Main sorting function - now accepts anything that can be converted to inputs
pub fn monarchy_sort<M, I>(inputs: I, config: Config<M>) -> Result<Structure<M>>
where
    M: Metadata,
    I: IntoInputs,
{
    let input_strings = inputs.into_inputs();
    let parser = Parser::new(config.clone());
    let mut items = Vec::new();

    for input in input_strings {
        items.push(parser.parse(input)?);
    }

    let organizer = Organizer::new(config);
    Ok(organizer.organize(items))
}

/// Trait for containers that can hold sorted items
pub trait Target<M: Metadata> {
    fn existing_items(&self) -> Vec<Item<M>>;
}
