//! Shared metadata for collections and variants.
//!
//! [`Metadata`] provides tags, description, and notes that can be attached
//! to any domain entity (collections, variants, templates, etc.).

use facet::Facet;
use serde::{Deserialize, Serialize};

// ─── Tags ───────────────────────────────────────────────────────

/// A set of string tags for categorization and filtering.
#[derive(Debug, Clone, PartialEq, Eq, Default, Serialize, Deserialize, Facet)]
pub struct Tags(Vec<String>);

impl Tags {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn from_vec(tags: Vec<String>) -> Self {
        Self(tags)
    }

    pub fn add(&mut self, tag: impl Into<String>) {
        let tag = tag.into();
        if !self.0.contains(&tag) {
            self.0.push(tag);
        }
    }

    pub fn remove(&mut self, tag: &str) {
        self.0.retain(|t| t != tag);
    }

    pub fn contains(&self, tag: &str) -> bool {
        self.0.iter().any(|t| t == tag)
    }

    pub fn as_slice(&self) -> &[String] {
        &self.0
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}

// ─── Metadata ───────────────────────────────────────────────────

/// Shared metadata for any domain entity.
#[derive(Debug, Clone, PartialEq, Default, Serialize, Deserialize, Facet)]
pub struct Metadata {
    pub tags: Tags,
    pub description: Option<String>,
    pub notes: Option<String>,
}

impl Metadata {
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn with_description(mut self, desc: impl Into<String>) -> Self {
        self.description = Some(desc.into());
        self
    }

    #[must_use]
    pub fn with_notes(mut self, notes: impl Into<String>) -> Self {
        self.notes = Some(notes.into());
        self
    }

    #[must_use]
    pub fn with_tag(mut self, tag: impl Into<String>) -> Self {
        self.tags.add(tag);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tags_add_remove() {
        let mut tags = Tags::new();
        tags.add("rock");
        tags.add("blues");
        tags.add("rock"); // duplicate — ignored
        assert_eq!(tags.len(), 2);
        assert!(tags.contains("rock"));

        tags.remove("rock");
        assert!(!tags.contains("rock"));
        assert_eq!(tags.len(), 1);
    }

    #[test]
    fn test_metadata_builder() {
        let meta = Metadata::new()
            .with_description("A cool preset")
            .with_notes("Tweak the gain for taste")
            .with_tag("guitar")
            .with_tag("high-gain");

        assert_eq!(meta.description.as_deref(), Some("A cool preset"));
        assert_eq!(meta.notes.as_deref(), Some("Tweak the gain for taste"));
        assert_eq!(meta.tags.len(), 2);
    }
}
