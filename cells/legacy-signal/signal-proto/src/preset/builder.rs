//! Preset builder — typestate builder for creating presets at any level.
//!
//! Ensures name and category are set before a preset can be built.

use std::marker::PhantomData;

use crate::category::PresetCategory;
use crate::normalized::Rating;
use crate::preset::PresetMetadata;
use crate::tags::Tags;

// ─── Builder states ──────────────────────────────────────────────

/// Name has not been set yet.
#[derive(..Marker)]
pub struct NeedsName;
/// Name is set, needs category.
#[derive(..Marker)]
pub struct NeedsCategory;
/// Name and category are set — ready to build.
#[derive(..Marker)]
pub struct Complete;

// ─── PresetMetadataBuilder ───────────────────────────────────────

/// Typestate builder for [`PresetMetadata`].
///
/// Enforces that name is set before category, and both are set before build.
pub struct PresetMetadataBuilder<State = NeedsName> {
    name: Option<String>,
    category: Option<PresetCategory>,
    author: Option<String>,
    description: Option<String>,
    rating: Rating,
    tags: Tags,
    _state: PhantomData<State>,
}

impl PresetMetadataBuilder<NeedsName> {
    /// Start building preset metadata.
    pub fn new() -> Self {
        Self {
            name: None,
            category: None,
            author: None,
            description: None,
            rating: Rating::default(),
            tags: Tags::new(),
            _state: PhantomData,
        }
    }

    /// Set the preset name.
    pub fn name(self, name: impl Into<String>) -> PresetMetadataBuilder<NeedsCategory> {
        PresetMetadataBuilder {
            name: Some(name.into()),
            category: self.category,
            author: self.author,
            description: self.description,
            rating: self.rating,
            tags: self.tags,
            _state: PhantomData,
        }
    }
}

impl Default for PresetMetadataBuilder<NeedsName> {
    fn default() -> Self {
        Self::new()
    }
}

impl PresetMetadataBuilder<NeedsCategory> {
    /// Set the preset category.
    pub fn category(self, category: PresetCategory) -> PresetMetadataBuilder<Complete> {
        PresetMetadataBuilder {
            name: self.name,
            category: Some(category),
            author: self.author,
            description: self.description,
            rating: self.rating,
            tags: self.tags,
            _state: PhantomData,
        }
    }
}

impl PresetMetadataBuilder<Complete> {
    /// Set an optional author.
    #[must_use]
    pub fn author(mut self, author: impl Into<String>) -> Self {
        self.author = Some(author.into());
        self
    }

    /// Set an optional description.
    #[must_use]
    pub fn description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }

    /// Set the rating.
    #[must_use]
    pub fn rating(mut self, rating: Rating) -> Self {
        self.rating = rating;
        self
    }

    /// Build the preset metadata. Only callable when name and category are set.
    pub fn build(self) -> PresetMetadata {
        PresetMetadata {
            name: self.name.expect("name set in NeedsName → NeedsCategory"),
            category: self
                .category
                .expect("category set in NeedsCategory → Complete"),
            author: self.author,
            description: self.description,
            rating: self.rating,
            tags: self.tags,
        }
    }
}

// ─── Tests ───────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::category::BaseTone;

    #[test]
    fn builder_full_path() {
        let meta = PresetMetadataBuilder::new()
            .name("My Preset")
            .category(PresetCategory::Generic {
                base_tone: BaseTone::Clean,
            })
            .author("Test Author")
            .description("A test preset")
            .build();

        assert_eq!(meta.name, "My Preset");
        assert_eq!(meta.author.as_deref(), Some("Test Author"));
        assert_eq!(meta.description.as_deref(), Some("A test preset"));
    }

    #[test]
    fn builder_minimal() {
        let meta = PresetMetadataBuilder::new()
            .name("Minimal")
            .category(PresetCategory::Generic {
                base_tone: BaseTone::Warm,
            })
            .build();

        assert_eq!(meta.name, "Minimal");
        assert!(meta.author.is_none());
        assert!(meta.description.is_none());
    }
}
