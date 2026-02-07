//! Typestate preset builder — compile-time enforcement of required fields.
//!
//! The builder uses phantom type parameters to ensure `name` and `category`
//! are always provided before `build()` is callable:
//!
//! ```ignore
//! let preset = PresetBuilder::new()
//!     .name("Blues Lead")
//!     .category(PresetCategory::Genre { ... })
//!     .rating(Rating::new(4))
//!     .build();
//! ```
//!
//! Calling `.build()` before `.name()` and `.category()` is a compile error.

use std::marker::PhantomData;

use crate::block::{BlockType, PluginId};
use crate::category::PresetCategory;
use crate::module_preset::ModuleAssignment;
use crate::normalized::{Order, Rating};
use crate::tags::Tags;

use super::{
    Credit, GlobalBlockConfig, Inspiration, PatchAssignment, Preset, PresetBlock, SectionConfig,
    Snapshot,
};

// ─────────────────────────────────────────────────────────────────────────────
// Typestate markers
// ─────────────────────────────────────────────────────────────────────────────

/// Builder state: name has not been set yet.
pub struct NeedsName;
/// Builder state: name is set, category is needed.
pub struct NeedsCategory;
/// Builder state: all required fields are set, ready to build.
pub struct Complete;

// ─────────────────────────────────────────────────────────────────────────────
// PresetBuilder
// ─────────────────────────────────────────────────────────────────────────────

/// Typestate builder for [`Preset`].
///
/// Transitions: `NeedsName` -> `NeedsCategory` -> `Complete`.
/// Only `Complete` has a `.build()` method.
pub struct PresetBuilder<State> {
    name: Option<String>,
    category: Option<PresetCategory>,
    tags: Tags,
    rating: Rating,
    notes: Option<String>,
    description: Option<String>,
    credits: Vec<Credit>,
    inspirations: Vec<Inspiration>,
    module_assignments: Vec<ModuleAssignment>,
    hidden: bool,
    section_config: Vec<SectionConfig>,
    patch_assignments: Vec<PatchAssignment>,
    global_block_config: Vec<GlobalBlockConfig>,
    blocks: Vec<PresetBlock>,
    snapshots: Vec<Snapshot>,
    _state: PhantomData<State>,
}

impl PresetBuilder<NeedsName> {
    /// Start building a new preset. The first required step is `.name()`.
    pub fn new() -> Self {
        Self {
            name: None,
            category: None,
            tags: Tags::new(),
            rating: Rating::UNRATED,
            notes: None,
            description: None,
            credits: Vec::new(),
            inspirations: Vec::new(),
            module_assignments: Vec::new(),
            hidden: false,
            section_config: Vec::new(),
            patch_assignments: Vec::new(),
            global_block_config: Vec::new(),
            blocks: Vec::new(),
            snapshots: Vec::new(),
            _state: PhantomData,
        }
    }

    /// Set the preset name. Transitions to `NeedsCategory`.
    pub fn name(self, name: impl Into<String>) -> PresetBuilder<NeedsCategory> {
        PresetBuilder {
            name: Some(name.into()),
            category: self.category,
            tags: self.tags,
            rating: self.rating,
            notes: self.notes,
            description: self.description,
            credits: self.credits,
            inspirations: self.inspirations,
            module_assignments: self.module_assignments,
            hidden: self.hidden,
            section_config: self.section_config,
            patch_assignments: self.patch_assignments,
            global_block_config: self.global_block_config,
            blocks: self.blocks,
            snapshots: self.snapshots,
            _state: PhantomData,
        }
    }
}

impl Default for PresetBuilder<NeedsName> {
    fn default() -> Self {
        Self::new()
    }
}

impl PresetBuilder<NeedsCategory> {
    /// Set the preset category. Transitions to `Complete`.
    pub fn category(self, category: PresetCategory) -> PresetBuilder<Complete> {
        PresetBuilder {
            name: self.name,
            category: Some(category),
            tags: self.tags,
            rating: self.rating,
            notes: self.notes,
            description: self.description,
            credits: self.credits,
            inspirations: self.inspirations,
            module_assignments: self.module_assignments,
            hidden: self.hidden,
            section_config: self.section_config,
            patch_assignments: self.patch_assignments,
            global_block_config: self.global_block_config,
            blocks: self.blocks,
            snapshots: self.snapshots,
            _state: PhantomData,
        }
    }
}

impl PresetBuilder<Complete> {
    /// Set the star rating.
    #[must_use]
    pub fn rating(mut self, rating: Rating) -> Self {
        self.rating = rating;
        self
    }

    /// Set tags.
    #[must_use]
    pub fn tags(mut self, tags: Tags) -> Self {
        self.tags = tags;
        self
    }

    /// Set user notes.
    #[must_use]
    pub fn notes(mut self, notes: impl Into<String>) -> Self {
        self.notes = Some(notes.into());
        self
    }

    /// Mark the preset as hidden.
    #[must_use]
    pub fn hidden(mut self) -> Self {
        self.hidden = true;
        self
    }

    /// Add a snapshot.
    #[must_use]
    pub fn snapshot(mut self, snapshot: Snapshot) -> Self {
        self.snapshots.push(snapshot);
        self
    }

    /// Add a preset-local block.
    #[must_use]
    pub fn block(mut self, block: PresetBlock) -> Self {
        self.blocks.push(block);
        self
    }

    /// Add a section configuration.
    #[must_use]
    pub fn section_config(mut self, config: SectionConfig) -> Self {
        self.section_config.push(config);
        self
    }

    /// Add a patch assignment.
    #[must_use]
    pub fn patch_assignment(mut self, assignment: PatchAssignment) -> Self {
        self.patch_assignments.push(assignment);
        self
    }

    /// Add a global block configuration.
    #[must_use]
    pub fn global_block_config(mut self, config: GlobalBlockConfig) -> Self {
        self.global_block_config.push(config);
        self
    }

    /// Add a module assignment.
    #[must_use]
    pub fn module_assignment(mut self, assignment: ModuleAssignment) -> Self {
        self.module_assignments.push(assignment);
        self
    }

    /// Add a credit.
    #[must_use]
    pub fn credit(mut self, credit: Credit) -> Self {
        self.credits.push(credit);
        self
    }

    /// Add an inspiration.
    #[must_use]
    pub fn inspiration(mut self, inspiration: Inspiration) -> Self {
        self.inspirations.push(inspiration);
        self
    }

    /// Set the description.
    #[must_use]
    pub fn description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }

    /// Build the final [`Preset`].
    ///
    /// Consumes the builder and returns a fully constructed preset.
    pub fn build(self) -> Preset {
        Preset {
            id: crate::id::PresetId::new(),
            // SAFETY: name and category are guaranteed to be Some by the typestate.
            name: self.name.expect("name must be set (typestate guarantees this)"),
            category: self.category.expect("category must be set (typestate guarantees this)"),
            tags: self.tags,
            rating: self.rating,
            notes: self.notes,
            description: self.description,
            credits: self.credits,
            inspirations: self.inspirations,
            module_assignments: self.module_assignments,
            hidden: self.hidden,
            section_config: self.section_config,
            patch_assignments: self.patch_assignments,
            global_block_config: self.global_block_config,
            blocks: self.blocks,
            snapshots: self.snapshots,
            default_snapshot_id: None,
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use crate::category::BaseTone;
    use crate::id::*;

    use super::*;

    fn test_category() -> PresetCategory {
        PresetCategory::Generic {
            base_tone: BaseTone::Clean,
        }
    }

    #[test]
    fn builder_minimal() {
        let preset = PresetBuilder::new()
            .name("Clean")
            .category(test_category())
            .build();

        assert_eq!(preset.name, "Clean");
        assert_eq!(preset.rating, Rating::UNRATED);
        assert!(preset.notes.is_none());
        assert!(!preset.hidden);
        assert!(preset.snapshots.is_empty());
        assert!(preset.blocks.is_empty());
    }

    #[test]
    fn builder_with_all_optional_fields() {
        let tags = Tags::from_ids([TagId::new()]);

        let preset = PresetBuilder::new()
            .name("Blues Lead")
            .category(PresetCategory::Genre {
                base_tone: BaseTone::Lead,
                genre: crate::category::Genre::Blues,
            })
            .rating(Rating::new(5))
            .tags(tags)
            .notes("My favorite preset")
            .hidden()
            .build();

        assert_eq!(preset.name, "Blues Lead");
        assert_eq!(preset.rating, Rating::new(5));
        assert_eq!(preset.notes.as_deref(), Some("My favorite preset"));
        assert!(preset.hidden);
        assert!(!preset.tags.is_empty());
    }

    #[test]
    fn builder_with_snapshot() {
        let snap = Snapshot::new("Verse");
        let preset = PresetBuilder::new()
            .name("Test")
            .category(test_category())
            .snapshot(snap)
            .build();

        assert_eq!(preset.snapshots.len(), 1);
        assert_eq!(preset.snapshots[0].name, "Verse");
    }

    #[test]
    fn builder_with_block() {
        let block = PresetBlock::new("Drive", BlockType::Drive);
        let preset = PresetBuilder::new()
            .name("Test")
            .category(test_category())
            .block(block)
            .build();

        assert_eq!(preset.blocks.len(), 1);
        assert_eq!(preset.blocks[0].name, "Drive");
    }

    #[test]
    fn builder_with_section_config() {
        let config = SectionConfig::enabled(SectionId::new());
        let preset = PresetBuilder::new()
            .name("Test")
            .category(test_category())
            .section_config(config)
            .build();

        assert_eq!(preset.section_config.len(), 1);
    }

    #[test]
    fn builder_with_patch_assignment() {
        let assignment = PatchAssignment::new(SectionId::new(), 0, PatchId::new());
        let preset = PresetBuilder::new()
            .name("Test")
            .category(test_category())
            .patch_assignment(assignment)
            .build();

        assert_eq!(preset.patch_assignments.len(), 1);
    }

    #[test]
    fn builder_with_global_block_config() {
        let config = GlobalBlockConfig::enabled(GlobalBlockId::new());
        let preset = PresetBuilder::new()
            .name("Test")
            .category(test_category())
            .global_block_config(config)
            .build();

        assert_eq!(preset.global_block_config.len(), 1);
    }

    #[test]
    fn builder_chaining_multiple_snapshots_and_blocks() {
        let preset = PresetBuilder::new()
            .name("Full Preset")
            .category(test_category())
            .snapshot(Snapshot::new("Verse"))
            .snapshot(Snapshot::new("Chorus"))
            .snapshot(Snapshot::new("Solo"))
            .block(PresetBlock::new("Drive", BlockType::Drive))
            .block(PresetBlock::new("Delay", BlockType::Delay))
            .block(PresetBlock::new("Reverb", BlockType::Reverb))
            .build();

        assert_eq!(preset.snapshots.len(), 3);
        assert_eq!(preset.blocks.len(), 3);
        assert_eq!(preset.snapshots[0].name, "Verse");
        assert_eq!(preset.snapshots[1].name, "Chorus");
        assert_eq!(preset.snapshots[2].name, "Solo");
    }

    #[test]
    fn builder_default_creates_needs_name() {
        // Ensure Default is implemented for the initial state
        let builder = PresetBuilder::default();
        let preset = builder
            .name("Default Test")
            .category(test_category())
            .build();
        assert_eq!(preset.name, "Default Test");
    }

    #[test]
    fn builder_with_plugin_block() {
        let plugin = PluginId::vst3("com.example.drive", "Example Drive");
        let block = PresetBlock::new("My Drive", BlockType::Drive)
            .with_plugin(plugin)
            .with_order(Order::new(1));

        let preset = PresetBuilder::new()
            .name("Plugin Test")
            .category(test_category())
            .block(block)
            .build();

        assert!(preset.blocks[0].plugin_id.is_some());
        assert_eq!(preset.blocks[0].order, Order::new(1));
    }
}
