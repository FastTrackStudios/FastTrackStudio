//! Browse and sort modes for the preset browser.
//!
//! These types control how the multi-column preset browser organizes
//! its tag layout and sorts results.

use crate::tags::{TagCategory, Taggable, Tags};

// ─── BrowseMode ──────────────────────────────────────────────────

/// How the preset browser organizes its multi-column tag layout.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, ::facet::Facet, Default)]
#[repr(u8)]
pub enum BrowseMode {
    /// Instrument tag hierarchy — organized by purpose/sound.
    /// Guitar: BaseTone | Genre | Character | Gear
    /// Keys: Instrument | Plugin | Character | Context
    #[default]
    Semantic = 0,
    /// Manufacturer → Plugin → Preset.
    Vendor = 1,
    /// Genre → BaseTone → Character.
    Genre = 2,
    /// Song → Section → assigned presets.
    Song = 3,
}

impl BrowseMode {
    /// Display name for UI.
    pub const fn display_name(&self) -> &'static str {
        match self {
            Self::Semantic => "Semantic",
            Self::Vendor => "Vendor",
            Self::Genre => "Genre",
            Self::Song => "Song",
        }
    }

    /// All variants in display order.
    pub const fn all() -> &'static [BrowseMode] {
        &[Self::Semantic, Self::Vendor, Self::Genre, Self::Song]
    }

    /// Tag columns for this browse mode (guitar instrument type).
    pub fn guitar_columns(&self) -> &'static [TagCategory] {
        match self {
            Self::Semantic => &[
                TagCategory::BaseTone,
                TagCategory::Genre,
                TagCategory::Character,
                TagCategory::Gear,
            ],
            Self::Vendor => &[TagCategory::Plugin, TagCategory::BaseTone],
            Self::Genre => &[
                TagCategory::Genre,
                TagCategory::BaseTone,
                TagCategory::Character,
                TagCategory::Context,
            ],
            Self::Song => &[TagCategory::Song, TagCategory::Context],
        }
    }

    /// Tag columns for this browse mode (keys instrument type).
    pub fn keys_columns(&self) -> &'static [TagCategory] {
        match self {
            Self::Semantic => &[
                TagCategory::Instrument,
                TagCategory::Plugin,
                TagCategory::Character,
                TagCategory::Context,
            ],
            Self::Vendor => &[TagCategory::Plugin, TagCategory::Instrument],
            Self::Genre => &[
                TagCategory::Genre,
                TagCategory::BaseTone,
                TagCategory::Character,
                TagCategory::Context,
            ],
            Self::Song => &[TagCategory::Song, TagCategory::Context],
        }
    }

    /// Tag columns for this browse mode (bass instrument type).
    pub fn bass_columns(&self) -> &'static [TagCategory] {
        match self {
            Self::Semantic => &[
                TagCategory::BaseTone,
                TagCategory::Instrument,
                TagCategory::Character,
                TagCategory::Plugin,
            ],
            Self::Vendor => &[TagCategory::Plugin, TagCategory::BaseTone],
            Self::Genre => &[
                TagCategory::Genre,
                TagCategory::BaseTone,
                TagCategory::Character,
                TagCategory::Context,
            ],
            Self::Song => &[TagCategory::Song, TagCategory::Context],
        }
    }
}

// ─── SortMode ────────────────────────────────────────────────────

/// How items are sorted within a list.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, ::facet::Facet, Default)]
#[repr(u8)]
pub enum SortMode {
    /// Order as arranged in signal chain grid (for stompbox switching).
    StompboxOrder = 0,
    /// Original creation / index order.
    #[default]
    CreationOrder = 1,
    /// Sort by tag similarity to a reference (most shared tags first).
    Similarity = 2,
    /// Alphabetical by name.
    Name = 3,
    /// By rating (highest composite score first).
    Rating = 4,
    /// Most recently modified first.
    Recent = 5,
}

impl SortMode {
    /// Display name for UI.
    pub const fn display_name(&self) -> &'static str {
        match self {
            Self::StompboxOrder => "Stompbox Order",
            Self::CreationOrder => "Creation Order",
            Self::Similarity => "Similarity",
            Self::Name => "Name",
            Self::Rating => "Rating",
            Self::Recent => "Recent",
        }
    }

    /// All variants in display order.
    pub const fn all() -> &'static [SortMode] {
        &[
            Self::StompboxOrder,
            Self::CreationOrder,
            Self::Similarity,
            Self::Name,
            Self::Rating,
            Self::Recent,
        ]
    }
}

// ─── BrowseLevel ─────────────────────────────────────────────────

/// Which preset hierarchy level to browse.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, ::facet::Facet, Default)]
#[repr(u8)]
pub enum BrowseLevel {
    /// All levels simultaneously.
    #[default]
    All = 0,
    /// Rack-level presets.
    Rack = 1,
    /// Rig-level presets.
    Rig = 2,
    /// Engine-level presets.
    Engine = 3,
    /// Layer-level presets.
    Layer = 4,
    /// Module-level presets.
    Module = 5,
    /// Block-level presets.
    Block = 6,
}

impl BrowseLevel {
    /// Display name for UI.
    pub const fn display_name(&self) -> &'static str {
        match self {
            Self::All => "All",
            Self::Rack => "Rack",
            Self::Rig => "Rig",
            Self::Engine => "Engine",
            Self::Layer => "Layer",
            Self::Module => "Module",
            Self::Block => "Block",
        }
    }

    /// All variants in display order.
    pub const fn all() -> &'static [BrowseLevel] {
        &[
            Self::All,
            Self::Rack,
            Self::Rig,
            Self::Engine,
            Self::Layer,
            Self::Module,
            Self::Block,
        ]
    }
}

// ─── Sort helper ─────────────────────────────────────────────────

/// Sort items by tag similarity to a reference set.
///
/// Items with more shared tags appear first. Equal items preserve order.
pub fn sort_by_similarity<T: Taggable>(items: &mut [T], reference_tags: &Tags) {
    let ref_all = reference_tags.all();
    items.sort_by(|a, b| {
        let a_count = a.tags().all().intersection(&ref_all).count();
        let b_count = b.tags().all().intersection(&ref_all).count();
        b_count.cmp(&a_count) // Descending: more matches first
    });
}

// ─── Tests ───────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::id::TagId;

    #[test]
    fn browse_mode_default() {
        assert_eq!(BrowseMode::default(), BrowseMode::Semantic);
    }

    #[test]
    fn browse_mode_all() {
        assert_eq!(BrowseMode::all().len(), 4);
    }

    #[test]
    fn browse_mode_guitar_columns() {
        let cols = BrowseMode::Semantic.guitar_columns();
        assert_eq!(cols.len(), 4);
        assert_eq!(cols[0], TagCategory::BaseTone);
    }

    #[test]
    fn browse_mode_keys_columns() {
        let cols = BrowseMode::Semantic.keys_columns();
        assert_eq!(cols.len(), 4);
        assert_eq!(cols[0], TagCategory::Instrument);
    }

    #[test]
    fn sort_mode_default() {
        assert_eq!(SortMode::default(), SortMode::CreationOrder);
    }

    #[test]
    fn sort_mode_all() {
        assert_eq!(SortMode::all().len(), 6);
    }

    #[test]
    fn browse_level_default() {
        assert_eq!(BrowseLevel::default(), BrowseLevel::All);
    }

    #[test]
    fn browse_level_all() {
        assert_eq!(BrowseLevel::all().len(), 7);
    }

    #[test]
    fn sort_by_similarity_orders_correctly() {
        use crate::tags::Tags;

        struct Item {
            name: String,
            tags: Tags,
        }
        impl Taggable for Item {
            fn tags(&self) -> &Tags {
                &self.tags
            }
            fn tags_mut(&mut self) -> &mut Tags {
                &mut self.tags
            }
            fn name(&self) -> &str {
                &self.name
            }
        }

        let tag_a = TagId::new();
        let tag_b = TagId::new();
        let tag_c = TagId::new();

        let mut reference = Tags::new();
        reference.add(tag_a);
        reference.add(tag_b);
        reference.add(tag_c);

        let mut item1_tags = Tags::new();
        item1_tags.add(tag_a); // 1 match

        let mut item2_tags = Tags::new();
        item2_tags.add(tag_a);
        item2_tags.add(tag_b);
        item2_tags.add(tag_c); // 3 matches

        let mut item3_tags = Tags::new();
        item3_tags.add(tag_a);
        item3_tags.add(tag_b); // 2 matches

        let mut items = vec![
            Item {
                name: "one".into(),
                tags: item1_tags,
            },
            Item {
                name: "three".into(),
                tags: item2_tags,
            },
            Item {
                name: "two".into(),
                tags: item3_tags,
            },
        ];

        sort_by_similarity(&mut items, &reference);

        assert_eq!(items[0].name, "three"); // 3 matches
        assert_eq!(items[1].name, "two"); // 2 matches
        assert_eq!(items[2].name, "one"); // 1 match
    }
}
