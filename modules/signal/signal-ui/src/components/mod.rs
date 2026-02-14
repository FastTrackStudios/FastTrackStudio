//! Dumb presentation components for signal UI.
//!
//! These components are domain-agnostic: they take all data via props and
//! have zero knowledge of signal types, services, or global signals.
//! Domain-aware wrappers compose these into full editor views.

// Tier 1 — direct ports (zero domain deps)
mod entity_editor;
mod review_list;
mod star_rating;

// Tier 2 — ported with domain type erasure
mod block_colors;
mod create_modal;
mod crossfade_indicator;
mod morph_slider;
mod scene_tile;

// Re-exports: layout
pub use entity_editor::EntityEditor;

// Re-exports: ratings & reviews
pub use review_list::{ReviewCard, ReviewData, ReviewList};
pub use star_rating::{PresetRatingBadge, StarRating, StarRatingInput};

// Re-exports: block colors
pub use block_colors::{
    block_bypassed_style, block_color, block_instance_color, block_style, BlockColor,
};

// Re-exports: scene tiles
pub use scene_tile::{SceneTileCell, SceneTileGrid, TileData};

// Re-exports: morph slider
pub use morph_slider::{DropdownItem, MorphSlider};

// Re-exports: crossfade
pub use crossfade_indicator::CrossfadeIndicator;

// Re-exports: create modal
pub use create_modal::{CreateModal, CreateModalData, ModalConfig, TemplateOption};
