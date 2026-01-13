//! Mouse Modifiers Management
//!
//! Provides functionality to get, set, save, and restore REAPER mouse modifier assignments.
//! Organized by context categories for easy maintenance.
//!
//! ## Key Components
//!
//! - [`core`] - Low-level REAPER API wrappers (GetMouseModifier, SetMouseModifier)
//! - [`manager`] - Layered profile system with stackable overrides
//! - [`preset`] - JSON preset save/load functionality
//! - [`types`] - Type-safe context and modifier enums

pub mod actions;
pub mod behaviors;
pub mod contexts;
pub mod core;
pub mod manager;
pub mod preset;
pub mod types;

pub use contexts::ALL_CONTEXTS;
pub use core::{MouseModifierFlag, get_mouse_modifier, set_mouse_modifier};
pub use preset::{delete_preset, list_presets, load_preset, save_all_modifiers};
pub use types::{
    // Context interaction enums
    ArrangeViewInteraction,
    AutomationItemInteraction,
    ContextModifierMap,
    EdgeInteraction,
    EnvelopeInteraction,
    FixedLaneInteraction,
    MediaItemInteraction,
    MidiInteraction,
    MixerInteraction,
    ModifierFlag,
    // Base mouse button input enum
    MouseButtonInput,
    MouseModifierAction,
    MouseModifierContext,
    MouseModifierMap,
    // Special interaction enums (Edge, Rate, etc.)
    NoteInteraction,
    ProjectInteraction,
    RazorEditInteraction,
    RulerInteraction,
    StretchMarkerInteraction,
    TrackInteraction,
    apply_modifier_map,
    load_all_modifiers,
};
