//! Mouse Modifiers Management
//!
//! Provides functionality to get, set, save, and restore REAPER mouse modifier assignments.
//! Organized by context categories for easy maintenance.

pub mod core;
pub mod preset;
pub mod contexts;
pub mod types;
pub mod actions;
pub mod behaviors;

pub use core::{get_mouse_modifier, set_mouse_modifier, MouseModifierFlag};
pub use preset::{save_all_modifiers, load_preset, list_presets, delete_preset};
pub use contexts::ALL_CONTEXTS;
pub use types::{
    MouseModifierAction,
    MouseModifierContext,
    ModifierFlag,
    ContextModifierMap,
    MouseModifierMap,
    load_all_modifiers,
    apply_modifier_map,
    // Base mouse button input enum
    MouseButtonInput,
    // Context interaction enums
    ArrangeViewInteraction,
    AutomationItemInteraction,
    EnvelopeInteraction,
    FixedLaneInteraction,
    MidiInteraction,
    MediaItemInteraction,
    MixerInteraction,
    ProjectInteraction,
    RazorEditInteraction,
    RulerInteraction,
    TrackInteraction,
    // Special interaction enums (Edge, Rate, etc.)
    NoteInteraction,
    EdgeInteraction,
    StretchMarkerInteraction,
};
