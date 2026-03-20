//! UI module for FTS plugins — Blitz-compatible components and theme.
//!
//! All components use inline styles for reliable rendering in the
//! nih_plug_dioxus Blitz renderer. Import everything via the prelude:
//!
//! ```ignore
//! use fts_plugin_core::ui::prelude::*;
//! ```
//!
//! The shared component library lives in the `audio-gui` crate.
//! This module re-exports it alongside legacy components for convenience.

pub mod components;

/// Theme re-exported from `audio-gui`.
pub use audio_gui::theme;

/// UI prelude — import this for all FTS plugin UI building blocks.
///
/// Includes everything from `audio_gui::prelude` (Knob, ParamSlider,
/// LevelMeter, TransferCurve, etc.) plus legacy components (Toggle,
/// Section, etc.) that haven't been moved yet.
pub mod prelude {
    pub use super::components::*;
    pub use audio_gui::prelude::*;
}
