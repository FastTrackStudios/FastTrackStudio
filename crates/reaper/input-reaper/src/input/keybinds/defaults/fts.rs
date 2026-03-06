//! FastTrackStudio Keybind Preset
//!
//! Optimized keybindings for FastTrackStudio workflow.
//! Uses vim-inspired navigation (hjkl) with DAW-specific extensions.

use crate::input::keybinds::sections::{
    FtsEditing, FtsMarkersRegions, FtsMouseModifiers, FtsNavigation, FtsScrolling, FtsTransport,
};
use crate::input::keybinds::{KeybindPreset, PresetBuilder};

/// Create the FastTrackStudio preset using composable sections
pub fn fastrackstudio_preset() -> KeybindPreset {
    PresetBuilder::new(
        "fastrackstudio",
        "FastTrackStudio optimized keybindings with vim-style navigation",
    )
    .version("1.0.0")
    // Compose from sections - order matters for conflict resolution
    .with_section(FtsNavigation)
    .with_section(FtsTransport)
    .with_section(FtsEditing)
    .with_section(FtsScrolling)
    .with_section(FtsMarkersRegions)
    .with_section(FtsMouseModifiers)
    .build()
}
