//! Default Keybind Presets and Override Layers
//!
//! Built-in keybind configurations that ship with the extension.
//!
//! Presets are composed from sections in [`super::sections`].

mod fast_slip_edit;
mod fts;
mod logic;
mod reaper;
mod reavim;
mod tempo_map;

pub use fast_slip_edit::fast_slip_edit_override;
pub use fts::fastrackstudio_preset;
pub use logic::logic_preset;
pub use reaper::reaper_preset;
pub use reavim::reavim_preset;
pub use tempo_map::tempo_map_override;

use super::{KeybindOverride, KeybindPreset};

/// All built-in presets
pub static ALL_PRESETS: &[fn() -> KeybindPreset] = &[
    fastrackstudio_preset,
    reaper_preset,
    logic_preset,
    reavim_preset,
];

/// All built-in override layers
pub static ALL_OVERRIDES: &[fn() -> KeybindOverride] = &[tempo_map_override, fast_slip_edit_override];
