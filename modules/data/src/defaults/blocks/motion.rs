//! Motion blocks — tremolo, vibrato, rotary.

use crate::block::{Block, PluginId};

/// Tremolo block.
pub fn tremolo() -> Block {
    Block::new("Tremolo", PluginId::vst3("com.fts.tremolo", "FTS Tremolo"))
}

/// Harmonic tremolo block.
pub fn harmonic_tremolo() -> Block {
    Block::new("Harmonic Tremolo", PluginId::vst3("com.fts.harmonic-trem", "FTS Harmonic Tremolo"))
}

/// Vibrato block.
pub fn vibrato() -> Block {
    Block::new("Vibrato", PluginId::vst3("com.fts.vibrato", "FTS Vibrato"))
}

/// Rotary speaker block.
pub fn rotary() -> Block {
    Block::new("Rotary", PluginId::vst3("com.fts.rotary", "FTS Rotary"))
}
