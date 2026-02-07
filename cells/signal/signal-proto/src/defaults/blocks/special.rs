//! Special effect blocks — envelope filter, wah, pitch, doubler.

use crate::block::{Block, PluginId};

/// Envelope filter (auto-wah) block.
pub fn envelope_filter() -> Block {
    Block::new("Envelope Filter", PluginId::vst3("com.fts.envelope-filter", "FTS Envelope Filter"))
}

/// Wah pedal block.
pub fn wah_pedal() -> Block {
    Block::new("Wah Pedal", PluginId::vst3("com.fts.wah", "FTS Wah Pedal"))
}

/// Pitch / octave effect block.
pub fn pitch_octave() -> Block {
    Block::new("Pitch Octave FX", PluginId::vst3("com.fts.pitch-octave", "FTS Pitch Octave"))
}

/// Doubler effect block.
pub fn doubler() -> Block {
    Block::new("Doubler", PluginId::vst3("com.fts.doubler", "FTS Doubler"))
}
