//! Source blocks — input gate and volume.

use crate::block::{Block, PluginId};

/// Input gate block.
pub fn input_gate() -> Block {
    Block::new("Input Gate", PluginId::vst3("com.fts.input-gate", "FTS Input Gate"))
}

/// Input volume block.
pub fn input_volume() -> Block {
    Block::new("Input Volume", PluginId::vst3("com.fts.input-volume", "FTS Input Volume"))
}
