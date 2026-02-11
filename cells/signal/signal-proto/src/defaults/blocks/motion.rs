//! Motion blocks — tremolo, vibrato, rotary.

use crate::block::{Block, BlockType, PluginId};

/// Tremolo block.
pub fn tremolo() -> Block {
    Block::new("Tremolo", PluginId::vst3("com.fts.tremolo", "FTS Tremolo"))
        .with_block_type(BlockType::Tremolo)
}

/// Harmonic tremolo block.
pub fn harmonic_tremolo() -> Block {
    Block::new(
        "Harmonic Tremolo",
        PluginId::vst3("com.fts.harmonic-trem", "FTS Harmonic Tremolo"),
    )
    .with_block_type(BlockType::Tremolo)
}

/// Vibrato block.
pub fn vibrato() -> Block {
    Block::new("Vibrato", PluginId::vst3("com.fts.vibrato", "FTS Vibrato"))
        .with_block_type(BlockType::Vibrato)
}

/// Rotary speaker block.
pub fn rotary() -> Block {
    Block::new("Rotary", PluginId::vst3("com.fts.rotary", "FTS Rotary"))
        .with_block_type(BlockType::Rotary)
}
