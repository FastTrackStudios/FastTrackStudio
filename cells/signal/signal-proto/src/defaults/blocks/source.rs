//! Source blocks — input gate and volume.

use crate::block::{Block, BlockType, PluginId};

/// Input gate block.
pub fn input_gate() -> Block {
    Block::new(
        "Input Gate",
        PluginId::vst3("com.fts.input-gate", "FTS Input Gate"),
    )
    .with_block_type(BlockType::Gate)
    .with_description("Noise gate to clean up input signal")
}

/// Input volume block.
pub fn input_volume() -> Block {
    Block::new(
        "Input Volume",
        PluginId::vst3("com.fts.input-volume", "FTS Input Volume"),
    )
    .with_block_type(BlockType::Volume)
    .with_description("Input level trim before processing")
}
