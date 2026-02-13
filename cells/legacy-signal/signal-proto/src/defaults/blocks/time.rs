//! Time-based blocks — delay, reverb, freeze.

use crate::block::{Block, BlockType, PluginId};

/// Delay block.
pub fn delay() -> Block {
    Block::new("Delay", PluginId::vst3("com.fts.delay", "FTS Delay"))
        .with_block_type(BlockType::Delay)
}

/// Reverb block.
pub fn reverb() -> Block {
    Block::new("Reverb", PluginId::vst3("com.fts.reverb", "FTS Reverb"))
        .with_block_type(BlockType::Reverb)
}

/// Spring reverb block.
pub fn spring_reverb() -> Block {
    Block::new(
        "Spring Reverb",
        PluginId::vst3("com.fts.spring-reverb", "FTS Spring Reverb"),
    )
    .with_block_type(BlockType::Reverb)
}

/// Freeze / infinite hold block.
pub fn freeze() -> Block {
    Block::new("Freeze", PluginId::vst3("com.fts.freeze", "FTS Freeze"))
        .with_block_type(BlockType::Freeze)
}

/// Plex delay block (tape-style multi-head).
pub fn plex() -> Block {
    Block::new("Plex", PluginId::vst3("com.fts.plex", "FTS Plex"))
        .with_block_type(BlockType::Reverb)
}
