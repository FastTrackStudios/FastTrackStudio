//! Modulation blocks — chorus, flanger, phaser.

use crate::block::{Block, BlockType, PluginId};

/// Chorus block.
pub fn chorus() -> Block {
    Block::new("Chorus", PluginId::vst3("com.fts.chorus", "FTS Chorus"))
        .with_block_type(BlockType::Chorus)
}

/// Flanger block.
pub fn flanger() -> Block {
    Block::new("Flanger", PluginId::vst3("com.fts.flanger", "FTS Flanger"))
        .with_block_type(BlockType::Flanger)
}

/// Phaser block.
pub fn phaser() -> Block {
    Block::new("Phaser", PluginId::vst3("com.fts.phaser", "FTS Phaser"))
        .with_block_type(BlockType::Phaser)
}
