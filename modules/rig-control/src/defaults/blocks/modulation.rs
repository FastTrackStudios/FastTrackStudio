//! Modulation blocks — chorus, flanger, phaser.

use crate::block::{Block, PluginId};

/// Chorus block.
pub fn chorus() -> Block {
    Block::new("Chorus", PluginId::vst3("com.fts.chorus", "FTS Chorus"))
}

/// Flanger block.
pub fn flanger() -> Block {
    Block::new("Flanger", PluginId::vst3("com.fts.flanger", "FTS Flanger"))
}

/// Phaser block.
pub fn phaser() -> Block {
    Block::new("Phaser", PluginId::vst3("com.fts.phaser", "FTS Phaser"))
}
