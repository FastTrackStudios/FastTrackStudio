//! Time-based blocks — delay, reverb, freeze.

use crate::block::{Block, PluginId};

/// Delay block.
pub fn delay() -> Block {
    Block::new("Delay", PluginId::vst3("com.fts.delay", "FTS Delay"))
}

/// Reverb block.
pub fn reverb() -> Block {
    Block::new("Reverb", PluginId::vst3("com.fts.reverb", "FTS Reverb"))
}

/// Spring reverb block.
pub fn spring_reverb() -> Block {
    Block::new("Spring Reverb", PluginId::vst3("com.fts.spring-reverb", "FTS Spring Reverb"))
}

/// Freeze / infinite hold block.
pub fn freeze() -> Block {
    Block::new("Freeze", PluginId::vst3("com.fts.freeze", "FTS Freeze"))
}

/// Plex delay block (tape-style multi-head).
pub fn plex() -> Block {
    Block::new("Plex", PluginId::vst3("com.fts.plex", "FTS Plex"))
}
