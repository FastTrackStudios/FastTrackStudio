//! Dynamics blocks — compressors, limiters.

use crate::block::{Block, PluginId};

/// Studio compressor block.
pub fn compressor() -> Block {
    Block::new("Compressor", PluginId::vst3("com.fts.compressor", "FTS Compressor"))
}

/// Multiband compressor block (mastering).
pub fn multiband_compressor() -> Block {
    Block::new("Multiband Compressor", PluginId::vst3("com.fts.multiband-comp", "FTS Multiband Compressor"))
}

/// Limiter block (mastering).
pub fn limiter() -> Block {
    Block::new("Limiter", PluginId::vst3("com.fts.limiter", "FTS Limiter"))
}
