//! Drive blocks — boost, overdrive, distortion.

use crate::block::{Block, PluginId};

/// Clean boost block.
pub fn boost() -> Block {
    Block::new("Boost", PluginId::vst3("com.fts.boost", "FTS Boost"))
}

/// Halfman overdrive block (Klon-style).
pub fn halfman() -> Block {
    Block::new("Halfman", PluginId::vst3("com.fts.halfman", "FTS Halfman"))
}

/// Teal overdrive block (TS-style).
pub fn teal() -> Block {
    Block::new("Teal", PluginId::vst3("com.fts.teal", "FTS Teal"))
}

/// Blues Breaker style overdrive block.
pub fn blues_breaker() -> Block {
    Block::new("Blues Breaker", PluginId::vst3("com.fts.blues-breaker", "FTS Blues Breaker"))
}

/// Protein drive block (dual-mode).
pub fn protein() -> Block {
    Block::new("Protein", PluginId::vst3("com.fts.protein", "FTS Protein"))
}

/// JHS Kilt drive block (fuzz/distortion).
pub fn jhs_kilt() -> Block {
    Block::new("JHS Kilt", PluginId::vst3("com.fts.jhs-kilt", "FTS JHS Kilt"))
}
