//! EQ blocks.

use crate::block::{Block, PluginId};

/// Parametric EQ block.
pub fn parametric_eq() -> Block {
    Block::new("Parametric EQ", PluginId::vst3("com.fts.parametric-eq", "FTS Parametric EQ"))
}

/// Post EQ block (output stage).
pub fn post_eq() -> Block {
    Block::new("Post EQ", PluginId::vst3("com.fts.post-eq", "FTS Post EQ"))
}

/// Mastering EQ block.
pub fn mastering_eq() -> Block {
    Block::new("Mastering EQ", PluginId::vst3("com.fts.mastering-eq", "FTS Mastering EQ"))
}
