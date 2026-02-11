//! EQ blocks.

use crate::block::{Block, BlockType, PluginId};

/// Parametric EQ block.
pub fn parametric_eq() -> Block {
    Block::new(
        "Parametric EQ",
        PluginId::vst3("com.fts.parametric-eq", "FTS Parametric EQ"),
    )
    .with_block_type(BlockType::Eq)
    .with_description("Parametric EQ for tone shaping")
}

/// Post EQ block (output stage).
pub fn post_eq() -> Block {
    Block::new("Post EQ", PluginId::vst3("com.fts.post-eq", "FTS Post EQ"))
        .with_block_type(BlockType::Eq)
        .with_description("Post-amp EQ for final tone adjustments")
}

/// Mastering EQ block.
pub fn mastering_eq() -> Block {
    Block::new(
        "Mastering EQ",
        PluginId::vst3("com.fts.mastering-eq", "FTS Mastering EQ"),
    )
    .with_block_type(BlockType::Eq)
    .with_alias("Mstr-EQ")
    .with_description("Final EQ for overall tone balance")
}
