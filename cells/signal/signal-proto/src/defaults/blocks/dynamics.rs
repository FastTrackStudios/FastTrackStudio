//! Dynamics blocks — compressors, limiters.

use crate::block::{Block, BlockType, PluginId};

/// Studio compressor block.
pub fn compressor() -> Block {
    Block::new(
        "Compressor",
        PluginId::vst3("com.fts.compressor", "FTS Compressor"),
    )
    .with_block_type(BlockType::Compressor)
    .with_description("Main compressor for dynamic control")
}

/// Multiband compressor block (mastering).
pub fn multiband_compressor() -> Block {
    Block::new(
        "Multiband Compressor",
        PluginId::vst3("com.fts.multiband-comp", "FTS Multiband Compressor"),
    )
    .with_block_type(BlockType::Compressor)
    .with_alias("MB-Comp")
    .with_description("Multiband compression for frequency-specific control")
}

/// Limiter block (mastering).
pub fn limiter() -> Block {
    Block::new("Limiter", PluginId::vst3("com.fts.limiter", "FTS Limiter"))
        .with_block_type(BlockType::Limiter)
        .with_description("Output limiter for peak protection")
}
