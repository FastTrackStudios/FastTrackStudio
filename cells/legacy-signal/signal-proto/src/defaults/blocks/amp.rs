//! Amp, cabinet, and room send blocks.

use crate::block::{Block, BlockType, PluginId};

/// Dream amp block (Fender-voiced clean).
pub fn dream() -> Block {
    Block::new("Dream", PluginId::vst3("com.fts.dream", "FTS Dream"))
        .with_block_type(BlockType::Amp)
}

/// Ruby amp block (Vox-voiced).
pub fn ruby() -> Block {
    Block::new("Ruby", PluginId::vst3("com.fts.ruby", "FTS Ruby")).with_block_type(BlockType::Amp)
}

/// Deluxe amp block (Fender Deluxe).
pub fn deluxe() -> Block {
    Block::new("Deluxe", PluginId::vst3("com.fts.deluxe", "FTS Deluxe"))
        .with_block_type(BlockType::Amp)
}

/// AC30 amp block (Vox AC30).
pub fn ac30() -> Block {
    Block::new("AC30", PluginId::vst3("com.fts.ac30", "FTS AC30")).with_block_type(BlockType::Amp)
}

/// Dumble amp block.
pub fn dumble() -> Block {
    Block::new("Dumble", PluginId::vst3("com.fts.dumble", "FTS Dumble"))
        .with_block_type(BlockType::Amp)
}

/// Two-Rock amp block.
pub fn two_rock() -> Block {
    Block::new(
        "Two-Rock",
        PluginId::vst3("com.fts.two-rock", "FTS Two-Rock"),
    )
    .with_block_type(BlockType::Amp)
}

/// Marshall amp block (JCM800 style).
pub fn marshall() -> Block {
    Block::new(
        "Marshall",
        PluginId::vst3("com.fts.marshall", "FTS Marshall"),
    )
    .with_block_type(BlockType::Amp)
}

/// Cabinet IR block.
pub fn cabinet() -> Block {
    Block::new(
        "Cabinet",
        PluginId::vst3("com.fts.cabinet-ir", "FTS Cabinet IR"),
    )
    .with_block_type(BlockType::Cabinet)
}

/// Room send block (reverb return for amp room sim).
pub fn room_send() -> Block {
    Block::new(
        "Room Send",
        PluginId::vst3("com.fts.room-send", "FTS Room Send"),
    )
    .with_block_type(BlockType::Send)
}

/// Volume pedal block.
pub fn volume_pedal() -> Block {
    Block::new(
        "Volume Pedal",
        PluginId::vst3("com.fts.volume-pedal", "FTS Volume Pedal"),
    )
    .with_block_type(BlockType::Volume)
    .with_description("Expression pedal for volume swells and control")
}

/// Output volume block (mastering).
pub fn output_volume() -> Block {
    Block::new(
        "Output Volume",
        PluginId::vst3("com.fts.output-volume", "FTS Output Volume"),
    )
    .with_block_type(BlockType::Volume)
    .with_alias("Out-Vol")
    .with_description("Final output level")
}
