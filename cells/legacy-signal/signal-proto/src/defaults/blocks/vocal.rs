//! Vocal-specific blocks — de-esser, tuner, saturator, rescue EQ, tonal EQ.

use crate::block::{Block, BlockType, PluginId};

/// De-Esser block (Techivation T-De-Esser 2).
pub fn de_esser() -> Block {
    Block::new(
        "De-Esser",
        PluginId::vst3("com.techivation.t-de-esser-2", "T-De-Esser 2"),
    )
    .with_block_type(BlockType::DeEsser)
    .with_description("Sibilance reduction — tame harsh S and T sounds")
}

/// Gate block (Techivation Renegate).
pub fn renegate() -> Block {
    Block::new(
        "Renegate",
        PluginId::vst3("com.techivation.renegate", "Renegate"),
    )
    .with_block_type(BlockType::Gate)
    .with_description("Noise gate for room noise and bleed reduction")
}

/// Rescue EQ block (REAPER native ReaEQ).
pub fn rescue_eq() -> Block {
    Block::new("Rescue EQ", PluginId::vst3("com.cockos.reaeq", "ReaEQ"))
        .with_block_type(BlockType::Eq)
        .with_alias("Rescue-EQ")
        .with_description("Subtractive EQ for fixing problem frequencies")
}

/// Control compressor block (REAPER native ReaComp).
pub fn control_compressor() -> Block {
    Block::new(
        "Control Compressor",
        PluginId::vst3("com.cockos.reacomp", "ReaComp"),
    )
    .with_block_type(BlockType::Compressor)
    .with_alias("Ctrl-Comp")
    .with_description("Gentle compression to even out dynamic range")
}

/// Tuner / pitch correction block (Auburn Sounds Graillon 3).
pub fn tuner() -> Block {
    Block::new(
        "Tuner",
        PluginId::vst3("com.auburnsounds.graillon3", "Graillon 3"),
    )
    .with_block_type(BlockType::Tuner)
    .with_description("Real-time pitch correction and tuning")
}

/// Style compressor block (REAPER native ReaComp).
pub fn style_compressor() -> Block {
    Block::new(
        "Style Compressor",
        PluginId::vst3("com.cockos.reacomp", "ReaComp"),
    )
    .with_block_type(BlockType::Compressor)
    .with_alias("Style-Comp")
    .with_description("Character compression for vocal tone and glue")
}

/// Tonal EQ block (REAPER native ReaEQ).
pub fn tonal_eq() -> Block {
    Block::new("Tonal EQ", PluginId::vst3("com.cockos.reaeq", "ReaEQ"))
        .with_block_type(BlockType::Eq)
        .with_alias("Tone-EQ")
        .with_description("Additive EQ for presence, air, and warmth")
}

/// Saturator block (Techivation T-Saturator).
pub fn saturator() -> Block {
    Block::new(
        "Saturator",
        PluginId::vst3("com.techivation.t-saturator", "T-Saturator"),
    )
    .with_block_type(BlockType::Saturator)
    .with_description("Harmonic saturation for warmth and grit")
}

/// Vocal chorus block (TAL-Chorus-LX).
pub fn vocal_chorus() -> Block {
    Block::new(
        "Vocal Chorus",
        PluginId::vst3("com.tal-software.tal-chorus-lx", "TAL-Chorus-LX"),
    )
    .with_block_type(BlockType::Chorus)
    .with_description("Subtle chorus for vocal width and shimmer")
}

/// Vocal flanger block.
pub fn vocal_flanger() -> Block {
    Block::new(
        "Vocal Flanger",
        PluginId::vst3("com.fts.vocal-flanger", "FTS Vocal Flanger"),
    )
    .with_block_type(BlockType::Flanger)
    .with_description("Flanger for creative vocal effects")
}

/// Verb send block.
pub fn verb_send() -> Block {
    Block::new(
        "Verb Send",
        PluginId::vst3("com.fts.verb-send", "FTS Verb Send"),
    )
    .with_block_type(BlockType::Send)
    .with_description("Send to reverb bus for space and depth")
}

/// Delay send block.
pub fn delay_send() -> Block {
    Block::new(
        "Delay Send",
        PluginId::vst3("com.fts.delay-send", "FTS Delay Send"),
    )
    .with_block_type(BlockType::Send)
    .with_description("Send to delay bus for echo and repeat")
}

/// Special send block.
pub fn special_send() -> Block {
    Block::new(
        "Special Send",
        PluginId::vst3("com.fts.special-send", "FTS Special Send"),
    )
    .with_block_type(BlockType::Send)
    .with_description("Send to special effects bus")
}
