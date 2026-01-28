//! Vocal-specific blocks — de-esser, tuner, saturator, rescue EQ, tonal EQ.

use crate::block::{Block, PluginId};

/// De-Esser block (Techivation T-De-Esser 2).
pub fn de_esser() -> Block {
    Block::new("De-Esser", PluginId::vst3("com.techivation.t-de-esser-2", "T-De-Esser 2"))
}

/// Gate block (Techivation Renegate).
pub fn renegate() -> Block {
    Block::new("Renegate", PluginId::vst3("com.techivation.renegate", "Renegate"))
}

/// Rescue EQ block (REAPER native ReaEQ).
pub fn rescue_eq() -> Block {
    Block::new("Rescue EQ", PluginId::vst3("com.cockos.reaeq", "ReaEQ"))
}

/// Control compressor block (REAPER native ReaComp).
pub fn control_compressor() -> Block {
    Block::new("Control Compressor", PluginId::vst3("com.cockos.reacomp", "ReaComp"))
}

/// Tuner / pitch correction block (Auburn Sounds Graillon 3).
pub fn tuner() -> Block {
    Block::new("Tuner", PluginId::vst3("com.auburnsounds.graillon3", "Graillon 3"))
}

/// Style compressor block (REAPER native ReaComp).
pub fn style_compressor() -> Block {
    Block::new("Style Compressor", PluginId::vst3("com.cockos.reacomp", "ReaComp"))
}

/// Tonal EQ block (REAPER native ReaEQ).
pub fn tonal_eq() -> Block {
    Block::new("Tonal EQ", PluginId::vst3("com.cockos.reaeq", "ReaEQ"))
}

/// Saturator block (Techivation T-Saturator).
pub fn saturator() -> Block {
    Block::new("Saturator", PluginId::vst3("com.techivation.t-saturator", "T-Saturator"))
}

/// Vocal chorus block (TAL-Chorus-LX).
pub fn vocal_chorus() -> Block {
    Block::new("Vocal Chorus", PluginId::vst3("com.tal-software.tal-chorus-lx", "TAL-Chorus-LX"))
}

/// Vocal flanger block.
pub fn vocal_flanger() -> Block {
    Block::new("Vocal Flanger", PluginId::vst3("com.fts.vocal-flanger", "FTS Vocal Flanger"))
}

/// Verb send block.
pub fn verb_send() -> Block {
    Block::new("Verb Send", PluginId::vst3("com.fts.verb-send", "FTS Verb Send"))
}

/// Delay send block.
pub fn delay_send() -> Block {
    Block::new("Delay Send", PluginId::vst3("com.fts.delay-send", "FTS Delay Send"))
}

/// Special send block.
pub fn special_send() -> Block {
    Block::new("Special Send", PluginId::vst3("com.fts.special-send", "FTS Special Send"))
}
