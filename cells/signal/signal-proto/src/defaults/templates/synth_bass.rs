//! Synth bass rig template — 7-module signal chain with ~14 block slots.
//!
//! Signal chain order:
//! Source → Special → Drive → Dynamics → EQ → Time → Master
//!
//! Note: filter before drive is the classic synth bass signal flow —
//! shape the raw oscillator first, then add harmonic saturation.

use crate::block::BlockType;
use crate::module::ModuleType;
use crate::rig::InstrumentType;
use crate::template::{BlockTemplate, EngineTemplate, LayerTemplate, ModuleTemplate, RigTemplate};
use crate::version::LayerIndex;

/// Synth bass rig template with 7 modules in a single engine/layer.
pub fn synth_bass_rig_template() -> RigTemplate {
    let layer = LayerTemplate::new("Main", LayerIndex::new(1))
        .with_module(sb_source())
        .with_module(sb_special())
        .with_module(sb_drive())
        .with_module(sb_dynamics())
        .with_module(sb_eq())
        .with_module(sb_time())
        .with_module(sb_master());

    let engine = EngineTemplate::new(
        "Synth Bass Engine",
        InstrumentType::Custom("Synth Bass".into()),
        layer,
    );

    RigTemplate::new(
        "Synth Bass Rig Template",
        InstrumentType::Custom("Synth Bass".into()),
        engine,
    )
    .with_description("Synth bass chain — filter shaping, saturation, and compression")
}

fn sb_source() -> ModuleTemplate {
    ModuleTemplate::new("Source", ModuleType::Source)
        .with_description("Synth bass input")
        .with_block(
            BlockTemplate::new("Input", BlockType::Input)
                .with_description("Synth oscillator / plugin input"),
        )
}

fn sb_special() -> ModuleTemplate {
    ModuleTemplate::new("Special", ModuleType::Special)
        .with_description("Filter and pitch — shape the raw oscillator")
        .with_block(
            BlockTemplate::new("Filter", BlockType::Filter)
                .with_description("Low-pass / resonant filter for synth bass character"),
        )
        .with_block(
            BlockTemplate::new("Sub Octave", BlockType::Pitch)
                .with_description("Sub-octave generator for deep bass"),
        )
}

fn sb_drive() -> ModuleTemplate {
    ModuleTemplate::new("Drive", ModuleType::Drive)
        .with_description("Saturation and harmonic richness")
        .with_block(
            BlockTemplate::new("Drive", BlockType::Drive)
                .with_description("Overdrive for growl and presence"),
        )
        .with_block(
            BlockTemplate::new("Saturator", BlockType::Saturator)
                .with_description("Tape/tube saturation for warmth"),
        )
}

fn sb_dynamics() -> ModuleTemplate {
    ModuleTemplate::new("Dynamics", ModuleType::Dynamics)
        .with_description("Compression for consistent level")
        .with_block(
            BlockTemplate::new("Compressor", BlockType::Compressor)
                .with_description("Heavy compression for tight, even bass"),
        )
}

fn sb_eq() -> ModuleTemplate {
    ModuleTemplate::new("EQ", ModuleType::Eq)
        .with_description("Tone sculpting")
        .with_block(
            BlockTemplate::new("Synth Bass EQ", BlockType::Eq)
                .with_description("Parametric EQ — boost sub, cut mud, add presence"),
        )
}

fn sb_time() -> ModuleTemplate {
    ModuleTemplate::new("Time", ModuleType::Time)
        .with_description("Delay and reverb")
        .with_block(
            BlockTemplate::new("Delay", BlockType::Delay)
                .with_description("Tempo-synced delay for rhythmic effects"),
        )
        .with_block(
            BlockTemplate::new("Reverb", BlockType::Reverb)
                .with_description("Short reverb for space without muddiness"),
        )
}

fn sb_master() -> ModuleTemplate {
    ModuleTemplate::new("Master", ModuleType::Master)
        .with_description("Output processing — limiter and volume")
        .with_block(
            BlockTemplate::new("Limiter", BlockType::Limiter)
                .with_description("Brick-wall limiter for output protection"),
        )
        .with_block(
            BlockTemplate::new("Output Volume", BlockType::Volume)
                .with_description("Master output level"),
        )
}
