//! Drum replacement rig template — 7-module signal chain with ~14 block slots.
//!
//! Designed for triggering replacement samples from live drum signals.
//!
//! Signal chain order:
//! Source → Dynamics → Special → Drive → EQ → Time → Master

use crate::block::BlockType;
use crate::module::ModuleType;
use crate::rig::InstrumentType;
use crate::template::{BlockTemplate, EngineTemplate, LayerTemplate, ModuleTemplate, RigTemplate};
use crate::version::LayerIndex;

/// Drum replacement rig template with 7 modules in a single engine/layer.
pub fn drum_replacement_rig_template() -> RigTemplate {
    let layer = LayerTemplate::new("Main", LayerIndex::new(1))
        .with_module(dr_source())
        .with_module(dr_dynamics())
        .with_module(dr_special())
        .with_module(dr_drive())
        .with_module(dr_eq())
        .with_module(dr_time())
        .with_module(dr_master());

    let engine = EngineTemplate::new(
        "Drum Replace Engine",
        InstrumentType::Custom("Drum Replacement".into()),
        layer,
    );

    RigTemplate::new(
        "Drum Replacement Rig Template",
        InstrumentType::Custom("Drum Replacement".into()),
        engine,
    )
    .with_description("Drum replacement chain — trigger detection, sample blending, and processing")
}

fn dr_source() -> ModuleTemplate {
    ModuleTemplate::new("Source", ModuleType::Source)
        .with_description("Input conditioning — gate for trigger isolation")
        .with_block(
            BlockTemplate::new("Input", BlockType::Input)
                .with_description("Drum mic input for trigger detection"),
        )
        .with_block(
            BlockTemplate::new("Trigger Gate", BlockType::Gate)
                .with_description("Tight gate for isolating individual drum hits"),
        )
}

fn dr_dynamics() -> ModuleTemplate {
    ModuleTemplate::new("Dynamics", ModuleType::Dynamics)
        .with_description("Detection gating and compression")
        .with_block(
            BlockTemplate::new("Detection Gate", BlockType::Gate)
                .with_description("Velocity-sensitive gate for trigger accuracy"),
        )
        .with_block(
            BlockTemplate::new("Compressor", BlockType::Compressor)
                .with_description("Level compression for consistent trigger response"),
        )
}

fn dr_special() -> ModuleTemplate {
    ModuleTemplate::new("Special", ModuleType::Special)
        .with_description("Trigger detection and frequency splitting")
        .with_block(
            BlockTemplate::new("Trigger", BlockType::Special)
                .with_description("Transient trigger — converts drum hits to MIDI/sample triggers"),
        )
        .with_block(
            BlockTemplate::new("Crossover", BlockType::Crossover)
                .with_description("Frequency crossover for blending original and replacement"),
        )
}

fn dr_drive() -> ModuleTemplate {
    ModuleTemplate::new("Drive", ModuleType::Drive)
        .with_description("Saturation and harmonic enhancement")
        .with_block(
            BlockTemplate::new("Saturator", BlockType::Saturator)
                .with_description("Subtle saturation to blend replacement with original"),
        )
}

fn dr_eq() -> ModuleTemplate {
    ModuleTemplate::new("EQ", ModuleType::Eq)
        .with_description("Tone matching between original and replacement")
        .with_block(
            BlockTemplate::new("Match EQ", BlockType::Eq)
                .with_description("Parametric EQ for tonal matching"),
        )
}

fn dr_time() -> ModuleTemplate {
    ModuleTemplate::new("Time", ModuleType::Time)
        .with_description("Ambience for blending")
        .with_block(
            BlockTemplate::new("Room Send", BlockType::Send)
                .with_description("Send to room reverb bus for natural blending"),
        )
        .with_block(
            BlockTemplate::new("Ambience", BlockType::Reverb)
                .with_description("Short ambience reverb for sample realism"),
        )
}

fn dr_master() -> ModuleTemplate {
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
