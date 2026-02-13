//! Drums rig template — 6-module signal chain with ~12 block slots.
//!
//! Signal chain order:
//! Source → Dynamics → EQ → Special → Time → Master

use crate::block::BlockType;
use crate::module::ModuleType;
use crate::rig::InstrumentType;
use crate::template::{BlockTemplate, EngineTemplate, LayerTemplate, ModuleTemplate, RigTemplate};
use crate::version::LayerIndex;

/// Standard drums rig template with 6 modules in a single engine/layer.
pub fn drums_rig_template() -> RigTemplate {
    let layer = LayerTemplate::new("Main", LayerIndex::new(1))
        .with_module(drums_source())
        .with_module(drums_dynamics())
        .with_module(drums_eq())
        .with_module(drums_special())
        .with_module(drums_time())
        .with_module(drums_master());

    let engine = EngineTemplate::new("Drums Engine", InstrumentType::Drums, layer);

    RigTemplate::new("Drums Rig Template", InstrumentType::Drums, engine)
        .with_description("Standard drums signal chain with 6 processing stages")
}

fn drums_source() -> ModuleTemplate {
    ModuleTemplate::new("Source", ModuleType::Source)
        .with_description("Input conditioning — gate and input level")
        .with_block(
            BlockTemplate::new("Input", BlockType::Input)
                .with_description("Drum mic / trigger input"),
        )
        .with_block(
            BlockTemplate::new("Gate", BlockType::Gate)
                .with_description("Noise gate for bleed reduction"),
        )
}

fn drums_dynamics() -> ModuleTemplate {
    ModuleTemplate::new("Dynamics", ModuleType::Dynamics)
        .with_description("Compression and transient shaping")
        .with_block(
            BlockTemplate::new("Compressor", BlockType::Compressor)
                .with_description("Drum bus compression for punch and glue"),
        )
        .with_block(
            BlockTemplate::new("Expander Gate", BlockType::Gate)
                .with_description("Expander/gate for tightening drum hits"),
        )
}

fn drums_eq() -> ModuleTemplate {
    ModuleTemplate::new("EQ", ModuleType::Eq)
        .with_description("Drum tone shaping")
        .with_block(
            BlockTemplate::new("Drum EQ", BlockType::Eq)
                .with_description("Parametric EQ for drum tuning and resonance control"),
        )
}

fn drums_special() -> ModuleTemplate {
    ModuleTemplate::new("Special", ModuleType::Special)
        .with_description("Transient shaping and pitch")
        .with_block(
            BlockTemplate::new("Transient Shaper", BlockType::Special)
                .with_description("Attack and sustain control for drum transients"),
        )
        .with_block(
            BlockTemplate::new("Pitch", BlockType::Pitch)
                .with_description("Drum tuning and pitch correction"),
        )
}

fn drums_time() -> ModuleTemplate {
    ModuleTemplate::new("Time", ModuleType::Time)
        .with_description("Ambience and space")
        .with_block(
            BlockTemplate::new("Room Reverb", BlockType::Reverb)
                .with_description("Room reverb for natural ambience"),
        )
        .with_block(
            BlockTemplate::new("Plate Reverb", BlockType::Reverb)
                .with_description("Plate reverb for snare and toms"),
        )
}

fn drums_master() -> ModuleTemplate {
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
