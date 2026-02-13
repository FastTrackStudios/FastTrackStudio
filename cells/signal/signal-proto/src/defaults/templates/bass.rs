//! Bass rig template — 8-module signal chain with ~18 block slots.
//!
//! Signal chain order:
//! Source → Dynamics → Special → Drive → Amp → EQ → Time → Master

use crate::block::BlockType;
use crate::module::ModuleType;
use crate::rig::InstrumentType;
use crate::template::{BlockTemplate, EngineTemplate, LayerTemplate, ModuleTemplate, RigTemplate};
use crate::version::LayerIndex;

/// Standard bass rig template with 8 modules in a single engine/layer.
pub fn bass_rig_template() -> RigTemplate {
    let layer = LayerTemplate::new("Main", LayerIndex::new(1))
        .with_module(bass_source())
        .with_module(bass_dynamics())
        .with_module(bass_special())
        .with_module(bass_drive())
        .with_module(bass_amp())
        .with_module(bass_eq())
        .with_module(bass_time())
        .with_module(bass_master());

    let engine = EngineTemplate::new("Bass Engine", InstrumentType::Bass, layer);

    RigTemplate::new("Bass Rig Template", InstrumentType::Bass, engine)
        .with_description("Standard bass signal chain with 8 processing stages")
}

fn bass_source() -> ModuleTemplate {
    ModuleTemplate::new("Source", ModuleType::Source)
        .with_description("Input conditioning — gate and input level")
        .with_block(
            BlockTemplate::new("Input", BlockType::Input)
                .with_description("Bass input with impedance matching"),
        )
        .with_block(
            BlockTemplate::new("Gate", BlockType::Gate)
                .with_description("Noise gate for string noise and hum"),
        )
}

fn bass_dynamics() -> ModuleTemplate {
    ModuleTemplate::new("Dynamics", ModuleType::Dynamics)
        .with_description("Compression and dynamic control")
        .with_block(
            BlockTemplate::new("Compressor", BlockType::Compressor)
                .with_description("Main compressor for even dynamics"),
        )
}

fn bass_special() -> ModuleTemplate {
    ModuleTemplate::new("Special", ModuleType::Special)
        .with_description("Filter and pitch effects")
        .with_block(
            BlockTemplate::new("Envelope Filter", BlockType::Filter)
                .with_description("Auto-wah / envelope filter for funk tones"),
        )
        .with_block(
            BlockTemplate::new("Octave", BlockType::Pitch)
                .with_description("Octave up/down for extended range"),
        )
}

fn bass_drive() -> ModuleTemplate {
    ModuleTemplate::new("Drive", ModuleType::Drive)
        .with_description("Overdrive and distortion")
        .with_block(
            BlockTemplate::new("Boost", BlockType::Boost)
                .with_description("Clean boost for pushing the amp"),
        )
        .with_block(
            BlockTemplate::new("Drive", BlockType::Drive)
                .with_description("Bass overdrive / distortion"),
        )
}

fn bass_amp() -> ModuleTemplate {
    ModuleTemplate::new("Amp", ModuleType::Amp)
        .with_description("Bass amp and cabinet simulation")
        .with_block(
            BlockTemplate::new("Bass Amp", BlockType::Amp).with_description("Bass amplifier model"),
        )
        .with_block(
            BlockTemplate::new("Bass Cabinet", BlockType::Cabinet)
                .with_description("Bass cabinet impulse response"),
        )
}

fn bass_eq() -> ModuleTemplate {
    ModuleTemplate::new("EQ", ModuleType::Eq)
        .with_description("Tone shaping and frequency sculpting")
        .with_block(
            BlockTemplate::new("Bass EQ", BlockType::Eq)
                .with_description("Parametric EQ for bass tone shaping"),
        )
}

fn bass_time() -> ModuleTemplate {
    ModuleTemplate::new("Time", ModuleType::Time)
        .with_description("Delay and reverb")
        .with_block(
            BlockTemplate::new("Delay", BlockType::Delay)
                .with_description("Delay for rhythmic and ambient effects"),
        )
        .with_block(
            BlockTemplate::new("Reverb", BlockType::Reverb)
                .with_description("Reverb for space and depth"),
        )
}

fn bass_master() -> ModuleTemplate {
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
