//! Synth rig template — 3 engines (Keys, Organ, Synth) in a single rig.
//!
//! Each engine has its own module chain tailored to its instrument type:
//! - **Keys Engine**: Source → Dynamics → EQ → Time → Master
//! - **Organ Engine**: Source → Drive → Modulation → Motion → Master
//! - **Synth Engine**: Source → Special → Modulation → Time → Master

use crate::block::BlockType;
use crate::module::ModuleType;
use crate::rig::InstrumentType;
use crate::template::{BlockTemplate, EngineTemplate, LayerTemplate, ModuleTemplate, RigTemplate};
use crate::version::LayerIndex;

// ─────────────────────────────────────────────────────────────────────────────
// Synth Rig Template (3 engines)
// ─────────────────────────────────────────────────────────────────────────────

/// Synth rig template with three engines: Keys, Organ, and Synth.
///
/// Each engine has a tailored module chain for its instrument type.
/// The rig's overall `InstrumentType` is `Synth` while individual
/// engines carry their own types (Keys, Custom("Organ"), Synth).
pub fn synth_rig_template() -> RigTemplate {
    let keys = keys_engine_template();
    let organ = organ_engine_template();
    let synth = synth_engine_template();

    let mut template = RigTemplate::new("Synth Rig Template", InstrumentType::Synth, keys);
    template.engines.push(organ);
    template.engines.push(synth);
    template.description = Some(
        "Multi-engine synth rig: Keys (piano/EP), Organ (B3/tonewheel), Synth (pads/leads)".into(),
    );
    template
}

// ─────────────────────────────────────────────────────────────────────────────
// Keys Engine — Piano / Electric Piano
// ─────────────────────────────────────────────────────────────────────────────

/// Keys engine: Source → Dynamics → EQ → Time → Master.
fn keys_engine_template() -> EngineTemplate {
    let layer = LayerTemplate::new("Main", LayerIndex::new(1))
        .with_module(keys_source())
        .with_module(keys_dynamics())
        .with_module(keys_eq())
        .with_module(keys_time())
        .with_module(keys_master());

    EngineTemplate::new("Keys Engine", InstrumentType::Keys, layer)
        .with_description("Piano and electric piano processing")
}

fn keys_source() -> ModuleTemplate {
    ModuleTemplate::new("Source", ModuleType::Source)
        .with_description("Keys input conditioning")
        .with_block(
            BlockTemplate::new("Keys Input", BlockType::Input)
                .with_description("Keys instrument source input"),
        )
}

fn keys_dynamics() -> ModuleTemplate {
    ModuleTemplate::new("Dynamics", ModuleType::Dynamics)
        .with_description("Gentle compression for piano dynamics")
        .with_block(
            BlockTemplate::new("Keys Compressor", BlockType::Compressor)
                .with_description("Light compression to control piano dynamics"),
        )
}

fn keys_eq() -> ModuleTemplate {
    ModuleTemplate::new("EQ", ModuleType::Eq)
        .with_description("Tonal shaping for keys")
        .with_block(
            BlockTemplate::new("Keys EQ", BlockType::Eq)
                .with_description("Shape piano/EP tone — warmth, presence, air"),
        )
}

fn keys_time() -> ModuleTemplate {
    ModuleTemplate::new("Time", ModuleType::Time)
        .with_description("Ambient effects for keys")
        .with_block(
            BlockTemplate::new("Keys Delay", BlockType::Delay)
                .with_description("Subtle delay for rhythmic interest"),
        )
        .with_block(
            BlockTemplate::new("Keys Reverb", BlockType::Reverb)
                .with_description("Hall/plate reverb for space and depth"),
        )
}

fn keys_master() -> ModuleTemplate {
    ModuleTemplate::new("Master", ModuleType::Master)
        .with_description("Keys output stage")
        .with_block(
            BlockTemplate::new("Keys Limiter", BlockType::Limiter)
                .with_description("Peak protection for keys output"),
        )
        .with_block(
            BlockTemplate::new("Keys Volume", BlockType::Volume)
                .with_alias("Keys-Vol")
                .with_description("Keys output level"),
        )
}

// ─────────────────────────────────────────────────────────────────────────────
// Organ Engine — B3 / Tonewheel
// ─────────────────────────────────────────────────────────────────────────────

/// Organ engine: Source → Drive → Modulation → Motion → Master.
fn organ_engine_template() -> EngineTemplate {
    let layer = LayerTemplate::new("Main", LayerIndex::new(1))
        .with_module(organ_source())
        .with_module(organ_drive())
        .with_module(organ_modulation())
        .with_module(organ_motion())
        .with_module(organ_master());

    EngineTemplate::new(
        "Organ Engine",
        InstrumentType::Custom("Organ".into()),
        layer,
    )
    .with_description("B3 / tonewheel organ with Leslie speaker simulation")
}

fn organ_source() -> ModuleTemplate {
    ModuleTemplate::new("Source", ModuleType::Source)
        .with_description("Organ input conditioning")
        .with_block(
            BlockTemplate::new("Organ Input", BlockType::Input)
                .with_description("Organ instrument source input"),
        )
}

fn organ_drive() -> ModuleTemplate {
    ModuleTemplate::new("Drive", ModuleType::Drive)
        .with_description("Tube overdrive for organ grit")
        .with_block(
            BlockTemplate::new("Organ Overdrive", BlockType::Drive)
                .with_description("Light tube overdrive — classic B3 breakup"),
        )
}

fn organ_modulation() -> ModuleTemplate {
    ModuleTemplate::new("Modulation", ModuleType::Modulation)
        .with_description("Chorus for Leslie-style width")
        .with_block(
            BlockTemplate::new("Organ Chorus", BlockType::Chorus)
                .with_description("Chorus for stereo width and Leslie modulation"),
        )
}

fn organ_motion() -> ModuleTemplate {
    ModuleTemplate::new("Motion", ModuleType::Motion)
        .with_description("Rotary speaker — the heart of the organ sound")
        .with_block(
            BlockTemplate::new("Rotary Speaker", BlockType::Rotary)
                .with_description("Leslie rotary speaker simulation — slow/fast toggle"),
        )
}

fn organ_master() -> ModuleTemplate {
    ModuleTemplate::new("Master", ModuleType::Master)
        .with_description("Organ output stage")
        .with_block(
            BlockTemplate::new("Organ Limiter", BlockType::Limiter)
                .with_description("Peak protection for organ output"),
        )
        .with_block(
            BlockTemplate::new("Organ Volume", BlockType::Volume)
                .with_alias("Organ-Vol")
                .with_description("Organ output level"),
        )
}

// ─────────────────────────────────────────────────────────────────────────────
// Synth Engine — Pads / Leads
// ─────────────────────────────────────────────────────────────────────────────

/// Synth engine: Source → Special → Modulation → Time → Master.
fn synth_engine_template() -> EngineTemplate {
    let layer = LayerTemplate::new("Main", LayerIndex::new(1))
        .with_module(synth_source())
        .with_module(synth_special())
        .with_module(synth_modulation())
        .with_module(synth_time())
        .with_module(synth_master());

    EngineTemplate::new("Synth Engine", InstrumentType::Synth, layer)
        .with_description("Synthesizer pads and leads with ambient processing")
}

fn synth_source() -> ModuleTemplate {
    ModuleTemplate::new("Source", ModuleType::Source)
        .with_description("Synth input conditioning")
        .with_block(
            BlockTemplate::new("Synth Input", BlockType::Input)
                .with_description("Synth instrument source input"),
        )
}

fn synth_special() -> ModuleTemplate {
    ModuleTemplate::new("Special", ModuleType::Special)
        .with_description("Synth filter and special effects")
        .with_block(
            BlockTemplate::new("Synth Filter", BlockType::Filter)
                .with_description("Resonant filter for synth tone sculpting"),
        )
}

fn synth_modulation() -> ModuleTemplate {
    ModuleTemplate::new("Modulation", ModuleType::Modulation)
        .with_description("Movement and width for synth sounds")
        .with_block(
            BlockTemplate::new("Synth Chorus", BlockType::Chorus)
                .with_description("Chorus for pad width and shimmer"),
        )
        .with_block(
            BlockTemplate::new("Synth Phaser", BlockType::Phaser)
                .with_description("Phaser for swirling lead textures"),
        )
}

fn synth_time() -> ModuleTemplate {
    ModuleTemplate::new("Time", ModuleType::Time)
        .with_description("Ambient delay and reverb for synth pads")
        .with_block(
            BlockTemplate::new("Synth Delay", BlockType::Delay)
                .with_description("Long ambient delay for pad textures"),
        )
        .with_block(
            BlockTemplate::new("Synth Reverb", BlockType::Reverb)
                .with_description("Large hall/shimmer reverb for atmospheric pads"),
        )
}

fn synth_master() -> ModuleTemplate {
    ModuleTemplate::new("Master", ModuleType::Master)
        .with_description("Synth output stage")
        .with_block(
            BlockTemplate::new("Synth Limiter", BlockType::Limiter)
                .with_description("Peak protection for synth output"),
        )
        .with_block(
            BlockTemplate::new("Synth Volume", BlockType::Volume)
                .with_alias("Synth-Vol")
                .with_description("Synth output level"),
        )
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rig::Rig;
    use crate::template::Templatable;

    #[test]
    fn synth_rig_has_3_engines() {
        let t = synth_rig_template();
        assert_eq!(t.engines.len(), 3);
        assert_eq!(t.engines.first().name, "Keys Engine");
        assert_eq!(t.engines.iter().nth(1).unwrap().name, "Organ Engine");
        assert_eq!(t.engines.iter().nth(2).unwrap().name, "Synth Engine");
    }

    #[test]
    fn keys_engine_has_5_modules() {
        let t = synth_rig_template();
        let keys = t.engines.first();
        let modules = &keys.layers.first().modules;
        assert_eq!(modules.len(), 5);
        assert_eq!(modules[0].module_type, ModuleType::Source);
        assert_eq!(modules[1].module_type, ModuleType::Dynamics);
        assert_eq!(modules[2].module_type, ModuleType::Eq);
        assert_eq!(modules[3].module_type, ModuleType::Time);
        assert_eq!(modules[4].module_type, ModuleType::Master);
    }

    #[test]
    fn organ_engine_has_rotary() {
        let t = synth_rig_template();
        let organ = t.engines.iter().nth(1).unwrap();
        let modules = &organ.layers.first().modules;
        assert_eq!(modules.len(), 5);
        assert_eq!(modules[3].module_type, ModuleType::Motion);
        assert_eq!(modules[3].blocks[0].name, "Rotary Speaker");
    }

    #[test]
    fn synth_engine_has_filter_and_modulation() {
        let t = synth_rig_template();
        let synth = t.engines.iter().nth(2).unwrap();
        let modules = &synth.layers.first().modules;
        assert_eq!(modules.len(), 5);
        assert_eq!(modules[1].module_type, ModuleType::Special);
        assert_eq!(modules[1].blocks[0].name, "Synth Filter");
        assert_eq!(modules[2].module_type, ModuleType::Modulation);
        assert_eq!(modules[2].blocks.len(), 2); // chorus + phaser
    }

    #[test]
    fn synth_rig_total_blocks() {
        let t = synth_rig_template();
        let total: usize = t
            .engines
            .iter()
            .flat_map(|e| e.layers.iter())
            .flat_map(|l| l.modules.iter())
            .map(|m| m.blocks.len())
            .sum();
        // Keys: 1+1+1+2+2=7, Organ: 1+1+1+1+2=6, Synth: 1+1+2+2+2=8 → 21
        assert_eq!(total, 21);
    }

    #[test]
    fn synth_rig_instantiates_via_templatable() {
        let t = synth_rig_template();
        let rig = Rig::from_template(&t);

        assert_eq!(rig.name, "Synth Rig Template");
        assert_eq!(rig.engine_count(), 3);
        assert_eq!(rig.instrument_type, InstrumentType::Synth);

        // All blocks should be placeholders
        for engine in rig.engines.iter() {
            for layer in engine.layers.iter() {
                for module in &layer.modules {
                    for mb in &module.blocks {
                        assert!(
                            mb.block.is_placeholder(),
                            "Block '{}' in engine '{}' should be a placeholder",
                            mb.block.name,
                            engine.name,
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn each_engine_has_correct_instrument_type() {
        let t = synth_rig_template();
        assert_eq!(t.engines.first().engine_type, InstrumentType::Keys);
        assert_eq!(
            t.engines.iter().nth(1).unwrap().engine_type,
            InstrumentType::Custom("Organ".into())
        );
        assert_eq!(
            t.engines.iter().nth(2).unwrap().engine_type,
            InstrumentType::Synth
        );
    }
}
