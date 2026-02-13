//! Keys rig template — 4 engines (Keys, Synth, Organ, Pad) with multi-layer support.
//!
//! Each engine has multiple layers for different sounds/splits and
//! engine-level global FX that apply to the combined layer output:
//!
//! - **Keys Engine** (2 layers): Piano + Electric Piano → Master EQ + Limiter
//! - **Synth Engine** (3 layers): Pads + Leads + Bass → Master Compressor + Limiter
//! - **Organ Engine** (2 layers): B3 + Liturgical → Master EQ + Volume
//! - **Pad Engine** (2 layers): Ambient + Texture → Master Reverb + Volume

use crate::block::BlockType;
use crate::module::ModuleType;
use crate::rig::InstrumentType;
use crate::template::{BlockTemplate, EngineTemplate, LayerTemplate, ModuleTemplate, RigTemplate};
use crate::version::LayerIndex;

// ─────────────────────────────────────────────────────────────────────────────
// Keys Rig Template (4 engines, parallel)
// ─────────────────────────────────────────────────────────────────────────────

/// Keys rig template with four engines in parallel.
///
/// Each engine has multiple layers and engine-level global FX.
/// The engines run in parallel — their outputs are mixed together.
pub fn keys_rig_template() -> RigTemplate {
    let keys = keys_engine_template();
    let synth = synth_engine_template();
    let organ = organ_engine_template();
    let pad = pad_engine_template();

    let mut template = RigTemplate::new("Keys Rig", InstrumentType::Keys, keys);
    template.engines.push(synth);
    template.engines.push(organ);
    template.engines.push(pad);
    template.description = Some(
        "Multi-engine keys rig: Keys (piano/EP), Synth (pads/leads/bass), \
         Organ (B3/liturgical), Pad (ambient/texture)"
            .into(),
    );
    template
}

// ─────────────────────────────────────────────────────────────────────────────
// Keys Engine — Piano / Electric Piano (2 layers)
// ─────────────────────────────────────────────────────────────────────────────

fn keys_engine_template() -> EngineTemplate {
    let piano = LayerTemplate::new("Piano", LayerIndex::new(1))
        .with_module(
            ModuleTemplate::new("Source", ModuleType::Source)
                .with_description("Piano input")
                .with_block(BlockTemplate::new("Piano Input", BlockType::Input)),
        )
        .with_module(
            ModuleTemplate::new("Dynamics", ModuleType::Dynamics)
                .with_description("Piano compression")
                .with_block(BlockTemplate::new(
                    "Piano Compressor",
                    BlockType::Compressor,
                )),
        )
        .with_module(
            ModuleTemplate::new("EQ", ModuleType::Eq)
                .with_description("Piano tone shaping")
                .with_block(BlockTemplate::new("Piano EQ", BlockType::Eq)),
        )
        .with_module(
            ModuleTemplate::new("Master", ModuleType::Master)
                .with_block(BlockTemplate::new("Piano Volume", BlockType::Volume)),
        );

    let ep = LayerTemplate::new("Electric Piano", LayerIndex::new(2))
        .with_module(
            ModuleTemplate::new("Source", ModuleType::Source)
                .with_description("EP input")
                .with_block(BlockTemplate::new("EP Input", BlockType::Input)),
        )
        .with_module(
            ModuleTemplate::new("Drive", ModuleType::Drive)
                .with_description("EP warmth and saturation")
                .with_block(BlockTemplate::new("EP Drive", BlockType::Drive)),
        )
        .with_module(
            ModuleTemplate::new("Modulation", ModuleType::Modulation)
                .with_description("EP chorus/tremolo")
                .with_block(BlockTemplate::new("EP Chorus", BlockType::Chorus))
                .with_block(BlockTemplate::new("EP Tremolo", BlockType::Tremolo)),
        )
        .with_module(
            ModuleTemplate::new("Master", ModuleType::Master)
                .with_block(BlockTemplate::new("EP Volume", BlockType::Volume)),
        );

    let mut et = EngineTemplate::new("Keys Engine", InstrumentType::Keys, piano);
    et.layers.push(ep);
    et.with_description("Piano and electric piano — 2 layers")
        .with_engine_module(
            ModuleTemplate::new("Master EQ", ModuleType::Eq)
                .with_description("Engine-level EQ on combined keys output")
                .with_block(BlockTemplate::new("Keys Master EQ", BlockType::Eq)),
        )
        .with_engine_module(
            ModuleTemplate::new("Master Limiter", ModuleType::Master)
                .with_description("Engine output protection")
                .with_block(BlockTemplate::new("Keys Limiter", BlockType::Limiter)),
        )
}

// ─────────────────────────────────────────────────────────────────────────────
// Synth Engine — Pads / Leads / Bass (3 layers)
// ─────────────────────────────────────────────────────────────────────────────

fn synth_engine_template() -> EngineTemplate {
    let pads = LayerTemplate::new("Pads", LayerIndex::new(1))
        .with_module(
            ModuleTemplate::new("Source", ModuleType::Source)
                .with_block(BlockTemplate::new("Pad Input", BlockType::Input)),
        )
        .with_module(
            ModuleTemplate::new("Special", ModuleType::Special)
                .with_description("Pad filter")
                .with_block(BlockTemplate::new("Pad Filter", BlockType::Filter)),
        )
        .with_module(
            ModuleTemplate::new("Modulation", ModuleType::Modulation)
                .with_block(BlockTemplate::new("Pad Chorus", BlockType::Chorus)),
        )
        .with_module(
            ModuleTemplate::new("Time", ModuleType::Time)
                .with_block(BlockTemplate::new("Pad Delay", BlockType::Delay))
                .with_block(BlockTemplate::new("Pad Reverb", BlockType::Reverb)),
        )
        .with_module(
            ModuleTemplate::new("Master", ModuleType::Master)
                .with_block(BlockTemplate::new("Pad Volume", BlockType::Volume)),
        );

    let leads = LayerTemplate::new("Leads", LayerIndex::new(2))
        .with_module(
            ModuleTemplate::new("Source", ModuleType::Source)
                .with_block(BlockTemplate::new("Lead Input", BlockType::Input)),
        )
        .with_module(
            ModuleTemplate::new("Drive", ModuleType::Drive)
                .with_description("Lead grit")
                .with_block(BlockTemplate::new("Lead Drive", BlockType::Drive)),
        )
        .with_module(
            ModuleTemplate::new("Special", ModuleType::Special)
                .with_block(BlockTemplate::new("Lead Filter", BlockType::Filter)),
        )
        .with_module(
            ModuleTemplate::new("Time", ModuleType::Time)
                .with_block(BlockTemplate::new("Lead Delay", BlockType::Delay)),
        )
        .with_module(
            ModuleTemplate::new("Master", ModuleType::Master)
                .with_block(BlockTemplate::new("Lead Volume", BlockType::Volume)),
        );

    let bass = LayerTemplate::new("Bass", LayerIndex::new(3))
        .with_module(
            ModuleTemplate::new("Source", ModuleType::Source)
                .with_block(BlockTemplate::new("Bass Input", BlockType::Input)),
        )
        .with_module(
            ModuleTemplate::new("Dynamics", ModuleType::Dynamics)
                .with_block(BlockTemplate::new("Bass Compressor", BlockType::Compressor)),
        )
        .with_module(
            ModuleTemplate::new("EQ", ModuleType::Eq)
                .with_block(BlockTemplate::new("Bass EQ", BlockType::Eq)),
        )
        .with_module(
            ModuleTemplate::new("Master", ModuleType::Master)
                .with_block(BlockTemplate::new("Bass Volume", BlockType::Volume)),
        );

    let mut et = EngineTemplate::new("Synth Engine", InstrumentType::Synth, pads);
    et.layers.push(leads);
    et.layers.push(bass);
    et.with_description("Pads, leads, and bass synth — 3 layers")
        .with_engine_module(
            ModuleTemplate::new("Master Dynamics", ModuleType::Dynamics)
                .with_description("Engine-level compression on combined synth output")
                .with_block(BlockTemplate::new(
                    "Synth Master Comp",
                    BlockType::Compressor,
                )),
        )
        .with_engine_module(
            ModuleTemplate::new("Master Limiter", ModuleType::Master)
                .with_block(BlockTemplate::new("Synth Limiter", BlockType::Limiter)),
        )
}

// ─────────────────────────────────────────────────────────────────────────────
// Organ Engine — B3 / Liturgical (2 layers)
// ─────────────────────────────────────────────────────────────────────────────

fn organ_engine_template() -> EngineTemplate {
    let b3 = LayerTemplate::new("B3", LayerIndex::new(1))
        .with_module(
            ModuleTemplate::new("Source", ModuleType::Source)
                .with_block(BlockTemplate::new("B3 Input", BlockType::Input)),
        )
        .with_module(
            ModuleTemplate::new("Drive", ModuleType::Drive)
                .with_description("B3 tube overdrive")
                .with_block(BlockTemplate::new("B3 Overdrive", BlockType::Drive)),
        )
        .with_module(
            ModuleTemplate::new("Motion", ModuleType::Motion)
                .with_description("Leslie rotary speaker")
                .with_block(BlockTemplate::new("Rotary Speaker", BlockType::Rotary)),
        )
        .with_module(
            ModuleTemplate::new("Master", ModuleType::Master)
                .with_block(BlockTemplate::new("B3 Volume", BlockType::Volume)),
        );

    let liturgical = LayerTemplate::new("Liturgical", LayerIndex::new(2))
        .with_module(
            ModuleTemplate::new("Source", ModuleType::Source)
                .with_block(BlockTemplate::new("Pipe Input", BlockType::Input)),
        )
        .with_module(
            ModuleTemplate::new("EQ", ModuleType::Eq)
                .with_description("Pipe organ tone shaping")
                .with_block(BlockTemplate::new("Pipe EQ", BlockType::Eq)),
        )
        .with_module(
            ModuleTemplate::new("Time", ModuleType::Time)
                .with_description("Cathedral reverb")
                .with_block(BlockTemplate::new("Cathedral Reverb", BlockType::Reverb)),
        )
        .with_module(
            ModuleTemplate::new("Master", ModuleType::Master)
                .with_block(BlockTemplate::new("Pipe Volume", BlockType::Volume)),
        );

    let mut et = EngineTemplate::new("Organ Engine", InstrumentType::Custom("Organ".into()), b3);
    et.layers.push(liturgical);
    et.with_description("B3 and liturgical organ — 2 layers")
        .with_engine_module(
            ModuleTemplate::new("Master EQ", ModuleType::Eq)
                .with_description("Engine-level EQ on combined organ output")
                .with_block(BlockTemplate::new("Organ Master EQ", BlockType::Eq)),
        )
        .with_engine_module(
            ModuleTemplate::new("Master Volume", ModuleType::Master).with_block(
                BlockTemplate::new("Organ Volume", BlockType::Volume).with_alias("Organ-Vol"),
            ),
        )
}

// ─────────────────────────────────────────────────────────────────────────────
// Pad Engine — Ambient / Texture (2 layers)
// ─────────────────────────────────────────────────────────────────────────────

fn pad_engine_template() -> EngineTemplate {
    let ambient = LayerTemplate::new("Ambient", LayerIndex::new(1))
        .with_module(
            ModuleTemplate::new("Source", ModuleType::Source)
                .with_block(BlockTemplate::new("Ambient Input", BlockType::Input)),
        )
        .with_module(
            ModuleTemplate::new("Modulation", ModuleType::Modulation)
                .with_block(BlockTemplate::new("Ambient Chorus", BlockType::Chorus)),
        )
        .with_module(
            ModuleTemplate::new("Time", ModuleType::Time)
                .with_block(BlockTemplate::new("Ambient Delay", BlockType::Delay))
                .with_block(BlockTemplate::new("Ambient Reverb", BlockType::Reverb)),
        )
        .with_module(
            ModuleTemplate::new("Master", ModuleType::Master)
                .with_block(BlockTemplate::new("Ambient Volume", BlockType::Volume)),
        );

    let texture = LayerTemplate::new("Texture", LayerIndex::new(2))
        .with_module(
            ModuleTemplate::new("Source", ModuleType::Source)
                .with_block(BlockTemplate::new("Texture Input", BlockType::Input)),
        )
        .with_module(
            ModuleTemplate::new("Special", ModuleType::Special)
                .with_description("Granular/shimmer effects")
                .with_block(BlockTemplate::new("Texture Filter", BlockType::Filter))
                .with_block(BlockTemplate::new("Texture Freeze", BlockType::Freeze)),
        )
        .with_module(
            ModuleTemplate::new("Time", ModuleType::Time)
                .with_block(BlockTemplate::new("Texture Reverb", BlockType::Reverb)),
        )
        .with_module(
            ModuleTemplate::new("Master", ModuleType::Master)
                .with_block(BlockTemplate::new("Texture Volume", BlockType::Volume)),
        );

    let mut et = EngineTemplate::new("Pad Engine", InstrumentType::Custom("Pad".into()), ambient);
    et.layers.push(texture);
    et.with_description("Ambient pads and textures — 2 layers")
        .with_engine_module(
            ModuleTemplate::new("Master Reverb", ModuleType::Time)
                .with_description("Engine-level shimmer reverb on combined pad output")
                .with_block(BlockTemplate::new("Pad Master Reverb", BlockType::Reverb)),
        )
        .with_engine_module(
            ModuleTemplate::new("Master Volume", ModuleType::Master).with_block(
                BlockTemplate::new("Pad Volume", BlockType::Volume).with_alias("Pad-Vol"),
            ),
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
    fn keys_rig_has_4_engines() {
        let t = keys_rig_template();
        assert_eq!(t.engines.len(), 4);
        assert_eq!(t.engines.first().name, "Keys Engine");
        assert_eq!(t.engines.iter().nth(1).unwrap().name, "Synth Engine");
        assert_eq!(t.engines.iter().nth(2).unwrap().name, "Organ Engine");
        assert_eq!(t.engines.iter().nth(3).unwrap().name, "Pad Engine");
    }

    #[test]
    fn keys_engine_has_2_layers() {
        let t = keys_rig_template();
        let keys = t.engines.first();
        assert_eq!(keys.layers.len(), 2);
        assert_eq!(keys.layers.first().name, "Piano");
        assert_eq!(keys.layers.iter().nth(1).unwrap().name, "Electric Piano");
    }

    #[test]
    fn synth_engine_has_3_layers() {
        let t = keys_rig_template();
        let synth = t.engines.iter().nth(1).unwrap();
        assert_eq!(synth.layers.len(), 3);
        assert_eq!(synth.layers.first().name, "Pads");
        assert_eq!(synth.layers.iter().nth(1).unwrap().name, "Leads");
        assert_eq!(synth.layers.iter().nth(2).unwrap().name, "Bass");
    }

    #[test]
    fn organ_engine_has_2_layers() {
        let t = keys_rig_template();
        let organ = t.engines.iter().nth(2).unwrap();
        assert_eq!(organ.layers.len(), 2);
        assert_eq!(organ.layers.first().name, "B3");
        assert_eq!(organ.layers.iter().nth(1).unwrap().name, "Liturgical");
    }

    #[test]
    fn pad_engine_has_2_layers() {
        let t = keys_rig_template();
        let pad = t.engines.iter().nth(3).unwrap();
        assert_eq!(pad.layers.len(), 2);
        assert_eq!(pad.layers.first().name, "Ambient");
        assert_eq!(pad.layers.iter().nth(1).unwrap().name, "Texture");
    }

    #[test]
    fn engines_have_global_fx() {
        let t = keys_rig_template();
        // Keys Engine: Master EQ + Master Limiter
        assert_eq!(t.engines.first().modules.len(), 2);
        // Synth Engine: Master Dynamics + Master Limiter
        assert_eq!(t.engines.iter().nth(1).unwrap().modules.len(), 2);
        // Organ Engine: Master EQ + Master Volume
        assert_eq!(t.engines.iter().nth(2).unwrap().modules.len(), 2);
        // Pad Engine: Master Reverb + Master Volume
        assert_eq!(t.engines.iter().nth(3).unwrap().modules.len(), 2);
    }

    #[test]
    fn keys_rig_instantiates_via_templatable() {
        let t = keys_rig_template();
        let rig = Rig::from_template(&t);

        assert_eq!(rig.name, "Keys Rig");
        assert_eq!(rig.engine_count(), 4);
        assert_eq!(rig.instrument_type, InstrumentType::Keys);

        // All layer blocks should be placeholders
        for engine in rig.engines.iter() {
            for layer in engine.layers.iter() {
                for module in &layer.modules {
                    for mb in &module.blocks {
                        assert!(
                            mb.block.is_placeholder(),
                            "Block '{}' in engine '{}' layer '{}' should be placeholder",
                            mb.block.name,
                            engine.name,
                            layer.name,
                        );
                    }
                }
            }
            // Engine-level modules should also be placeholders
            for module in &engine.modules {
                for mb in &module.blocks {
                    assert!(
                        mb.block.is_placeholder(),
                        "Engine-level block '{}' in engine '{}' should be placeholder",
                        mb.block.name,
                        engine.name,
                    );
                }
            }
        }
    }

    #[test]
    fn keys_rig_total_blocks() {
        let t = keys_rig_template();
        let layer_blocks: usize = t
            .engines
            .iter()
            .flat_map(|e| e.layers.iter())
            .flat_map(|l| l.modules.iter())
            .map(|m| m.blocks.len())
            .sum();
        let engine_blocks: usize = t
            .engines
            .iter()
            .flat_map(|e| e.modules.iter())
            .map(|m| m.blocks.len())
            .sum();
        // Keys: Piano(4) + EP(5) = 9 layer blocks + 2 engine blocks
        // Synth: Pads(6) + Leads(5) + Bass(4) = 15 layer blocks + 2 engine blocks
        // Organ: B3(4) + Liturgical(4) = 8 layer blocks + 2 engine blocks
        // Pad: Ambient(5) + Texture(5) = 10 layer blocks + 2 engine blocks
        // Total layer: 9 + 15 + 8 + 10 = 42
        // Total engine: 2 + 2 + 2 + 2 = 8
        assert_eq!(layer_blocks, 42);
        assert_eq!(engine_blocks, 8);
    }

    #[test]
    fn each_engine_has_correct_type() {
        let t = keys_rig_template();
        assert_eq!(t.engines.first().engine_type, InstrumentType::Keys);
        assert_eq!(
            t.engines.iter().nth(1).unwrap().engine_type,
            InstrumentType::Synth
        );
        assert_eq!(
            t.engines.iter().nth(2).unwrap().engine_type,
            InstrumentType::Custom("Organ".into())
        );
        assert_eq!(
            t.engines.iter().nth(3).unwrap().engine_type,
            InstrumentType::Custom("Pad".into())
        );
    }
}
