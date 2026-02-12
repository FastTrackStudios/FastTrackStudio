//! Vocal rig template — 5-module signal chain with 13 block slots.
//!
//! Signal chain order: Rescue → Correction → Tonal → Modulation → Sends

use crate::block::BlockType;
use crate::module::ModuleType;
use crate::rig::InstrumentType;
use crate::template::{BlockTemplate, EngineTemplate, LayerTemplate, ModuleTemplate, RigTemplate};
use crate::version::LayerIndex;

/// Standard vocal rig template with 5 modules in a single engine/layer.
pub fn vocal_rig_template() -> RigTemplate {
    let layer = LayerTemplate::new("Main", LayerIndex::new(1))
        .with_module(vocal_rescue())
        .with_module(vocal_correction())
        .with_module(vocal_tonal())
        .with_module(vocal_modulation())
        .with_module(vocal_sends());

    let engine = EngineTemplate::new("Vocal Engine", InstrumentType::Vocals, layer);

    RigTemplate::new("Vocal Rig Template", InstrumentType::Vocals, engine)
        .with_description("Standard vocal signal chain with 5 processing stages")
}

fn vocal_rescue() -> ModuleTemplate {
    ModuleTemplate::new("Rescue", ModuleType::Rescue)
        .with_description("Emergency cleanup — de-ess, gate, rescue EQ, control compression")
        .with_block(
            BlockTemplate::new("De-Esser", BlockType::DeEsser)
                .with_description("Sibilance reduction — tame harsh S and T sounds"),
        )
        .with_block(
            BlockTemplate::new("Gate", BlockType::Gate)
                .with_alias("Renegate")
                .with_description("Noise gate for room noise and bleed reduction"),
        )
        .with_block(
            BlockTemplate::new("Rescue EQ", BlockType::Eq)
                .with_alias("Rescue-EQ")
                .with_description("Subtractive EQ for fixing problem frequencies"),
        )
        .with_block(
            BlockTemplate::new("Control Compressor", BlockType::Compressor)
                .with_alias("Ctrl-Comp")
                .with_description("Gentle compression to even out dynamic range"),
        )
}

fn vocal_correction() -> ModuleTemplate {
    ModuleTemplate::new("Correction", ModuleType::Correction)
        .with_description("Pitch correction and tuning")
        .with_block(
            BlockTemplate::new("Tuner", BlockType::Tuner)
                .with_description("Real-time pitch correction and tuning"),
        )
}

fn vocal_tonal() -> ModuleTemplate {
    ModuleTemplate::new("Tonal", ModuleType::Tonal)
        .with_description("Tonal shaping — style compression, EQ, saturation")
        .with_block(
            BlockTemplate::new("Style Compressor", BlockType::Compressor)
                .with_alias("Style-Comp")
                .with_description("Character compression for vocal tone and glue"),
        )
        .with_block(
            BlockTemplate::new("Tonal EQ", BlockType::Eq)
                .with_alias("Tone-EQ")
                .with_description("Additive EQ for presence, air, and warmth"),
        )
        .with_block(
            BlockTemplate::new("Saturator", BlockType::Saturator)
                .with_description("Harmonic saturation for warmth and grit"),
        )
}

fn vocal_modulation() -> ModuleTemplate {
    ModuleTemplate::new("Modulation", ModuleType::VocalModulation)
        .with_description("Vocal modulation effects — chorus, flanger")
        .with_block(
            BlockTemplate::new("Vocal Chorus", BlockType::Chorus)
                .with_description("Subtle chorus for vocal width and shimmer"),
        )
        .with_block(
            BlockTemplate::new("Vocal Flanger", BlockType::Flanger)
                .with_description("Flanger for creative vocal effects"),
        )
}

fn vocal_sends() -> ModuleTemplate {
    ModuleTemplate::new("Sends", ModuleType::Sends)
        .with_description("Send effects — reverb, delay, special")
        .with_block(
            BlockTemplate::new("Verb Send", BlockType::Send)
                .with_description("Send to reverb bus for space and depth"),
        )
        .with_block(
            BlockTemplate::new("Delay Send", BlockType::Send)
                .with_description("Send to delay bus for echo and repeat"),
        )
        .with_block(
            BlockTemplate::new("Special Send", BlockType::Send)
                .with_description("Send to special effects bus"),
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
    fn vocal_template_has_5_modules() {
        let t = vocal_rig_template();
        let modules = t.modules();
        assert_eq!(modules.len(), 5);
        assert_eq!(modules[0].module_type, ModuleType::Rescue);
        assert_eq!(modules[1].module_type, ModuleType::Correction);
        assert_eq!(modules[2].module_type, ModuleType::Tonal);
        assert_eq!(modules[3].module_type, ModuleType::VocalModulation);
        assert_eq!(modules[4].module_type, ModuleType::Sends);
    }

    #[test]
    fn vocal_template_block_counts() {
        let t = vocal_rig_template();
        let m = t.modules();
        assert_eq!(m[0].blocks.len(), 4, "Rescue: de-esser, gate, eq, comp");
        assert_eq!(m[1].blocks.len(), 1, "Correction: tuner");
        assert_eq!(m[2].blocks.len(), 3, "Tonal: comp, eq, saturator");
        assert_eq!(m[3].blocks.len(), 2, "Modulation: chorus, flanger");
        assert_eq!(m[4].blocks.len(), 3, "Sends: verb, delay, special");
    }

    #[test]
    fn vocal_template_total_blocks() {
        let t = vocal_rig_template();
        let total: usize = t.modules().iter().map(|m| m.blocks.len()).sum();
        assert_eq!(total, 13);
    }

    #[test]
    fn vocal_template_instantiates_via_templatable() {
        let t = vocal_rig_template();
        let rig = Rig::from_template(&t);

        assert_eq!(rig.name, "Vocal Rig Template");
        let layer = rig.engines.first().layers.first();
        assert_eq!(layer.modules.len(), 5);

        for module in &layer.modules {
            for mb in &module.blocks {
                assert!(
                    mb.block.is_placeholder(),
                    "Block '{}' should be a placeholder",
                    mb.block.name
                );
            }
        }
    }

    #[test]
    fn vocal_rescue_has_aliases() {
        let t = vocal_rig_template();
        let rescue = &t.modules()[0];
        assert_eq!(rescue.blocks[1].alias.as_deref(), Some("Renegate"));
        assert_eq!(rescue.blocks[2].alias.as_deref(), Some("Rescue-EQ"));
        assert_eq!(rescue.blocks[3].alias.as_deref(), Some("Ctrl-Comp"));
    }
}
