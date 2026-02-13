//! Vocal rack template — 3 vocal rigs (Lead, Harmony, Background).
//!
//! Each vocal rig uses the standard 5-module vocal chain but with
//! different processing emphasis:
//! - **Lead**: Full chain — every module active
//! - **Harmony**: Lighter processing — no saturator, gentler compression
//! - **Background**: Minimal chain — no pitch correction, heavier gate, more sends

use crate::block::BlockType;
use crate::module::ModuleType;
use crate::rig::InstrumentType;
use crate::template::{
    BlockTemplate, EngineTemplate, LayerTemplate, ModuleTemplate, RackTemplate, RigTemplate,
};
use crate::version::LayerIndex;

use super::vocal::vocal_rig_template;

// ─────────────────────────────────────────────────────────────────────────────
// Vocal Rack Template
// ─────────────────────────────────────────────────────────────────────────────

/// Vocal rack containing Lead, Harmony, and Background vocal rig templates.
pub fn vocal_rack_template() -> RackTemplate {
    RackTemplate {
        id: crate::id::RackTemplateId::new(),
        name: "Vocal Rack".into(),
        rigs: vec![
            lead_vocal_template(),
            harmony_vocal_template(),
            background_vocal_template(),
        ],
        description: Some(
            "3 vocal rigs: Lead (full chain), Harmony (lighter), Background (minimal)".into(),
        ),
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Lead Vocal — full chain, everything active
// ─────────────────────────────────────────────────────────────────────────────

/// Lead vocal rig — full vocal chain, every module populated.
///
/// Uses the standard vocal template as-is — this is the reference
/// configuration for a lead vocalist.
pub fn lead_vocal_template() -> RigTemplate {
    let mut t = vocal_rig_template();
    t.name = "Lead Vocal".into();
    t.description =
        Some("Full vocal chain — de-esser, gate, correction, tonal shaping, sends".into());
    t
}

// ─────────────────────────────────────────────────────────────────────────────
// Harmony Vocal — lighter processing
// ─────────────────────────────────────────────────────────────────────────────

/// Harmony vocal rig — lighter processing for backing harmonies.
///
/// Differences from Lead:
/// - Tonal module has no Saturator (2 blocks instead of 3)
/// - Lighter descriptions reflecting gentler settings
pub fn harmony_vocal_template() -> RigTemplate {
    let layer = LayerTemplate::new("Main", LayerIndex::new(1))
        .with_module(harmony_rescue())
        .with_module(harmony_correction())
        .with_module(harmony_tonal())
        .with_module(harmony_modulation())
        .with_module(harmony_sends());

    let engine = EngineTemplate::new("Vocal Engine", InstrumentType::Vocals, layer);

    RigTemplate::new("Harmony Vocal", InstrumentType::Vocals, engine)
        .with_description("Lighter vocal chain for harmony parts — no saturation, gentler dynamics")
}

fn harmony_rescue() -> ModuleTemplate {
    ModuleTemplate::new("Rescue", ModuleType::Rescue)
        .with_description("Cleanup for harmony vocal")
        .with_block(
            BlockTemplate::new("De-Esser", BlockType::DeEsser)
                .with_description("Sibilance reduction"),
        )
        .with_block(
            BlockTemplate::new("Gate", BlockType::Gate)
                .with_alias("Renegate")
                .with_description("Noise gate for bleed reduction"),
        )
        .with_block(
            BlockTemplate::new("Rescue EQ", BlockType::Eq)
                .with_alias("Rescue-EQ")
                .with_description("Subtractive EQ for problem frequencies"),
        )
        .with_block(
            BlockTemplate::new("Control Compressor", BlockType::Compressor)
                .with_alias("Ctrl-Comp")
                .with_description("Gentle leveling compression"),
        )
}

fn harmony_correction() -> ModuleTemplate {
    ModuleTemplate::new("Correction", ModuleType::Correction)
        .with_description("Pitch correction for harmony accuracy")
        .with_block(
            BlockTemplate::new("Tuner", BlockType::Tuner)
                .with_description("Pitch correction — tighter for harmonies"),
        )
}

fn harmony_tonal() -> ModuleTemplate {
    // No saturator — keep harmonies clean and transparent
    ModuleTemplate::new("Tonal", ModuleType::Tonal)
        .with_description("Gentle tonal shaping — no saturation for transparency")
        .with_block(
            BlockTemplate::new("Style Compressor", BlockType::Compressor)
                .with_alias("Style-Comp")
                .with_description("Gentle character compression"),
        )
        .with_block(
            BlockTemplate::new("Tonal EQ", BlockType::Eq)
                .with_alias("Tone-EQ")
                .with_description("Carve space for harmony to sit under lead"),
        )
}

fn harmony_modulation() -> ModuleTemplate {
    ModuleTemplate::new("Modulation", ModuleType::VocalModulation)
        .with_description("Subtle modulation for harmony width")
        .with_block(
            BlockTemplate::new("Vocal Chorus", BlockType::Chorus)
                .with_description("Gentle chorus for width"),
        )
}

fn harmony_sends() -> ModuleTemplate {
    ModuleTemplate::new("Sends", ModuleType::Sends)
        .with_description("Send effects — shared reverb and delay buses")
        .with_block(
            BlockTemplate::new("Verb Send", BlockType::Send).with_description("Send to reverb bus"),
        )
        .with_block(
            BlockTemplate::new("Delay Send", BlockType::Send).with_description("Send to delay bus"),
        )
        .with_block(
            BlockTemplate::new("Special Send", BlockType::Send)
                .with_description("Send to special effects bus"),
        )
}

// ─────────────────────────────────────────────────────────────────────────────
// Background Vocal — minimal chain
// ─────────────────────────────────────────────────────────────────────────────

/// Background vocal rig — minimal processing, heavier gate, more sends.
///
/// Differences from Lead:
/// - No Correction module (no pitch correction)
/// - Tonal has only compressor + EQ (no saturator)
/// - No modulation module
/// - Heavier gate description (more aggressive noise reduction)
pub fn background_vocal_template() -> RigTemplate {
    let layer = LayerTemplate::new("Main", LayerIndex::new(1))
        .with_module(bg_rescue())
        .with_module(bg_tonal())
        .with_module(bg_sends());

    let engine = EngineTemplate::new("Vocal Engine", InstrumentType::Vocals, layer);

    RigTemplate::new("Background Vocal", InstrumentType::Vocals, engine)
        .with_description("Minimal vocal chain for background parts — no correction, no modulation")
}

fn bg_rescue() -> ModuleTemplate {
    ModuleTemplate::new("Rescue", ModuleType::Rescue)
        .with_description("Aggressive cleanup for background vocals")
        .with_block(
            BlockTemplate::new("De-Esser", BlockType::DeEsser)
                .with_description("Heavy de-essing for background clarity"),
        )
        .with_block(
            BlockTemplate::new("Gate", BlockType::Gate)
                .with_alias("Renegate")
                .with_description("Aggressive gate — tight threshold for clean gaps"),
        )
        .with_block(
            BlockTemplate::new("Rescue EQ", BlockType::Eq)
                .with_alias("Rescue-EQ")
                .with_description("Cut lows and harsh mids to sit behind lead"),
        )
        .with_block(
            BlockTemplate::new("Control Compressor", BlockType::Compressor)
                .with_alias("Ctrl-Comp")
                .with_description("Firm compression for consistent background level"),
        )
}

fn bg_tonal() -> ModuleTemplate {
    ModuleTemplate::new("Tonal", ModuleType::Tonal)
        .with_description("Simple tonal shaping for background blend")
        .with_block(
            BlockTemplate::new("Style Compressor", BlockType::Compressor)
                .with_alias("Style-Comp")
                .with_description("Glue compression for blend"),
        )
        .with_block(
            BlockTemplate::new("Tonal EQ", BlockType::Eq)
                .with_alias("Tone-EQ")
                .with_description("Narrow presence for intelligibility without competing"),
        )
}

fn bg_sends() -> ModuleTemplate {
    ModuleTemplate::new("Sends", ModuleType::Sends)
        .with_description("Heavier sends for background depth and space")
        .with_block(
            BlockTemplate::new("Verb Send", BlockType::Send)
                .with_description("Heavy reverb send for depth and space"),
        )
        .with_block(
            BlockTemplate::new("Delay Send", BlockType::Send)
                .with_description("Delay send for rhythmic interest"),
        )
        .with_block(
            BlockTemplate::new("Special Send", BlockType::Send)
                .with_description("Special effects send"),
        )
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rack::Rack;
    use crate::template::Templatable;

    #[test]
    fn vocal_rack_has_3_rigs() {
        let t = vocal_rack_template();
        assert_eq!(t.rigs.len(), 3);
        assert_eq!(t.rigs[0].name, "Lead Vocal");
        assert_eq!(t.rigs[1].name, "Harmony Vocal");
        assert_eq!(t.rigs[2].name, "Background Vocal");
    }

    #[test]
    fn lead_vocal_has_5_modules() {
        let t = lead_vocal_template();
        assert_eq!(t.modules().len(), 5);
    }

    #[test]
    fn lead_vocal_has_13_blocks() {
        let t = lead_vocal_template();
        let total: usize = t.modules().iter().map(|m| m.blocks.len()).sum();
        assert_eq!(total, 13);
    }

    #[test]
    fn harmony_vocal_has_no_saturator() {
        let t = harmony_vocal_template();
        let tonal = t
            .modules()
            .iter()
            .find(|m| m.module_type == ModuleType::Tonal)
            .unwrap();
        assert_eq!(tonal.blocks.len(), 2, "Harmony tonal: compressor + EQ only");
        assert!(
            !tonal
                .blocks
                .iter()
                .any(|b| b.block_type == BlockType::Saturator),
            "Harmony should not have saturator"
        );
    }

    #[test]
    fn harmony_vocal_has_no_flanger() {
        let t = harmony_vocal_template();
        let modulation = t
            .modules()
            .iter()
            .find(|m| m.module_type == ModuleType::VocalModulation)
            .unwrap();
        assert_eq!(
            modulation.blocks.len(),
            1,
            "Harmony modulation: chorus only"
        );
    }

    #[test]
    fn background_vocal_has_no_correction() {
        let t = background_vocal_template();
        assert!(
            !t.modules()
                .iter()
                .any(|m| m.module_type == ModuleType::Correction),
            "Background should not have correction module"
        );
    }

    #[test]
    fn background_vocal_has_3_modules() {
        let t = background_vocal_template();
        assert_eq!(t.modules().len(), 3, "Background: rescue, tonal, sends");
    }

    #[test]
    fn background_vocal_has_no_modulation() {
        let t = background_vocal_template();
        assert!(
            !t.modules()
                .iter()
                .any(|m| m.module_type == ModuleType::VocalModulation),
            "Background should not have modulation module"
        );
    }

    #[test]
    fn vocal_rack_instantiates() {
        let t = vocal_rack_template();
        let rack = Rack::from_template(&t);
        assert_eq!(rack.name, "Vocal Rack");
        assert_eq!(rack.rig_count(), 3);
        assert_eq!(rack.rigs[0].name, "Lead Vocal");
        assert_eq!(rack.rigs[1].name, "Harmony Vocal");
        assert_eq!(rack.rigs[2].name, "Background Vocal");
    }

    #[test]
    fn all_vocal_rigs_are_vocal_type() {
        let t = vocal_rack_template();
        for rig in &t.rigs {
            assert_eq!(rig.instrument_type, InstrumentType::Vocals);
        }
    }
}
