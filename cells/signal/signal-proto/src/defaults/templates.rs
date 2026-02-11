//! Default rig templates — structural blueprints for guitar and vocal rigs.
//!
//! These templates define which modules and block slots exist in a rig
//! without binding to specific plugins. Instantiate with
//! [`RigTemplate::instantiate()`] to get a preset with placeholder blocks.

use crate::block::BlockType;
use crate::module::ModuleType;
use crate::template::{BlockPlaceholder, ModuleTemplate, RigTemplate};

// ─────────────────────────────────────────────────────────────────────────────
// Guitar Rig Template
// ─────────────────────────────────────────────────────────────────────────────

/// Standard guitar rig template with 11 modules.
///
/// Signal chain order:
/// Source → Dynamics → Special → Drive → Volume Pedal → Pre-FX →
/// Amp/Cab → Modulation → Time → Motion → Mastering
pub fn guitar_rig_template() -> RigTemplate {
    RigTemplate::new("Guitar Rig Template")
        .with_description("Standard guitar signal chain with 11 processing stages")
        .with_module(guitar_source())
        .with_module(guitar_dynamics())
        .with_module(guitar_special())
        .with_module(guitar_drive())
        .with_module(guitar_volume())
        .with_module(guitar_prefx())
        .with_module(guitar_amp_cab())
        .with_module(guitar_modulation())
        .with_module(guitar_time())
        .with_module(guitar_motion())
        .with_module(guitar_mastering())
}

fn guitar_source() -> ModuleTemplate {
    ModuleTemplate::new("Source", ModuleType::Source)
        .with_description("Input conditioning — gate and initial volume")
        .with_block(
            BlockPlaceholder::new("Input Gate", BlockType::Gate)
                .with_description("Noise gate to clean up input signal"),
        )
        .with_block(
            BlockPlaceholder::new("Input Volume", BlockType::Volume)
                .with_description("Input level trim before processing"),
        )
}

fn guitar_dynamics() -> ModuleTemplate {
    ModuleTemplate::new("Dynamics", ModuleType::Dynamics)
        .with_description("Compression and dynamic control")
        .with_block(
            BlockPlaceholder::new("Compressor", BlockType::Compressor)
                .with_description("Main compressor for dynamic control"),
        )
}

fn guitar_special() -> ModuleTemplate {
    ModuleTemplate::new("Special", ModuleType::Special)
        .with_description("Special effects — wah, filter, pitch, doubler")
        .with_block(
            BlockPlaceholder::new("Envelope Filter", BlockType::Filter)
                .with_description("Auto-wah / envelope filter effect"),
        )
        .with_block(
            BlockPlaceholder::new("Wah Pedal", BlockType::Wah)
                .with_description("Expression-controlled wah"),
        )
        .with_block(
            BlockPlaceholder::new("Pitch Octave FX", BlockType::Pitch)
                .with_description("Pitch shifting and octave effects"),
        )
        .with_block(
            BlockPlaceholder::new("Doubler", BlockType::Doubler)
                .with_description("Signal doubling for width and thickness"),
        )
}

fn guitar_drive() -> ModuleTemplate {
    ModuleTemplate::new("Drive", ModuleType::Drive)
        .with_description("Overdrive and distortion — boost, drive 1, drive 2")
        .with_block(
            BlockPlaceholder::new("Boost", BlockType::Boost)
                .with_description("Clean boost for pushing the amp or stacking"),
        )
        .with_block(
            BlockPlaceholder::new("Drive 1", BlockType::Drive)
                .with_description("Primary overdrive pedal"),
        )
        .with_block(
            BlockPlaceholder::new("Drive 2", BlockType::Drive)
                .with_description("Secondary overdrive for stacking"),
        )
}

fn guitar_volume() -> ModuleTemplate {
    ModuleTemplate::new("Volume", ModuleType::Volume)
        .with_description("Volume pedal — expression-controlled level")
        .with_block(
            BlockPlaceholder::new("Volume Pedal", BlockType::Volume)
                .with_description("Expression pedal for volume swells and control"),
        )
}

fn guitar_prefx() -> ModuleTemplate {
    ModuleTemplate::new("Pre-FX", ModuleType::PreFx)
        .with_description("Effects before the amp — EQ and color")
        .with_block(
            BlockPlaceholder::new("Pre EQ", BlockType::Eq)
                .with_alias("Pre-EQ")
                .with_description("Pre-amp EQ for tone shaping before saturation"),
        )
}

fn guitar_amp_cab() -> ModuleTemplate {
    ModuleTemplate::new("Amp", ModuleType::Amp)
        .with_description("Parallel amplifier pair — input splits to both amps")
        .with_grid_size(1, 2)
        .with_block(
            BlockPlaceholder::new("Amp L", BlockType::Amp)
                .with_description("Left amplifier (includes cabinet)")
                .at(0, 0),
        )
        .with_block(
            BlockPlaceholder::new("Amp R", BlockType::Amp)
                .with_description("Right amplifier (includes cabinet)")
                .at(0, 1),
        )
}

fn guitar_modulation() -> ModuleTemplate {
    ModuleTemplate::new("Modulation", ModuleType::Modulation)
        .with_description("Modulation effects — chorus, flanger, phaser")
        .with_block(
            BlockPlaceholder::new("Chorus", BlockType::Chorus)
                .with_description("Chorus effect for width and shimmer"),
        )
        .with_block(
            BlockPlaceholder::new("Flanger", BlockType::Flanger)
                .with_description("Flanger effect for jet and sweep sounds"),
        )
        .with_block(
            BlockPlaceholder::new("Phaser", BlockType::Phaser)
                .with_description("Phaser effect for swirling modulation"),
        )
}

fn guitar_time() -> ModuleTemplate {
    ModuleTemplate::new("Time", ModuleType::Time)
        .with_description("Parallel time FX — 3 lanes (top, direct, bottom)")
        .with_grid_size(2, 3)
        .with_block(
            BlockPlaceholder::new("DLY 1", BlockType::Delay)
                .with_description("Delay lane 1 (top)")
                .at(0, 0),
        )
        .with_block(
            BlockPlaceholder::new("VERB 1", BlockType::Reverb)
                .with_description("Reverb lane 1 (top)")
                .at(1, 0),
        )
        // Row 1 is intentionally empty — direct/dry pass-through lane
        .with_block(
            BlockPlaceholder::new("DLY 2", BlockType::Delay)
                .with_description("Delay lane 2 (bottom)")
                .at(0, 2),
        )
        .with_block(
            BlockPlaceholder::new("VERB 2", BlockType::Reverb)
                .with_description("Reverb lane 2 (bottom)")
                .at(1, 2),
        )
}

fn guitar_motion() -> ModuleTemplate {
    ModuleTemplate::new("Motion", ModuleType::Motion)
        .with_description("Rhythmic motion effects — tremolo, vibrato, rotary")
        .with_block(
            BlockPlaceholder::new("Tremolo", BlockType::Tremolo)
                .with_description("Amplitude tremolo for rhythmic pulsing"),
        )
        .with_block(
            BlockPlaceholder::new("Vibrato", BlockType::Vibrato)
                .with_description("Pitch vibrato effect"),
        )
        .with_block(
            BlockPlaceholder::new("Rotary", BlockType::Rotary)
                .with_description("Leslie / rotary speaker simulation"),
        )
}

fn guitar_mastering() -> ModuleTemplate {
    ModuleTemplate::new("Mastering", ModuleType::Master)
        .with_description("Output processing — EQ, multiband comp, limiter, volume")
        .with_block(
            BlockPlaceholder::new("Mastering EQ", BlockType::Eq)
                .with_alias("Mstr-EQ")
                .with_description("Final EQ for overall tone balance"),
        )
        .with_block(
            BlockPlaceholder::new("Multiband Compressor", BlockType::Compressor)
                .with_alias("MB-Comp")
                .with_description("Multiband compression for frequency-specific control"),
        )
        .with_block(
            BlockPlaceholder::new("Limiter", BlockType::Limiter)
                .with_description("Output limiter for peak protection"),
        )
        .with_block(
            BlockPlaceholder::new("Output Volume", BlockType::Volume)
                .with_alias("Out-Vol")
                .with_description("Final output level"),
        )
}

// ─────────────────────────────────────────────────────────────────────────────
// Vocal Rig Template
// ─────────────────────────────────────────────────────────────────────────────

/// Standard vocal rig template with 5 modules.
///
/// Signal chain order: Rescue → Correction → Tonal → Modulation → Time
pub fn vocal_rig_template() -> RigTemplate {
    RigTemplate::new("Vocal Rig Template")
        .with_description("Standard vocal signal chain with 5 processing stages")
        .with_module(vocal_rescue())
        .with_module(vocal_correction())
        .with_module(vocal_tonal())
        .with_module(vocal_modulation())
        .with_module(vocal_sends())
}

fn vocal_rescue() -> ModuleTemplate {
    ModuleTemplate::new("Rescue", ModuleType::Rescue)
        .with_description("Emergency cleanup — de-ess, gate, rescue EQ, control compression")
        .with_block(
            BlockPlaceholder::new("De-Esser", BlockType::DeEsser)
                .with_description("Sibilance reduction — tame harsh S and T sounds"),
        )
        .with_block(
            BlockPlaceholder::new("Gate", BlockType::Gate)
                .with_alias("Renegate")
                .with_description("Noise gate for room noise and bleed reduction"),
        )
        .with_block(
            BlockPlaceholder::new("Rescue EQ", BlockType::Eq)
                .with_alias("Rescue-EQ")
                .with_description("Subtractive EQ for fixing problem frequencies"),
        )
        .with_block(
            BlockPlaceholder::new("Control Compressor", BlockType::Compressor)
                .with_alias("Ctrl-Comp")
                .with_description("Gentle compression to even out dynamic range"),
        )
}

fn vocal_correction() -> ModuleTemplate {
    ModuleTemplate::new("Correction", ModuleType::Correction)
        .with_description("Pitch correction and tuning")
        .with_block(
            BlockPlaceholder::new("Tuner", BlockType::Tuner)
                .with_description("Real-time pitch correction and tuning"),
        )
}

fn vocal_tonal() -> ModuleTemplate {
    ModuleTemplate::new("Tonal", ModuleType::Tonal)
        .with_description("Tonal shaping — style compression, EQ, saturation")
        .with_block(
            BlockPlaceholder::new("Style Compressor", BlockType::Compressor)
                .with_alias("Style-Comp")
                .with_description("Character compression for vocal tone and glue"),
        )
        .with_block(
            BlockPlaceholder::new("Tonal EQ", BlockType::Eq)
                .with_alias("Tone-EQ")
                .with_description("Additive EQ for presence, air, and warmth"),
        )
        .with_block(
            BlockPlaceholder::new("Saturator", BlockType::Saturator)
                .with_description("Harmonic saturation for warmth and grit"),
        )
}

fn vocal_modulation() -> ModuleTemplate {
    ModuleTemplate::new("Modulation", ModuleType::VocalModulation)
        .with_description("Vocal modulation effects — chorus, flanger")
        .with_block(
            BlockPlaceholder::new("Vocal Chorus", BlockType::Chorus)
                .with_description("Subtle chorus for vocal width and shimmer"),
        )
        .with_block(
            BlockPlaceholder::new("Vocal Flanger", BlockType::Flanger)
                .with_description("Flanger for creative vocal effects"),
        )
}

fn vocal_sends() -> ModuleTemplate {
    ModuleTemplate::new("Sends", ModuleType::Sends)
        .with_description("Send effects — reverb, delay, special")
        .with_block(
            BlockPlaceholder::new("Verb Send", BlockType::Send)
                .with_description("Send to reverb bus for space and depth"),
        )
        .with_block(
            BlockPlaceholder::new("Delay Send", BlockType::Send)
                .with_description("Send to delay bus for echo and repeat"),
        )
        .with_block(
            BlockPlaceholder::new("Special Send", BlockType::Send)
                .with_description("Send to special effects bus"),
        )
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn guitar_template_has_11_modules() {
        let t = guitar_rig_template();
        assert_eq!(t.modules.len(), 11);
        assert_eq!(t.modules[0].module_type, ModuleType::Source);
        assert_eq!(t.modules[1].module_type, ModuleType::Dynamics);
        assert_eq!(t.modules[2].module_type, ModuleType::Special);
        assert_eq!(t.modules[3].module_type, ModuleType::Drive);
        assert_eq!(t.modules[4].module_type, ModuleType::Volume);
        assert_eq!(t.modules[5].module_type, ModuleType::PreFx);
        assert_eq!(t.modules[6].module_type, ModuleType::Amp);
        assert_eq!(t.modules[7].module_type, ModuleType::Modulation);
        assert_eq!(t.modules[8].module_type, ModuleType::Time);
        assert_eq!(t.modules[9].module_type, ModuleType::Motion);
        assert_eq!(t.modules[10].module_type, ModuleType::Master);
    }

    #[test]
    fn guitar_template_block_counts() {
        let t = guitar_rig_template();
        assert_eq!(t.modules[0].blocks.len(), 2, "Source: gate + volume");
        assert_eq!(t.modules[1].blocks.len(), 1, "Dynamics: compressor");
        assert_eq!(
            t.modules[2].blocks.len(),
            4,
            "Special: filter, wah, pitch, doubler"
        );
        assert_eq!(t.modules[3].blocks.len(), 3, "Drive: boost, drive1, drive2");
        assert_eq!(t.modules[4].blocks.len(), 1, "Volume: pedal");
        assert_eq!(t.modules[5].blocks.len(), 1, "PreFX: eq");
        assert_eq!(t.modules[6].blocks.len(), 2, "Amp: amp L, amp R");
        assert_eq!(
            t.modules[7].blocks.len(),
            3,
            "Modulation: chorus, flanger, phaser"
        );
        assert_eq!(
            t.modules[8].blocks.len(),
            4,
            "Time: dly1, verb1, dly2, verb2"
        );
        assert_eq!(
            t.modules[9].blocks.len(),
            3,
            "Motion: tremolo, vibrato, rotary"
        );
        assert_eq!(
            t.modules[10].blocks.len(),
            4,
            "Mastering: eq, mb-comp, limiter, vol"
        );
    }

    #[test]
    fn guitar_template_total_blocks() {
        let t = guitar_rig_template();
        let total: usize = t.modules.iter().map(|m| m.blocks.len()).sum();
        assert_eq!(total, 28);
    }

    #[test]
    fn guitar_template_instantiates() {
        let t = guitar_rig_template();
        let (preset, module_presets) = t.instantiate();

        assert_eq!(preset.module_assignments.len(), 11);
        assert_eq!(module_presets.len(), 11);

        for mp in &module_presets {
            for mb in &mp.blocks {
                assert!(mb.block.is_placeholder());
            }
        }
    }

    #[test]
    fn vocal_template_has_5_modules() {
        let t = vocal_rig_template();
        assert_eq!(t.modules.len(), 5);
        assert_eq!(t.modules[0].module_type, ModuleType::Rescue);
        assert_eq!(t.modules[1].module_type, ModuleType::Correction);
        assert_eq!(t.modules[2].module_type, ModuleType::Tonal);
        assert_eq!(t.modules[3].module_type, ModuleType::VocalModulation);
        assert_eq!(t.modules[4].module_type, ModuleType::Sends);
    }

    #[test]
    fn vocal_template_block_counts() {
        let t = vocal_rig_template();
        assert_eq!(
            t.modules[0].blocks.len(),
            4,
            "Rescue: de-esser, gate, eq, comp"
        );
        assert_eq!(t.modules[1].blocks.len(), 1, "Correction: tuner");
        assert_eq!(t.modules[2].blocks.len(), 3, "Tonal: comp, eq, saturator");
        assert_eq!(t.modules[3].blocks.len(), 2, "Modulation: chorus, flanger");
        assert_eq!(t.modules[4].blocks.len(), 3, "Sends: verb, delay, special");
    }

    #[test]
    fn vocal_template_total_blocks() {
        let t = vocal_rig_template();
        let total: usize = t.modules.iter().map(|m| m.blocks.len()).sum();
        assert_eq!(total, 13);
    }

    #[test]
    fn vocal_template_instantiates() {
        let t = vocal_rig_template();
        let (preset, module_presets) = t.instantiate();

        assert_eq!(preset.module_assignments.len(), 5);
        assert_eq!(module_presets.len(), 5);

        for mp in &module_presets {
            for mb in &mp.blocks {
                assert!(mb.block.is_placeholder());
            }
        }
    }

    #[test]
    fn vocal_rescue_has_aliases() {
        let t = vocal_rig_template();
        let rescue = &t.modules[0];
        assert_eq!(rescue.blocks[1].alias.as_deref(), Some("Renegate"));
        assert_eq!(rescue.blocks[2].alias.as_deref(), Some("Rescue-EQ"));
        assert_eq!(rescue.blocks[3].alias.as_deref(), Some("Ctrl-Comp"));
    }
}
