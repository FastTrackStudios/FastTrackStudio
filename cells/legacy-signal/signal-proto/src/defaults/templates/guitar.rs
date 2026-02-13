//! Guitar rig template — 11-module signal chain with 28 block slots.
//!
//! Signal chain order:
//! Source → Dynamics → Special → Drive → Volume Pedal → Pre-FX →
//! Amp/Cab → Modulation → Time → Motion → Mastering

use crate::block::BlockType;
use crate::module::ModuleType;
use crate::rig::InstrumentType;
use crate::template::{BlockTemplate, EngineTemplate, LayerTemplate, ModuleTemplate, RigTemplate};
use crate::version::LayerIndex;

/// Standard guitar rig template with 11 modules in a single engine/layer.
pub fn guitar_rig_template() -> RigTemplate {
    let layer = LayerTemplate::new("Main", LayerIndex::new(1))
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
        .with_module(guitar_mastering());

    let engine = EngineTemplate::new("Guitar Engine", InstrumentType::Guitar, layer);

    RigTemplate::new("Guitar Rig Template", InstrumentType::Guitar, engine)
        .with_description("Standard guitar signal chain with 11 processing stages")
}

fn guitar_source() -> ModuleTemplate {
    ModuleTemplate::new("Source", ModuleType::Source)
        .with_description("Input conditioning — gate and initial volume")
        .with_block(
            BlockTemplate::new("Input Gate", BlockType::Gate)
                .with_description("Noise gate to clean up input signal"),
        )
        .with_block(
            BlockTemplate::new("Input Volume", BlockType::Volume)
                .with_description("Input level trim before processing"),
        )
}

fn guitar_dynamics() -> ModuleTemplate {
    ModuleTemplate::new("Dynamics", ModuleType::Dynamics)
        .with_description("Compression and dynamic control")
        .with_block(
            BlockTemplate::new("Compressor", BlockType::Compressor)
                .with_description("Main compressor for dynamic control"),
        )
}

fn guitar_special() -> ModuleTemplate {
    ModuleTemplate::new("Special", ModuleType::Special)
        .with_description("Special effects — wah, filter, pitch, doubler")
        .with_block(
            BlockTemplate::new("Envelope Filter", BlockType::Filter)
                .with_description("Auto-wah / envelope filter effect"),
        )
        .with_block(
            BlockTemplate::new("Wah Pedal", BlockType::Wah)
                .with_description("Expression-controlled wah"),
        )
        .with_block(
            BlockTemplate::new("Pitch Octave FX", BlockType::Pitch)
                .with_description("Pitch shifting and octave effects"),
        )
        .with_block(
            BlockTemplate::new("Doubler", BlockType::Doubler)
                .with_description("Signal doubling for width and thickness"),
        )
}

fn guitar_drive() -> ModuleTemplate {
    ModuleTemplate::new("Drive", ModuleType::Drive)
        .with_description("Overdrive and distortion — boost, drive 1, drive 2")
        .with_block(
            BlockTemplate::new("Boost", BlockType::Boost)
                .with_description("Clean boost for pushing the amp or stacking"),
        )
        .with_block(
            BlockTemplate::new("Drive 1", BlockType::Drive)
                .with_description("Primary overdrive pedal"),
        )
        .with_block(
            BlockTemplate::new("Drive 2", BlockType::Drive)
                .with_description("Secondary overdrive for stacking"),
        )
}

fn guitar_volume() -> ModuleTemplate {
    ModuleTemplate::new("Volume", ModuleType::Volume)
        .with_description("Volume pedal — expression-controlled level")
        .with_block(
            BlockTemplate::new("Volume Pedal", BlockType::Volume)
                .with_description("Expression pedal for volume swells and control"),
        )
}

fn guitar_prefx() -> ModuleTemplate {
    ModuleTemplate::new("Pre-FX", ModuleType::PreFx)
        .with_description("Effects before the amp — EQ and color")
        .with_block(
            BlockTemplate::new("Pre EQ", BlockType::Eq)
                .with_alias("Pre-EQ")
                .with_description("Pre-amp EQ for tone shaping before saturation"),
        )
}

fn guitar_amp_cab() -> ModuleTemplate {
    ModuleTemplate::new("Amp", ModuleType::Amp)
        .with_description("Parallel amplifier pair — input splits to both amps")
        .with_grid_size(1, 2)
        .with_block(
            BlockTemplate::new("Amp L", BlockType::Amp)
                .with_description("Left amplifier (includes cabinet)")
                .at(0, 0),
        )
        .with_block(
            BlockTemplate::new("Amp R", BlockType::Amp)
                .with_description("Right amplifier (includes cabinet)")
                .at(0, 1),
        )
}

fn guitar_modulation() -> ModuleTemplate {
    ModuleTemplate::new("Modulation", ModuleType::Modulation)
        .with_description("Modulation effects — chorus, flanger, phaser")
        .with_block(
            BlockTemplate::new("Chorus", BlockType::Chorus)
                .with_description("Chorus effect for width and shimmer"),
        )
        .with_block(
            BlockTemplate::new("Flanger", BlockType::Flanger)
                .with_description("Flanger effect for jet and sweep sounds"),
        )
        .with_block(
            BlockTemplate::new("Phaser", BlockType::Phaser)
                .with_description("Phaser effect for swirling modulation"),
        )
}

fn guitar_time() -> ModuleTemplate {
    ModuleTemplate::new("Time", ModuleType::Time)
        .with_description("Parallel time FX — 3 lanes (top, direct, bottom)")
        .with_grid_size(2, 3)
        .with_block(
            BlockTemplate::new("DLY 1", BlockType::Delay)
                .with_description("Delay lane 1 (top)")
                .at(0, 0),
        )
        .with_block(
            BlockTemplate::new("VERB 1", BlockType::Reverb)
                .with_description("Reverb lane 1 (top)")
                .at(1, 0),
        )
        // Row 1 is intentionally empty — direct/dry pass-through lane
        .with_block(
            BlockTemplate::new("DLY 2", BlockType::Delay)
                .with_description("Delay lane 2 (bottom)")
                .at(0, 2),
        )
        .with_block(
            BlockTemplate::new("VERB 2", BlockType::Reverb)
                .with_description("Reverb lane 2 (bottom)")
                .at(1, 2),
        )
}

fn guitar_motion() -> ModuleTemplate {
    ModuleTemplate::new("Motion", ModuleType::Motion)
        .with_description("Rhythmic motion effects — tremolo, vibrato, rotary")
        .with_block(
            BlockTemplate::new("Tremolo", BlockType::Tremolo)
                .with_description("Amplitude tremolo for rhythmic pulsing"),
        )
        .with_block(
            BlockTemplate::new("Vibrato", BlockType::Vibrato)
                .with_description("Pitch vibrato effect"),
        )
        .with_block(
            BlockTemplate::new("Rotary", BlockType::Rotary)
                .with_description("Leslie / rotary speaker simulation"),
        )
}

fn guitar_mastering() -> ModuleTemplate {
    ModuleTemplate::new("Mastering", ModuleType::Master)
        .with_description("Output processing — EQ, multiband comp, limiter, volume")
        .with_block(
            BlockTemplate::new("Mastering EQ", BlockType::Eq)
                .with_alias("Mstr-EQ")
                .with_description("Final EQ for overall tone balance"),
        )
        .with_block(
            BlockTemplate::new("Multiband Compressor", BlockType::Compressor)
                .with_alias("MB-Comp")
                .with_description("Multiband compression for frequency-specific control"),
        )
        .with_block(
            BlockTemplate::new("Limiter", BlockType::Limiter)
                .with_description("Output limiter for peak protection"),
        )
        .with_block(
            BlockTemplate::new("Output Volume", BlockType::Volume)
                .with_alias("Out-Vol")
                .with_description("Final output level"),
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
    fn guitar_template_has_11_modules() {
        let t = guitar_rig_template();
        let modules = t.modules();
        assert_eq!(modules.len(), 11);
        assert_eq!(modules[0].module_type, ModuleType::Source);
        assert_eq!(modules[1].module_type, ModuleType::Dynamics);
        assert_eq!(modules[2].module_type, ModuleType::Special);
        assert_eq!(modules[3].module_type, ModuleType::Drive);
        assert_eq!(modules[4].module_type, ModuleType::Volume);
        assert_eq!(modules[5].module_type, ModuleType::PreFx);
        assert_eq!(modules[6].module_type, ModuleType::Amp);
        assert_eq!(modules[7].module_type, ModuleType::Modulation);
        assert_eq!(modules[8].module_type, ModuleType::Time);
        assert_eq!(modules[9].module_type, ModuleType::Motion);
        assert_eq!(modules[10].module_type, ModuleType::Master);
    }

    #[test]
    fn guitar_template_block_counts() {
        let t = guitar_rig_template();
        let m = t.modules();
        assert_eq!(m[0].blocks.len(), 2, "Source: gate + volume");
        assert_eq!(m[1].blocks.len(), 1, "Dynamics: compressor");
        assert_eq!(m[2].blocks.len(), 4, "Special: filter, wah, pitch, doubler");
        assert_eq!(m[3].blocks.len(), 3, "Drive: boost, drive1, drive2");
        assert_eq!(m[4].blocks.len(), 1, "Volume: pedal");
        assert_eq!(m[5].blocks.len(), 1, "PreFX: eq");
        assert_eq!(m[6].blocks.len(), 2, "Amp: amp L, amp R");
        assert_eq!(m[7].blocks.len(), 3, "Modulation: chorus, flanger, phaser");
        assert_eq!(m[8].blocks.len(), 4, "Time: dly1, verb1, dly2, verb2");
        assert_eq!(m[9].blocks.len(), 3, "Motion: tremolo, vibrato, rotary");
        assert_eq!(
            m[10].blocks.len(),
            4,
            "Mastering: eq, mb-comp, limiter, vol"
        );
    }

    #[test]
    fn guitar_template_total_blocks() {
        let t = guitar_rig_template();
        let total: usize = t.modules().iter().map(|m| m.blocks.len()).sum();
        assert_eq!(total, 28);
    }

    #[test]
    fn guitar_template_instantiates_via_templatable() {
        let t = guitar_rig_template();
        let rig = Rig::from_template(&t);

        assert_eq!(rig.name, "Guitar Rig Template");
        assert_eq!(rig.engines.len(), 1);

        let layer = rig.engines.first().layers.first();
        assert_eq!(layer.modules.len(), 11);

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
    fn guitar_template_hierarchy() {
        let t = guitar_rig_template();
        assert_eq!(t.engines.len(), 1);
        assert_eq!(t.engines.first().name, "Guitar Engine");
        assert_eq!(t.engines.first().layers.len(), 1);
        assert_eq!(t.engines.first().layers.first().name, "Main");
    }

    #[test]
    fn guitar_amp_has_grid_positions() {
        let t = guitar_rig_template();
        let amp = &t.modules()[6];
        assert_eq!(amp.grid_width, Some(1));
        assert_eq!(amp.grid_height, Some(2));
        assert_eq!(amp.blocks[0].local_col, Some(0));
        assert_eq!(amp.blocks[0].local_row, Some(0));
        assert_eq!(amp.blocks[1].local_col, Some(0));
        assert_eq!(amp.blocks[1].local_row, Some(1));
    }

    #[test]
    fn guitar_time_has_grid_positions() {
        let t = guitar_rig_template();
        let time = &t.modules()[8];
        assert_eq!(time.grid_width, Some(2));
        assert_eq!(time.grid_height, Some(3));
        assert_eq!(time.blocks[0].local_col, Some(0)); // DLY 1
        assert_eq!(time.blocks[0].local_row, Some(0));
        assert_eq!(time.blocks[1].local_col, Some(1)); // VERB 1
        assert_eq!(time.blocks[1].local_row, Some(0));
        assert_eq!(time.blocks[2].local_col, Some(0)); // DLY 2
        assert_eq!(time.blocks[2].local_row, Some(2));
        assert_eq!(time.blocks[3].local_col, Some(1)); // VERB 2
        assert_eq!(time.blocks[3].local_row, Some(2));
    }
}
