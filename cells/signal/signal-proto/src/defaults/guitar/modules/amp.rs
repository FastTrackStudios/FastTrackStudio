//! Amp module presets — stereo amp pairs (L/R).
//!
//! Each preset contains two amp blocks running in stereo. Cabinets are
//! included within the amp models themselves.

use crate::defaults::blocks;
use crate::module::ModuleBlock;
use crate::module::ModuleType;
use crate::module_preset::{ModulePreset, ModuleSnapshot};
use crate::normalized::Order;

/// "Dream and Ruby" amp preset — Fender Dream (L) + Vox Ruby (R) stereo pair.
///
/// Snapshots: Clean, Breakup, Drive.
pub fn dream_and_ruby() -> ModulePreset {
    let mut preset = ModulePreset::new("Dream and Ruby", ModuleType::Amp)
        .with_description("Fender-voiced clean (L) and Vox-voiced chime (R), stereo");

    preset.add_block(ModuleBlock::new(blocks::amp::dream(), Order::new(0)));
    preset.add_block(ModuleBlock::new(blocks::amp::ruby(), Order::new(1)));

    let clean = ModuleSnapshot::new("Clean");
    let breakup = ModuleSnapshot::new("Breakup");
    let drive = ModuleSnapshot::new("Drive");

    let default_id = preset.add_snapshot(clean);
    preset.add_snapshot(breakup);
    preset.add_snapshot(drive);
    preset.set_default_snapshot(default_id);

    preset
}

/// "Deluxe and AC30" amp preset — Fender Deluxe (L) + Vox AC30 (R) stereo pair.
///
/// Snapshots: Clean, Breakup, Drive.
pub fn deluxe_and_ac30() -> ModulePreset {
    let mut preset = ModulePreset::new("Deluxe and AC30", ModuleType::Amp)
        .with_description("Classic Fender Deluxe (L) and Vox AC30 (R), stereo");

    preset.add_block(ModuleBlock::new(blocks::amp::deluxe(), Order::new(0)));
    preset.add_block(ModuleBlock::new(blocks::amp::ac30(), Order::new(1)));

    let clean = ModuleSnapshot::new("Clean");
    let breakup = ModuleSnapshot::new("Breakup");
    let drive = ModuleSnapshot::new("Drive");

    let default_id = preset.add_snapshot(clean);
    preset.add_snapshot(breakup);
    preset.add_snapshot(drive);
    preset.set_default_snapshot(default_id);

    preset
}

/// "Dumble and Two-Rock" amp preset — boutique stereo pair.
///
/// Snapshots: Ultra-Clean, Breakup, Can't Find the Light, Roomy.
pub fn dumble_and_two_rock() -> ModulePreset {
    let mut preset = ModulePreset::new("Dumble and Two-Rock", ModuleType::Amp)
        .with_description("Boutique amps in stereo — ultra-clean to singing breakup");

    preset.add_block(ModuleBlock::new(blocks::amp::dumble(), Order::new(0)));
    preset.add_block(ModuleBlock::new(blocks::amp::two_rock(), Order::new(1)));

    let ultra_clean = ModuleSnapshot::new("Ultra-Clean");
    let breakup = ModuleSnapshot::new("Breakup");
    let light = ModuleSnapshot::new("Can't Find the Light");
    let roomy = ModuleSnapshot::new("Roomy");

    let default_id = preset.add_snapshot(ultra_clean);
    preset.add_snapshot(breakup);
    preset.add_snapshot(light);
    preset.add_snapshot(roomy);
    preset.set_default_snapshot(default_id);

    preset
}

/// "Marshall Stack" amp preset — dual JCM800 stereo pair.
///
/// Snapshots: Clean, Crunch, Drive.
pub fn marshall_stack() -> ModulePreset {
    let mut preset = ModulePreset::new("Marshall Stack", ModuleType::Amp)
        .with_description("Dual Marshall JCM800 in stereo");

    preset.add_block(ModuleBlock::new(blocks::amp::marshall(), Order::new(0)));
    preset.add_block(ModuleBlock::new(blocks::amp::marshall(), Order::new(1)));

    let clean = ModuleSnapshot::new("Clean");
    let crunch = ModuleSnapshot::new("Crunch");
    let drive = ModuleSnapshot::new("Drive");

    let default_id = preset.add_snapshot(clean);
    preset.add_snapshot(crunch);
    preset.add_snapshot(drive);
    preset.set_default_snapshot(default_id);

    preset
}
