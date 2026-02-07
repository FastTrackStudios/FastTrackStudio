//! Amp module presets — each includes amp(s) + cabinet + room send.

use crate::defaults::blocks;
use crate::module::ModuleBlock;
use crate::module_preset::{ModulePreset, ModuleSnapshot};
use crate::module::ModuleType;
use crate::normalized::Order;

/// "Dream and Ruby" amp preset — Fender Dream + Vox Ruby + Cab + Room.
///
/// Snapshots: Clean, Breakup, Drive.
pub fn dream_and_ruby() -> ModulePreset {
    let mut preset = ModulePreset::new("Dream and Ruby", ModuleType::Amp)
        .with_description("Fender-voiced clean and Vox-voiced chime, blended");

    preset.add_block(ModuleBlock::new(blocks::amp::dream(), Order::new(0)));
    preset.add_block(ModuleBlock::new(blocks::amp::ruby(), Order::new(1)));
    preset.add_block(ModuleBlock::new(blocks::amp::cabinet(), Order::new(2)));
    preset.add_block(ModuleBlock::new(blocks::amp::room_send(), Order::new(3)));

    let clean = ModuleSnapshot::new("Clean");
    let breakup = ModuleSnapshot::new("Breakup");
    let drive = ModuleSnapshot::new("Drive");

    let default_id = preset.add_snapshot(clean);
    preset.add_snapshot(breakup);
    preset.add_snapshot(drive);
    preset.set_default_snapshot(default_id);

    preset
}

/// "Deluxe and AC30" amp preset — Fender Deluxe + Vox AC30 + Cab + Room.
///
/// Snapshots: Clean, Breakup, Drive.
pub fn deluxe_and_ac30() -> ModulePreset {
    let mut preset = ModulePreset::new("Deluxe and AC30", ModuleType::Amp)
        .with_description("Classic Fender Deluxe and Vox AC30 tones");

    preset.add_block(ModuleBlock::new(blocks::amp::deluxe(), Order::new(0)));
    preset.add_block(ModuleBlock::new(blocks::amp::ac30(), Order::new(1)));
    preset.add_block(ModuleBlock::new(blocks::amp::cabinet(), Order::new(2)));
    preset.add_block(ModuleBlock::new(blocks::amp::room_send(), Order::new(3)));

    let clean = ModuleSnapshot::new("Clean");
    let breakup = ModuleSnapshot::new("Breakup");
    let drive = ModuleSnapshot::new("Drive");

    let default_id = preset.add_snapshot(clean);
    preset.add_snapshot(breakup);
    preset.add_snapshot(drive);
    preset.set_default_snapshot(default_id);

    preset
}

/// "Dumble and Two-Rock" amp preset — boutique clean/breakup tones.
///
/// Snapshots: Ultra-Clean, Breakup, Can't Find the Light, Roomy.
pub fn dumble_and_two_rock() -> ModulePreset {
    let mut preset = ModulePreset::new("Dumble and Two-Rock", ModuleType::Amp)
        .with_description("Ultra-clean to singing breakup with boutique amps");

    preset.add_block(ModuleBlock::new(blocks::amp::dumble(), Order::new(0)));
    preset.add_block(ModuleBlock::new(blocks::amp::two_rock(), Order::new(1)));
    preset.add_block(ModuleBlock::new(blocks::amp::cabinet(), Order::new(2)));
    preset.add_block(ModuleBlock::new(blocks::amp::room_send(), Order::new(3)));

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

/// "Marshall Stack" amp preset — JCM800 style + Cab + Room.
///
/// Snapshots: Clean, Crunch, Drive.
pub fn marshall_stack() -> ModulePreset {
    let mut preset = ModulePreset::new("Marshall Stack", ModuleType::Amp)
        .with_description("Classic British high-gain stack");

    preset.add_block(ModuleBlock::new(blocks::amp::marshall(), Order::new(0)));
    preset.add_block(ModuleBlock::new(blocks::amp::cabinet(), Order::new(1)));
    preset.add_block(ModuleBlock::new(blocks::amp::room_send(), Order::new(2)));

    let clean = ModuleSnapshot::new("Clean");
    let crunch = ModuleSnapshot::new("Crunch");
    let drive = ModuleSnapshot::new("Drive");

    let default_id = preset.add_snapshot(clean);
    preset.add_snapshot(crunch);
    preset.add_snapshot(drive);
    preset.set_default_snapshot(default_id);

    preset
}
