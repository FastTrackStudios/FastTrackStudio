//! Special effect module presets — envelope filter, wah, pitch, doubler.

use crate::defaults::blocks;
use crate::module::ModuleBlock;
use crate::module_preset::ModulePreset;
use crate::module::ModuleType;
use crate::normalized::Order;

/// "Envelope Filter" module preset — auto-wah effect.
pub fn envelope_filter() -> ModulePreset {
    let mut preset = ModulePreset::new("Envelope Filter", ModuleType::Special);
    preset.add_block(ModuleBlock::new(blocks::special::envelope_filter(), Order::new(0)));
    preset
}

/// "Wah Pedal" module preset — expression-controlled wah.
pub fn wah_pedal() -> ModulePreset {
    let mut preset = ModulePreset::new("Wah Pedal", ModuleType::Special);
    preset.add_block(ModuleBlock::new(blocks::special::wah_pedal(), Order::new(0)));
    preset
}

/// "Pitch Octave FX" module preset — pitch shifting / octave effects.
pub fn pitch_octave() -> ModulePreset {
    let mut preset = ModulePreset::new("Pitch Octave FX", ModuleType::Special);
    preset.add_block(ModuleBlock::new(blocks::special::pitch_octave(), Order::new(0)));
    preset
}

/// "Doubler" module preset — doubling / thickening effect.
pub fn doubler() -> ModulePreset {
    let mut preset = ModulePreset::new("Doubler", ModuleType::Special);
    preset.add_block(ModuleBlock::new(blocks::special::doubler(), Order::new(0)));
    preset
}
