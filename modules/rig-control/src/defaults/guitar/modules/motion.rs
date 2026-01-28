//! Motion module presets — tremolo, vibrato, rotary, and character effects.

use crate::defaults::blocks;
use crate::module::ModuleBlock;
use crate::module_preset::ModulePreset;
use crate::module::ModuleType;
use crate::normalized::Order;

/// "Tremolo 8th" motion preset — tremolo at 8th-note subdivision.
pub fn tremolo_8th() -> ModulePreset {
    let mut preset = ModulePreset::new("Tremolo 8th", ModuleType::Motion);
    preset.add_block(ModuleBlock::new(blocks::motion::tremolo(), Order::new(0)));
    preset
}

/// "Tremolo 16th" motion preset — tremolo at 16th-note subdivision.
pub fn tremolo_16th() -> ModulePreset {
    let mut preset = ModulePreset::new("Tremolo 16th", ModuleType::Motion);
    preset.add_block(ModuleBlock::new(blocks::motion::tremolo(), Order::new(0)));
    preset
}

/// "Vibrato" motion preset — pitch vibrato.
pub fn vibrato() -> ModulePreset {
    let mut preset = ModulePreset::new("Vibrato", ModuleType::Motion);
    preset.add_block(ModuleBlock::new(blocks::motion::vibrato(), Order::new(0)));
    preset
}

/// "Rotary" motion preset — Leslie rotary speaker effect.
pub fn rotary() -> ModulePreset {
    let mut preset = ModulePreset::new("Rotary", ModuleType::Motion);
    preset.add_block(ModuleBlock::new(blocks::motion::rotary(), Order::new(0)));
    preset
}

/// "Too Much to Drink" motion preset — pitch wobble / seasick effect.
pub fn too_much_to_drink() -> ModulePreset {
    let mut preset = ModulePreset::new("Too Much to Drink", ModuleType::Motion);
    preset.add_block(ModuleBlock::new(blocks::motion::vibrato(), Order::new(0)));
    preset.add_block(ModuleBlock::new(blocks::motion::tremolo(), Order::new(1)));
    preset
}
