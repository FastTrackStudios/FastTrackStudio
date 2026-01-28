//! Time-based module presets — delay, reverb, freeze.

use crate::defaults::blocks;
use crate::module::ModuleBlock;
use crate::module_preset::ModulePreset;
use crate::module::ModuleType;
use crate::normalized::Order;

/// "Delay" time preset — single delay block.
pub fn delay() -> ModulePreset {
    let mut preset = ModulePreset::new("Delay", ModuleType::Time);
    preset.add_block(ModuleBlock::new(blocks::time::delay(), Order::new(0)));
    preset
}

/// "Reverb" time preset — single reverb block.
pub fn reverb() -> ModulePreset {
    let mut preset = ModulePreset::new("Reverb", ModuleType::Time);
    preset.add_block(ModuleBlock::new(blocks::time::reverb(), Order::new(0)));
    preset
}

/// "Freeze" time preset — infinite hold / freeze block.
pub fn freeze() -> ModulePreset {
    let mut preset = ModulePreset::new("Freeze", ModuleType::Time);
    preset.add_block(ModuleBlock::new(blocks::time::freeze(), Order::new(0)));
    preset
}
