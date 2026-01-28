//! Modulation module presets — chorus, flanger, phaser.

use crate::defaults::blocks;
use crate::module::ModuleBlock;
use crate::module_preset::ModulePreset;
use crate::module::ModuleType;
use crate::normalized::Order;

/// "Chorus" modulation preset — single chorus block.
pub fn chorus() -> ModulePreset {
    let mut preset = ModulePreset::new("Chorus", ModuleType::Modulation);
    preset.add_block(ModuleBlock::new(blocks::modulation::chorus(), Order::new(0)));
    preset
}

/// "Flanger" modulation preset — single flanger block.
pub fn flanger() -> ModulePreset {
    let mut preset = ModulePreset::new("Flanger", ModuleType::Modulation);
    preset.add_block(ModuleBlock::new(blocks::modulation::flanger(), Order::new(0)));
    preset
}

/// "Phaser" modulation preset — single phaser block.
pub fn phaser() -> ModulePreset {
    let mut preset = ModulePreset::new("Phaser", ModuleType::Modulation);
    preset.add_block(ModuleBlock::new(blocks::modulation::phaser(), Order::new(0)));
    preset
}
