//! Dynamics module preset — compressor.

use crate::defaults::blocks;
use crate::module::ModuleBlock;
use crate::module_preset::ModulePreset;
use crate::module::ModuleType;
use crate::normalized::Order;

/// "Compressor" module preset — single compressor block, no snapshots.
pub fn compressor() -> ModulePreset {
    let mut preset = ModulePreset::new("Compressor", ModuleType::Dynamics);

    preset.add_block(ModuleBlock::new(blocks::dynamics::compressor(), Order::new(0)));

    preset
}
