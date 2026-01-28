//! Master module preset — mastering EQ, multiband compressor, output volume.

use crate::defaults::blocks;
use crate::module::ModuleBlock;
use crate::module_preset::ModulePreset;
use crate::module::ModuleType;
use crate::normalized::Order;

/// "Master" module preset — EQ + multiband compressor + output volume.
pub fn master() -> ModulePreset {
    let mut preset = ModulePreset::new("Master", ModuleType::Master);

    preset.add_block(ModuleBlock::new(blocks::master::mastering_eq(), Order::new(0)));
    preset.add_block(ModuleBlock::new(blocks::master::multiband_compressor(), Order::new(1)));
    preset.add_block(ModuleBlock::new(blocks::master::output_volume(), Order::new(2)));

    preset
}
