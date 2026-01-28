//! Source module preset — input gate and volume.

use crate::defaults::blocks;
use crate::module::ModuleBlock;
use crate::module_preset::{ModulePreset, ModuleSnapshot};
use crate::module::ModuleType;
use crate::normalized::Order;

/// "Guitar Input" module preset with input gate + input volume.
///
/// Snapshots: Subtle Gate, No Gate, Heavy Gate.
pub fn guitar_input() -> ModulePreset {
    let mut preset = ModulePreset::new("Guitar Input", ModuleType::Source);

    preset.add_block(ModuleBlock::new(blocks::source::input_gate(), Order::new(0)));
    preset.add_block(ModuleBlock::new(blocks::source::input_volume(), Order::new(1)));

    let subtle = ModuleSnapshot::new("Subtle Gate");
    let no_gate = ModuleSnapshot::new("No Gate");
    let heavy = ModuleSnapshot::new("Heavy Gate");

    let default_id = preset.add_snapshot(subtle);
    preset.add_snapshot(no_gate);
    preset.add_snapshot(heavy);
    preset.set_default_snapshot(default_id);

    preset
}
