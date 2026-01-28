//! Rescue module preset — De-Esser, Gate, Rescue-EQ, Control Compressor.

use crate::defaults::blocks;
use crate::module::ModuleBlock;
use crate::module_preset::{ModulePreset, ModuleSnapshot};
use crate::module::ModuleType;
use crate::normalized::Order;

/// "Vocal Rescue" module preset — input cleanup chain.
///
/// De-Esser (always on) → Gate → Rescue EQ → Control Compressor.
///
/// Snapshots: Standard, Heavy Gate, Light Touch, No Gate.
pub fn vocal_rescue() -> ModulePreset {
    let mut preset = ModulePreset::new("Vocal Rescue", ModuleType::Rescue)
        .with_description("Input cleanup: de-esser, gate, rescue EQ, and control compressor");

    preset.add_block(ModuleBlock::new(blocks::vocal::de_esser(), Order::new(0)));
    preset.add_block(ModuleBlock::new(blocks::vocal::renegate(), Order::new(1)));
    preset.add_block(ModuleBlock::new(blocks::vocal::rescue_eq(), Order::new(2)));
    preset.add_block(ModuleBlock::new(blocks::vocal::control_compressor(), Order::new(3)));

    let standard = ModuleSnapshot::new("Standard");
    let heavy_gate = ModuleSnapshot::new("Heavy Gate");
    let light_touch = ModuleSnapshot::new("Light Touch");
    let no_gate = ModuleSnapshot::new("No Gate");

    let default_id = preset.add_snapshot(standard);
    preset.add_snapshot(heavy_gate);
    preset.add_snapshot(light_touch);
    preset.add_snapshot(no_gate);
    preset.set_default_snapshot(default_id);

    preset
}
