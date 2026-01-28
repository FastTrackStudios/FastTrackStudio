//! Correction module preset — pitch correction (Graillon 3).

use crate::defaults::blocks;
use crate::module::ModuleBlock;
use crate::module_preset::{ModulePreset, ModuleSnapshot};
use crate::module::ModuleType;
use crate::normalized::Order;

/// "Pitch Correction" module preset — Graillon 3 tuner.
///
/// Snapshots: Subtle, Standard, Hard Tune, Off.
pub fn pitch_correction() -> ModulePreset {
    let mut preset = ModulePreset::new("Pitch Correction", ModuleType::Correction)
        .with_description("Pitch correction via Graillon 3");

    preset.add_block(ModuleBlock::new(blocks::vocal::tuner(), Order::new(0)));

    let subtle = ModuleSnapshot::new("Subtle");
    let standard = ModuleSnapshot::new("Standard");
    let hard_tune = ModuleSnapshot::new("Hard Tune");
    let off = ModuleSnapshot::new("Off");

    let default_id = preset.add_snapshot(subtle);
    preset.add_snapshot(standard);
    preset.add_snapshot(hard_tune);
    preset.add_snapshot(off);
    preset.set_default_snapshot(default_id);

    preset
}
