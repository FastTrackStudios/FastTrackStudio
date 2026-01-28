//! Drive module presets — Blues Stack and Protein + JHS Kilt.

use crate::defaults::blocks;
use crate::module::ModuleBlock;
use crate::module_preset::{ModulePreset, ModuleSnapshot};
use crate::module::ModuleType;
use crate::normalized::Order;

/// "Blues Stack" drive preset — Boost, Halfman, Teal, Blues Breaker.
///
/// Snapshots: Halfman, Halfman + Teal, Halfman + Blues Breaker.
pub fn blues_stack() -> ModulePreset {
    let mut preset = ModulePreset::new("Blues Stack", ModuleType::Drive)
        .with_description("Classic blues drive tones using Klon, TS, and BB-style overdrives");

    preset.add_block(ModuleBlock::new(blocks::drive::boost(), Order::new(0)));
    preset.add_block(ModuleBlock::new(blocks::drive::halfman(), Order::new(1)));
    preset.add_block(ModuleBlock::new(blocks::drive::teal(), Order::new(2)));
    preset.add_block(ModuleBlock::new(blocks::drive::blues_breaker(), Order::new(3)));

    let halfman = ModuleSnapshot::new("Halfman");
    let halfman_teal = ModuleSnapshot::new("Halfman + Teal");
    let halfman_bb = ModuleSnapshot::new("Halfman + Blues Breaker");

    let default_id = preset.add_snapshot(halfman);
    preset.add_snapshot(halfman_teal);
    preset.add_snapshot(halfman_bb);
    preset.set_default_snapshot(default_id);

    preset
}

/// "Protein + JHS Kilt" drive preset — Boost, Protein, JHS Kilt.
///
/// Snapshots: Blue Light, Green Light, Blue Heavy, Green Heavy, Blue + Green.
pub fn protein_kilt() -> ModulePreset {
    let mut preset = ModulePreset::new("Protein + JHS Kilt", ModuleType::Drive)
        .with_description("High-gain drive tones using Protein and JHS Kilt");

    preset.add_block(ModuleBlock::new(blocks::drive::boost(), Order::new(0)));
    preset.add_block(ModuleBlock::new(blocks::drive::protein(), Order::new(1)));
    preset.add_block(ModuleBlock::new(blocks::drive::jhs_kilt(), Order::new(2)));

    preset.add_snapshot(ModuleSnapshot::new("Blue Light"));
    preset.add_snapshot(ModuleSnapshot::new("Green Light"));
    preset.add_snapshot(ModuleSnapshot::new("Blue Heavy"));
    preset.add_snapshot(ModuleSnapshot::new("Green Heavy"));

    let blue_green = ModuleSnapshot::new("Blue + Green");
    let default_id = preset.add_snapshot(blue_green);
    preset.set_default_snapshot(default_id);

    preset
}
