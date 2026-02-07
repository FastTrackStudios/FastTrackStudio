//! Pre-FX module preset — Gravity Tank.

use crate::defaults::blocks;
use crate::module::ModuleBlock;
use crate::module_preset::{ModulePreset, ModuleSnapshot};
use crate::module::ModuleType;
use crate::normalized::Order;

/// "Gravity Tank" pre-FX preset — harmonic tremolo + spring reverb.
///
/// Snapshots: Harmonic Tremolo Light, Harmonic Tremolo Strong,
/// Spring Reverb Light, Boing.
pub fn gravity_tank() -> ModulePreset {
    let mut preset = ModulePreset::new("Gravity Tank", ModuleType::PreFx)
        .with_description("Vintage tremolo and spring reverb effects before the amp");

    preset.add_block(ModuleBlock::new(blocks::motion::harmonic_tremolo(), Order::new(0)));
    preset.add_block(ModuleBlock::new(blocks::time::spring_reverb(), Order::new(1)));

    let ht_light = ModuleSnapshot::new("Harmonic Tremolo Light");
    let ht_strong = ModuleSnapshot::new("Harmonic Tremolo Strong");
    let spring_light = ModuleSnapshot::new("Spring Reverb Light");
    let boing = ModuleSnapshot::new("Boing");

    let default_id = preset.add_snapshot(ht_light);
    preset.add_snapshot(ht_strong);
    preset.add_snapshot(spring_light);
    preset.add_snapshot(boing);
    preset.set_default_snapshot(default_id);

    preset
}
