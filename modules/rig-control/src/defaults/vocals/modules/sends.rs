//! Sends module preset — Verb, Delay, Special (routed separately).

use crate::defaults::blocks;
use crate::module::ModuleBlock;
use crate::module_preset::{ModulePreset, ModuleSnapshot};
use crate::module::ModuleType;
use crate::normalized::Order;

/// "Vocal Sends" module preset — Verb + Delay + Special sends.
///
/// Snapshots: Dry, Room, Hall, Plate, Slapback, Long Delay, Spacey.
pub fn vocal_sends() -> ModulePreset {
    let mut preset = ModulePreset::new("Vocal Sends", ModuleType::Sends)
        .with_description("Verb, delay, and special effect sends routed separately");

    preset.add_block(ModuleBlock::new(blocks::vocal::verb_send(), Order::new(0)));
    preset.add_block(ModuleBlock::new(blocks::vocal::delay_send(), Order::new(1)));
    preset.add_block(ModuleBlock::new(blocks::vocal::special_send(), Order::new(2)));

    let dry = ModuleSnapshot::new("Dry");
    let room = ModuleSnapshot::new("Room");
    let hall = ModuleSnapshot::new("Hall");
    let plate = ModuleSnapshot::new("Plate");
    let slapback = ModuleSnapshot::new("Slapback");
    let long_delay = ModuleSnapshot::new("Long Delay");
    let spacey = ModuleSnapshot::new("Spacey");

    let default_id = preset.add_snapshot(dry);
    preset.add_snapshot(room);
    preset.add_snapshot(hall);
    preset.add_snapshot(plate);
    preset.add_snapshot(slapback);
    preset.add_snapshot(long_delay);
    preset.add_snapshot(spacey);
    preset.set_default_snapshot(default_id);

    preset
}
