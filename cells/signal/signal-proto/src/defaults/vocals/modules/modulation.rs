//! Vocal Modulation module presets — Chorus, Flanger (MIDI-triggered).

use crate::defaults::blocks;
use crate::module::ModuleBlock;
use crate::module_preset::ModulePreset;
use crate::module::ModuleType;
use crate::normalized::Order;

/// "Vocal Chorus" modulation preset — TAL Chorus (always on, MIDI-toggleable).
pub fn vocal_chorus() -> ModulePreset {
    let mut preset = ModulePreset::new("Vocal Chorus", ModuleType::VocalModulation)
        .with_description("TAL Chorus for subtle vocal thickening, MIDI-triggered");
    preset.add_block(ModuleBlock::new(blocks::vocal::vocal_chorus(), Order::new(0)));
    preset
}

/// "Vocal Flanger" modulation preset.
pub fn vocal_flanger() -> ModulePreset {
    let mut preset = ModulePreset::new("Vocal Flanger", ModuleType::VocalModulation)
        .with_description("Vocal flanger effect, MIDI-triggered");
    preset.add_block(ModuleBlock::new(blocks::vocal::vocal_flanger(), Order::new(0)));
    preset
}
