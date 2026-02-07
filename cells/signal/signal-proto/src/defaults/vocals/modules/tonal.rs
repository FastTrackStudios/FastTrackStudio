//! Tonal module preset — Style Compressor, Tonal EQ, Saturator.

use crate::defaults::blocks;
use crate::module::ModuleBlock;
use crate::module_preset::{ModulePreset, ModuleSnapshot};
use crate::module::ModuleType;
use crate::normalized::Order;

/// "Vocal Tone" module preset — tonal shaping chain.
///
/// Style Compressor → Tonal EQ → Saturator.
///
/// Snapshots: Natural, Warm, Bright, Powerful, Breathy.
pub fn vocal_tone() -> ModulePreset {
    let mut preset = ModulePreset::new("Vocal Tone", ModuleType::Tonal)
        .with_description("Tonal shaping: style compressor, EQ, and saturator");

    preset.add_block(ModuleBlock::new(blocks::vocal::style_compressor(), Order::new(0)));
    preset.add_block(ModuleBlock::new(blocks::vocal::tonal_eq(), Order::new(1)));
    preset.add_block(ModuleBlock::new(blocks::vocal::saturator(), Order::new(2)));

    let natural = ModuleSnapshot::new("Natural");
    let warm = ModuleSnapshot::new("Warm");
    let bright = ModuleSnapshot::new("Bright");
    let powerful = ModuleSnapshot::new("Powerful");
    let breathy = ModuleSnapshot::new("Breathy");

    let default_id = preset.add_snapshot(natural);
    preset.add_snapshot(warm);
    preset.add_snapshot(bright);
    preset.add_snapshot(powerful);
    preset.add_snapshot(breathy);
    preset.set_default_snapshot(default_id);

    preset
}
