//! Default top-level vocal presets.
//!
//! Each function returns a [`Preset`] with module assignments pointing to
//! module presets from [`super::modules`]. These are the "starting point"
//! presets that profiles and songs reference.

use crate::category::{BaseTone, Genre, PresetCategory};
use crate::module_preset::{ModuleAssignment, ModulePreset};
use crate::normalized::Order;
use crate::preset::{Credit, Inspiration, Preset};

/// Helper to build a module assignment from a preset reference.
fn assign(module_preset: &ModulePreset, order: u8) -> ModuleAssignment {
    ModuleAssignment::new(module_preset.module_type, module_preset.id, Order::new(order))
}

/// Helper to build a module assignment with a named snapshot.
fn assign_snap(module_preset: &ModulePreset, order: u8, snap_name: &str) -> ModuleAssignment {
    let snap_id = module_preset
        .snapshot_by_name(snap_name)
        .expect("snapshot not found")
        .id;
    ModuleAssignment::new(module_preset.module_type, module_preset.id, Order::new(order))
        .with_snapshot(snap_id)
}

/// All module presets needed for the default vocal rig, bundled together.
///
/// Callers create this once and pass references when building presets,
/// profiles, and songs so they all share the same IDs.
pub struct VocalModulePresets {
    pub rescue: ModulePreset,
    pub pitch_correction: ModulePreset,
    pub vocal_tone: ModulePreset,
    pub vocal_chorus: ModulePreset,
    pub vocal_flanger: ModulePreset,
    pub vocal_sends: ModulePreset,
}

impl Default for VocalModulePresets {
    fn default() -> Self {
        Self::new()
    }
}

impl VocalModulePresets {
    /// Build all default vocal module presets.
    pub fn new() -> Self {
        use super::modules::{correction, modulation, rescue, sends, tonal};

        Self {
            rescue: rescue::vocal_rescue(),
            pitch_correction: correction::pitch_correction(),
            vocal_tone: tonal::vocal_tone(),
            vocal_chorus: modulation::vocal_chorus(),
            vocal_flanger: modulation::vocal_flanger(),
            vocal_sends: sends::vocal_sends(),
        }
    }

    /// Collect all module presets into a Vec (for adding to a Rig).
    pub fn all(&self) -> Vec<&ModulePreset> {
        vec![
            &self.rescue,
            &self.pitch_correction,
            &self.vocal_tone,
            &self.vocal_chorus,
            &self.vocal_flanger,
            &self.vocal_sends,
        ]
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Top-level Presets
// ─────────────────────────────────────────────────────────────────────────────

/// "Natural Clean" — Rescue/Standard, Correction/Subtle, Tone/Natural, Sends/Room.
///
/// The starting-point vocal preset. Clean, minimal processing. Good for
/// speaking, soft ballads, and anything where the raw voice should come through.
pub fn natural_clean(m: &VocalModulePresets) -> Preset {
    let mut preset = Preset::new(
        "Natural Clean",
        PresetCategory::Generic {
            base_tone: BaseTone::Natural,
        },
    );
    preset.description =
        Some("Clean natural vocal tone with subtle correction and room verb".into());

    preset.add_module_assignment(assign_snap(&m.rescue, 0, "Standard"));
    preset.add_module_assignment(assign_snap(&m.pitch_correction, 1, "Subtle"));
    preset.add_module_assignment(assign_snap(&m.vocal_tone, 2, "Natural"));
    preset.add_module_assignment(assign_snap(&m.vocal_sends, 4, "Room"));

    preset.add_credit(Credit::new("FTS", "Sound Designer"));
    preset.add_inspiration(
        Inspiration::new("Natural Vocal Chain")
            .with_notes("Minimal processing — let the voice shine"),
    );

    preset
}

/// "Warm Worship" — Rescue/Standard, Correction/Subtle, Tone/Warm, Chorus, Sends/Hall.
///
/// Rich warm vocal with hall reverb for worship contexts.
pub fn warm_worship(m: &VocalModulePresets) -> Preset {
    let mut preset = Preset::new(
        "Warm Worship",
        PresetCategory::Genre {
            base_tone: BaseTone::Warm,
            genre: Genre::Worship,
        },
    );
    preset.description =
        Some("Warm vocal tone with chorus shimmer and hall reverb for worship".into());

    preset.add_module_assignment(assign_snap(&m.rescue, 0, "Standard"));
    preset.add_module_assignment(assign_snap(&m.pitch_correction, 1, "Subtle"));
    preset.add_module_assignment(assign_snap(&m.vocal_tone, 2, "Warm"));
    preset.add_module_assignment(assign(&m.vocal_chorus, 3));
    preset.add_module_assignment(assign_snap(&m.vocal_sends, 4, "Hall"));

    preset.add_credit(Credit::new("FTS", "Sound Designer"));
    preset.add_inspiration(
        Inspiration::new("Modern Worship Vocals")
            .with_source("Elevation Worship, Hillsong")
            .with_notes("Warm, present vocal with ambient hall reverb"),
    );

    preset
}

/// "Bright Pop" — Rescue/Standard, Correction/Standard, Tone/Bright, Sends/Plate.
///
/// Forward-sounding bright vocal for pop music.
pub fn bright_pop(m: &VocalModulePresets) -> Preset {
    let mut preset = Preset::new(
        "Bright Pop",
        PresetCategory::Genre {
            base_tone: BaseTone::Bright,
            genre: Genre::Pop,
        },
    );
    preset.description =
        Some("Bright, forward vocal with plate reverb for pop production".into());

    preset.add_module_assignment(assign_snap(&m.rescue, 0, "Standard"));
    preset.add_module_assignment(assign_snap(&m.pitch_correction, 1, "Standard"));
    preset.add_module_assignment(assign_snap(&m.vocal_tone, 2, "Bright"));
    preset.add_module_assignment(assign_snap(&m.vocal_sends, 4, "Plate"));

    preset.add_credit(Credit::new("FTS", "Sound Designer"));
    preset.add_inspiration(
        Inspiration::new("Modern Pop Vocals")
            .with_source("Charlie Puth, Bruno Mars")
            .with_notes("Bright, in-your-face vocal with tight plate reverb"),
    );

    preset
}

/// "Powerful Rock" — Rescue/Light Touch, Correction/Standard, Tone/Powerful, Sends/Room.
///
/// Aggressive, compressed vocal for rock and high-energy performances.
pub fn powerful_rock(m: &VocalModulePresets) -> Preset {
    let mut preset = Preset::new(
        "Powerful Rock",
        PresetCategory::Genre {
            base_tone: BaseTone::Powerful,
            genre: Genre::Rock,
        },
    );
    preset.description =
        Some("Aggressive, heavily saturated vocal for rock performance".into());

    preset.add_module_assignment(assign_snap(&m.rescue, 0, "Light Touch"));
    preset.add_module_assignment(assign_snap(&m.pitch_correction, 1, "Standard"));
    preset.add_module_assignment(assign_snap(&m.vocal_tone, 2, "Powerful"));
    preset.add_module_assignment(assign_snap(&m.vocal_sends, 4, "Room"));

    preset.add_credit(Credit::new("FTS", "Sound Designer"));
    preset.add_inspiration(
        Inspiration::new("Rock Vocal Energy")
            .with_source("Chris Cornell, Freddie Mercury")
            .with_notes("Heavy compression and saturation for aggressive delivery"),
    );

    preset
}

/// "Breathy Ballad" — Rescue/Light Touch, Correction/Subtle, Tone/Breathy, Sends/Hall.
///
/// Intimate, breathy vocal for ballads and soft passages.
pub fn breathy_ballad(m: &VocalModulePresets) -> Preset {
    let mut preset = Preset::new(
        "Breathy Ballad",
        PresetCategory::Genre {
            base_tone: BaseTone::Breathy,
            genre: Genre::Pop,
        },
    );
    preset.description =
        Some("Intimate breathy vocal for ballads with lush hall reverb".into());

    preset.add_module_assignment(assign_snap(&m.rescue, 0, "Light Touch"));
    preset.add_module_assignment(assign_snap(&m.pitch_correction, 1, "Subtle"));
    preset.add_module_assignment(assign_snap(&m.vocal_tone, 2, "Breathy"));
    preset.add_module_assignment(assign_snap(&m.vocal_sends, 4, "Hall"));

    preset.add_credit(Credit::new("FTS", "Sound Designer"));
    preset.add_inspiration(
        Inspiration::new("Intimate Vocal Style")
            .with_source("Billie Eilish, Adele")
            .with_notes("Minimal compression, preserve breath and dynamics"),
    );

    preset
}
