//! Default vocal songs — Oceans, Thriller, Bohemian Rhapsody.
//!
//! Each function returns a [`PerformanceSong`] with scenes that reference
//! the default presets. Scenes may include module overrides for
//! song-specific tone changes.

use crate::module::ModuleType;
use crate::module_preset::ModuleOverride;
use crate::performance::{PerformanceSong, Scene};
use crate::preset::Preset;

use super::presets::VocalModulePresets;

// ─────────────────────────────────────────────────────────────────────────────
// Oceans (Hillsong United)
// ─────────────────────────────────────────────────────────────────────────────

/// "Oceans" by Hillsong United — 4 scenes for worship vocal.
///
/// Scenes: Verse, Chorus, Bridge, Outro.
pub fn oceans(presets: &OceansPresets, modules: &VocalModulePresets) -> PerformanceSong {
    let mut song = PerformanceSong::new("Oceans");

    // Song-level: lock pitch correction to Subtle
    song.add_module_override(ModuleOverride::swap_preset(
        ModuleType::Correction,
        modules.pitch_correction.id,
        modules
            .pitch_correction
            .snapshot_by_name("Subtle")
            .map(|s| s.id),
    ));

    // 1. Verse — Natural Clean (intimate)
    song.add_scene(Scene::new("Verse", presets.natural_clean.id));

    // 2. Chorus — Warm Worship
    song.add_scene(Scene::new("Chorus", presets.warm_worship.id));

    // 3. Bridge — Warm Worship with spacey sends
    let mut bridge = Scene::new("Bridge", presets.warm_worship.id);
    bridge.add_module_override(ModuleOverride::swap_preset(
        ModuleType::Sends,
        modules.vocal_sends.id,
        modules.vocal_sends.snapshot_by_name("Spacey").map(|s| s.id),
    ));
    song.add_scene(bridge);

    // 4. Outro — Natural Clean
    song.add_scene(Scene::new("Outro", presets.natural_clean.id));

    song
}

// ─────────────────────────────────────────────────────────────────────────────
// Thriller (Dirty Loops vocal version)
// ─────────────────────────────────────────────────────────────────────────────

/// "Thriller" (Dirty Loops version) — 4 scenes for pop/R&B vocal.
///
/// Scenes: Verse, Chorus, Bridge, Outro.
pub fn thriller(
    presets: &ThrillerVocalPresets,
    _modules: &VocalModulePresets,
) -> PerformanceSong {
    let mut song = PerformanceSong::new("Thriller");

    // 1. Verse — Breathy Ballad
    song.add_scene(Scene::new("Verse", presets.breathy_ballad.id));

    // 2. Chorus — Bright Pop
    song.add_scene(Scene::new("Chorus", presets.bright_pop.id));

    // 3. Bridge — Natural Clean
    song.add_scene(Scene::new("Bridge", presets.natural_clean.id));

    // 4. Outro — Bright Pop
    song.add_scene(Scene::new("Outro", presets.bright_pop.id));

    song
}

// ─────────────────────────────────────────────────────────────────────────────
// Bohemian Rhapsody (Queen)
// ─────────────────────────────────────────────────────────────────────────────

/// "Bohemian Rhapsody" by Queen — 5 scenes for a rock vocal showcase.
///
/// Scenes: Ballad, Opera, Hard Rock, Outro Ballad, Finale.
pub fn bohemian_rhapsody(
    presets: &BohemianPresets,
    modules: &VocalModulePresets,
) -> PerformanceSong {
    let mut song = PerformanceSong::new("Bohemian Rhapsody");

    // 1. Ballad — Breathy Ballad
    song.add_scene(Scene::new("Ballad", presets.breathy_ballad.id));

    // 2. Opera — Bright Pop with chorus modulation
    let mut opera = Scene::new("Opera", presets.bright_pop.id);
    opera.add_module_override(ModuleOverride::swap_preset(
        ModuleType::VocalModulation,
        modules.vocal_chorus.id,
        None,
    ));
    song.add_scene(opera);

    // 3. Hard Rock — Powerful Rock
    song.add_scene(Scene::new("Hard Rock", presets.powerful_rock.id));

    // 4. Outro Ballad — Breathy Ballad
    song.add_scene(Scene::new("Outro Ballad", presets.breathy_ballad.id));

    // 5. Finale — Natural Clean with hall sends
    let mut finale = Scene::new("Finale", presets.natural_clean.id);
    finale.add_module_override(ModuleOverride::swap_preset(
        ModuleType::Sends,
        modules.vocal_sends.id,
        modules.vocal_sends.snapshot_by_name("Hall").map(|s| s.id),
    ));
    song.add_scene(finale);

    song
}

// ─────────────────────────────────────────────────────────────────────────────
// Preset reference bundles for song builders
// ─────────────────────────────────────────────────────────────────────────────

/// Preset references needed by Oceans.
pub struct OceansPresets<'a> {
    pub natural_clean: &'a Preset,
    pub warm_worship: &'a Preset,
}

/// Preset references needed by Thriller (vocal version).
pub struct ThrillerVocalPresets<'a> {
    pub breathy_ballad: &'a Preset,
    pub bright_pop: &'a Preset,
    pub natural_clean: &'a Preset,
}

/// Preset references needed by Bohemian Rhapsody.
pub struct BohemianPresets<'a> {
    pub breathy_ballad: &'a Preset,
    pub bright_pop: &'a Preset,
    pub powerful_rock: &'a Preset,
    pub natural_clean: &'a Preset,
}
