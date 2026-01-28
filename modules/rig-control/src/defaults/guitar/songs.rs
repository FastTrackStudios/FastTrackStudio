//! Default guitar songs — Cryin', Thriller, Girl Goodbye.
//!
//! Each function returns a [`PerformanceSong`] with scenes that reference
//! the default presets. Scenes may include module overrides for
//! song-specific tone changes.

use crate::module::ModuleType;
use crate::module_preset::ModuleOverride;
use crate::performance::{PerformanceSong, Scene};
use crate::preset::Preset;

use super::presets::GuitarModulePresets;

// ─────────────────────────────────────────────────────────────────────────────
// Cryin' (Mateus Asato)
// ─────────────────────────────────────────────────────────────────────────────

/// "Cryin'" by Mateus Asato — 4 scenes.
///
/// Scenes: Ambient, Rhythm, Lead, Solo.
pub fn cryin(presets: &CryinPresets, modules: &GuitarModulePresets) -> PerformanceSong {
    let mut song = PerformanceSong::new("Cryin'");

    // 1. Ambient — AC30 Ambient Clean with reverb + chorus
    let ambient = Scene::new("Ambient", presets.ac30_ambient_clean.id);
    song.add_scene(ambient);

    // 2. Rhythm — Edge of Breakup
    let rhythm = Scene::new("Rhythm", presets.edge_of_breakup.id);
    song.add_scene(rhythm);

    // 3. Lead — 80's Drive
    let lead = Scene::new("Lead", presets.eighties_drive.id);
    song.add_scene(lead);

    // 4. Solo — Stank with delay
    let mut solo = Scene::new("Solo", presets.stank.id);
    solo.add_module_override(ModuleOverride::swap_preset(
        ModuleType::Time,
        modules.delay.id,
        None,
    ));
    song.add_scene(solo);

    song
}

// ─────────────────────────────────────────────────────────────────────────────
// Thriller (Dirty Loops)
// ─────────────────────────────────────────────────────────────────────────────

/// "Thriller" (Dirty Loops version) — 5 scenes.
///
/// Scenes: Crunch, Rock Lead, Bridge, Solo, Ambient.
pub fn thriller(presets: &ThrillerPresets, modules: &GuitarModulePresets) -> PerformanceSong {
    let mut song = PerformanceSong::new("Thriller");

    // 1. Crunch — Edge of Breakup
    let crunch = Scene::new("Crunch", presets.edge_of_breakup.id);
    song.add_scene(crunch);

    // 2. Rock Lead — Stank
    let rock_lead = Scene::new("Rock Lead", presets.stank.id);
    song.add_scene(rock_lead);

    // 3. Bridge — AC30 Ambient Clean with phaser modulation
    let mut bridge = Scene::new("Bridge", presets.ac30_ambient_clean.id);
    bridge.add_module_override(ModuleOverride::swap_preset(
        ModuleType::Modulation,
        modules.phaser.id,
        None,
    ));
    song.add_scene(bridge);

    // 4. Solo — Stank with delay
    let mut solo = Scene::new("Solo", presets.stank.id);
    solo.add_module_override(ModuleOverride::swap_preset(
        ModuleType::Time,
        modules.delay.id,
        None,
    ));
    song.add_scene(solo);

    // 5. Ambient — AC30 Ambient Clean
    let ambient = Scene::new("Ambient", presets.ac30_ambient_clean.id);
    song.add_scene(ambient);

    song
}

// ─────────────────────────────────────────────────────────────────────────────
// Girl Goodbye (Toto)
// ─────────────────────────────────────────────────────────────────────────────

/// "Girl Goodbye" by Toto — 4 scenes.
///
/// Scenes: Drive, Verse, Chorus, Solo.
pub fn girl_goodbye(presets: &GirlGoodbyePresets, modules: &GuitarModulePresets) -> PerformanceSong {
    let mut song = PerformanceSong::new("Girl Goodbye");

    // Song-level module override: lock motion to Tremolo 8th for the whole song
    song.add_module_override(ModuleOverride::swap_preset(
        ModuleType::Motion,
        modules.tremolo_8th.id,
        None,
    ));

    // 1. Drive — 80's Drive
    let drive = Scene::new("Drive", presets.eighties_drive.id);
    song.add_scene(drive);

    // 2. Verse — Edge of Breakup
    let verse = Scene::new("Verse", presets.edge_of_breakup.id);
    song.add_scene(verse);

    // 3. Chorus — Stank
    let chorus = Scene::new("Chorus", presets.stank.id);
    song.add_scene(chorus);

    // 4. Solo — Stank with delay
    let mut solo = Scene::new("Solo", presets.stank.id);
    solo.add_module_override(ModuleOverride::swap_preset(
        ModuleType::Time,
        modules.delay.id,
        None,
    ));
    song.add_scene(solo);

    song
}

// ─────────────────────────────────────────────────────────────────────────────
// Preset reference bundles for song builders
// ─────────────────────────────────────────────────────────────────────────────

/// Preset references needed by Cryin'.
pub struct CryinPresets<'a> {
    pub ac30_ambient_clean: &'a Preset,
    pub edge_of_breakup: &'a Preset,
    pub eighties_drive: &'a Preset,
    pub stank: &'a Preset,
}

/// Preset references needed by Thriller.
pub struct ThrillerPresets<'a> {
    pub ac30_ambient_clean: &'a Preset,
    pub edge_of_breakup: &'a Preset,
    pub stank: &'a Preset,
}

/// Preset references needed by Girl Goodbye.
pub struct GirlGoodbyePresets<'a> {
    pub eighties_drive: &'a Preset,
    pub edge_of_breakup: &'a Preset,
    pub stank: &'a Preset,
}
