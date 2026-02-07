//! Default guitar profiles — Worship, Blues, Rock.
//!
//! Each function returns a [`Profile`] with scene templates that reference
//! the default presets by ID. Scene templates may include module overrides
//! and block overrides on top of the base preset.

use crate::id::RigId;
use crate::module::ModuleType;
use crate::module_preset::ModuleOverride;
use crate::preset::Preset;
use crate::profile::{Profile, SceneTemplate};

use super::presets::GuitarModulePresets;

// ─────────────────────────────────────────────────────────────────────────────
// Worship Profile
// ─────────────────────────────────────────────────────────────────────────────

/// "Worship" profile — 8 scenes for worship contexts.
///
/// Scenes: Clean, Crunch, Drive, Lead, Ambient, Tremolo, Delay, Solo.
pub fn worship(
    rig_id: RigId,
    presets: &WorshipPresets,
    modules: &GuitarModulePresets,
) -> Profile {
    let mut profile = Profile::new("Worship", rig_id);
    profile.description = Some("Worship service guitar tones — ambient cleans to soaring leads".into());

    // 1. Clean — AC30 Ambient Clean with Pre-FX override to Gravity Tank / Spring Reverb Light
    let mut clean = SceneTemplate::direct("Clean", presets.ac30_ambient_clean.id);
    clean.add_module_override(ModuleOverride::swap_preset(
        ModuleType::PreFx,
        modules.gravity_tank.id,
        modules.gravity_tank.snapshot_by_name("Spring Reverb Light").map(|s| s.id),
    ));
    profile.add_scene_template(clean);

    // 2. Crunch — Edge of Breakup
    profile.add_scene_template(SceneTemplate::direct("Crunch", presets.edge_of_breakup.id));

    // 3. Drive — 80's Drive
    profile.add_scene_template(SceneTemplate::direct("Drive", presets.eighties_drive.id));

    // 4. Lead — Stank
    profile.add_scene_template(SceneTemplate::direct("Lead", presets.stank.id));

    // 5. Ambient — AC30 Ambient Clean
    profile.add_scene_template(SceneTemplate::direct("Ambient", presets.ac30_ambient_clean.id));

    // 6. Tremolo — Tremolo Swells
    profile.add_scene_template(SceneTemplate::direct("Tremolo", presets.tremolo_swells.id));

    // 7. Delay — AC30 Ambient Clean with Time module override to Delay
    let mut delay = SceneTemplate::direct("Delay", presets.ac30_ambient_clean.id);
    delay.add_module_override(ModuleOverride::swap_preset(
        ModuleType::Time,
        modules.delay.id,
        None,
    ));
    profile.add_scene_template(delay);

    // 8. Solo — Edge of Breakup (higher gain variant)
    profile.add_scene_template(SceneTemplate::direct("Solo", presets.edge_of_breakup.id));

    profile
}

// ─────────────────────────────────────────────────────────────────────────────
// Blues Profile
// ─────────────────────────────────────────────────────────────────────────────

/// "Blues" profile — 8 scenes for blues contexts.
///
/// Scenes: Clean, Crunch, Drive, Lead, Funk, Q-Tron, Solo, Ambient.
pub fn blues(
    rig_id: RigId,
    presets: &BluesPresets,
    modules: &GuitarModulePresets,
) -> Profile {
    let mut profile = Profile::new("Blues", rig_id);
    profile.description = Some("Blues guitar tones — clean sparkle to singing leads".into());

    // 1. Clean — Edge of Breakup (Dumble ultra-clean)
    let mut clean = SceneTemplate::direct("Clean", presets.edge_of_breakup.id);
    clean.add_module_override(ModuleOverride::swap_preset(
        ModuleType::Amp,
        modules.dumble_two_rock.id,
        modules.dumble_two_rock.snapshot_by_name("Ultra-Clean").map(|s| s.id),
    ));
    profile.add_scene_template(clean);

    // 2. Crunch — Edge of Breakup
    profile.add_scene_template(SceneTemplate::direct("Crunch", presets.edge_of_breakup.id));

    // 3. Drive — 80's Drive
    profile.add_scene_template(SceneTemplate::direct("Drive", presets.eighties_drive.id));

    // 4. Lead — Stank (lower gain for blues lead)
    profile.add_scene_template(SceneTemplate::direct("Lead", presets.stank.id));

    // 5. Funk — Edge of Breakup with Special module override to Envelope Filter
    let mut funk = SceneTemplate::direct("Funk", presets.edge_of_breakup.id);
    funk.add_module_override(ModuleOverride::swap_preset(
        ModuleType::Special,
        modules.envelope_filter.id,
        None,
    ));
    profile.add_scene_template(funk);

    // 6. Q-Tron — Edge of Breakup with Wah Pedal
    let mut qtron = SceneTemplate::direct("Q-Tron", presets.edge_of_breakup.id);
    qtron.add_module_override(ModuleOverride::swap_preset(
        ModuleType::Special,
        modules.wah_pedal.id,
        None,
    ));
    profile.add_scene_template(qtron);

    // 7. Solo — Stank
    profile.add_scene_template(SceneTemplate::direct("Solo", presets.stank.id));

    // 8. Ambient — AC30 Ambient Clean
    profile.add_scene_template(SceneTemplate::direct("Ambient", presets.ac30_ambient_clean.id));

    profile
}

// ─────────────────────────────────────────────────────────────────────────────
// Rock Profile
// ─────────────────────────────────────────────────────────────────────────────

/// "Rock" profile — 8 scenes for rock contexts.
///
/// Scenes: Clean, Crunch, Drive, Lead, Ambient, Phaser, DLY Lead, Solo.
pub fn rock(
    rig_id: RigId,
    presets: &RockPresets,
    modules: &GuitarModulePresets,
) -> Profile {
    let mut profile = Profile::new("Rock", rig_id);
    profile.description = Some("Rock guitar tones — power cleans to searing solos".into());

    // 1. Clean — AC30 Ambient Clean
    profile.add_scene_template(SceneTemplate::direct("Clean", presets.ac30_ambient_clean.id));

    // 2. Crunch — 80's Drive
    profile.add_scene_template(SceneTemplate::direct("Crunch", presets.eighties_drive.id));

    // 3. Drive — Stank
    profile.add_scene_template(SceneTemplate::direct("Drive", presets.stank.id));

    // 4. Lead — Stank
    profile.add_scene_template(SceneTemplate::direct("Lead", presets.stank.id));

    // 5. Ambient — AC30 Ambient Clean with Modulation=Chorus
    let mut ambient = SceneTemplate::direct("Ambient", presets.ac30_ambient_clean.id);
    ambient.add_module_override(ModuleOverride::swap_preset(
        ModuleType::Modulation,
        modules.chorus.id,
        None,
    ));
    profile.add_scene_template(ambient);

    // 6. Phaser — 80's Drive with Modulation=Phaser
    let mut phaser = SceneTemplate::direct("Phaser", presets.eighties_drive.id);
    phaser.add_module_override(ModuleOverride::swap_preset(
        ModuleType::Modulation,
        modules.phaser.id,
        None,
    ));
    profile.add_scene_template(phaser);

    // 7. DLY Lead — Stank with Time=Delay
    let mut dly_lead = SceneTemplate::direct("DLY Lead", presets.stank.id);
    dly_lead.add_module_override(ModuleOverride::swap_preset(
        ModuleType::Time,
        modules.delay.id,
        None,
    ));
    profile.add_scene_template(dly_lead);

    // 8. Solo — Stank
    profile.add_scene_template(SceneTemplate::direct("Solo", presets.stank.id));

    profile
}

// ─────────────────────────────────────────────────────────────────────────────
// Preset reference bundles for profile builders
// ─────────────────────────────────────────────────────────────────────────────

/// Preset references needed by the Worship profile.
pub struct WorshipPresets<'a> {
    pub ac30_ambient_clean: &'a Preset,
    pub tremolo_swells: &'a Preset,
    pub eighties_drive: &'a Preset,
    pub stank: &'a Preset,
    pub edge_of_breakup: &'a Preset,
}

/// Preset references needed by the Blues profile.
pub struct BluesPresets<'a> {
    pub ac30_ambient_clean: &'a Preset,
    pub edge_of_breakup: &'a Preset,
    pub eighties_drive: &'a Preset,
    pub stank: &'a Preset,
}

/// Preset references needed by the Rock profile.
pub struct RockPresets<'a> {
    pub ac30_ambient_clean: &'a Preset,
    pub eighties_drive: &'a Preset,
    pub stank: &'a Preset,
}
