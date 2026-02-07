//! Default vocal profiles — Worship, Pop/R&B, Rock.
//!
//! Each function returns a [`Profile`] with scene templates that reference
//! the default presets by ID. Scene templates may include module overrides
//! for context-specific tone changes.

use crate::id::RigId;
use crate::module::ModuleType;
use crate::module_preset::ModuleOverride;
use crate::preset::Preset;
use crate::profile::{Profile, SceneTemplate};

use super::presets::VocalModulePresets;

// ─────────────────────────────────────────────────────────────────────────────
// Worship Profile
// ─────────────────────────────────────────────────────────────────────────────

/// "Worship" vocal profile — 6 scenes for worship contexts.
///
/// Scenes: Verse, Pre-Chorus, Chorus, Bridge, Altar, Outro.
pub fn worship(
    rig_id: RigId,
    presets: &WorshipVocalPresets,
    modules: &VocalModulePresets,
) -> Profile {
    let mut profile = Profile::new("Worship", rig_id);
    profile.description =
        Some("Worship vocal tones — intimate verse to soaring chorus".into());

    // 1. Verse — Natural Clean (quiet, intimate)
    profile.add_scene_template(SceneTemplate::direct("Verse", presets.natural_clean.id));

    // 2. Pre-Chorus — Warm Worship
    profile.add_scene_template(SceneTemplate::direct(
        "Pre-Chorus",
        presets.warm_worship.id,
    ));

    // 3. Chorus — Warm Worship with chorus modulation
    let mut chorus = SceneTemplate::direct("Chorus", presets.warm_worship.id);
    chorus.add_module_override(ModuleOverride::swap_preset(
        ModuleType::VocalModulation,
        modules.vocal_chorus.id,
        None,
    ));
    profile.add_scene_template(chorus);

    // 4. Bridge — Breathy Ballad
    profile.add_scene_template(SceneTemplate::direct(
        "Bridge",
        presets.breathy_ballad.id,
    ));

    // 5. Altar — Natural Clean with Long Delay sends
    let mut altar = SceneTemplate::direct("Altar", presets.natural_clean.id);
    altar.add_module_override(ModuleOverride::swap_preset(
        ModuleType::Sends,
        modules.vocal_sends.id,
        modules.vocal_sends.snapshot_by_name("Long Delay").map(|s| s.id),
    ));
    profile.add_scene_template(altar);

    // 6. Outro — Warm Worship
    profile.add_scene_template(SceneTemplate::direct("Outro", presets.warm_worship.id));

    profile
}

// ─────────────────────────────────────────────────────────────────────────────
// Pop/R&B Profile
// ─────────────────────────────────────────────────────────────────────────────

/// "Pop/R&B" vocal profile — 6 scenes.
///
/// Scenes: Verse, Pre-Chorus, Chorus, Bridge, Ad Lib, Outro.
pub fn pop_rnb(
    rig_id: RigId,
    presets: &PopRnbVocalPresets,
    modules: &VocalModulePresets,
) -> Profile {
    let mut profile = Profile::new("Pop/R&B", rig_id);
    profile.description =
        Some("Pop and R&B vocal tones — breathy verse to bright chorus".into());

    // 1. Verse — Breathy Ballad
    profile.add_scene_template(SceneTemplate::direct(
        "Verse",
        presets.breathy_ballad.id,
    ));

    // 2. Pre-Chorus — Natural Clean
    profile.add_scene_template(SceneTemplate::direct(
        "Pre-Chorus",
        presets.natural_clean.id,
    ));

    // 3. Chorus — Bright Pop
    profile.add_scene_template(SceneTemplate::direct("Chorus", presets.bright_pop.id));

    // 4. Bridge — Warm Worship with slapback delay
    let mut bridge = SceneTemplate::direct("Bridge", presets.warm_worship.id);
    bridge.add_module_override(ModuleOverride::swap_preset(
        ModuleType::Sends,
        modules.vocal_sends.id,
        modules.vocal_sends.snapshot_by_name("Slapback").map(|s| s.id),
    ));
    profile.add_scene_template(bridge);

    // 5. Ad Lib — Bright Pop with spacey sends
    let mut ad_lib = SceneTemplate::direct("Ad Lib", presets.bright_pop.id);
    ad_lib.add_module_override(ModuleOverride::swap_preset(
        ModuleType::Sends,
        modules.vocal_sends.id,
        modules.vocal_sends.snapshot_by_name("Spacey").map(|s| s.id),
    ));
    profile.add_scene_template(ad_lib);

    // 6. Outro — Natural Clean
    profile.add_scene_template(SceneTemplate::direct("Outro", presets.natural_clean.id));

    profile
}

// ─────────────────────────────────────────────────────────────────────────────
// Rock Profile
// ─────────────────────────────────────────────────────────────────────────────

/// "Rock" vocal profile — 6 scenes.
///
/// Scenes: Verse, Pre-Chorus, Chorus, Bridge, Solo, Outro.
pub fn rock(
    rig_id: RigId,
    presets: &RockVocalPresets,
    _modules: &VocalModulePresets,
) -> Profile {
    let mut profile = Profile::new("Rock", rig_id);
    profile.description =
        Some("Rock vocal tones — clean verse to powerful chorus".into());

    // 1. Verse — Natural Clean
    profile.add_scene_template(SceneTemplate::direct("Verse", presets.natural_clean.id));

    // 2. Pre-Chorus — Warm Worship
    profile.add_scene_template(SceneTemplate::direct(
        "Pre-Chorus",
        presets.warm_worship.id,
    ));

    // 3. Chorus — Powerful Rock
    profile.add_scene_template(SceneTemplate::direct("Chorus", presets.powerful_rock.id));

    // 4. Bridge — Breathy Ballad
    profile.add_scene_template(SceneTemplate::direct(
        "Bridge",
        presets.breathy_ballad.id,
    ));

    // 5. Solo — Powerful Rock
    profile.add_scene_template(SceneTemplate::direct("Solo", presets.powerful_rock.id));

    // 6. Outro — Natural Clean
    profile.add_scene_template(SceneTemplate::direct("Outro", presets.natural_clean.id));

    profile
}

// ─────────────────────────────────────────────────────────────────────────────
// Preset reference bundles for profile builders
// ─────────────────────────────────────────────────────────────────────────────

/// Preset references needed by the Worship vocal profile.
pub struct WorshipVocalPresets<'a> {
    pub natural_clean: &'a Preset,
    pub warm_worship: &'a Preset,
    pub breathy_ballad: &'a Preset,
}

/// Preset references needed by the Pop/R&B vocal profile.
pub struct PopRnbVocalPresets<'a> {
    pub breathy_ballad: &'a Preset,
    pub natural_clean: &'a Preset,
    pub bright_pop: &'a Preset,
    pub warm_worship: &'a Preset,
}

/// Preset references needed by the Rock vocal profile.
pub struct RockVocalPresets<'a> {
    pub natural_clean: &'a Preset,
    pub warm_worship: &'a Preset,
    pub powerful_rock: &'a Preset,
    pub breathy_ballad: &'a Preset,
}
