//! Typestate selection types — make invalid selection states unrepresentable.
//!
//! These types encode the domain rule that certain selections require
//! sub-selections:
//!
//! - A **Preset** is just a container of **Snapshots**. You can't use a preset
//!   without selecting a snapshot.
//! - A **Profile** is a container of **Scenes** (scene templates). You can't
//!   use a profile without selecting a scene.
//! - A **Song** is a sequence of **Scenes**. You can't use a song without
//!   being in a scene.
//! - A **Scene** always references a **Preset + Snapshot** pair.
//!
//! # Typestate Pattern
//!
//! Each selection type uses `PhantomData<State>` to encode the selection level
//! at compile time. Methods that require a child selection are only available
//! on the "resolved" state, making it a compile error to access them before
//! the child is selected.
//!
//! ```ignore
//! // Compile error: no method `snapshot` on ActivePreset<Unresolved>
//! let preset = ActivePreset::new(preset_data);
//! let snap = preset.snapshot(); // ERROR
//!
//! // Works: resolve first, then access
//! let preset = ActivePreset::new(preset_data).resolve(snapshot_id)?;
//! let snap = preset.snapshot(); // OK — returns &Snapshot
//! ```

use std::marker::PhantomData;

use uuid::Uuid;

use super::performance::Scene;
use super::preset::{Preset, Snapshot};
use super::profile::{Profile, SceneTemplate};

// ============================================================================
// State Markers
// ============================================================================

/// Marker: child selection has NOT been made yet.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Unresolved;

/// Marker: child selection HAS been made.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Resolved;

// ============================================================================
// ActivePreset — Preset with required Snapshot selection
// ============================================================================

/// A preset selection that enforces snapshot selection via typestate.
///
/// - `ActivePreset<Unresolved>`: Preset loaded, no snapshot chosen yet.
///   Only exposes preset metadata. Cannot be used for playback.
/// - `ActivePreset<Resolved>`: Preset loaded AND snapshot selected.
///   Full access to the resolved snapshot.
///
/// # Domain Rule
/// "You can't select a preset without selecting a snapshot —
///  presets are just containers of snapshots."
#[derive(Debug, Clone)]
pub struct ActivePreset<State = Unresolved> {
    preset: Preset,
    snapshot: Option<Snapshot>,
    snapshot_id: Option<Uuid>,
    _state: PhantomData<State>,
}

impl ActivePreset<Unresolved> {
    /// Create a new unresolved preset selection.
    ///
    /// At this point only preset-level metadata is accessible.
    /// Call [`resolve`] to select a snapshot and transition to `Resolved`.
    pub fn new(preset: Preset) -> Self {
        Self {
            preset,
            snapshot: None,
            snapshot_id: None,
            _state: PhantomData,
        }
    }

    /// Resolve this preset by selecting a snapshot.
    ///
    /// If `snapshot_id` is found in the preset's snapshots, transitions to
    /// `ActivePreset<Resolved>`. Returns `None` if the snapshot doesn't exist.
    pub fn resolve(self, snapshot_id: Uuid) -> Option<ActivePreset<Resolved>> {
        let snapshot = self.preset.get_snapshot(snapshot_id)?.clone();
        Some(ActivePreset {
            preset: self.preset,
            snapshot: Some(snapshot),
            snapshot_id: Some(snapshot_id),
            _state: PhantomData,
        })
    }

    /// Resolve using the preset's default snapshot.
    ///
    /// Returns `None` if the preset has no default snapshot or no snapshots at all.
    pub fn resolve_default(self) -> Option<ActivePreset<Resolved>> {
        let default_id = self.preset.default_snapshot_id?;
        self.resolve(default_id)
    }

    /// Access preset metadata (available in any state).
    pub fn preset(&self) -> &Preset {
        &self.preset
    }

    /// List available snapshots (for UI selection).
    pub fn available_snapshots(&self) -> &[Snapshot] {
        &self.preset.snapshots
    }
}

impl ActivePreset<Resolved> {
    /// The currently selected snapshot.
    ///
    /// This is only available after resolution — the typestate guarantees
    /// a snapshot has been selected.
    pub fn snapshot(&self) -> &Snapshot {
        // SAFETY: Resolved state guarantees snapshot is Some
        self.snapshot.as_ref().expect("Resolved state guarantees snapshot is set")
    }

    /// The selected snapshot's ID.
    pub fn snapshot_id(&self) -> Uuid {
        self.snapshot_id.expect("Resolved state guarantees snapshot_id is set")
    }

    /// Access preset metadata (available in any state).
    pub fn preset(&self) -> &Preset {
        &self.preset
    }

    /// List available snapshots (for UI to switch).
    pub fn available_snapshots(&self) -> &[Snapshot] {
        &self.preset.snapshots
    }

    /// Switch to a different snapshot within the same preset.
    ///
    /// Returns `None` if the snapshot doesn't exist.
    /// The result stays in `Resolved` state.
    pub fn switch_snapshot(self, snapshot_id: Uuid) -> Option<ActivePreset<Resolved>> {
        let snapshot = self.preset.get_snapshot(snapshot_id)?.clone();
        Some(ActivePreset {
            preset: self.preset,
            snapshot: Some(snapshot),
            snapshot_id: Some(snapshot_id),
            _state: PhantomData,
        })
    }

    /// Unresolve — go back to just having a preset selected without a snapshot.
    /// Useful when the snapshot is removed or invalidated.
    pub fn unresolve(self) -> ActivePreset<Unresolved> {
        ActivePreset {
            preset: self.preset,
            snapshot: None,
            snapshot_id: None,
            _state: PhantomData,
        }
    }
}

// ============================================================================
// ActiveScene — Scene with enforced Preset+Snapshot backing
// ============================================================================

/// A scene selection that enforces its preset+snapshot backing via typestate.
///
/// - `ActiveScene<Unresolved>`: Scene loaded, but its preset/snapshot not yet resolved.
/// - `ActiveScene<Resolved>`: Scene loaded AND its preset+snapshot are resolved.
///
/// # Domain Rule
/// "If a section selects a scene, that scene must have an underlying snapshot."
#[derive(Debug, Clone)]
pub struct ActiveScene<State = Unresolved> {
    scene: Scene,
    active_preset: Option<ActivePreset<Resolved>>,
    _state: PhantomData<State>,
}

impl ActiveScene<Unresolved> {
    /// Create a new unresolved scene selection.
    pub fn new(scene: Scene) -> Self {
        Self {
            scene,
            active_preset: None,
            _state: PhantomData,
        }
    }

    /// Resolve this scene by providing its backing preset with a resolved snapshot.
    ///
    /// The preset must already be resolved (snapshot selected), enforcing the
    /// rule that a scene always has an underlying snapshot.
    pub fn resolve(self, active_preset: ActivePreset<Resolved>) -> ActiveScene<Resolved> {
        ActiveScene {
            scene: self.scene,
            active_preset: Some(active_preset),
            _state: PhantomData,
        }
    }

    /// Resolve by looking up the scene's preset_id and snapshot_id in a preset.
    ///
    /// Convenience method that creates the `ActivePreset<Resolved>` from a raw `Preset`.
    pub fn resolve_from_preset(self, preset: Preset) -> Option<ActiveScene<Resolved>> {
        let unresolved = ActivePreset::new(preset);
        let resolved = if let Some(snap_id) = self.scene.snapshot_id {
            unresolved.resolve(snap_id)?
        } else {
            unresolved.resolve_default()?
        };
        Some(self.resolve(resolved))
    }

    /// Access scene metadata.
    pub fn scene(&self) -> &Scene {
        &self.scene
    }
}

impl ActiveScene<Resolved> {
    /// The scene data.
    pub fn scene(&self) -> &Scene {
        &self.scene
    }

    /// The resolved preset+snapshot backing this scene.
    ///
    /// Typestate guarantees this is always available in `Resolved` state.
    pub fn active_preset(&self) -> &ActivePreset<Resolved> {
        self.active_preset.as_ref().expect("Resolved state guarantees active_preset is set")
    }

    /// Convenience: the snapshot for this scene.
    pub fn snapshot(&self) -> &Snapshot {
        self.active_preset().snapshot()
    }

    /// Convenience: the preset for this scene.
    pub fn preset(&self) -> &Preset {
        self.active_preset().preset()
    }

    /// Unresolve — go back to just having a scene without its backing.
    pub fn unresolve(self) -> ActiveScene<Unresolved> {
        ActiveScene {
            scene: self.scene,
            active_preset: None,
            _state: PhantomData,
        }
    }
}

// ============================================================================
// ActiveProfile — Profile with required Scene selection
// ============================================================================

/// A profile selection that enforces scene selection via typestate.
///
/// - `ActiveProfile<Unresolved>`: Profile loaded, no scene chosen yet.
/// - `ActiveProfile<Resolved>`: Profile loaded AND scene selected.
///
/// # Domain Rule
/// "You can't use a profile without selecting a scene."
#[derive(Debug, Clone)]
pub struct ActiveProfile<State = Unresolved> {
    profile: Profile,
    scene_template: Option<SceneTemplate>,
    scene_template_id: Option<Uuid>,
    _state: PhantomData<State>,
}

impl ActiveProfile<Unresolved> {
    /// Create a new unresolved profile selection.
    pub fn new(profile: Profile) -> Self {
        Self {
            profile,
            scene_template: None,
            scene_template_id: None,
            _state: PhantomData,
        }
    }

    /// Resolve this profile by selecting a scene template.
    ///
    /// Returns `None` if the scene template doesn't exist in this profile.
    pub fn resolve(self, scene_template_id: Uuid) -> Option<ActiveProfile<Resolved>> {
        let template = self.profile.get_scene_template(scene_template_id)?.clone();
        Some(ActiveProfile {
            profile: self.profile,
            scene_template: Some(template),
            scene_template_id: Some(scene_template_id),
            _state: PhantomData,
        })
    }

    /// Resolve using the profile's default scene template.
    pub fn resolve_default(self) -> Option<ActiveProfile<Resolved>> {
        let default_id = self.profile.default_scene_template_id?;
        self.resolve(default_id)
    }

    /// Access profile metadata.
    pub fn profile(&self) -> &Profile {
        &self.profile
    }

    /// List available scene templates (for UI selection).
    pub fn available_scenes(&self) -> &[SceneTemplate] {
        &self.profile.scene_templates
    }
}

impl ActiveProfile<Resolved> {
    /// The currently selected scene template.
    pub fn scene_template(&self) -> &SceneTemplate {
        self.scene_template.as_ref().expect("Resolved state guarantees scene_template is set")
    }

    /// The selected scene template's ID.
    pub fn scene_template_id(&self) -> Uuid {
        self.scene_template_id.expect("Resolved state guarantees scene_template_id is set")
    }

    /// Access profile metadata.
    pub fn profile(&self) -> &Profile {
        &self.profile
    }

    /// List available scene templates (for UI to switch).
    pub fn available_scenes(&self) -> &[SceneTemplate] {
        &self.profile.scene_templates
    }

    /// Switch to a different scene template within the same profile.
    pub fn switch_scene(self, scene_template_id: Uuid) -> Option<ActiveProfile<Resolved>> {
        let template = self.profile.get_scene_template(scene_template_id)?.clone();
        Some(ActiveProfile {
            profile: self.profile,
            scene_template: Some(template),
            scene_template_id: Some(scene_template_id),
            _state: PhantomData,
        })
    }

    /// Unresolve — go back to just having a profile without a scene.
    pub fn unresolve(self) -> ActiveProfile<Unresolved> {
        ActiveProfile {
            profile: self.profile,
            scene_template: None,
            scene_template_id: None,
            _state: PhantomData,
        }
    }
}

// ============================================================================
// ActiveSong — Song (PerformanceSong) with required Scene selection
// ============================================================================

/// A song selection that enforces scene selection via typestate.
///
/// - `ActiveSong<Unresolved>`: Song loaded, no scene active.
/// - `ActiveSong<Resolved>`: Song loaded AND positioned at a scene
///   with a fully resolved preset+snapshot.
///
/// # Domain Rule
/// "You can't use a song without selecting a section (scene)."
#[derive(Debug, Clone)]
pub struct ActiveSong<State = Unresolved> {
    song: super::performance::PerformanceSong,
    scene_index: Option<usize>,
    active_scene: Option<ActiveScene<Resolved>>,
    _state: PhantomData<State>,
}

impl ActiveSong<Unresolved> {
    /// Create a new unresolved song selection.
    pub fn new(song: super::performance::PerformanceSong) -> Self {
        Self {
            song,
            scene_index: None,
            active_scene: None,
            _state: PhantomData,
        }
    }

    /// Resolve this song by selecting a scene index and providing the
    /// resolved scene (with its preset+snapshot backing).
    ///
    /// Returns `None` if the scene index is out of bounds.
    pub fn resolve(
        self,
        scene_index: usize,
        active_scene: ActiveScene<Resolved>,
    ) -> Option<ActiveSong<Resolved>> {
        if scene_index >= self.song.scenes.len() {
            return None;
        }
        Some(ActiveSong {
            song: self.song,
            scene_index: Some(scene_index),
            active_scene: Some(active_scene),
            _state: PhantomData,
        })
    }

    /// Access song metadata.
    pub fn song(&self) -> &super::performance::PerformanceSong {
        &self.song
    }

    /// List scenes in this song (for UI selection).
    pub fn available_scenes(&self) -> &[Scene] {
        &self.song.scenes
    }
}

impl ActiveSong<Resolved> {
    /// The song data.
    pub fn song(&self) -> &super::performance::PerformanceSong {
        &self.song
    }

    /// The currently active scene index.
    pub fn scene_index(&self) -> usize {
        self.scene_index.expect("Resolved state guarantees scene_index is set")
    }

    /// The currently active scene (with resolved preset+snapshot).
    pub fn active_scene(&self) -> &ActiveScene<Resolved> {
        self.active_scene.as_ref().expect("Resolved state guarantees active_scene is set")
    }

    /// Convenience: the current scene data.
    pub fn scene(&self) -> &Scene {
        self.active_scene().scene()
    }

    /// Convenience: the current snapshot.
    pub fn snapshot(&self) -> &Snapshot {
        self.active_scene().snapshot()
    }

    /// Convenience: the current preset.
    pub fn preset(&self) -> &Preset {
        self.active_scene().preset()
    }

    /// List scenes in this song (for UI navigation).
    pub fn available_scenes(&self) -> &[Scene] {
        &self.song.scenes
    }

    /// Navigate to a different scene within the same song.
    ///
    /// Requires providing the resolved scene for the new index.
    pub fn go_to_scene(
        self,
        scene_index: usize,
        active_scene: ActiveScene<Resolved>,
    ) -> Option<ActiveSong<Resolved>> {
        if scene_index >= self.song.scenes.len() {
            return None;
        }
        Some(ActiveSong {
            song: self.song,
            scene_index: Some(scene_index),
            active_scene: Some(active_scene),
            _state: PhantomData,
        })
    }

    /// Unresolve — go back to just having a song without an active scene.
    pub fn unresolve(self) -> ActiveSong<Unresolved> {
        ActiveSong {
            song: self.song,
            scene_index: None,
            active_scene: None,
            _state: PhantomData,
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rig::core::category::PresetCategory;
    use crate::rig::core::performance::{PerformanceSong, Scene, SceneTransition};
    use crate::rig::core::preset::{Preset, Snapshot};
    use crate::rig::core::profile::{Profile, SceneTemplate};

    fn test_preset_with_snapshots() -> Preset {
        let mut preset = Preset::new("Test Preset", PresetCategory::default());
        preset.add_snapshot(Snapshot::verse());
        preset.add_snapshot(Snapshot::chorus());
        preset
    }

    fn test_profile_with_scenes() -> Profile {
        let rig_id = Uuid::new_v4();
        let mut profile = Profile::new("Test Profile", rig_id);
        let preset_id = Uuid::new_v4();
        profile.add_scene_template(SceneTemplate::direct("Clean", preset_id));
        profile.add_scene_template(SceneTemplate::direct("Lead", preset_id));
        profile
    }

    // ── Preset → Snapshot ─────────────────────────────────────────────

    #[test]
    fn preset_unresolved_cannot_access_snapshot() {
        let preset = test_preset_with_snapshots();
        let active = ActivePreset::new(preset);

        // Can access preset metadata
        assert_eq!(active.preset().name, "Test Preset");
        assert_eq!(active.available_snapshots().len(), 2);

        // But NOT snapshot — the method doesn't exist on Unresolved.
        // (This is enforced at compile time, not runtime.)
    }

    #[test]
    fn preset_resolves_with_valid_snapshot() {
        let preset = test_preset_with_snapshots();
        let snap_id = preset.snapshots[0].id;
        let active = ActivePreset::new(preset).resolve(snap_id);

        assert!(active.is_some());
        let resolved = active.unwrap();
        assert_eq!(resolved.snapshot().name, "Verse");
        assert_eq!(resolved.snapshot_id(), snap_id);
    }

    #[test]
    fn preset_fails_to_resolve_with_invalid_snapshot() {
        let preset = test_preset_with_snapshots();
        let bogus_id = Uuid::new_v4();
        let active = ActivePreset::new(preset).resolve(bogus_id);

        assert!(active.is_none());
    }

    #[test]
    fn preset_resolves_default_snapshot() {
        let preset = test_preset_with_snapshots();
        let active = ActivePreset::new(preset).resolve_default();

        assert!(active.is_some());
        let resolved = active.unwrap();
        assert_eq!(resolved.snapshot().name, "Verse"); // first snapshot becomes default
    }

    #[test]
    fn preset_can_switch_snapshot() {
        let preset = test_preset_with_snapshots();
        let snap_a = preset.snapshots[0].id;
        let snap_b = preset.snapshots[1].id;

        let resolved = ActivePreset::new(preset).resolve(snap_a).unwrap();
        assert_eq!(resolved.snapshot().name, "Verse");

        let switched = resolved.switch_snapshot(snap_b).unwrap();
        assert_eq!(switched.snapshot().name, "Chorus");
    }

    #[test]
    fn preset_can_unresolve() {
        let preset = test_preset_with_snapshots();
        let snap_id = preset.snapshots[0].id;
        let resolved = ActivePreset::new(preset).resolve(snap_id).unwrap();
        let unresolved = resolved.unresolve();

        // Back to unresolved — can only access preset metadata
        assert_eq!(unresolved.preset().name, "Test Preset");
    }

    // ── Profile → Scene ───────────────────────────────────────────────

    #[test]
    fn profile_unresolved_cannot_access_scene() {
        let profile = test_profile_with_scenes();
        let active = ActiveProfile::new(profile);

        assert_eq!(active.profile().name, "Test Profile");
        assert_eq!(active.available_scenes().len(), 2);
    }

    #[test]
    fn profile_resolves_with_valid_scene() {
        let profile = test_profile_with_scenes();
        let scene_id = profile.scene_templates[0].id;
        let active = ActiveProfile::new(profile).resolve(scene_id);

        assert!(active.is_some());
        let resolved = active.unwrap();
        assert_eq!(resolved.scene_template().name, "Clean");
    }

    #[test]
    fn profile_fails_to_resolve_with_invalid_scene() {
        let profile = test_profile_with_scenes();
        let bogus_id = Uuid::new_v4();
        let active = ActiveProfile::new(profile).resolve(bogus_id);

        assert!(active.is_none());
    }

    #[test]
    fn profile_resolves_default_scene() {
        let profile = test_profile_with_scenes();
        let active = ActiveProfile::new(profile).resolve_default();

        assert!(active.is_some());
        assert_eq!(active.unwrap().scene_template().name, "Clean");
    }

    #[test]
    fn profile_can_switch_scene() {
        let profile = test_profile_with_scenes();
        let scene_a = profile.scene_templates[0].id;
        let scene_b = profile.scene_templates[1].id;

        let resolved = ActiveProfile::new(profile).resolve(scene_a).unwrap();
        assert_eq!(resolved.scene_template().name, "Clean");

        let switched = resolved.switch_scene(scene_b).unwrap();
        assert_eq!(switched.scene_template().name, "Lead");
    }

    // ── Scene → Preset+Snapshot ───────────────────────────────────────

    #[test]
    fn scene_resolves_with_preset_and_snapshot() {
        let preset = test_preset_with_snapshots();
        let snap_id = preset.snapshots[0].id;
        let scene = Scene::new("Verse", preset.id).with_snapshot(snap_id);

        let active = ActiveScene::new(scene);
        assert_eq!(active.scene().name, "Verse");

        let resolved_preset = ActivePreset::new(preset).resolve(snap_id).unwrap();
        let resolved_scene = active.resolve(resolved_preset);

        assert_eq!(resolved_scene.scene().name, "Verse");
        assert_eq!(resolved_scene.snapshot().name, "Verse");
        assert_eq!(resolved_scene.preset().name, "Test Preset");
    }

    #[test]
    fn scene_resolves_from_preset_directly() {
        let preset = test_preset_with_snapshots();
        let snap_id = preset.snapshots[1].id;
        let scene = Scene::new("Chorus", preset.id).with_snapshot(snap_id);

        let resolved = ActiveScene::new(scene).resolve_from_preset(preset);

        assert!(resolved.is_some());
        let resolved = resolved.unwrap();
        assert_eq!(resolved.snapshot().name, "Chorus");
    }

    // ── Song → Scene ──────────────────────────────────────────────────

    #[test]
    fn song_resolves_with_scene() {
        let preset = test_preset_with_snapshots();
        let snap_id = preset.snapshots[0].id;
        let scene = Scene::new("Verse", preset.id).with_snapshot(snap_id);

        let mut song = PerformanceSong::new("Amazing Grace");
        song.add_scene(scene.clone());

        let active_scene = ActiveScene::new(scene)
            .resolve_from_preset(preset)
            .unwrap();

        let active_song = ActiveSong::new(song).resolve(0, active_scene);
        assert!(active_song.is_some());

        let resolved = active_song.unwrap();
        assert_eq!(resolved.song().name, "Amazing Grace");
        assert_eq!(resolved.scene_index(), 0);
        assert_eq!(resolved.scene().name, "Verse");
        assert_eq!(resolved.snapshot().name, "Verse");
    }

    #[test]
    fn song_fails_to_resolve_with_out_of_bounds_scene() {
        let song = PerformanceSong::new("Empty Song");
        let preset = test_preset_with_snapshots();
        let snap_id = preset.snapshots[0].id;
        let scene = Scene::new("Phantom", preset.id).with_snapshot(snap_id);
        let active_scene = ActiveScene::new(scene)
            .resolve_from_preset(preset)
            .unwrap();

        let active_song = ActiveSong::new(song).resolve(0, active_scene);
        assert!(active_song.is_none()); // no scenes in song, index 0 is out of bounds
    }

    #[test]
    fn song_can_navigate_scenes() {
        let preset = test_preset_with_snapshots();
        let snap_verse = preset.snapshots[0].id;
        let snap_chorus = preset.snapshots[1].id;

        let verse_scene = Scene::new("Verse", preset.id).with_snapshot(snap_verse);
        let chorus_scene = Scene::new("Chorus", preset.id).with_snapshot(snap_chorus);

        let mut song = PerformanceSong::new("Amazing Grace");
        song.add_scene(verse_scene.clone());
        song.add_scene(chorus_scene.clone());

        // Start at verse
        let active_verse = ActiveScene::new(verse_scene)
            .resolve_from_preset(preset.clone())
            .unwrap();
        let active_song = ActiveSong::new(song).resolve(0, active_verse).unwrap();
        assert_eq!(active_song.scene().name, "Verse");

        // Navigate to chorus
        let active_chorus = ActiveScene::new(chorus_scene)
            .resolve_from_preset(preset)
            .unwrap();
        let active_song = active_song.go_to_scene(1, active_chorus).unwrap();
        assert_eq!(active_song.scene().name, "Chorus");
        assert_eq!(active_song.scene_index(), 1);
    }

    // ── End-to-end: Song → Scene → Preset → Snapshot ─────────────────

    #[test]
    fn full_hierarchy_enforces_all_selections() {
        // Setup: Preset with snapshots
        let mut preset = Preset::new("Worship Lead", PresetCategory::default());
        let verse_snap = Snapshot::verse();
        let chorus_snap = Snapshot::chorus();
        let verse_snap_id = verse_snap.id;
        let chorus_snap_id = chorus_snap.id;
        preset.add_snapshot(verse_snap);
        preset.add_snapshot(chorus_snap);

        // Setup: Scenes referencing preset+snapshots
        let verse_scene = Scene::new("Verse", preset.id).with_snapshot(verse_snap_id);
        let chorus_scene = Scene::new("Chorus", preset.id).with_snapshot(chorus_snap_id);

        // Setup: Song with scenes
        let mut song = PerformanceSong::new("Amazing Grace");
        song.add_scene(verse_scene.clone());
        song.add_scene(chorus_scene);

        // Step 1: Load song (unresolved)
        let song_selection = ActiveSong::new(song);
        assert_eq!(song_selection.song().name, "Amazing Grace");

        // Step 2: Resolve scene 0 → needs preset+snapshot
        let scene_selection = ActiveScene::new(verse_scene)
            .resolve_from_preset(preset)
            .unwrap();

        // Step 3: Resolve song with resolved scene
        let song_selection = song_selection.resolve(0, scene_selection).unwrap();

        // Now the entire chain is accessible:
        assert_eq!(song_selection.song().name, "Amazing Grace");
        assert_eq!(song_selection.scene().name, "Verse");
        assert_eq!(song_selection.preset().name, "Worship Lead");
        assert_eq!(song_selection.snapshot().name, "Verse");
    }
}
