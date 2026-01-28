//! Typestate selection — compile-time enforcement of hierarchical resolution.
//!
//! Each "Active" wrapper uses a phantom type parameter to track whether a child
//! selection has been made:
//!
//! - [`Unresolved`] — the entity is selected but no child has been picked yet.
//! - [`Resolved`] — a child selection exists and can be accessed safely.
//!
//! The type system prevents accessing child data before it is resolved. Calling
//! `.snapshot()` on an `ActivePreset<Unresolved>` is a **compile error**, not a
//! runtime panic.
//!
//! # Hierarchy
//!
//! ```text
//! ActiveSong<R>
//!   └─ ActiveScene<R>
//!        └─ ActivePreset<R>
//!             └─ Snapshot
//!
//! ActiveProfile<R>
//!   └─ SceneTemplate
//! ```

use std::marker::PhantomData;

use crate::id::{SceneTemplateId, SnapshotId};
use crate::performance::{PerformanceSong, Scene};
use crate::preset::{Preset, Snapshot};
use crate::profile::{Profile, SceneTemplate};

// ─────────────────────────────────────────────────────────────────────────────
// State markers
// ─────────────────────────────────────────────────────────────────────────────

/// Typestate marker: child selection has **not** been made.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Unresolved;

/// Typestate marker: child selection **has** been made.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Resolved;

// ─────────────────────────────────────────────────────────────────────────────
// ActivePreset
// ─────────────────────────────────────────────────────────────────────────────

/// A preset that is currently active, parameterized by resolution state.
///
/// - `ActivePreset<Unresolved>` — preset chosen, snapshot not yet selected.
/// - `ActivePreset<Resolved>` — preset chosen **and** a snapshot is selected.
#[derive(Debug, Clone)]
pub struct ActivePreset<State> {
    preset: Preset,
    snapshot: Option<Snapshot>,
    snapshot_id: Option<SnapshotId>,
    _state: PhantomData<State>,
}

impl ActivePreset<Unresolved> {
    /// Create a new unresolved active preset.
    pub fn new(preset: Preset) -> Self {
        Self {
            preset,
            snapshot: None,
            snapshot_id: None,
            _state: PhantomData,
        }
    }

    /// Resolve by selecting a snapshot by ID.
    ///
    /// Returns `None` if no snapshot with the given ID exists in the preset.
    pub fn resolve(self, snapshot_id: SnapshotId) -> Option<ActivePreset<Resolved>> {
        let snapshot = self
            .preset
            .snapshots
            .iter()
            .find(|s| s.id == snapshot_id)
            .cloned()?;
        Some(ActivePreset {
            preset: self.preset,
            snapshot: Some(snapshot),
            snapshot_id: Some(snapshot_id),
            _state: PhantomData,
        })
    }

    /// Resolve by selecting the first (default) snapshot.
    ///
    /// Returns `None` if the preset has no snapshots.
    pub fn resolve_default(self) -> Option<ActivePreset<Resolved>> {
        let first = self.preset.snapshots.first().cloned()?;
        let id = first.id;
        Some(ActivePreset {
            preset: self.preset,
            snapshot: Some(first),
            snapshot_id: Some(id),
            _state: PhantomData,
        })
    }

    /// Access the underlying preset.
    pub fn preset(&self) -> &Preset {
        &self.preset
    }

    /// List all snapshots available in this preset.
    pub fn available_snapshots(&self) -> &[Snapshot] {
        &self.preset.snapshots
    }
}

impl ActivePreset<Resolved> {
    /// Access the resolved snapshot.
    ///
    /// # Panics
    ///
    /// Panics if the internal snapshot is `None`. The typestate guarantees this
    /// cannot happen when constructed through the public API.
    pub fn snapshot(&self) -> &Snapshot {
        self.snapshot
            .as_ref()
            .expect("Resolved ActivePreset must have a snapshot")
    }

    /// Get the ID of the resolved snapshot.
    ///
    /// # Panics
    ///
    /// Panics if the internal snapshot ID is `None`. The typestate guarantees
    /// this cannot happen when constructed through the public API.
    pub fn snapshot_id(&self) -> SnapshotId {
        self.snapshot_id
            .expect("Resolved ActivePreset must have a snapshot_id")
    }

    /// Access the underlying preset.
    pub fn preset(&self) -> &Preset {
        &self.preset
    }

    /// List all snapshots available in this preset.
    pub fn available_snapshots(&self) -> &[Snapshot] {
        &self.preset.snapshots
    }

    /// Switch to a different snapshot.
    ///
    /// Returns `None` if no snapshot with the given ID exists in the preset.
    pub fn switch_snapshot(self, snapshot_id: SnapshotId) -> Option<ActivePreset<Resolved>> {
        let snapshot = self
            .preset
            .snapshots
            .iter()
            .find(|s| s.id == snapshot_id)
            .cloned()?;
        Some(ActivePreset {
            preset: self.preset,
            snapshot: Some(snapshot),
            snapshot_id: Some(snapshot_id),
            _state: PhantomData,
        })
    }

    /// Discard the snapshot selection, returning to unresolved state.
    pub fn unresolve(self) -> ActivePreset<Unresolved> {
        ActivePreset {
            preset: self.preset,
            snapshot: None,
            snapshot_id: None,
            _state: PhantomData,
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// ActiveScene
// ─────────────────────────────────────────────────────────────────────────────

/// A scene that is currently active, parameterized by resolution state.
///
/// - `ActiveScene<Unresolved>` — scene chosen, preset not yet selected.
/// - `ActiveScene<Resolved>` — scene chosen **and** a preset+snapshot are selected.
#[derive(Debug, Clone)]
pub struct ActiveScene<State> {
    scene: Scene,
    active_preset: Option<ActivePreset<Resolved>>,
    _state: PhantomData<State>,
}

impl ActiveScene<Unresolved> {
    /// Create a new unresolved active scene.
    pub fn new(scene: Scene) -> Self {
        Self {
            scene,
            active_preset: None,
            _state: PhantomData,
        }
    }

    /// Resolve by providing an already-resolved active preset.
    pub fn resolve(self, active_preset: ActivePreset<Resolved>) -> ActiveScene<Resolved> {
        ActiveScene {
            scene: self.scene,
            active_preset: Some(active_preset),
            _state: PhantomData,
        }
    }

    /// Resolve by creating an active preset from a `Preset` and resolving its
    /// default snapshot.
    ///
    /// Returns `None` if the preset has no snapshots.
    pub fn resolve_from_preset(self, preset: Preset) -> Option<ActiveScene<Resolved>> {
        let active_preset = ActivePreset::new(preset).resolve_default()?;
        Some(ActiveScene {
            scene: self.scene,
            active_preset: Some(active_preset),
            _state: PhantomData,
        })
    }

    /// Access the underlying scene.
    pub fn scene(&self) -> &Scene {
        &self.scene
    }
}

impl ActiveScene<Resolved> {
    /// Access the underlying scene.
    pub fn scene(&self) -> &Scene {
        &self.scene
    }

    /// Access the resolved active preset.
    ///
    /// # Panics
    ///
    /// Panics if the internal active preset is `None`. The typestate guarantees
    /// this cannot happen when constructed through the public API.
    pub fn active_preset(&self) -> &ActivePreset<Resolved> {
        self.active_preset
            .as_ref()
            .expect("Resolved ActiveScene must have an active_preset")
    }

    /// Shortcut: access the snapshot from the resolved preset.
    pub fn snapshot(&self) -> &Snapshot {
        self.active_preset().snapshot()
    }

    /// Shortcut: access the preset from the resolved active preset.
    pub fn preset(&self) -> &Preset {
        self.active_preset().preset()
    }

    /// Discard the preset selection, returning to unresolved state.
    pub fn unresolve(self) -> ActiveScene<Unresolved> {
        ActiveScene {
            scene: self.scene,
            active_preset: None,
            _state: PhantomData,
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// ActiveProfile
// ─────────────────────────────────────────────────────────────────────────────

/// A profile that is currently active, parameterized by resolution state.
///
/// - `ActiveProfile<Unresolved>` — profile chosen, scene template not yet selected.
/// - `ActiveProfile<Resolved>` — profile chosen **and** a scene template is selected.
#[derive(Debug, Clone)]
pub struct ActiveProfile<State> {
    profile: Profile,
    scene_template: Option<SceneTemplate>,
    scene_template_id: Option<SceneTemplateId>,
    _state: PhantomData<State>,
}

impl ActiveProfile<Unresolved> {
    /// Create a new unresolved active profile.
    pub fn new(profile: Profile) -> Self {
        Self {
            profile,
            scene_template: None,
            scene_template_id: None,
            _state: PhantomData,
        }
    }

    /// Resolve by selecting a scene template by ID.
    ///
    /// Returns `None` if no scene template with the given ID exists in the profile.
    pub fn resolve(
        self,
        scene_template_id: SceneTemplateId,
    ) -> Option<ActiveProfile<Resolved>> {
        let scene_template = self
            .profile
            .scene_templates
            .iter()
            .find(|st| st.id == scene_template_id)
            .cloned()?;
        Some(ActiveProfile {
            profile: self.profile,
            scene_template: Some(scene_template),
            scene_template_id: Some(scene_template_id),
            _state: PhantomData,
        })
    }

    /// Resolve by selecting the first (default) scene template.
    ///
    /// Returns `None` if the profile has no scene templates.
    pub fn resolve_default(self) -> Option<ActiveProfile<Resolved>> {
        let first = self.profile.scene_templates.first().cloned()?;
        let id = first.id;
        Some(ActiveProfile {
            profile: self.profile,
            scene_template: Some(first),
            scene_template_id: Some(id),
            _state: PhantomData,
        })
    }

    /// Access the underlying profile.
    pub fn profile(&self) -> &Profile {
        &self.profile
    }

    /// List all scene templates available in this profile.
    pub fn available_scenes(&self) -> &[SceneTemplate] {
        &self.profile.scene_templates
    }
}

impl ActiveProfile<Resolved> {
    /// Access the resolved scene template.
    ///
    /// # Panics
    ///
    /// Panics if the internal scene template is `None`. The typestate guarantees
    /// this cannot happen when constructed through the public API.
    pub fn scene_template(&self) -> &SceneTemplate {
        self.scene_template
            .as_ref()
            .expect("Resolved ActiveProfile must have a scene_template")
    }

    /// Get the ID of the resolved scene template.
    ///
    /// # Panics
    ///
    /// Panics if the internal scene template ID is `None`. The typestate
    /// guarantees this cannot happen when constructed through the public API.
    pub fn scene_template_id(&self) -> SceneTemplateId {
        self.scene_template_id
            .expect("Resolved ActiveProfile must have a scene_template_id")
    }

    /// Access the underlying profile.
    pub fn profile(&self) -> &Profile {
        &self.profile
    }

    /// List all scene templates available in this profile.
    pub fn available_scenes(&self) -> &[SceneTemplate] {
        &self.profile.scene_templates
    }

    /// Switch to a different scene template.
    ///
    /// Returns `None` if no scene template with the given ID exists in the profile.
    pub fn switch_scene(
        self,
        scene_template_id: SceneTemplateId,
    ) -> Option<ActiveProfile<Resolved>> {
        let scene_template = self
            .profile
            .scene_templates
            .iter()
            .find(|st| st.id == scene_template_id)
            .cloned()?;
        Some(ActiveProfile {
            profile: self.profile,
            scene_template: Some(scene_template),
            scene_template_id: Some(scene_template_id),
            _state: PhantomData,
        })
    }

    /// Discard the scene template selection, returning to unresolved state.
    pub fn unresolve(self) -> ActiveProfile<Unresolved> {
        ActiveProfile {
            profile: self.profile,
            scene_template: None,
            scene_template_id: None,
            _state: PhantomData,
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// ActiveSong
// ─────────────────────────────────────────────────────────────────────────────

/// A song that is currently active, parameterized by resolution state.
///
/// - `ActiveSong<Unresolved>` — song chosen, scene not yet selected.
/// - `ActiveSong<Resolved>` — song chosen **and** a scene+preset+snapshot chain is selected.
#[derive(Debug, Clone)]
pub struct ActiveSong<State> {
    song: PerformanceSong,
    scene_index: Option<usize>,
    active_scene: Option<ActiveScene<Resolved>>,
    _state: PhantomData<State>,
}

impl ActiveSong<Unresolved> {
    /// Create a new unresolved active song.
    pub fn new(song: PerformanceSong) -> Self {
        Self {
            song,
            scene_index: None,
            active_scene: None,
            _state: PhantomData,
        }
    }

    /// Resolve by providing a scene index and an already-resolved active scene.
    ///
    /// Returns `None` if `scene_index` is out of bounds.
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

    /// Access the underlying song.
    pub fn song(&self) -> &PerformanceSong {
        &self.song
    }

    /// List all scenes available in this song.
    pub fn available_scenes(&self) -> &[Scene] {
        &self.song.scenes
    }
}

impl ActiveSong<Resolved> {
    /// Access the underlying song.
    pub fn song(&self) -> &PerformanceSong {
        &self.song
    }

    /// Get the index of the currently active scene.
    ///
    /// # Panics
    ///
    /// Panics if the internal scene index is `None`. The typestate guarantees
    /// this cannot happen when constructed through the public API.
    pub fn scene_index(&self) -> usize {
        self.scene_index
            .expect("Resolved ActiveSong must have a scene_index")
    }

    /// Access the resolved active scene.
    ///
    /// # Panics
    ///
    /// Panics if the internal active scene is `None`. The typestate guarantees
    /// this cannot happen when constructed through the public API.
    pub fn active_scene(&self) -> &ActiveScene<Resolved> {
        self.active_scene
            .as_ref()
            .expect("Resolved ActiveSong must have an active_scene")
    }

    /// Shortcut: access the scene from the resolved active scene.
    pub fn scene(&self) -> &Scene {
        self.active_scene().scene()
    }

    /// Shortcut: access the snapshot from the resolved scene's preset.
    pub fn snapshot(&self) -> &Snapshot {
        self.active_scene().snapshot()
    }

    /// Shortcut: access the preset from the resolved scene's active preset.
    pub fn preset(&self) -> &Preset {
        self.active_scene().preset()
    }

    /// List all scenes available in this song.
    pub fn available_scenes(&self) -> &[Scene] {
        &self.song.scenes
    }

    /// Navigate to a different scene in the song.
    ///
    /// Returns `None` if `scene_index` is out of bounds.
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

    /// Discard the scene selection, returning to unresolved state.
    pub fn unresolve(self) -> ActiveSong<Unresolved> {
        ActiveSong {
            song: self.song,
            scene_index: None,
            active_scene: None,
            _state: PhantomData,
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::id::{PresetId, ProfileId, SceneId, SceneTemplateId, SnapshotId, SongId};

    // ── Test helpers ─────────────────────────────────────────────────────

    fn make_snapshot(name: &str) -> Snapshot {
        Snapshot::new(name)
    }

    fn make_snapshot_with_id(id: SnapshotId, name: &str) -> Snapshot {
        let mut snap = Snapshot::new(name);
        snap.id = id;
        snap
    }

    fn make_preset(name: &str, snapshots: Vec<Snapshot>) -> Preset {
        use crate::category::PresetCategory;
        let mut preset = Preset::new(name, PresetCategory::default());
        for snap in snapshots {
            preset.add_snapshot(snap);
        }
        preset
    }

    fn make_scene_template(name: &str) -> SceneTemplate {
        SceneTemplate::direct(name, PresetId::new())
    }

    fn make_scene_template_with_id(id: SceneTemplateId, name: &str) -> SceneTemplate {
        let mut tmpl = SceneTemplate::direct(name, PresetId::new());
        tmpl.id = id;
        tmpl
    }

    fn make_profile(name: &str, templates: Vec<SceneTemplate>) -> Profile {
        use crate::id::RigId;
        let mut profile = Profile::new(name, RigId::new());
        for tmpl in templates {
            profile.add_scene_template(tmpl);
        }
        profile
    }

    fn make_scene(name: &str) -> Scene {
        Scene::new(name, PresetId::new())
    }

    fn make_song(name: &str, scenes: Vec<Scene>) -> PerformanceSong {
        let mut song = PerformanceSong::new(name);
        for scene in scenes {
            song.add_scene(scene);
        }
        song
    }

    // ── ActivePreset tests ──────────────────────────────────────────────

    #[test]
    fn preset_resolve_and_unresolve() {
        let snap_id = SnapshotId::new();
        let snapshot = make_snapshot_with_id(snap_id, "Clean");
        let preset = make_preset("My Preset", vec![snapshot]);

        let active = ActivePreset::new(preset);
        assert_eq!(active.preset().name, "My Preset");
        assert_eq!(active.available_snapshots().len(), 1);

        // Resolve
        let resolved = active.resolve(snap_id).expect("should resolve");
        assert_eq!(resolved.snapshot().name, "Clean");
        assert_eq!(resolved.snapshot_id(), snap_id);
        assert_eq!(resolved.preset().name, "My Preset");
        assert_eq!(resolved.available_snapshots().len(), 1);

        // Unresolve
        let unresolved = resolved.unresolve();
        assert_eq!(unresolved.preset().name, "My Preset");
    }

    #[test]
    fn preset_resolve_default() {
        let snap_a = make_snapshot("Default");
        let snap_b = make_snapshot("Alternate");
        let preset = make_preset("Test", vec![snap_a.clone(), snap_b]);

        let active = ActivePreset::new(preset);
        let resolved = active.resolve_default().expect("should resolve default");
        assert_eq!(resolved.snapshot().name, "Default");
        assert_eq!(resolved.snapshot_id(), snap_a.id);
    }

    #[test]
    fn preset_resolve_default_empty_returns_none() {
        let preset = make_preset("Empty", vec![]);
        let active = ActivePreset::new(preset);
        assert!(active.resolve_default().is_none());
    }

    #[test]
    fn preset_switch_snapshot() {
        let snap_a_id = SnapshotId::new();
        let snap_b_id = SnapshotId::new();
        let snap_a = make_snapshot_with_id(snap_a_id, "Clean");
        let snap_b = make_snapshot_with_id(snap_b_id, "Dirty");
        let preset = make_preset("Switchable", vec![snap_a, snap_b]);

        let active = ActivePreset::new(preset);
        let resolved = active.resolve(snap_a_id).expect("resolve A");
        assert_eq!(resolved.snapshot().name, "Clean");

        let switched = resolved.switch_snapshot(snap_b_id).expect("switch to B");
        assert_eq!(switched.snapshot().name, "Dirty");
        assert_eq!(switched.snapshot_id(), snap_b_id);
    }

    #[test]
    fn preset_invalid_snapshot_id_returns_none() {
        let snapshot = make_snapshot("Only One");
        let preset = make_preset("Test", vec![snapshot]);

        let active = ActivePreset::new(preset);
        let bogus_id = SnapshotId::new();
        assert!(active.resolve(bogus_id).is_none());
    }

    #[test]
    fn preset_switch_to_invalid_snapshot_returns_none() {
        let snap_id = SnapshotId::new();
        let snapshot = make_snapshot_with_id(snap_id, "Only");
        let preset = make_preset("Test", vec![snapshot]);

        let active = ActivePreset::new(preset);
        let resolved = active.resolve(snap_id).expect("resolve");
        let bogus_id = SnapshotId::new();
        assert!(resolved.switch_snapshot(bogus_id).is_none());
    }

    // ── ActiveScene tests ───────────────────────────────────────────────

    #[test]
    fn scene_resolve_with_preset() {
        let snap_id = SnapshotId::new();
        let snapshot = make_snapshot_with_id(snap_id, "Snap");
        let preset = make_preset("Preset", vec![snapshot]);
        let scene = make_scene("Verse");

        let active_scene = ActiveScene::new(scene);
        assert_eq!(active_scene.scene().name, "Verse");

        let active_preset = ActivePreset::new(preset).resolve(snap_id).unwrap();
        let resolved = active_scene.resolve(active_preset);

        assert_eq!(resolved.scene().name, "Verse");
        assert_eq!(resolved.preset().name, "Preset");
        assert_eq!(resolved.snapshot().name, "Snap");
        assert_eq!(resolved.active_preset().snapshot_id(), snap_id);
    }

    #[test]
    fn scene_resolve_from_preset() {
        let snapshot = make_snapshot("Default Snap");
        let preset = make_preset("Auto Preset", vec![snapshot]);
        let scene = make_scene("Chorus");

        let active_scene = ActiveScene::new(scene);
        let resolved = active_scene
            .resolve_from_preset(preset)
            .expect("should resolve from preset");

        assert_eq!(resolved.scene().name, "Chorus");
        assert_eq!(resolved.preset().name, "Auto Preset");
        assert_eq!(resolved.snapshot().name, "Default Snap");
    }

    #[test]
    fn scene_resolve_from_empty_preset_returns_none() {
        let preset = make_preset("No Snaps", vec![]);
        let scene = make_scene("Bridge");

        let active_scene = ActiveScene::new(scene);
        assert!(active_scene.resolve_from_preset(preset).is_none());
    }

    #[test]
    fn scene_unresolve() {
        let snapshot = make_snapshot("Snap");
        let preset = make_preset("P", vec![snapshot]);
        let scene = make_scene("Intro");

        let active_scene = ActiveScene::new(scene);
        let resolved = active_scene.resolve_from_preset(preset).unwrap();
        assert_eq!(resolved.scene().name, "Intro");

        let unresolved = resolved.unresolve();
        assert_eq!(unresolved.scene().name, "Intro");
    }

    // ── ActiveProfile tests ─────────────────────────────────────────────

    #[test]
    fn profile_resolve_and_unresolve() {
        let tmpl_id = SceneTemplateId::new();
        let tmpl = make_scene_template_with_id(tmpl_id, "Electric");
        let profile = make_profile("Live", vec![tmpl]);

        let active = ActiveProfile::new(profile);
        assert_eq!(active.profile().name, "Live");
        assert_eq!(active.available_scenes().len(), 1);

        let resolved = active.resolve(tmpl_id).expect("should resolve");
        assert_eq!(resolved.scene_template().name, "Electric");
        assert_eq!(resolved.scene_template_id(), tmpl_id);
        assert_eq!(resolved.profile().name, "Live");
        assert_eq!(resolved.available_scenes().len(), 1);

        let unresolved = resolved.unresolve();
        assert_eq!(unresolved.profile().name, "Live");
    }

    #[test]
    fn profile_resolve_default() {
        let tmpl_a = make_scene_template("First");
        let tmpl_b = make_scene_template("Second");
        let profile = make_profile("Worship", vec![tmpl_a.clone(), tmpl_b]);

        let active = ActiveProfile::new(profile);
        let resolved = active.resolve_default().expect("should resolve default");
        assert_eq!(resolved.scene_template().name, "First");
        assert_eq!(resolved.scene_template_id(), tmpl_a.id);
    }

    #[test]
    fn profile_resolve_default_empty_returns_none() {
        let profile = make_profile("Empty", vec![]);
        let active = ActiveProfile::new(profile);
        assert!(active.resolve_default().is_none());
    }

    #[test]
    fn profile_switch_scene() {
        let tmpl_a_id = SceneTemplateId::new();
        let tmpl_b_id = SceneTemplateId::new();
        let tmpl_a = make_scene_template_with_id(tmpl_a_id, "Acoustic");
        let tmpl_b = make_scene_template_with_id(tmpl_b_id, "Electric");
        let profile = make_profile("Set", vec![tmpl_a, tmpl_b]);

        let active = ActiveProfile::new(profile);
        let resolved = active.resolve(tmpl_a_id).expect("resolve A");
        assert_eq!(resolved.scene_template().name, "Acoustic");

        let switched = resolved.switch_scene(tmpl_b_id).expect("switch to B");
        assert_eq!(switched.scene_template().name, "Electric");
        assert_eq!(switched.scene_template_id(), tmpl_b_id);
    }

    #[test]
    fn profile_invalid_template_id_returns_none() {
        let tmpl = make_scene_template("Only");
        let profile = make_profile("Test", vec![tmpl]);

        let active = ActiveProfile::new(profile);
        let bogus_id = SceneTemplateId::new();
        assert!(active.resolve(bogus_id).is_none());
    }

    // ── ActiveSong tests ────────────────────────────────────────────────

    #[test]
    fn song_resolve_with_scene() {
        let snap_id = SnapshotId::new();
        let snapshot = make_snapshot_with_id(snap_id, "Main Snap");
        let preset = make_preset("Main Preset", vec![snapshot]);
        let scene = make_scene("Verse 1");
        let song = make_song("Amazing Grace", vec![scene.clone(), make_scene("Chorus")]);

        let active_song = ActiveSong::new(song);
        assert_eq!(active_song.song().name, "Amazing Grace");
        assert_eq!(active_song.available_scenes().len(), 2);

        let active_preset = ActivePreset::new(preset).resolve(snap_id).unwrap();
        let active_scene = ActiveScene::new(scene).resolve(active_preset);
        let resolved = active_song.resolve(0, active_scene).expect("should resolve");

        assert_eq!(resolved.song().name, "Amazing Grace");
        assert_eq!(resolved.scene_index(), 0);
        assert_eq!(resolved.scene().name, "Verse 1");
        assert_eq!(resolved.preset().name, "Main Preset");
        assert_eq!(resolved.snapshot().name, "Main Snap");
        assert_eq!(resolved.available_scenes().len(), 2);
    }

    #[test]
    fn song_resolve_out_of_bounds_returns_none() {
        let song = make_song("Short Song", vec![make_scene("Only Scene")]);
        let snap = make_snapshot("S");
        let preset = make_preset("P", vec![snap]);
        let scene = make_scene("X");

        let active_song = ActiveSong::new(song);
        let active_preset = ActivePreset::new(preset).resolve_default().unwrap();
        let active_scene = ActiveScene::new(scene).resolve(active_preset);

        // Index 1 is out of bounds (only index 0 exists)
        assert!(active_song.resolve(1, active_scene).is_none());
    }

    #[test]
    fn song_navigation() {
        let snap_id_a = SnapshotId::new();
        let snap_id_b = SnapshotId::new();
        let snap_a = make_snapshot_with_id(snap_id_a, "Verse Snap");
        let snap_b = make_snapshot_with_id(snap_id_b, "Chorus Snap");
        let preset_a = make_preset("Verse Preset", vec![snap_a]);
        let preset_b = make_preset("Chorus Preset", vec![snap_b]);

        let scene_verse = make_scene("Verse");
        let scene_chorus = make_scene("Chorus");
        let song = make_song("Test Song", vec![scene_verse.clone(), scene_chorus.clone()]);

        // Start at scene 0 (Verse)
        let active_song = ActiveSong::new(song);
        let active_preset_a = ActivePreset::new(preset_a).resolve(snap_id_a).unwrap();
        let active_scene_a = ActiveScene::new(scene_verse).resolve(active_preset_a);
        let resolved = active_song.resolve(0, active_scene_a).unwrap();

        assert_eq!(resolved.scene_index(), 0);
        assert_eq!(resolved.scene().name, "Verse");
        assert_eq!(resolved.snapshot().name, "Verse Snap");

        // Navigate to scene 1 (Chorus)
        let active_preset_b = ActivePreset::new(preset_b).resolve(snap_id_b).unwrap();
        let active_scene_b = ActiveScene::new(scene_chorus).resolve(active_preset_b);
        let navigated = resolved.go_to_scene(1, active_scene_b).unwrap();

        assert_eq!(navigated.scene_index(), 1);
        assert_eq!(navigated.scene().name, "Chorus");
        assert_eq!(navigated.snapshot().name, "Chorus Snap");
    }

    #[test]
    fn song_go_to_scene_out_of_bounds_returns_none() {
        let snap = make_snapshot("S");
        let preset = make_preset("P", vec![snap.clone()]);
        let scene = make_scene("Only");
        let song = make_song("Tiny", vec![scene.clone()]);

        let active_preset = ActivePreset::new(preset.clone())
            .resolve(snap.id)
            .unwrap();
        let active_scene = ActiveScene::new(scene.clone()).resolve(active_preset);
        let resolved = ActiveSong::new(song).resolve(0, active_scene).unwrap();

        // Try to navigate to index 5 (out of bounds)
        let active_preset2 = ActivePreset::new(preset).resolve(snap.id).unwrap();
        let active_scene2 = ActiveScene::new(scene).resolve(active_preset2);
        assert!(resolved.go_to_scene(5, active_scene2).is_none());
    }

    #[test]
    fn song_unresolve() {
        let snap = make_snapshot("S");
        let preset = make_preset("P", vec![snap.clone()]);
        let scene = make_scene("V");
        let song = make_song("Song", vec![scene.clone()]);

        let active_preset = ActivePreset::new(preset).resolve(snap.id).unwrap();
        let active_scene = ActiveScene::new(scene).resolve(active_preset);
        let resolved = ActiveSong::new(song).resolve(0, active_scene).unwrap();

        let unresolved = resolved.unresolve();
        assert_eq!(unresolved.song().name, "Song");
        assert_eq!(unresolved.available_scenes().len(), 1);
    }

    // ── Full hierarchy test ─────────────────────────────────────────────

    #[test]
    fn full_hierarchy_song_scene_preset_snapshot() {
        // Build from the bottom up
        let snap_verse = make_snapshot("Verse Clean");
        let snap_chorus = make_snapshot("Chorus Drive");
        let preset_verse = make_preset("Verse Tone", vec![snap_verse.clone()]);
        let preset_chorus = make_preset("Chorus Tone", vec![snap_chorus.clone()]);

        let scene_verse = make_scene("Verse");
        let scene_chorus = make_scene("Chorus");
        let scene_bridge = make_scene("Bridge");

        let song = make_song(
            "How Great Is Our God",
            vec![scene_verse.clone(), scene_chorus.clone(), scene_bridge],
        );

        // 1. Create unresolved song
        let active_song = ActiveSong::new(song);
        assert_eq!(active_song.available_scenes().len(), 3);

        // 2. Resolve preset for verse scene
        let active_preset = ActivePreset::new(preset_verse)
            .resolve(snap_verse.id)
            .expect("verse snapshot exists");
        assert_eq!(active_preset.snapshot().name, "Verse Clean");

        // 3. Resolve scene with that preset
        let active_scene = ActiveScene::new(scene_verse).resolve(active_preset);
        assert_eq!(active_scene.scene().name, "Verse");
        assert_eq!(active_scene.snapshot().name, "Verse Clean");

        // 4. Resolve song with that scene at index 0
        let resolved_song = active_song
            .resolve(0, active_scene)
            .expect("scene index 0 exists");

        // Verify full chain
        assert_eq!(resolved_song.song().name, "How Great Is Our God");
        assert_eq!(resolved_song.scene_index(), 0);
        assert_eq!(resolved_song.scene().name, "Verse");
        assert_eq!(resolved_song.preset().name, "Verse Tone");
        assert_eq!(resolved_song.snapshot().name, "Verse Clean");

        // 5. Navigate to chorus
        let chorus_preset = ActivePreset::new(preset_chorus)
            .resolve(snap_chorus.id)
            .expect("chorus snapshot exists");
        let chorus_scene = ActiveScene::new(scene_chorus).resolve(chorus_preset);
        let navigated = resolved_song
            .go_to_scene(1, chorus_scene)
            .expect("scene index 1 exists");

        assert_eq!(navigated.scene_index(), 1);
        assert_eq!(navigated.scene().name, "Chorus");
        assert_eq!(navigated.preset().name, "Chorus Tone");
        assert_eq!(navigated.snapshot().name, "Chorus Drive");

        // 6. Unresolve back up the chain
        let unresolved_song = navigated.unresolve();
        assert_eq!(unresolved_song.song().name, "How Great Is Our God");
    }
}
