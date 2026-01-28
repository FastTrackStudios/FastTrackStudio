//! Performance — live performance structures for scenes, songs, and setlists.
//!
//! A performance setlist organizes songs into an ordered sequence, each song
//! containing scenes that control preset/snapshot switching and MIDI triggers.

use std::collections::HashMap;

use facet::Facet;

use uuid::Uuid;

use crate::id::{PresetId, SceneId, SnapshotId, SongId};
use crate::module_preset::ModuleOverride;
use crate::normalized::{MidiChannel, MidiNote};

// ─────────────────────────────────────────────────────────────────────────────
// SyncUnit
// ─────────────────────────────────────────────────────────────────────────────

/// Musical unit to synchronize transitions against.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Facet)]
#[repr(u8)]
pub enum SyncUnit {
    /// Synchronize to the next beat.
    Beat,
    /// Synchronize to the next bar.
    Bar,
    /// Synchronize to the next phrase boundary (N bars).
    Phrase(u8),
}

// ─────────────────────────────────────────────────────────────────────────────
// TransitionType
// ─────────────────────────────────────────────────────────────────────────────

/// How a scene transition is executed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Facet)]
#[repr(u8)]
pub enum TransitionType {
    /// Switch immediately with no delay.
    Instant,
    /// Crossfade between the current and next scene.
    Crossfade,
    /// Insert a silent gap between scenes.
    Gap,
    /// Wait for a MIDI sync point before switching.
    MidiSync { sync_to: SyncUnit },
}

// ─────────────────────────────────────────────────────────────────────────────
// SceneTransition
// ─────────────────────────────────────────────────────────────────────────────

/// Configuration for transitioning into a scene.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Facet)]
pub struct SceneTransition {
    pub transition_type: TransitionType,
    /// Duration of the fade in milliseconds (relevant for `Crossfade`).
    pub fade_time_ms: u32,
    /// Duration of the gap in milliseconds (relevant for `Gap`).
    pub gap_time_ms: u32,
}

impl Default for SceneTransition {
    fn default() -> Self {
        Self::instant()
    }
}

impl SceneTransition {
    /// Instant transition — no fade, no gap.
    pub fn instant() -> Self {
        Self {
            transition_type: TransitionType::Instant,
            fade_time_ms: 0,
            gap_time_ms: 0,
        }
    }

    /// Crossfade transition with the given fade duration.
    pub fn crossfade(fade_time_ms: u32) -> Self {
        Self {
            transition_type: TransitionType::Crossfade,
            fade_time_ms,
            gap_time_ms: 0,
        }
    }

    /// Gap transition with the given silence duration.
    pub fn gap(gap_time_ms: u32) -> Self {
        Self {
            transition_type: TransitionType::Gap,
            fade_time_ms: 0,
            gap_time_ms,
        }
    }

    /// MIDI-synchronized transition — waits for the given musical boundary.
    pub fn midi_sync(sync_to: SyncUnit) -> Self {
        Self {
            transition_type: TransitionType::MidiSync { sync_to },
            fade_time_ms: 0,
            gap_time_ms: 0,
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// MidiTriggerType
// ─────────────────────────────────────────────────────────────────────────────

/// The kind of MIDI message that activates a trigger.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Facet)]
#[repr(u8)]
pub enum MidiTriggerType {
    /// Trigger on a specific MIDI note-on event.
    NoteOn { note: MidiNote },
    /// Trigger on a control-change message within a value range.
    ControlChange { cc: u8, value_range: (u8, u8) },
    /// Trigger on a program-change message.
    ProgramChange { program: u8 },
}

// ─────────────────────────────────────────────────────────────────────────────
// MidiTrigger
// ─────────────────────────────────────────────────────────────────────────────

/// A MIDI event that can trigger a scene change.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Facet)]
pub struct MidiTrigger {
    /// Optional MIDI channel filter. `None` matches any channel.
    pub channel: Option<MidiChannel>,
    pub trigger_type: MidiTriggerType,
}

impl MidiTrigger {
    /// Create a note-on trigger on any channel.
    pub fn note_on(note: MidiNote) -> Self {
        Self {
            channel: None,
            trigger_type: MidiTriggerType::NoteOn { note },
        }
    }

    /// Create a program-change trigger on any channel.
    pub fn program_change(program: u8) -> Self {
        Self {
            channel: None,
            trigger_type: MidiTriggerType::ProgramChange { program },
        }
    }

    /// Create a control-change trigger on any channel.
    pub fn control_change(cc: u8, value_range: (u8, u8)) -> Self {
        Self {
            channel: None,
            trigger_type: MidiTriggerType::ControlChange { cc, value_range },
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Scene
// ─────────────────────────────────────────────────────────────────────────────

/// A scene within a performance song.
///
/// Each scene references a preset (and optionally a snapshot within it),
/// defines how to transition into it, and lists MIDI triggers that activate it.
#[derive(Debug, Clone, Facet)]
pub struct Scene {
    pub id: SceneId,
    pub name: String,
    pub preset_id: PresetId,
    pub snapshot_id: Option<SnapshotId>,
    pub transition: SceneTransition,
    pub midi_triggers: Vec<MidiTrigger>,
    /// Module overrides for this scene — replace specific modules while
    /// keeping the rest from the base preset.

    pub module_overrides: Vec<ModuleOverride>,
}

impl Scene {
    /// Create a new scene targeting a preset with no snapshot.
    pub fn new(name: impl Into<String>, preset_id: PresetId) -> Self {
        Self {
            id: SceneId::new(),
            name: name.into(),
            preset_id,
            snapshot_id: None,
            transition: SceneTransition::default(),
            midi_triggers: Vec::new(),
            module_overrides: Vec::new(),
        }
    }

    /// Set the snapshot for this scene (builder pattern).
    #[must_use]
    pub fn with_snapshot(mut self, snapshot_id: SnapshotId) -> Self {
        self.snapshot_id = Some(snapshot_id);
        self
    }

    /// Set the transition for this scene (builder pattern).
    #[must_use]
    pub fn with_transition(mut self, transition: SceneTransition) -> Self {
        self.transition = transition;
        self
    }

    /// Add a MIDI trigger that activates this scene.
    pub fn add_midi_trigger(&mut self, trigger: MidiTrigger) {
        self.midi_triggers.push(trigger);
    }

    /// Add a module override for this scene.
    pub fn add_module_override(&mut self, module_override: ModuleOverride) {
        self.module_overrides.push(module_override);
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// PerformanceSong
// ─────────────────────────────────────────────────────────────────────────────

/// A song in a performance setlist, containing an ordered list of scenes.
#[derive(Debug, Clone, Facet)]
pub struct PerformanceSong {
    pub id: SongId,
    pub name: String,
    pub scenes: Vec<Scene>,
    /// Index into `scenes` for the default scene when the song is loaded.
    pub default_scene_index: usize,
    /// Whether to auto-advance to the next song when the last scene completes.
    pub auto_advance: bool,
    /// Optional link to another song (e.g., for medleys).
    pub linked_song_id: Option<SongId>,
    /// Song-level module overrides — apply to all scenes in this song.

    pub module_overrides: Vec<ModuleOverride>,
}

impl PerformanceSong {
    /// Create a new performance song with no scenes.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            id: SongId::new(),
            name: name.into(),
            scenes: Vec::new(),
            default_scene_index: 0,
            auto_advance: false,
            linked_song_id: None,
            module_overrides: Vec::new(),
        }
    }

    /// Add a scene to this song.
    pub fn add_scene(&mut self, scene: Scene) {
        self.scenes.push(scene);
    }

    /// Get a scene by its ID.
    pub fn get_scene(&self, id: SceneId) -> Option<&Scene> {
        self.scenes.iter().find(|s| s.id == id)
    }

    /// Number of scenes in this song.
    pub fn scene_count(&self) -> usize {
        self.scenes.len()
    }

    /// Link this song to another (e.g., for medleys or segues).
    pub fn link_to_song(&mut self, song_id: SongId) {
        self.linked_song_id = Some(song_id);
    }

    /// Add a song-level module override (applies to all scenes).
    pub fn add_module_override(&mut self, module_override: ModuleOverride) {
        self.module_overrides.push(module_override);
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// PerformanceSetlist
// ─────────────────────────────────────────────────────────────────────────────

/// An ordered list of performance songs with preload hints and metadata.
#[derive(Debug, Clone, Facet)]
pub struct PerformanceSetlist {
    pub id: Uuid,
    pub name: String,
    pub songs: Vec<PerformanceSong>,
    /// Preset IDs to preload before the performance starts.
    pub preload_preset_ids: Vec<PresetId>,
    /// Arbitrary key-value metadata (venue, date, notes, etc.).
    pub metadata: HashMap<String, String>,
}

impl PerformanceSetlist {
    /// Create a new empty performance setlist.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            songs: Vec::new(),
            preload_preset_ids: Vec::new(),
            metadata: HashMap::new(),
        }
    }

    /// Add a song to the setlist.
    pub fn add_song(&mut self, song: PerformanceSong) {
        self.songs.push(song);
    }

    /// Get a song by its ID.
    pub fn get_song(&self, id: SongId) -> Option<&PerformanceSong> {
        self.songs.iter().find(|s| s.id == id)
    }

    /// Number of songs in the setlist.
    pub fn song_count(&self) -> usize {
        self.songs.len()
    }

    /// Set a metadata key-value pair.
    pub fn set_metadata(&mut self, key: impl Into<String>, value: impl Into<String>) {
        self.metadata.insert(key.into(), value.into());
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // Scene creation

    #[test]
    fn scene_new_has_no_snapshot() {
        let scene = Scene::new("Verse", PresetId::new());
        assert!(scene.snapshot_id.is_none());
        assert!(scene.midi_triggers.is_empty());
        assert_eq!(scene.transition, SceneTransition::instant());
    }

    #[test]
    fn scene_with_snapshot() {
        let snapshot = SnapshotId::new();
        let scene = Scene::new("Chorus", PresetId::new()).with_snapshot(snapshot);
        assert_eq!(scene.snapshot_id, Some(snapshot));
    }

    #[test]
    fn scene_with_transition() {
        let scene = Scene::new("Bridge", PresetId::new())
            .with_transition(SceneTransition::crossfade(500));
        assert_eq!(scene.transition.transition_type, TransitionType::Crossfade);
        assert_eq!(scene.transition.fade_time_ms, 500);
    }

    #[test]
    fn scene_add_midi_trigger() {
        let mut scene = Scene::new("Solo", PresetId::new());
        scene.add_midi_trigger(MidiTrigger::note_on(MidiNote::MIDDLE_C));
        assert_eq!(scene.midi_triggers.len(), 1);
    }

    // SceneTransition types

    #[test]
    fn transition_default_is_instant() {
        let t = SceneTransition::default();
        assert_eq!(t.transition_type, TransitionType::Instant);
        assert_eq!(t.fade_time_ms, 0);
        assert_eq!(t.gap_time_ms, 0);
    }

    #[test]
    fn transition_crossfade() {
        let t = SceneTransition::crossfade(250);
        assert_eq!(t.transition_type, TransitionType::Crossfade);
        assert_eq!(t.fade_time_ms, 250);
        assert_eq!(t.gap_time_ms, 0);
    }

    #[test]
    fn transition_gap() {
        let t = SceneTransition::gap(100);
        assert_eq!(t.transition_type, TransitionType::Gap);
        assert_eq!(t.fade_time_ms, 0);
        assert_eq!(t.gap_time_ms, 100);
    }

    #[test]
    fn transition_midi_sync() {
        let t = SceneTransition::midi_sync(SyncUnit::Bar);
        assert_eq!(
            t.transition_type,
            TransitionType::MidiSync {
                sync_to: SyncUnit::Bar
            }
        );
    }

    #[test]
    fn transition_midi_sync_phrase() {
        let t = SceneTransition::midi_sync(SyncUnit::Phrase(4));
        assert_eq!(
            t.transition_type,
            TransitionType::MidiSync {
                sync_to: SyncUnit::Phrase(4)
            }
        );
    }

    // MidiTrigger creation

    #[test]
    fn midi_trigger_note_on() {
        let trigger = MidiTrigger::note_on(MidiNote::new(60));
        assert!(trigger.channel.is_none());
        assert_eq!(
            trigger.trigger_type,
            MidiTriggerType::NoteOn {
                note: MidiNote::MIDDLE_C
            }
        );
    }

    #[test]
    fn midi_trigger_program_change() {
        let trigger = MidiTrigger::program_change(42);
        assert_eq!(
            trigger.trigger_type,
            MidiTriggerType::ProgramChange { program: 42 }
        );
    }

    #[test]
    fn midi_trigger_control_change() {
        let trigger = MidiTrigger::control_change(7, (0, 127));
        assert_eq!(
            trigger.trigger_type,
            MidiTriggerType::ControlChange {
                cc: 7,
                value_range: (0, 127)
            }
        );
    }

    // PerformanceSong scene management

    #[test]
    fn song_starts_empty() {
        let song = PerformanceSong::new("Amazing Grace");
        assert_eq!(song.scene_count(), 0);
        assert!(!song.auto_advance);
        assert!(song.linked_song_id.is_none());
    }

    #[test]
    fn song_add_and_get_scene() {
        let mut song = PerformanceSong::new("Test Song");
        let scene = Scene::new("Intro", PresetId::new());
        let scene_id = scene.id;
        song.add_scene(scene);

        assert_eq!(song.scene_count(), 1);
        assert!(song.get_scene(scene_id).is_some());
        assert_eq!(song.get_scene(scene_id).unwrap().name, "Intro");
    }

    #[test]
    fn song_get_scene_not_found() {
        let song = PerformanceSong::new("Empty");
        assert!(song.get_scene(SceneId::new()).is_none());
    }

    #[test]
    fn song_link_to_song() {
        let mut song_a = PerformanceSong::new("Song A");
        let song_b = PerformanceSong::new("Song B");
        song_a.link_to_song(song_b.id);
        assert_eq!(song_a.linked_song_id, Some(song_b.id));
    }

    // PerformanceSetlist

    #[test]
    fn setlist_starts_empty() {
        let setlist = PerformanceSetlist::new("Friday Gig");
        assert_eq!(setlist.song_count(), 0);
        assert!(setlist.preload_preset_ids.is_empty());
        assert!(setlist.metadata.is_empty());
    }

    #[test]
    fn setlist_add_and_get_song() {
        let mut setlist = PerformanceSetlist::new("Test");
        let song = PerformanceSong::new("Opener");
        let song_id = song.id;
        setlist.add_song(song);

        assert_eq!(setlist.song_count(), 1);
        assert!(setlist.get_song(song_id).is_some());
        assert_eq!(setlist.get_song(song_id).unwrap().name, "Opener");
    }

    #[test]
    fn setlist_get_song_not_found() {
        let setlist = PerformanceSetlist::new("Empty");
        assert!(setlist.get_song(SongId::new()).is_none());
    }

    #[test]
    fn setlist_metadata() {
        let mut setlist = PerformanceSetlist::new("Show");
        setlist.set_metadata("venue", "The Roxy");
        setlist.set_metadata("date", "2026-01-27");

        assert_eq!(setlist.metadata.get("venue").unwrap(), "The Roxy");
        assert_eq!(setlist.metadata.get("date").unwrap(), "2026-01-27");
    }

    #[test]
    fn setlist_auto_preload() {
        let mut setlist = PerformanceSetlist::new("Preloaded");
        let preset_a = PresetId::new();
        let preset_b = PresetId::new();
        setlist.preload_preset_ids.push(preset_a);
        setlist.preload_preset_ids.push(preset_b);

        assert_eq!(setlist.preload_preset_ids.len(), 2);
        assert!(setlist.preload_preset_ids.contains(&preset_a));
        assert!(setlist.preload_preset_ids.contains(&preset_b));
    }

}
