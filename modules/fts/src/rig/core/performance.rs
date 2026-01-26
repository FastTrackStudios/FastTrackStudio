//! Performance types - Scene, Song, Setlist
//!
//! These types organize presets for live performance, allowing navigation
//! through song sections with smooth transitions.

use std::collections::HashMap;

use facet::Facet;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// A song section that selects a Preset or Snapshot.
///
/// Scenes represent parts of a song (Intro, Verse, Chorus) and define
/// which preset/snapshot to use and how to transition to it.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct Scene {
    /// Unique identifier
    pub id: Uuid,
    /// Scene name (e.g., "Verse 1", "Chorus")
    pub name: String,
    /// Preset to use for this scene
    pub preset_id: Uuid,
    /// Optional snapshot within the preset
    pub snapshot_id: Option<Uuid>,
    /// Transition settings when entering this scene
    pub transition: SceneTransition,
    /// MIDI triggers to activate this scene
    pub midi_triggers: Vec<MidiTrigger>,
}

/// Transition settings when entering a scene.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct SceneTransition {
    /// Type of transition
    pub transition_type: TransitionType,
    /// Fade time in milliseconds (for crossfade transitions)
    pub fade_time_ms: u32,
    /// Gap time in milliseconds (for gap transitions)
    pub gap_time_ms: u32,
}

/// Type of transition between scenes.
#[repr(u8)]
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub enum TransitionType {
    /// Instant switch (for snapshots of the same preset)
    Instant,
    /// Crossfade between presets (if compatible)
    Crossfade,
    /// Gap with volume fade out/in
    Gap,
    /// MIDI-synchronized switch (wait for next beat/bar)
    MidiSync { sync_to: SyncUnit },
}

/// Unit to synchronize transitions to.
#[repr(u8)]
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub enum SyncUnit {
    /// Sync to the next beat
    Beat,
    /// Sync to the next bar
    Bar,
    /// Sync to the next N-bar phrase
    Phrase(u8),
}

/// MIDI trigger for scene activation.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct MidiTrigger {
    /// MIDI channel (None = any channel)
    pub channel: Option<u8>,
    /// Trigger type
    pub trigger_type: MidiTriggerType,
}

/// Type of MIDI trigger.
#[repr(u8)]
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub enum MidiTriggerType {
    /// Note on message
    NoteOn { note: u8 },
    /// Control change message
    ControlChange { cc: u8, value_range: (u8, u8) },
    /// Program change message
    ProgramChange { program: u8 },
}

/// A song containing scenes for performance.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct PerformanceSong {
    /// Unique identifier
    pub id: Uuid,
    /// Song name
    pub name: String,
    /// Scenes in this song
    pub scenes: Vec<Scene>,
    /// Default scene index to start on
    pub default_scene_index: usize,
    /// Whether to auto-advance scenes with transport
    pub auto_advance: bool,
    /// Reference to existing song in setlist (if linked)
    pub linked_song_id: Option<Uuid>,
}

/// A collection of songs for performance.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct PerformanceSetlist {
    /// Unique identifier
    pub id: Uuid,
    /// Setlist name (e.g., "Sunday Service")
    pub name: String,
    /// Songs in performance order
    pub songs: Vec<PerformanceSong>,
    /// Presets to preload for this setlist
    pub preload_preset_ids: Vec<Uuid>,
    /// Metadata (date, venue, notes)
    pub metadata: HashMap<String, String>,
}

impl Scene {
    /// Create a new scene
    pub fn new(name: impl Into<String>, preset_id: Uuid) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            preset_id,
            snapshot_id: None,
            transition: SceneTransition::default(),
            midi_triggers: Vec::new(),
        }
    }

    /// With a specific snapshot
    pub fn with_snapshot(mut self, snapshot_id: Uuid) -> Self {
        self.snapshot_id = Some(snapshot_id);
        self
    }

    /// With a specific transition
    pub fn with_transition(mut self, transition: SceneTransition) -> Self {
        self.transition = transition;
        self
    }

    /// Add a MIDI trigger
    pub fn add_midi_trigger(&mut self, trigger: MidiTrigger) {
        self.midi_triggers.push(trigger);
    }
}

impl Default for SceneTransition {
    fn default() -> Self {
        Self {
            transition_type: TransitionType::Instant,
            fade_time_ms: 100,
            gap_time_ms: 0,
        }
    }
}

impl SceneTransition {
    /// Create an instant transition
    pub fn instant() -> Self {
        Self {
            transition_type: TransitionType::Instant,
            ..Default::default()
        }
    }

    /// Create a crossfade transition
    pub fn crossfade(fade_time_ms: u32) -> Self {
        Self {
            transition_type: TransitionType::Crossfade,
            fade_time_ms,
            gap_time_ms: 0,
        }
    }

    /// Create a gap transition
    pub fn gap(gap_time_ms: u32) -> Self {
        Self {
            transition_type: TransitionType::Gap,
            fade_time_ms: 50,
            gap_time_ms,
        }
    }

    /// Create a MIDI-synced transition
    pub fn midi_sync(sync_to: SyncUnit) -> Self {
        Self {
            transition_type: TransitionType::MidiSync { sync_to },
            fade_time_ms: 0,
            gap_time_ms: 0,
        }
    }
}

impl MidiTrigger {
    /// Create a note-on trigger
    pub fn note_on(note: u8, channel: Option<u8>) -> Self {
        Self {
            channel,
            trigger_type: MidiTriggerType::NoteOn { note },
        }
    }

    /// Create a program change trigger
    pub fn program_change(program: u8, channel: Option<u8>) -> Self {
        Self {
            channel,
            trigger_type: MidiTriggerType::ProgramChange { program },
        }
    }

    /// Create a control change trigger
    pub fn control_change(cc: u8, value_range: (u8, u8), channel: Option<u8>) -> Self {
        Self {
            channel,
            trigger_type: MidiTriggerType::ControlChange { cc, value_range },
        }
    }
}

impl PerformanceSong {
    /// Create a new performance song
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            scenes: Vec::new(),
            default_scene_index: 0,
            auto_advance: false,
            linked_song_id: None,
        }
    }

    /// Add a scene
    pub fn add_scene(&mut self, scene: Scene) {
        self.scenes.push(scene);
    }

    /// Get a scene by index
    pub fn get_scene(&self, index: usize) -> Option<&Scene> {
        self.scenes.get(index)
    }

    /// Get the number of scenes
    pub fn scene_count(&self) -> usize {
        self.scenes.len()
    }

    /// Link to an existing song
    pub fn link_to_song(mut self, song_id: Uuid) -> Self {
        self.linked_song_id = Some(song_id);
        self
    }
}

impl PerformanceSetlist {
    /// Create a new performance setlist
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            songs: Vec::new(),
            preload_preset_ids: Vec::new(),
            metadata: HashMap::new(),
        }
    }

    /// Add a song
    pub fn add_song(&mut self, song: PerformanceSong) {
        // Collect all preset IDs from the song for preloading
        for scene in &song.scenes {
            if !self.preload_preset_ids.contains(&scene.preset_id) {
                self.preload_preset_ids.push(scene.preset_id);
            }
        }
        self.songs.push(song);
    }

    /// Get a song by index
    pub fn get_song(&self, index: usize) -> Option<&PerformanceSong> {
        self.songs.get(index)
    }

    /// Get the number of songs
    pub fn song_count(&self) -> usize {
        self.songs.len()
    }

    /// Set metadata
    pub fn set_metadata(&mut self, key: impl Into<String>, value: impl Into<String>) {
        self.metadata.insert(key.into(), value.into());
    }
}
