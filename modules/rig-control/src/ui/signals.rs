//! Rig Control Global Signals
//!
//! This module defines global Dioxus signals for rig state that UI components
//! can read and subscribe to. The signals are updated by the rig service via
//! the `use_rig_subscription()` hook.

use dioxus::prelude::*;
use uuid::Uuid;

// Re-export service types for convenience
pub use crate::service::{
    PresetInfo, PresetSnapshotInfo, ProfileInfo, ProfileSceneInfo, RigInfo, SetlistInfo, SongInfo,
};

/// Current profile loaded in the rig
pub static RIG_PROFILE: GlobalSignal<Option<ProfileInfo>> = Signal::global(|| None);

/// All available profiles
pub static RIG_AVAILABLE_PROFILES: GlobalSignal<Vec<ProfileInfo>> = Signal::global(Vec::new);

/// Current rig information
pub static RIG_INFO: GlobalSignal<Option<RigInfo>> = Signal::global(|| None);

/// Currently loaded preset
pub static RIG_CURRENT_PRESET: GlobalSignal<Option<PresetInfo>> = Signal::global(|| None);

/// Currently active preset snapshot ID (scene within preset)
pub static RIG_CURRENT_PRESET_SNAPSHOT_ID: GlobalSignal<Option<Uuid>> = Signal::global(|| None);

/// All available presets
pub static RIG_AVAILABLE_PRESETS: GlobalSignal<Vec<PresetInfo>> = Signal::global(Vec::new);

/// Preloaded presets (presets that have been loaded into memory for fast switching)
pub static RIG_PRELOADED_PRESETS: GlobalSignal<Vec<PresetInfo>> = Signal::global(Vec::new);

/// Current setlist
pub static RIG_CURRENT_SETLIST: GlobalSignal<Option<SetlistInfo>> = Signal::global(|| None);

/// All available setlists
pub static RIG_AVAILABLE_SETLISTS: GlobalSignal<Vec<SetlistInfo>> = Signal::global(Vec::new);

/// Songs in the current setlist
pub static RIG_SETLIST_SONGS: GlobalSignal<Vec<SongInfo>> = Signal::global(Vec::new);

/// Current song (in performance mode)
pub static RIG_CURRENT_SONG: GlobalSignal<Option<SongInfo>> = Signal::global(|| None);

/// Current song index
pub static RIG_SONG_INDEX: GlobalSignal<usize> = Signal::global(|| 0);

/// Current scene (in performance mode)
pub static RIG_CURRENT_SCENE: GlobalSignal<Option<ProfileSceneInfo>> = Signal::global(|| None);

/// Current scene index
pub static RIG_SCENE_INDEX: GlobalSignal<usize> = Signal::global(|| 0);

/// Connection status
pub static RIG_CONNECTED: GlobalSignal<bool> = Signal::global(|| false);

/// Loading status
pub static RIG_LOADING: GlobalSignal<bool> = Signal::global(|| false);
