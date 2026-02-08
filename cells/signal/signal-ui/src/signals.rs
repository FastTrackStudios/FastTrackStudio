//! Rig Control Global Signals
//!
//! This module defines global Dioxus signals for rig state that UI components
//! can read and subscribe to. The signals are updated by the rig service via
//! the `use_rig_subscription()` hook.

use crate::prelude::*;
use uuid::Uuid;

// Re-export service types for convenience (signal-control re-exports at crate root)
pub use signal_control::{
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

/// Current modules materialized from preset for UI display
pub static RIG_MODULES: GlobalSignal<Vec<signal_control::module::Module>> =
    Signal::global(Vec::new);

/// The rig service client — stored globally so any dock panel can dispatch actions
/// without needing a context provider wrapper.
pub static RIG_SERVICE: GlobalSignal<Option<signal_control::SignalControl>> =
    Signal::global(|| None);

/// Initialize the rig service with a mock guitar rig.
/// Call once at app startup before any rig panels render.
pub fn init_rig_service() {
    if RIG_SERVICE.read().is_some() {
        return; // already initialized
    }
    *RIG_SERVICE.write() = Some(signal_control::SignalControl::mock_guitar());
}

/// Connection status
pub static RIG_CONNECTED: GlobalSignal<bool> = Signal::global(|| false);

/// Loading status
pub static RIG_LOADING: GlobalSignal<bool> = Signal::global(|| false);

/// The node graph for the Flow/FlowCompact view.
///
/// Initialized with `sample_guitar_rig()` on first access. Modified by
/// drag interactions, wire creation/deletion, and the module browser.
pub static RIG_NODE_GRAPH: GlobalSignal<crate::components::rig_grid::node_graph::NodeGraph> =
    Signal::global(|| crate::components::rig_grid::node_graph::NodeGraph::sample_guitar_rig());

/// Saved rig snapshots — captured parameter states that can be recalled later.
pub static RIG_SNAPSHOTS: GlobalSignal<Vec<crate::components::rig_grid::node_graph::RigSnapshot>> =
    Signal::global(Vec::new);
