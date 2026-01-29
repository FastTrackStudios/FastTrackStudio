//! Dioxus integration for rig state management
//!
//! This module provides global signals for rig state synchronization
//! when the `dioxus` feature is enabled.

#[cfg(feature = "dioxus")]
mod dioxus_impl {
    use rig_control::{
        PresetInfo, ProfileInfo, ProfileSceneInfo, RigInfo, SetlistInfo, SongInfo,
    };
    use dioxus::prelude::*;
    use uuid::Uuid;

    // ============================================================================
    // Profile & Rig State
    // ============================================================================

    /// Currently loaded profile info
    pub static RIG_PROFILE: GlobalSignal<Option<ProfileInfo>> = Signal::global(|| None);

    /// Currently loaded rig info
    pub static RIG_INFO: GlobalSignal<Option<RigInfo>> = Signal::global(|| None);

    /// Available profiles
    pub static RIG_AVAILABLE_PROFILES: GlobalSignal<Vec<ProfileInfo>> =
        Signal::global(|| Vec::new());

    // ============================================================================
    // Sections & Blocks (TODO: migrate to rig-control)
    // ============================================================================

    // Note: These are commented out until rig-control provides equivalent types
    // /// Sections in the current rig
    // pub static RIG_SECTIONS: GlobalSignal<Vec<SectionInfo>> = Signal::global(|| Vec::new());
    //
    // /// Global blocks in the current rig
    // pub static RIG_GLOBAL_BLOCKS: GlobalSignal<Vec<GlobalBlockInfo>> =
    //     Signal::global(|| Vec::new());

    // ============================================================================
    // Preset State
    // ============================================================================

    /// Currently loaded preset info
    pub static RIG_CURRENT_PRESET: GlobalSignal<Option<PresetInfo>> = Signal::global(|| None);

    /// Available presets in the current profile
    pub static RIG_AVAILABLE_PRESETS: GlobalSignal<Vec<PresetInfo>> =
        Signal::global(|| Vec::new());

    /// Current preset scene ID (which scene of the preset is active)
    pub static RIG_CURRENT_PRESET_SCENE_ID: GlobalSignal<Option<Uuid>> = Signal::global(|| None);

    /// Current profile scene ID (which scene of the profile is active)
    pub static RIG_CURRENT_PROFILE_SCENE_ID: GlobalSignal<Option<Uuid>> = Signal::global(|| None);

    // ============================================================================
    // Patch State (per layer) (TODO: migrate to rig-control)
    // ============================================================================

    // Note: Commented out until rig-control provides equivalent types
    // /// Available patches (for patch browser)
    // pub static RIG_AVAILABLE_PATCHES: GlobalSignal<Vec<PatchInfo>> =
    //     Signal::global(|| Vec::new());

    // ============================================================================
    // Setlist State
    // ============================================================================

    /// All available setlists
    pub static RIG_AVAILABLE_SETLISTS: GlobalSignal<Vec<SetlistInfo>> =
        Signal::global(|| Vec::new());

    /// Currently loaded setlist info
    pub static RIG_CURRENT_SETLIST: GlobalSignal<Option<SetlistInfo>> = Signal::global(|| None);

    // ============================================================================
    // Performance State (Song/Scene)
    // ============================================================================

    /// All songs in the current setlist
    pub static RIG_SETLIST_SONGS: GlobalSignal<Vec<SongInfo>> = Signal::global(|| Vec::new());

    /// Current song info (if in performance mode)
    pub static RIG_CURRENT_SONG: GlobalSignal<Option<SongInfo>> = Signal::global(|| None);

    /// Current scene info (if in performance mode)
    pub static RIG_CURRENT_SCENE: GlobalSignal<Option<ProfileSceneInfo>> = Signal::global(|| None);

    /// Current song index
    pub static RIG_SONG_INDEX: GlobalSignal<usize> = Signal::global(|| 0);

    /// Current scene index
    pub static RIG_SCENE_INDEX: GlobalSignal<usize> = Signal::global(|| 0);

    // ============================================================================
    // Connection State
    // ============================================================================

    /// Whether connected to rig service
    pub static RIG_CONNECTED: GlobalSignal<bool> = Signal::global(|| false);

    /// Whether rig is loading/transitioning
    pub static RIG_LOADING: GlobalSignal<bool> = Signal::global(|| false);

    // ============================================================================
    // Preload State
    // ============================================================================

    /// Preset IDs that have been preloaded
    pub static RIG_PRELOADED_PRESETS: GlobalSignal<Vec<Uuid>> = Signal::global(|| Vec::new());

    // ============================================================================
    // Module State (for guitar rig grid)
    // ============================================================================

    /// Current modules in the rig (for guitar rig grid UI)
    pub static RIG_CURRENT_MODULES: GlobalSignal<Vec<rig_control::module::Module>> =
        Signal::global(|| Vec::new());
}

#[cfg(feature = "dioxus")]
pub use dioxus_impl::*;
