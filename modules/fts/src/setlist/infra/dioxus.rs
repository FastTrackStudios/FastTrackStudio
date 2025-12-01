//! Dioxus integration for setlist state management
//! 
//! This module provides global signals for setlist state synchronization
//! when the `dioxus` feature is enabled.

#[cfg(feature = "dioxus")]
mod dioxus_impl {
use dioxus::prelude::*;
    use crate::SetlistApi;
    use std::collections::HashMap;

// ============================================================================
    // Setlist - Full setlist API with computed fields
// ============================================================================

    /// Full setlist API - contains setlist data and computed fields (active song, etc.)
    pub static SETLIST: GlobalSignal<Option<SetlistApi>> = Signal::global(|| None);

// ============================================================================
    // Transport Info Per Project - Transport state for each project (multiple songs can play at once)
// ============================================================================

    /// Transport information for a specific project
    #[derive(Debug, Clone, PartialEq)]
    pub struct ProjectTransportInfo {
        pub position_seconds: f64,
        pub tempo_bpm: f64,
        pub time_sig_num: i32,
        pub time_sig_den: i32,
        pub is_playing: bool,
        pub is_looping: bool,
    }

    impl ProjectTransportInfo {
        pub fn new() -> Self {
            Self {
                position_seconds: 0.0,
                tempo_bpm: 120.0,
                time_sig_num: 4,
                time_sig_den: 4,
                is_playing: false,
                is_looping: false,
            }
        }
    }

    impl Default for ProjectTransportInfo {
        fn default() -> Self {
            Self::new()
        }
    }

    /// Transport info per project name - allows multiple songs to have their own transport state
    pub static TRANSPORT_INFO: GlobalSignal<HashMap<String, ProjectTransportInfo>> = Signal::global(|| HashMap::new());

// ============================================================================
    // Transport State - Playback and recording state (needed for computing active song/section)
// ============================================================================

/// Whether transport is currently playing
pub static IS_PLAYING: GlobalSignal<bool> = Signal::global(|| false);

/// Whether transport is currently recording
pub static IS_RECORDING: GlobalSignal<bool> = Signal::global(|| false);

/// Current transport position in seconds
pub static CURRENT_POSITION_SECONDS: GlobalSignal<f64> = Signal::global(|| 0.0);

/// Current transport position in beats
pub static CURRENT_POSITION_BEATS: GlobalSignal<f64> = Signal::global(|| 0.0);

/// Current tempo (BPM)
pub static CURRENT_TEMPO: GlobalSignal<f64> = Signal::global(|| 120.0);

/// Current time signature numerator (e.g., 4 for 4/4)
pub static CURRENT_TIME_SIG_NUMERATOR: GlobalSignal<i32> = Signal::global(|| 4);

/// Current time signature denominator (e.g., 4 for 4/4)
pub static CURRENT_TIME_SIG_DENOMINATOR: GlobalSignal<i32> = Signal::global(|| 4);

    // ============================================================================
    // Active Song Index - Which song is currently active (needed for computation)
    // ============================================================================

    /// Index of the currently active song (None if no song is active)
    pub static ACTIVE_SONG_INDEX: GlobalSignal<Option<usize>> = Signal::global(|| None);

    /// Index of the currently active slide (for lyrics view)
    pub static ACTIVE_SLIDE_INDEX: GlobalSignal<Option<usize>> = Signal::global(|| None);

    // ============================================================================
    // Granular Signals - Separate signals for tracks and transport per song
    // ============================================================================

    /// Tracks per song index - only updates when tracks change for a specific song
    /// Key: song_index, Value: tracks for that song
    pub static SONG_TRACKS: GlobalSignal<HashMap<usize, Vec<daw::tracks::Track>>> = Signal::global(|| HashMap::new());

    /// Transport per song index - only updates when transport changes for a specific song
    /// Key: song_index, Value: transport state for that song
    pub static SONG_TRANSPORT: GlobalSignal<HashMap<usize, daw::transport::Transport>> = Signal::global(|| HashMap::new());

    /// Setlist structure (songs, sections, metadata) - only updates when structure changes
    /// This excludes tracks and transport which are in separate signals
    pub static SETLIST_STRUCTURE: GlobalSignal<Option<crate::setlist::core::Setlist>> = Signal::global(|| None);

    /// Active indices (song, section, slide) - updates frequently during playback
    pub static ACTIVE_INDICES: GlobalSignal<(Option<usize>, Option<usize>, Option<usize>)> = Signal::global(|| (None, None, None));

    // ============================================================================
    // App State - Application state like counting in, loop, etc.
    // ============================================================================

    /// Whether FTS is currently counting in
    pub static IS_COUNTING_IN: GlobalSignal<bool> = Signal::global(|| false);

    /// Whether the loop bracket is enabled
    pub static LOOP_ENABLED: GlobalSignal<bool> = Signal::global(|| false);

    /// The loop's start position in beats
    pub static LOOP_START: GlobalSignal<f64> = Signal::global(|| 0.0);

    /// The loop's end position in beats
    pub static LOOP_END: GlobalSignal<f64> = Signal::global(|| 0.0);

    /// Queued song and section names
    pub static QUEUED_NAME: GlobalSignal<(String, String)> = Signal::global(|| (String::new(), String::new()));

    /// Queued song and section indices
    pub static QUEUED_INDEX: GlobalSignal<(usize, usize)> = Signal::global(|| (0, 0));
}

#[cfg(feature = "dioxus")]
pub use dioxus_impl::*;
