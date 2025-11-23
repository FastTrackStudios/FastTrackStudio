//! Dioxus integration for setlist state management
//! 
//! This module provides global signals for setlist state synchronization
//! when the `dioxus` feature is enabled.

#[cfg(feature = "dioxus")]
mod dioxus_impl {
    use dioxus::prelude::*;
    use crate::Setlist;

    // ============================================================================
    // Setlist - Full setlist struct
    // ============================================================================

    /// Full setlist struct - contains all setlist data
    pub static SETLIST: GlobalSignal<Option<Setlist>> = Signal::global(|| None);

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
