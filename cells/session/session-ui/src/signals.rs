//! Global Domain Signals and Session Access
//!
//! This module defines the global signals that represent the domain state for the UI,
//! and provides the `Session` singleton for accessing service clients.
//!
//! ## Signal Architecture
//!
//! - `SETLIST_STRUCTURE`: Song/section structure (updates infrequently)
//! - `ACTIVE_INDICES`: Current playback position (song/section indices, progress)
//! - `SONG_TRANSPORT`: Per-song transport state (playhead, tempo, time signature)
//! - `PLAYBACK_STATE`: Global playback state (playing, paused, stopped)
//!
//! Components subscribe only to the signals they need, preventing unnecessary rerenders.
//!
//! ## Session Access
//!
//! Service clients are accessed via `Session::get()`, following the same pattern as
//! `Daw::get()` in daw-control:
//!
//! ```rust,ignore
//! // During app startup (or on reconnection)
//! Session::init(setlist_client);
//!
//! // In UI components
//! let session = Session::get();
//! session.setlist().play().await;
//! ```

use daw_proto::PlayState;
use dioxus::prelude::*;
use session_proto::{ActiveIndices, Setlist, SetlistServiceClient};
use std::collections::HashMap;

#[cfg(target_arch = "wasm32")]
use std::cell::RefCell;

#[cfg(not(target_arch = "wasm32"))]
use std::sync::OnceLock;

/// Global setlist structure (songs, sections, timing)
/// Updates when setlist is rebuilt or structure changes
pub static SETLIST_STRUCTURE: GlobalSignal<Setlist> = Signal::global(|| Setlist::default());

/// Current playback position and progress
/// Updates frequently during playback (10-60 times per second)
pub static ACTIVE_INDICES: GlobalSignal<ActiveIndices> =
    Signal::global(|| ActiveIndices::default());

/// Per-song transport state (playhead position, tempo, time signature)
/// Key is song index, updates when transport state changes for that song
pub static SONG_TRANSPORT: GlobalSignal<HashMap<usize, TransportState>> =
    Signal::global(HashMap::new);

/// Global playback state
/// Updates when play/pause/stop state changes
pub static PLAYBACK_STATE: GlobalSignal<PlayState> = Signal::global(|| PlayState::Stopped);

// ============================================================================
// Session singleton
// ============================================================================

/// Thread-local storage for the Session (allows re-initialization on reconnect)
/// We use thread_local + RefCell because:
/// 1. WASM is single-threaded, so thread_local is effectively global
/// 2. RefCell allows interior mutability for replacing the client on reconnect
#[cfg(target_arch = "wasm32")]
thread_local! {
    static GLOBAL_SESSION: RefCell<Option<Session>> = const { RefCell::new(None) };
}

/// For non-WASM targets, use OnceLock (no reconnection support needed for tests/native)
#[cfg(not(target_arch = "wasm32"))]
static GLOBAL_SESSION: OnceLock<Session> = OnceLock::new();

/// Session provides access to session service clients
///
/// Similar to `Daw::get()` in daw-control, this provides a global singleton
/// for accessing service clients from UI components.
///
/// # Reconnection Support
///
/// On WASM targets, `init()` can be called multiple times to update the client
/// after a reconnection. On native targets, it can only be called once.
///
/// # Example
///
/// ```rust,ignore
/// use session_ui::Session;
///
/// // During app startup (or on reconnection)
/// Session::init(setlist_client);
///
/// // In UI components - call service methods directly
/// Session::with(|session| {
///     session.setlist().play().await;
/// });
/// ```
#[derive(Clone)]
pub struct Session {
    setlist_client: SetlistServiceClient,
}

impl Session {
    /// Initialize or reinitialize the global Session with service clients.
    ///
    /// On WASM: Can be called multiple times (replaces the existing client).
    /// On native: Can only be called once.
    ///
    /// # Errors
    ///
    /// Returns an error if already initialized (native only).
    #[cfg(target_arch = "wasm32")]
    pub fn init(setlist_client: SetlistServiceClient) -> eyre::Result<()> {
        GLOBAL_SESSION.with(|cell| {
            *cell.borrow_mut() = Some(Session { setlist_client });
        });
        Ok(())
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub fn init(setlist_client: SetlistServiceClient) -> eyre::Result<()> {
        GLOBAL_SESSION
            .set(Session { setlist_client })
            .map_err(|_| eyre::eyre!("Session already initialized"))
    }

    /// Get the global Session instance.
    ///
    /// # Panics
    ///
    /// Panics if `init()` has not been called.
    #[cfg(target_arch = "wasm32")]
    pub fn get() -> Session {
        GLOBAL_SESSION.with(|cell| {
            cell.borrow()
                .clone()
                .expect("Session not initialized. Call Session::init() first.")
        })
    }

    #[cfg(not(target_arch = "wasm32"))]
    pub fn get() -> &'static Session {
        GLOBAL_SESSION
            .get()
            .expect("Session not initialized. Call Session::init() first.")
    }

    /// Get the SetlistService client
    pub fn setlist(&self) -> &SetlistServiceClient {
        &self.setlist_client
    }
}

// ============================================================================
// Transport State
// ============================================================================

/// Simplified transport state for UI display
#[derive(Debug, Clone, Default, PartialEq)]
pub struct TransportState {
    /// Current position in seconds
    pub position: f64,
    /// Current tempo in BPM
    pub bpm: f64,
    /// Time signature numerator
    pub time_sig_num: i32,
    /// Time signature denominator
    pub time_sig_denom: i32,
    /// Whether this song's project is currently playing
    pub is_playing: bool,
    /// Whether this song is currently looping
    pub is_looping: bool,
    /// Loop region start/end (if looping), as percentages (0.0-1.0)
    pub loop_region: Option<(f64, f64)>,
}

impl TransportState {
    /// Create a new transport state
    pub fn new(position: f64, bpm: f64, time_sig_num: i32, time_sig_denom: i32) -> Self {
        Self {
            position,
            bpm,
            time_sig_num,
            time_sig_denom,
            is_playing: false,
            is_looping: false,
            loop_region: None,
        }
    }

    /// Set loop region (as time values in seconds)
    pub fn with_loop_region(mut self, start: f64, end: f64, song_duration: f64) -> Self {
        self.is_looping = true;
        self.loop_region = Some((start / song_duration, end / song_duration));
        self
    }
}
