//! Transport streaming event types.
//!
//! Two categories, matching the helgobox ProtoHub split:
//!
//! - **`TransportEvent`** — *occasional*. Emitted on every discrete
//!   state transition (play, stop, record, loop toggle, tempo
//!   change). Every event matters; subscribers shouldn't miss any.
//!
//! - **`PositionTick`** — *continuous*. Pushed at ~30Hz from the
//!   REAPER main thread. Subscribers may drop intermediate ticks
//!   under backpressure — the next tick is "close enough."
//!
//! The two streams are separate so a sleepy web-UI client doesn't
//! accumulate hundreds of position samples in its queue. See
//! `docs/streaming-design.md`.

use crate::primitives::{MusicalPosition, Tempo, TimeSignature};
use crate::transport::transport::{LoopRegion, PlayState, RecordMode, Transport};
use facet::Facet;

// ── Occasional: state transitions ────────────────────────────────────

/// A discrete change to transport state. Emitted whenever the user
/// (or an action) transitions REAPER's transport — play, stop,
/// record, loop on/off, tempo change, time signature change.
///
/// Carries the project the event applies to so a single subscriber
/// can fan out updates across all open projects.
#[derive(Clone, Debug, Facet)]
#[repr(u8)]
pub enum TransportEvent {
    /// Play state transitioned (Stopped → Playing → Paused → Recording).
    PlayStateChanged {
        project_guid: String,
        play_state: PlayState,
    },
    /// Recording mode toggled.
    RecordModeChanged {
        project_guid: String,
        record_mode: RecordMode,
    },
    /// Loop on/off toggled.
    LoopingChanged { project_guid: String, looping: bool },
    /// Loop region edited.
    LoopRegionChanged {
        project_guid: String,
        loop_region: Option<LoopRegion>,
    },
    /// Time-selection region edited.
    TimeSelectionChanged {
        project_guid: String,
        time_selection: Option<LoopRegion>,
    },
    /// Tempo or time signature changed.
    TempoChanged {
        project_guid: String,
        tempo: Tempo,
        time_signature: TimeSignature,
    },
    /// Playrate changed.
    PlayrateChanged { project_guid: String, playrate: f64 },
    /// Full state snapshot — emitted on initial subscribe and on
    /// project switch, so clients can sync without re-querying.
    Snapshot {
        project_guid: String,
        state: Transport,
    },
}

// ── Continuous: position ticks ───────────────────────────────────────

/// A position sample. Pushed at ~30Hz by the REAPER main thread.
/// `playhead_position` is the playback cursor in seconds; the loop
/// pulls it via `GetPlayPosition()` (or the equivalent for the
/// project context).
///
/// `qn_position` is the same cursor in quarter notes — provided so
/// musical-time UIs don't need to do their own time-map conversion
/// on every tick.
#[derive(Clone, Debug, Facet)]
pub struct PositionTick {
    /// Project this tick refers to. The streaming hub emits one tick
    /// per open project per cycle; subscribers that only care about
    /// the active project filter on this field.
    pub project_guid: String,
    /// Cursor position in seconds.
    pub playhead_seconds: f64,
    /// Cursor position in quarter notes.
    pub playhead_qn: f64,
    /// Whether transport is currently playing (so a single subscriber
    /// can stop drawing the playhead when stopped without subscribing
    /// to `TransportEvent` separately).
    pub is_playing: bool,
    /// Edit cursor position in seconds — independent of playhead.
    /// While stopped these match; while playing the playhead moves
    /// and the edit cursor stays put (unless `Options: Edit cursor
    /// follows playback` is on).
    pub edit_cursor_seconds: f64,
    /// Edit cursor position in quarter notes.
    pub edit_cursor_qn: f64,
    /// Playhead position in measures/beats/subdivisions, computed by
    /// the DAW with `projmeasoffs` applied. Clients should display
    /// this directly rather than recomputing from `playhead_qn`.
    pub playhead_musical: MusicalPosition,
    /// Edit cursor position in measures/beats/subdivisions, with
    /// `projmeasoffs` applied. Same caveat as `playhead_musical`.
    pub edit_cursor_musical: MusicalPosition,
}

impl PositionTick {
    pub fn stopped_at_origin() -> Self {
        Self {
            project_guid: String::new(),
            playhead_seconds: 0.0,
            playhead_qn: 0.0,
            is_playing: false,
            edit_cursor_seconds: 0.0,
            edit_cursor_qn: 0.0,
            playhead_musical: MusicalPosition::ZERO,
            edit_cursor_musical: MusicalPosition::ZERO,
        }
    }
}

/// Unified event delivered by `Transport::subscribe`. Carries either
/// a discrete `State` change or a continuous `Position` tick — the
/// subscription's filter decides which kinds are forwarded.
#[derive(Clone, Debug, Facet)]
#[repr(u8)]
pub enum TransportStreamEvent {
    State(TransportEvent),
    Position(PositionTick),
}

/// Per-subscriber filter for `Transport::subscribe`. Toggle each
/// kind on or off; defaults to state-only since most consumers care
/// about transitions but not every tick.
#[derive(Clone, Copy, Debug, Facet)]
pub struct TransportSubscription {
    /// Include discrete transport-state events (play/stop/tempo/etc.).
    pub state: bool,
    /// Include continuous ~30Hz position ticks.
    pub position: bool,
}

impl Default for TransportSubscription {
    fn default() -> Self {
        Self::state_only()
    }
}

impl TransportSubscription {
    pub const fn all() -> Self {
        Self {
            state: true,
            position: true,
        }
    }
    pub const fn state_only() -> Self {
        Self {
            state: true,
            position: false,
        }
    }
    pub const fn position_only() -> Self {
        Self {
            state: false,
            position: true,
        }
    }
}

#[cfg(feature = "vox")]
#[allow(unsafe_code)]
mod reborrow_impls {
    use super::{PositionTick, TransportEvent, TransportStreamEvent, TransportSubscription};
    unsafe impl vox_types::Reborrow for TransportEvent {
        type Ref<'a> = TransportEvent;
    }
    unsafe impl vox_types::Reborrow for PositionTick {
        type Ref<'a> = PositionTick;
    }
    unsafe impl vox_types::Reborrow for TransportStreamEvent {
        type Ref<'a> = TransportStreamEvent;
    }
    unsafe impl vox_types::Reborrow for TransportSubscription {
        type Ref<'a> = TransportSubscription;
    }
}
