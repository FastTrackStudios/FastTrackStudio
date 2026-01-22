//! Chart stream types for web viewers
//!
//! These types are shared between native (REAPER) and WASM (web) builds.
//! They contain only serializable data structures, no networking code.

use facet::Facet;
use keyflow::Chart;
use serde::{Deserialize, Serialize};

/// Serializable beat position for cursor/highlight rendering on clients.
///
/// This mirrors `engraver::layout::chart::BeatPosition` but is serializable
/// for network transmission. Clients use this to render playback cursor
/// without needing the full layout engine.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct SerializableBeatPosition {
    /// Page number (1-indexed).
    pub page: u32,
    /// System index on this page (0-indexed).
    pub system: usize,
    /// Global measure index (0-indexed across entire chart).
    pub measure: usize,
    /// Beat/segment index within measure (0-indexed).
    pub beat: usize,
    /// Absolute tick position from song start (for tempo-independent lookup).
    /// Uses 480 ticks per quarter note (MIDI standard).
    pub absolute_tick: i64,
    /// Duration in ticks.
    pub duration_ticks: i32,
    /// Absolute X position on page (in points).
    pub x: f64,
    /// Width of this beat segment (in points).
    pub width: f64,
    /// Y position of the staff top line (in points).
    pub staff_y: f64,
    /// Staff height (in points, for cursor rendering).
    pub staff_height: f64,
    /// Time in seconds from song start.
    pub time_start: f64,
    /// Time in seconds when this beat ends.
    pub time_end: f64,
    /// Glyph codepoint for the primary element (notehead/rest) for highlighting.
    pub glyph_codepoint: Option<char>,
    /// Size of the glyph in spatiums.
    pub glyph_size: f64,
}

impl SerializableBeatPosition {
    /// Check if a given absolute tick falls within this beat.
    #[must_use]
    pub fn contains_tick(&self, tick: i64) -> bool {
        tick >= self.absolute_tick && tick < self.absolute_tick + self.duration_ticks as i64
    }

    /// Get interpolated X position for an absolute tick within this beat.
    #[must_use]
    pub fn x_at_tick(&self, tick: i64) -> f64 {
        if self.duration_ticks <= 0 {
            return self.x;
        }
        let progress =
            ((tick - self.absolute_tick) as f64 / self.duration_ticks as f64).clamp(0.0, 1.0);
        self.x + self.width * progress
    }

    /// Check if a given time falls within this beat.
    #[must_use]
    pub fn contains_time(&self, time: f64) -> bool {
        time >= self.time_start && time < self.time_end
    }

    /// Get interpolated X position for a time within this beat.
    #[must_use]
    pub fn x_at_time(&self, time: f64) -> f64 {
        if self.time_end <= self.time_start {
            return self.x;
        }
        let progress =
            ((time - self.time_start) / (self.time_end - self.time_start)).clamp(0.0, 1.0);
        self.x + self.width * progress
    }
}

/// Page layout information for multi-page charts.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct SerializablePageLayout {
    /// Page number (1-indexed).
    pub number: u32,
    /// Page width in points.
    pub width: f64,
    /// Page height in points.
    pub height: f64,
}

/// Chart data with playback information for web viewers.
///
/// Sent on initial connection and when the chart changes.
/// Contains everything needed for client-side layout and rendering.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct ChartWithPlayback {
    /// Project/song name for display.
    pub project_name: String,
    /// The parsed chart data.
    pub chart: Chart,
    /// Pre-computed beat positions for cursor rendering.
    /// Sorted by absolute_tick for efficient binary search.
    pub beat_positions: Vec<SerializableBeatPosition>,
    /// Page layouts (dimensions for multi-page rendering).
    pub pages: Vec<SerializablePageLayout>,
    /// Current tempo in BPM.
    pub tempo: f64,
    /// Song start time in project seconds (for relative position calculation).
    pub song_start_seconds: f64,
    /// Song end time in project seconds.
    pub song_end_seconds: f64,
    /// Time signature numerator.
    pub time_sig_num: u8,
    /// Time signature denominator.
    pub time_sig_denom: u8,
}

impl ChartWithPlayback {
    /// Find the beat position at a given absolute tick.
    /// Uses binary search for efficiency.
    #[must_use]
    pub fn beat_at_tick(&self, tick: i64) -> Option<&SerializableBeatPosition> {
        let idx = self
            .beat_positions
            .partition_point(|b| b.absolute_tick + b.duration_ticks as i64 <= tick);
        self.beat_positions
            .get(idx)
            .filter(|b| b.contains_tick(tick))
    }

    /// Find the beat position at a given time.
    #[must_use]
    pub fn beat_at_time(&self, time: f64) -> Option<&SerializableBeatPosition> {
        let idx = self.beat_positions.partition_point(|b| b.time_end <= time);
        self.beat_positions
            .get(idx)
            .filter(|b| b.contains_time(time))
    }
}

/// Lightweight playback position update for streaming during playback.
///
/// Sent at ~30Hz during playback to update cursor position.
/// Small payload (~32 bytes) for minimal latency.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct PlaybackPositionMessage {
    /// Current playback position in project seconds.
    pub position_seconds: f64,
    /// Whether playback is active.
    pub is_playing: bool,
    /// Whether playback is paused.
    pub is_paused: bool,
    /// Current tempo in BPM (may change during playback with tempo track).
    pub tempo: f64,
}

/// Stream messages for web chart viewer.
///
/// Clients subscribe and receive a continuous stream of these messages.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
#[repr(u8)]
pub enum ChartStreamMessage {
    /// Full chart update with playback data (sent on connect and when chart changes).
    ChartUpdate(ChartWithPlayback),
    /// Lightweight playback position update (sent during playback at ~30Hz).
    PlaybackPosition(PlaybackPositionMessage),
    /// Chart was closed/unloaded.
    ChartClosed,
}
