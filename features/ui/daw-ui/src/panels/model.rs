//! Shared view-model for the top-level DAW panels.
//!
//! [`TrackView`] is a single track as the three panels see it — a reusable,
//! self-contained model (no `daw-proto` dependency) so the panels can drive
//! any host's data. Mutable per-track state lives in `Signal`s (so a fader
//! moved in the mixer also moves in the TCP), while layout/metadata is owned.

use dioxus::prelude::*;

/// One clip/item on a track's arrangement lane.
#[derive(Clone, PartialEq)]
pub struct ClipView {
    /// Start position, in seconds on the timeline.
    pub start: f64,
    /// Length, in seconds.
    pub length: f64,
    pub name: String,
    /// `#rrggbb`; falls back to the track colour when `None`.
    pub color: Option<String>,
    /// Fade-in length in seconds (0 = none); drawn REAPER-style as a fade
    /// triangle at the item head.
    pub fade_in: f64,
    /// Fade-out length in seconds (0 = none).
    pub fade_out: f64,
    pub selected: bool,
    pub muted: bool,
}

impl ClipView {
    pub fn new(start: f64, length: f64, name: impl Into<String>, color: Option<&str>) -> Self {
        Self {
            start,
            length,
            name: name.into(),
            color: color.map(|c| c.to_string()),
            fade_in: 0.0,
            fade_out: 0.0,
            selected: false,
            muted: false,
        }
    }
}

/// A project marker on the ruler's marker lane.
#[derive(Clone, PartialEq)]
pub struct MarkerView {
    /// Position in seconds.
    pub time: f64,
    pub name: String,
    /// `#rrggbb`; theme marker colour when `None`.
    pub color: Option<String>,
    /// Marker number (REAPER shows it in the flag).
    pub idx: u32,
}

/// A project region band on the ruler's region lane.
#[derive(Clone, PartialEq)]
pub struct RegionView {
    /// Start/end in seconds.
    pub start: f64,
    pub end: f64,
    pub name: String,
    /// `#rrggbb`; theme region colour when `None`.
    pub color: Option<String>,
    pub idx: u32,
}

/// A track as the TrackControlPanel, MixerControlPanel, and ArrangeView all
/// see it. Shared `Signal`s keep the three panels in sync.
#[derive(Clone, PartialEq)]
pub struct TrackView {
    pub id: usize,
    pub name: String,
    /// Track colour (`#rrggbb`). Tints the strip, TCP row, and lane.
    pub color: Option<String>,

    // ── live, shared state ──
    /// Normalized fader position (0–1).
    pub fader: Signal<f32>,
    /// Pan (bipolar; 0.5 = centre).
    pub pan: Signal<f32>,
    pub mute: Signal<bool>,
    pub solo: Signal<bool>,
    pub record_arm: Signal<bool>,
    /// Track selection (drives the selected row/strip styling).
    pub selected: Signal<bool>,

    // ── metering (live inputs) ──
    /// Left/mono meter level (linear, 0–1). A `Signal` so a host can push live
    /// levels each frame and only the meters re-render.
    pub level: Signal<f32>,
    /// Right meter level (linear, 0–1); only shown when `stereo` is set.
    pub level_right: Signal<f32>,
    /// Peak-hold marker (linear, 0–1).
    pub peak: Signal<f32>,
    /// Whether to render two meter columns (stereo) vs one (mono).
    pub stereo: bool,

    // ── routing flags (for the mixer routing button) ──
    pub sends: bool,
    pub receives: bool,

    // ── hierarchy + layout ──
    /// Absolute folder depth: 0 = top level, 1 = inside one folder, etc.
    pub depth: u32,
    /// Whether this track is a folder parent (renders a folder header).
    pub is_folder: bool,
    /// Lane height in px (TCP rows + arrange lanes share this so they align).
    pub height: u32,

    // ── arrangement ──
    pub clips: Vec<ClipView>,
}

impl TrackView {
    /// Convenience constructor for showcase/sample data. Signals are created
    /// from the given initial values; the caller owns them thereafter.
    #[allow(clippy::too_many_arguments)]
    pub fn new(id: usize, name: impl Into<String>, color: Option<&str>) -> Self {
        Self {
            id,
            name: name.into(),
            color: color.map(|c| c.to_string()),
            fader: Signal::new(0.75),
            pan: Signal::new(0.5),
            mute: Signal::new(false),
            solo: Signal::new(false),
            record_arm: Signal::new(false),
            selected: Signal::new(false),
            level: Signal::new(0.0),
            level_right: Signal::new(0.0),
            peak: Signal::new(0.0),
            stereo: false,
            sends: false,
            receives: false,
            depth: 0,
            is_folder: false,
            height: DEFAULT_LANE_HEIGHT,
            clips: Vec::new(),
        }
    }

    /// Builder: set the initial fader position.
    pub fn fader(mut self, v: f32) -> Self {
        self.fader = Signal::new(v);
        self
    }
    /// Builder: set folder depth (0 = top level).
    pub fn depth(mut self, depth: u32) -> Self {
        self.depth = depth;
        self
    }
    /// Builder: mark as a folder parent.
    pub fn folder(mut self) -> Self {
        self.is_folder = true;
        self
    }
    /// Builder: set lane/row height in px.
    pub fn height(mut self, h: u32) -> Self {
        self.height = h;
        self
    }
    /// Builder: set initial stereo metering levels (+ peak hold) and mark the
    /// track stereo. Hosts with live metering push into the `level` signals
    /// directly instead.
    pub fn levels(mut self, left: f32, right: f32, peak: f32) -> Self {
        self.level = Signal::new(left);
        self.level_right = Signal::new(right);
        self.peak = Signal::new(peak);
        self.stereo = true;
        self
    }
    /// Builder: mark this track as stereo (two meter columns) without setting
    /// initial levels — for hosts that push live levels into the signals.
    pub fn stereo(mut self) -> Self {
        self.stereo = true;
        self
    }
    /// Builder: attach arrangement clips.
    pub fn clips(mut self, clips: Vec<ClipView>) -> Self {
        self.clips = clips;
        self
    }
    /// Builder: routing flags.
    pub fn routing(mut self, sends: bool, receives: bool) -> Self {
        self.sends = sends;
        self.receives = receives;
        self
    }

    /// Resolved track colour (`#rrggbb`), with a neutral fallback.
    pub fn hex(&self) -> String {
        self.color.clone().unwrap_or_else(|| NEUTRAL.to_string())
    }
}

/// Default lane / TCP-row height in px.
pub const DEFAULT_LANE_HEIGHT: u32 = 72;
/// Neutral accent when a track has no colour.
pub const NEUTRAL: &str = "#a1a1aa";
