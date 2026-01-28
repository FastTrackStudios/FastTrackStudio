//! Chart Window - Docked window displaying parsed chart from Keyflow
//!
//! Displays a parsed chart in a docked REAPER window using engraver's
//! ChartLayoutEngine and VelloSceneRenderer for proper music notation rendering.
//!
//! Features:
//! - Dynamic playhead cursor synchronized with REAPER transport via FTS setlist
//! - Element highlighting (chords/notes) during playback
//! - Auto-follow modes (page-turn, smooth scroll)
//! - Performance-optimized rendering (120+ fps target)

use std::cell::RefCell;
use std::ptr::NonNull;
use std::sync::{Arc, Mutex, RwLock};

use daw::primitives::TimeSignature;
use keyflow::engraver::fonts::ChartFontBundle;
use keyflow::engraver::layout::chart::{
    BeatPosition, ChartCursor, ChartLayoutConfig, ChartLayoutEngine, ChartLayoutResult,
    CursorConfig, CursorState, CursorStyle as KeyflowCursorStyle, HighlightCommand, LayoutMode,
};
use keyflow::engraver::renderer::scene_renderer::{SceneRenderBuilder, VelloSceneRenderer};
use keyflow::engraver::style::MStyle;
use fts::setlist::core::{Song, SongKeyChange};
use fts::setlist::infra::traits::SetlistBuilder;
use keyflow::Chart;
use keyflow::sections::SectionType;
use keyflow::time::{AbsolutePosition, MusicalPosition};
use raw_window_handle::{
    AppKitDisplayHandle, AppKitWindowHandle, DisplayHandle, HandleError, HasDisplayHandle,
    HasWindowHandle, RawDisplayHandle, RawWindowHandle, WindowHandle,
};
use reaper_embed::{Event, EventStatus, MouseButton, MouseEvent, ScrollDelta};
use reaper_high::Reaper;
use reaper_low::raw::HWND;
use tracing::{info, warn};
use vello::Scene;
use vello::kurbo::{Affine, BezPath, Point, Rect, RoundedRect, Stroke};
use vello::peniko::{Color, Fill};

use crate::infrastructure::action_registry::{ActionDef, ActionSection};
use crate::chart::{KeyEntry, read_all_keys_from_track};
use crate::services::{
    find_track_by_name, generate_chart_text_from_reaper, get_beats_at_time,
    get_first_midi_take, get_internal_measure_at_time, get_project_time_signature,
    read_midi_notes_from_take,
};
use reaper_embed::{DockedWindow, ReaperWindow, VelloEmbedSourceWithEvents, WindowConfig};

// ============================================================================
// Shared Playback State (Updated by Timer, Read by Chart Window)
// ============================================================================

/// Shared playback state populated by the timer callback from FTS setlist infrastructure.
/// This decouples the chart window from direct REAPER API calls.
#[derive(Debug, Clone, Default)]
pub struct SharedPlaybackState {
    /// Current playhead position in seconds (from setlist transport)
    pub position_seconds: f64,
    /// Whether REAPER is currently playing
    pub is_playing: bool,
    /// Whether REAPER is paused
    pub is_paused: bool,
    /// Whether REAPER is recording
    pub is_recording: bool,
    /// Active song index in the setlist (if any)
    pub active_song_index: Option<usize>,
    /// Active section index within the song (if any)
    pub active_section_index: Option<usize>,
    /// Song progress (0.0 - 1.0) within the active song
    pub song_progress: f64,
    /// Section progress (0.0 - 1.0) within the active section
    pub section_progress: f64,
    /// Current tempo (BPM)
    pub tempo: f64,
    /// Current time signature
    pub time_signature: TimeSignature,
    /// Song start time (for calculating relative position)
    pub song_start_seconds: f64,
    /// Song end time
    pub song_end_seconds: f64,
    /// Frame counter (for detecting updates)
    pub frame: u64,
}

/// Global shared playback state - updated by timer, read by chart window
static SHARED_PLAYBACK: RwLock<SharedPlaybackState> = RwLock::new(SharedPlaybackState {
    position_seconds: 0.0,
    is_playing: false,
    is_paused: false,
    is_recording: false,
    active_song_index: None,
    active_section_index: None,
    song_progress: 0.0,
    section_progress: 0.0,
    tempo: 120.0,
    time_signature: TimeSignature {
        numerator: 4,
        denominator: 4,
    },
    song_start_seconds: 0.0,
    song_end_seconds: 0.0,
    frame: 0,
});

/// Update the shared playback state (called from timer callback)
pub fn update_shared_playback_state(
    position_seconds: f64,
    is_playing: bool,
    is_paused: bool,
    is_recording: bool,
    active_song_index: Option<usize>,
    active_section_index: Option<usize>,
    song_progress: f64,
    section_progress: f64,
    tempo: f64,
    time_signature: TimeSignature,
    song_start_seconds: f64,
    song_end_seconds: f64,
) {
    if let Ok(mut state) = SHARED_PLAYBACK.write() {
        state.position_seconds = position_seconds;
        state.is_playing = is_playing;
        state.is_paused = is_paused;
        state.is_recording = is_recording;
        state.active_song_index = active_song_index;
        state.active_section_index = active_section_index;
        state.song_progress = song_progress;
        state.section_progress = section_progress;
        state.tempo = tempo;
        state.time_signature = time_signature;
        state.song_start_seconds = song_start_seconds;
        state.song_end_seconds = song_end_seconds;
        state.frame += 1;
    }
}

/// Get a snapshot of the shared playback state (called from chart window render)
fn get_shared_playback_state() -> SharedPlaybackState {
    SHARED_PLAYBACK
        .read()
        .map(|s| s.clone())
        .unwrap_or_default()
}

// Fonts are now provided by ChartFontBundle from keyflow

/// Screen DPI for rendering
const SCREEN_DPI: f64 = 96.0;
/// Points per inch (typographical standard)
const POINTS_PER_INCH: f64 = 72.0;
/// DPI scaling factor: converts points to screen pixels
const DPI_SCALE: f64 = SCREEN_DPI / POINTS_PER_INCH;

/// Convert a `[u8; 4]` RGBA color from the keyflow cursor module to a Vello `Color`.
fn rgba_to_color(rgba: &[u8; 4]) -> Color {
    Color::from_rgba8(rgba[0], rgba[1], rgba[2], rgba[3])
}

// ============================================================================
// Playback Cursor Types
// ============================================================================

/// Auto-follow mode during playback
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum FollowMode {
    /// Jump to next system/page when cursor exits viewport (DEFAULT)
    #[default]
    PageTurn,
    /// Smoothly scroll to keep cursor near center
    SmoothScroll,
    /// No automatic following - user scrolls manually
    None,
}

/// Playback transport state tracking REAPER transport.
///
/// Position and geometry fields are now computed by `ChartCursor` and stored
/// in `CursorState`. This struct only tracks transport-level state.
#[derive(Debug, Clone, Default)]
struct PlaybackState {
    /// Current playhead position in seconds
    position_seconds: f64,
    /// Whether REAPER is currently playing
    is_playing: bool,
    /// Whether REAPER is recording
    is_recording: bool,
    /// Whether REAPER is paused
    is_paused: bool,
}

/// Wrapper for REAPER's main window that implements HasWindowHandle
struct ReaperMainWindow {
    hwnd: HWND,
}

impl ReaperMainWindow {
    fn new() -> Option<Self> {
        let reaper = Reaper::get();
        let hwnd = reaper.main_window().as_ptr();
        if hwnd.is_null() {
            None
        } else {
            Some(Self { hwnd })
        }
    }
}

// Implement HasWindowHandle for ReaperMainWindow (macOS)
#[cfg(target_os = "macos")]
impl HasWindowHandle for ReaperMainWindow {
    fn window_handle(&self) -> Result<WindowHandle<'_>, HandleError> {
        let ns_view = self.hwnd as *mut std::ffi::c_void;
        let handle =
            AppKitWindowHandle::new(NonNull::new(ns_view).ok_or(HandleError::Unavailable)?);
        let raw = RawWindowHandle::AppKit(handle);
        Ok(unsafe { WindowHandle::borrow_raw(raw) })
    }
}

#[cfg(target_os = "macos")]
impl HasDisplayHandle for ReaperMainWindow {
    fn display_handle(&self) -> Result<DisplayHandle<'_>, HandleError> {
        let raw = RawDisplayHandle::AppKit(AppKitDisplayHandle::new());
        Ok(unsafe { DisplayHandle::borrow_raw(raw) })
    }
}

#[cfg(target_os = "windows")]
impl HasWindowHandle for ReaperMainWindow {
    fn window_handle(&self) -> Result<WindowHandle<'_>, HandleError> {
        use raw_window_handle::Win32WindowHandle;
        let handle = Win32WindowHandle::new(
            std::num::NonZeroIsize::new(self.hwnd as isize).ok_or(HandleError::Unavailable)?,
        );
        let raw = RawWindowHandle::Win32(handle);
        Ok(unsafe { WindowHandle::borrow_raw(raw) })
    }
}

#[cfg(target_os = "windows")]
impl HasDisplayHandle for ReaperMainWindow {
    fn display_handle(&self) -> Result<DisplayHandle<'_>, HandleError> {
        use raw_window_handle::WindowsDisplayHandle;
        let raw = RawDisplayHandle::Windows(WindowsDisplayHandle::new());
        Ok(unsafe { DisplayHandle::borrow_raw(raw) })
    }
}

#[cfg(target_os = "linux")]
impl HasWindowHandle for ReaperMainWindow {
    fn window_handle(&self) -> Result<WindowHandle<'_>, HandleError> {
        use raw_window_handle::XlibWindowHandle;
        let handle = XlibWindowHandle::new(self.hwnd as std::ffi::c_ulong);
        let raw = RawWindowHandle::Xlib(handle);
        Ok(unsafe { WindowHandle::borrow_raw(raw) })
    }
}

#[cfg(target_os = "linux")]
impl HasDisplayHandle for ReaperMainWindow {
    fn display_handle(&self) -> Result<DisplayHandle<'_>, HandleError> {
        use raw_window_handle::XlibDisplayHandle;
        let raw = RawDisplayHandle::Xlib(XlibDisplayHandle::new(None, 0));
        Ok(unsafe { DisplayHandle::borrow_raw(raw) })
    }
}

/// Chart renderer state
struct ChartRendererState {
    /// Font bundle (single source of truth for all chart fonts)
    font_bundle: ChartFontBundle,
    /// Layout engine
    layout_engine: ChartLayoutEngine,
    /// Current chart
    chart: Option<Chart>,
    /// Layout result (cached)
    layout_result: Option<ChartLayoutResult>,
    /// Last rendered viewport size (for detecting resize)
    last_viewport: (u32, u32),
    /// View transform for pan/zoom
    transform: Affine,
    /// Whether mouse button is held (for drag-to-pan)
    mouse_down: bool,
    /// Last known mouse position
    prior_position: Option<Point>,
    /// Whether to auto-fit on first render
    needs_initial_fit: bool,

    // === Playback Cursor Fields ===
    /// Song reference for time-to-measure mapping
    song: Option<Song>,
    /// Transport state (playing, position)
    playback: PlaybackState,
    /// Renderer-agnostic cursor from keyflow engraver
    chart_cursor: ChartCursor,
    /// Last computed cursor state (updated each frame during playback)
    cursor_state: Option<CursorState>,
    /// Auto-follow mode
    follow_mode: FollowMode,
    /// Last page we jumped to (for page-turn follow)
    last_followed_page: u32,
    /// Song start time offset (for mapping transport position to chart time)
    song_start_offset: f64,
    /// Tempo (BPM) for calculating measure timing
    tempo: f64,
    /// Time signature for calculating measure timing
    time_signature: TimeSignature,

    // === Dynamic Update Fields ===
    /// Last known project state hash (for change detection)
    last_project_hash: u64,
    /// Time of last chart rebuild (for debouncing)
    last_rebuild_time: std::time::Instant,
    /// Minimum time between rebuilds (debounce)
    rebuild_debounce_ms: u64,

    // === Performance Optimization Fields ===
    /// Frame counter for throttling expensive operations
    frame_count: u64,
    /// Last playback position (for dirty detection)
    last_playback_position: f64,
    /// Last transform (for dirty detection)
    last_rendered_transform: Affine,
    /// Whether the static chart content needs re-rendering
    static_dirty: bool,
    /// Last frame time for FPS calculation
    last_frame_time: std::time::Instant,
    /// Rolling average frame time (microseconds)
    avg_frame_time_us: f64,

    // === Rhythm Notation Fields ===
    /// Whether to use stemmed notation (for charts with push/pull timing)
    use_stems: bool,
    /// Project measure offset (from REAPER's projmeasoffs) for correct measure numbering
    measure_offset: i32,
    /// Number of count-in measures (0, 1, or 2) from Count-In marker to SONGSTART
    count_in_measures: u8,
}

impl ChartRendererState {
    fn new(
        chart: Option<Chart>,
        song: Option<Song>,
        use_stems: bool,
        measure_offset: i32,
        count_in_measures: u8,
    ) -> Self {
        // Load font bundle (single source of truth)
        let font_bundle = ChartFontBundle::new().expect("Failed to create chart font bundle");

        // Create layout engine with correct font wiring via bundle
        let style: &'static MStyle = Box::leak(Box::new(MStyle::default()));
        let layout_engine = font_bundle.create_layout_engine(style);

        // Create layout config with stemmed notation, measure offset, and count-in
        let config = ChartLayoutConfig {
            use_stems,
            measure_number_offset: measure_offset,
            count_in_measures,
            ..Default::default()
        };

        // Layout chart if present
        let layout_result = chart
            .as_ref()
            .map(|c| layout_engine.layout_chart_with_config(c, &LayoutMode::default(), &config));

        // Extract timing info from song
        let (song_start_offset, tempo, time_signature) = if let Some(ref s) = song {
            (
                s.effective_start(),
                s.starting_tempo.unwrap_or(120.0),
                s.starting_time_signature
                    .clone()
                    .unwrap_or_else(|| TimeSignature::new(4, 4)),
            )
        } else {
            (0.0, 120.0, TimeSignature::new(4, 4))
        };

        // Initial transform: translate to show content, then apply DPI scale
        let initial_transform = Affine::translate((50.0, 50.0)) * Affine::scale(DPI_SCALE);

        let mut state = Self {
            font_bundle,
            layout_engine,
            chart,
            layout_result,
            last_viewport: (0, 0),
            transform: initial_transform,
            mouse_down: false,
            prior_position: None,
            needs_initial_fit: true,
            // Playback fields
            song,
            playback: PlaybackState::default(),
            chart_cursor: ChartCursor::default(),
            cursor_state: None,
            follow_mode: FollowMode::default(),
            last_followed_page: 0,
            song_start_offset,
            tempo,
            time_signature,
            // Dynamic update fields
            last_project_hash: 0,
            last_rebuild_time: std::time::Instant::now(),
            rebuild_debounce_ms: 200, // 200ms debounce
            // Performance optimization fields
            frame_count: 0,
            last_playback_position: -1.0,
            last_rendered_transform: Affine::IDENTITY,
            static_dirty: true, // Need initial render
            last_frame_time: std::time::Instant::now(),
            avg_frame_time_us: 0.0,
            // Rhythm notation fields
            use_stems,
            measure_offset,
            count_in_measures,
        };

        // Compute initial project hash
        state.last_project_hash = state.compute_project_hash();

        state
    }

    /// Update playhead position from shared FTS setlist state.
    ///
    /// Uses REAPER's `TimeMap2_timeToBeats` API for tempo-accurate beat positions
    /// instead of estimating ticks from time and tempo. This correctly handles
    /// tempo changes, time signature changes, and the project measure offset.
    ///
    /// The chart layout uses 480 ticks per quarter note with tick 0 at SONGSTART.
    /// Count-in measures have negative tick values.
    fn update_playhead(&mut self) {
        // Get shared state (populated by timer from FTS setlist)
        let shared = get_shared_playback_state();

        // Copy transport state
        self.playback.is_playing = shared.is_playing;
        self.playback.is_paused = shared.is_paused;
        self.playback.is_recording = shared.is_recording;
        self.playback.position_seconds = shared.position_seconds;

        // Update tempo/time signature if they've changed
        if shared.tempo > 0.0 && (shared.tempo - self.tempo).abs() > 0.01 {
            self.tempo = shared.tempo;
        }
        if shared.time_signature.numerator != self.time_signature.numerator
            || shared.time_signature.denominator != self.time_signature.denominator
        {
            self.time_signature = shared.time_signature.clone();
        }

        // Update song start offset if the shared state has been populated (non-zero)
        // and differs from our current value. Don't override with 0.0 from uninitialized state.
        if shared.song_start_seconds > 0.001
            && (shared.song_start_seconds - self.song_start_offset).abs() > 0.001
        {
            self.song_start_offset = shared.song_start_seconds;
        }

        // Use REAPER's beat API for tempo-accurate tick calculation.
        // TimeMap2_timeToBeats returns total quarter-note beats from project start,
        // accounting for all tempo and time signature changes.
        let reaper = Reaper::get();
        let project = reaper.current_project();

        let current_beats = get_beats_at_time(project, self.playback.position_seconds);
        let songstart_beats = get_beats_at_time(project, self.song_start_offset);

        // Relative beats from SONGSTART (negative = before SONGSTART, i.e. count-in)
        let relative_beats = current_beats - songstart_beats;

        // Convert to ticks: 480 ticks per quarter note (matches layout engine's PPQ)
        let current_tick = (relative_beats * 480.0) as i64;

        // Compute cursor state via the keyflow ChartCursor abstraction
        if let Some(ref layout) = self.layout_result {
            self.cursor_state = self.chart_cursor.compute(layout, current_tick);
        } else {
            self.cursor_state = None;
        }
    }

    /// Find the beat at a given time position (from layout result)
    fn find_beat_at_time(&self, time: f64) -> Option<&BeatPosition> {
        let reaper = Reaper::get();
        let project = reaper.current_project();
        let current_beats = get_beats_at_time(project, time);
        let songstart_beats = get_beats_at_time(project, self.song_start_offset);
        let relative_beats = current_beats - songstart_beats;
        let tick = (relative_beats * 480.0) as i64;
        self.layout_result.as_ref()?.beat_at_tick(tick)
    }

    /// Render the playback cursor overlay.
    ///
    /// Translates renderer-agnostic `HighlightCommand` values from the keyflow
    /// `ChartCursor` into Vello draw calls.
    fn render_cursor(&self, scene: &mut Scene) {
        // Check if we should show cursor
        let should_show = self.playback.is_playing
            || self.playback.is_paused
            || (self.chart_cursor.config.show_when_stopped && self.cursor_state.is_some());

        if !should_show {
            return;
        }

        let Some(ref state) = self.cursor_state else {
            return;
        };

        for cmd in &state.commands {
            match cmd {
                HighlightCommand::StrokeLine {
                    x,
                    y_top,
                    y_bottom,
                    color,
                    width,
                } => {
                    let top = self.transform * Point::new(*x, *y_top);
                    let bottom = self.transform * Point::new(*x, *y_bottom);
                    let mut path = BezPath::new();
                    path.move_to(top);
                    path.line_to(bottom);
                    scene.stroke(
                        &Stroke::new(*width),
                        Affine::IDENTITY,
                        rgba_to_color(color),
                        None,
                        &path,
                    );
                }
                HighlightCommand::FillRect {
                    x,
                    y,
                    width,
                    height,
                    color,
                } => {
                    let tl = self.transform * Point::new(*x, *y);
                    let br = self.transform * Point::new(*x + *width, *y + *height);
                    let rect = Rect::new(tl.x, tl.y, br.x, br.y);
                    scene.fill(
                        Fill::NonZero,
                        Affine::IDENTITY,
                        rgba_to_color(color),
                        None,
                        &rect,
                    );
                }
                HighlightCommand::FillRoundedRect {
                    x,
                    y,
                    width,
                    height,
                    radius,
                    color,
                } => {
                    let tl = self.transform * Point::new(*x, *y);
                    let br = self.transform * Point::new(*x + *width, *y + *height);
                    let rect = Rect::new(tl.x, tl.y, br.x, br.y);
                    let rounded = RoundedRect::from_rect(rect, *radius);
                    scene.fill(
                        Fill::NonZero,
                        Affine::IDENTITY,
                        rgba_to_color(color),
                        None,
                        &rounded,
                    );
                }
                HighlightCommand::StrokeGlyph {
                    codepoint,
                    font_size,
                    x,
                    y,
                    stroke_width,
                    color,
                } => {
                    if let Some(glyph_path) =
                        self.font_bundle.smufl_font().get_glyph_path(*codepoint, *font_size)
                    {
                        // Font outlines are Y-up; screen is Y-down → flip Y
                        let glyph_transform = self.transform
                            * Affine::translate((*x, *y))
                            * Affine::scale_non_uniform(1.0, -1.0);
                        scene.stroke(
                            &Stroke::new(*stroke_width),
                            glyph_transform,
                            rgba_to_color(color),
                            None,
                            &glyph_path,
                        );
                    }
                }
                HighlightCommand::FillGlyph {
                    codepoint,
                    font_size,
                    x,
                    y,
                    color,
                } => {
                    if let Some(glyph_path) =
                        self.font_bundle.smufl_font().get_glyph_path(*codepoint, *font_size)
                    {
                        let glyph_transform = self.transform
                            * Affine::translate((*x, *y))
                            * Affine::scale_non_uniform(1.0, -1.0);
                        scene.fill(
                            Fill::NonZero,
                            glyph_transform,
                            rgba_to_color(color),
                            None,
                            &glyph_path,
                        );
                    }
                }
            }
        }
    }

    /// Update auto-follow based on cursor position
    fn update_follow(&mut self, viewport_width: f64, _viewport_height: f64) {
        if !self.playback.is_playing {
            return;
        }

        // Extract the values we need before any mutable borrows
        let Some((cs_page, cs_cursor_x)) = self
            .cursor_state
            .as_ref()
            .map(|cs| (cs.page, cs.cursor_x))
        else {
            return;
        };

        match self.follow_mode {
            FollowMode::PageTurn => {
                if cs_page != self.last_followed_page && cs_page > 0 {
                    self.jump_to_page(cs_page);
                    self.last_followed_page = cs_page;
                } else {
                    let cursor_screen_x =
                        (self.transform * Point::new(cs_cursor_x, 0.0)).x;
                    if cursor_screen_x < 0.0 || cursor_screen_x > viewport_width {
                        let scale = self.current_scale();
                        let target_x = -cs_cursor_x * scale + 50.0;
                        let current_y = self.transform.translation().y;
                        self.transform =
                            Affine::translate((target_x, current_y)) * Affine::scale(scale);
                    }
                }
            }
            FollowMode::SmoothScroll => {
                let scale = self.current_scale();
                let target_x = -cs_cursor_x * scale + viewport_width / 2.0;
                let current_x = self.transform.translation().x;
                let new_x = current_x + (target_x - current_x) * 0.1;
                let current_y = self.transform.translation().y;
                self.transform = Affine::translate((new_x, current_y)) * Affine::scale(scale);
            }
            FollowMode::None => {}
        }
    }

    /// Jump to show a specific page
    fn jump_to_page(&mut self, page: u32) {
        if let Some(ref layout) = self.layout_result {
            if let Some(page_layout) = layout.pages.get(page.saturating_sub(1) as usize) {
                // Calculate page X position in layout coordinates
                // Pages are arranged horizontally with gaps
                let page_gap = 40.0;
                let page_x = (page - 1) as f64 * (page_layout.width + page_gap);
                let scale = self.current_scale();
                let target_x = -page_x * scale + 20.0; // 20px margin

                let current_y = self.transform.translation().y;
                self.transform = Affine::translate((target_x, current_y)) * Affine::scale(scale);
            }
        }
    }

    /// Get the current zoom scale from the transform
    fn current_scale(&self) -> f64 {
        // Extract scale from affine transform
        // For a simple scale+translate transform, this is the diagonal element
        let coeffs = self.transform.as_coeffs();
        coeffs[0] // Scale X (assuming uniform scaling)
    }

    // =========================================================================
    // Dynamic Chart Updates
    // =========================================================================

    /// Compute a hash of project state that affects the chart.
    ///
    /// This is used to detect when the project has changed and the chart
    /// needs to be rebuilt.
    fn compute_project_hash(&self) -> u64 {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let reaper = Reaper::get();
        let project = reaper.current_project();
        let mut hasher = DefaultHasher::new();

        // Hash marker/region count (quick check for structural changes)
        let bookmark_count = project.bookmark_count();
        bookmark_count.marker_count.hash(&mut hasher);
        bookmark_count.region_count.hash(&mut hasher);

        // Hash tempo marker count
        if let Some(ref song) = self.song {
            song.tempo_time_sig_changes.len().hash(&mut hasher);
        }

        // Hash CHORDS track state - hash ALL note data to detect any change
        // (pitch changes, position changes, added/removed notes, etc.)
        if let Some(track) = find_track_by_name(project, "CHORDS") {
            true.hash(&mut hasher); // Track exists
            if let Some(midi_info) = get_first_midi_take(&track) {
                let notes = read_midi_notes_from_take(midi_info.take);
                notes.len().hash(&mut hasher);
                // Hash every note's pitch, start, and end to detect any change
                for note in &notes {
                    note.pitch.hash(&mut hasher);
                    note.start_ppq.hash(&mut hasher);
                    note.end_ppq.hash(&mut hasher);
                }
            }
        } else {
            false.hash(&mut hasher); // Track doesn't exist
        }

        // Hash KEY track state for key signature change detection
        let key_entries = read_all_keys_from_track(project);
        key_entries.len().hash(&mut hasher);
        for entry in &key_entries {
            // Hash key name and position (quantized to avoid float comparison issues)
            format!("{}", entry.key).hash(&mut hasher);
            ((entry.position_seconds * 1000.0) as i64).hash(&mut hasher);
        }

        hasher.finish()
    }

    /// Check if the project has changed and rebuild the chart if needed.
    ///
    /// Uses debouncing to avoid rebuilding too frequently during rapid edits.
    fn maybe_rebuild_chart(&mut self) {
        let now = std::time::Instant::now();

        // Check if we're still in debounce period
        let elapsed = now.duration_since(self.last_rebuild_time).as_millis() as u64;
        if elapsed < self.rebuild_debounce_ms {
            return;
        }

        // Compute current project hash
        let current_hash = self.compute_project_hash();

        // If hash hasn't changed, nothing to do
        if current_hash == self.last_project_hash {
            return;
        }

        // Hash changed - rebuild chart
        info!(
            "Project changed (hash {} -> {}), rebuilding chart...",
            self.last_project_hash, current_hash
        );

        if let Some((chart, song, use_stems, measure_offset, count_in_measures)) =
            build_chart_from_current_project()
        {
            // Update timing info
            self.song_start_offset = song.effective_start();
            self.tempo = song.starting_tempo.unwrap_or(120.0);
            self.time_signature = song
                .starting_time_signature
                .clone()
                .unwrap_or_else(|| TimeSignature::new(4, 4));

            // Update stem notation flag, measure offset, and count-in
            self.use_stems = use_stems;
            self.measure_offset = measure_offset;
            self.count_in_measures = count_in_measures;

            // Re-layout chart with config (beat positions are computed during layout)
            let config = ChartLayoutConfig {
                use_stems,
                measure_number_offset: measure_offset,
                count_in_measures,
                ..Default::default()
            };
            let layout = self.layout_engine.layout_chart_with_config(
                &chart,
                &LayoutMode::default(),
                &config,
            );
            let beat_count = layout.beat_positions.len();
            self.layout_result = Some(layout);

            // Store chart and song
            self.chart = Some(chart);
            self.song = Some(song);

            info!(
                "Chart rebuilt: {} beat positions, use_stems: {}, measure_offset: {}, count_in: {}",
                beat_count, use_stems, measure_offset, count_in_measures
            );
        }

        // Update tracking state
        self.last_project_hash = current_hash;
        self.last_rebuild_time = now;
    }

    /// Handle baseview events for zoom/pan
    fn handle_event(&mut self, event: &Event) -> EventStatus {
        const BASE: f64 = 1.05; // 5% per scroll increment
        const PIXELS_PER_LINE: f64 = 20.0;

        match event {
            Event::Mouse(MouseEvent::WheelScrolled { delta, .. }) => {
                // Zoom about cursor position
                if let Some(cursor) = self.prior_position {
                    let delta_y = match delta {
                        ScrollDelta::Lines { y, .. } => *y as f64 * PIXELS_PER_LINE,
                        ScrollDelta::Pixels { y, .. } => *y as f64,
                    };
                    // Negate delta: scroll up (negative delta) = zoom in
                    let exponent = -delta_y / PIXELS_PER_LINE;
                    self.transform = self.transform.then_scale_about(BASE.powf(exponent), cursor);
                }
                EventStatus::Captured
            }
            Event::Mouse(MouseEvent::CursorMoved { position, .. }) => {
                let pos = Point::new(position.x, position.y);
                // Handle drag-to-pan when mouse is held down
                if self.mouse_down {
                    if let Some(prior) = self.prior_position {
                        let delta = pos - prior;
                        self.transform = self.transform.then_translate(delta);
                    }
                }
                self.prior_position = Some(pos);
                EventStatus::Captured
            }
            Event::Mouse(MouseEvent::ButtonPressed {
                button: MouseButton::Left,
                ..
            }) => {
                self.mouse_down = true;
                EventStatus::Captured
            }
            Event::Mouse(MouseEvent::ButtonReleased {
                button: MouseButton::Left,
                ..
            }) => {
                self.mouse_down = false;
                EventStatus::Captured
            }
            Event::Mouse(MouseEvent::CursorLeft) => {
                self.mouse_down = false;
                self.prior_position = None;
                EventStatus::Ignored
            }
            _ => EventStatus::Ignored,
        }
    }

    /// Calculate transform to fit page in viewport while maintaining aspect ratio
    fn calculate_fit_transform(&self, viewport_width: f64, viewport_height: f64) -> Affine {
        if let Some(ref layout) = self.layout_result {
            if !layout.pages.is_empty() {
                let page = &layout.pages[0];
                // Calculate scale to fit page in viewport (with margins)
                let margin = 20.0;
                let available_width = viewport_width - margin * 2.0;
                let available_height = viewport_height - margin * 2.0;

                let scale_x = available_width / page.width;
                let scale_y = available_height / page.height;
                // Use smaller scale to maintain aspect ratio
                let scale = scale_x.min(scale_y);

                // Center the page in the viewport
                let scaled_width = page.width * scale;
                let scaled_height = page.height * scale;
                let offset_x = (viewport_width - scaled_width) / 2.0;
                let offset_y = (viewport_height - scaled_height) / 2.0;

                return Affine::translate((offset_x, offset_y)) * Affine::scale(scale);
            }
        }
        // Fallback to default transform
        Affine::translate((50.0, 50.0)) * Affine::scale(DPI_SCALE)
    }

    fn render(&mut self, scene: &mut Scene, width: u32, height: u32) {
        // === Frame timing start ===
        let frame_start = std::time::Instant::now();
        self.frame_count += 1;

        // === 0. Throttled project change detection (every 10 frames) ===
        if self.frame_count % 10 == 0 {
            self.maybe_rebuild_chart();
        }

        // === 1. Update playhead position from REAPER transport ===
        self.update_playhead();

        // === 2. Update auto-follow (may modify transform) ===
        self.update_follow(width as f64, height as f64);

        // Update viewport tracking
        let viewport_changed = self.last_viewport != (width, height);
        self.last_viewport = (width, height);

        // Auto-fit on first render
        if self.needs_initial_fit && self.layout_result.is_some() {
            self.transform = self.calculate_fit_transform(width as f64, height as f64);
            self.needs_initial_fit = false;
            self.static_dirty = true;
        }

        // === Dirty detection ===
        // Check if transform changed (pan/zoom)
        let transform_changed =
            !transforms_approx_equal(self.transform, self.last_rendered_transform);

        // Check if playback position changed significantly (>1ms movement)
        let playback_changed =
            (self.playback.position_seconds - self.last_playback_position).abs() > 0.001;

        // Mark static as dirty if transform or viewport changed
        if transform_changed || viewport_changed {
            self.static_dirty = true;
        }

        // === 3. Draw background (always needed for Vello) ===
        scene.fill(
            Fill::NonZero,
            Affine::IDENTITY,
            Color::from_rgb8(64, 64, 64),
            None,
            &Rect::new(0.0, 0.0, width as f64, height as f64),
        );

        // === 4. Render static chart content ===
        // Note: With Vello's immediate-mode rendering, we must render every frame
        // but the scene graph itself is cached in layout_result
        if let Some(ref layout) = self.layout_result {
            // Create scene renderer with fonts
            // Arc clones are cheap (atomic increment only)
            let base_renderer = SceneRenderBuilder::new().spatium(5.0).build();
            let mut renderer = self.font_bundle.configure_renderer(base_renderer);

            // Set viewport for culling optimization (skip off-screen nodes)
            renderer.set_viewport(Rect::new(0.0, 0.0, width as f64, height as f64));

            // Render with user's view transform (no scene cloning!)
            renderer.render_with_transform(scene, &layout.scene, self.transform);

            // Log render stats every 60 frames
            if self.frame_count % 60 == 0 {
                let stats = renderer.stats();
                let cull_rate = if stats.nodes_visited > 0 {
                    (stats.nodes_culled_viewport as f64 / stats.nodes_visited as f64) * 100.0
                } else {
                    0.0
                };
                tracing::debug!(
                    "Render stats: {} visited, {} rendered, {} culled ({:.1}% cull rate), {} commands",
                    stats.nodes_visited,
                    stats.nodes_rendered,
                    stats.nodes_culled_viewport,
                    cull_rate,
                    stats.commands_rendered
                );
            }
        }

        // === 5. Render dynamic cursor overlay (on top of static content) ===
        self.render_cursor(scene);

        // === Update dirty tracking state ===
        self.last_rendered_transform = self.transform;
        self.last_playback_position = self.playback.position_seconds;
        self.static_dirty = false;

        // === Frame timing end ===
        let frame_time = frame_start.elapsed().as_micros() as f64;
        // Exponential moving average (alpha = 0.1)
        self.avg_frame_time_us = self.avg_frame_time_us * 0.9 + frame_time * 0.1;

        // Log FPS every 60 frames (roughly once per second at 60fps)
        if self.frame_count % 60 == 0 {
            let fps = 1_000_000.0 / self.avg_frame_time_us;
            tracing::debug!(
                "Chart render: {:.1} fps ({:.2}ms avg), frame {}",
                fps,
                self.avg_frame_time_us / 1000.0,
                self.frame_count
            );
        }

        self.last_frame_time = frame_start;
    }
}

/// Check if two transforms are approximately equal (within epsilon)
fn transforms_approx_equal(a: Affine, b: Affine) -> bool {
    const EPSILON: f64 = 0.0001;
    let a_coeffs = a.as_coeffs();
    let b_coeffs = b.as_coeffs();
    a_coeffs
        .iter()
        .zip(b_coeffs.iter())
        .all(|(a, b)| (a - b).abs() < EPSILON)
}

// Type alias for the docked window
type ChartDockedWindow = DockedWindow;
type ChartFloatingWindow = ReaperWindow;

thread_local! {
    static CHART_WINDOW: RefCell<Option<ChartDockedWindow>> = const { RefCell::new(None) };
    static FLOATING_CHART_WINDOW: RefCell<Option<ChartFloatingWindow>> = const { RefCell::new(None) };
}

/// Build a chart from MIDI chords on the "CHORDS" track using the unified pipeline.
///
/// Populate a Song's `initial_key` and `key_changes` from KEY track entries.
///
/// The first entry becomes `initial_key`; subsequent entries become `key_changes`.
fn populate_song_keys(song: &mut Song, key_entries: &[KeyEntry]) {
    if let Some(first) = key_entries.first() {
        song.initial_key = Some(first.key.clone());
    }
    song.key_changes = key_entries
        .iter()
        .skip(1)
        .map(|e| SongKeyChange {
            position_seconds: e.position_seconds,
            key: e.key.clone(),
        })
        .collect();
}

/// Uses `generate_chart_text_from_reaper()` → `Chart::parse()`, which is the EXACT same
/// pipeline as test 22. This ensures identical chord detection, rhythm analysis, groove
/// pattern detection, and measure formatting.
///
/// Returns the chart and a flag indicating if any chords have push/pull timing
/// (i.e., the chart has rhythmic complexity and should use stemmed notation).
fn build_chart_from_midi_chords(song: &Song) -> Option<(Chart, bool)> {
    let reaper = Reaper::get();
    let project = reaper.current_project();

    // Get key root from the song's initial key for chord respelling
    let key_root = song.initial_key.as_ref().map(|k| k.to_string());
    let title = Some(song.name.as_str());

    // Generate chart text using the same pipeline as test 22
    let chart_text = generate_chart_text_from_reaper(
        project,
        key_root.as_deref(),
        title,
    )?;

    if chart_text.trim().is_empty() {
        warn!("generate_chart_text_from_reaper returned empty text");
        return None;
    }

    // Parse the chart text — identical to how the web app and tests do it
    let chart = match Chart::parse(&chart_text) {
        Ok(c) => c,
        Err(e) => {
            warn!("Failed to parse generated chart text: {}", e);
            return None;
        }
    };

    // Detect rhythmic complexity from the chart text (push/pull directives)
    let has_complexity = chart_text.contains("/push")
        || chart.sections.iter().any(|s| {
            s.measures().iter().any(|m| {
                m.chords.iter().any(|c| c.push_pull.is_some())
            })
        });

    info!(
        "Built chart from unified MIDI pipeline: {} sections, {} measures, rhythmic_complexity: {}",
        chart.sections.len(),
        chart.sections.iter().map(|s| s.measures().len()).sum::<usize>(),
        has_complexity,
    );

    Some((chart, has_complexity))
}

/// Apply key entries from the KEY track to a chart.
///
/// The first key entry becomes the chart's initial/current key.
/// Subsequent key entries (at different positions) become key changes,
/// mapped to the section they fall within.
fn apply_key_entries_to_chart(
    chart: &mut Chart,
    key_entries: &[crate::chart::KeyEntry],
    song: &Song,
) {
    if key_entries.is_empty() {
        return;
    }

    // First entry is the initial key
    let first = &key_entries[0];
    chart.initial_key = Some(first.key.clone());
    chart.current_key = Some(first.key.clone());

    info!("Applied initial key: {}", first.key);

    // Subsequent entries are key changes
    if key_entries.len() > 1 {
        let reaper = Reaper::get();
        let project = reaper.current_project();
        let songstart_measure =
            get_internal_measure_at_time(project, song.effective_start());

        for entry in &key_entries[1..] {
            // Use REAPER beat API for exact measure calculation (no tempo estimates)
            let entry_measure = get_internal_measure_at_time(project, entry.position_seconds);
            let measure_idx = (entry_measure - songstart_measure).max(0);

            let section_idx = find_section_at_measure(chart, measure_idx as usize)
                .unwrap_or(0);

            let position = AbsolutePosition::new(
                MusicalPosition::try_new(measure_idx, 0, 0)
                    .unwrap_or_else(|_| MusicalPosition::start()),
                section_idx,
            );

            chart.add_key_change(entry.key.clone(), position, section_idx);

            info!(
                "Applied key change to {} at section {}, measure {} ({:.3}s)",
                entry.key, section_idx, measure_idx, entry.position_seconds
            );
        }
    }
}

/// Find the section index that contains a given measure index (0-indexed from SONGSTART).
fn find_section_at_measure(chart: &Chart, target_measure: usize) -> Option<usize> {
    let mut cumulative_measures = 0;
    for (idx, section) in chart.sections.iter().enumerate() {
        let section_measures = section.measures().len();
        if target_measure < cumulative_measures + section_measures {
            return Some(idx);
        }
        cumulative_measures += section_measures;
    }

    // If past end, return last section
    if !chart.sections.is_empty() {
        Some(chart.sections.len() - 1)
    } else {
        None
    }
}

/// Build a chart and song from the current REAPER project.
///
/// Returns (Chart, Song, use_stems, measure_offset, count_in_measures) where:
/// - use_stems indicates if the chart has rhythmic complexity requiring stemmed notation
/// - measure_offset is the project's measure offset (from REAPER's projmeasoffs)
/// - count_in_measures is the number of count-in measures (0, 1, or 2) from Count-In marker to SONGSTART
fn build_chart_from_current_project() -> Option<(Chart, Song, bool, i32, u8)> {
    let reaper = Reaper::get();
    let project = reaper.current_project();

    // Get project measure offset (e.g., if project starts at measure 5, offset is 4)
    let measure_offset = project.measure_offset();

    match reaper.build_song_from_current_project() {
        Ok(mut song) => {
            // Read key signatures from KEY track and populate Song fields
            let key_entries = read_all_keys_from_track(project);
            populate_song_keys(&mut song, &key_entries);

            // Calculate count-in measures from song markers
            let count_in_measures = calculate_count_in_measures(&song);

            // First, try to detect chords from MIDI track with timing analysis
            if let Some((mut midi_chart, has_complexity)) = build_chart_from_midi_chords(&song) {
                // Apply key signatures from KEY track
                apply_key_entries_to_chart(&mut midi_chart, &key_entries, &song);

                let title = midi_chart.metadata.title.as_deref().unwrap_or("Untitled");
                info!(
                    "Built chart from MIDI: {} - {} sections, {} total measures, use_stems: {}, measure_offset: {}, count_in: {}, key: {:?}",
                    title,
                    midi_chart.sections.len(),
                    midi_chart
                        .sections
                        .iter()
                        .map(|s| s.measures().len())
                        .sum::<usize>(),
                    has_complexity,
                    measure_offset,
                    count_in_measures,
                    midi_chart.initial_key
                );
                return Some((
                    midi_chart,
                    song,
                    has_complexity,
                    measure_offset,
                    count_in_measures,
                ));
            }

            // Fall back to marker-based chart parsing
            let mut chart = song.to_chart();

            // Remove CountIn sections - we use REAPER's measure_offset for count-in rendering
            chart
                .sections
                .retain(|section| section.section.section_type != SectionType::CountIn);

            // Apply key signatures from KEY track
            apply_key_entries_to_chart(&mut chart, &key_entries, &song);

            let title = chart.metadata.title.as_deref().unwrap_or("Untitled");
            info!(
                "Built chart from markers: {} - {} sections, {} total measures, tempo: {:.1} BPM, measure_offset: {}, count_in: {}, key: {:?}",
                title,
                chart.sections.len(),
                chart
                    .sections
                    .iter()
                    .map(|s| s.measures().len())
                    .sum::<usize>(),
                song.starting_tempo.unwrap_or(120.0),
                measure_offset,
                count_in_measures,
                chart.initial_key
            );
            Some((chart, song, false, measure_offset, count_in_measures)) // Marker-based charts don't have push/pull
        }
        Err(e) => {
            warn!("Failed to build song from current project: {}", e);
            None
        }
    }
}

/// Calculate the number of count-in measures from the song's markers.
///
/// Returns the number of measures between Count-In marker and SONGSTART marker (0, 1, or 2).
fn calculate_count_in_measures(song: &Song) -> u8 {
    let count_in_pos = match song.count_in_marker.as_ref() {
        Some(m) => m.position.time.to_seconds(),
        None => return 0, // No count-in marker
    };

    let start_pos = match song.start_marker.as_ref() {
        Some(m) => m.position.time.to_seconds(),
        None => return 0, // No start marker
    };

    if count_in_pos >= start_pos {
        return 0; // Count-in is not before start
    }

    // Calculate duration in seconds
    let duration = start_pos - count_in_pos;

    // Get tempo and time signature from song
    let tempo = song.starting_tempo.unwrap_or(120.0);
    let time_sig = song
        .starting_time_signature
        .clone()
        .unwrap_or_else(|| TimeSignature::new(4, 4));

    // Calculate seconds per measure
    let beats_per_measure = time_sig.numerator as f64;
    let seconds_per_beat = 60.0 / tempo;
    let seconds_per_measure = seconds_per_beat * beats_per_measure;

    // Calculate number of measures (round to nearest integer)
    let measure_count = (duration / seconds_per_measure).round() as u8;

    // Clamp to 0-2 (we only support 0, 1, or 2 count-in measures)
    measure_count.min(2)
}

/// Open the chart window
fn open_chart_window() {
    info!("Opening chart window from current project...");

    let Some(parent) = ReaperMainWindow::new() else {
        warn!("Failed to get REAPER main window handle");
        return;
    };

    let reaper = Reaper::get();
    let low_reaper = reaper.medium_reaper().low();

    // Build chart and song from current project
    let (chart, song, use_stems, measure_offset, count_in_measures) =
        match build_chart_from_current_project() {
            Some((c, s, stems, offset, count_in)) => (Some(c), Some(s), stems, offset, count_in),
            None => (None, None, false, 0, 0),
        };

    // Create renderer state (shared between render and event handlers)
    let state = Arc::new(Mutex::new(ChartRendererState::new(
        chart,
        song,
        use_stems,
        measure_offset,
        count_in_measures,
    )));
    let render_state = state.clone();
    let event_state = state;

    // Create the render function
    let render_fn = move |scene: &mut Scene, w: u32, h: u32| {
        if let Ok(mut renderer) = render_state.lock() {
            renderer.render(scene, w, h);
        }
    };

    // Create the event handler function
    let event_fn = move |event: &Event| -> EventStatus {
        if let Ok(mut renderer) = event_state.lock() {
            renderer.handle_event(event)
        } else {
            EventStatus::Ignored
        }
    };

    let source = VelloEmbedSourceWithEvents::new(render_fn, event_fn);

    // Open as docked window
    let window = DockedWindow::open(
        low_reaper,
        &parent,
        source,
        "fts_chart_window",
        "FTS Chart",
        (400, 600),
        0, // Dock to left initially
    );

    CHART_WINDOW.with(|cell| {
        if let Some(mut old) = cell.borrow_mut().take() {
            old.close(low_reaper);
        }
        *cell.borrow_mut() = Some(window);
    });

    info!("Chart window opened");
}

/// Close the chart window
fn close_chart_window() {
    let reaper = Reaper::get();
    let low_reaper = reaper.medium_reaper().low();

    CHART_WINDOW.with(|cell| {
        if let Some(mut window) = cell.borrow_mut().take() {
            window.close(low_reaper);
            info!("Chart window closed");
        }
    });
}

/// Open the chart window as floating (not docked)
fn open_floating_chart_window() {
    info!("Opening floating chart window from current project...");

    let Some(parent) = ReaperMainWindow::new() else {
        warn!("Failed to get REAPER main window handle");
        return;
    };

    // Build chart and song from current project
    let (chart, song, use_stems, measure_offset, count_in_measures) =
        match build_chart_from_current_project() {
            Some((c, s, stems, offset, count_in)) => (Some(c), Some(s), stems, offset, count_in),
            None => (None, None, false, 0, 0),
        };

    // Create renderer state (shared between render and event handlers)
    let state = Arc::new(Mutex::new(ChartRendererState::new(
        chart,
        song,
        use_stems,
        measure_offset,
        count_in_measures,
    )));
    let render_state = state.clone();
    let event_state = state;

    // Create the render function
    let render_fn = move |scene: &mut Scene, w: u32, h: u32| {
        if let Ok(mut renderer) = render_state.lock() {
            renderer.render(scene, w, h);
        }
    };

    // Create the event handler function
    let event_fn = move |event: &Event| -> EventStatus {
        if let Ok(mut renderer) = event_state.lock() {
            renderer.handle_event(event)
        } else {
            EventStatus::Ignored
        }
    };

    let source = VelloEmbedSourceWithEvents::new(render_fn, event_fn);

    // Open as floating window
    let window = ReaperWindow::open(
        &parent,
        source,
        WindowConfig {
            title: "FTS Chart (Floating)".into(),
            size: (400, 600),
            ..Default::default()
        },
    );

    FLOATING_CHART_WINDOW.with(|cell| {
        if let Some(mut old) = cell.borrow_mut().take() {
            old.close();
        }
        *cell.borrow_mut() = Some(window);
    });

    info!("Floating chart window opened");
}

/// Close the floating chart window
fn close_floating_chart_window() {
    FLOATING_CHART_WINDOW.with(|cell| {
        if let Some(mut window) = cell.borrow_mut().take() {
            window.close();
            info!("Floating chart window closed");
        }
    });
}

/// Check if the floating chart window is open
pub fn is_floating_window_open() -> bool {
    FLOATING_CHART_WINDOW.with(|cell| cell.borrow().as_ref().map(|w| w.is_open()).unwrap_or(false))
}

/// Toggle the floating chart window
fn toggle_floating_chart_window() {
    if is_floating_window_open() {
        close_floating_chart_window();
    } else {
        open_floating_chart_window();
    }
}

/// Toggle the chart window
fn toggle_chart_window() {
    CHART_WINDOW.with(|cell| {
        let has_window = cell.borrow().as_ref().map(|w| w.is_open()).unwrap_or(false);
        if has_window {
            close_chart_window();
        } else {
            open_chart_window();
        }
    });
}

/// Check if the chart window is open
pub fn is_window_open() -> bool {
    CHART_WINDOW.with(|cell| cell.borrow().as_ref().map(|w| w.is_open()).unwrap_or(false))
}

/// Get action definitions for the chart window
pub fn actions() -> Vec<ActionDef> {
    vec![
        // Docked window actions
        ActionDef {
            command_id: "FTS_CHART_WINDOW_TOGGLE",
            display_name: "Toggle Chart Window (Docked)".to_string(),
            handler: toggle_chart_window,
            appears_in_menu: true,
            section: ActionSection::Main,
            toggle_state: Some(is_window_open),
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_CHART_WINDOW_OPEN",
            display_name: "Open Chart Window (Docked)".to_string(),
            handler: open_chart_window,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_CHART_WINDOW_CLOSE",
            display_name: "Close Chart Window".to_string(),
            handler: close_chart_window,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        // Floating window actions
        ActionDef {
            command_id: "FTS_CHART_WINDOW_FLOATING_TOGGLE",
            display_name: "Toggle Chart Window (Floating)".to_string(),
            handler: toggle_floating_chart_window,
            appears_in_menu: true,
            section: ActionSection::Main,
            toggle_state: Some(is_floating_window_open),
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_CHART_WINDOW_FLOATING_OPEN",
            display_name: "Open Chart Window (Floating)".to_string(),
            handler: open_floating_chart_window,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_CHART_WINDOW_FLOATING_CLOSE",
            display_name: "Close Chart Window (Floating)".to_string(),
            handler: close_floating_chart_window,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
    ]
}
