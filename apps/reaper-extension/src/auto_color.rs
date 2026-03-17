//! Auto-color: classify tracks, regions, and markers by name and apply colors.
//!
//! This module uses REAPER's `IReaperControlSurface` protocol for event-driven
//! auto-coloring. Instead of polling on a timer, we register a hidden control
//! surface that receives callbacks when state changes:
//!
//! **Tracks:**
//! - `set_track_list_change()` → tracks added/removed/reordered
//! - `set_track_title()` → track renamed
//! - `ext_set_fx_change()` → FX chain modified
//! - `ext_set_input_monitor()` → input/output routing changed
//!
//! **Regions & Markers:**
//! - `ext_set_project_marker_change()` → marker/region added/removed/renamed/moved
//!
//! These callbacks set dirty flags, and the next `run()` cycle (~30Hz) applies
//! colors. This gives near-instant response with zero wasted work.

use dynamic_template::colors;
use reaper_high::Reaper;
use reaper_low::raw;
use reaper_medium::{
    ControlSurface, ExtSetFxChangeArgs, ExtSetInputMonitorArgs, ExtSetProjectMarkerChangeArgs,
    SetTrackTitleArgs,
};
use std::ffi::{CStr, CString};
use std::sync::atomic::{AtomicBool, AtomicI32, Ordering};
use tracing::info;

// ============================================================================
// Global state (atomics — no mutex contention on the audio thread)
// ============================================================================

/// Whether auto-color is enabled (on by default, togglable via action).
static AUTO_COLOR_ENABLED: AtomicBool = AtomicBool::new(true);

/// Dirty flag for tracks: set by CSurf callbacks, cleared by `run()`.
/// Starts true so existing tracks get colored on first run() after extension load.
static NEEDS_TRACK_RECOLOR: AtomicBool = AtomicBool::new(true);

/// Dirty flag for markers/regions: set by ext_set_project_marker_change.
/// Starts true so existing markers/regions get colored on first run().
static NEEDS_MARKER_RECOLOR: AtomicBool = AtomicBool::new(true);

/// Ignore counter for deduplicating SetTrackTitle calls after SetTrackListChange.
/// When SetTrackListChange fires, REAPER also calls SetTrackTitle for every track.
/// We suppress those redundant notifications (same pattern as SWS).
static TITLE_IGNORE_COUNT: AtomicI32 = AtomicI32::new(0);

/// Suppression flag: when true, callbacks from our own color changes are ignored.
/// Prevents the feedback loop: apply_colors → SetMediaTrackInfo → set_track_list_change → dirty → apply_colors → ...
static APPLYING_COLORS: AtomicBool = AtomicBool::new(false);

// ============================================================================
// Control Surface
// ============================================================================

/// Hidden control surface that listens for track/marker/region state changes
/// and triggers auto-color reapplication. Registered via `plugin_register_add_csurf_inst`.
#[derive(Debug)]
pub struct AutoColorSurface;

impl ControlSurface for AutoColorSurface {
    /// Called ~30x/sec on the main thread. If dirty flags are set and
    /// auto-color is enabled, re-apply colors.
    fn run(&mut self) {
        if !AUTO_COLOR_ENABLED.load(Ordering::Relaxed) {
            return;
        }

        if NEEDS_TRACK_RECOLOR.swap(false, Ordering::Relaxed) {
            apply_colors_to_all_tracks();
        }

        if NEEDS_MARKER_RECOLOR.swap(false, Ordering::Relaxed) {
            apply_colors_to_all_markers_and_regions();
        }
    }

    /// Track list changed (tracks added, removed, or reordered).
    fn set_track_list_change(&self) {
        if APPLYING_COLORS.load(Ordering::Relaxed) {
            return;
        }
        if AUTO_COLOR_ENABLED.load(Ordering::Relaxed) {
            mark_tracks_dirty();
            // REAPER will call set_track_title for every track after this.
            // Set ignore counter to suppress those redundant notifications.
            let low = Reaper::get().medium_reaper().low();
            let track_count = unsafe { low.CountTracks(std::ptr::null_mut()) };
            TITLE_IGNORE_COUNT.store(track_count + 1, Ordering::Relaxed);
        }
    }

    /// Track renamed. Only triggers recolor if this isn't a redundant call
    /// following a SetTrackListChange.
    fn set_track_title(&self, _args: SetTrackTitleArgs) {
        if APPLYING_COLORS.load(Ordering::Relaxed) {
            return;
        }
        if AUTO_COLOR_ENABLED.load(Ordering::Relaxed) {
            let prev = TITLE_IGNORE_COUNT.load(Ordering::Relaxed);
            if prev > 0 {
                TITLE_IGNORE_COUNT.store(prev - 1, Ordering::Relaxed);
            } else {
                mark_tracks_dirty();
            }
        }
    }

    /// FX chain changed (FX added/removed/reordered).
    fn ext_set_fx_change(&self, _args: ExtSetFxChangeArgs) -> i32 {
        if APPLYING_COLORS.load(Ordering::Relaxed) {
            return 0;
        }
        if AUTO_COLOR_ENABLED.load(Ordering::Relaxed) {
            mark_tracks_dirty();
        }
        0
    }

    /// Input monitoring / routing changed.
    fn ext_set_input_monitor(&self, _args: ExtSetInputMonitorArgs) -> i32 {
        if APPLYING_COLORS.load(Ordering::Relaxed) {
            return 0;
        }
        if AUTO_COLOR_ENABLED.load(Ordering::Relaxed) {
            mark_tracks_dirty();
        }
        0
    }

    /// Marker or region added, removed, renamed, moved, or color changed.
    fn ext_set_project_marker_change(&self, _args: ExtSetProjectMarkerChangeArgs) -> i32 {
        if APPLYING_COLORS.load(Ordering::Relaxed) {
            return 0;
        }
        if AUTO_COLOR_ENABLED.load(Ordering::Relaxed) {
            mark_markers_dirty();
        }
        0
    }
}

fn mark_tracks_dirty() {
    NEEDS_TRACK_RECOLOR.store(true, Ordering::Relaxed);
}

fn mark_markers_dirty() {
    NEEDS_MARKER_RECOLOR.store(true, Ordering::Relaxed);
}

// ============================================================================
// Public API
// ============================================================================

/// Apply colors to all tracks in the current project based on instrument classification.
pub fn apply_colors_to_all_tracks() {
    let low = Reaper::get().medium_reaper().low();
    let track_count = unsafe { low.CountTracks(std::ptr::null_mut()) };
    if track_count == 0 {
        return;
    }

    let tracks = collect_track_info(low, 0..track_count);
    apply_colors_to_tracks(low, &tracks);
}

/// Apply colors to currently selected tracks.
pub fn apply_colors_to_selected() {
    let low = Reaper::get().medium_reaper().low();
    let mut tracks = Vec::new();

    unsafe {
        let sel_count = low.CountSelectedTracks(std::ptr::null_mut());
        for i in 0..sel_count {
            let track = low.GetSelectedTrack(std::ptr::null_mut(), i);
            if track.is_null() {
                continue;
            }
            let name = get_track_name(low, track);
            tracks.push(TrackInfo { ptr: track, name });
        }
    }

    apply_colors_to_tracks(low, &tracks);
}

/// Clear colors from all tracks (reset to default).
pub fn clear_all_track_colors() {
    let low = Reaper::get().medium_reaper().low();
    let track_count = unsafe { low.CountTracks(std::ptr::null_mut()) };
    let param = CString::new("I_CUSTOMCOLOR").unwrap();
    unsafe {
        for i in 0..track_count {
            let track = low.GetTrack(std::ptr::null_mut(), i);
            if !track.is_null() {
                low.SetMediaTrackInfo_Value(track, param.as_ptr(), 0.0);
            }
        }
        low.TrackList_AdjustWindows(false);
        low.UpdateArrange();
    }
}

/// Clear colors from selected tracks.
pub fn clear_selected_track_colors() {
    let low = Reaper::get().medium_reaper().low();
    let param = CString::new("I_CUSTOMCOLOR").unwrap();
    unsafe {
        let sel_count = low.CountSelectedTracks(std::ptr::null_mut());
        for i in 0..sel_count {
            let track = low.GetSelectedTrack(std::ptr::null_mut(), i);
            if !track.is_null() {
                low.SetMediaTrackInfo_Value(track, param.as_ptr(), 0.0);
            }
        }
        low.TrackList_AdjustWindows(false);
        low.UpdateArrange();
    }
}

/// Apply colors to all markers and regions based on section name matching.
pub fn apply_colors_to_all_markers_and_regions() {
    let low = Reaper::get().medium_reaper().low();

    // Count total markers + regions
    let mut num_markers: i32 = 0;
    let mut num_regions: i32 = 0;
    let total = unsafe {
        low.CountProjectMarkers(std::ptr::null_mut(), &mut num_markers, &mut num_regions)
    };
    if total == 0 {
        return;
    }

    // Suppress callbacks while we're applying colors
    APPLYING_COLORS.store(true, Ordering::Relaxed);

    let mut colored = 0u32;

    unsafe {
        for idx in 0..total {
            let mut is_region: bool = false;
            let mut pos: f64 = 0.0;
            let mut region_end: f64 = 0.0;
            let mut name_ptr: *const std::os::raw::c_char = std::ptr::null();
            let mut marker_id: i32 = 0;
            let mut color: i32 = 0;

            let ok = low.EnumProjectMarkers3(
                std::ptr::null_mut(),
                idx,
                &mut is_region,
                &mut pos,
                &mut region_end,
                &mut name_ptr,
                &mut marker_id,
                &mut color,
            );
            if ok == 0 {
                break;
            }

            // Read the name
            let name = if name_ptr.is_null() {
                String::new()
            } else {
                CStr::from_ptr(name_ptr).to_string_lossy().into_owned()
            };

            if name.is_empty() {
                continue;
            }

            // Look up section color for this name
            if let Some(section_color) = colors::color_for_region(&name) {
                let native = colors::to_reaper_color(section_color);

                // Only set if different from current
                if color != native {
                    low.SetProjectMarkerByIndex2(
                        std::ptr::null_mut(), // current project
                        idx,                  // enumeration index
                        is_region,
                        pos,
                        region_end,
                        marker_id,
                        std::ptr::null(), // keep existing name
                        native,
                        0, // flags
                    );
                    colored += 1;
                }
            }
        }

        if colored > 0 {
            low.UpdateArrange();
        }
    }

    APPLYING_COLORS.store(false, Ordering::Relaxed);

    if colored > 0 {
        info!(
            "Auto-color: colored {} markers/regions (of {} total)",
            colored, total
        );
    }
}

/// Clear colors from all markers and regions.
pub fn clear_all_marker_colors() {
    let low = Reaper::get().medium_reaper().low();

    let total = unsafe {
        low.CountProjectMarkers(
            std::ptr::null_mut(),
            std::ptr::null_mut(),
            std::ptr::null_mut(),
        )
    };
    if total == 0 {
        return;
    }

    unsafe {
        for idx in 0..total {
            let mut is_region: bool = false;
            let mut pos: f64 = 0.0;
            let mut region_end: f64 = 0.0;
            let mut marker_id: i32 = 0;
            let mut color: i32 = 0;

            let ok = low.EnumProjectMarkers3(
                std::ptr::null_mut(),
                idx,
                &mut is_region,
                &mut pos,
                &mut region_end,
                std::ptr::null_mut(), // don't need name
                &mut marker_id,
                &mut color,
            );
            if ok == 0 {
                break;
            }

            // Only clear if a color is set
            if color != 0 {
                low.SetProjectMarkerByIndex2(
                    std::ptr::null_mut(),
                    idx,
                    is_region,
                    pos,
                    region_end,
                    marker_id,
                    std::ptr::null(), // keep existing name
                    0,                // clear color
                    0,
                );
            }
        }
        low.UpdateArrange();
    }
}

/// Toggle auto-color on/off. When enabled, immediately applies colors to
/// tracks, markers, and regions. When disabled, clears all auto-applied colors.
pub fn toggle_auto_color() -> bool {
    let was_enabled = AUTO_COLOR_ENABLED.load(Ordering::Relaxed);
    let enabled = !was_enabled;
    AUTO_COLOR_ENABLED.store(enabled, Ordering::Relaxed);

    if enabled {
        apply_colors_to_all_tracks();
        apply_colors_to_all_markers_and_regions();
    } else {
        NEEDS_TRACK_RECOLOR.store(false, Ordering::Relaxed);
        NEEDS_MARKER_RECOLOR.store(false, Ordering::Relaxed);
        TITLE_IGNORE_COUNT.store(0, Ordering::Relaxed);
        clear_all_track_colors();
        clear_all_marker_colors();
    }
    enabled
}

/// Check if auto-color is currently enabled.
pub fn is_auto_color_enabled() -> bool {
    AUTO_COLOR_ENABLED.load(Ordering::Relaxed)
}

// ============================================================================
// Internal — Tracks
// ============================================================================

struct TrackInfo {
    ptr: *mut raw::MediaTrack,
    name: String,
}

/// Collect track pointers and names for a range of track indices.
fn collect_track_info(low: &reaper_low::Reaper, range: std::ops::Range<i32>) -> Vec<TrackInfo> {
    let mut tracks = Vec::new();
    unsafe {
        for i in range {
            let track = low.GetTrack(std::ptr::null_mut(), i);
            if track.is_null() {
                continue;
            }
            let name = get_track_name(low, track);
            tracks.push(TrackInfo { ptr: track, name });
        }
    }
    tracks
}

/// Read a track's name via REAPER API.
unsafe fn get_track_name(low: &reaper_low::Reaper, track: *mut raw::MediaTrack) -> String {
    let mut buf = [0i8; 512];
    let param = CString::new("P_NAME").unwrap();
    low.GetSetMediaTrackInfo_String(track, param.as_ptr(), buf.as_mut_ptr(), false);
    CStr::from_ptr(buf.as_ptr()).to_string_lossy().into_owned()
}

/// Core logic: classify track names via dynamic-template, then apply colors to REAPER tracks.
fn apply_colors_to_tracks(low: &reaper_low::Reaper, tracks: &[TrackInfo]) {
    if tracks.is_empty() {
        return;
    }

    let track_names: Vec<String> = tracks.iter().map(|t| t.name.clone()).collect();

    // Classify and get color mapping from dynamic-template
    let color_map = dynamic_template::auto_color::classify_and_color(track_names);

    // Suppress callbacks while we're applying colors
    APPLYING_COLORS.store(true, Ordering::Relaxed);

    // Apply colors to REAPER tracks (only if different from current)
    let color_param = CString::new("I_CUSTOMCOLOR").unwrap();
    let mut colored = 0u32;
    unsafe {
        for track_info in tracks {
            if let Some(color) = color_map.get(&track_info.name) {
                let native = colors::to_reaper_color(*color);
                let current =
                    low.GetMediaTrackInfo_Value(track_info.ptr, color_param.as_ptr()) as i32;
                if current != native as i32 {
                    low.SetMediaTrackInfo_Value(
                        track_info.ptr,
                        color_param.as_ptr(),
                        native as f64,
                    );
                    colored += 1;
                }
            }
        }
        if colored > 0 {
            low.TrackList_AdjustWindows(false);
            low.UpdateArrange();
        }
    }

    APPLYING_COLORS.store(false, Ordering::Relaxed);

    if colored > 0 {
        info!("Auto-color: colored {}/{} tracks", colored, tracks.len());
    }
}
