//! Auto-color: classify tracks by instrument group and apply colors.
//!
//! This module mirrors the SWS auto-color concept but uses monarchy-based
//! classification from dynamic-template rather than regex rules. Track names
//! are fed through `monarchy_sort()` → Structure, then `color_for_path()` maps
//! each group to a color from the shared palette.

use dynamic_template::colors;
use reaper_high::Reaper;
use reaper_low::raw;
use std::ffi::CString;
use std::sync::Mutex;
use tracing::info;

/// Whether auto-color is enabled (persists across action invocations)
static AUTO_COLOR_ENABLED: Mutex<bool> = Mutex::new(false);

/// Cached track names from last poll (for change detection)
static TRACK_NAME_CACHE: Mutex<Vec<String>> = Mutex::new(Vec::new());

/// Tick counter for throttling polls (resets at POLL_INTERVAL)
static POLL_TICK_COUNTER: Mutex<u32> = Mutex::new(0);

/// Poll every N timer ticks (~30 ticks ≈ 1 second at 30Hz)
const POLL_INTERVAL: u32 = 30;

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

/// Toggle auto-color on/off. When enabled, applies colors and starts
/// continuous polling; when disabled, clears colors and stops polling.
pub fn toggle_auto_color() -> bool {
    let mut guard = AUTO_COLOR_ENABLED.lock().unwrap();
    *guard = !*guard;
    let enabled = *guard;
    drop(guard);

    if enabled {
        apply_colors_to_all_tracks();
        // Seed cache so the first poll doesn't redundantly re-apply
        *TRACK_NAME_CACHE.lock().unwrap() = read_all_track_names();
        *POLL_TICK_COUNTER.lock().unwrap() = 0;
    } else {
        clear_all_track_colors();
        TRACK_NAME_CACHE.lock().unwrap().clear();
    }
    enabled
}

/// Called from `timer_callback()`. When auto-color is enabled, periodically
/// checks if track names changed and re-applies colors if so.
pub fn poll_and_recolor() {
    if !is_auto_color_enabled() {
        return;
    }

    // Throttle: only poll every POLL_INTERVAL ticks
    {
        let mut counter = POLL_TICK_COUNTER.lock().unwrap();
        *counter += 1;
        if *counter < POLL_INTERVAL {
            return;
        }
        *counter = 0;
    }

    let current_names = read_all_track_names();

    let mut cache = TRACK_NAME_CACHE.lock().unwrap();
    if *cache == current_names {
        return;
    }

    // Names changed — update cache and re-apply colors
    *cache = current_names;
    drop(cache);

    apply_colors_to_all_tracks();
}

/// Check if auto-color is currently enabled.
pub fn is_auto_color_enabled() -> bool {
    *AUTO_COLOR_ENABLED.lock().unwrap()
}

// ============================================================================
// Internal
// ============================================================================

/// Read all track names from REAPER (lightweight — just string reads).
fn read_all_track_names() -> Vec<String> {
    let low = Reaper::get().medium_reaper().low();
    let count = unsafe { low.CountTracks(std::ptr::null_mut()) };
    let mut names = Vec::with_capacity(count as usize);
    unsafe {
        for i in 0..count {
            let track = low.GetTrack(std::ptr::null_mut(), i);
            if !track.is_null() {
                names.push(get_track_name(low, track));
            }
        }
    }
    names
}

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
    std::ffi::CStr::from_ptr(buf.as_ptr())
        .to_string_lossy()
        .into_owned()
}

/// Core logic: classify track names via dynamic-template, then apply colors to REAPER tracks.
fn apply_colors_to_tracks(low: &reaper_low::Reaper, tracks: &[TrackInfo]) {
    if tracks.is_empty() {
        return;
    }

    let track_names: Vec<String> = tracks.iter().map(|t| t.name.clone()).collect();

    // Classify and get color mapping from dynamic-template
    let color_map = dynamic_template::auto_color::classify_and_color(track_names);

    // Apply colors to REAPER tracks
    let param = CString::new("I_CUSTOMCOLOR").unwrap();
    let mut colored = 0u32;
    unsafe {
        for track_info in tracks {
            if let Some(color) = color_map.get(&track_info.name) {
                let native = colors::to_reaper_color(*color);
                low.SetMediaTrackInfo_Value(track_info.ptr, param.as_ptr(), native as f64);
                colored += 1;
            }
        }
        low.TrackList_AdjustWindows(false);
        low.UpdateArrange();
    }

    info!("Auto-color: colored {}/{} tracks", colored, tracks.len());
}
