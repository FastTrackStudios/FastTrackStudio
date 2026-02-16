//! Auto-color: classify tracks by instrument group and apply colors.
//!
//! This module mirrors the SWS auto-color concept but uses monarchy-based
//! classification from dynamic-template rather than regex rules. Track names
//! are fed through `monarchy_sort()` → Structure, then `color_for_path()` maps
//! each group to a color from the shared palette.

use color_palette::Color;
use dynamic_template::colors;
use reaper_high::Reaper;
use reaper_low::raw;
use std::collections::HashMap;
use std::ffi::CString;
use std::sync::Mutex;
use tracing::info;

/// Whether auto-color is enabled (persists across action invocations)
static AUTO_COLOR_ENABLED: Mutex<bool> = Mutex::new(false);

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

/// Toggle auto-color on/off. When enabled, applies colors; when disabled, clears them.
pub fn toggle_auto_color() -> bool {
    let mut guard = AUTO_COLOR_ENABLED.lock().unwrap();
    *guard = !*guard;
    let enabled = *guard;
    drop(guard);

    if enabled {
        apply_colors_to_all_tracks();
    } else {
        clear_all_track_colors();
    }
    enabled
}

/// Check if auto-color is currently enabled.
pub fn is_auto_color_enabled() -> bool {
    *AUTO_COLOR_ENABLED.lock().unwrap()
}

// ============================================================================
// Internal
// ============================================================================

struct TrackInfo {
    ptr: *mut raw::MediaTrack,
    name: String,
}

/// Collect track pointers and names for a range of track indices.
fn collect_track_info(
    low: &reaper_low::Reaper,
    range: std::ops::Range<i32>,
) -> Vec<TrackInfo> {
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

/// Core logic: classify track names via monarchy sort, look up colors, apply to REAPER tracks.
fn apply_colors_to_tracks(low: &reaper_low::Reaper, tracks: &[TrackInfo]) {
    if tracks.is_empty() {
        return;
    }

    let track_names: Vec<String> = tracks.iter().map(|t| t.name.clone()).collect();

    // Run monarchy classification
    let config = dynamic_template::default_config();
    let structure = match monarchy::monarchy_sort(track_names.clone(), config) {
        Ok(s) => s,
        Err(e) => {
            info!("Auto-color classification failed: {}", e);
            return;
        }
    };

    // Build name → color map by walking the classified structure
    let mut color_map: HashMap<String, Color> = HashMap::new();
    collect_colors_from_structure(&structure, &[], &mut color_map);

    // Apply colors
    let param = CString::new("I_CUSTOMCOLOR").unwrap();
    let mut colored = 0u32;
    unsafe {
        for track_info in tracks {
            if let Some(color) = color_map.get(&track_info.name) {
                let native = colors::to_reaper_color(*color);
                low.SetMediaTrackInfo_Value(
                    track_info.ptr,
                    param.as_ptr(),
                    native as f64,
                );
                colored += 1;
            }
        }
        low.TrackList_AdjustWindows(false);
        low.UpdateArrange();
    }

    info!("Auto-color: colored {}/{} tracks", colored, tracks.len());
}

/// Walk a Structure tree, mapping item original names → their group's color.
fn collect_colors_from_structure<M: monarchy::Metadata>(
    structure: &monarchy::Structure<M>,
    parent_path: &[&str],
    color_map: &mut HashMap<String, Color>,
) {
    // Build path for this node
    let mut current_path: Vec<&str> = parent_path.to_vec();
    if !structure.name.is_empty() && structure.name != "root" {
        current_path.push(&structure.name);
    }

    // Look up color for this group path
    let color = colors::color_for_path(&current_path);

    // Assign color to items at this level
    if let Some(c) = color {
        for item in &structure.items {
            color_map.insert(item.original.clone(), c);
        }
    }

    // Recurse into children — children may have more specific colors
    for child in &structure.children {
        collect_colors_from_structure(child, &current_path, color_map);
    }
}
