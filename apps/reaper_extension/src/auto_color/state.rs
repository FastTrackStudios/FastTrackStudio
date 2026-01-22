//! Auto Color state management
//!
//! Handles track classification and color application using dynamic-template.

use dynamic_template::colors::{
    GradientDirection, color_for_group, color_for_path, color_for_region, from_reaper_custom_color,
    generate_child_gradient, to_reaper_custom_color,
};
use dynamic_template::icons::icon_for_group;
use dynamic_template::layouts::layout_for_group;
use dynamic_template::{
    DynamicTemplateConfig, ItemMetadata, Structure, default_config, monarchy_sort,
};
use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::sync::Mutex;
use tracing::info;

/// Whether auto-coloring is enabled (applies on track changes)
static AUTO_COLOR_ENABLED: Mutex<bool> = Mutex::new(false);

/// Whether auto-region-coloring is enabled
static AUTO_REGION_COLOR_ENABLED: Mutex<bool> = Mutex::new(false);

/// Whether auto-marker-coloring is enabled
static AUTO_MARKER_COLOR_ENABLED: Mutex<bool> = Mutex::new(false);

/// Whether auto-icon assignment is enabled
static AUTO_ICON_ENABLED: Mutex<bool> = Mutex::new(false);

/// Whether auto-layout assignment is enabled
static AUTO_LAYOUT_ENABLED: Mutex<bool> = Mutex::new(false);

/// Check if auto-coloring is enabled
pub fn is_enabled() -> bool {
    *AUTO_COLOR_ENABLED.lock().unwrap()
}

/// Toggle auto-coloring enabled state
pub fn toggle_enabled() -> bool {
    let mut enabled = AUTO_COLOR_ENABLED.lock().unwrap();
    *enabled = !*enabled;
    *enabled
}

/// Set auto-coloring enabled state
pub fn set_enabled(enabled: bool) {
    *AUTO_COLOR_ENABLED.lock().unwrap() = enabled;
}

/// Check if auto-region-coloring is enabled
pub fn is_region_color_enabled() -> bool {
    *AUTO_REGION_COLOR_ENABLED.lock().unwrap()
}

/// Toggle auto-region-coloring enabled state
pub fn toggle_region_color_enabled() -> bool {
    let mut enabled = AUTO_REGION_COLOR_ENABLED.lock().unwrap();
    *enabled = !*enabled;
    *enabled
}

/// Check if auto-marker-coloring is enabled
pub fn is_marker_color_enabled() -> bool {
    *AUTO_MARKER_COLOR_ENABLED.lock().unwrap()
}

/// Toggle auto-marker-coloring enabled state
pub fn toggle_marker_color_enabled() -> bool {
    let mut enabled = AUTO_MARKER_COLOR_ENABLED.lock().unwrap();
    *enabled = !*enabled;
    *enabled
}

/// Check if auto-icon assignment is enabled
pub fn is_icon_enabled() -> bool {
    *AUTO_ICON_ENABLED.lock().unwrap()
}

/// Toggle auto-icon assignment enabled state
pub fn toggle_icon_enabled() -> bool {
    let mut enabled = AUTO_ICON_ENABLED.lock().unwrap();
    *enabled = !*enabled;
    *enabled
}

/// Check if auto-layout assignment is enabled
pub fn is_layout_enabled() -> bool {
    *AUTO_LAYOUT_ENABLED.lock().unwrap()
}

/// Toggle auto-layout assignment enabled state
pub fn toggle_layout_enabled() -> bool {
    let mut enabled = AUTO_LAYOUT_ENABLED.lock().unwrap();
    *enabled = !*enabled;
    *enabled
}

/// Build a classification map from track names to their group paths
///
/// Uses monarchy_sort directly to properly classify each track name
/// and extract the matched group path.
fn build_classification_map(
    track_names: &[String],
    config: &DynamicTemplateConfig,
) -> HashMap<String, Vec<String>> {
    let mut classifications: HashMap<String, Vec<String>> = HashMap::new();

    // Run monarchy_sort to classify all tracks at once
    let structure = match monarchy_sort(track_names.to_vec(), config.clone()) {
        Ok(s) => s,
        Err(e) => {
            info!("FTS / Auto Color: Classification failed: {:?}", e);
            return classifications;
        }
    };

    // Walk the structure and collect paths for each item
    fn collect_item_paths(
        structure: &Structure<ItemMetadata>,
        current_path: Vec<String>,
        classifications: &mut HashMap<String, Vec<String>>,
    ) {
        // Build path for this node
        let mut path = current_path;
        if !structure.name.is_empty() && structure.name != "root" {
            path.push(structure.name.clone());
        }

        // Record path for each item at this node
        for item in &structure.items {
            // Use the matched_groups from the item if available
            let item_path = if !item.matched_groups.is_empty() {
                item.matched_groups.iter().map(|g| g.name.clone()).collect()
            } else {
                path.clone()
            };
            classifications.insert(item.original.clone(), item_path);
        }

        // Recurse into children
        for child in &structure.children {
            collect_item_paths(child, path.clone(), classifications);
        }
    }

    collect_item_paths(&structure, Vec::new(), &mut classifications);

    classifications
}

/// Apply colors to all tracks in the project based on classification
///
/// Uses a two-pass approach:
/// 1. First pass: Classify and color tracks that match directly
/// 2. Second pass: For uncolored tracks inside colored folders, inherit/gradient from parent
pub fn apply_colors_to_all_tracks() -> (usize, usize) {
    use reaper_high::Reaper;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    // Collect all track names first
    let mut track_names: Vec<String> = Vec::new();

    unsafe {
        let num_tracks = low.CountTracks(std::ptr::null_mut());

        for i in 0..num_tracks {
            let reaper_track = low.GetTrack(std::ptr::null_mut(), i);
            if reaper_track.is_null() {
                continue;
            }

            // Get track name
            let mut name_buf = [0i8; 256];
            low.GetSetMediaTrackInfo_String(
                reaper_track,
                c"P_NAME".as_ptr(),
                name_buf.as_mut_ptr(),
                false,
            );
            let name = CStr::from_ptr(name_buf.as_ptr())
                .to_string_lossy()
                .into_owned();

            track_names.push(name);
        }
    }

    // Build classification map using monarchy
    let config = default_config();
    let classifications = build_classification_map(&track_names, &config);

    let mut colored = 0;
    let mut skipped = 0;

    // Track which indices got colored in first pass (for second pass inheritance)
    let mut colored_tracks: HashMap<i32, u32> = HashMap::new(); // index -> RGB color

    unsafe {
        let num_tracks = low.CountTracks(std::ptr::null_mut());

        // === First Pass: Direct classification ===
        for i in 0..num_tracks {
            let reaper_track = low.GetTrack(std::ptr::null_mut(), i);
            if reaper_track.is_null() {
                continue;
            }

            // Get track name
            let mut name_buf = [0i8; 256];
            low.GetSetMediaTrackInfo_String(
                reaper_track,
                c"P_NAME".as_ptr(),
                name_buf.as_mut_ptr(),
                false,
            );
            let name = CStr::from_ptr(name_buf.as_ptr())
                .to_string_lossy()
                .into_owned();

            // Try to get color from classification path
            let color = if let Some(path) = classifications.get(&name) {
                let path_refs: Vec<&str> = path.iter().map(|s| s.as_str()).collect();
                // Try full path first, then leaf group, then root group
                color_for_path(&path_refs)
                    .or_else(|| path.last().and_then(|g| color_for_group(g)))
                    .or_else(|| path.first().and_then(|g| color_for_group(g)))
            } else {
                // Fall back to simple name matching (for tracks not in classification)
                color_for_group(&name)
            };

            if let Some(rgb_color) = color {
                let reaper_color = to_reaper_custom_color(rgb_color);
                low.SetMediaTrackInfo_Value(
                    reaper_track,
                    c"I_CUSTOMCOLOR".as_ptr(),
                    reaper_color as f64,
                );
                colored_tracks.insert(i, rgb_color);
                colored += 1;
                info!("Colored track '{}' -> color 0x{:06X}", name, rgb_color);
            }
        }

        // === Second Pass: Inherit colors from parent folders ===
        // Track the current folder stack with their colors
        let mut folder_stack: Vec<(i32, u32)> = Vec::new(); // (folder_index, RGB color)

        for i in 0..num_tracks {
            let reaper_track = low.GetTrack(std::ptr::null_mut(), i);
            if reaper_track.is_null() {
                continue;
            }

            // Get folder depth info
            let folder_depth =
                low.GetMediaTrackInfo_Value(reaper_track, c"I_FOLDERDEPTH".as_ptr()) as i32;

            // Check if this track already has a color from first pass
            if let Some(&rgb_color) = colored_tracks.get(&i) {
                // This is a colored track - if it's a folder, push to stack
                if folder_depth >= 1 {
                    folder_stack.push((i, rgb_color));
                }
            } else {
                // This track wasn't colored - try to inherit from parent folder
                if let Some(&(_, parent_color)) = folder_stack.last() {
                    let reaper_color = to_reaper_custom_color(parent_color);
                    low.SetMediaTrackInfo_Value(
                        reaper_track,
                        c"I_CUSTOMCOLOR".as_ptr(),
                        reaper_color as f64,
                    );
                    colored += 1;

                    // Get name for logging
                    let mut name_buf = [0i8; 256];
                    low.GetSetMediaTrackInfo_String(
                        reaper_track,
                        c"P_NAME".as_ptr(),
                        name_buf.as_mut_ptr(),
                        false,
                    );
                    let name = CStr::from_ptr(name_buf.as_ptr())
                        .to_string_lossy()
                        .into_owned();
                    info!(
                        "Inherited color for '{}' from parent -> 0x{:06X}",
                        name, parent_color
                    );

                    // If this is also a folder, push it with inherited color
                    if folder_depth >= 1 {
                        folder_stack.push((i, parent_color));
                    }
                } else {
                    skipped += 1;
                }
            }

            // Handle folder depth changes (closing folders)
            if folder_depth <= -1 {
                // Pop folders from stack based on depth change
                for _ in 0..(-folder_depth) {
                    folder_stack.pop();
                }
            }
        }

        // Force UI update
        low.TrackList_AdjustWindows(false);
        low.UpdateArrange();
    }

    (colored, skipped)
}

/// Apply colors to selected tracks only
pub fn apply_colors_to_selected_tracks() -> (usize, usize) {
    use reaper_high::Reaper;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    let mut colored = 0;
    let mut skipped = 0;

    unsafe {
        let num_selected = low.CountSelectedTracks(std::ptr::null_mut());

        for i in 0..num_selected {
            let reaper_track = low.GetSelectedTrack(std::ptr::null_mut(), i);
            if reaper_track.is_null() {
                skipped += 1;
                continue;
            }

            // Get track name
            let mut name_buf = [0i8; 256];
            low.GetSetMediaTrackInfo_String(
                reaper_track,
                c"P_NAME".as_ptr(),
                name_buf.as_mut_ptr(),
                false,
            );
            let name = CStr::from_ptr(name_buf.as_ptr())
                .to_string_lossy()
                .into_owned();

            // Try to get color for this track
            if let Some(color) = color_for_group(&name) {
                let reaper_color = to_reaper_custom_color(color);
                low.SetMediaTrackInfo_Value(
                    reaper_track,
                    c"I_CUSTOMCOLOR".as_ptr(),
                    reaper_color as f64,
                );
                colored += 1;
            } else {
                skipped += 1;
            }
        }

        // Force UI update
        low.TrackList_AdjustWindows(false);
        low.UpdateArrange();
    }

    (colored, skipped)
}

/// Clear colors from all tracks (reset to default)
pub fn clear_all_track_colors() -> usize {
    use reaper_high::Reaper;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    let mut cleared = 0;

    unsafe {
        let num_tracks = low.CountTracks(std::ptr::null_mut());

        for i in 0..num_tracks {
            let reaper_track = low.GetTrack(std::ptr::null_mut(), i);
            if !reaper_track.is_null() {
                // Set to 0 to clear custom color
                low.SetMediaTrackInfo_Value(reaper_track, c"I_CUSTOMCOLOR".as_ptr(), 0.0);
                cleared += 1;
            }
        }

        // Force UI update
        low.TrackList_AdjustWindows(false);
        low.UpdateArrange();
    }

    cleared
}

/// Clear colors from selected tracks
pub fn clear_selected_track_colors() -> usize {
    use reaper_high::Reaper;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    let mut cleared = 0;

    unsafe {
        let num_selected = low.CountSelectedTracks(std::ptr::null_mut());

        for i in 0..num_selected {
            let reaper_track = low.GetSelectedTrack(std::ptr::null_mut(), i);
            if !reaper_track.is_null() {
                low.SetMediaTrackInfo_Value(reaper_track, c"I_CUSTOMCOLOR".as_ptr(), 0.0);
                cleared += 1;
            }
        }

        // Force UI update
        low.TrackList_AdjustWindows(false);
        low.UpdateArrange();
    }

    cleared
}

/// Apply colors to all regions in the project based on their names
pub fn apply_colors_to_all_regions() -> (usize, usize) {
    use reaper_high::Reaper;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    let mut colored = 0;
    let mut skipped = 0;

    unsafe {
        // Get number of markers/regions
        let (num_markers, num_regions) = {
            let mut markers = 0;
            let mut regions = 0;
            low.CountProjectMarkers(std::ptr::null_mut(), &mut markers, &mut regions);
            (markers, regions)
        };

        let total = num_markers + num_regions;

        // Iterate through all markers/regions
        let mut idx = 0;
        while idx < total as i32 {
            let mut is_region = false;
            let mut pos = 0.0;
            let mut region_end = 0.0;
            let mut name_ptr: *const std::os::raw::c_char = std::ptr::null();
            let mut marker_idx = 0;

            let result = low.EnumProjectMarkers(
                idx,
                &mut is_region,
                &mut pos,
                &mut region_end,
                &mut name_ptr,
                &mut marker_idx,
            );

            if result == 0 {
                break;
            }

            // Only process regions (not markers)
            if is_region && !name_ptr.is_null() {
                let name = CStr::from_ptr(name_ptr).to_string_lossy();

                // Use color_for_region which handles abbreviations like V1, CH, BR, etc.
                if let Some(rgb_color) = color_for_region(&name) {
                    let reaper_color = to_reaper_custom_color(rgb_color);

                    // SetProjectMarker3 sets the color for a region/marker
                    low.SetProjectMarker3(
                        std::ptr::null_mut(),
                        marker_idx,
                        is_region,
                        pos,
                        region_end,
                        name_ptr,
                        reaper_color as i32,
                    );

                    colored += 1;
                    info!("Colored region '{}' -> color 0x{:06X}", name, rgb_color);
                } else {
                    skipped += 1;
                }
            }

            idx += 1;
        }

        // Force UI update
        low.UpdateArrange();
    }

    (colored, skipped)
}

/// Apply colors to all markers in the project based on their names
pub fn apply_colors_to_all_markers() -> (usize, usize) {
    use reaper_high::Reaper;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    let mut colored = 0;
    let mut skipped = 0;

    unsafe {
        // Get number of markers/regions
        let (num_markers, num_regions) = {
            let mut markers = 0;
            let mut regions = 0;
            low.CountProjectMarkers(std::ptr::null_mut(), &mut markers, &mut regions);
            (markers, regions)
        };

        let total = num_markers + num_regions;

        // Iterate through all markers/regions
        let mut idx = 0;
        while idx < total as i32 {
            let mut is_region = false;
            let mut pos = 0.0;
            let mut region_end = 0.0;
            let mut name_ptr: *const std::os::raw::c_char = std::ptr::null();
            let mut marker_idx = 0;

            let result = low.EnumProjectMarkers(
                idx,
                &mut is_region,
                &mut pos,
                &mut region_end,
                &mut name_ptr,
                &mut marker_idx,
            );

            if result == 0 {
                break;
            }

            // Only process markers (not regions)
            if !is_region && !name_ptr.is_null() {
                let name = CStr::from_ptr(name_ptr).to_string_lossy();

                // Use color_for_region which handles abbreviations like V1, CH, BR, etc.
                if let Some(rgb_color) = color_for_region(&name) {
                    let reaper_color = to_reaper_custom_color(rgb_color);

                    // SetProjectMarker3 sets the color for a region/marker
                    low.SetProjectMarker3(
                        std::ptr::null_mut(),
                        marker_idx,
                        is_region,
                        pos,
                        region_end,
                        name_ptr,
                        reaper_color as i32,
                    );

                    colored += 1;
                    info!("Colored marker '{}' -> color 0x{:06X}", name, rgb_color);
                } else {
                    skipped += 1;
                }
            }

            idx += 1;
        }

        // Force UI update
        low.UpdateArrange();
    }

    (colored, skipped)
}

/// Clear colors from all regions (reset to default)
pub fn clear_all_region_colors() -> usize {
    use reaper_high::Reaper;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    let mut cleared = 0;

    unsafe {
        let (num_markers, num_regions) = {
            let mut markers = 0;
            let mut regions = 0;
            low.CountProjectMarkers(std::ptr::null_mut(), &mut markers, &mut regions);
            (markers, regions)
        };

        let total = num_markers + num_regions;
        let mut idx = 0;

        while idx < total as i32 {
            let mut is_region = false;
            let mut pos = 0.0;
            let mut region_end = 0.0;
            let mut name_ptr: *const std::os::raw::c_char = std::ptr::null();
            let mut marker_idx = 0;

            let result = low.EnumProjectMarkers(
                idx,
                &mut is_region,
                &mut pos,
                &mut region_end,
                &mut name_ptr,
                &mut marker_idx,
            );

            if result == 0 {
                break;
            }

            if is_region {
                // Set color to 0 to clear
                low.SetProjectMarker3(
                    std::ptr::null_mut(),
                    marker_idx,
                    is_region,
                    pos,
                    region_end,
                    name_ptr,
                    0,
                );
                cleared += 1;
            }

            idx += 1;
        }

        low.UpdateArrange();
    }

    cleared
}

/// Clear colors from all markers (reset to default)
pub fn clear_all_marker_colors() -> usize {
    use reaper_high::Reaper;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    let mut cleared = 0;

    unsafe {
        let (num_markers, num_regions) = {
            let mut markers = 0;
            let mut regions = 0;
            low.CountProjectMarkers(std::ptr::null_mut(), &mut markers, &mut regions);
            (markers, regions)
        };

        let total = num_markers + num_regions;
        let mut idx = 0;

        while idx < total as i32 {
            let mut is_region = false;
            let mut pos = 0.0;
            let mut region_end = 0.0;
            let mut name_ptr: *const std::os::raw::c_char = std::ptr::null();
            let mut marker_idx = 0;

            let result = low.EnumProjectMarkers(
                idx,
                &mut is_region,
                &mut pos,
                &mut region_end,
                &mut name_ptr,
                &mut marker_idx,
            );

            if result == 0 {
                break;
            }

            if !is_region {
                // Set color to 0 to clear
                low.SetProjectMarker3(
                    std::ptr::null_mut(),
                    marker_idx,
                    is_region,
                    pos,
                    region_end,
                    name_ptr,
                    0,
                );
                cleared += 1;
            }

            idx += 1;
        }

        low.UpdateArrange();
    }

    cleared
}

/// Apply icons to all tracks in the project based on classification
pub fn apply_icons_to_all_tracks() -> (usize, usize) {
    use reaper_high::Reaper;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    let mut iconified = 0;
    let mut skipped = 0;

    unsafe {
        let num_tracks = low.CountTracks(std::ptr::null_mut());

        for i in 0..num_tracks {
            let reaper_track = low.GetTrack(std::ptr::null_mut(), i);
            if reaper_track.is_null() {
                skipped += 1;
                continue;
            }

            // Get track name
            let mut name_buf = [0i8; 256];
            low.GetSetMediaTrackInfo_String(
                reaper_track,
                c"P_NAME".as_ptr(),
                name_buf.as_mut_ptr(),
                false,
            );
            let name = CStr::from_ptr(name_buf.as_ptr())
                .to_string_lossy()
                .into_owned();

            // Try to get icon for this track
            if let Some(icon_name) = icon_for_group(&name) {
                let icon_cstring = CString::new(icon_name).unwrap();
                low.GetSetMediaTrackInfo_String(
                    reaper_track,
                    c"P_ICON".as_ptr(),
                    icon_cstring.as_ptr() as *mut i8,
                    true,
                );
                iconified += 1;
                info!("Set icon for track '{}' -> {}", name, icon_name);
            } else {
                skipped += 1;
            }
        }

        // Force UI update
        low.TrackList_AdjustWindows(false);
    }

    (iconified, skipped)
}

/// Apply icons to selected tracks only
pub fn apply_icons_to_selected_tracks() -> (usize, usize) {
    use reaper_high::Reaper;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    let mut iconified = 0;
    let mut skipped = 0;

    unsafe {
        let num_selected = low.CountSelectedTracks(std::ptr::null_mut());

        for i in 0..num_selected {
            let reaper_track = low.GetSelectedTrack(std::ptr::null_mut(), i);
            if reaper_track.is_null() {
                skipped += 1;
                continue;
            }

            // Get track name
            let mut name_buf = [0i8; 256];
            low.GetSetMediaTrackInfo_String(
                reaper_track,
                c"P_NAME".as_ptr(),
                name_buf.as_mut_ptr(),
                false,
            );
            let name = CStr::from_ptr(name_buf.as_ptr())
                .to_string_lossy()
                .into_owned();

            // Try to get icon for this track
            if let Some(icon_name) = icon_for_group(&name) {
                let icon_cstring = CString::new(icon_name).unwrap();
                low.GetSetMediaTrackInfo_String(
                    reaper_track,
                    c"P_ICON".as_ptr(),
                    icon_cstring.as_ptr() as *mut i8,
                    true,
                );
                iconified += 1;
            } else {
                skipped += 1;
            }
        }

        // Force UI update
        low.TrackList_AdjustWindows(false);
    }

    (iconified, skipped)
}

/// Clear icons from all tracks (reset to default)
pub fn clear_all_track_icons() -> usize {
    use reaper_high::Reaper;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    let mut cleared = 0;

    unsafe {
        let num_tracks = low.CountTracks(std::ptr::null_mut());
        let empty_icon = CString::new("").unwrap();

        for i in 0..num_tracks {
            let reaper_track = low.GetTrack(std::ptr::null_mut(), i);
            if !reaper_track.is_null() {
                // Set to empty string to clear icon
                low.GetSetMediaTrackInfo_String(
                    reaper_track,
                    c"P_ICON".as_ptr(),
                    empty_icon.as_ptr() as *mut i8,
                    true,
                );
                cleared += 1;
            }
        }

        // Force UI update
        low.TrackList_AdjustWindows(false);
    }

    cleared
}

/// Clear icons from selected tracks
pub fn clear_selected_track_icons() -> usize {
    use reaper_high::Reaper;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    let mut cleared = 0;

    unsafe {
        let num_selected = low.CountSelectedTracks(std::ptr::null_mut());
        let empty_icon = CString::new("").unwrap();

        for i in 0..num_selected {
            let reaper_track = low.GetSelectedTrack(std::ptr::null_mut(), i);
            if !reaper_track.is_null() {
                low.GetSetMediaTrackInfo_String(
                    reaper_track,
                    c"P_ICON".as_ptr(),
                    empty_icon.as_ptr() as *mut i8,
                    true,
                );
                cleared += 1;
            }
        }

        // Force UI update
        low.TrackList_AdjustWindows(false);
    }

    cleared
}

/// Apply layouts to all tracks in the project based on classification
pub fn apply_layouts_to_all_tracks() -> (usize, usize) {
    use reaper_high::Reaper;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    let mut layoutified = 0;
    let mut skipped = 0;

    unsafe {
        let num_tracks = low.CountTracks(std::ptr::null_mut());

        for i in 0..num_tracks {
            let reaper_track = low.GetTrack(std::ptr::null_mut(), i);
            if reaper_track.is_null() {
                skipped += 1;
                continue;
            }

            // Get track name
            let mut name_buf = [0i8; 256];
            low.GetSetMediaTrackInfo_String(
                reaper_track,
                c"P_NAME".as_ptr(),
                name_buf.as_mut_ptr(),
                false,
            );
            let name = CStr::from_ptr(name_buf.as_ptr())
                .to_string_lossy()
                .into_owned();

            // Try to get layout for this track
            if let Some(layout) = layout_for_group(&name) {
                // Only apply if non-empty layout is specified
                if !layout.tcp.is_empty() {
                    let tcp_cstring = CString::new(layout.tcp).unwrap();
                    low.GetSetMediaTrackInfo_String(
                        reaper_track,
                        c"P_TCP_LAYOUT".as_ptr(),
                        tcp_cstring.as_ptr() as *mut i8,
                        true,
                    );
                }
                if !layout.mcp.is_empty() {
                    let mcp_cstring = CString::new(layout.mcp).unwrap();
                    low.GetSetMediaTrackInfo_String(
                        reaper_track,
                        c"P_MCP_LAYOUT".as_ptr(),
                        mcp_cstring.as_ptr() as *mut i8,
                        true,
                    );
                }
                if !layout.tcp.is_empty() || !layout.mcp.is_empty() {
                    layoutified += 1;
                    info!(
                        "Set layout for track '{}' -> TCP: {}, MCP: {}",
                        name, layout.tcp, layout.mcp
                    );
                } else {
                    skipped += 1;
                }
            } else {
                skipped += 1;
            }
        }

        // Force UI update
        low.TrackList_AdjustWindows(false);
    }

    (layoutified, skipped)
}

/// Apply layouts to selected tracks only
pub fn apply_layouts_to_selected_tracks() -> (usize, usize) {
    use reaper_high::Reaper;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    let mut layoutified = 0;
    let mut skipped = 0;

    unsafe {
        let num_selected = low.CountSelectedTracks(std::ptr::null_mut());

        for i in 0..num_selected {
            let reaper_track = low.GetSelectedTrack(std::ptr::null_mut(), i);
            if reaper_track.is_null() {
                skipped += 1;
                continue;
            }

            // Get track name
            let mut name_buf = [0i8; 256];
            low.GetSetMediaTrackInfo_String(
                reaper_track,
                c"P_NAME".as_ptr(),
                name_buf.as_mut_ptr(),
                false,
            );
            let name = CStr::from_ptr(name_buf.as_ptr())
                .to_string_lossy()
                .into_owned();

            // Try to get layout for this track
            if let Some(layout) = layout_for_group(&name) {
                if !layout.tcp.is_empty() {
                    let tcp_cstring = CString::new(layout.tcp).unwrap();
                    low.GetSetMediaTrackInfo_String(
                        reaper_track,
                        c"P_TCP_LAYOUT".as_ptr(),
                        tcp_cstring.as_ptr() as *mut i8,
                        true,
                    );
                }
                if !layout.mcp.is_empty() {
                    let mcp_cstring = CString::new(layout.mcp).unwrap();
                    low.GetSetMediaTrackInfo_String(
                        reaper_track,
                        c"P_MCP_LAYOUT".as_ptr(),
                        mcp_cstring.as_ptr() as *mut i8,
                        true,
                    );
                }
                if !layout.tcp.is_empty() || !layout.mcp.is_empty() {
                    layoutified += 1;
                } else {
                    skipped += 1;
                }
            } else {
                skipped += 1;
            }
        }

        // Force UI update
        low.TrackList_AdjustWindows(false);
    }

    (layoutified, skipped)
}

/// Clear layouts from all tracks (reset to default)
pub fn clear_all_track_layouts() -> usize {
    use reaper_high::Reaper;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    let mut cleared = 0;

    unsafe {
        let num_tracks = low.CountTracks(std::ptr::null_mut());
        let empty_layout = CString::new("").unwrap();

        for i in 0..num_tracks {
            let reaper_track = low.GetTrack(std::ptr::null_mut(), i);
            if !reaper_track.is_null() {
                // Set to empty string to use default layout
                low.GetSetMediaTrackInfo_String(
                    reaper_track,
                    c"P_TCP_LAYOUT".as_ptr(),
                    empty_layout.as_ptr() as *mut i8,
                    true,
                );
                low.GetSetMediaTrackInfo_String(
                    reaper_track,
                    c"P_MCP_LAYOUT".as_ptr(),
                    empty_layout.as_ptr() as *mut i8,
                    true,
                );
                cleared += 1;
            }
        }

        // Force UI update
        low.TrackList_AdjustWindows(false);
    }

    cleared
}

/// Clear layouts from selected tracks
pub fn clear_selected_track_layouts() -> usize {
    use reaper_high::Reaper;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    let mut cleared = 0;

    unsafe {
        let num_selected = low.CountSelectedTracks(std::ptr::null_mut());
        let empty_layout = CString::new("").unwrap();

        for i in 0..num_selected {
            let reaper_track = low.GetSelectedTrack(std::ptr::null_mut(), i);
            if !reaper_track.is_null() {
                low.GetSetMediaTrackInfo_String(
                    reaper_track,
                    c"P_TCP_LAYOUT".as_ptr(),
                    empty_layout.as_ptr() as *mut i8,
                    true,
                );
                low.GetSetMediaTrackInfo_String(
                    reaper_track,
                    c"P_MCP_LAYOUT".as_ptr(),
                    empty_layout.as_ptr() as *mut i8,
                    true,
                );
                cleared += 1;
            }
        }

        // Force UI update
        low.TrackList_AdjustWindows(false);
    }

    cleared
}

/// Get all track names in the current project (for classification)
pub fn get_all_track_names() -> Vec<String> {
    use reaper_high::Reaper;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    let mut names = Vec::new();

    unsafe {
        let num_tracks = low.CountTracks(std::ptr::null_mut());

        for i in 0..num_tracks {
            let reaper_track = low.GetTrack(std::ptr::null_mut(), i);
            if reaper_track.is_null() {
                continue;
            }

            let mut name_buf = [0i8; 256];
            low.GetSetMediaTrackInfo_String(
                reaper_track,
                c"P_NAME".as_ptr(),
                name_buf.as_mut_ptr(),
                false,
            );
            let name = CStr::from_ptr(name_buf.as_ptr())
                .to_string_lossy()
                .into_owned();

            names.push(name);
        }
    }

    names
}

// === Color Children (Gradient) Functions ===

/// Get the children track indices of a folder track
///
/// Returns a vector of track indices that are children of the folder at `folder_index`.
/// Uses the same logic as visibility_manager for folder depth tracking.
fn get_folder_children_indices(folder_index: i32) -> Vec<i32> {
    use reaper_high::Reaper;

    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    let mut children = Vec::new();

    unsafe {
        let num_tracks = low.CountTracks(std::ptr::null_mut());

        // Get the folder track
        let folder_track = low.GetTrack(std::ptr::null_mut(), folder_index);
        if folder_track.is_null() {
            return children;
        }

        // Check if this is actually a folder
        let folder_depth = low.GetMediaTrackInfo_Value(folder_track, c"I_FOLDERDEPTH".as_ptr());
        if folder_depth < 1.0 {
            // Not a folder
            return children;
        }

        // Track depth relative to folder
        let mut depth = 1i32;

        // Iterate through subsequent tracks
        for i in (folder_index + 1)..num_tracks {
            if depth <= 0 {
                break;
            }

            let track = low.GetTrack(std::ptr::null_mut(), i);
            if track.is_null() {
                continue;
            }

            // This track is a child of our folder
            children.push(i);

            // Get this track's folder depth change
            let track_depth = low.GetMediaTrackInfo_Value(track, c"I_FOLDERDEPTH".as_ptr()) as i32;

            // Update depth: positive means start of folder, negative means end
            if track_depth >= 1 {
                depth += 1;
            } else if track_depth <= -1 {
                depth += track_depth; // track_depth is negative
            }
        }
    }

    children
}

/// Apply gradient colors to children of selected folder tracks
///
/// For each selected folder track:
/// 1. Get the folder's color (or use classification to get one)
/// 2. Generate gradient colors for all children
/// 3. Apply gradient colors to children
///
/// Returns (folders_processed, children_colored)
pub fn apply_gradient_to_selected_folder_children(direction: GradientDirection) -> (usize, usize) {
    use reaper_high::Reaper;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    let mut folders_processed = 0;
    let mut children_colored = 0;

    unsafe {
        let num_selected = low.CountSelectedTracks(std::ptr::null_mut());

        for sel_idx in 0..num_selected {
            let folder_track = low.GetSelectedTrack(std::ptr::null_mut(), sel_idx);
            if folder_track.is_null() {
                continue;
            }

            // Check if this is a folder
            let folder_depth = low.GetMediaTrackInfo_Value(folder_track, c"I_FOLDERDEPTH".as_ptr());
            if folder_depth < 1.0 {
                // Not a folder, skip
                continue;
            }

            // Get the folder's track index
            let folder_index =
                low.GetMediaTrackInfo_Value(folder_track, c"IP_TRACKNUMBER".as_ptr()) as i32 - 1;

            // Get folder's current color
            let folder_color_raw =
                low.GetMediaTrackInfo_Value(folder_track, c"I_CUSTOMCOLOR".as_ptr()) as u32;

            // Extract RGB from REAPER color
            let folder_color = if let Some(color) = from_reaper_custom_color(folder_color_raw) {
                color
            } else {
                // No custom color set - try to get from classification
                let mut name_buf = [0i8; 256];
                low.GetSetMediaTrackInfo_String(
                    folder_track,
                    c"P_NAME".as_ptr(),
                    name_buf.as_mut_ptr(),
                    false,
                );
                let name = CStr::from_ptr(name_buf.as_ptr())
                    .to_string_lossy()
                    .into_owned();

                match color_for_group(&name) {
                    Some(c) => c,
                    None => continue, // Skip if no color
                }
            };

            // Get children indices
            let children = get_folder_children_indices(folder_index);
            if children.is_empty() {
                continue;
            }

            // Generate gradient colors
            let gradient_colors = generate_child_gradient(folder_color, children.len(), direction);

            // Apply colors to children
            for (i, &child_idx) in children.iter().enumerate() {
                let child_track = low.GetTrack(std::ptr::null_mut(), child_idx);
                if child_track.is_null() {
                    continue;
                }

                let reaper_color = to_reaper_custom_color(gradient_colors[i]);
                low.SetMediaTrackInfo_Value(
                    child_track,
                    c"I_CUSTOMCOLOR".as_ptr(),
                    reaper_color as f64,
                );
                children_colored += 1;
            }

            folders_processed += 1;
        }

        // Force UI update
        low.TrackList_AdjustWindows(false);
        low.UpdateArrange();
    }

    (folders_processed, children_colored)
}

/// Apply gradient colors to children of ALL folder tracks in the project
///
/// Returns (folders_processed, children_colored)
pub fn apply_gradient_to_all_folder_children(direction: GradientDirection) -> (usize, usize) {
    use reaper_high::Reaper;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    let mut folders_processed = 0;
    let mut children_colored = 0;

    unsafe {
        let num_tracks = low.CountTracks(std::ptr::null_mut());

        for i in 0..num_tracks {
            let folder_track = low.GetTrack(std::ptr::null_mut(), i);
            if folder_track.is_null() {
                continue;
            }

            // Check if this is a folder
            let folder_depth = low.GetMediaTrackInfo_Value(folder_track, c"I_FOLDERDEPTH".as_ptr());
            if folder_depth < 1.0 {
                continue;
            }

            // Get folder's current color
            let folder_color_raw =
                low.GetMediaTrackInfo_Value(folder_track, c"I_CUSTOMCOLOR".as_ptr()) as u32;

            // Extract RGB from REAPER color
            let folder_color = if let Some(color) = from_reaper_custom_color(folder_color_raw) {
                color
            } else {
                // No custom color set - try to get from classification
                let mut name_buf = [0i8; 256];
                low.GetSetMediaTrackInfo_String(
                    folder_track,
                    c"P_NAME".as_ptr(),
                    name_buf.as_mut_ptr(),
                    false,
                );
                let name = CStr::from_ptr(name_buf.as_ptr())
                    .to_string_lossy()
                    .into_owned();

                match color_for_group(&name) {
                    Some(c) => c,
                    None => continue,
                }
            };

            // Get children indices
            let children = get_folder_children_indices(i);
            if children.is_empty() {
                continue;
            }

            // Generate gradient colors
            let gradient_colors = generate_child_gradient(folder_color, children.len(), direction);

            // Apply colors to children
            for (j, &child_idx) in children.iter().enumerate() {
                let child_track = low.GetTrack(std::ptr::null_mut(), child_idx);
                if child_track.is_null() {
                    continue;
                }

                let reaper_color = to_reaper_custom_color(gradient_colors[j]);
                low.SetMediaTrackInfo_Value(
                    child_track,
                    c"I_CUSTOMCOLOR".as_ptr(),
                    reaper_color as f64,
                );
                children_colored += 1;
            }

            folders_processed += 1;
        }

        // Force UI update
        low.TrackList_AdjustWindows(false);
        low.UpdateArrange();
    }

    (folders_processed, children_colored)
}

/// Set children of selected folder tracks to the same color as parent
pub fn set_children_to_parent_color() -> (usize, usize) {
    use reaper_high::Reaper;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    let mut folders_processed = 0;
    let mut children_colored = 0;

    unsafe {
        let num_selected = low.CountSelectedTracks(std::ptr::null_mut());

        for sel_idx in 0..num_selected {
            let folder_track = low.GetSelectedTrack(std::ptr::null_mut(), sel_idx);
            if folder_track.is_null() {
                continue;
            }

            // Check if this is a folder
            let folder_depth = low.GetMediaTrackInfo_Value(folder_track, c"I_FOLDERDEPTH".as_ptr());
            if folder_depth < 1.0 {
                continue;
            }

            // Get the folder's track index
            let folder_index =
                low.GetMediaTrackInfo_Value(folder_track, c"IP_TRACKNUMBER".as_ptr()) as i32 - 1;

            // Get folder's current color (raw REAPER format)
            let folder_color_raw =
                low.GetMediaTrackInfo_Value(folder_track, c"I_CUSTOMCOLOR".as_ptr()) as u32;

            if folder_color_raw & 0x01000000 == 0 {
                // No custom color set, skip
                continue;
            }

            // Get children indices
            let children = get_folder_children_indices(folder_index);
            if children.is_empty() {
                continue;
            }

            // Apply same color to all children
            for &child_idx in &children {
                let child_track = low.GetTrack(std::ptr::null_mut(), child_idx);
                if child_track.is_null() {
                    continue;
                }

                low.SetMediaTrackInfo_Value(
                    child_track,
                    c"I_CUSTOMCOLOR".as_ptr(),
                    folder_color_raw as f64,
                );
                children_colored += 1;
            }

            folders_processed += 1;
        }

        // Force UI update
        low.TrackList_AdjustWindows(false);
        low.UpdateArrange();
    }

    (folders_processed, children_colored)
}
