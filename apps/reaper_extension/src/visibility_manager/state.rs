//! Global visibility manager state for REAPER extension

use daw::tracks::api::folder::FolderDepthChange;
use std::sync::Mutex;
use visibility_manager::{ViewMode, VisibilityManager, VisibilityTarget};

/// Global visibility manager state
static VISIBILITY_MANAGER: Mutex<Option<VisibilityManager>> = Mutex::new(None);

/// Cached track folder information for hierarchy operations
static TRACK_FOLDER_INFO: Mutex<Vec<TrackFolderInfo>> = Mutex::new(Vec::new());

/// Information about a track's folder state
#[derive(Debug, Clone)]
pub struct TrackFolderInfo {
    pub index: usize,
    pub name: String,
    pub folder_depth_change: FolderDepthChange,
    pub depth: i32, // Cumulative depth (0 = top level)
}

/// Get the visibility manager, initializing if needed
pub fn get_or_init_manager() -> std::sync::MutexGuard<'static, Option<VisibilityManager>> {
    let mut guard = VISIBILITY_MANAGER.lock().unwrap();
    if guard.is_none() {
        *guard = Some(VisibilityManager::new());
    }
    guard
}

/// Get the visibility manager (may be None if not initialized)
pub fn get_manager() -> std::sync::MutexGuard<'static, Option<VisibilityManager>> {
    VISIBILITY_MANAGER.lock().unwrap()
}

/// Initialize/refresh the visibility manager with current project tracks
pub fn refresh_from_project() {
    use daw::tracks::Track;
    use reaper_high::Reaper;
    use std::ffi::CStr;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    // Get all tracks from the current project
    let mut tracks: Vec<Track> = Vec::new();
    let mut folder_info: Vec<TrackFolderInfo> = Vec::new();
    let mut current_depth: i32 = 0;

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

            // Get visibility state
            let show_tcp_param = c"B_SHOWINTCP".as_ptr();
            let show_mcp_param = c"B_SHOWINMIXER".as_ptr();
            let show_in_tcp = low.GetMediaTrackInfo_Value(reaper_track, show_tcp_param) != 0.0;
            let show_in_mcp = low.GetMediaTrackInfo_Value(reaper_track, show_mcp_param) != 0.0;

            // Get folder depth change (I_FOLDERDEPTH)
            let folder_depth_param = c"I_FOLDERDEPTH".as_ptr();
            let folder_depth_value = low.GetMediaTrackInfo_Value(reaper_track, folder_depth_param) as i32;
            let folder_depth_change = FolderDepthChange::from_reaper_value(folder_depth_value);

            // Store folder info BEFORE updating depth (depth is for this track's position)
            folder_info.push(TrackFolderInfo {
                index: i as usize,
                name: name.clone(),
                folder_depth_change,
                depth: current_depth,
            });

            // Update depth for next track
            current_depth += folder_depth_value;

            let mut track = Track::new(name);
            track.show_in_track_list = show_in_tcp;
            track.show_in_mixer = show_in_mcp;
            track.index = Some(i as usize);
            track.folder_depth_change = folder_depth_change;
            track.is_folder = folder_depth_change.is_folder_start();

            tracks.push(track);
        }
    }

    // Update folder info cache
    {
        let mut info_guard = TRACK_FOLDER_INFO.lock().unwrap();
        *info_guard = folder_info;
    }

    // Update visibility manager
    let mut guard = get_or_init_manager();
    if let Some(manager) = guard.as_mut() {
        manager.analyze_tracks(&tracks);
    }
}

/// Get children indices for a folder track at the given index
///
/// Returns all track indices that are children of the folder at `folder_index`.
/// This includes direct children and all nested descendants.
pub fn get_folder_children(folder_index: usize) -> Vec<usize> {
    let info_guard = TRACK_FOLDER_INFO.lock().unwrap();

    if folder_index >= info_guard.len() {
        return Vec::new();
    }

    let folder_info = &info_guard[folder_index];

    // Only folder tracks have children
    if !folder_info.folder_depth_change.is_folder_start() {
        return Vec::new();
    }

    let folder_depth = folder_info.depth;
    let mut children = Vec::new();
    let mut current_depth = folder_depth;

    // Iterate through tracks after the folder
    for i in (folder_index + 1)..info_guard.len() {
        let track_info = &info_guard[i];

        // This track is at the folder's depth level or deeper, so it's a child
        // We need to track depth changes to know when we've exited the folder
        if track_info.depth > folder_depth {
            children.push(track_info.index);
        }

        // Update depth after processing this track
        current_depth += track_info.folder_depth_change.to_reaper_value();

        // If depth drops back to or below the folder's depth, we've exited
        if current_depth <= folder_depth {
            break;
        }
    }

    children
}

/// Check if a track at the given index is a folder
pub fn is_folder_track(index: usize) -> bool {
    let info_guard = TRACK_FOLDER_INFO.lock().unwrap();
    info_guard
        .get(index)
        .map(|info| info.folder_depth_change.is_folder_start())
        .unwrap_or(false)
}

/// Apply visibility changes to REAPER tracks
///
/// This function expands the changes to include folder children automatically.
/// If a track in the show/hide list is a folder, all its children will also be shown/hidden.
pub fn apply_changes(changes: &visibility_manager::VisibilityChanges) {
    use reaper_high::Reaper;
    use std::collections::HashSet;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    // Expand show indices to include folder children
    let mut show_indices: HashSet<usize> = changes.show.iter().copied().collect();
    for &index in &changes.show {
        if is_folder_track(index) {
            show_indices.extend(get_folder_children(index));
        }
    }

    // Expand hide indices to include folder children
    let mut hide_indices: HashSet<usize> = changes.hide.iter().copied().collect();
    for &index in &changes.hide {
        if is_folder_track(index) {
            hide_indices.extend(get_folder_children(index));
        }
    }

    unsafe {
        // Apply show changes
        for &index in &show_indices {
            let track = low.GetTrack(std::ptr::null_mut(), index as i32);
            if !track.is_null() {
                if changes.target.affects_tcp() {
                    let param = c"B_SHOWINTCP".as_ptr();
                    low.SetMediaTrackInfo_Value(track, param, 1.0);
                }
                if changes.target.affects_mcp() {
                    let param = c"B_SHOWINMIXER".as_ptr();
                    low.SetMediaTrackInfo_Value(track, param, 1.0);
                }
            }
        }

        // Apply hide changes
        for &index in &hide_indices {
            let track = low.GetTrack(std::ptr::null_mut(), index as i32);
            if !track.is_null() {
                if changes.target.affects_tcp() {
                    let param = c"B_SHOWINTCP".as_ptr();
                    low.SetMediaTrackInfo_Value(track, param, 0.0);
                }
                if changes.target.affects_mcp() {
                    let param = c"B_SHOWINMIXER".as_ptr();
                    low.SetMediaTrackInfo_Value(track, param, 0.0);
                }
            }
        }

        // Force UI update
        low.TrackList_AdjustWindows(false);
        low.UpdateArrange();
    }
}
