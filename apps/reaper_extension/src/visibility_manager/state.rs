//! Global visibility manager state for REAPER extension

use std::sync::Mutex;
use visibility_manager::{ViewMode, VisibilityManager, VisibilityTarget};

/// Global visibility manager state
static VISIBILITY_MANAGER: Mutex<Option<VisibilityManager>> = Mutex::new(None);

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

            let mut track = Track::new(name);
            track.show_in_track_list = show_in_tcp;
            track.show_in_mixer = show_in_mcp;
            track.index = Some(i as usize);

            tracks.push(track);
        }
    }

    // Update visibility manager
    let mut guard = get_or_init_manager();
    if let Some(manager) = guard.as_mut() {
        manager.analyze_tracks(&tracks);
    }
}

/// Apply visibility changes to REAPER tracks
pub fn apply_changes(changes: &visibility_manager::VisibilityChanges) {
    use reaper_high::Reaper;

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    unsafe {
        // Apply show changes
        for &index in &changes.show {
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
        for &index in &changes.hide {
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
