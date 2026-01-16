//! Actions for Visibility Manager REAPER Extension
//!
//! Provides actions for managing track visibility using dynamic classification.

use crate::infrastructure::action_registry::{ActionDef, ActionSection};
use reaper_high::Reaper;
use std::ffi::CString;
use tracing::{info, warn};
use visibility_manager::{ViewMode, VisibilityGroupId, VisibilityTarget};

use super::state::{apply_changes, get_folder_children, get_manager, get_or_init_manager, is_folder_track, refresh_from_project};

/// Analyze project and show current visibility groups
fn analyze_project_handler() {
    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    info!("\n=== FTS / Visibility Manager: Analyze Project ===\n");
    reaper.show_console_msg("\n=== Visibility Manager: Analyzing Project ===\n\n");

    // Refresh visibility manager from current project
    refresh_from_project();

    // Display groups
    let guard = get_manager();
    if let Some(manager) = guard.as_ref() {
        let groups = manager.groups();

        if groups.is_empty() {
            reaper.show_console_msg("No visibility groups found. Project may be empty.\n");
        } else {
            reaper.show_console_msg(format!("Found {} visibility groups:\n\n", groups.len()).as_str());

            for group in groups {
                let status = if group.active { "VISIBLE" } else { "HIDDEN" };
                reaper.show_console_msg(
                    format!(
                        "  [{}] {} - {} track(s)\n",
                        status,
                        group.name,
                        group.track_count()
                    ).as_str()
                );
            }
        }

        reaper.show_console_msg(format!("\nTotal tracks: {}\n", manager.track_count()).as_str());
        reaper.show_console_msg(format!("View mode: {:?}\n", manager.view_mode()).as_str());
    }

    reaper.show_console_msg("\n=====================================\n\n");
}

/// Show all tracks
fn show_all_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Visibility Manager: Show All");

    unsafe {
        let undo_name = CString::new("Show all tracks").unwrap();
        low.Undo_BeginBlock();

        // Refresh state first
        refresh_from_project();

        let mut guard = get_or_init_manager();
        if let Some(manager) = guard.as_mut() {
            let changes = manager.show_all();
            apply_changes(&changes);

            reaper.show_console_msg(
                format!("Showed {} tracks\n", changes.show.len()).as_str()
            );
        }

        low.Undo_EndBlock(undo_name.as_ptr(), 0);
    }
}

/// Hide all tracks
fn hide_all_handler() {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Visibility Manager: Hide All");

    unsafe {
        let undo_name = CString::new("Hide all tracks").unwrap();
        low.Undo_BeginBlock();

        refresh_from_project();

        let mut guard = get_or_init_manager();
        if let Some(manager) = guard.as_mut() {
            let changes = manager.hide_all();
            apply_changes(&changes);

            reaper.show_console_msg(
                format!("Hid {} tracks\n", changes.hide.len()).as_str()
            );
        }

        low.Undo_EndBlock(undo_name.as_ptr(), 0);
    }
}

/// Toggle Drums visibility
fn toggle_drums_handler() {
    toggle_group_handler("drums", "Drums");
}

/// Toggle Bass visibility
fn toggle_bass_handler() {
    toggle_group_handler("bass", "Bass");
}

/// Toggle Guitars visibility
fn toggle_guitars_handler() {
    toggle_group_handler("guitars", "Guitars");
}

/// Toggle Keys visibility
fn toggle_keys_handler() {
    toggle_group_handler("keys", "Keys");
}

/// Toggle Synths visibility
fn toggle_synths_handler() {
    toggle_group_handler("synths", "Synths");
}

/// Toggle Vocals visibility
fn toggle_vocals_handler() {
    toggle_group_handler("vocals", "Vocals");
}

/// Toggle Horns visibility
fn toggle_horns_handler() {
    toggle_group_handler("horns", "Horns");
}

/// Toggle SFX visibility
fn toggle_sfx_handler() {
    toggle_group_handler("sfx", "SFX");
}

/// Toggle Percussion visibility
fn toggle_percussion_handler() {
    toggle_group_handler("percussion", "Percussion");
}

/// Toggle Orchestra visibility
fn toggle_orchestra_handler() {
    toggle_group_handler("orchestra", "Orchestra");
}

/// Generic group toggle handler
fn toggle_group_handler(group_id: &str, group_name: &str) {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Visibility Manager: Toggle {}", group_name);

    unsafe {
        let undo_name = CString::new(format!("Toggle {} visibility", group_name)).unwrap();
        low.Undo_BeginBlock();

        // Refresh state first
        refresh_from_project();

        let mut guard = get_or_init_manager();
        if let Some(manager) = guard.as_mut() {
            let id = VisibilityGroupId::new(group_id);

            match manager.toggle_group(&id) {
                Ok(changes) => {
                    apply_changes(&changes);

                    let action = if !changes.show.is_empty() {
                        format!("Showed {} {} tracks", changes.show.len(), group_name)
                    } else {
                        format!("Hid {} {} tracks", changes.hide.len(), group_name)
                    };
                    reaper.show_console_msg(format!("{}\n", action).as_str());
                }
                Err(e) => {
                    reaper.show_console_msg(
                        format!("No {} tracks in project\n", group_name).as_str()
                    );
                }
            }
        }

        low.Undo_EndBlock(undo_name.as_ptr(), 0);
    }
}

/// Set view mode to Toggle (independent groups)
fn set_mode_toggle_handler() {
    let reaper = Reaper::get();

    let mut guard = get_or_init_manager();
    if let Some(manager) = guard.as_mut() {
        manager.set_view_mode(ViewMode::Toggle);
        reaper.show_console_msg("View mode set to Toggle (independent groups)\n");
    }
}

/// Set view mode to Exclusive (one group at a time)
fn set_mode_exclusive_handler() {
    let reaper = Reaper::get();

    let mut guard = get_or_init_manager();
    if let Some(manager) = guard.as_mut() {
        manager.set_view_mode(ViewMode::Exclusive);
        reaper.show_console_msg("View mode set to Exclusive (one group at a time)\n");
    }
}

/// Set visibility target to TCP only
fn set_target_tcp_handler() {
    let reaper = Reaper::get();

    let mut guard = get_or_init_manager();
    if let Some(manager) = guard.as_mut() {
        manager.set_target(VisibilityTarget::TCP);
        reaper.show_console_msg("Visibility target set to TCP (arrange view)\n");
    }
}

/// Set visibility target to MCP only
fn set_target_mcp_handler() {
    let reaper = Reaper::get();

    let mut guard = get_or_init_manager();
    if let Some(manager) = guard.as_mut() {
        manager.set_target(VisibilityTarget::MCP);
        reaper.show_console_msg("Visibility target set to MCP (mixer)\n");
    }
}

/// Set visibility target to Both
fn set_target_both_handler() {
    let reaper = Reaper::get();

    let mut guard = get_or_init_manager();
    if let Some(manager) = guard.as_mut() {
        manager.set_target(VisibilityTarget::Both);
        reaper.show_console_msg("Visibility target set to Both (TCP and MCP)\n");
    }
}

/// Save current visibility as a snapshot
fn save_snapshot_handler() {
    let reaper = Reaper::get();

    refresh_from_project();

    let mut guard = get_or_init_manager();
    if let Some(manager) = guard.as_mut() {
        let snapshot_id = manager.save_snapshot("Quick Snapshot");
        reaper.show_console_msg(
            format!("Saved snapshot: {}\n", snapshot_id.0).as_str()
        );
    }
}

// =============================================================================
// STATELESS ACTIONS
// These read directly from REAPER state without maintaining internal state
// =============================================================================

/// Stateless toggle - checks current REAPER visibility state
fn stateless_toggle_group(group_id: &str, group_name: &str) {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Visibility Manager: Stateless Toggle {}", group_name);

    unsafe {
        let undo_name = CString::new(format!("Toggle {} visibility", group_name)).unwrap();
        low.Undo_BeginBlock();

        // Refresh to get current classification and folder hierarchy
        refresh_from_project();

        let guard = get_manager();
        if let Some(manager) = guard.as_ref() {
            let id = VisibilityGroupId::new(group_id);

            if let Some(group) = manager.get_group(&id) {
                if group.is_empty() {
                    reaper.show_console_msg(
                        format!("No {} tracks in project\n", group_name).as_str()
                    );
                    low.Undo_EndBlock(undo_name.as_ptr(), 0);
                    return;
                }

                // Build the full set of track indices including folder children
                let mut all_indices: std::collections::HashSet<usize> = group.track_indices.clone();

                // For each track in the group, if it's a folder, also include its children
                for &track_idx in &group.track_indices {
                    if is_folder_track(track_idx) {
                        let children = get_folder_children(track_idx);
                        all_indices.extend(children);
                    }
                }

                // Check actual REAPER visibility - if ANY track is hidden, we'll show all
                let mut any_hidden = false;
                for &track_idx in &all_indices {
                    let track = low.GetTrack(std::ptr::null_mut(), track_idx as i32);
                    if !track.is_null() {
                        let visible = low.GetMediaTrackInfo_Value(track, c"B_SHOWINTCP".as_ptr()) != 0.0;
                        if !visible {
                            any_hidden = true;
                            break;
                        }
                    }
                }

                // Apply visibility changes
                let show = any_hidden; // If any hidden, show all; if all visible, hide all
                let mut count = 0;

                for &track_idx in &all_indices {
                    let track = low.GetTrack(std::ptr::null_mut(), track_idx as i32);
                    if !track.is_null() {
                        let value = if show { 1.0 } else { 0.0 };
                        low.SetMediaTrackInfo_Value(track, c"B_SHOWINTCP".as_ptr(), value);
                        low.SetMediaTrackInfo_Value(track, c"B_SHOWINMIXER".as_ptr(), value);
                        count += 1;
                    }
                }

                low.TrackList_AdjustWindows(false);
                low.UpdateArrange();

                let action = if show { "Showed" } else { "Hid" };
                reaper.show_console_msg(
                    format!("{} {} {} tracks (including folder children)\n", action, count, group_name).as_str()
                );
            } else {
                reaper.show_console_msg(
                    format!("No {} tracks in project\n", group_name).as_str()
                );
            }
        }

        low.Undo_EndBlock(undo_name.as_ptr(), 0);
    }
}

/// Show only this group, hide all others (shift-click equivalent)
fn show_only_group(group_id: &str, group_name: &str) {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Visibility Manager: Show Only {}", group_name);

    unsafe {
        let undo_name = CString::new(format!("Show only {}", group_name)).unwrap();
        low.Undo_BeginBlock();

        // Refresh to get current classification and folder hierarchy
        refresh_from_project();

        let guard = get_manager();
        if let Some(manager) = guard.as_ref() {
            let id = VisibilityGroupId::new(group_id);

            // Get the track indices for this group
            let base_indices: std::collections::HashSet<usize> = manager
                .get_group(&id)
                .map(|g| g.track_indices.clone())
                .unwrap_or_default();

            if base_indices.is_empty() {
                reaper.show_console_msg(
                    format!("No {} tracks in project\n", group_name).as_str()
                );
                low.Undo_EndBlock(undo_name.as_ptr(), 0);
                return;
            }

            // Build the full set of track indices including folder children
            let mut target_indices = base_indices.clone();
            for &track_idx in &base_indices {
                if is_folder_track(track_idx) {
                    let children = get_folder_children(track_idx);
                    target_indices.extend(children);
                }
            }

            // Hide all tracks, then show only the target group (including children)
            let num_tracks = low.CountTracks(std::ptr::null_mut());
            let mut shown = 0;
            let mut hidden = 0;

            for i in 0..num_tracks {
                let track = low.GetTrack(std::ptr::null_mut(), i);
                if !track.is_null() {
                    if target_indices.contains(&(i as usize)) {
                        // Show this track
                        low.SetMediaTrackInfo_Value(track, c"B_SHOWINTCP".as_ptr(), 1.0);
                        low.SetMediaTrackInfo_Value(track, c"B_SHOWINMIXER".as_ptr(), 1.0);
                        shown += 1;
                    } else {
                        // Hide this track
                        low.SetMediaTrackInfo_Value(track, c"B_SHOWINTCP".as_ptr(), 0.0);
                        low.SetMediaTrackInfo_Value(track, c"B_SHOWINMIXER".as_ptr(), 0.0);
                        hidden += 1;
                    }
                }
            }

            low.TrackList_AdjustWindows(false);
            low.UpdateArrange();

            reaper.show_console_msg(
                format!("Showing {} {} tracks (including folder children), hid {} others\n", shown, group_name, hidden).as_str()
            );
        }

        low.Undo_EndBlock(undo_name.as_ptr(), 0);
    }
}

/// Show this group and all others (ensure group is visible along with everything else)
fn show_with_others_group(group_id: &str, group_name: &str) {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    info!("FTS / Visibility Manager: Show {} With Others", group_name);

    unsafe {
        let undo_name = CString::new(format!("Show {} with others", group_name)).unwrap();
        low.Undo_BeginBlock();

        // Show all tracks
        let num_tracks = low.CountTracks(std::ptr::null_mut());

        for i in 0..num_tracks {
            let track = low.GetTrack(std::ptr::null_mut(), i);
            if !track.is_null() {
                low.SetMediaTrackInfo_Value(track, c"B_SHOWINTCP".as_ptr(), 1.0);
                low.SetMediaTrackInfo_Value(track, c"B_SHOWINMIXER".as_ptr(), 1.0);
            }
        }

        low.TrackList_AdjustWindows(false);
        low.UpdateArrange();

        reaper.show_console_msg(
            format!("Showed all {} tracks (including {})\n", num_tracks, group_name).as_str()
        );

        low.Undo_EndBlock(undo_name.as_ptr(), 0);
    }
}

// Stateless Toggle handlers
fn stateless_toggle_drums() { stateless_toggle_group("drums", "Drums"); }
fn stateless_toggle_bass() { stateless_toggle_group("bass", "Bass"); }
fn stateless_toggle_guitars() { stateless_toggle_group("guitars", "Guitars"); }
fn stateless_toggle_keys() { stateless_toggle_group("keys", "Keys"); }
fn stateless_toggle_synths() { stateless_toggle_group("synths", "Synths"); }
fn stateless_toggle_vocals() { stateless_toggle_group("vocals", "Vocals"); }
fn stateless_toggle_horns() { stateless_toggle_group("horns", "Horns"); }
fn stateless_toggle_sfx() { stateless_toggle_group("sfx", "SFX"); }
fn stateless_toggle_percussion() { stateless_toggle_group("percussion", "Percussion"); }
fn stateless_toggle_orchestra() { stateless_toggle_group("orchestra", "Orchestra"); }

// Show Only handlers (shift-click equivalent)
fn show_only_drums() { show_only_group("drums", "Drums"); }
fn show_only_bass() { show_only_group("bass", "Bass"); }
fn show_only_guitars() { show_only_group("guitars", "Guitars"); }
fn show_only_keys() { show_only_group("keys", "Keys"); }
fn show_only_synths() { show_only_group("synths", "Synths"); }
fn show_only_vocals() { show_only_group("vocals", "Vocals"); }
fn show_only_horns() { show_only_group("horns", "Horns"); }
fn show_only_sfx() { show_only_group("sfx", "SFX"); }
fn show_only_percussion() { show_only_group("percussion", "Percussion"); }
fn show_only_orchestra() { show_only_group("orchestra", "Orchestra"); }

// Show With Others handlers
fn show_with_others_drums() { show_with_others_group("drums", "Drums"); }
fn show_with_others_bass() { show_with_others_group("bass", "Bass"); }
fn show_with_others_guitars() { show_with_others_group("guitars", "Guitars"); }
fn show_with_others_keys() { show_with_others_group("keys", "Keys"); }
fn show_with_others_synths() { show_with_others_group("synths", "Synths"); }
fn show_with_others_vocals() { show_with_others_group("vocals", "Vocals"); }
fn show_with_others_horns() { show_with_others_group("horns", "Horns"); }
fn show_with_others_sfx() { show_with_others_group("sfx", "SFX"); }
fn show_with_others_percussion() { show_with_others_group("percussion", "Percussion"); }
fn show_with_others_orchestra() { show_with_others_group("orchestra", "Orchestra"); }

/// Get Visibility Manager actions for registration
pub fn actions() -> Vec<ActionDef> {
    vec![
        // Analysis
        ActionDef {
            command_id: "FTS_VM_ANALYZE",
            display_name: "Analyze Project".to_string(),
            handler: analyze_project_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        // Show/Hide All
        ActionDef {
            command_id: "FTS_VM_SHOW_ALL",
            display_name: "Show All Tracks".to_string(),
            handler: show_all_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_HIDE_ALL",
            display_name: "Hide All Tracks".to_string(),
            handler: hide_all_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        // Group Toggles
        ActionDef {
            command_id: "FTS_VM_TOGGLE_DRUMS",
            display_name: "Toggle Drums".to_string(),
            handler: toggle_drums_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_TOGGLE_BASS",
            display_name: "Toggle Bass".to_string(),
            handler: toggle_bass_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_TOGGLE_GUITARS",
            display_name: "Toggle Guitars".to_string(),
            handler: toggle_guitars_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_TOGGLE_KEYS",
            display_name: "Toggle Keys".to_string(),
            handler: toggle_keys_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_TOGGLE_SYNTHS",
            display_name: "Toggle Synths".to_string(),
            handler: toggle_synths_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_TOGGLE_VOCALS",
            display_name: "Toggle Vocals".to_string(),
            handler: toggle_vocals_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_TOGGLE_HORNS",
            display_name: "Toggle Horns".to_string(),
            handler: toggle_horns_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_TOGGLE_SFX",
            display_name: "Toggle SFX".to_string(),
            handler: toggle_sfx_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_TOGGLE_PERCUSSION",
            display_name: "Toggle Percussion".to_string(),
            handler: toggle_percussion_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_TOGGLE_ORCHESTRA",
            display_name: "Toggle Orchestra".to_string(),
            handler: toggle_orchestra_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        // View Mode
        ActionDef {
            command_id: "FTS_VM_MODE_TOGGLE",
            display_name: "Set Mode: Toggle".to_string(),
            handler: set_mode_toggle_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_MODE_EXCLUSIVE",
            display_name: "Set Mode: Exclusive".to_string(),
            handler: set_mode_exclusive_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        // Target
        ActionDef {
            command_id: "FTS_VM_TARGET_TCP",
            display_name: "Target: TCP Only".to_string(),
            handler: set_target_tcp_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_TARGET_MCP",
            display_name: "Target: MCP Only".to_string(),
            handler: set_target_mcp_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_TARGET_BOTH",
            display_name: "Target: Both".to_string(),
            handler: set_target_both_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        // Snapshots
        ActionDef {
            command_id: "FTS_VM_SAVE_SNAPSHOT",
            display_name: "Save Snapshot".to_string(),
            handler: save_snapshot_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },

        // =========================================================================
        // STATELESS TOGGLE ACTIONS (check actual REAPER state)
        // =========================================================================
        ActionDef {
            command_id: "FTS_VM_STATELESS_TOGGLE_DRUMS",
            display_name: "[Stateless] Toggle Drums".to_string(),
            handler: stateless_toggle_drums,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_STATELESS_TOGGLE_BASS",
            display_name: "[Stateless] Toggle Bass".to_string(),
            handler: stateless_toggle_bass,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_STATELESS_TOGGLE_GUITARS",
            display_name: "[Stateless] Toggle Guitars".to_string(),
            handler: stateless_toggle_guitars,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_STATELESS_TOGGLE_KEYS",
            display_name: "[Stateless] Toggle Keys".to_string(),
            handler: stateless_toggle_keys,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_STATELESS_TOGGLE_SYNTHS",
            display_name: "[Stateless] Toggle Synths".to_string(),
            handler: stateless_toggle_synths,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_STATELESS_TOGGLE_VOCALS",
            display_name: "[Stateless] Toggle Vocals".to_string(),
            handler: stateless_toggle_vocals,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_STATELESS_TOGGLE_HORNS",
            display_name: "[Stateless] Toggle Horns".to_string(),
            handler: stateless_toggle_horns,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_STATELESS_TOGGLE_SFX",
            display_name: "[Stateless] Toggle SFX".to_string(),
            handler: stateless_toggle_sfx,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_STATELESS_TOGGLE_PERCUSSION",
            display_name: "[Stateless] Toggle Percussion".to_string(),
            handler: stateless_toggle_percussion,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_STATELESS_TOGGLE_ORCHESTRA",
            display_name: "[Stateless] Toggle Orchestra".to_string(),
            handler: stateless_toggle_orchestra,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },

        // =========================================================================
        // SHOW ONLY ACTIONS (shift-click equivalent - show group, hide all others)
        // =========================================================================
        ActionDef {
            command_id: "FTS_VM_SHOW_ONLY_DRUMS",
            display_name: "Show Only Drums".to_string(),
            handler: show_only_drums,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_SHOW_ONLY_BASS",
            display_name: "Show Only Bass".to_string(),
            handler: show_only_bass,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_SHOW_ONLY_GUITARS",
            display_name: "Show Only Guitars".to_string(),
            handler: show_only_guitars,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_SHOW_ONLY_KEYS",
            display_name: "Show Only Keys".to_string(),
            handler: show_only_keys,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_SHOW_ONLY_SYNTHS",
            display_name: "Show Only Synths".to_string(),
            handler: show_only_synths,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_SHOW_ONLY_VOCALS",
            display_name: "Show Only Vocals".to_string(),
            handler: show_only_vocals,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_SHOW_ONLY_HORNS",
            display_name: "Show Only Horns".to_string(),
            handler: show_only_horns,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_SHOW_ONLY_SFX",
            display_name: "Show Only SFX".to_string(),
            handler: show_only_sfx,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_SHOW_ONLY_PERCUSSION",
            display_name: "Show Only Percussion".to_string(),
            handler: show_only_percussion,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_SHOW_ONLY_ORCHESTRA",
            display_name: "Show Only Orchestra".to_string(),
            handler: show_only_orchestra,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },

        // =========================================================================
        // SHOW WITH OTHERS ACTIONS (show group + all other tracks)
        // =========================================================================
        ActionDef {
            command_id: "FTS_VM_SHOW_WITH_OTHERS_DRUMS",
            display_name: "Show Drums With Others".to_string(),
            handler: show_with_others_drums,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_SHOW_WITH_OTHERS_BASS",
            display_name: "Show Bass With Others".to_string(),
            handler: show_with_others_bass,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_SHOW_WITH_OTHERS_GUITARS",
            display_name: "Show Guitars With Others".to_string(),
            handler: show_with_others_guitars,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_SHOW_WITH_OTHERS_KEYS",
            display_name: "Show Keys With Others".to_string(),
            handler: show_with_others_keys,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_SHOW_WITH_OTHERS_SYNTHS",
            display_name: "Show Synths With Others".to_string(),
            handler: show_with_others_synths,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_SHOW_WITH_OTHERS_VOCALS",
            display_name: "Show Vocals With Others".to_string(),
            handler: show_with_others_vocals,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_SHOW_WITH_OTHERS_HORNS",
            display_name: "Show Horns With Others".to_string(),
            handler: show_with_others_horns,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_SHOW_WITH_OTHERS_SFX",
            display_name: "Show SFX With Others".to_string(),
            handler: show_with_others_sfx,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_SHOW_WITH_OTHERS_PERCUSSION",
            display_name: "Show Percussion With Others".to_string(),
            handler: show_with_others_percussion,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_VM_SHOW_WITH_OTHERS_ORCHESTRA",
            display_name: "Show Orchestra With Others".to_string(),
            handler: show_with_others_orchestra,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
    ]
}

/// Register Visibility Manager actions
pub fn register_visibility_manager_actions() {
    crate::infrastructure::action_registry::register_actions(&actions(), "Visibility Manager");
}
