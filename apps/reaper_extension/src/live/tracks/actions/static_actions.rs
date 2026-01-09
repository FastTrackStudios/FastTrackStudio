//! Static action definitions for Live Tracks
//!
//! These are the core actions that are always available for setlist management
//! and smooth seek operations.

use super::navigation::{
    go_to_next_section, go_to_next_section_song_smart, go_to_next_song, go_to_previous_section,
    go_to_previous_section_song_smart, go_to_previous_song,
};
use super::zoom::zoom_horizontally_to_song;
use crate::infrastructure::action_registry::{
    ActionDef, get_registered_actions_storage, register_actions,
};
use crate::live::tracks::tab_navigation::TabNavigator;
use daw::transport::TransportActions;
use fts::setlist::infra::reaper::{ReaperTransport, read_markers_from_project};
use fts::setlist::infra::traits::SetlistBuilder;
use reaper_high::{ActionKind, Project, Reaper};
use reaper_medium::{PositionInSeconds, ProjectRef, SetEditCurPosOptions};
use std::sync::{Mutex, OnceLock};
use std::time::{Duration, Instant};
use tracing::{debug, info, warn};

/// Helper: Find Count-In marker position in a project
fn find_count_in_marker(project: &Project) -> Option<f64> {
    let markers = read_markers_from_project(project).ok()?;
    markers
        .iter()
        .find(|m| m.name.trim().eq_ignore_ascii_case("Count-In"))
        .map(|m| m.position.time.to_seconds())
}

/// Helper: Stop all projects across all tabs
fn stop_all_projects() {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();

    // Stop all projects by iterating through tabs
    for i in 0..128u32 {
        if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 512) {
            let project = Project::new(result.project);
            if project.is_playing() || project.is_paused() {
                project.stop();
            }
        } else {
            break;
        }
    }
}

/// Helper: Play the current project
fn play_current_project() {
    let reaper = Reaper::get();
    let current_project = reaper.current_project();
    let mut transport = ReaperTransport::new(current_project);
    if let Err(e) = transport.play() {
        warn!(error = %e, "Failed to play current project");
    }
}

/// Helper: Pause the current project
fn pause_current_project() {
    let reaper = Reaper::get();
    let current_project = reaper.current_project();
    let mut transport = ReaperTransport::new(current_project);
    if let Err(e) = transport.pause() {
        warn!(error = %e, "Failed to pause current project");
    }
}

/// Action handlers

pub fn setlist_play() {
    let reaper = Reaper::get();
    debug!("Setlist Play action executed");
    play_current_project();
}

pub fn setlist_stop() {
    let reaper = Reaper::get();
    debug!("Setlist Stop action executed");
    stop_all_projects();
}

pub fn setlist_pause() {
    let reaper = Reaper::get();
    debug!("Setlist Pause action executed");
    pause_current_project();
}

fn setlist_resume() {
    let reaper = Reaper::get();
    debug!("Setlist Resume action executed");
    play_current_project(); // Resume is same as play in REAPER
}

fn setlist_resume_smart() {
    let reaper = Reaper::get();
    debug!("Setlist Resume Smart action executed");
    // For now, same as regular resume
    play_current_project();
}

/// Check if any project is currently playing
fn is_any_project_playing() -> bool {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();

    for i in 0..128u32 {
        if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 512) {
            let project = Project::new(result.project);
            let play_state = project.play_state();
            if play_state.is_playing {
                return true;
            }
        } else {
            break;
        }
    }
    false
}

/// Toggle Play/Pause: If playing, pause; otherwise, play
pub fn setlist_play_pause_toggle() {
    let reaper = Reaper::get();
    let is_playing = is_any_project_playing();

    if is_playing {
        debug!("Setlist Play/Pause toggle: Currently playing, pausing");
        pause_current_project();
    } else {
        debug!("Setlist Play/Pause toggle: Not playing, starting playback");
        play_current_project();
    }
}

/// Toggle Play/Stop: If playing, stop; otherwise, play
fn setlist_play_stop_toggle() {
    let reaper = Reaper::get();
    let is_playing = is_any_project_playing();

    if is_playing {
        debug!("Setlist Play/Stop toggle: Currently playing, stopping");
        stop_all_projects();
    } else {
        debug!("Setlist Play/Stop toggle: Not playing, starting playback");
        play_current_project();
    }
}

/// Reset current song to beginning (0.0 or count-in/start marker position)
fn reset_to_beginning_of_song() {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();

    info!("Reset to Beginning of Song action executed");

    // Get current project
    let project_result = match medium_reaper.enum_projects(ProjectRef::Current, 0) {
        Some(result) => result,
        None => {
            warn!("No current project for reset to beginning of song");
            reaper.show_console_msg("Reset to Beginning of Song: No current project\n");
            return;
        }
    };

    let project = Project::new(project_result.project);
    let project_name = project_result
        .file_path
        .as_ref()
        .and_then(|p| p.as_std_path().file_stem())
        .and_then(|s| s.to_str())
        .map(|s| s.to_string())
        .unwrap_or_else(|| "unsaved".to_string());

    // Calculate start position (use Count-In marker if available, otherwise 0.0)
    let start_pos = if let Some(count_in) = find_count_in_marker(&project) {
        PositionInSeconds::new(count_in).unwrap_or(PositionInSeconds::ZERO)
    } else {
        PositionInSeconds::ZERO
    };

    // Stop playback if playing
    if project.is_playing() || project.is_paused() {
        project.stop();
    }

    // Set cursor to beginning position
    project.set_edit_cursor_position(
        start_pos,
        SetEditCurPosOptions {
            move_view: false,
            seek_play: false,
        },
    );

    info!(
        project_name = %project_name,
        position = start_pos.get(),
        "Reset to beginning of song"
    );
    reaper.show_console_msg(format!("Reset to Beginning of Song: {}\n", project_name));
}

/// Reset to beginning of setlist (first song, beginning position)
fn reset_to_beginning_of_setlist() {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();

    info!("Reset to Beginning of Setlist action executed");

    // Build setlist from tabs - use trait method on Reaper (operates on all open projects)
    let setlist = match reaper.build_setlist_from_open_projects(None) {
        Ok(s) => s,
        Err(e) => {
            warn!(error = %e, "Failed to build setlist for reset");
            reaper.show_console_msg(
                "Reset to Beginning of Setlist: Failed to build setlist\n".to_string(),
            );
            return;
        }
    };

    if setlist.songs.is_empty() {
        warn!("Setlist is empty, resetting current project instead");
        reset_to_beginning_of_song();
        return;
    }

    // Get first song
    let first_song = &setlist.songs[0];
    let first_project_name = first_song.project_name_from_metadata();

    // Find tab index for first song
    let tab_index = {
        let mut found_index = None;
        for i in 0..128u32 {
            if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 512) {
                let tab_name = if let Some(file_path) = result.file_path.as_ref() {
                    file_path
                        .as_std_path()
                        .file_stem()
                        .and_then(|s| s.to_str())
                        .map(|s| s.to_string())
                        .unwrap_or_else(|| format!("Tab {}", i))
                } else {
                    format!("Tab {}", i)
                };

                let normalized_tab = tab_name.to_uppercase().replace('_', "-");
                let normalized_target = first_project_name.to_uppercase().replace('_', "-");

                if normalized_tab == normalized_target {
                    found_index = Some(i);
                    break;
                }
            }
        }
        found_index
    };

    let tab_index = match tab_index {
        Some(idx) => idx,
        None => {
            warn!(project_name = %first_project_name, "First setlist project not found");
            reaper.show_console_msg(format!(
                "Reset to Beginning of Setlist: Project '{}' not found\n",
                first_project_name
            ));
            return;
        }
    };

    // Calculate start position for first song (use Count-In marker if available, otherwise 0.0)
    let start_pos = if let Some(count_in_marker) = &first_song.count_in_marker {
        PositionInSeconds::new(count_in_marker.position.time.to_seconds())
            .unwrap_or(PositionInSeconds::ZERO)
    } else {
        PositionInSeconds::ZERO
    };

    // Stop all playback
    stop_all_projects();

    // Switch to first tab using TabNavigator
    let tab_navigator = TabNavigator::new();
    if let Err(e) = tab_navigator.switch_to_tab(tab_index as usize) {
        warn!(error = %e, tab_index, "Failed to switch to first tab");
        reaper.show_console_msg(format!(
            "Reset to Beginning of Setlist: Failed to switch to tab {}: {}\n",
            tab_index, e
        ));
        return;
    }

    // Small delay for tab switch to complete
    std::thread::sleep(std::time::Duration::from_millis(50));

    // Get the now-current project and set cursor
    if let Some(result) = medium_reaper.enum_projects(ProjectRef::Current, 0) {
        let project = Project::new(result.project);
        project.set_edit_cursor_position(
            start_pos,
            SetEditCurPosOptions {
                move_view: false,
                seek_play: false,
            },
        );
    }

    info!(
        tab_index,
        project_name = %first_project_name,
        position = start_pos.get(),
        "Reset to beginning of setlist"
    );
    reaper.show_console_msg(format!(
        "Reset to Beginning of Setlist: {} (tab {})\n",
        first_project_name, tab_index
    ));
}

/// Track last execution time for double-tap detection
static LAST_RESET_ACTION_TIME: OnceLock<Mutex<Option<Instant>>> = OnceLock::new();

/// Get or initialize the last reset action time storage
fn get_last_reset_time() -> &'static Mutex<Option<Instant>> {
    LAST_RESET_ACTION_TIME.get_or_init(|| Mutex::new(None))
}

/// Smart reset: First tap goes to beginning of song, second tap (within 1 second) goes to beginning of setlist
fn reset_to_beginning_smart() {
    let reaper = Reaper::get();
    let now = Instant::now();
    let double_tap_window = Duration::from_millis(1000); // 1 second window

    // Check if this is a double-tap
    let is_double_tap = {
        let mut last_time = get_last_reset_time().lock().unwrap();
        if let Some(last) = *last_time {
            let elapsed = now.duration_since(last);
            if elapsed < double_tap_window {
                // Double-tap detected
                *last_time = None; // Clear after double-tap
                true
            } else {
                // Too long since last tap, treat as new single tap
                *last_time = Some(now);
                false
            }
        } else {
            // First tap
            *last_time = Some(now);
            false
        }
    };

    if is_double_tap {
        info!("Smart Reset: Double-tap detected, going to beginning of setlist");
        reset_to_beginning_of_setlist();
    } else {
        info!("Smart Reset: Single tap, going to beginning of song");
        reset_to_beginning_of_song();
    }
}

/// Get all live tracks actions
pub fn domain_actions() -> Vec<ActionDef> {
    vec![
        ActionDef {
            command_id: "FTS_LIVE_SETLIST_PLAY",
            display_name: "Setlist Play".to_string(),
            handler: setlist_play,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_LIVE_SETLIST_STOP",
            display_name: "Setlist Stop".to_string(),
            handler: setlist_stop,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_LIVE_SETLIST_PAUSE",
            display_name: "Setlist Pause".to_string(),
            handler: setlist_pause,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_LIVE_SETLIST_RESUME",
            display_name: "Setlist Resume".to_string(),
            handler: setlist_resume,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_LIVE_RESET_TO_BEGINNING_OF_SONG",
            display_name: "Reset to Beginning of Song".to_string(),
            handler: reset_to_beginning_of_song,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_LIVE_RESET_TO_BEGINNING_OF_SETLIST",
            display_name: "Reset to Beginning of Setlist".to_string(),
            handler: reset_to_beginning_of_setlist,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_LIVE_RESET_TO_BEGINNING_SMART",
            display_name: "Reset to Beginning (Double-Tap for Setlist)".to_string(),
            handler: reset_to_beginning_smart,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_LIVE_SETLIST_RESUME_SMART",
            display_name: "Setlist Resume (Smart: From Last Stopped if No Pause)".to_string(),
            handler: setlist_resume_smart,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_LIVE_ZOOM_HORIZONTALLY_TO_SONG",
            display_name: "Zoom Horizontally to Song".to_string(),
            handler: zoom_horizontally_to_song,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_LIVE_GO_TO_PREVIOUS_SONG",
            display_name: "Go To Previous Song".to_string(),
            handler: go_to_previous_song,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_LIVE_GO_TO_NEXT_SONG",
            display_name: "Go To Next Song".to_string(),
            handler: go_to_next_song,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_LIVE_GO_TO_NEXT_SECTION",
            display_name: "Go To Next Section".to_string(),
            handler: go_to_next_section,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_LIVE_GO_TO_PREVIOUS_SECTION",
            display_name: "Go To Previous Section".to_string(),
            handler: go_to_previous_section,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_LIVE_GO_TO_NEXT_SECTION_SONG_SMART",
            display_name: "Go To Next Section / Song (Smart)".to_string(),
            handler: go_to_next_section_song_smart,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_LIVE_GO_TO_PREVIOUS_SECTION_SONG_SMART",
            display_name: "Go To Previous Section / Song (Smart)".to_string(),
            handler: go_to_previous_section_song_smart,
            appears_in_menu: true,
            section: crate::infrastructure::action_registry::ActionSection::Main,
            ..Default::default()
        },
    ]
}

/// Register toggleable actions separately (they need ActionKind::Toggleable)
pub fn register_toggleable_actions() {
    use tracing::info;

    info!("Live Tracks: Registering toggleable actions");

    let reaper = Reaper::get();

    // Register Play/Pause toggle
    let play_pause_toggle_handler = setlist_play_pause_toggle;
    let play_pause_state_check = || is_any_project_playing();
    let registered_action1 = reaper.register_action(
        "FTS_LIVE_SETLIST_PLAY_PAUSE",
        "FTS / Live Tracks: Setlist Play/Pause",
        None,
        move || play_pause_toggle_handler(),
        ActionKind::Toggleable(Box::new(move || play_pause_state_check())),
    );

    // Register Play/Stop toggle
    let play_stop_toggle_handler = setlist_play_stop_toggle;
    let play_stop_state_check = || is_any_project_playing();
    let registered_action2 = reaper.register_action(
        "FTS_LIVE_SETLIST_PLAY_STOP",
        "FTS / Live Tracks: Setlist Play/Stop",
        None,
        move || play_stop_toggle_handler(),
        ActionKind::Toggleable(Box::new(move || play_stop_state_check())),
    );

    // Store the RegisteredActions to keep them alive
    if let Ok(mut storage) = get_registered_actions_storage().lock() {
        storage.push(registered_action1);
        storage.push(registered_action2);
    }

    // Look up and store command IDs for toggleable actions (must be done on main thread)
    // This allows background threads to trigger these actions
    let medium_reaper = reaper.medium_reaper();
    let command_ids_map = crate::infrastructure::action_registry::COMMAND_IDS
        .get_or_init(|| std::sync::Mutex::new(std::collections::HashMap::new()));
    if let Ok(mut map) = command_ids_map.lock() {
        // Look up command IDs for toggleable actions
        let toggleable_command_ids = ["FTS_LIVE_SETLIST_PLAY_PAUSE", "FTS_LIVE_SETLIST_PLAY_STOP"];

        for command_id_str in &toggleable_command_ids {
            // Try both with and without underscore prefix
            let lookup_names = [*command_id_str, &format!("_{}", command_id_str)];
            for lookup_name in &lookup_names {
                if let Some(cmd_id) = medium_reaper.named_command_lookup(*lookup_name) {
                    map.insert(*command_id_str, cmd_id);
                    debug!(
                        command_id = %command_id_str,
                        lookup_name = %lookup_name,
                        "Stored command ID for toggleable action"
                    );
                    break;
                }
            }
        }

        info!(
            stored_toggleable_count = map.len(),
            "Stored command IDs for toggleable actions"
        );
    }

    info!("Live Tracks: Toggleable actions registered");
}

/// Register all live tracks actions with REAPER
pub fn register_all_actions() {
    use tracing::info;

    info!("Live Tracks: Starting action registration");

    // Get all actions
    let actions = domain_actions();
    info!(
        action_count = actions.len(),
        "Live Tracks: Created {} actions, registering now",
        actions.len()
    );

    if actions.is_empty() {
        warn!("Live Tracks: WARNING - No actions to register!");
        return;
    }

    // Convert Vec<ActionDef> to &[ActionDef] for register_actions
    register_actions(&actions, "Live Tracks");
    info!("Live Tracks: Action registration complete");

    // Register toggleable actions separately
    register_toggleable_actions();
}

// Re-export as ACTION_DEFS for compatibility (empty for now, can be populated if needed)
pub static ACTION_DEFS: &[()] = &[];
