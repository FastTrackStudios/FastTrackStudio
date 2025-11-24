//! REAPER Setlist Stream Implementation
//!
//! Implements the setlist stream backend for REAPER extension.
//! Uses the protocol definition from the setlist crate.

use anyhow::Result;
use std::sync::{Arc, Mutex};
use std::sync::atomic::{AtomicBool, Ordering};
use tracing::{info, warn};
use setlist::{
    SetlistApi, SetlistStreamApi, SetlistStateProvider, SetlistCommandHandler,
    TransportCommand, NavigationCommand,
};

use crate::reaper_setlist::build_setlist_from_open_projects;
use crate::live::tracks::tab_navigation::TabNavigator;
use crate::live::tracks::actions::go_to_song;
use crate::live::tracks::smooth_seek::get_smooth_seek_handler;
use crate::action_registry::get_command_id;
use reaper_high::{Reaper, Project};
use reaper_medium::{ProjectRef, PositionInSeconds, PositionInBeats, SetEditCurPosOptions, CommandId, ProjectContext, MeasureMode};
use std::sync::OnceLock;

/// Get the project measure offset for a project
/// Returns the offset value, or 0 if not found
fn get_project_measure_offset(project: &Project) -> i32 {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    
    // Get the project measure offset using project_config_var_get_offs
    if let Some(offs_result) = medium_reaper.project_config_var_get_offs("projmeasoffs") {
        // Get the actual value using the offset
        if let Some(addr) = medium_reaper.project_config_var_addr(project.context(), offs_result.offset) {
            // Read the integer value directly from the pointer (it's a 32-bit integer)
            unsafe { *(addr.as_ptr() as *const i32) }
        } else {
            0
        }
    } else {
        0
    }
}

/// Shared state for setlist updates
/// Updated from REAPER's main thread, read from async tasks
static LATEST_SETLIST_API: OnceLock<Arc<std::sync::Mutex<Option<SetlistApi>>>> = OnceLock::new();
static SETLIST_UPDATED: AtomicBool = AtomicBool::new(false);

/// Type alias for seek request channel (using std::sync for main thread compatibility)
type SeekRequestSender = std::sync::mpsc::Sender<(usize, usize, std::sync::mpsc::Sender<Result<(), String>>)>;
type SeekRequestReceiver = std::sync::mpsc::Receiver<(usize, usize, std::sync::mpsc::Sender<Result<(), String>>)>;

/// Type alias for command execution request channel
type CommandRequestSender = std::sync::mpsc::Sender<(CommandRequest, std::sync::mpsc::Sender<Result<(), String>>)>;
type CommandRequestReceiver = std::sync::mpsc::Receiver<(CommandRequest, std::sync::mpsc::Sender<Result<(), String>>)>;

/// Command execution request
enum CommandRequest {
    Transport(TransportCommand),
    Navigation(NavigationCommand),
    SeekToSong(usize),
    ToggleLoop,
}

/// Channel for sending seek requests from async tasks to main thread
static SEEK_REQUEST_CHANNEL: OnceLock<Arc<Mutex<Option<SeekRequestSender>>>> = OnceLock::new();
static SEEK_REQUEST_RECEIVER: OnceLock<Arc<Mutex<Option<SeekRequestReceiver>>>> = OnceLock::new();

/// Channel for sending command execution requests from async tasks to main thread
static COMMAND_REQUEST_CHANNEL: OnceLock<Arc<Mutex<Option<CommandRequestSender>>>> = OnceLock::new();
static COMMAND_REQUEST_RECEIVER: OnceLock<Arc<Mutex<Option<CommandRequestReceiver>>>> = OnceLock::new();

/// Initialize the setlist state storage
pub fn init_setlist_state() -> Arc<std::sync::Mutex<Option<SetlistApi>>> {
    let state = Arc::new(std::sync::Mutex::new(None));
    LATEST_SETLIST_API.set(state.clone()).expect("Setlist API state already initialized");
    
    // Initialize seek request channel (using std::sync for main thread compatibility)
    let (tx, rx) = std::sync::mpsc::channel();
    SEEK_REQUEST_CHANNEL.set(Arc::new(Mutex::new(Some(tx)))).expect("Seek channel already initialized");
    SEEK_REQUEST_RECEIVER.set(Arc::new(Mutex::new(Some(rx)))).expect("Seek receiver already initialized");
    
    // Initialize command execution request channel
    let (cmd_tx, cmd_rx) = std::sync::mpsc::channel();
    COMMAND_REQUEST_CHANNEL.set(Arc::new(Mutex::new(Some(cmd_tx)))).expect("Command channel already initialized");
    COMMAND_REQUEST_RECEIVER.set(Arc::new(Mutex::new(Some(cmd_rx)))).expect("Command receiver already initialized");
    
    info!("[REAPER Setlist] Setlist API state storage initialized.");
    state
}

/// Process smooth seek queue (check if we should execute queued seeks)
pub fn process_smooth_seek_queue() {
    use crate::live::tracks::smooth_seek::get_smooth_seek_handler;
    use reaper_high::Reaper;
    use reaper_medium::ProjectRef;
    
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    
    // Get current project
    if let Some(project_result) = medium_reaper.enum_projects(ProjectRef::Current, 0) {
        let project = reaper_high::Project::new(project_result.project);
        let play_state = project.play_state();
        
        // Only process smooth seek queue if we're playing
        if play_state.is_playing || play_state.is_paused {
            // Get current play position using transport adapter
            use crate::reaper_transport::ReaperTransport;
            let transport = ReaperTransport::new(project.clone());
            if let Some(transport_info) = transport.read_transport().ok() {
                let current_pos = PositionInSeconds::new(transport_info.playhead_position.time.to_seconds())
                    .unwrap_or_else(|_| project.edit_cursor_position().unwrap_or(PositionInSeconds::ZERO));
                
                let smooth_seek_handler = get_smooth_seek_handler();
                if let Err(e) = smooth_seek_handler.process_playback_position(&project, current_pos) {
                    tracing::warn!(error = %e, "Error processing smooth seek queue");
                }
            }
        }
        
        // Process pending marker deletions (always, not just when playing)
        let smooth_seek_handler = get_smooth_seek_handler();
        smooth_seek_handler.process_pending_deletions(&project);
    }
}

/// Process pending seek requests (called from main thread)
pub fn process_seek_requests() {
    if let Some(receiver_arc) = SEEK_REQUEST_RECEIVER.get() {
        if let Ok(mut receiver_guard) = receiver_arc.lock() {
            if let Some(receiver) = receiver_guard.as_mut() {
                // Try to receive all pending requests (non-blocking)
                while let Ok((song_index, section_index, tx)) = receiver.try_recv() {
                    let result = seek_to_section_main_thread(song_index, section_index);
                    let _ = tx.send(result);
                }
            }
        }
    }
}

/// Process pending command execution requests (called from main thread)
pub fn process_command_requests() {
    if let Some(receiver_arc) = COMMAND_REQUEST_RECEIVER.get() {
        if let Ok(mut receiver_guard) = receiver_arc.lock() {
            if let Some(receiver) = receiver_guard.as_mut() {
                // Try to receive all pending requests (non-blocking)
                while let Ok((request, tx)) = receiver.try_recv() {
                    let result = execute_command_main_thread(request);
                    let _ = tx.send(result);
                }
            }
        }
    }
}

/// Execute command on main thread
fn execute_command_main_thread(request: CommandRequest) -> Result<(), String> {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    
    match request {
        CommandRequest::Transport(cmd) => {
            let command_id_str = match cmd {
                TransportCommand::Play => "FTS_LIVE_SETLIST_PLAY",
                TransportCommand::Pause => "FTS_LIVE_SETLIST_PAUSE",
                TransportCommand::Stop => "FTS_LIVE_SETLIST_STOP",
                TransportCommand::TogglePlayPause => "FTS_LIVE_SETLIST_PLAY_PAUSE",
            };
            
            let cmd_id = get_command_id(command_id_str)
                .ok_or_else(|| format!("Command ID not found for: {}", command_id_str))?;
            
            medium_reaper.main_on_command_ex(cmd_id, 0, ProjectContext::CurrentProject);
            info!("Executed transport command: {:?}", cmd);
            Ok(())
        }
        CommandRequest::Navigation(cmd) => {
            let command_id_str = match cmd {
                NavigationCommand::NextSectionOrSong => "FTS_LIVE_GO_TO_NEXT_SECTION_SONG_SMART",
                NavigationCommand::PreviousSectionOrSong => "FTS_LIVE_GO_TO_PREVIOUS_SECTION_SONG_SMART",
            };
            
            let cmd_id = get_command_id(command_id_str)
                .ok_or_else(|| format!("Command ID not found for: {}", command_id_str))?;
            
            medium_reaper.main_on_command_ex(cmd_id, 0, ProjectContext::CurrentProject);
            info!("Executed navigation command: {:?}", cmd);
            Ok(())
        }
        CommandRequest::SeekToSong(song_index) => {
            go_to_song(song_index);
            info!("Executed seek to song: {}", song_index);
            Ok(())
        }
        CommandRequest::ToggleLoop => {
            let cmd_id = CommandId::new(1068); // Toggle repeat
            medium_reaper.main_on_command_ex(cmd_id, 0, ProjectContext::CurrentProject);
            info!("Executed toggle loop command");
            Ok(())
        }
    }
}

/// Seek to section (executes on main thread)
fn seek_to_section_main_thread(song_index: usize, section_index: usize) -> Result<(), String> {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    
    // Build setlist to find the song
    let setlist = build_setlist_from_open_projects(None)
        .map_err(|e| format!("Failed to build setlist: {}", e))?;
    
    if song_index >= setlist.songs.len() {
        return Err(format!("Song index {} out of range", song_index));
    }
    
    let song = &setlist.songs[song_index];
    let project_name = song.project_name_from_metadata();
    
    // Find the project tab for this song
    let mut found_tab_index = None;
    for i in 0..128u32 {
        if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 512) {
            let tab_name = result.file_path.as_ref()
                .and_then(|p| p.as_std_path().file_stem())
                .and_then(|s| s.to_str())
                .map(|s| s.to_string())
                .unwrap_or_else(|| format!("Tab {}", i));
            
            let normalized_tab = tab_name.to_uppercase().replace('_', "-");
            let normalized_target = project_name.to_uppercase().replace('_', "-");
            
            if normalized_tab == normalized_target {
                found_tab_index = Some(i as usize);
                break;
            }
        }
    }
    
    let tab_index = found_tab_index
        .ok_or_else(|| format!("Could not find project for song: {}", project_name))?;
    
    // Get current project to check if we're playing and if it's the same song
    let current_project_result = medium_reaper.enum_projects(ProjectRef::Current, 0)
        .ok_or_else(|| "No current project".to_string())?;
    
    let current_project = reaper_high::Project::new(current_project_result.project);
    let play_state = current_project.play_state();
    let is_playing = play_state.is_playing || play_state.is_paused;
    
    // Check if we're clicking a section in the same song (same tab)
    let current_tab_index = {
        let current_project_raw = current_project_result.project;
        let mut found_current_tab = None;
        for i in 0..128u32 {
            if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 512) {
                if result.project == current_project_raw {
                    found_current_tab = Some(i as usize);
                    break;
                }
            }
        }
        found_current_tab
    };
    
    let is_same_song = current_tab_index == Some(tab_index);
    
    // Get the section position
    if section_index >= song.sections.len() {
        return Err(format!("Section index {} out of range", section_index));
    }
    
    let section = &song.sections[section_index];
    
    // Helper function to convert musical position to time using REAPER's TimeMap2_beatsToTime
    // This properly accounts for tempo and time signature changes
    // Note: musical_pos.measure already has the project measure offset applied,
    // so we need to subtract it to get REAPER's actual measure index
    let musical_pos_to_time = |project: &Project, musical_pos: &primitives::MusicalPosition| -> Result<PositionInSeconds, String> {
        let medium_reaper = reaper.medium_reaper();
        let project_context = project.context();
        
        // Get the project measure offset
        let measure_offset = get_project_measure_offset(project);
        
        // Convert back to REAPER's measure index (subtract the offset we added earlier)
        let reaper_measure_index = musical_pos.measure - measure_offset;
        
        // Calculate beats since measure start (beat + subdivision/1000.0)
        let beats_since_measure = musical_pos.beat as f64 + (musical_pos.subdivision as f64 / 1000.0);
        let beats_pos = PositionInBeats::new(beats_since_measure)
            .map_err(|e| format!("Invalid beats position: {:?}", e))?;
        
        // Use TimeMap2_beatsToTime with FromMeasureAtIndex to convert musical position to time
        // This properly accounts for tempo and time signature changes
        // Use REAPER's measure index (without offset) since that's what TimeMap2_beatsToTime expects
        let measure_mode = MeasureMode::FromMeasureAtIndex(reaper_measure_index);
        Ok(medium_reaper.time_map_2_beats_to_time(project_context, measure_mode, beats_pos))
    };
    
    // Convert section's musical position (which has measure offset applied) to time
    // For same song, calculate now. For different song, calculate after switching tabs
    let target_pos = if is_same_song {
        musical_pos_to_time(&current_project, &section.start_position.musical)
            .map_err(|e| format!("Failed to convert musical position to time: {}", e))?
    } else {
        // Placeholder - will be recalculated after switching tabs
        PositionInSeconds::ZERO
    };
    
    // If we're playing and clicking a section in the same song, use smooth seek
    if is_playing && is_same_song {
        // Move edit cursor immediately for visual feedback
        // GoToMarker() will handle the play cursor smoothly at the measure boundary
        current_project.set_edit_cursor_position(target_pos, SetEditCurPosOptions { 
            move_view: false, 
            seek_play: false, // Don't seek play cursor yet - that will happen smoothly at measure boundary
        });
        
        // Queue smooth seek for play cursor to execute at next measure start
        let smooth_seek_handler = get_smooth_seek_handler();
        if let Err(e) = smooth_seek_handler.queue_seek(&current_project, target_pos, true) {
            warn!(error = %e, "Failed to queue smooth seek, falling back to immediate seek");
            // Fall through to immediate seek
        } else {
            // Log position and musical position after queuing smooth seek
            let edit_pos = current_project.edit_cursor_position().unwrap_or(PositionInSeconds::ZERO);
            let edit_beat_info = current_project.beat_info_at(edit_pos);
            let measure_offset = get_project_measure_offset(&current_project);
            let musical_pos = format!("{}.{}.{:03}", 
                edit_beat_info.measure_index + measure_offset + 1,
                edit_beat_info.beats_since_measure.get().floor() as i32 + 1,
                ((edit_beat_info.beats_since_measure.get() - edit_beat_info.beats_since_measure.get().floor()) * 1000.0).round() as i32
            );
            info!(
                song_index,
                section_index,
                time_position = edit_pos.get(),
                musical_position = %musical_pos,
                "Queued smooth seek to section"
            );
            return Ok(());
        }
    }
    
    // Otherwise, do immediate seek (not playing, different song, or smooth seek failed)
    // When NOT playing, we don't need markers - just use SetEditCurPos directly
    // Switch to the tab if needed
    let final_project = if !is_same_song {
        let tab_navigator = TabNavigator::new();
        tab_navigator.switch_to_tab(tab_index)
            .map_err(|e| format!("Failed to switch to tab {}: {}", tab_index, e))?;
        
        // Get the current project (now switched to the target song)
        let project_result = medium_reaper.enum_projects(ProjectRef::Current, 0)
            .ok_or_else(|| "No current project after tab switch".to_string())?;
        
        let project = reaper_high::Project::new(project_result.project);
        
        // Convert section's musical position to time using the target project
        // This ensures we use the correct tempo/time sig map for this project
        let target_pos = musical_pos_to_time(&project, &section.start_position.musical)
            .map_err(|e| format!("Failed to convert musical position to time: {}", e))?;
        
        // Just move the cursor directly - no markers needed (whether playing or not)
        project.set_edit_cursor_position(target_pos, SetEditCurPosOptions { 
            move_view: false, 
            seek_play: true, // Seek play cursor immediately
        });
        project
    } else {
        // Same song - immediate seek (either not playing, or smooth seek failed)
        // No markers needed - just use SetEditCurPos directly
        current_project.set_edit_cursor_position(target_pos, SetEditCurPosOptions { 
            move_view: false, 
            seek_play: true, // Seek play cursor immediately
        });
        current_project
    };
    
    // Log position and musical position after immediate seek
    let edit_pos = final_project.edit_cursor_position().unwrap_or(PositionInSeconds::ZERO);
    let edit_beat_info = final_project.beat_info_at(edit_pos);
    let measure_offset = get_project_measure_offset(&final_project);
    let musical_pos = format!("{}.{}.{:03}", 
        edit_beat_info.measure_index + measure_offset + 1,
        edit_beat_info.beats_since_measure.get().floor() as i32 + 1,
        ((edit_beat_info.beats_since_measure.get() - edit_beat_info.beats_since_measure.get().floor()) * 1000.0).round() as i32
    );
    info!(
        song_index,
        section_index,
        time_position = edit_pos.get(),
        musical_position = %musical_pos,
        "Seeked to section"
    );
    Ok(())
}

/// Update setlist state from REAPER's main thread
/// This is called from REAPER's timer callback (target: 60Hz)
pub fn update_setlist_state() {
    if let Some(state) = LATEST_SETLIST_API.get() {
        match build_setlist_from_open_projects(None) {
            Ok(mut setlist) => {
                // Get current project name and transport position to determine active song
                let reaper = reaper_high::Reaper::get();
                let current_project = reaper.current_project();
                let transport_adapter = crate::reaper_transport::ReaperTransport::new(current_project.clone());
                
                // Get project name
                let project_name = {
                    let medium_reaper = reaper.medium_reaper();
                    let current_project_raw = current_project.raw();
                    let mut found_name = None;
                    
                    for i in 0..128u32 {
                        if let Some(result) = medium_reaper.enum_projects(reaper_medium::ProjectRef::Tab(i), 512) {
                            if result.project == current_project_raw {
                                found_name = result.file_path.as_ref()
                                    .and_then(|p| p.as_std_path().file_stem())
                                    .and_then(|s| s.to_str())
                                    .map(|s| s.to_string());
                                break;
                            }
                        }
                    }
                    found_name
                };
                
                // Get transport position
                let transport_position = transport_adapter.read_transport()
                    .ok()
                    .map(|t| t.playhead_position.time.to_seconds())
                    .unwrap_or(0.0);
                
                // Determine active song
                let active_song_idx = project_name.as_ref()
                    .and_then(|name| setlist.get_active_song_index(name, transport_position));
                
                // Determine active section within the active song
                let active_section_idx = active_song_idx
                    .and_then(|song_idx| {
                        setlist.songs.get(song_idx)
                            .and_then(|song| {
                                song.section_at_position_with_index(transport_position)
                                    .map(|(idx, _)| idx)
                            })
                    });
                
                // Populate transport_info for each song from its project
                // We need to look up the project by name and get transport from it
                let reaper = reaper_high::Reaper::get();
                let medium_reaper = reaper.medium_reaper();
                
                for song in &mut setlist.songs {
                    let project_name = song.project_name_from_metadata();
                    
                    // Find the project for this song by matching project name
                    let mut found_project = None;
                    for i in 0..128u32 {
                        if let Some(result) = medium_reaper.enum_projects(reaper_medium::ProjectRef::Tab(i), 512) {
                            let tab_project_name = result.file_path.as_ref()
                                .and_then(|p| p.as_std_path().file_stem())
                                .and_then(|s| s.to_str())
                                .map(|s| s.to_string())
                                .unwrap_or_else(|| format!("Tab {}", i));
                            
                            if tab_project_name == project_name {
                                found_project = Some(reaper_high::Project::new(result.project));
                                break;
                            }
                        }
                    }
                    
                    // Get transport info from the project
                    if let Some(project) = found_project {
                        let transport_adapter = crate::reaper_transport::ReaperTransport::new(project);
                        if let Ok(transport) = transport_adapter.read_transport() {
                            song.transport_info = Some(transport);
                        }
                    }
                }
                
                // Build SetlistApi with computed fields (now includes transport_info in each song)
                let setlist_api = SetlistApi::new(setlist.clone(), active_song_idx, active_section_idx);
                
                // Update the state (we'll use a blocking lock since we're on main thread)
                if let Ok(mut guard) = state.try_lock() {
                    *guard = Some(setlist_api.clone());
                    SETLIST_UPDATED.store(true, Ordering::Release);
                    
                    // Logging removed - too verbose at 30Hz
                } else {
                    // Lock is held by async task - this is fine, just skip this update
                }
            }
            Err(e) => {
                tracing::warn!("[REAPER Setlist] Failed to build setlist: {}", e);
            }
        }
    } else {
        warn!("[REAPER Setlist] update_setlist_state called but LATEST_SETLIST_API not initialized!");
    }
}

/// REAPER implementation of SetlistStateProvider
struct ReaperSetlistStateProvider;

#[async_trait::async_trait]
impl SetlistStateProvider for ReaperSetlistStateProvider {
    async fn get_setlist_api(&self) -> Result<SetlistApi, String> {
        let Some(state) = LATEST_SETLIST_API.get() else {
            return Err("Setlist API state not initialized".to_string());
        };
        
        // Reset the updated flag before waiting
        SETLIST_UPDATED.store(false, Ordering::Release);

        // Wait for setlist to be updated (with timeout)
        let mut attempts = 0;
        while !SETLIST_UPDATED.load(Ordering::Acquire) && attempts < 100 {
            tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
            attempts += 1;
        }
        
        let guard = state.lock().unwrap();
        guard.clone().ok_or_else(|| "No setlist API state available".to_string())
    }
    
    async fn get_setlist_api_with_transport(&self) -> Result<SetlistApi, String> {
        // Transport info is now embedded in each song's transport_info field
        self.get_setlist_api().await
    }
}

/// REAPER implementation of SetlistCommandHandler
struct ReaperSetlistCommandHandler;

#[async_trait::async_trait]
impl SetlistCommandHandler for ReaperSetlistCommandHandler {
    async fn execute_transport_command(&self, command: TransportCommand) -> Result<(), String> {
        // Send command execution request to main thread via channel
        let (result_tx, result_rx) = std::sync::mpsc::channel();
        
        if let Some(channel_arc) = COMMAND_REQUEST_CHANNEL.get() {
            if let Ok(channel_guard) = channel_arc.lock() {
                if let Some(sender) = channel_guard.as_ref() {
                    if let Err(e) = sender.send((CommandRequest::Transport(command), result_tx)) {
                        return Err(format!("Failed to send transport command to main thread: {}", e));
                    }
                    // Wait for the result from main thread
                    match result_rx.recv() {
                        Ok(result) => result,
                        Err(e) => Err(format!("Failed to receive command result: {}", e)),
                    }
                } else {
                    Err("Command request channel not initialized".to_string())
                }
            } else {
                Err("Failed to lock command request channel".to_string())
            }
        } else {
            Err("Command request channel not initialized".to_string())
        }
    }
    
    async fn execute_navigation_command(&self, command: NavigationCommand) -> Result<(), String> {
        // Send command execution request to main thread via channel
        let (result_tx, result_rx) = std::sync::mpsc::channel();
        
        if let Some(channel_arc) = COMMAND_REQUEST_CHANNEL.get() {
            if let Ok(channel_guard) = channel_arc.lock() {
                if let Some(sender) = channel_guard.as_ref() {
                    if let Err(e) = sender.send((CommandRequest::Navigation(command), result_tx)) {
                        return Err(format!("Failed to send navigation command to main thread: {}", e));
                    }
                    // Wait for the result from main thread
                    match result_rx.recv() {
                        Ok(result) => result,
                        Err(e) => Err(format!("Failed to receive command result: {}", e)),
                    }
                } else {
                    Err("Command request channel not initialized".to_string())
                }
            } else {
                Err("Failed to lock command request channel".to_string())
            }
        } else {
            Err("Command request channel not initialized".to_string())
        }
    }
    
    async fn seek_to_section(&self, song_index: usize, section_index: usize) -> Result<(), String> {
        // Send seek request to main thread via channel
        let (result_tx, result_rx) = std::sync::mpsc::channel();
        
        if let Some(channel_arc) = SEEK_REQUEST_CHANNEL.get() {
            if let Ok(channel_guard) = channel_arc.lock() {
                if let Some(sender) = channel_guard.as_ref() {
                    if let Err(e) = sender.send((song_index, section_index, result_tx)) {
                        return Err(format!("Failed to send seek request to main thread: {}", e));
                    }
                    // Wait for the result from main thread (blocking, but this is async so it's ok)
                    match result_rx.recv() {
                        Ok(result) => result,
                        Err(e) => Err(format!("Failed to receive seek result: {}", e)),
                    }
                } else {
                    Err("Seek request channel not initialized".to_string())
                }
            } else {
                Err("Failed to lock seek request channel".to_string())
            }
        } else {
            Err("Seek request channel not initialized".to_string())
        }
    }
    
    async fn seek_to_song(&self, song_index: usize) -> Result<(), String> {
        // Send command execution request to main thread via channel
        let (result_tx, result_rx) = std::sync::mpsc::channel();
        
        if let Some(channel_arc) = COMMAND_REQUEST_CHANNEL.get() {
            if let Ok(channel_guard) = channel_arc.lock() {
                if let Some(sender) = channel_guard.as_ref() {
                    if let Err(e) = sender.send((CommandRequest::SeekToSong(song_index), result_tx)) {
                        return Err(format!("Failed to send seek to song command to main thread: {}", e));
                    }
                    // Wait for the result from main thread
                    match result_rx.recv() {
                        Ok(result) => result,
                        Err(e) => Err(format!("Failed to receive command result: {}", e)),
                    }
                } else {
                    Err("Command request channel not initialized".to_string())
                }
            } else {
                Err("Failed to lock command request channel".to_string())
            }
        } else {
            Err("Command request channel not initialized".to_string())
        }
    }
    
    async fn toggle_loop(&self) -> Result<(), String> {
        // Send command execution request to main thread via channel
        let (result_tx, result_rx) = std::sync::mpsc::channel();
        
        if let Some(channel_arc) = COMMAND_REQUEST_CHANNEL.get() {
            if let Ok(channel_guard) = channel_arc.lock() {
                if let Some(sender) = channel_guard.as_ref() {
                    if let Err(e) = sender.send((CommandRequest::ToggleLoop, result_tx)) {
                        return Err(format!("Failed to send toggle loop command to main thread: {}", e));
                    }
                    // Wait for the result from main thread
                    match result_rx.recv() {
                        Ok(result) => result,
                        Err(e) => Err(format!("Failed to receive command result: {}", e)),
                    }
                } else {
                    Err("Command request channel not initialized".to_string())
                }
            } else {
                Err("Failed to lock command request channel".to_string())
            }
        } else {
            Err("Command request channel not initialized".to_string())
        }
    }
}

/// Create and expose the REAPER setlist stream service
/// 
/// This creates a setlist stream service with REAPER's specific
/// SetlistStateProvider and SetlistCommandHandler implementations.
/// Only the server (REAPER extension) needs to know about this implementation
/// - clients just connect via SetlistStreamApi::connect().
pub fn create_reaper_setlist_stream_service() -> Result<SetlistStreamApi> {
    let state_provider = Arc::new(ReaperSetlistStateProvider);
    let command_handler = Arc::new(ReaperSetlistCommandHandler);
    let api = SetlistStreamApi::spawn_with_handler(state_provider, Some(command_handler));
    info!("[REAPER Setlist] Setlist stream service created with command handler.");
    Ok(api)
}

