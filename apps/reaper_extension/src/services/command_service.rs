//! Command Service
//!
//! Handles command execution requests from async tasks.
//! Commands are queued and executed on the main thread.

use std::sync::mpsc;
use std::sync::{Arc, Mutex};
use setlist::{TransportCommand, NavigationCommand};
use crate::infrastructure::action_registry::get_command_id;
use reaper_medium::{CommandId, ProjectContext};
use tracing::info;

/// Command execution request
enum CommandRequest {
    Transport(TransportCommand),
    Navigation(NavigationCommand),
    SeekToSong(usize),
    SeekToTime(usize, f64), // (song_index, time_seconds)
    ToggleLoop,
}

/// Service for executing commands on the main thread
#[derive(Debug)]
pub struct CommandService {
    /// Channel sender for command requests
    command_tx: Arc<Mutex<Option<mpsc::Sender<(CommandRequest, mpsc::Sender<Result<(), String>>)>>>>,
    /// Channel receiver for command requests (used by timer callback)
    command_rx: Arc<Mutex<Option<mpsc::Receiver<(CommandRequest, mpsc::Sender<Result<(), String>>)>>>>,
}

impl CommandService {
    /// Create a new command service
    pub fn new() -> Self {
        let (tx, rx) = mpsc::channel();
        Self {
            command_tx: Arc::new(Mutex::new(Some(tx))),
            command_rx: Arc::new(Mutex::new(Some(rx))),
        }
    }

    /// Execute a transport command (called from async context)
    pub fn execute_transport_command(&self, command: TransportCommand) -> Result<(), String> {
        self.send_command(CommandRequest::Transport(command))
    }

    /// Execute a navigation command (called from async context)
    pub fn execute_navigation_command(&self, command: NavigationCommand) -> Result<(), String> {
        self.send_command(CommandRequest::Navigation(command))
    }

    /// Seek to a song (called from async context)
    pub fn seek_to_song(&self, song_index: usize) -> Result<(), String> {
        self.send_command(CommandRequest::SeekToSong(song_index))
    }

    /// Seek to a time position (called from async context)
    pub fn seek_to_time(&self, song_index: usize, time_seconds: f64) -> Result<(), String> {
        self.send_command(CommandRequest::SeekToTime(song_index, time_seconds))
    }

    /// Toggle loop (called from async context)
    pub fn toggle_loop(&self) -> Result<(), String> {
        self.send_command(CommandRequest::ToggleLoop)
    }

    /// Send a command request to the main thread
    fn send_command(&self, request: CommandRequest) -> Result<(), String> {
        let (result_tx, result_rx) = mpsc::channel();
        
        if let Ok(guard) = self.command_tx.lock() {
            if let Some(sender) = guard.as_ref() {
                sender.send((request, result_tx))
                    .map_err(|e| format!("Failed to send command to main thread: {}", e))?;
                
                // Wait for the result from main thread
                result_rx.recv()
                    .map_err(|e| format!("Failed to receive command result: {}", e))?
            } else {
                Err("Command request channel not initialized".to_string())
            }
        } else {
            Err("Failed to lock command request channel".to_string())
        }
    }

    /// Process pending command requests (called from main thread)
    pub fn process_pending_commands(&self) {
        if let Ok(guard) = self.command_rx.lock() {
            if let Some(receiver) = guard.as_ref() {
                // Try to receive all pending requests (non-blocking)
                while let Ok((request, tx)) = receiver.try_recv() {
                    let result = self.execute_command_main_thread(request);
                    let _ = tx.send(result);
                }
            }
        }
    }

    /// Execute command on main thread
    fn execute_command_main_thread(&self, request: CommandRequest) -> Result<(), String> {
        let reaper = reaper_high::Reaper::get();
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
                use crate::live::tracks::actions::go_to_song;
                go_to_song(song_index);
                info!("Executed seek to song: {}", song_index);
                Ok(())
            }
            CommandRequest::SeekToTime(song_index, time_seconds) => {
                // Delegate to SeekService for time-based seeks
                // For now, we'll implement it here since SeekService handles section seeks
                // TODO: Move this to SeekService
                use crate::implementation::setlist::build_setlist_from_open_projects;
                use crate::live::tracks::tab_navigation::TabNavigator;
                use reaper_high::{Reaper, Project};
                use reaper_medium::{ProjectRef, PositionInSeconds, SetEditCurPosOptions};
                
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
                        
                        if tab_name == project_name {
                            found_tab_index = Some(i as usize);
                            break;
                        }
                    }
                }
                
                let tab_index = found_tab_index
                    .ok_or_else(|| format!("Could not find project for song: {}", project_name))?;
                
                // Get current project to check if we're in the same song
                let current_project_result = medium_reaper.enum_projects(ProjectRef::Current, 0)
                    .ok_or_else(|| "No current project".to_string())?;
                
                let current_project = Project::new(current_project_result.project);
                
                // Check if we're clicking in the same song (same tab)
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
                
                // Calculate absolute time position (song start + relative time)
                let song_start = song.effective_start();
                let absolute_time = song_start + time_seconds;
                
                let target_pos = PositionInSeconds::new(absolute_time)
                    .map_err(|e| format!("Invalid position: {}", e))?;
                
                // Switch to the tab if needed
                let final_project = if !is_same_song {
                    let tab_navigator = TabNavigator::new();
                    tab_navigator.switch_to_tab(tab_index)
                        .map_err(|e| format!("Failed to switch to tab {}: {}", tab_index, e))?;
                    
                    // Get the current project (now switched to the target song)
                    let project_result = medium_reaper.enum_projects(ProjectRef::Current, 0)
                        .ok_or_else(|| "No current project after tab switch".to_string())?;
                    
                    Project::new(project_result.project)
                } else {
                    current_project
                };
                
                // Seek to the position
                final_project.set_edit_cursor_position(target_pos, SetEditCurPosOptions { 
                    move_view: false, 
                    seek_play: true, // Seek play cursor immediately
                });
                
                info!(
                    "Seeked to time position {}s (song: {}, absolute: {}s)",
                    time_seconds, song.name, absolute_time
                );
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
}

