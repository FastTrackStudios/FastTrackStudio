//! Seek Service
//!
//! Handles seek operations (to sections, songs, time positions).
//! Seeks are queued and executed on the main thread.

use std::sync::mpsc;
use std::sync::{Arc, Mutex};
use crate::implementation::setlist::build_setlist_from_open_projects;
use crate::live::tracks::tab_navigation::TabNavigator;
use crate::live::tracks::smooth_seek::get_smooth_seek_handler;
use reaper_high::{Reaper, Project};
use reaper_medium::{ProjectRef, PositionInSeconds, PositionInBeats, SetEditCurPosOptions, MeasureMode};
use daw::primitives::MusicalPosition;
use tracing::{info, warn};

/// Seek request
struct SeekRequest {
    song_index: usize,
    section_index: usize,
    response_tx: mpsc::Sender<Result<(), String>>,
}

/// Service for handling seek operations
#[derive(Debug)]
pub struct SeekService {
    /// Channel sender for seek requests
    seek_tx: Arc<Mutex<Option<mpsc::Sender<SeekRequest>>>>,
    /// Channel receiver for seek requests (used by timer callback)
    seek_rx: Arc<Mutex<Option<mpsc::Receiver<SeekRequest>>>>,
}

impl SeekService {
    /// Create a new seek service
    pub fn new() -> Self {
        let (tx, rx) = mpsc::channel();
        Self {
            seek_tx: Arc::new(Mutex::new(Some(tx))),
            seek_rx: Arc::new(Mutex::new(Some(rx))),
        }
    }

    /// Seek to a section (called from async context)
    pub fn seek_to_section(&self, song_index: usize, section_index: usize) -> Result<(), String> {
        let (result_tx, result_rx) = mpsc::channel();
        
        if let Ok(guard) = self.seek_tx.lock() {
            if let Some(sender) = guard.as_ref() {
                sender.send(SeekRequest {
                    song_index,
                    section_index,
                    response_tx: result_tx,
                })
                .map_err(|e| format!("Failed to send seek request to main thread: {}", e))?;
                
                // Wait for the result from main thread
                result_rx.recv()
                    .map_err(|e| format!("Failed to receive seek result: {}", e))?
            } else {
                Err("Seek request channel not initialized".to_string())
            }
        } else {
            Err("Failed to lock seek request channel".to_string())
        }
    }

    /// Process pending seek requests (called from main thread)
    pub fn process_pending_seeks(&self) {
        if let Ok(guard) = self.seek_rx.lock() {
            if let Some(receiver) = guard.as_ref() {
                // Try to receive all pending requests (non-blocking)
                while let Ok(request) = receiver.try_recv() {
                    let result = self.seek_to_section_main_thread(request.song_index, request.section_index);
                    let _ = request.response_tx.send(result);
                }
            }
        }
    }

    /// Seek to section (executes on main thread)
    fn seek_to_section_main_thread(&self, song_index: usize, section_index: usize) -> Result<(), String> {
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
        
        let current_project = Project::new(current_project_result.project);
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
        let get_project_measure_offset = |project: &Project| -> i32 {
            let reaper = Reaper::get();
            let medium_reaper = reaper.medium_reaper();
            
            if let Some(offs_result) = medium_reaper.project_config_var_get_offs("projmeasoffs") {
                if let Some(addr) = medium_reaper.project_config_var_addr(project.context(), offs_result.offset) {
                    unsafe { *(addr.as_ptr() as *const i32) }
                } else {
                    0
                }
            } else {
                0
            }
        };
        
        let musical_pos_to_time = |project: &Project, musical_pos: &MusicalPosition| -> Result<PositionInSeconds, String> {
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
            let measure_mode = MeasureMode::FromMeasureAtIndex(reaper_measure_index);
            Ok(medium_reaper.time_map_2_beats_to_time(project_context, measure_mode, beats_pos))
        };
        
        // Convert section's musical position (which has measure offset applied) to time
        let target_pos = if is_same_song {
            section.start_position.as_ref()
                .ok_or_else(|| "Section has no start position".to_string())
                .and_then(|pos| musical_pos_to_time(&current_project, &pos.musical)
                    .map_err(|e| format!("Failed to convert musical position to time: {}", e)))
                .map_err(|e| format!("Failed to get section start position: {}", e))?
        } else {
            // Placeholder - will be recalculated after switching tabs
            PositionInSeconds::ZERO
        };
        
        // If we're playing and clicking a section in the same song, use smooth seek
        if is_playing && is_same_song {
            // Move edit cursor immediately for visual feedback
            current_project.set_edit_cursor_position(target_pos, SetEditCurPosOptions { 
                move_view: false, 
                seek_play: false, // Don't seek play cursor yet - that will happen smoothly at measure boundary
            });
            
            // Queue smooth seek for play cursor to execute at next measure start
            let smooth_seek_handler = get_smooth_seek_handler();
            if let Err(e) = smooth_seek_handler.queue_seek(&current_project, target_pos, true) {
                warn!(error = %e, "Failed to queue smooth seek, falling back to immediate seek");
            } else {
                info!(
                    song_index,
                    section_index,
                    time_position = target_pos.get(),
                    "Queued smooth seek to section"
                );
                return Ok(());
            }
        }
        
        // Otherwise, do immediate seek (not playing, different song, or smooth seek failed)
        let final_project = if !is_same_song {
            let tab_navigator = TabNavigator::new();
            tab_navigator.switch_to_tab(tab_index)
                .map_err(|e| format!("Failed to switch to tab {}: {}", tab_index, e))?;
            
            // Get the current project (now switched to the target song)
            let project_result = medium_reaper.enum_projects(ProjectRef::Current, 0)
                .ok_or_else(|| "No current project after tab switch".to_string())?;
            
            let project = Project::new(project_result.project);
            
            // Convert section's musical position to time using the target project
            let target_pos = section.start_position.as_ref()
                .ok_or_else(|| "Section has no start position".to_string())
                .and_then(|pos| musical_pos_to_time(&project, &pos.musical)
                    .map_err(|e| format!("Failed to convert musical position to time: {}", e)))
                .map_err(|e| format!("Failed to get section start position: {}", e))?;
            
            // Just move the cursor directly - no markers needed
            project.set_edit_cursor_position(target_pos, SetEditCurPosOptions { 
                move_view: false, 
                seek_play: true, // Seek play cursor immediately
            });
            project
        } else {
            // Same song - immediate seek (either not playing, or smooth seek failed)
            current_project.set_edit_cursor_position(target_pos, SetEditCurPosOptions { 
                move_view: false, 
                seek_play: true, // Seek play cursor immediately
            });
            current_project
        };
        
        info!(
            song_index,
            section_index,
            time_position = final_project.edit_cursor_position().unwrap_or(PositionInSeconds::ZERO).get(),
            "Seeked to section"
        );
        Ok(())
    }
}

