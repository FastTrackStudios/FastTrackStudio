//! Direct trait implementations for ReaperProject
//!
//! Since we own the traits (SetlistBuilder, SeekAdapter, CommandAdapter),
//! we can implement them directly for ReaperProject from reaper-rs!
//!
//! This allows you to call methods directly on ReaperProject instances:
//! ```rust
//! let project = Reaper::get().current_project();
//! let setlist = project.build_setlist_from_open_projects(None)?;
//! project.seek_to_song(0)?;
//! ```
//!
//! Note: Some implementations delegate to existing free functions or require
//! additional context from the reaper_extension crate. These are marked with TODO.

use reaper_high::{Project as ReaperProject, Reaper, Project as ReaperProjectHigh};
use reaper_medium::{ProjectRef, PositionInSeconds, PositionInBeats, SetEditCurPosOptions, MeasureMode, ProjectContext, CommandId};
use crate::setlist::core::{Setlist, SetlistError, Song};
use crate::setlist::infra::traits::{SetlistBuilder, SeekAdapter, CommandAdapter};
use crate::setlist::{TransportCommand, NavigationCommand};
use crate::lyrics::infra::traits::{LyricsReader, LyricsWriter};
use crate::lyrics::core::Lyrics;
use crate::lyrics::infra::traits::{LyricsData, LyricsReadError, LyricsWriteError, SlideData, MidiTrackData, MidiItemData, MidiEventData};
use daw::primitives::MusicalPosition;
use tracing::{warn, debug, info, error};

use super::song_builder::{build_song_from_current_project, build_setlist_from_open_projects as build_setlist_impl};
use crate::lyrics::infra::reaper::{read_lyrics_from_project, update_lyrics_in_reaper};

/// Helper function to find the tab index for a project by name
/// This is a public utility function that can be used by other modules
pub fn find_tab_index_by_project_name(project_name: &str) -> Result<usize, String> {
    if project_name.is_empty() {
        return Err("Project name cannot be empty".to_string());
    }
    
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    
    let normalized_target = project_name.to_uppercase().replace('_', "-");
    
    // Search through all project tabs
    for i in 0..128u32 {
        if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 512) {
            let tab_name = result.file_path.as_ref()
                .and_then(|p| p.as_std_path().file_stem())
                .and_then(|s| s.to_str())
                .map(|s| s.to_string())
                .unwrap_or_else(|| format!("Tab {}", i));
            
            let normalized_tab = tab_name.to_uppercase().replace('_', "-");
            
            if normalized_tab == normalized_target {
                debug!(tab_index = i, project_name = %project_name, "Found project tab");
                return Ok(i as usize);
            }
        } else {
            // No more tabs
            break;
        }
    }
    
    error!(project_name = %project_name, "Could not find project tab");
    Err(format!("Could not find project tab for: {}", project_name))
}

/// Helper function to get current tab index
fn get_current_tab_index() -> Option<usize> {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    
    let current_project_result = medium_reaper.enum_projects(ProjectRef::Current, 0)?;
    let current_project_raw = current_project_result.project;
    
    for i in 0..128u32 {
        if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 512) {
            if result.project == current_project_raw {
                return Some(i as usize);
            }
        }
    }
    
    None
}

/// Helper function to switch to a specific tab by index
/// This is a public utility function that can be used by other modules
pub fn switch_to_tab(tab_index: usize) -> Result<ReaperProjectHigh, String> {
    // Validate input
    if tab_index >= 128 {
        return Err(format!("Tab index {} out of range (max 127)", tab_index));
    }
    
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    
    // Verify the target tab exists
    let tab_index_u32 = tab_index as u32;
    if medium_reaper.enum_projects(ProjectRef::Tab(tab_index_u32), 0).is_none() {
        return Err(format!("Tab {} does not exist", tab_index));
    }
    
    // Get current tab index
    let current_idx = get_current_tab_index()
        .ok_or_else(|| "No current tab".to_string())?;
    
    // If already on the target tab, return current project
    if current_idx == tab_index {
        debug!(tab_index, "Already on target tab");
        let current_project_result = medium_reaper.enum_projects(ProjectRef::Current, 0)
            .ok_or_else(|| "No current project".to_string())?;
        return Ok(ReaperProjectHigh::new(current_project_result.project));
    }
    
    // Count total tabs
    let mut total_tab_count = 0;
    for i in 0..128u32 {
        if medium_reaper.enum_projects(ProjectRef::Tab(i), 0).is_some() {
            total_tab_count += 1;
        } else {
            break;
        }
    }
    
    if total_tab_count <= 1 {
        return Err("Only one tab available - cannot switch".to_string());
    }
    
    // Validate tab_index is within range
    if tab_index >= total_tab_count {
        return Err(format!(
            "Tab index {} out of range (only {} tabs available)",
            tab_index,
            total_tab_count
        ));
    }
    
    // Calculate distance (forward and backward)
    let forward_dist = if tab_index > current_idx {
        tab_index - current_idx
    } else {
        (total_tab_count - current_idx) + tab_index
    };
    
    let backward_dist = if tab_index < current_idx {
        current_idx - tab_index
    } else {
        current_idx + (total_tab_count - tab_index)
    };
    
    // Use the shorter path
    let (action_id, distance) = if forward_dist <= backward_dist {
        (40861u32, forward_dist) // Action 40861: Next project tab
    } else {
        (40862u32, backward_dist) // Action 40862: Previous project tab
    };
    
    debug!(
        from_tab = current_idx,
        to_tab = tab_index,
        distance,
        direction = if forward_dist <= backward_dist { "forward" } else { "backward" },
        "Switching project tab"
    );
    
    // Execute the action the required number of times
    for step in 0..distance {
        let cmd_id = CommandId::new(action_id);
        medium_reaper.main_on_command_ex(cmd_id, 0, ProjectContext::CurrentProject);
        debug!(step = step + 1, total = distance, "Tab switch step");
    }
    
    // Get the current project (now switched to the target tab)
    let project_result = medium_reaper.enum_projects(ProjectRef::Current, 0)
        .ok_or_else(|| {
            error!(tab_index, "No current project after tab switch");
            "No current project after tab switch".to_string()
        })?;
    
    // Verify we're on the correct tab
    let final_tab = get_current_tab_index()
        .ok_or_else(|| "Could not determine final tab index".to_string())?;
    
    if final_tab != tab_index {
        warn!(
            expected_tab = tab_index,
            actual_tab = final_tab,
            "Tab switch may have failed - tab mismatch"
        );
    } else {
        info!(
            from_tab = current_idx,
            to_tab = tab_index,
            "Successfully switched to project tab"
        );
    }
    
    Ok(ReaperProjectHigh::new(project_result.project))
}

/// Helper function to convert musical position to time using REAPER's tempo map
fn musical_pos_to_time(project: &ReaperProjectHigh, musical_pos: &MusicalPosition) -> Result<PositionInSeconds, String> {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    let project_context = project.context();
    
    // Get project measure offset
    let measure_offset = if let Some(offs_result) = medium_reaper.project_config_var_get_offs("projmeasoffs") {
        if let Some(addr) = medium_reaper.project_config_var_addr(project_context, offs_result.offset) {
            unsafe { *(addr.as_ptr() as *const i32) }
        } else {
            0
        }
    } else {
        0
    };
    
    // Convert back to REAPER's measure index (subtract the offset)
    let reaper_measure_index = musical_pos.measure - measure_offset;
    
    // Calculate beats since measure start (beat + subdivision/1000.0)
    let beats_since_measure = musical_pos.beat as f64 + (musical_pos.subdivision as f64 / 1000.0);
    let beats_pos = PositionInBeats::new(beats_since_measure)
        .map_err(|e| format!("Invalid beats position: {:?}", e))?;
    
    // Use TimeMap2_beatsToTime with FromMeasureAtIndex to convert musical position to time
    let measure_mode = MeasureMode::FromMeasureAtIndex(reaper_measure_index);
    Ok(medium_reaper.time_map_2_beats_to_time(project_context, measure_mode, beats_pos))
}

// Implement SetlistBuilder for Reaper (operates on all open projects and current project)
impl SetlistBuilder for Reaper {
    fn build_setlist_from_open_projects(
        &self,
        existing_setlist: Option<&Setlist>,
    ) -> Result<Setlist, SetlistError> {
        // Delegate to the existing free function
        // This builds from ALL open projects, which is what Reaper-level operations should do
        info!("Building setlist from all open projects");
        let result = build_setlist_impl(existing_setlist);
        
        match &result {
            Ok(setlist) => {
                info!(
                    song_count = setlist.songs.len(),
                    setlist_name = %setlist.name,
                    "Successfully built setlist"
                );
            }
            Err(e) => {
                error!(error = %e, "Failed to build setlist");
            }
        }
        
        result
    }
    
    fn build_song_from_current_project(&self) -> Result<Song, SetlistError> {
        // Delegate to existing free function
        // Since we're on Reaper, we can build from the current project
        info!("Building song from current project");
        let result = build_song_from_current_project();
        
        match &result {
            Ok(song) => {
                info!(
                    song_name = %song.name,
                    section_count = song.sections.len(),
                    "Successfully built song from current project"
                );
            }
            Err(e) => {
                error!(error = %e, "Failed to build song from current project");
            }
        }
        
        result
    }
}

impl SeekAdapter for ReaperProject {
    fn seek_to_section(&self, song_index: usize, section_index: usize) -> Result<(), String> {
        // Validate inputs
        if song_index == usize::MAX {
            return Err("Invalid song index".to_string());
        }
        if section_index == usize::MAX {
            return Err("Invalid section index".to_string());
        }
        
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();
        
        // Build setlist to find the song
        let setlist = build_setlist_impl(None)
            .map_err(|e| {
                error!(error = %e, "Failed to build setlist for seek operation");
                format!("Failed to build setlist: {}", e)
            })?;
        
        if setlist.songs.is_empty() {
            return Err("Setlist is empty - no songs available".to_string());
        }
        
        if song_index >= setlist.songs.len() {
            return Err(format!(
                "Song index {} out of range (setlist has {} songs)",
                song_index,
                setlist.songs.len()
            ));
        }
        
        let song = &setlist.songs[song_index];
        let project_name = song.project_name_from_metadata();
        
        // Find the project tab for this song
        let tab_index = find_tab_index_by_project_name(&project_name)?;
        
        // Validate section index
        if song.sections.is_empty() {
            return Err(format!("Song '{}' has no sections", song.name));
        }
        
        if section_index >= song.sections.len() {
            return Err(format!(
                "Section index {} out of range (song '{}' has {} sections)",
                section_index,
                song.name,
                song.sections.len()
            ));
        }
        
        let section = &song.sections[section_index];
        
        // Get section start position
        let target_pos = if let Some(start_pos) = &section.start_position {
            // We need to convert the musical position to time
            // First, switch to the correct tab to get the project
            let target_project = if get_current_tab_index() == Some(tab_index) {
                // Already on the correct tab, use current project
                let current_project_result = medium_reaper.enum_projects(ProjectRef::Current, 0)
                    .ok_or_else(|| "No current project".to_string())?;
                ReaperProjectHigh::new(current_project_result.project)
            } else {
                // Switch to the target tab
                switch_to_tab(tab_index)?
            };
            
            // Convert musical position to time
            musical_pos_to_time(&target_project, &start_pos.musical)
                .map_err(|e| format!("Failed to convert musical position to time: {}", e))?
        } else {
            // Fallback: use time position if available
            let time_sec = section.start_position.as_ref()
                .map(|p| p.time.to_seconds())
                .ok_or_else(|| "Section has no start position".to_string())?;
            
            // Get song start to calculate absolute position
            let song_start = song.effective_start();
            let absolute_time = song_start + time_sec;
            
            PositionInSeconds::new(absolute_time)
                .map_err(|e| format!("Invalid position: {}", e))?
        };
        
        // Switch to the tab if needed
        let final_project = if get_current_tab_index() != Some(tab_index) {
            switch_to_tab(tab_index)?
        } else {
            let current_project_result = medium_reaper.enum_projects(ProjectRef::Current, 0)
                .ok_or_else(|| "No current project".to_string())?;
            ReaperProjectHigh::new(current_project_result.project)
        };
        
        // Seek to the position
        final_project.set_edit_cursor_position(target_pos, SetEditCurPosOptions { 
            move_view: false, 
            seek_play: true,
        });
        
        info!(
            song_index,
            section_index,
            song_name = %song.name,
            section_name = ?section.name.as_ref(),
            time_position = target_pos.get(),
            "Successfully seeked to section"
        );
        
        Ok(())
    }
    
    fn seek_to_song(&self, song_index: usize) -> Result<(), String> {
        // Validate input
        if song_index == usize::MAX {
            return Err("Invalid song index".to_string());
        }
        
        // Seek to the start of the song (first section or song start marker)
        // If the song has no sections, we'll get a clear error from seek_to_section
        self.seek_to_section(song_index, 0)
            .map_err(|e| format!("Failed to seek to song {}: {}", song_index, e))
    }
    
    fn seek_to_time(&self, song_index: usize, time_seconds: f64) -> Result<(), String> {
        // Validate inputs
        if song_index == usize::MAX {
            return Err("Invalid song index".to_string());
        }
        if !time_seconds.is_finite() {
            return Err(format!("Invalid time value: {} (must be finite)", time_seconds));
        }
        if time_seconds < 0.0 {
            return Err(format!("Time cannot be negative: {}", time_seconds));
        }
        
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();
        
        // Build setlist to find the song
        let setlist = build_setlist_impl(None)
            .map_err(|e| {
                error!(error = %e, "Failed to build setlist for time seek");
                format!("Failed to build setlist: {}", e)
            })?;
        
        if setlist.songs.is_empty() {
            return Err("Setlist is empty - no songs available".to_string());
        }
        
        if song_index >= setlist.songs.len() {
            return Err(format!(
                "Song index {} out of range (setlist has {} songs)",
                song_index,
                setlist.songs.len()
            ));
        }
        
        let song = &setlist.songs[song_index];
        let project_name = song.project_name_from_metadata();
        
        // Find the project tab for this song
        let tab_index = find_tab_index_by_project_name(&project_name)?;
        
        // Calculate absolute time position (song start + relative time)
        let song_start = song.effective_start();
        let absolute_time = song_start + time_seconds;
        
        // Validate the calculated position
        if !absolute_time.is_finite() {
            return Err(format!(
                "Calculated absolute time is not finite: song_start={}, time_seconds={}",
                song_start,
                time_seconds
            ));
        }
        
        let target_pos = PositionInSeconds::new(absolute_time)
            .map_err(|e| format!("Invalid position {}: {:?}", absolute_time, e))?;
        
        // Switch to the tab if needed
        let final_project = if get_current_tab_index() != Some(tab_index) {
            switch_to_tab(tab_index)?
        } else {
            let current_project_result = medium_reaper.enum_projects(ProjectRef::Current, 0)
                .ok_or_else(|| "No current project".to_string())?;
            ReaperProjectHigh::new(current_project_result.project)
        };
        
        // Seek to the position
        final_project.set_edit_cursor_position(target_pos, SetEditCurPosOptions { 
            move_view: false, 
            seek_play: true,
        });
        
        info!(
            song_index,
            song_name = %song.name,
            time_seconds,
            absolute_time,
            "Successfully seeked to time position"
        );
        
        Ok(())
    }
    
    fn seek_to_musical_position(
        &self,
        song_index: usize,
        musical_position: MusicalPosition,
    ) -> Result<(), String> {
        // Validate inputs
        if song_index == usize::MAX {
            return Err("Invalid song index".to_string());
        }
        
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();
        
        // Build setlist to find the song
        let setlist = build_setlist_impl(None)
            .map_err(|e| {
                error!(error = %e, "Failed to build setlist for musical position seek");
                format!("Failed to build setlist: {}", e)
            })?;
        
        if setlist.songs.is_empty() {
            return Err("Setlist is empty - no songs available".to_string());
        }
        
        if song_index >= setlist.songs.len() {
            return Err(format!(
                "Song index {} out of range (setlist has {} songs)",
                song_index,
                setlist.songs.len()
            ));
        }
        
        let song = &setlist.songs[song_index];
        let project_name = song.project_name_from_metadata();
        
        // Find the project tab for this song
        let tab_index = find_tab_index_by_project_name(&project_name)?;
        
        // Switch to the tab if needed
        let final_project = if get_current_tab_index() != Some(tab_index) {
            switch_to_tab(tab_index)?
        } else {
            let current_project_result = medium_reaper.enum_projects(ProjectRef::Current, 0)
                .ok_or_else(|| "No current project".to_string())?;
            ReaperProjectHigh::new(current_project_result.project)
        };
        
        // Convert musical position to time using REAPER's tempo map
        let target_pos = musical_pos_to_time(&final_project, &musical_position)
            .map_err(|e| format!("Failed to convert musical position to time: {}", e))?;
        
        // Seek to the position
        final_project.set_edit_cursor_position(target_pos, SetEditCurPosOptions { 
            move_view: false, 
            seek_play: true,
        });
        
        info!(
            song_index,
            song_name = %song.name,
            musical_position = ?musical_position,
            time_position = target_pos.get(),
            "Successfully seeked to musical position"
        );
        
        Ok(())
    }
}

impl CommandAdapter for ReaperProject {
    fn execute_transport_command(&self, command: TransportCommand) -> Result<(), String> {
        let medium_reaper = Reaper::get().medium_reaper();
        
        // Map transport commands to REAPER action names
        // Note: These are standard REAPER actions, but you may need to use
        // custom action IDs from reaper_extension if you have registered actions
        let action_name = match command {
            TransportCommand::Play => "Transport: Play",
            TransportCommand::Pause => "Transport: Pause",
            TransportCommand::Stop => "Transport: Stop",
            TransportCommand::TogglePlayPause => "Transport: Toggle play/pause",
        };
        
        // Look up command ID by name
        let action_id = medium_reaper.named_command_lookup(action_name)
            .ok_or_else(|| {
                error!(action_name, "Transport command not found");
                format!("Command '{}' not found in REAPER", action_name)
            })?;
        
        // Execute the action
        medium_reaper.main_on_command_ex(action_id, 0, ProjectContext::CurrentProject);
        
        info!(command = ?command, action_name, "Executed transport command");
        Ok(())
    }
    
    fn execute_navigation_command(&self, command: NavigationCommand) -> Result<(), String> {
        // Navigation commands require custom action IDs that are registered in reaper_extension
        // We can't access those from the fts module, but we can provide a helpful error message
        // and suggest using the reaper_extension's command service instead
        
        let command_name = match command {
            NavigationCommand::NextSectionOrSong => "FTS_LIVE_GO_TO_NEXT_SECTION_SONG_SMART",
            NavigationCommand::PreviousSectionOrSong => "FTS_LIVE_GO_TO_PREVIOUS_SECTION_SONG_SMART",
        };
        
        warn!(
            command = ?command,
            command_name,
            "Navigation commands require custom action IDs from reaper_extension. \
             These actions must be registered in the reaper_extension app. \
             Consider using the CommandService from reaper_extension instead."
        );
        
        Err(format!(
            "Navigation command '{:?}' requires custom action ID '{}' which is not available in the fts module. \
             Please use the CommandService from reaper_extension to execute navigation commands.",
            command,
            command_name
        ))
    }
    
    fn toggle_loop(&self) -> Result<(), String> {
        let medium_reaper = Reaper::get().medium_reaper();
        
        // Look up command ID by name
        let action_id = medium_reaper.named_command_lookup("Transport: Toggle repeat")
            .ok_or_else(|| {
                error!("Transport: Toggle repeat command not found");
                "Command 'Transport: Toggle repeat' not found in REAPER".to_string()
            })?;
        
        // Execute the action
        medium_reaper.main_on_command_ex(action_id, 0, ProjectContext::CurrentProject);
        
        info!("Toggled loop state");
        Ok(())
    }
}

impl LyricsReader for ReaperProject {
    fn read_lyrics_from_project(&self, _project: &dyn std::any::Any) -> Result<LyricsData, LyricsReadError> {
        // Use self as the project (the trait design allows for flexibility with &dyn std::any::Any)
        // The reaper module has its own LyricsData type that we need to convert to the trait type
        use crate::lyrics::infra::reaper::read::{LyricsData as ReaperLyricsData, LyricsReadError as ReaperLyricsReadError};
        
        info!("Reading lyrics from project");
        
        let reaper_data: ReaperLyricsData = read_lyrics_from_project(self.clone())
            .map_err(|e: ReaperLyricsReadError| {
                let error = match e {
                    ReaperLyricsReadError::LyricsFolderNotFound => LyricsReadError::LyricsFolderNotFound,
                    ReaperLyricsReadError::SlidesTrackNotFound => LyricsReadError::SlidesTrackNotFound,
                    ReaperLyricsReadError::MidiReadFailed => LyricsReadError::MidiReadFailed,
                };
                warn!(error = %error, "Failed to read lyrics from project");
                error
            })?;
        
        // Convert ReaperLyricsData to trait LyricsData (they have the same structure)
        let result = LyricsData {
            slides: reaper_data.slides.into_iter().map(|s| SlideData {
                text: s.text,
                position: s.position,
                length: s.length,
            }).collect(),
            midi_tracks: reaper_data.midi_tracks.into_iter().map(|t| MidiTrackData {
                name: t.name,
                items: t.items.into_iter().map(|i| MidiItemData {
                    position: i.position,
                    length: i.length,
                    events: i.events.into_iter().map(|e| MidiEventData {
                        offset_ticks: e.offset_ticks,
                        flag: e.flag,
                        message: e.message,
                    }).collect(),
                }).collect(),
            }).collect(),
        };
        
        info!(
            slide_count = result.slides.len(),
            midi_track_count = result.midi_tracks.len(),
            "Successfully read lyrics from project"
        );
        
        Ok(result)
    }
}

impl LyricsWriter for ReaperProject {
    fn write_lyrics_to_project(
        &self,
        _project: &dyn std::any::Any,
        lyrics: &Lyrics,
    ) -> Result<(), LyricsWriteError> {
        // Validate input - check if lyrics has any content
        if lyrics.sections.is_empty() {
            warn!("Attempted to write empty lyrics (no sections)");
            return Err(LyricsWriteError::WriteFailed("Cannot write empty lyrics (no sections)".to_string()));
        }
        
        // Use self as the project
        // The existing write function requires finding the project by song index,
        // so we need to find which song this project corresponds to
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();
        
        info!("Writing lyrics to project (finding song index)");
        
        // Build setlist to find which song this project corresponds to
        let setlist = build_setlist_impl(None)
            .map_err(|e| {
                error!(error = %e, "Failed to build setlist for lyrics write");
                LyricsWriteError::WriteFailed(format!("Failed to build setlist: {}", e))
            })?;
        
        if setlist.songs.is_empty() {
            return Err(LyricsWriteError::ProjectNotFound(
                "Setlist is empty - no songs available".to_string()
            ));
        }
        
        // Find the song index for this project
        let self_raw = self.raw();
        let mut song_index = None;
        
        for (idx, song) in setlist.songs.iter().enumerate() {
            let project_name = song.project_name_from_metadata();
            
            // Find the project tab for this song
            for i in 0..128u32 {
                if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 512) {
                    if result.project == self_raw {
                        let tab_name = result.file_path.as_ref()
                            .and_then(|p| p.as_std_path().file_stem())
                            .and_then(|s| s.to_str())
                            .map(|s| s.to_string())
                            .unwrap_or_else(|| format!("Tab {}", i));
                        
                        let normalized_tab = tab_name.to_uppercase().replace('_', "-");
                        let normalized_target = project_name.to_uppercase().replace('_', "-");
                        
                        if normalized_tab == normalized_target {
                            song_index = Some(idx);
                            debug!(
                                song_index = idx,
                                song_name = %song.name,
                                project_name = %project_name,
                                "Found matching song for project"
                            );
                            break;
                        }
                    }
                } else {
                    break;
                }
            }
            
            if song_index.is_some() {
                break;
            }
        }
        
        let song_idx = song_index.ok_or_else(|| {
            error!("Could not find song for this project");
            LyricsWriteError::ProjectNotFound(
                "Could not find song for this project in the setlist".to_string()
            )
        })?;
        
        // Delegate to existing function
        let section_count = lyrics.sections.len();
        let total_line_count: usize = lyrics.sections.iter().map(|s| s.lines.len()).sum();
        info!(
            song_index = song_idx,
            section_count,
            total_line_count,
            "Updating lyrics in project"
        );
        update_lyrics_in_reaper(song_idx, lyrics.clone())
            .map_err(|e| {
                error!(error = %e, song_index = song_idx, "Failed to update lyrics");
                LyricsWriteError::WriteFailed(e)
            })
    }
    
    fn update_lyrics_in_project(
        &self,
        song_index: usize,
        lyrics: Lyrics,
    ) -> Result<(), LyricsWriteError> {
        // Validate inputs
        if song_index == usize::MAX {
            return Err(LyricsWriteError::WriteFailed("Invalid song index".to_string()));
        }
        
        info!(
            song_index,
            "Updating lyrics in project for song"
        );
        
        // Delegate to existing function
        update_lyrics_in_reaper(song_index, lyrics)
            .map_err(|e| {
                error!(error = %e, song_index, "Failed to update lyrics in project");
                LyricsWriteError::WriteFailed(e)
            })
    }
}
