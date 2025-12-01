//! Setlist Service
//!
//! Manages setlist state and provides access to the current setlist API.

use std::sync::{Arc, Mutex};
use fts::setlist::SetlistApi;
use crate::implementation::setlist::build_setlist_from_open_projects;
use crate::infrastructure::reactive_polling::ReactivePollingService;
use tracing::info;

/// Service for managing setlist state
#[derive(Debug)]
pub struct SetlistService {
    /// Current setlist API state
    current_setlist: Arc<Mutex<Option<SetlistApi>>>,
}

impl SetlistService {
    /// Create a new setlist service
    pub fn new() -> Self {
        Self {
            current_setlist: Arc::new(Mutex::new(None)),
        }
    }

    /// Update the setlist from REAPER projects
    /// This should be called from the main thread (e.g., timer callback)
    /// 
    /// This method:
    /// 1. Rebuilds the setlist from open projects (reads fresh transport info for each song)
    /// 2. Determines the active song/section based on current transport position
    /// 3. Updates the internal state that the stream API reads from
    pub fn update_setlist(&self) -> Result<(), Box<dyn std::error::Error>> {
        use tracing::trace;
        
        // Build setlist from open projects - this reads fresh transport info for each song
        // Each song's transport_info field is populated with current transport state
        let setlist = build_setlist_from_open_projects(None)?;
        
        // Get current project name and transport position to determine active song
        let reaper = reaper_high::Reaper::get();
        let current_project = reaper.current_project();
        let transport_adapter = crate::implementation::transport::ReaperTransport::new(current_project.clone());
        
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
        
        // Get current transport state (for determining active song/section)
        let transport_result = transport_adapter.read_transport();
        let transport_position = transport_result
            .as_ref()
            .map(|t| t.playhead_position.time.to_seconds())
            .unwrap_or(0.0);
        
        // Extract musical position early (with measure offset already applied)
        let transport_musical_opt = transport_result
            .as_ref()
            .ok()
            .map(|t| t.playhead_position.musical.clone());
        
        // Log transport state periodically for debugging
        static UPDATE_COUNT: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
        let update_count = UPDATE_COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        if update_count % 1000 == 0 && update_count > 0 {
            if let Ok(transport) = transport_result {
                info!(
                    update_count,
                    position_seconds = transport_position,
                    is_playing = matches!(transport.play_state, daw::transport::PlayState::Playing | daw::transport::PlayState::Recording),
                    tempo_bpm = transport.tempo.bpm,
                    "Setlist update: transport state"
                );
            }
        }
        
        // Determine active song based on project name and transport position
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
        
        // Determine active slide index within the active section (for lyrics view)
        // Use time positions directly for comparison - this is more reliable and avoids
        // measure offset conversion issues. Time positions are absolute and don't change with tempo.
        let active_slide_idx = active_song_idx
            .and_then(|song_idx| {
                setlist.songs.get(song_idx)
                    .and_then(|song| {
                        // Get lyrics for this song
                        song.lyrics.as_ref().and_then(|lyrics| {
                            // Use transport time position directly (no conversion needed)
                            // Slides are stored as time positions, so compare time to time
                            
                            // Calculate global slide index across all sections
                            let mut global_slide_idx: usize = 0;
                            
                            // Iterate through all sections and lines to find the active slide
                            for section in &lyrics.sections {
                                for line in &section.lines {
                                    // Compare time positions directly (slides are stored as time positions)
                                    if let (Some(line_start_time), Some(line_end_time)) = (line.start_time, line.end_time) {
                                        // Check if transport time position is within the line's time range
                                        // Use >= for start (inclusive) and < for end (exclusive)
                                        if transport_position >= line_start_time && transport_position < line_end_time {
                                            // Log slide match for debugging
                                            static LAST_SLIDE: std::sync::Mutex<Option<usize>> = std::sync::Mutex::new(None);
                                            let mut last_slide = LAST_SLIDE.lock().unwrap();
                                            // Subtract 1 from index to correct off-by-one error
                                            let adjusted_index: usize = global_slide_idx.saturating_sub(1);
                                            if *last_slide != Some(adjusted_index) {
                                                // Also show musical positions for reference
                                                let transport_musical_str = transport_musical_opt.as_ref()
                                                    .map(|m| format!("{}.{}.{:03}", m.measure, m.beat, m.subdivision))
                                                    .unwrap_or_else(|| "N/A".to_string());
                                                
                                                info!(
                                                    slide_index = adjusted_index,
                                                    transport_time = transport_position,
                                                    line_start_time = line_start_time,
                                                    line_end_time = line_end_time,
                                                    transport_musical = %transport_musical_str,
                                                    "Active slide matched (time position comparison)"
                                                );
                                                *last_slide = Some(adjusted_index);
                                            }
                                            return Some(adjusted_index);
                                        }
                                    }
                                    
                                    global_slide_idx += 1;
                                }
                            }
                            
                            // If no line matched with timing, try to find by section position
                            // This is a fallback when timing info isn't available
                            active_section_idx.and_then(|section_idx| {
                                // Find the section in lyrics that matches the active song section
                                if let Some(song_section) = song.sections.get(section_idx) {
                                    // Find matching lyric section by name
                                    lyrics.sections.iter()
                                        .find(|lyric_section| lyric_section.name == song_section.name)
                                        .and_then(|lyric_section| {
                                            // Return first slide index in this section
                                            // Calculate offset: sum of lines in previous sections
                                            // Start at 1 for 1-based indexing (offset by 1)
                                            let mut offset = 1;
                                            for prev_section in &lyrics.sections {
                                                if prev_section.name == lyric_section.name {
                                                    break;
                                                }
                                                offset += prev_section.lines.len();
                                            }
                                            // Return first slide in this section (or None if empty)
                                            // Subtract 1 from index to correct off-by-one error
                                            if lyric_section.lines.is_empty() {
                                                None
                                            } else {
                                                Some(offset.saturating_sub(1))
                                            }
                                        })
                                } else {
                                    None
                                }
                            })
                        })
                    })
            });
        
        // Convert setlist to SetlistApi with active indices
        // Note: Each song in the setlist already has transport_info populated with current state
        let setlist_api = SetlistApi::new(
            setlist,
            active_song_idx,
            active_section_idx,
            active_slide_idx,
        );
        
        // Update state - this is what the stream API reads from
        let mut current = self.current_setlist.lock().unwrap();
        let was_none = current.is_none();
        
        // Track previous active slide index to detect changes
        let prev_active_slide = current.as_ref().and_then(|api| api.active_slide_index());
        let new_active_slide = setlist_api.active_slide_index();
        
        // Log when active slide changes
        if prev_active_slide != new_active_slide {
            if let Some(slide_idx) = new_active_slide {
                info!(
                    active_slide_index = slide_idx,
                    previous_slide_index = ?prev_active_slide,
                    transport_position = transport_position,
                    "Active slide changed"
                );
            } else if prev_active_slide.is_some() {
                // Slide became None (e.g., moved to section without lyrics)
                info!(
                    previous_slide_index = ?prev_active_slide,
                    transport_position = transport_position,
                    "Active slide cleared (no active slide)"
                );
            }
        }
        
        *current = Some(setlist_api.clone());
        
        // Log when state transitions from None to Some (first update)
        if was_none {
            info!(
                song_count = setlist_api.setlist.song_count(),
                active_song = ?setlist_api.active_song_index(),
                active_section = ?setlist_api.active_section_index(),
                active_slide = ?setlist_api.active_slide_index(),
                "Setlist state initialized - stream API can now send updates"
            );
            
            // Now that the first project is loaded, we can initialize deferred loggers
            // This will be done in the timer callback to avoid RefCell borrow panics
        }
        
        trace!("Setlist state updated successfully");
        Ok(())
    }

    /// Update setlist and notify reactive polling service of active indices
    /// This version emits reactive events only when indices change
    pub fn update_setlist_with_polling(
        &self,
        polling_service: Arc<ReactivePollingService>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        // First, do the normal update
        self.update_setlist()?;
        
        // Then, get the active indices and notify the polling service
        if let Some(setlist_api) = self.get_setlist() {
            polling_service.update_active_indices(
                setlist_api.active_song_index(),
                setlist_api.active_section_index(),
                setlist_api.active_slide_index(),
            );
        }
        
        Ok(())
    }

    /// Get the current setlist API
    /// Returns None if no setlist has been built yet
    pub fn get_setlist(&self) -> Option<SetlistApi> {
        self.current_setlist.lock().unwrap().clone()
    }

    /// Get a reference to the internal state (for use by state provider)
    pub(crate) fn state(&self) -> Arc<Mutex<Option<SetlistApi>>> {
        self.current_setlist.clone()
    }
}

