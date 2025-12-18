//! Smooth Seek Service
//!
//! Handles smooth seek queue processing for seamless playback position changes.

use reaper_high::{Reaper, Project};
use reaper_medium::{ProjectRef, PositionInSeconds};
use fts::setlist::infra::reaper::ReaperTransport;
#[cfg(feature = "live")]
use crate::live::tracks::smooth_seek::get_smooth_seek_handler;
use tracing::warn;

/// Service for processing smooth seek operations
#[derive(Debug)]
pub struct SmoothSeekService;

impl SmoothSeekService {
    /// Create a new smooth seek service
    pub fn new() -> Self {
        Self
    }

    /// Process smooth seek queue (called from main thread/timer callback)
    /// 
    /// This checks if there are any queued smooth seeks that should be executed
    /// based on the current playback position.
    pub fn process_smooth_seek_queue(&self) {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();
        
        // Get current project
        let Some(project_result) = medium_reaper.enum_projects(ProjectRef::Current, 0) else {
            return;
        };
        
        let project = Project::new(project_result.project);
        let play_state = project.play_state();
        
        #[cfg(feature = "live")]
        {
            // Only process smooth seek queue if we're playing
            if play_state.is_playing || play_state.is_paused {
                // Get current play position using transport adapter
                let transport = ReaperTransport::new(project.clone());
                if let Some(transport_info) = transport.read_transport().ok() {
                    let current_pos = PositionInSeconds::new(transport_info.playhead_position.time.to_seconds())
                        .unwrap_or_else(|_| project.edit_cursor_position().unwrap_or(PositionInSeconds::ZERO));
                    
                    let smooth_seek_handler = get_smooth_seek_handler();
                    if let Err(e) = smooth_seek_handler.process_playback_position(&project, current_pos) {
                        warn!(error = %e, "Error processing smooth seek queue");
                    }
                }
            }
            
            // Process pending marker deletions (always, not just when playing)
            let smooth_seek_handler = get_smooth_seek_handler();
            smooth_seek_handler.process_pending_deletions(&project);
        }
    }
}

