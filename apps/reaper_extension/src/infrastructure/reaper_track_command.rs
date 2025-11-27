//! REAPER implementation of TrackCommandHandler

use async_trait::async_trait;
use daw::tracks::reactive::irpc::TrackCommandHandler;
use daw::tracks::api::solo::SoloMode;
use reaper_high::{Reaper, Track as ReaperTrack, Project as ReaperProject, TaskSupport};
use reaper_medium::{ProjectRef, SoloMode as ReaperSoloMode};
use std::sync::{Arc, OnceLock};
use fragile::Fragile;
use tracing::{info, warn};

/// Static storage for track reactive service (main thread only)
/// Wrapped in Fragile to ensure main-thread-only access
static TRACK_REACTIVE_SERVICE: OnceLock<Fragile<Arc<crate::infrastructure::reaper_track_reactive::ReaperTrackReactiveService>>> = OnceLock::new();

/// Initialize track reactive service (called from main thread during setup)
pub fn init_track_reactive_service(service: Arc<crate::infrastructure::reaper_track_reactive::ReaperTrackReactiveService>) {
    if TRACK_REACTIVE_SERVICE.set(Fragile::new(service)).is_err() {
        panic!("Track reactive service already initialized");
    }
}

/// Get track reactive service (main thread only)
/// Panics if called from wrong thread
fn get_track_reactive_service() -> Option<Arc<crate::infrastructure::reaper_track_reactive::ReaperTrackReactiveService>> {
    TRACK_REACTIVE_SERVICE.get().map(|f| f.get().clone())
}

/// REAPER implementation of TrackCommandHandler
pub struct ReaperTrackCommandHandler {
    setlist_service: Arc<crate::services::setlist_service::SetlistService>,
    task_support: Arc<TaskSupport>,
}

impl ReaperTrackCommandHandler {
    pub fn new(
        setlist_service: Arc<crate::services::setlist_service::SetlistService>,
        task_support: Arc<TaskSupport>,
    ) -> Self {
        Self {
            setlist_service,
            task_support,
        }
    }

    /// Get the REAPER project from project name
    fn get_project_from_name(&self, project_name: &str) -> Option<ReaperProject> {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();
        
        // Find project by name
        for i in 0..128u32 {
            if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 512) {
                if let Some(file_path) = result.file_path.as_ref() {
                    if let Some(name) = file_path.as_std_path().file_stem().and_then(|s| s.to_str()) {
                        if name == project_name {
                            return Some(ReaperProject::new(result.project));
                        }
                    }
                }
            } else {
                break;
            }
        }
        
        None
    }
}

#[async_trait]
impl TrackCommandHandler for ReaperTrackCommandHandler {
    async fn set_track_mute(&self, project_id: String, track_index: usize, muted: bool) -> Result<(), String> {
        info!("[Track Command] Setting track {} mute to {} in project {}", track_index, muted, project_id);
        
        let project_name = project_id.clone();
        let setlist_service_for_closure = self.setlist_service.clone();
        let task_support_for_closure = self.task_support.clone();
        let task_support_ref = &*self.task_support;
        
        // Dispatch to main thread using task_support
        let result = task_support_ref
            .main_thread_future(move || {
                // This closure runs on the main thread
                let handler = ReaperTrackCommandHandler {
                    setlist_service: setlist_service_for_closure,
                    task_support: task_support_for_closure,
                };
                
                let project = handler.get_project_from_name(&project_name)
                    .ok_or_else(|| format!("Project not found: {}", project_name))?;
                
                let track = project.track_by_index(track_index as u32)
                    .ok_or_else(|| format!("Track {} not found in project {}", track_index, project_name))?;
                
                use reaper_medium::GangBehavior;
                use reaper_high::GroupingBehavior;
                
                track.set_mute(muted, GangBehavior::DenyGang, GroupingBehavior::PreventGrouping);
                
                // Immediately update the reactive service to reflect the change
                // We're on the main thread, so we can safely access the track service
                if let Some(track_service) = get_track_reactive_service() {
                    track_service.update_track_from_reaper(project, track_index);
                }
                
                Ok(())
            })
            .await
            .map_err(|e| format!("Failed to dispatch to main thread: {}", e))?;
        
        match result {
            Ok(()) => {
                info!("[Track Command] ✅ Successfully set track {} mute to {} in project {}", track_index, muted, project_id);
                Ok(())
            }
            Err(e) => {
                warn!("[Track Command] ❌ Failed to set track mute: {}", e);
                Err(e)
            }
        }
    }

    async fn set_track_solo(&self, project_id: String, track_index: usize, solo_mode: SoloMode) -> Result<(), String> {
        info!("[Track Command] Setting track {} solo to {:?} in project {}", track_index, solo_mode, project_id);
        
        let project_name = project_id.clone();
        let setlist_service_for_closure = self.setlist_service.clone();
        let task_support_for_closure = self.task_support.clone();
        
        // Convert SoloMode to REAPER solo mode (can be done outside main thread)
        let reaper_solo_mode = match solo_mode {
            SoloMode::Off => ReaperSoloMode::Off,
            SoloMode::Solo => ReaperSoloMode::SoloInPlace, // Map Solo to SoloInPlace
            SoloMode::SoloInPlace => ReaperSoloMode::SoloInPlace,
            SoloMode::SafeSolo => ReaperSoloMode::SafeSoloIgnoreRouting,
            SoloMode::SafeSoloInPlace => ReaperSoloMode::SafeSoloInPlace,
            SoloMode::Unknown(hidden) => {
                // For unknown variants, try to preserve the raw value
                // REAPER's SoloMode::Unknown is private, so we'll use SoloInPlace as fallback
                warn!("Unknown solo mode value: {}, using SoloInPlace as fallback", hidden.0);
                ReaperSoloMode::SoloInPlace
            }
        };
        
        // Dispatch to main thread using task_support
        // Use a reference to avoid borrowing issues
        let result = (&*self.task_support)
            .main_thread_future(move || {
                // This closure runs on the main thread
                let handler = ReaperTrackCommandHandler {
                    setlist_service: setlist_service_for_closure,
                    task_support: task_support_for_closure,
                };
                
                let project = handler.get_project_from_name(&project_name)
                    .ok_or_else(|| format!("Project not found: {}", project_name))?;
                
                let track = project.track_by_index(track_index as u32)
                    .ok_or_else(|| format!("Track {} not found in project {}", track_index, project_name))?;
                
                track.set_solo_mode(reaper_solo_mode);
                
                // Immediately update the reactive service to reflect the change
                // We're on the main thread, so we can safely access the track service
                if let Some(track_service) = get_track_reactive_service() {
                    track_service.update_track_from_reaper(project, track_index);
                }
                
                Ok(())
            })
            .await
            .map_err(|e| format!("Failed to dispatch to main thread: {}", e))?;
        
        match result {
            Ok(()) => {
                info!("[Track Command] ✅ Successfully set track {} solo to {:?} in project {}", track_index, solo_mode, project_id);
                Ok(())
            }
            Err(e) => {
                warn!("[Track Command] ❌ Failed to set track solo: {}", e);
                Err(e)
            }
        }
    }
}

