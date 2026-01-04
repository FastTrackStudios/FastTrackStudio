//! REAPER implementation of TrackCommandHandler
//!
//! This module provides the REAPER-specific implementation for executing track commands
//! like mute, solo, etc. It handles main-thread dispatching for REAPER API calls.
//!
//! This module is only available when the `reaper` feature is enabled.

use async_trait::async_trait;
use std::sync::{Arc, OnceLock};

use fragile::Fragile;
use reaper_high::{Reaper, Project as ReaperProject, TaskSupport};
use reaper_medium::{ProjectRef, SoloMode as ReaperSoloMode};
use tracing::{info, warn};

use daw::tracks::reactive::irpc::TrackCommandHandler;
use daw::tracks::api::solo::SoloMode;

use super::tracks::ReaperTrackReactiveService;
use super::SetlistProvider;

/// Type alias for the static track reactive service
type StaticTrackService = Fragile<Arc<dyn TrackServiceAccessor>>;

/// Trait for accessing the track reactive service
/// This allows us to store a type-erased reference to the track service
/// 
/// Note: This trait is NOT Send + Sync because ReaperTrackReactiveService contains
/// non-thread-safe types (RefCell/Rc). Access is controlled via Fragile wrapper
/// which ensures main-thread-only access.
pub trait TrackServiceAccessor {
    /// Update a single track from REAPER (called after command execution)
    fn update_track_from_reaper(&self, project: ReaperProject, track_index: usize);
}

/// Static storage for track reactive service (main thread only)
/// Wrapped in Fragile to ensure main-thread-only access
static TRACK_REACTIVE_SERVICE: OnceLock<StaticTrackService> = OnceLock::new();

/// Initialize track reactive service (called from main thread during setup)
pub fn init_track_reactive_service(service: Arc<dyn TrackServiceAccessor>) {
    if TRACK_REACTIVE_SERVICE.set(Fragile::new(service)).is_err() {
        panic!("Track reactive service already initialized");
    }
}

/// Get track reactive service (main thread only)
/// Panics if called from wrong thread
fn get_track_reactive_service() -> Option<Arc<dyn TrackServiceAccessor>> {
    TRACK_REACTIVE_SERVICE.get().map(|f| f.get().clone())
}

/// Trait for providing project lookup from project name
pub trait ProjectProvider: Send + Sync {
    /// Get the REAPER project from a project name
    fn get_project_from_name(&self, project_name: &str) -> Option<ReaperProject>;
}

/// REAPER implementation of TrackCommandHandler
pub struct ReaperTrackCommandHandler<P: ProjectProvider> {
    project_provider: Arc<P>,
    task_support: Arc<TaskSupport>,
}

impl<P: ProjectProvider> ReaperTrackCommandHandler<P> {
    pub fn new(project_provider: Arc<P>, task_support: Arc<TaskSupport>) -> Self {
        Self {
            project_provider,
            task_support,
        }
    }
}

/// Default project provider that looks up projects by name
pub struct DefaultProjectProvider;

impl ProjectProvider for DefaultProjectProvider {
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
impl<P: ProjectProvider + 'static> TrackCommandHandler for ReaperTrackCommandHandler<P> {
    async fn set_track_mute(&self, project_id: String, track_index: usize, muted: bool) -> Result<(), String> {
        info!("[Track Command] Setting track {} mute to {} in project {}", track_index, muted, project_id);
        
        let project_name = project_id.clone();
        let project_provider = self.project_provider.clone();
        
        // Dispatch to main thread using task_support
        let result = (&*self.task_support)
            .main_thread_future(move || {
                // This closure runs on the main thread
                let project = project_provider.get_project_from_name(&project_name)
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
                info!("[Track Command] Successfully set track {} mute to {} in project {}", track_index, muted, project_id);
                Ok(())
            }
            Err(e) => {
                warn!("[Track Command] Failed to set track mute: {}", e);
                Err(e)
            }
        }
    }

    async fn set_track_solo(&self, project_id: String, track_index: usize, solo_mode: SoloMode) -> Result<(), String> {
        info!("[Track Command] Setting track {} solo to {:?} in project {}", track_index, solo_mode, project_id);
        
        let project_name = project_id.clone();
        let project_provider = self.project_provider.clone();
        
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
        let result = (&*self.task_support)
            .main_thread_future(move || {
                // This closure runs on the main thread
                let project = project_provider.get_project_from_name(&project_name)
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
                info!("[Track Command] Successfully set track {} solo to {:?} in project {}", track_index, solo_mode, project_id);
                Ok(())
            }
            Err(e) => {
                warn!("[Track Command] Failed to set track solo: {}", e);
                Err(e)
            }
        }
    }
}

/// Wrapper type to implement TrackServiceAccessor for ReaperTrackReactiveService
/// This allows us to store it in the static without the generic
pub struct TrackServiceWrapper<S: SetlistProvider> {
    inner: Arc<ReaperTrackReactiveService<S>>,
}

impl<S: SetlistProvider> TrackServiceWrapper<S> {
    pub fn new(service: Arc<ReaperTrackReactiveService<S>>) -> Self {
        Self { inner: service }
    }
}

impl<S: SetlistProvider + 'static> TrackServiceAccessor for TrackServiceWrapper<S> {
    fn update_track_from_reaper(&self, project: ReaperProject, track_index: usize) {
        self.inner.update_track_from_reaper(project, track_index);
    }
}
