//! REAPER Setlist Stream Implementation
//!
//! Implements the setlist stream backend for REAPER extension.
//! Uses the protocol definition from the setlist crate.

use anyhow::Result;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use tracing::{info, warn};
use setlist::{SetlistApi, SetlistStreamApi, SetlistStateProvider};

use crate::reaper_setlist::build_setlist_from_open_projects;
use std::sync::OnceLock;

/// Shared state for setlist updates
/// Updated from REAPER's main thread, read from async tasks
static LATEST_SETLIST_API: OnceLock<Arc<std::sync::Mutex<Option<SetlistApi>>>> = OnceLock::new();
static SETLIST_UPDATED: AtomicBool = AtomicBool::new(false);

/// Initialize the setlist state storage
pub fn init_setlist_state() -> Arc<std::sync::Mutex<Option<SetlistApi>>> {
    let state = Arc::new(std::sync::Mutex::new(None));
    LATEST_SETLIST_API.set(state.clone()).expect("Setlist API state already initialized");
    info!("[REAPER Setlist] Setlist API state storage initialized.");
    state
}

/// Update setlist state from REAPER's main thread
/// This is called from REAPER's timer callback at 30Hz
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

/// Create and expose the REAPER setlist stream service
/// 
/// This creates a setlist stream service with REAPER's specific
/// SetlistStateProvider implementation. Only the server (REAPER extension)
/// needs to know about this implementation - clients just connect via
/// SetlistStreamApi::connect().
pub fn create_reaper_setlist_stream_service() -> Result<SetlistStreamApi> {
    let state_provider = Arc::new(ReaperSetlistStateProvider);
    let api = SetlistStreamApi::spawn(state_provider);
    info!("[REAPER Setlist] Setlist stream service created.");
    Ok(api)
}

