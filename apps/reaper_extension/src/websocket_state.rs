//! Global WebSocket server state for the REAPER extension
//!
//! Provides a thread-safe way to access the WebSocket server state from anywhere in the extension.

use std::sync::{OnceLock, Mutex};
use std::sync::Arc;
use crate::websocket_server::WebSocketState;

/// Global WebSocket server state instance
static WS_STATE: OnceLock<Arc<WebSocketState>> = OnceLock::new();

/// Global tokio runtime handle for spawning tasks from non-tokio contexts
static RT_HANDLE: OnceLock<tokio::runtime::Handle> = OnceLock::new();

/// Channel for requesting setlist rebuilds from the main thread
/// The WebSocket server can send requests here, and the main thread can poll it
static SETLIST_REBUILD_REQUESTS: OnceLock<tokio::sync::mpsc::UnboundedSender<()>> = OnceLock::new();

/// Channel for requesting project switches from background threads
/// Format: project_name (String)
/// Uses std::sync::mpsc because receiver needs to be accessed from main thread (non-tokio context)
static PROJECT_SWITCH_SENDER: OnceLock<tokio::sync::mpsc::UnboundedSender<String>> = OnceLock::new();
static PROJECT_SWITCH_RECEIVER: OnceLock<Mutex<std::sync::mpsc::Receiver<String>>> = OnceLock::new();

/// Channel for requesting section seeks from background threads
/// Format: (project_name, song_name, section_name)
/// Uses std::sync::mpsc because receiver needs to be accessed from main thread (non-tokio context)
static SECTION_SEEK_SENDER: OnceLock<tokio::sync::mpsc::UnboundedSender<(String, String, String)>> = OnceLock::new();
static SECTION_SEEK_RECEIVER: OnceLock<Mutex<std::sync::mpsc::Receiver<(String, String, String)>>> = OnceLock::new();

/// Set the global WebSocket server state
pub fn set_global_ws_state(state: Arc<WebSocketState>) -> Result<(), Arc<WebSocketState>> {
    WS_STATE.set(state)
}

/// Set the global tokio runtime handle
pub fn set_global_rt_handle(handle: tokio::runtime::Handle) -> Result<(), tokio::runtime::Handle> {
    RT_HANDLE.set(handle)
}

/// Get the global WebSocket server state
pub fn get_global_ws_state() -> Option<Arc<WebSocketState>> {
    WS_STATE.get().cloned()
}

/// Get the global tokio runtime handle
pub fn get_global_rt_handle() -> Option<tokio::runtime::Handle> {
    RT_HANDLE.get().cloned()
}

/// Set the setlist rebuild request channel sender
pub fn set_setlist_rebuild_sender(sender: tokio::sync::mpsc::UnboundedSender<()>) -> Result<(), tokio::sync::mpsc::UnboundedSender<()>> {
    SETLIST_REBUILD_REQUESTS.set(sender)
}

/// Request a setlist rebuild from the main thread
/// This can be called from any thread (including WebSocket worker threads)
pub fn request_setlist_rebuild() {
    if let Some(sender) = SETLIST_REBUILD_REQUESTS.get() {
        let _ = sender.send(());
    }
}

/// Get the setlist rebuild request channel receiver
/// This should be called from the main thread to poll for rebuild requests
pub fn get_setlist_rebuild_receiver() -> Option<tokio::sync::mpsc::UnboundedReceiver<()>> {
    // We can't get the receiver from a OnceLock, so we'll need a different approach
    // For now, return None - we'll handle this differently
    None
}

/// Broadcast a setlist update
/// This can be called from any thread (including REAPER's main thread)
pub fn broadcast_setlist(setlist: setlist::core::Setlist, active_song_index: Option<usize>) {
    let song_count = setlist.songs.len();
    let song_names: Vec<String> = setlist.songs.iter()
        .map(|song| song.name.clone())
        .collect();
    
    tracing::info!(
        song_count = song_count,
        song_names = ?song_names,
        active_song_index = ?active_song_index,
        "📤 Attempting to broadcast setlist with {} songs: {:?}, active_song_index: {:?}",
        song_count,
        song_names,
        active_song_index
    );
    
    if let Some(state) = get_global_ws_state() {
        tracing::debug!("WebSocket state found, checking runtime handle...");
        if let Some(rt_handle) = get_global_rt_handle() {
            tracing::debug!("Runtime handle available, using broadcast_setlist_with_runtime");
            crate::websocket_server::broadcast_setlist_with_runtime(&state, setlist, active_song_index, &rt_handle);
        } else {
            tracing::warn!("Tokio runtime handle not available, cannot update cache");
            // Still try to broadcast even without cache update
            use crate::websocket_server::SetlistMessage;
            let msg = SetlistMessage::SetlistUpdate { 
                setlist: setlist.clone(),
                active_song_index,
            };
            match state.tx.send(msg) {
                Ok(count) => {
                    tracing::info!(
                        client_count = count,
                        song_count = song_count,
                        song_names = ?song_names,
                        active_song_index = ?active_song_index,
                        "📡 Broadcasted setlist (without cache update) to {} client(s) at 30Hz - {} songs: {:?}, active_song_index: {:?}",
                        count,
                        song_count,
                        song_names,
                        active_song_index
                    );
                }
                Err(e) => {
                    tracing::warn!(error = %e, "Failed to broadcast setlist");
                }
            }
        }
    } else {
        tracing::warn!("WebSocket server not initialized, cannot broadcast setlist");
    }
}

/// Broadcast a transport update for a specific project
/// This is a convenience wrapper - prefer using send_transport_update_for_project from main thread
pub fn broadcast_transport(
    project_name: &str,
    is_active: bool,
    playing: bool,
    position: f64,
    tempo: f64,
    song_progress: std::collections::HashMap<String, f64>,
    section_progress: std::collections::HashMap<String, f64>,
) {
    if let Some(state) = get_global_ws_state() {
        crate::websocket_server::broadcast_transport(&state, project_name, is_active, playing, position, tempo, song_progress, section_progress);
    } else {
        tracing::warn!("WebSocket server not initialized, cannot broadcast transport");
    }
}

/// Channel for requesting transport updates from the main thread
/// Format: (project_name, is_active, playing, position, tempo, song_progress, section_progress)
static TRANSPORT_REQUEST_SENDER: OnceLock<tokio::sync::mpsc::UnboundedSender<(String, bool, bool, f64, f64, std::collections::HashMap<String, f64>, std::collections::HashMap<String, f64>)>> = OnceLock::new();

/// Channel for requesting action execution from background threads
/// Command IDs are sent here and executed on the main thread via defer
static ACTION_EXECUTION_SENDER: OnceLock<std::sync::mpsc::Sender<reaper_medium::CommandId>> = OnceLock::new();
static ACTION_EXECUTION_RECEIVER: OnceLock<Mutex<std::sync::mpsc::Receiver<reaper_medium::CommandId>>> = OnceLock::new();

/// Set the transport update sender (called from main thread)
pub fn set_transport_update_sender(sender: tokio::sync::mpsc::UnboundedSender<(String, bool, bool, f64, f64, std::collections::HashMap<String, f64>, std::collections::HashMap<String, f64>)>) -> Result<(), tokio::sync::mpsc::UnboundedSender<(String, bool, bool, f64, f64, std::collections::HashMap<String, f64>, std::collections::HashMap<String, f64>)>> {
    TRANSPORT_REQUEST_SENDER.set(sender)
}

/// Calculate progress (0-1) for a song given transport position
fn calculate_song_progress(song: &setlist::core::Song, position: f64) -> f64 {
    // Try song_region markers first, then fall back to start_marker/song_end_marker
    let song_start = song.song_region_start_marker
        .as_ref()
        .map(|m| m.position.time.to_seconds())
        .or_else(|| song.start_marker.as_ref().map(|m| m.position.time.to_seconds()))
        .unwrap_or_else(|| {
            song.sections.first()
                .map(|s| s.start_position.time.to_seconds())
                .unwrap_or(0.0)
        });
    
    let song_end = song.song_region_end_marker
        .as_ref()
        .map(|m| m.position.time.to_seconds())
        .or_else(|| song.song_end_marker.as_ref().map(|m| m.position.time.to_seconds()))
        .unwrap_or_else(|| {
            song.sections.last()
                .map(|s| s.end_position.time.to_seconds())
                .unwrap_or(song_start)
        });
    
    if song_end <= song_start {
        return 0.0;
    }
    
    let relative_pos = (position - song_start).max(0.0);
    let song_length = song_end - song_start;
    
    (relative_pos / song_length).clamp(0.0, 1.0)
}

/// Calculate progress (0-1) for a section given transport position
fn calculate_section_progress(section: &setlist::core::Section, position: f64) -> f64 {
    let start = section.start_position.time.to_seconds();
    let end = section.end_position.time.to_seconds();
    
    if end <= start {
        return 0.0;
    }
    
    if position >= end {
        return 1.0;
    }
    if position < start {
        return 0.0;
    }
    
    ((position - start) / (end - start)).clamp(0.0, 1.0)
}

/// Send a transport update for a specific project from the main thread
/// Calculates progress for all songs and sections in the project
/// Uses cached setlist to avoid expensive rebuilds at 30Hz
pub fn send_transport_update_for_project(project_name: &str, is_active: bool, playing: bool, position: f64, tempo: f64) {
    let mut song_progress = std::collections::HashMap::new();
    let mut section_progress = std::collections::HashMap::new();
    
    // Use cached setlist instead of rebuilding - this is called at 30Hz!
    let cache = CACHED_SETLIST.get_or_init(|| Mutex::new(None));
    let setlist_opt = if let Ok(cached) = cache.lock() {
        cached.as_ref().cloned()
    } else {
        None
    };
    
    // If no cached setlist, skip this update (it will be available on next setlist rebuild)
    let setlist = match setlist_opt {
        Some(s) => s,
        None => {
            // No cached setlist yet - this is fine, just skip this transport update
            return;
        }
    };
    
    for song in &setlist.songs {
        // Check if this song belongs to this project
        let song_project_name = song.metadata.get("project_name")
            .or_else(|| song.metadata.get("Project"))
            .or_else(|| song.metadata.get("project"));
        
        if song_project_name.map(|s| s.as_str()) != Some(project_name) {
            continue;
        }
        
        // Calculate song progress
        let progress = calculate_song_progress(song, position);
        song_progress.insert(song.name.clone(), progress);
        
        // Calculate progress for each section
        for section in &song.sections {
            let section_prog = calculate_section_progress(section, position);
            let key = format!("{}:{}", song.name, section.name);
            section_progress.insert(key, section_prog);
        }
    }
    
    if let Some(sender) = TRANSPORT_REQUEST_SENDER.get() {
        let _ = sender.send((project_name.to_string(), is_active, playing, position, tempo, song_progress, section_progress));
    }
}

/// Poll current transport state from all open REAPER projects and broadcast them
/// This must be called from REAPER's main thread
pub fn poll_and_broadcast_transport() {
    use reaper_high::{Project, Reaper as HighReaper};
    use reaper_medium::ProjectRef;
    
    let reaper = HighReaper::get();
    let medium_reaper = reaper.medium_reaper();
    let current_project = reaper.current_project();
    let current_project_raw = current_project.raw();
    
    // Enumerate all project tabs and get transport state for each
    for i in 0..128u32 {
        if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 512) {
            // Get project name for identification
            let project_name = if let Some(file_path) = result.file_path.as_ref() {
                file_path
                    .as_std_path()
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| format!("Tab {}", i))
            } else {
                format!("Tab {}", i)
            };
            
            // Skip FTS-ROUTING tab
            let normalized_name = project_name.to_uppercase().replace('_', "-");
            if normalized_name == "FTS-ROUTING" {
                continue;
            }
            
            // Check if this is the active project
            let is_active = result.project == current_project_raw;
            
            let project = Project::new(result.project);
            let play_state = project.play_state();
            let is_playing = play_state.is_playing;
            
            // Get play position (latency-compensated when playing)
            let position = if is_playing {
                project.play_position_latency_compensated().get()
            } else {
                project.edit_cursor_position()
                    .unwrap_or(reaper_medium::PositionInSeconds::ZERO)
                    .get()
            };
            
            // Get tempo
            let tempo = project.tempo().bpm().get();
            
            // Send transport update for this project to WebSocket server
            // Always calculate progress, even if not playing (for active project display)
            send_transport_update_for_project(&project_name, is_active, is_playing, position, tempo);
        } else {
            break;
        }
    }
}

/// Determine the active song index based on transport state
/// Priority:
/// 1. If something is playing, use the most recently started playing project's song
/// 2. If nothing is playing, use the active project's song
/// Returns None if no active song can be determined
pub fn determine_active_song_index(setlist: &setlist::core::Setlist) -> Option<usize> {
    use reaper_high::{Project, Reaper as HighReaper};
    use reaper_medium::ProjectRef;
    
    if setlist.songs.is_empty() {
        return None;
    }
    
    let reaper = HighReaper::get();
    let medium_reaper = reaper.medium_reaper();
    let current_project = reaper.current_project();
    let current_project_raw = current_project.raw();
    
    // Track playing projects with their start times (we'll use current time as proxy)
    // Since we can't track exact start times in the backend easily, we'll use a simpler approach:
    // If multiple projects are playing, prefer the one that's currently active
    let mut playing_projects: Vec<(String, bool, f64)> = Vec::new(); // (project_name, is_active, position)
    let mut active_project_name: Option<String> = None;
    
    // Enumerate all project tabs to find playing projects
    for i in 0..128u32 {
        if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 512) {
            // Get project name
            let project_name = if let Some(file_path) = result.file_path.as_ref() {
                file_path
                    .as_std_path()
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| format!("Tab {}", i))
            } else {
                format!("Tab {}", i)
            };
            
            // Skip FTS-ROUTING tab
            let normalized_name = project_name.to_uppercase().replace('_', "-");
            if normalized_name == "FTS-ROUTING" {
                continue;
            }
            
            let is_active = result.project == current_project_raw;
            if is_active {
                active_project_name = Some(project_name.clone());
            }
            
            let project = Project::new(result.project);
            let play_state = project.play_state();
            let is_playing = play_state.is_playing;
            
            if is_playing {
                let position = project.play_position_latency_compensated().get();
                playing_projects.push((project_name, is_active, position));
            }
        } else {
            break;
        }
    }
    
    // Helper to find song index by project name and position
    let find_song_by_project = |project_name: &str, position: f64| -> Option<usize> {
        for (idx, song) in setlist.songs.iter().enumerate() {
            let song_project_name = song.metadata.get("project_name")
                .or_else(|| song.metadata.get("Project"))
                .or_else(|| song.metadata.get("project"));
            
            if song_project_name.map(|s| s.as_str()) != Some(project_name) {
                continue;
            }
            
            // Check if position is within song boundaries
            let song_start = song.start_position()
                .map(|p| p.time.to_seconds())
                .or_else(|| song.song_region_start().map(|p| p.time.to_seconds()))
                .unwrap_or(0.0);
            let song_end = song.song_end_position()
                .map(|p| p.time.to_seconds())
                .or_else(|| song.song_region_end().map(|p| p.time.to_seconds()))
                .unwrap_or(f64::MAX);
            
            if position >= song_start && position <= song_end {
                return Some(idx);
            }
        }
        None
    };
    
    // Helper to find first song in a project
    let find_first_song_in_project = |project_name: &str| -> Option<usize> {
        for (idx, song) in setlist.songs.iter().enumerate() {
            let song_project_name = song.metadata.get("project_name")
                .or_else(|| song.metadata.get("Project"))
                .or_else(|| song.metadata.get("project"));
            
            if song_project_name.map(|s| s.as_str()) == Some(project_name) {
                return Some(idx);
            }
        }
        None
    };
    
    // If something is playing, prioritize active project, then any playing project
    if !playing_projects.is_empty() {
        // Sort: active projects first, then by project name for consistency
        playing_projects.sort_by(|a, b| {
            match (a.1, b.1) {
                (true, false) => std::cmp::Ordering::Less,
                (false, true) => std::cmp::Ordering::Greater,
                _ => a.0.cmp(&b.0),
            }
        });
        
        let (project_name, _is_active, position) = &playing_projects[0];
        
        // Try to find song by position
        if let Some(idx) = find_song_by_project(project_name, *position) {
            return Some(idx);
        }
        
        // Fallback to first song in project
        if let Some(idx) = find_first_song_in_project(project_name) {
            return Some(idx);
        }
    }
    
    // If nothing is playing, use the active project
    if let Some(project_name) = active_project_name {
        let project = current_project;
        let position = project.edit_cursor_position()
            .unwrap_or(reaper_medium::PositionInSeconds::ZERO)
            .get();
        
        // Try to find song by position
        if let Some(idx) = find_song_by_project(&project_name, position) {
            return Some(idx);
        }
        
        // Fallback to first song in active project
        if let Some(idx) = find_first_song_in_project(&project_name) {
            return Some(idx);
        }
    }
    
    // Final fallback: return first song
    Some(0)
}

/// Cached setlist to avoid rebuilding too frequently
static CACHED_SETLIST: OnceLock<Mutex<Option<setlist::core::Setlist>>> = OnceLock::new();

/// Generate a simple hash of setlist structure for change detection
/// This compares song names, counts, and section counts without full deep comparison
fn setlist_structure_hash(setlist: &setlist::core::Setlist) -> u64 {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    
    let mut hasher = DefaultHasher::new();
    setlist.songs.len().hash(&mut hasher);
    for song in &setlist.songs {
        song.name.hash(&mut hasher);
        song.sections.len().hash(&mut hasher);
        // Hash section names/order
        for section in &song.sections {
            section.name.hash(&mut hasher);
            section.start_position.time.to_seconds().to_bits().hash(&mut hasher);
        }
    }
    hasher.finish()
}

/// Lightweight check: count open projects without building full setlist
fn count_open_projects() -> usize {
    use reaper_high::Reaper;
    use reaper_medium::ProjectRef;
    
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    let mut count = 0;
    
    for i in 0..128u32 {
        if medium_reaper.enum_projects(ProjectRef::Tab(i), 0).is_some() {
            count += 1;
        } else {
            break;
        }
    }
    count
}

/// This must be called from REAPER's main thread
pub fn poll_and_broadcast_setlist() {
    use crate::reaper_setlist::build_setlist_from_open_projects;
    
    // Rebuild setlist
    match build_setlist_from_open_projects(None) {
        Ok(setlist) => {
            let new_hash = setlist_structure_hash(&setlist);
            
            // Check if setlist structure has changed
            let cache = CACHED_SETLIST.get_or_init(|| Mutex::new(None));
            let mut should_broadcast = false;
            
            if let Ok(mut cached) = cache.lock() {
                if let Some(ref cached_setlist) = *cached {
                    let old_hash = setlist_structure_hash(cached_setlist);
                    if new_hash != old_hash {
                        // Structure changed - update cache and broadcast
                        *cached = Some(setlist.clone());
                        should_broadcast = true;
                        
                    } else {
                        // Structure unchanged - update cache but don't broadcast
                        *cached = Some(setlist.clone());
                        return;
                    }
                } else {
                    // No cached setlist - this is the first run, broadcast it
                    *cached = Some(setlist.clone());
                    should_broadcast = true;
                    
                }
            }
            
            if should_broadcast {
                // Determine active song index
                let active_song_index = determine_active_song_index(&setlist);
                
                // Broadcast setlist update with active song index
                broadcast_setlist(setlist, active_song_index);
            }
        }
        Err(e) => {
            tracing::warn!(
                error = %e,
                "Failed to poll setlist - build_setlist_from_open_projects() returned error"
            );
        }
    }
}

/// Set the action execution channel sender (called from main thread during initialization)
pub fn set_action_execution_sender(sender: std::sync::mpsc::Sender<reaper_medium::CommandId>) -> Result<(), std::sync::mpsc::Sender<reaper_medium::CommandId>> {
    ACTION_EXECUTION_SENDER.set(sender)
}

/// Request action execution from a background thread
/// This sends the command ID to the main thread via channel
pub fn request_action_execution(cmd_id: reaper_medium::CommandId) {
    if let Some(sender) = ACTION_EXECUTION_SENDER.get() {
        let _ = sender.send(cmd_id);
    }
}

/// Set the action execution receiver (called from main thread during initialization)
pub fn set_action_execution_receiver(receiver: std::sync::mpsc::Receiver<reaper_medium::CommandId>) -> Result<(), Mutex<std::sync::mpsc::Receiver<reaper_medium::CommandId>>> {
    ACTION_EXECUTION_RECEIVER.set(Mutex::new(receiver))
}

/// Process pending action execution requests (must be called from main thread)
/// This is called from the polling action handler which runs on the main thread
/// Returns the number of requests processed
pub fn process_action_execution_requests() -> u32 {
    use reaper_medium::ProjectContext;
    
    let mut count = 0u32;
    if let Some(receiver_mutex) = ACTION_EXECUTION_RECEIVER.get() {
        if let Ok(receiver) = receiver_mutex.lock() {
            // Process all pending requests
            while let Ok(cmd_id) = receiver.try_recv() {
                count += 1;
                if let Ok(reaper) = std::panic::catch_unwind(|| reaper_high::Reaper::get()) {
                    reaper.medium_reaper().main_on_command_ex(cmd_id, 0, ProjectContext::CurrentProject);
                }
            }
        }
    }
    count
}

/// Set the project switch channel sender (called from main thread during initialization)
pub fn set_project_switch_sender(sender: tokio::sync::mpsc::UnboundedSender<String>) -> Result<(), tokio::sync::mpsc::UnboundedSender<String>> {
    PROJECT_SWITCH_SENDER.set(sender)
}

/// Set the project switch receiver (called from main thread during initialization)
pub fn set_project_switch_receiver(receiver: std::sync::mpsc::Receiver<String>) -> Result<(), Mutex<std::sync::mpsc::Receiver<String>>> {
    PROJECT_SWITCH_RECEIVER.set(Mutex::new(receiver))
}

/// Request project switch from a background thread
/// This sends the project name to the main thread via channel
pub fn request_project_switch(project_name: String) {
    if let Some(sender) = PROJECT_SWITCH_SENDER.get() {
        let _ = sender.send(project_name);
    }
}

/// Process pending project switch requests (must be called from main thread)
/// Returns the number of requests processed
pub fn process_project_switch_requests() -> u32 {
    use reaper_high::Reaper as HighReaper;
    use reaper_medium::{CommandId, ProjectContext, ProjectRef};
    
    let mut count = 0u32;
    if let Some(receiver_mutex) = PROJECT_SWITCH_RECEIVER.get() {
        if let Ok(receiver) = receiver_mutex.lock() {
            // Process all pending requests
            while let Ok(project_name) = receiver.try_recv() {
                count += 1;
                tracing::info!(
                    project_name = %project_name,
                    request_number = count,
                    "Processing project switch request"
                );
                
                let reaper = HighReaper::get();
                let medium_reaper = reaper.medium_reaper();
                
                // Find tab index for this project name
                let mut target_tab_index: Option<usize> = None;
                for i in 0..128u32 {
                    if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 512) {
                        let tab_project_name = if let Some(file_path) = result.file_path.as_ref() {
                            file_path
                                .as_std_path()
                                .file_stem()
                                .and_then(|s| s.to_str())
                                .map(|s| s.to_string())
                                .unwrap_or_else(|| format!("Tab {}", i))
                        } else {
                            format!("Tab {}", i)
                        };
                        
                        // Skip FTS-ROUTING tab
                        let normalized_name = tab_project_name.to_uppercase().replace('_', "-");
                        if normalized_name == "FTS-ROUTING" {
                            continue;
                        }
                        
                        // Check if this matches the requested project name
                        if tab_project_name == project_name {
                            target_tab_index = Some(i as usize);
                            break;
                        }
                    } else {
                        break;
                    }
                }
                
                if let Some(tab_index) = target_tab_index {
                    tracing::info!(
                        project_name = %project_name,
                        target_tab_index = tab_index,
                        "Found target project tab"
                    );
                    
                    // Get current tab index
                    let current_project = reaper.current_project();
                    let current_project_raw = current_project.raw();
                    
                    let mut current_tab_index: Option<usize> = None;
                    for i in 0..128u32 {
                        if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 0) {
                            if result.project == current_project_raw {
                                current_tab_index = Some(i as usize);
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                    
                    if let Some(current_idx) = current_tab_index {
                        if current_idx == tab_index {
                            // Already on target tab
                            tracing::debug!(project_name = %project_name, tab_index, "Already on target project tab");
                            continue;
                        }
                        
                        tracing::info!(
                            project_name = %project_name,
                            current_tab_index = current_idx,
                            target_tab_index = tab_index,
                            "Switching from tab {} to tab {}",
                            current_idx,
                            tab_index
                        );
                        
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
                            tracing::warn!("Only one tab available, cannot switch");
                            continue;
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
                        
                        tracing::info!(
                            project_name = %project_name,
                            current_tab_index = current_idx,
                            target_tab_index = tab_index,
                            total_tabs = total_tab_count,
                            forward_dist = forward_dist,
                            backward_dist = backward_dist,
                            action_id = action_id,
                            distance = distance,
                            "Executing {} tab switch command(s) (action_id: {})",
                            distance,
                            action_id
                        );
                        
                        // Execute the action the required number of times
                        for i in 0..distance {
                            let cmd_id = CommandId::new(action_id);
                            medium_reaper.main_on_command_ex(cmd_id, 0, ProjectContext::CurrentProject);
                            tracing::debug!(
                                project_name = %project_name,
                                iteration = i + 1,
                                total = distance,
                                "Executed tab switch command {}/{}",
                                i + 1,
                                distance
                            );
                        }
                        
                        tracing::info!(
                            project_name = %project_name,
                            "✅ Completed project switch"
                        );
                        
                        tracing::info!(
                            project_name = %project_name,
                            from_tab = current_idx,
                            to_tab = tab_index,
                            distance,
                            "Switched to project tab"
                        );
                    } else {
                        tracing::warn!(project_name = %project_name, "Could not determine current tab index");
                    }
                } else {
                    tracing::warn!(project_name = %project_name, "Project not found in open tabs");
                }
            }
        }
    }
    count
}

/// Set the section seek channel sender (called from main thread during initialization)
pub fn set_section_seek_sender(sender: tokio::sync::mpsc::UnboundedSender<(String, String, String)>) -> Result<(), tokio::sync::mpsc::UnboundedSender<(String, String, String)>> {
    SECTION_SEEK_SENDER.set(sender)
}

/// Set the section seek receiver (called from main thread during initialization)
pub fn set_section_seek_receiver(receiver: std::sync::mpsc::Receiver<(String, String, String)>) -> Result<(), Mutex<std::sync::mpsc::Receiver<(String, String, String)>>> {
    SECTION_SEEK_RECEIVER.set(Mutex::new(receiver))
}

/// Request section seek from a background thread
/// This sends the project name, song name, and section name to the main thread via channel
pub fn request_section_seek(project_name: String, song_name: String, section_name: String) {
    if let Some(sender) = SECTION_SEEK_SENDER.get() {
        let _ = sender.send((project_name, song_name, section_name));
    }
}

/// Process pending section seek requests (must be called from main thread)
/// Returns the number of requests processed
pub fn process_section_seek_requests() -> u32 {
    use reaper_high::Reaper as HighReaper;
    use reaper_medium::{PositionInSeconds, ProjectRef, SetEditCurPosOptions, CommandId, ProjectContext};
    use crate::reaper_setlist::build_setlist_from_open_projects;
    
    let mut count = 0u32;
    if let Some(receiver_mutex) = SECTION_SEEK_RECEIVER.get() {
        if let Ok(receiver) = receiver_mutex.lock() {
            // Process all pending requests
            while let Ok((project_name, song_name, section_name)) = receiver.try_recv() {
                count += 1;
                
                let reaper = HighReaper::get();
                let medium_reaper = reaper.medium_reaper();
                
                // First, switch to the project if needed
                let current_project = reaper.current_project();
                let current_project_raw = current_project.raw();
                
                // Find tab index for this project name
                let mut target_tab_index: Option<usize> = None;
                for i in 0..128u32 {
                    if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 512) {
                        let tab_project_name = if let Some(file_path) = result.file_path.as_ref() {
                            file_path
                                .as_std_path()
                                .file_stem()
                                .and_then(|s| s.to_str())
                                .map(|s| s.to_string())
                                .unwrap_or_else(|| format!("Tab {}", i))
                        } else {
                            format!("Tab {}", i)
                        };
                        
                        // Skip FTS-ROUTING tab
                        let normalized_name = tab_project_name.to_uppercase().replace('_', "-");
                        if normalized_name == "FTS-ROUTING" {
                            continue;
                        }
                        
                        // Check if this matches the requested project name
                        if tab_project_name == project_name {
                            target_tab_index = Some(i as usize);
                            break;
                        }
                    } else {
                        break;
                    }
                }
                
                if let Some(tab_index) = target_tab_index {
                    // Check if we need to switch projects
                    let mut current_tab_index: Option<usize> = None;
                    for i in 0..128u32 {
                        if let Some(result) = medium_reaper.enum_projects(ProjectRef::Tab(i), 0) {
                            if result.project == current_project_raw {
                                current_tab_index = Some(i as usize);
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                    
                    // Switch to project if needed (reuse logic from process_project_switch_requests)
                    if let Some(current_idx) = current_tab_index {
                        if current_idx != tab_index {
                            // Need to switch projects first
                            let mut total_tab_count = 0;
                            for i in 0..128u32 {
                                if medium_reaper.enum_projects(ProjectRef::Tab(i), 0).is_some() {
                                    total_tab_count += 1;
                                } else {
                                    break;
                                }
                            }
                            
                            if total_tab_count > 1 {
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
                                
                                let (action_id, distance) = if forward_dist <= backward_dist {
                                    (40861u32, forward_dist)
                                } else {
                                    (40862u32, backward_dist)
                                };
                                
                                for _ in 0..distance {
                                    let cmd_id = CommandId::new(action_id);
                                    medium_reaper.main_on_command_ex(cmd_id, 0, ProjectContext::CurrentProject);
                                }
                            }
                        }
                    }
                    
                    // Now find the section and seek to it
                    if let Ok(setlist) = build_setlist_from_open_projects(None) {
                        for song in &setlist.songs {
                            // Check if this song matches
                            if song.name != song_name {
                                continue;
                            }
                            
                            // Check if song belongs to this project
                            let song_project_name = song.metadata.get("project_name")
                                .or_else(|| song.metadata.get("Project"))
                                .or_else(|| song.metadata.get("project"));
                            
                            if song_project_name.map(|s| s.as_str()) != Some(&project_name) {
                                continue;
                            }
                            
                            // Find the section
                            for section in &song.sections {
                                if section.name == section_name {
                                    // Found the section! Move edit cursor to its start position
                                    let target_project = reaper.current_project();
                                    let start_pos_seconds = section.start_position.time.to_seconds();
                                    
                                    // Create PositionInSeconds (unwrap is safe here as we're using valid time values)
                                    if let Ok(start_pos) = PositionInSeconds::new(start_pos_seconds) {
                                        target_project.set_edit_cursor_position(
                                            start_pos,
                                            SetEditCurPosOptions {
                                                move_view: false,
                                                seek_play: false,
                                            }
                                        );
                                    } else {
                                        tracing::warn!(
                                            project_name = %project_name,
                                            song_name = %song_name,
                                            section_name = %section_name,
                                            position = start_pos_seconds,
                                            "Failed to create PositionInSeconds for section seek"
                                        );
                                    }
                                    
                                    tracing::info!(
                                        project_name = %project_name,
                                        song_name = %song_name,
                                        section_name = %section_name,
                                        position = start_pos_seconds,
                                        "Moved edit cursor to section start"
                                    );
                                    break;
                                }
                            }
                        }
                    } else {
                        tracing::warn!(project_name = %project_name, "Failed to build setlist for section seek");
                    }
                } else {
                    tracing::warn!(project_name = %project_name, "Project not found in open tabs for section seek");
                }
            }
        }
    }
    count
}

