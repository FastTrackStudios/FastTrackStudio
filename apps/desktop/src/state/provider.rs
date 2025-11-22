//! Centralized state provider
//!
//! This module provides a single source of truth for all application state.
//! The provider can be swapped to use different backends (local, server, reaper, phone, etc.)

use dioxus::prelude::*;
use setlist::{Setlist, Song};
use marker_region::Marker;
use std::collections::HashMap;
use std::sync::Arc;
use crate::state::StateManagerMode;
#[cfg(not(target_arch = "wasm32"))]
use crate::reaper_connection::{ReaperConnection, ReaperCommandSender};
#[cfg(not(target_arch = "wasm32"))]
use peer_2_peer::reaper_api::{ReaperStateUpdate, SetlistState, TransportState};
use tracing::{info, warn};

/// Application state - all state in one place
#[derive(Clone)]
pub struct AppState {
    pub setlist: Signal<Setlist>,
    pub is_playing: Signal<bool>,
    pub is_looping: Signal<bool>,
    pub transport_positions: Signal<HashMap<String, f64>>,
    pub song_positions: Signal<HashMap<String, f64>>,
    pub current_song_index: Signal<Option<usize>>,
    pub current_section_index: Signal<Option<usize>>,
    pub mode: Signal<StateManagerMode>,
    pub positions_initialized: Signal<bool>,
    pub connection_status: Signal<bool>,
}

/// Context provider for application state
#[component]
pub fn StateProvider(children: Element) -> Element {
    let setlist = use_signal(|| Setlist::new("Default Setlist".to_string()).unwrap());
    let is_playing = use_signal(|| false);
    let is_looping = use_signal(|| false);
    let transport_positions = use_signal(|| HashMap::new());
    let song_positions = use_signal(|| HashMap::new());
    let current_song_index = use_signal(|| None);
    let current_section_index = use_signal(|| None);
    let mode = use_signal(|| StateManagerMode::Reaper);
    let positions_initialized = use_signal(|| false);
    let connection_status = use_signal(|| false);

    let state = AppState {
        setlist,
        is_playing,
        is_looping,
        transport_positions,
        song_positions,
        current_song_index,
        current_section_index,
        mode,
        positions_initialized,
        connection_status,
    };

    // Connect to REAPER extension via irpc
    #[cfg(not(target_arch = "wasm32"))]
    {
        let setlist_signal = setlist.clone();
        let is_playing_signal = is_playing.clone();
        let transport_positions_signal = transport_positions.clone();
        let current_song_index_signal = current_song_index.clone();
        let current_section_index_signal = current_section_index.clone();
        let mode_signal = mode.clone();
        let connection_status_signal = connection_status.clone();
        
        use_effect(move || {
            let (reaper_conn, mut connected_rx, command_sender) = ReaperConnection::new();
            
            // Watch connection status changes
            let mut connection_status_for_watch = connection_status_signal.clone();
            spawn(async move {
                while connected_rx.changed().await.is_ok() {
                    let is_connected = *connected_rx.borrow();
                    connection_status_for_watch.set(is_connected);
                }
            });
            
            // Set up handler for state updates
            // This runs in a tokio task, so we use a channel to forward updates to Dioxus
            let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel::<ReaperStateUpdate>();
            
            // Set handler asynchronously
            let reaper_conn_for_handler = reaper_conn.clone();
            let handler_tx = tx.clone();
            spawn(async move {
                reaper_conn_for_handler.set_state_update_handler(Arc::new(move |update| {
                    if let Err(e) = handler_tx.send(update) {
                        warn!("[DESKTOP] Failed to forward state update to channel: {}", e);
                    }
                })).await;
            });
            
            // Start connection with automatic reconnection
            // The start() method spawns its own retry loop
            spawn(async move {
                // Start immediately - no delay needed
                if let Err(e) = reaper_conn.start().await {
                    error!("[DESKTOP] Failed to start connection manager: {}", e);
                }
            });
            
            // Spawn task to process state updates and update signals
            let mut setlist_for_spawn = setlist_signal.clone();
            let mut is_playing_for_spawn = is_playing_signal.clone();
            let mut transport_positions_for_spawn = transport_positions_signal.clone();
            let mut current_song_index_for_spawn = current_song_index_signal.clone();
            let mut current_section_index_for_spawn = current_section_index_signal.clone();
            let mode_for_spawn = mode_signal.clone();
            
            spawn(async move {
                let mut processed_count = 0u64;
                while let Some(update) = rx.recv().await {
                    processed_count += 1;
                    let current_mode = mode_for_spawn();
                    
                    // Only process if we're in REAPER mode
                    if !matches!(current_mode, StateManagerMode::Reaper) {
                        continue;
                    }
                    
                    info!("[DESKTOP] Processing state update #{}", processed_count);
                    
                    match update {
                        ReaperStateUpdate::Heartbeat => {
                            info!("[DESKTOP] 💓 Received heartbeat in provider");
                            // Heartbeat received - connection is alive
                        }
                        ReaperStateUpdate::SetlistState(state) => {
                            info!("[DESKTOP] Received SetlistState update");
                            
                            // Deserialize setlist from JSON
                            match state.deserialize_setlist() {
                                Ok(new_setlist) => {
                                    info!("[DESKTOP] Parsed setlist: {} songs", new_setlist.songs.len());
                                    setlist_for_spawn.set(new_setlist);
                                    
                                    // Update active song/section indices
                                    current_song_index_for_spawn.set(Some(state.active_song_index));
                                    current_section_index_for_spawn.set(Some(state.active_section_index));
                                    
                                    // Update transport position if we have progress info
                                    if let Ok(setlist_ref) = setlist_for_spawn.try_read() {
                                        if let Some(song) = setlist_ref.songs.get(state.active_song_index) {
                                            if let Some(progress) = state.active_song_progress {
                                                let song_start = song.start_marker.as_ref()
                                                    .map(|m| m.position.time.to_seconds())
                                                    .unwrap_or(0.0);
                                                let song_end = song.song_end_marker.as_ref()
                                                    .map(|m| m.position.time.to_seconds())
                                                    .unwrap_or(0.0);
                                                let position_seconds = song_start + (song_end - song_start) * progress;
                                            
                                                if let Some(project_name) = crate::utils::get_project_name(song) {
                                                    let mut positions = transport_positions_for_spawn.read().clone();
                                                    positions.insert(project_name.clone(), position_seconds);
                                                    transport_positions_for_spawn.set(positions);
                            }
                                            }
                                        }
                                    }
                                }
                                Err(e) => {
                                    warn!("[DESKTOP] Failed to deserialize Setlist: {}", e);
                                }
                            }
                        }
                        ReaperStateUpdate::TransportState(state) => {
                            info!("[DESKTOP] Received TransportState update - playing: {}, position: {:.2}s", 
                                state.is_playing, state.position_seconds);
                            
                            is_playing_for_spawn.set(state.is_playing || state.is_recording);
                            
                            // Update transport position
                            if let Some(project_name) = &state.active_project_name {
                                        let mut positions = transport_positions_for_spawn.read().clone();
                                positions.insert(project_name.clone(), state.position_seconds);
                                        transport_positions_for_spawn.set(positions);
                            }
                        }
                    }
                }
            });
        });
    }
    
    // Reset initialization flag when restoring from snapshot
    {
        let mut mode = mode.clone();
        let mut positions_initialized = positions_initialized.clone();
        
        use_effect(move || {
            if matches!(mode(), StateManagerMode::Local) {
                // When switching to local, don't reset the flag - we want to preserve restored positions
                // The flag will prevent re-initialization
            } else {
                // When switching to server, reset flag so we can initialize again when switching back
                // But only if we're actually switching (not on initial mount)
                // Actually, let's not reset it - we want to preserve the initialization state
            }
        });
    }
    
    // Server-side features removed - will be re-added incrementally
    // For now, we only support local state management
    
    // Provide state context to children
    use_context_provider(|| StateContext {
        state: AppState {
            setlist,
            is_playing,
            is_looping,
            transport_positions,
            song_positions,
            current_song_index,
            current_section_index,
            mode,
            positions_initialized,
            connection_status,
        },
    });
    
    rsx! {
        {children}
    }
}

/// Hook to access state from context
pub fn use_app_state() -> AppState {
    use_context::<StateContext>().state
}

/// State context for dependency injection
#[derive(Clone)]
pub struct StateContext {
    pub state: AppState,
}
