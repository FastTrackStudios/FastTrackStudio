//! Integration helpers for using state management in components
//!
//! This module provides utilities for setting up state management
//! that works in both local and server modes.

use dioxus::prelude::*;
use dioxus_fullstack::use_websocket;
use setlist::Setlist;
use std::collections::HashMap;
use crate::state::messages::{ClientEvent, ServerEvent};
use crate::state::server::{handle_server_message, setlist_ws};
use crate::state::local::*;
use crate::state::StateManagerMode;

/// Initialize state management based on mode
/// 
/// This sets up signals and WebSocket connection (if server mode),
/// and returns a StateManager instance.
pub fn initialize_state_manager(
    initial_setlist: Setlist,
    mode: StateManagerMode,
) -> (
    Signal<Setlist>,
    Signal<HashMap<String, f64>>,
    Signal<HashMap<String, f64>>,
    Signal<Option<usize>>,
    Signal<Option<usize>>,
    Signal<bool>,
    Signal<bool>,
    Signal<bool>,
) {
    // Create all signals
    let setlist = use_signal(|| initial_setlist);
    let transport_positions = use_signal(|| HashMap::<String, f64>::new());
    let song_positions = use_signal(|| HashMap::<String, f64>::new());
    let current_song_index = use_signal(|| Some(0));
    let current_section_index = use_signal(|| Some(0));
    let is_playing = use_signal(|| false);
    let is_looping = use_signal(|| false);
    let connection_status = use_signal(|| matches!(mode, StateManagerMode::Server { .. }));
    
    // Initialize local state
    use_effect(move || {
        let setlist_ref = setlist.read();
        let mut project_positions = HashMap::new();
        let mut song_pos = HashMap::new();
        
        for (idx, song) in setlist_ref.songs.iter().enumerate() {
            use crate::utils::get_project_name;
            let project_name = get_project_name(song).unwrap_or_else(|| "default".to_string());
            if !project_positions.contains_key(&project_name) {
                if let Some(section) = song.sections.first() {
                    project_positions.insert(project_name.clone(), section.start_seconds());
                } else {
                    project_positions.insert(project_name.clone(), 0.0);
                }
            }
            
            let song_start = if song.effective_start() > 0.0 {
                song.effective_start()
            } else {
                song.sections.first()
                    .map(|s| s.start_seconds())
                    .unwrap_or(0.0)
            };
            song_pos.insert(idx.to_string(), song_start);
        }
        
        transport_positions.set(project_positions);
        song_positions.set(song_pos);
    });
    
    // Setup WebSocket connection if in server mode
    // When connecting to desktop app, pass None for external_server_url (desktop manages state)
    // When desktop app bridges to external server, it would pass the external URL
    let socket = if let StateManagerMode::Server { url } = mode {
        // Check if URL is "localhost" or similar - means connect to desktop app
        // Otherwise, it's an external server URL
        let is_desktop_app = url.starts_with("localhost") || url.starts_with("127.0.0.1");
        let external_url = if is_desktop_app {
            None // Connect to desktop app, it manages state
        } else {
            Some(url.clone()) // Desktop app bridges to external server
        };
        
        let socket = use_websocket(move || setlist_ws(external_url.clone(), dioxus_fullstack::WebSocketOptions::new()));
        
        // Handle incoming messages from server
        {
            let mut socket_clone = socket.clone();
            let setlist = setlist.clone();
            let transport_positions = transport_positions.clone();
            let song_positions = song_positions.clone();
            let current_song_index = current_song_index.clone();
            let current_section_index = current_section_index.clone();
            let is_playing = is_playing.clone();
            let connection_status = connection_status.clone();
            
            use_future(move || {
                let mut socket = socket_clone.clone();
                let setlist = setlist.clone();
                let transport_positions = transport_positions.clone();
                let song_positions = song_positions.clone();
                let current_song_index = current_song_index.clone();
                let current_section_index = current_section_index.clone();
                let is_playing = is_playing.clone();
                let connection_status = connection_status.clone();
                
                async move {
                    while let Ok(msg) = socket.recv().await {
                        handle_server_message(
                            msg,
                            setlist,
                            transport_positions,
                            song_positions,
                            current_song_index,
                            current_section_index,
                            is_playing,
                        );
                        connection_status.set(true);
                    }
                    connection_status.set(false);
                }
            });
        }
        
        // Request initial setlist
        {
            let mut socket_clone = socket.clone();
            use_future(move || {
                let mut socket = socket_clone.clone();
                async move {
                    let _ = socket.send(ClientEvent::RequestSetlist).await;
                }
            });
        }
        
        Some(socket)
    } else {
        None
    };
    
    (
        setlist,
        transport_positions,
        song_positions,
        current_song_index,
        current_section_index,
        is_playing,
        is_looping,
        connection_status,
    )
}

/// Execute a navigation action based on mode
pub fn execute_navigation_action(
    action: NavigationAction,
    mode: StateManagerMode,
    setlist: Signal<Setlist>,
    transport_positions: Signal<HashMap<String, f64>>,
    song_positions: Signal<HashMap<String, f64>>,
    current_song_index: Signal<Option<usize>>,
    current_section_index: Signal<Option<usize>>,
    socket: Option<&dioxus_fullstack::Websocket<ClientEvent, ServerEvent>>,
) {
    match mode {
        StateManagerMode::Local => {
            // Execute locally
            match action {
                NavigationAction::NextSection => {
                    navigate_to_next_section_local(
                        setlist,
                        transport_positions,
                        song_positions,
                        current_song_index,
                        current_section_index,
                    );
                }
                NavigationAction::PreviousSection => {
                    navigate_to_previous_section_local(
                        setlist,
                        transport_positions,
                        song_positions,
                        current_song_index,
                        current_section_index,
                    );
                }
                NavigationAction::NextSong => {
                    navigate_to_next_song_local(
                        setlist,
                        transport_positions,
                        song_positions,
                        current_song_index,
                        current_section_index,
                    );
                }
                NavigationAction::PreviousSong => {
                    navigate_to_previous_song_local(
                        setlist,
                        transport_positions,
                        song_positions,
                        current_song_index,
                        current_section_index,
                    );
                }
                NavigationAction::SeekToSection { song_idx, section_idx } => {
                    seek_to_section_local(
                        setlist,
                        transport_positions,
                        song_positions,
                        current_song_index,
                        current_section_index,
                        song_idx,
                        section_idx,
                    );
                }
            }
        }
        StateManagerMode::Server { .. } => {
            // Send command to server
            if let Some(socket) = socket {
                let event = match action {
                    NavigationAction::NextSection => ClientEvent::NavigateNextSection,
                    NavigationAction::PreviousSection => ClientEvent::NavigatePreviousSection,
                    NavigationAction::NextSong => ClientEvent::NavigateNextSong,
                    NavigationAction::PreviousSong => ClientEvent::NavigatePreviousSong,
                    NavigationAction::SeekToSection { .. } => {
                        // Would need song/section names for this
                        return;
                    }
                };
                // Note: This would need to be in an async context
                // In practice, you'd use use_future or similar
            }
        }
    }
}

/// Navigation actions
#[derive(Debug, Clone)]
pub enum NavigationAction {
    NextSection,
    PreviousSection,
    NextSong,
    PreviousSong,
    SeekToSection { song_idx: usize, section_idx: usize },
}

