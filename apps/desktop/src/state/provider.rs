//! Centralized state provider
//!
//! This module provides a single source of truth for all application state.
//! The provider can be swapped to use different backends (local, server, reaper, phone, etc.)

use dioxus::prelude::*;
use setlist::Setlist;
use std::collections::HashMap;
use crate::state::StateManagerMode;

/// Application state - all state in one place
#[derive(Clone)]
pub struct AppState {
    pub setlist: Signal<Setlist>,
    pub transport_positions: Signal<HashMap<String, f64>>,
    pub song_positions: Signal<HashMap<String, f64>>,
    pub current_song_index: Signal<Option<usize>>,
    pub current_section_index: Signal<Option<usize>>,
    pub is_playing: Signal<bool>,
    pub is_looping: Signal<bool>,
    pub connection_status: Signal<bool>,
    pub mode: Signal<StateManagerMode>,
}

impl AppState {
}

/// State provider context
#[derive(Clone)]
pub struct StateContext {
    pub state: AppState,
}

/// Provider component that wraps the app and provides state via context
#[component]
pub fn StateProvider(
    initial_setlist: Setlist,
    initial_mode: StateManagerMode,
    children: Element,
) -> Element {
    // Create all signals
    let setlist = use_signal(|| initial_setlist);
    let transport_positions = use_signal(|| HashMap::<String, f64>::new());
    let song_positions = use_signal(|| HashMap::<String, f64>::new());
    let current_song_index = use_signal(|| Some(0));
    let current_section_index = use_signal(|| Some(0));
    let is_playing = use_signal(|| false);
    let is_looping = use_signal(|| false);
    let connection_status = use_signal(|| false); // Always false for local-only mode
    let mode = use_signal(|| initial_mode);
    
    let state = AppState {
        setlist,
        transport_positions,
        song_positions,
        current_song_index,
        current_section_index,
        is_playing,
        is_looping,
        connection_status,
        mode,
    };
    
    // Provide state to children via context
    use_context_provider(|| StateContext { state: state.clone() });
    
    // Track if we've initialized positions (to prevent overwriting on restore)
    let positions_initialized = use_signal(|| false);
    
    // Initialize positions when setlist changes (but only once, and only if positions are empty)
    // This prevents overwriting positions when restoring from snapshot
    {
        let setlist = setlist.clone();
        let mut transport_positions = transport_positions.clone();
        let mut song_positions = song_positions.clone();
        let mut mode = mode.clone();
        let mut positions_initialized = positions_initialized.clone();
        
        use_effect(move || {
            // Only initialize positions if:
            // 1. We're in local mode (server mode gets positions from server)
            // 2. We haven't initialized yet OR positions are empty
            // 3. Setlist has songs
            if matches!(mode(), StateManagerMode::Local) {
                let setlist_ref = setlist.read();
                if !setlist_ref.songs.is_empty() {
                    let transport_empty = transport_positions.read().is_empty();
                    let song_empty = song_positions.read().is_empty();
                    
                    if !positions_initialized() && (transport_empty || song_empty) {
                        use crate::utils::get_project_name;
                        
                        let mut project_positions = HashMap::new();
                        let mut song_pos = HashMap::new();
                        
                        for (idx, song) in setlist_ref.songs.iter().enumerate() {
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
                        
                        // Only set if empty to preserve existing positions
                        if transport_empty {
                            transport_positions.set(project_positions);
                        }
                        if song_empty {
                            song_positions.set(song_pos);
                        }
                        
                        positions_initialized.set(true);
                    }
                }
            }
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
    
    rsx! {
        {children}
    }
}

/// Hook to access state from context
pub fn use_app_state() -> AppState {
    use_context::<StateContext>().state
}

