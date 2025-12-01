//! State management abstraction layer
//!
//! Provides a unified interface for managing application state that can work
//! either locally (client-managed) or server-side (stateless client).
//!
//! The key insight: Signals are always created in component context, but the
//! operations on them can be abstracted. In local mode, operations update signals
//! directly. In server mode, operations send commands to server, and signals are
//! updated from server messages.

use dioxus::prelude::*;
use fts::fts::setlist::Setlist;
use std::collections::HashMap;

pub mod local;
pub mod messages;
pub mod provider;

/// State manager - wraps signals and provides operations
/// 
/// Signals are created in component context and passed to the manager.
/// The manager provides operations that either:
/// - Update signals directly (local mode)
/// - Send commands to server and update signals from server responses (server mode)
pub struct StateManager {
    // Signals (always present, regardless of mode)
    pub setlist: Signal<Setlist>,
    pub transport_positions: Signal<HashMap<String, f64>>,
    pub song_positions: Signal<HashMap<String, f64>>,
    pub current_song_index: Signal<Option<usize>>,
    pub current_section_index: Signal<Option<usize>>,
    pub is_playing: Signal<bool>,
    pub is_looping: Signal<bool>,
    pub connection_status: Signal<bool>,
    
    // Mode
    pub mode: StateManagerMode,
}

/// State manager mode
#[derive(Clone, Debug, PartialEq)]
pub enum StateManagerMode {
    /// Local mode - all state managed client-side
    Local,
    /// Server mode - state managed server-side, client is stateless
    Server { url: String },
    /// REAPER mode - state managed by REAPER extension, desktop app receives updates
    Reaper,
}

impl StateManager {
    /// Create a new local state manager
    pub fn new_local(
        setlist: Signal<Setlist>,
        transport_positions: Signal<HashMap<String, f64>>,
        song_positions: Signal<HashMap<String, f64>>,
        current_song_index: Signal<Option<usize>>,
        current_section_index: Signal<Option<usize>>,
        is_playing: Signal<bool>,
        is_looping: Signal<bool>,
        connection_status: Signal<bool>,
    ) -> Self {
        Self {
            setlist,
            transport_positions,
            song_positions,
            current_song_index,
            current_section_index,
            is_playing,
            is_looping,
            connection_status,
            mode: StateManagerMode::Local,
        }
    }
    
    /// Create a new server state manager
    pub fn new_server(
        setlist: Signal<Setlist>,
        transport_positions: Signal<HashMap<String, f64>>,
        song_positions: Signal<HashMap<String, f64>>,
        current_song_index: Signal<Option<usize>>,
        current_section_index: Signal<Option<usize>>,
        is_playing: Signal<bool>,
        is_looping: Signal<bool>,
        connection_status: Signal<bool>,
        url: String,
    ) -> Self {
        Self {
            setlist,
            transport_positions,
            song_positions,
            current_song_index,
            current_section_index,
            is_playing,
            is_looping,
            connection_status,
            mode: StateManagerMode::Server { url },
        }
    }
    
    /// Check if this is server-managed
    pub fn is_server_managed(&self) -> bool {
        matches!(self.mode, StateManagerMode::Server { .. })
    }
}

