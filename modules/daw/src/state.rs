//! Composed state types for DAW domain
//!
//! This module provides convenience types for reading combined state
//! from transport and tracks modules. Each domain manages its own state,
//! and these types provide a convenient way to read them together.

use crate::transport::Transport;
use crate::tracks::Track;
use std::collections::HashMap;
use uuid::Uuid;

/// Composed state for convenience (combines transport and tracks state)
/// 
/// This is useful for clients that need to read both transport and tracks state together.
/// Individual services manage their own state separately.
#[derive(Debug, Clone)]
pub struct DawReactiveState {
    /// Transport state for each project (project_id -> transport)
    pub transport: HashMap<Uuid, Transport>,
    
    /// Tracks for each project (project_id -> tracks)
    pub tracks: HashMap<Uuid, Vec<Track>>,
    
    /// Currently active project ID
    pub active_project_id: Option<Uuid>,
}

impl Default for DawReactiveState {
    fn default() -> Self {
        Self {
            transport: HashMap::new(),
            tracks: HashMap::new(),
            active_project_id: None,
        }
    }
}

