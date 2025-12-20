//! Composed state types for DAW domain
//!
//! This module provides convenience types for reading combined state
//! from transport and tracks modules. Each domain manages its own state,
//! and these types provide a convenient way to read them together.

use crate::transport::Transport;
use crate::tracks::Track;
use std::collections::HashMap;

/// Composed state for convenience (combines transport and tracks state)
/// 
/// This is useful for clients that need to read both transport and tracks state together.
/// Individual services manage their own state separately.
#[derive(Debug, Clone)]
#[derive(Default)]
pub struct DawReactiveState {
    /// Transport state for each project (project_name -> transport)
    pub transport: HashMap<String, Transport>,
    
    /// Tracks for each project (project_name -> tracks)
    pub tracks: HashMap<String, Vec<Track>>,
    
    /// Currently active project name
    pub active_project_id: Option<String>,
}


