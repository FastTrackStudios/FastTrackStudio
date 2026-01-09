//! REAPER implementation of SetlistCommandHandler
//!
//! Handles commands from the stream API by delegating to services.

use crate::setlist::infra::stream::LyricsState;
use crate::setlist::{NavigationCommand, SetlistCommandHandler, TransportCommand};
use std::sync::Arc;

/// REAPER implementation of SetlistCommandHandler
///
/// This is a placeholder that will be implemented by the reaper_extension services.
/// The actual implementation will be provided by the services in reaper_extension.
pub struct ReaperSetlistCommandHandler;

impl ReaperSetlistCommandHandler {
    /// Create a new command handler
    ///
    /// Note: This is a placeholder. The actual implementation will be in reaper_extension
    /// that uses the trait-based services.
    pub fn new() -> Self {
        Self
    }
}

// Note: The actual trait implementation will be in reaper_extension/src/core/setlist_command_handler.rs
// which will use the trait-based services from this module.
