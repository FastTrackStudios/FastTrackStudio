//! DAW Reactive REAPER Implementations
//!
//! This module contains REAPER-specific implementations of the DAW reactive services
//! (transport, tracks). These implementations are in the `fts` crate because they
//! need access to both:
//! - `daw` types (Transport, Track, etc.)
//! - `fts::setlist::infra::reaper` functions (for reading from REAPER)
//!
//! This module is only available when the `reaper` feature is enabled.

#[cfg(feature = "reaper")]
use reaper_high::Project as ReaperProject;

#[cfg(feature = "reaper")]
use daw::project::Project;

#[cfg(feature = "reaper")]
use daw::transport::Transport;

/// Trait for providing setlist context to the REAPER reactive services
///
/// This abstraction allows the transport and track services to look up project information
/// from the setlist without depending directly on the setlist service implementation.
#[cfg(feature = "reaper")]
pub trait SetlistProvider: Send + Sync {
    /// Get a Project<Transport> from a REAPER project
    fn get_project_from_reaper(&self, reaper_project: ReaperProject) -> Option<Project<Transport>>;
}

#[cfg(feature = "reaper")]
pub mod transport;

#[cfg(feature = "reaper")]
pub mod tracks;

#[cfg(feature = "reaper")]
pub mod tracks_command;

#[cfg(feature = "reaper")]
pub mod logging;

// Re-export main types at module level for convenience
#[cfg(feature = "reaper")]
pub use transport::ReaperTransportReactiveService;

#[cfg(feature = "reaper")]
pub use tracks::ReaperTrackReactiveService;
