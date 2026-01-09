//! Infrastructure code
//!
//! Infrastructure modules handle REAPER-specific setup and integration.

pub mod action_registry;
pub mod change_detection;
pub mod color_utils;
pub mod iroh_server;
pub mod menu;
pub mod reactive_app_state;
pub mod reactive_logger;
pub mod reactive_polling;
pub mod task_support;
pub mod timer;
pub mod tracing_config;

// Re-export REAPER reactive services from fts::daw_reactive
// These are the canonical implementations that live in the fts module
pub use fts::daw_reactive::SetlistProvider;
pub use fts::daw_reactive::logging as formatted_logging;
pub use fts::daw_reactive::tracks::ReaperTrackReactiveService;
pub use fts::daw_reactive::tracks_command::{
    DefaultProjectProvider, ProjectProvider, ReaperTrackCommandHandler, TrackServiceAccessor,
    TrackServiceWrapper, init_track_reactive_service,
};
pub use fts::daw_reactive::transport::ReaperTransportReactiveService;

// Re-export REAPER lyrics reactive service (if lyrics feature is enabled)
#[cfg(feature = "lyrics")]
pub use fts::lyrics::reactive::reaper::ReaperLyricsReactiveService;

// Re-export REAPER chords reactive service (if keyflow feature is enabled)
#[cfg(feature = "keyflow")]
pub use fts::chords::reactive::reaper::ReaperChordsReactiveService;
