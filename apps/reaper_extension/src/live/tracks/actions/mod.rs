//! Live Tracks Actions
//!
//! Defines and registers REAPER actions for the Live Tracks setlist management system.
//! Uses the shared action registration infrastructure.

pub mod navigation;
mod static_actions;
pub mod zoom;

// Re-export navigation functions
pub use navigation::go_to_song;

// Re-export for external use
pub use static_actions::register_all_actions;

// Compatibility re-export (empty for now)
