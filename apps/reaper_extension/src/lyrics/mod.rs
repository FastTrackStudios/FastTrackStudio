//! REAPER integration for lyrics functionality
//!
//! This module provides REAPER-specific actions and re-exports implementations from fts.
//! The actual read/write/stream implementations are in `fts::lyrics::infra::reaper`.

mod actions;
#[cfg(debug_assertions)]
mod dev_actions;

// Re-export implementations from fts module
pub use fts::lyrics::infra::reaper::read::{
    find_folder_tracks, find_track_by_name, read_lyrics_from_reaper,
};
pub use fts::lyrics::infra::reaper::stream::ReaperLyricsCommandHandler;
pub use fts::lyrics::infra::reaper::write::{
    create_text_items_from_lyrics, update_lyrics_in_reaper,
};

pub use actions::register_lyrics_actions;
#[cfg(debug_assertions)]
pub use dev_actions::register_lyrics_dev_actions;
