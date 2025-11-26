//! Core implementations of traits from `daw` and `setlist` crates
//!
//! These modules implement the trait interfaces defined in the external crates
//! for REAPER-specific functionality.

pub mod setlist_state_provider;
pub mod setlist_command_handler;

pub use setlist_state_provider::ReaperSetlistStateProvider;
pub use setlist_command_handler::ReaperSetlistCommandHandler;

