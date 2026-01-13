//! Visibility Manager REAPER Extension Module
//!
//! Provides actions for managing track visibility using dynamic classification.

mod actions;
mod state;

pub use actions::actions;
pub use actions::register_visibility_manager_actions;
