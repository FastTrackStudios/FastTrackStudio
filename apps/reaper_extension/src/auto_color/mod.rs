//! Auto Color REAPER Extension Module
//!
//! Provides automatic track coloring based on dynamic-template classification.
//! Similar to SWS Auto Color but uses intelligent instrument classification
//! instead of simple pattern matching.

mod actions;
mod state;

pub use actions::actions;
pub use actions::register_auto_color_actions;
