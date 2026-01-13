//! Dynamic-Template REAPER Extension Module
//!
//! Provides actions for organizing DAW items into tracks using the dynamic-template
//! library's hierarchical sorting algorithm.

mod actions;

pub use actions::actions;
pub use actions::register_dynamic_template_actions;
