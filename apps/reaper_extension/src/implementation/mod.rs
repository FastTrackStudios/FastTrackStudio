//! REAPER-specific implementations
//!
//! This module contains REAPER-specific wrapper code.
//! Most implementations have been moved to the `fts` module behind feature flags.
//! See `fts::setlist::infra::reaper` and `fts::lyrics::infra::reaper` for the moved implementations.

pub mod project;

// Re-export project wrapper for convenience
pub use project::create_reaper_project_wrapper;
