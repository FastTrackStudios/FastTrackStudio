//! Shared module
//!
//! Contains shared traits and utilities used across template, group, and visibility_manager.

pub mod config_loader;
pub mod group_mode;

pub use config_loader::*;
pub use group_mode::*;
