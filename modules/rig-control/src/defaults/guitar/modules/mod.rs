//! Guitar module presets — pre-built [`ModulePreset`]s for the FTS guitar signal chain.
//!
//! Each submodule provides factory functions that return configured
//! [`ModulePreset`] instances with blocks from [`crate::defaults::blocks`]
//! and named snapshots for parameter variations.

pub mod source;
pub mod dynamics;
pub mod special;
pub mod drive;
pub mod prefx;
pub mod amp;
pub mod modulation;
pub mod time;
pub mod motion;
pub mod master;
