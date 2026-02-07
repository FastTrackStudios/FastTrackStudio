//! Vocal module presets — pre-built [`ModulePreset`]s for the FTS vocal signal chain.
//!
//! Each submodule provides factory functions that return configured
//! [`ModulePreset`] instances with blocks from [`crate::defaults::blocks`]
//! and named snapshots for parameter variations.

pub mod rescue;
pub mod correction;
pub mod tonal;
pub mod modulation;
pub mod sends;
