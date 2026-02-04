//! Semantic color mappings for audio production.
//!
//! This module provides meaningful color assignments for:
//! - Instrument groups and subgroups (drums, bass, guitars, vocals, etc.)
//! - Song sections (verse, chorus, bridge, etc.)
//! - Syntax highlighting for chord charts

pub mod instruments;
pub mod sections;

pub use instruments::*;
pub use sections::*;
