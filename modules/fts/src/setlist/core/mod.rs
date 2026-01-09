//! Core domain types for setlist functionality
//!
//! This module contains the fundamental domain types and traits for managing
//! setlists, songs, and sections in a DAW environment.

mod api;
mod error;
pub mod section;
mod setlist;
mod song;

pub use api::*;
pub use error::*;
pub use section::{
    Section, SectionExt, SectionType, SectionTypeExt, section_from_seconds,
    section_from_seconds_with_tempo, section_new, section_with_id, validate,
};
pub use setlist::*;
pub use song::*;

// Re-export Color from setlist module
pub use setlist::Color;
