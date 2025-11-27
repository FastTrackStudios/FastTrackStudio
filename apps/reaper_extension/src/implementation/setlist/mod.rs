//! REAPER Setlist Building
//!
//! Builds setlists from open REAPER projects, reading markers and regions from each project.
//!
//! This module is organized into submodules:
//! - `stats` - Build statistics tracking
//! - `tempo` - Tempo/time signature reading
//! - `parser` - Song name and section type parsing
//! - `region_finder` - Region and marker finding utilities
//! - `song_builder` - Song building logic
//! - `section_builder` - Section building logic

mod stats;
mod tempo;
mod parser;
mod region_finder;
mod song_builder;
mod section_builder;

pub use song_builder::{
    build_song_from_current_project,
    build_setlist_from_open_projects,
};

