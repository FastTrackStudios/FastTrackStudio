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

pub use stats::{record_section_created, record_song_built, record_song_reused, flush_build_stats_if_needed};
pub use tempo::read_tempo_time_sig_markers_from_project;
pub use parser::{parse_song_name, parse_section_type_from_name, extract_number_from_name};
pub use region_finder::{find_marker_by_name, find_song_regions, find_song_region_for_marker, find_song_region};
pub use song_builder::{
    build_song_from_current_project,
    build_setlist_from_open_projects,
    build_songs_from_project,
    build_songs_from_project_with_cache,
    build_song_from_region,
    build_song_from_project_simple,
};

