//! REAPER implementation of setlist infrastructure
//!
//! This module provides REAPER-specific implementations of setlist building,
//! seeking, and command execution. It's only available when the `reaper` feature is enabled.

#![allow(unsafe_code)] // Required for low-level REAPER API calls

mod color_utils;
mod command_handler;
mod markers;
mod parser;
mod region_finder;
mod section_builder;
mod services;
mod song_builder;
mod state_provider;
mod stats;
mod tempo;
mod tracks;
mod trait_impls;
mod transport;
pub use trait_impls::{find_tab_index_by_project_name, switch_to_tab};

pub use command_handler::ReaperSetlistCommandHandler;
pub use markers::{read_markers_from_project, read_regions_from_project};
pub use parser::{extract_number_from_name, parse_section_type_from_name, parse_song_name};
pub use region_finder::{
    find_marker_by_name, find_song_region, find_song_region_for_marker, find_song_regions,
};
pub use section_builder::{build_sections_from_regions, build_sections_from_regions_simple};
pub use song_builder::{build_setlist_from_open_projects, build_song_from_current_project};
pub use state_provider::ReaperSetlistStateProvider;
pub use stats::{flush_build_stats_if_needed, record_section_created, record_song_built};
pub use tempo::read_tempo_time_sig_markers_from_project;
pub use tracks::{
    TrackSummary, get_all_tracks, get_track, get_track_summaries, invalidate_all_track_caches,
    invalidate_track_cache,
};
pub use transport::ReaperTransport;
