//! REAPER implementation of setlist infrastructure
//!
//! This module provides REAPER-specific implementations of setlist building,
//! seeking, and command execution. It's only available when the `reaper` feature is enabled.

#![allow(unsafe_code)] // Required for low-level REAPER API calls

mod color_utils;
mod markers;
mod tracks;
mod parser;
mod region_finder;
mod tempo;
mod stats;
mod section_builder;
mod song_builder;
mod transport;
mod services;
mod state_provider;
mod command_handler;
mod trait_impls;
pub use trait_impls::{find_tab_index_by_project_name, switch_to_tab};

pub use song_builder::{build_setlist_from_open_projects, build_song_from_current_project};
pub use transport::ReaperTransport;
pub use markers::{read_markers_from_project, read_regions_from_project};
pub use tracks::{get_track_summaries, get_all_tracks, get_track, invalidate_track_cache, invalidate_all_track_caches, TrackSummary};
pub use parser::{parse_song_name, parse_section_type_from_name, extract_number_from_name};
pub use region_finder::{find_marker_by_name, find_song_regions, find_song_region_for_marker, find_song_region};
pub use tempo::read_tempo_time_sig_markers_from_project;
pub use stats::{record_section_created, record_song_built, flush_build_stats_if_needed};
pub use section_builder::{build_sections_from_regions, build_sections_from_regions_simple};
pub use state_provider::ReaperSetlistStateProvider;
pub use command_handler::ReaperSetlistCommandHandler;
