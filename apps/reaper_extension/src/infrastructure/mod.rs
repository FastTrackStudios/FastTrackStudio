//! Infrastructure code
//!
//! Infrastructure modules handle REAPER-specific setup and integration.

pub mod action_registry;
pub mod change_detection;
pub mod timer;
pub mod iroh_server;
pub mod task_support;
pub mod menu;
pub mod tracing_config;
pub mod color_utils;
pub mod reactive_logger;
pub mod reactive_polling;
pub mod reactive_app_state;
pub mod reaper_transport_reactive;
pub mod reaper_track_reactive;
pub mod reaper_track_command;
pub mod reaper_lyrics_reactive;
pub mod reaper_chords_reactive;
pub mod formatted_logging;


