//! DAW Extension - REAPER Implementation
//!
//! CRITICAL: This is the ONLY crate allowed to depend on reaper-rs.
//! All other extensions call DAW services through daw-proto RPC.
//!
//! Implements all DAW services using REAPER API:
//! - TransportService
//! - MarkerRegionService
//! - TrackService
//!
//! Uses RoutedDispatcher to combine multiple services into one extension.

use daw_proto::*;
use extension_runtime::{run_extension, Context, RoutedDispatcher};
use reaper_medium::{PositionInSeconds, ProjectContext, Reaper, SetEditCurPosOptions};
use tracing::info;

// ============================================================================
// Transport Service Implementation
// ============================================================================

#[derive(Clone)]
struct TransportReaper;

impl TransportService for TransportReaper {
    async fn play(&self, _cx: &Context) -> TransportResult {
        info!("Transport: play");
        let reaper = Reaper::get();

        // Trigger play button (works globally across all projects)
        reaper.csurf_on_play();

        TransportResult::Success
    }

    async fn stop(&self, _cx: &Context) -> TransportResult {
        info!("Transport: stop");
        let reaper = Reaper::get();

        // Trigger stop button (works globally across all projects)
        reaper.csurf_on_stop();

        TransportResult::Success
    }

    async fn record(&self, _cx: &Context) -> TransportResult {
        info!("Transport: record");
        let reaper = Reaper::get();

        // Trigger record button (works globally across all projects)
        reaper.csurf_on_record();

        TransportResult::Success
    }

    async fn get_state(&self, _cx: &Context) -> PlaybackState {
        let reaper = Reaper::get();

        // Get current project context
        let project = ProjectContext::CurrentProject;

        // Get playback state (is_playing, is_paused, is_recording)
        let play_state = reaper.get_play_state_ex(project);

        // Get current play position
        let position = reaper.get_play_position_ex(project);

        // Get current tempo (BPM)
        let tempo = reaper.master_get_tempo();

        info!(
            "Transport: get_state - playing={}, recording={}, pos={:.2}s, tempo={:.1}bpm",
            play_state.is_playing,
            play_state.is_recording,
            position.get(),
            tempo.get()
        );

        PlaybackState {
            is_playing: play_state.is_playing,
            is_recording: play_state.is_recording,
            position_seconds: position.get(),
            tempo_bpm: tempo.get(),
        }
    }

    async fn set_position(&self, _cx: &Context, seconds: f64) -> TransportResult {
        info!("Transport: set_position to {:.2}s", seconds);
        let reaper = Reaper::get();

        // Get current project context
        let project = ProjectContext::CurrentProject;

        // Set edit cursor position
        // move_view = true means the arrange view will scroll to follow the cursor
        // seek_play = true means if playing, the play position will jump too
        let options = SetEditCurPosOptions {
            move_view: true,
            seek_play: true,
        };

        reaper.set_edit_curs_pos_2(
            project,
            PositionInSeconds::new_panic(seconds),
            options,
        );

        TransportResult::Success
    }
}

// ============================================================================
// Marker/Region Service Implementation
// ============================================================================

#[derive(Clone)]
struct MarkerRegionReaper;

impl MarkerRegionService for MarkerRegionReaper {
    async fn add_marker(&self, _cx: &Context, position: f64, name: String) -> MarkerResult {
        info!("Marker: add at {}s, name={}", position, name);
        // TODO: Implement with REAPER API
        MarkerResult::Error {
            message: "Not yet implemented".to_string(),
        }
    }

    async fn get_markers(&self, _cx: &Context) -> MarkerResult {
        info!("Marker: get_all");
        // TODO: Implement with REAPER API
        MarkerResult::SuccessList { markers: vec![] }
    }

    async fn delete_marker(&self, _cx: &Context, index: usize) -> MarkerResult {
        info!("Marker: delete index={}", index);
        // TODO: Implement with REAPER API
        MarkerResult::Error {
            message: "Not yet implemented".to_string(),
        }
    }
}

// ============================================================================
// Track Service Implementation
// ============================================================================

#[derive(Clone)]
struct TrackReaper;

impl TrackService for TrackReaper {
    async fn get_track_count(&self, _cx: &Context) -> usize {
        // TODO: Implement with REAPER API
        0
    }

    async fn get_track(&self, _cx: &Context, index: usize) -> TrackResult {
        // TODO: Implement with REAPER API
        TrackResult::Error {
            message: format!("Track {} not found (not yet implemented)", index),
        }
    }

    async fn get_all_tracks(&self, _cx: &Context) -> TrackResult {
        // TODO: Implement with REAPER API
        TrackResult::SuccessList { tracks: vec![] }
    }

    async fn set_track_name(&self, _cx: &Context, index: usize, _name: String) -> TrackResult {
        // TODO: Implement with REAPER API
        TrackResult::Error {
            message: format!("Track {} not found (not yet implemented)", index),
        }
    }
}

// ============================================================================
// Logger Service Implementation
// ============================================================================

#[derive(Clone)]
struct LoggerReaper;

impl DawLoggerService for LoggerReaper {
    async fn log(&self, _cx: &Context, message: String) -> LoggerResult {
        let reaper = Reaper::get();

        // Show message in REAPER console (ReaScript console)
        reaper.show_console_msg(format!("[DAW Extension] {}\n", message));

        LoggerResult::Success
    }
}

// ============================================================================
// Main Entry Point - Combine All Services
// ============================================================================

fn main() -> Result<(), Box<dyn std::error::Error>> {
    run_extension!("daw", |_handle| {
        // Create service implementations
        let transport = TransportReaper;
        let markers = MarkerRegionReaper;
        let tracks = TrackReaper;
        let logger = LoggerReaper;

        // Create dispatchers
        let transport_disp = TransportServiceDispatcher::new(transport);
        let markers_disp = MarkerRegionServiceDispatcher::new(markers);
        let tracks_disp = TrackServiceDispatcher::new(tracks);
        let logger_disp = DawLoggerServiceDispatcher::new(logger);

        // Combine using RoutedDispatcher (dodeca pattern)
        RoutedDispatcher::new(
            transport_disp,
            RoutedDispatcher::new(
                markers_disp,
                RoutedDispatcher::new(tracks_disp, logger_disp),
            ),
        )
    })
}
