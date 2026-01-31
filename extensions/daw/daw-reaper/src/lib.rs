//! DAW Implementation for REAPER
//!
//! Provides Transport and other DAW services using REAPER API.
//! This is the ONLY crate allowed to depend on reaper-rs.
//!
//! Used by reaper_extension to provide DAW services to extensions.

use daw_proto::*;
use extension_runtime::Context;
use reaper_medium::{PositionInSeconds, ProjectContext, Reaper, SetEditCurPosOptions};
use tracing::info;

/// REAPER transport implementation
#[derive(Clone)]
pub struct ReaperTransport;

impl ReaperTransport {
    pub fn new() -> Self {
        Self
    }
}

impl Default for ReaperTransport {
    fn default() -> Self {
        Self::new()
    }
}

impl Transport for ReaperTransport {
    async fn play(&self, _cx: &Context) -> TransportResult {
        info!("ReaperTransport: play");
        Reaper::get().csurf_on_play();
        TransportResult::Success
    }

    async fn stop(&self, _cx: &Context) -> TransportResult {
        info!("ReaperTransport: stop");
        Reaper::get().csurf_on_stop();
        TransportResult::Success
    }

    async fn pause(&self, _cx: &Context) -> TransportResult {
        info!("ReaperTransport: pause");
        Reaper::get().csurf_on_pause();
        TransportResult::Success
    }

    async fn record(&self, _cx: &Context) -> TransportResult {
        info!("ReaperTransport: record");
        Reaper::get().csurf_on_record();
        TransportResult::Success
    }

    async fn get_state(&self, _cx: &Context) -> PlaybackState {
        let reaper = Reaper::get();
        let project = ProjectContext::CurrentProject;
        let state = reaper.get_play_state_ex(project);
        let position = reaper.get_play_position_ex(project);
        let tempo = reaper.master_get_tempo();

        PlaybackState {
            is_playing: state.is_playing,
            is_paused: state.is_paused,
            is_recording: state.is_recording,
            position_seconds: position.get(),
            tempo_bpm: tempo.get(),
        }
    }

    async fn set_position(&self, _cx: &Context, seconds: f64) -> TransportResult {
        info!("ReaperTransport: set_position to {:.2}s", seconds);
        let reaper = Reaper::get();
        let options = SetEditCurPosOptions {
            move_view: true,
            seek_play: true,
        };
        reaper.set_edit_curs_pos_2(
            ProjectContext::CurrentProject,
            PositionInSeconds::new_panic(seconds),
            options,
        );
        TransportResult::Success
    }
}

/// Re-export the dispatcher type for convenience
pub use daw_proto::TransportDispatcher;
