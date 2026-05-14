//! `impl Transport for Standalone` — post-architect::rpc port.
//!
//! Backed by `ProjectState::transport: TransportState` in the
//! existing in-memory state. The old async `StandaloneTransport`
//! (~700 LOC, with timer-driven position advance + broadcaster + mock
//! data) retired in favor of operating directly on the canonical
//! state. Position advancement isn't simulated here — callers tick
//! the clock externally if they need playback simulation.

use daw_proto::transport::service::Transport;
use daw_proto::{
    DawError, DawResult, LoopRegion, PlayState, ProjectContext, TimeSignature,
    Transport as TransportState,
    primitives::{Position, PositionInSeconds, Tempo},
};

use crate::sync::Standalone;

fn resolve_project(daw: &Standalone, ctx: &ProjectContext) -> Option<String> {
    match ctx {
        ProjectContext::Project(guid) => Some(guid.clone()),
        ProjectContext::Current => {
            let state = daw.state.lock().ok()?;
            state.current_project_guid.clone()
        }
    }
}

fn not_found_proj() -> DawError {
    DawError::not_found("Project", "context")
}

impl Transport for Standalone {
    fn play(&self, project: ProjectContext) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            p.transport.play_state = PlayState::Playing;
        })
    }

    fn pause(&self, project: ProjectContext) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            p.transport.play_state = PlayState::Paused;
        })
    }

    fn stop(&self, project: ProjectContext) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            p.transport.play_state = PlayState::Stopped;
        })
    }

    fn play_pause(&self, project: ProjectContext) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            p.transport.play_state = match p.transport.play_state {
                PlayState::Playing => PlayState::Paused,
                _ => PlayState::Playing,
            };
        })
    }

    fn play_stop(&self, project: ProjectContext) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            p.transport.play_state = match p.transport.play_state {
                PlayState::Playing | PlayState::Recording => PlayState::Stopped,
                _ => PlayState::Playing,
            };
        })
    }

    fn record(&self, project: ProjectContext) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            p.transport.play_state = PlayState::Recording;
        })
    }

    fn stop_recording(&self, project: ProjectContext) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            if matches!(p.transport.play_state, PlayState::Recording) {
                p.transport.play_state = PlayState::Stopped;
            }
        })
    }

    fn toggle_recording(&self, project: ProjectContext) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            p.transport.play_state = match p.transport.play_state {
                PlayState::Recording => PlayState::Stopped,
                _ => PlayState::Recording,
            };
        })
    }

    fn set_position(&self, project: ProjectContext, seconds: f64) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            let pos = Position::from_time(PositionInSeconds::from_seconds(seconds));
            p.transport.playhead_position = pos.clone();
            p.transport.edit_position = pos;
        })
    }

    fn get_position(&self, project: ProjectContext) -> f64 {
        let Some(guid) = resolve_project(self, &project) else {
            return 0.0;
        };
        self.with_project(&guid, |p| {
            p.transport
                .playhead_position
                .time
                .as_ref()
                .map(|t| t.as_seconds())
                .unwrap_or(0.0)
        })
        .unwrap_or(0.0)
    }

    fn goto_start(&self, project: ProjectContext) -> DawResult<()> {
        <Self as Transport>::set_position(self, project, 0.0)
    }

    fn goto_end(&self, project: ProjectContext) -> DawResult<()> {
        // Standalone has no real "end" — approximate with loop end.
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        let end = self
            .with_project(&guid, |p| {
                p.transport
                    .loop_region
                    .as_ref()
                    .map(|r| r.end_seconds)
                    .unwrap_or(0.0)
            })
            .unwrap_or(0.0);
        <Self as Transport>::set_position(self, ProjectContext::Project(guid), end)
    }

    fn get_state(&self, project: ProjectContext) -> TransportState {
        let Some(guid) = resolve_project(self, &project) else {
            return TransportState::default();
        };
        self.with_project(&guid, |p| p.transport.clone())
            .unwrap_or_default()
    }

    fn get_play_state(&self, project: ProjectContext) -> PlayState {
        let Some(guid) = resolve_project(self, &project) else {
            return PlayState::Stopped;
        };
        self.with_project(&guid, |p| p.transport.play_state)
            .unwrap_or(PlayState::Stopped)
    }

    fn is_playing(&self, project: ProjectContext) -> bool {
        matches!(
            <Self as Transport>::get_play_state(self, project),
            PlayState::Playing | PlayState::Recording
        )
    }

    fn is_recording(&self, project: ProjectContext) -> bool {
        matches!(
            <Self as Transport>::get_play_state(self, project),
            PlayState::Recording
        )
    }

    fn get_tempo(&self, project: ProjectContext) -> f64 {
        let Some(guid) = resolve_project(self, &project) else {
            return 120.0;
        };
        self.with_project(&guid, |p| p.transport.tempo.bpm())
            .unwrap_or(120.0)
    }

    fn set_tempo(&self, project: ProjectContext, bpm: f64) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            p.transport.tempo = Tempo::from_bpm(bpm);
        })
    }

    fn toggle_loop(&self, project: ProjectContext) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            p.transport.looping = !p.transport.looping;
        })
    }

    fn is_looping(&self, project: ProjectContext) -> bool {
        let Some(guid) = resolve_project(self, &project) else {
            return false;
        };
        self.with_project(&guid, |p| p.transport.looping)
            .unwrap_or(false)
    }

    fn set_loop(&self, project: ProjectContext, enabled: bool) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            p.transport.looping = enabled;
        })
    }

    fn get_time_selection(&self, project: ProjectContext) -> Option<LoopRegion> {
        let guid = resolve_project(self, &project)?;
        self.with_project(&guid, |p| p.transport.time_selection.clone())
            .ok()
            .flatten()
    }

    fn set_time_selection(
        &self,
        project: ProjectContext,
        start_seconds: f64,
        end_seconds: f64,
    ) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            p.transport.time_selection = Some(LoopRegion::new(
                start_seconds.min(end_seconds),
                start_seconds.max(end_seconds),
            ));
        })
    }

    fn clear_time_selection(&self, project: ProjectContext) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            p.transport.time_selection = None;
        })
    }

    fn get_playrate(&self, project: ProjectContext) -> f64 {
        let Some(guid) = resolve_project(self, &project) else {
            return 1.0;
        };
        self.with_project(&guid, |p| p.transport.playrate)
            .unwrap_or(1.0)
    }

    fn set_playrate(&self, project: ProjectContext, rate: f64) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        self.with_project_mut(&guid, |p| {
            p.transport.playrate = rate.clamp(0.25, 4.0);
        })
    }

    fn get_time_signature(&self, project: ProjectContext) -> TimeSignature {
        let Some(guid) = resolve_project(self, &project) else {
            return TimeSignature::default();
        };
        self.with_project(&guid, |p| p.transport.time_signature.clone())
            .unwrap_or_default()
    }
}
