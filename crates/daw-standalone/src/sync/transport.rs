//! `StandaloneTransport` — sync transport sub-handle.

use daw_proto::{
    DawResult, LoopRegion, PlayState, Position, PositionInSeconds, Tempo,
    Transport as TransportState, sync::Transport,
};

use super::daw::Standalone;

pub struct StandaloneTransport<'a> {
    daw: &'a Standalone,
    guid: String,
}

impl<'a> StandaloneTransport<'a> {
    pub(crate) fn new(daw: &'a Standalone, guid: String) -> Self {
        Self { daw, guid }
    }
}

impl<'a> Transport for StandaloneTransport<'a> {
    fn state(&self) -> DawResult<TransportState> {
        self.daw.with_project(&self.guid, |p| p.transport.clone())
    }

    fn position(&self) -> f64 {
        self.daw
            .with_project(&self.guid, |p| {
                p.transport
                    .playhead_position
                    .time
                    .as_ref()
                    .map(|t| t.as_seconds())
                    .unwrap_or(0.0)
            })
            .unwrap_or(0.0)
    }

    fn set_position(&self, seconds: f64) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            p.transport.playhead_position =
                Position::from_time(PositionInSeconds::from_seconds(seconds));
            p.transport.edit_position =
                Position::from_time(PositionInSeconds::from_seconds(seconds));
        })
    }

    fn time_selection(&self) -> Option<(f64, f64)> {
        self.daw
            .with_project(&self.guid, |p| {
                p.transport
                    .time_selection
                    .as_ref()
                    .map(|r| (r.start_seconds, r.end_seconds))
            })
            .ok()
            .flatten()
    }

    fn set_time_selection(&self, start: f64, end: f64) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            p.transport.time_selection = Some(LoopRegion::new(start.min(end), start.max(end)));
        })
    }

    fn clear_time_selection(&self) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            p.transport.time_selection = None;
        })
    }

    fn play_state(&self) -> PlayState {
        self.daw
            .with_project(&self.guid, |p| p.transport.play_state)
            .unwrap_or(PlayState::Stopped)
    }

    fn play(&self) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            p.transport.play_state = PlayState::Playing;
        })
    }

    fn pause(&self) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            p.transport.play_state = PlayState::Paused;
        })
    }

    fn stop(&self) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            p.transport.play_state = PlayState::Stopped;
        })
    }

    fn record(&self) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            p.transport.play_state = PlayState::Recording;
        })
    }

    fn tempo_bpm(&self) -> f64 {
        self.daw
            .with_project(&self.guid, |p| p.transport.tempo.bpm())
            .unwrap_or(120.0)
    }

    fn set_tempo_bpm(&self, bpm: f64) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            p.transport.tempo = Tempo::from_bpm(bpm);
        })
    }

    fn is_looping(&self) -> bool {
        self.daw
            .with_project(&self.guid, |p| p.transport.looping)
            .unwrap_or(false)
    }

    fn set_looping(&self, looping: bool) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            p.transport.looping = looping;
        })
    }
}
