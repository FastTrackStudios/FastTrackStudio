//! MockTransport - In-memory transport service for testing
//!
//! This implementation provides a fully functional transport service that doesn't
//! require REAPER. Useful for:
//! - Unit testing business logic
//! - Integration testing without DAW
//! - Development/prototyping

use std::sync::{Arc, RwLock};

use roam::{Context, Tx};
use tokio::sync::broadcast;

use super::core::transport::{PlayState, RecordMode, Tempo};
use super::service::{TransportCommand, TransportEvent, TransportService};
use crate::Transport;

// region:    --- MockTransport

/// Mock implementation of TransportService for testing.
///
/// Maintains transport state in memory and broadcasts events when state changes.
/// Does not interact with any real DAW.
pub struct MockTransport {
    state: Arc<RwLock<Transport>>,
    event_tx: broadcast::Sender<TransportEvent>,
    position_tx: broadcast::Sender<f64>,
}

impl MockTransport {
    /// Create a new MockTransport with default state
    pub fn new() -> Self {
        let (event_tx, _) = broadcast::channel(64);
        let (position_tx, _) = broadcast::channel(64);

        Self {
            state: Arc::new(RwLock::new(Transport::new())),
            event_tx,
            position_tx,
        }
    }

    /// Create a MockTransport with a specific initial state
    pub fn with_state(state: Transport) -> Self {
        let (event_tx, _) = broadcast::channel(64);
        let (position_tx, _) = broadcast::channel(64);

        Self {
            state: Arc::new(RwLock::new(state)),
            event_tx,
            position_tx,
        }
    }

    /// Get a reference to the internal state for testing assertions
    pub fn state(&self) -> Transport {
        self.state.read().unwrap().clone()
    }

    /// Manually set state (for testing)
    pub fn set_state(&self, state: Transport) {
        let mut s = self.state.write().unwrap();
        *s = state.clone();
        let _ = self.event_tx.send(TransportEvent::StateChanged(state));
    }

    /// Simulate position update (for testing playback)
    pub fn update_position(&self, position: f64) {
        {
            let mut state = self.state.write().unwrap();
            state.playhead_position = crate::Position::from_seconds(position);
        }
        let _ = self.position_tx.send(position);
        let _ = self
            .event_tx
            .send(TransportEvent::PositionChanged(position));
    }

    fn broadcast_state_change(&self, state: &Transport) {
        let _ = self
            .event_tx
            .send(TransportEvent::StateChanged(state.clone()));
    }
}

impl Default for MockTransport {
    fn default() -> Self {
        Self::new()
    }
}

// endregion: --- MockTransport

// region:    --- TransportService Implementation

impl TransportService for MockTransport {
    async fn get_state(&self, _cx: &Context) -> Transport {
        self.state.read().unwrap().clone()
    }

    async fn execute(&self, _cx: &Context, cmd: TransportCommand) {
        let mut state = self.state.write().unwrap();

        match cmd {
            TransportCommand::Play => {
                state.play_state = PlayState::Playing;
                let _ = self
                    .event_tx
                    .send(TransportEvent::PlayStateChanged(PlayState::Playing));
            }
            TransportCommand::Pause => {
                state.play_state = PlayState::Paused;
                let _ = self
                    .event_tx
                    .send(TransportEvent::PlayStateChanged(PlayState::Paused));
            }
            TransportCommand::Stop => {
                state.play_state = PlayState::Stopped;
                let _ = self
                    .event_tx
                    .send(TransportEvent::PlayStateChanged(PlayState::Stopped));
            }
            TransportCommand::PlayPause => {
                state.play_state = match state.play_state {
                    PlayState::Playing => PlayState::Paused,
                    _ => PlayState::Playing,
                };
                let _ = self
                    .event_tx
                    .send(TransportEvent::PlayStateChanged(state.play_state));
            }
            TransportCommand::PlayStop => {
                state.play_state = match state.play_state {
                    PlayState::Playing | PlayState::Recording => PlayState::Stopped,
                    _ => PlayState::Playing,
                };
                let _ = self
                    .event_tx
                    .send(TransportEvent::PlayStateChanged(state.play_state));
            }
            TransportCommand::Record => {
                state.play_state = PlayState::Recording;
                let _ = self
                    .event_tx
                    .send(TransportEvent::PlayStateChanged(PlayState::Recording));
            }
            TransportCommand::StopRecording => {
                if state.play_state == PlayState::Recording {
                    state.play_state = PlayState::Playing;
                    let _ = self
                        .event_tx
                        .send(TransportEvent::PlayStateChanged(PlayState::Playing));
                }
            }
            TransportCommand::ToggleRecording => {
                state.play_state = match state.play_state {
                    PlayState::Recording => PlayState::Playing,
                    _ => PlayState::Recording,
                };
                let _ = self
                    .event_tx
                    .send(TransportEvent::PlayStateChanged(state.play_state));
            }
            TransportCommand::SetTempo(bpm) => {
                state.tempo = Tempo::new(bpm);
                let _ = self
                    .event_tx
                    .send(TransportEvent::TempoChanged(state.tempo));
            }
            TransportCommand::SetPosition(seconds) => {
                state.playhead_position = crate::Position::from_seconds(seconds);
                let _ = self.event_tx.send(TransportEvent::PositionChanged(seconds));
                let _ = self.position_tx.send(seconds);
            }
            TransportCommand::SetRecordMode(mode) => {
                state.record_mode = mode;
                let _ = self.event_tx.send(TransportEvent::RecordModeChanged(mode));
            }
            TransportCommand::ToggleLoop => {
                state.looping = !state.looping;
                let _ = self
                    .event_tx
                    .send(TransportEvent::LoopChanged(state.looping));
            }
        }

        self.broadcast_state_change(&state);
    }

    async fn subscribe(&self, _cx: &Context, events: Tx<TransportEvent>) {
        let mut rx = self.event_tx.subscribe();

        tokio::spawn(async move {
            while let Ok(event) = rx.recv().await {
                if events.send(&event).await.is_err() {
                    break;
                }
            }
        });
    }

    async fn subscribe_position(&self, _cx: &Context, positions: Tx<f64>) {
        let mut rx = self.position_tx.subscribe();

        tokio::spawn(async move {
            while let Ok(pos) = rx.recv().await {
                if positions.send(&pos).await.is_err() {
                    break;
                }
            }
        });
    }

    async fn get_play_state(&self, _cx: &Context) -> PlayState {
        self.state.read().unwrap().play_state
    }

    async fn get_tempo(&self, _cx: &Context) -> Tempo {
        self.state.read().unwrap().tempo
    }

    async fn get_position(&self, _cx: &Context) -> f64 {
        self.state
            .read()
            .unwrap()
            .playhead_position
            .time
            .to_seconds()
    }

    async fn is_playing(&self, _cx: &Context) -> bool {
        matches!(
            self.state.read().unwrap().play_state,
            PlayState::Playing | PlayState::Recording
        )
    }

    async fn is_recording(&self, _cx: &Context) -> bool {
        self.state.read().unwrap().play_state == PlayState::Recording
    }
}

// endregion: --- TransportService Implementation

// region:    --- Tests

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_initial_state() {
        let mock = MockTransport::new();
        let state = mock.state();

        assert_eq!(state.play_state, PlayState::Stopped);
        assert_eq!(state.tempo.bpm, 120.0);
        assert!(!state.looping);
    }

    #[test]
    fn test_with_custom_state() {
        let mut custom = Transport::new();
        custom.play_state = PlayState::Playing;
        custom.tempo = Tempo::new(140.0);

        let mock = MockTransport::with_state(custom);
        let state = mock.state();

        assert_eq!(state.play_state, PlayState::Playing);
        assert_eq!(state.tempo.bpm, 140.0);
    }

    #[test]
    fn test_set_state() {
        let mock = MockTransport::new();

        let mut new_state = Transport::new();
        new_state.play_state = PlayState::Paused;
        new_state.tempo = Tempo::new(90.0);

        mock.set_state(new_state);

        let state = mock.state();
        assert_eq!(state.play_state, PlayState::Paused);
        assert_eq!(state.tempo.bpm, 90.0);
    }

    #[test]
    fn test_update_position() {
        let mock = MockTransport::new();

        mock.update_position(42.5);

        let state = mock.state();
        assert!((state.playhead_position.time.to_seconds() - 42.5).abs() < 0.001);
    }
}

// endregion: --- Tests
