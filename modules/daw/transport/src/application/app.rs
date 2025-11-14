//! Transport Application Layer
//!
//! This module provides the main transport application that manages state,
//! handles timing, and coordinates between the core domain and infrastructure layers.
//! This serves as the default transport implementation when not driven by external sources.

use crate::core::{PlayState, RecordMode, Tempo, Transport, TransportActions, TransportError};
use primitives::{Position, TimeSignature};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::{broadcast, RwLock};
use tokio::time::{interval, MissedTickBehavior};
use uuid::Uuid;

/// Transport update events that can be broadcast to listeners
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum TransportEvent {
    /// Playback started
    PlaybackStarted {
        timestamp: chrono::DateTime<chrono::Utc>,
        position: f64,
    },
    /// Playback stopped
    PlaybackStopped {
        timestamp: chrono::DateTime<chrono::Utc>,
        position: f64,
    },
    /// Playback paused
    PlaybackPaused {
        timestamp: chrono::DateTime<chrono::Utc>,
        position: f64,
    },
    /// Recording started
    RecordingStarted {
        timestamp: chrono::DateTime<chrono::Utc>,
        position: f64,
    },
    /// Recording stopped
    RecordingStopped {
        timestamp: chrono::DateTime<chrono::Utc>,
        position: f64,
    },
    /// Position changed (seeks, loops, etc.)
    PositionChanged {
        timestamp: chrono::DateTime<chrono::Utc>,
        old_position: f64,
        new_position: f64,
    },
    /// Tempo changed
    TempoChanged {
        timestamp: chrono::DateTime<chrono::Utc>,
        old_tempo: f64,
        new_tempo: f64,
    },
    /// Time signature changed
    TimeSignatureChanged {
        timestamp: chrono::DateTime<chrono::Utc>,
        numerator: u8,
        denominator: u8,
    },
    /// Transport state synchronization event
    StateSync {
        timestamp: chrono::DateTime<chrono::Utc>,
        transport: Transport,
    },
}

/// Configuration for the transport application
#[derive(Debug, Clone)]
pub struct TransportConfig {
    /// Update rate in Hz for position updates
    pub update_rate: f64,
    /// Maximum number of event subscribers
    pub max_subscribers: usize,
    /// Enable high-resolution timing
    pub high_resolution: bool,
    /// Preroll time in seconds
    pub default_preroll: f64,
    /// Postroll time in seconds
    pub default_postroll: f64,
}

impl Default for TransportConfig {
    fn default() -> Self {
        Self {
            update_rate: 60.0, // 60 FPS for smooth updates
            max_subscribers: 100,
            high_resolution: true,
            default_preroll: 0.0,
            default_postroll: 0.0,
        }
    }
}

/// Internal transport state with timing information
#[derive(Debug, Clone)]
struct TransportState {
    /// Core transport data
    transport: Transport,
    /// When playback was started (for calculating current position)
    playback_start_time: Option<Instant>,
    /// Position when playback was started
    playback_start_position: Position,
    /// Last update timestamp
    last_update: Instant,
    /// Session identifier
    session_id: Uuid,
}

impl TransportState {
    fn new() -> Self {
        Self {
            transport: Transport::new(),
            playback_start_time: None,
            playback_start_position: Position::start(),
            last_update: Instant::now(),
            session_id: Uuid::new_v4(),
        }
    }

    /// Calculate current playhead position based on elapsed time
    fn calculate_current_position(&self) -> Position {
        if let Some(start_time) = self.playback_start_time {
            if self.transport.is_playing() {
                let elapsed = start_time.elapsed().as_secs_f64();
                let elapsed_scaled = elapsed * self.transport.playrate;
                let new_seconds = self.playback_start_position.time.to_seconds() + elapsed_scaled;

                // Handle looping
                if let Some(loop_selection) = &self.transport.loop_selection {
                    if self.transport.looping {
                        let loop_start = loop_selection.start.time.to_seconds();
                        let loop_end = loop_selection.end.time.to_seconds();
                        let loop_length = loop_end - loop_start;

                        if new_seconds >= loop_end {
                            let loops_passed = ((new_seconds - loop_start) / loop_length).floor();
                            let looped_position = loop_start + (new_seconds - loop_start) - (loops_passed * loop_length);
                            return Position::from_seconds(looped_position);
                        }
                    }
                }

                Position::from_seconds(new_seconds)
            } else {
                self.transport.playhead_position
            }
        } else {
            self.transport.playhead_position
        }
    }

    /// Update the transport's playhead position
    fn update_position(&mut self) {
        self.transport.playhead_position = self.calculate_current_position();
        self.last_update = Instant::now();
    }
}

/// Main transport application with real-time state management
pub struct TransportApp {
    /// Protected transport state
    state: Arc<RwLock<TransportState>>,
    /// Event broadcaster
    event_sender: broadcast::Sender<TransportEvent>,
    /// Configuration
    config: TransportConfig,
    /// Update task handle
    update_handle: Option<tokio::task::JoinHandle<()>>,
}

impl TransportApp {
    /// Create a new transport application
    pub fn new(config: TransportConfig) -> Self {
        let (event_sender, _) = broadcast::channel(config.max_subscribers);

        let app = Self {
            state: Arc::new(RwLock::new(TransportState::new())),
            event_sender,
            config,
            update_handle: None,
        };

        // Apply default configuration
        if let Ok(mut state) = app.state.try_write() {
            state.transport.preroll = app.config.default_preroll;
            state.transport.postroll = app.config.default_postroll;
        }

        app
    }

    /// Create with default configuration
    pub fn with_defaults() -> Self {
        Self::new(TransportConfig::default())
    }

    /// Start the transport update loop
    pub async fn start(&mut self) -> Result<(), TransportError> {
        if self.update_handle.is_some() {
            return Err(TransportError::InvalidState("Transport already started".to_string()));
        }

        let state = Arc::clone(&self.state);
        let event_sender = self.event_sender.clone();
        let update_rate = self.config.update_rate;

        let handle = tokio::spawn(async move {
            let mut interval = interval(Duration::from_secs_f64(1.0 / update_rate));
            interval.set_missed_tick_behavior(MissedTickBehavior::Skip);

            loop {
                interval.tick().await;

                // Update position and broadcast events
                if let Ok(mut state) = state.try_write() {
                    let old_position = state.transport.playhead_position.time.to_seconds();
                    state.update_position();
                    let new_position = state.transport.playhead_position.time.to_seconds();

                    // Only broadcast position changes if significant
                    if (new_position - old_position).abs() > 0.001 && state.transport.is_playing() {
                        let event = TransportEvent::PositionChanged {
                            timestamp: chrono::Utc::now(),
                            old_position,
                            new_position,
                        };
                        let _ = event_sender.send(event);
                    }
                }
            }
        });

        self.update_handle = Some(handle);
        Ok(())
    }

    /// Stop the transport update loop
    pub fn stop(&mut self) {
        if let Some(handle) = self.update_handle.take() {
            handle.abort();
        }
    }

    /// Subscribe to transport events
    pub fn subscribe(&self) -> broadcast::Receiver<TransportEvent> {
        self.event_sender.subscribe()
    }

    /// Get the current session ID
    pub async fn session_id(&self) -> Uuid {
        self.state.read().await.session_id
    }

    /// Force a state synchronization event
    pub async fn sync_state(&self) -> Result<(), TransportError> {
        let state = self.state.read().await;
        let event = TransportEvent::StateSync {
            timestamp: chrono::Utc::now(),
            transport: state.transport.clone(),
        };
        self.event_sender.send(event)
            .map_err(|e| TransportError::EventBroadcast(e.to_string()))?;
        Ok(())
    }

    /// Get transport statistics
    pub async fn get_stats(&self) -> TransportStats {
        let state = self.state.read().await;
        TransportStats {
            session_id: state.session_id,
            uptime: state.last_update.elapsed(),
            subscriber_count: self.event_sender.receiver_count(),
            is_running: self.update_handle.is_some(),
            current_position: state.transport.playhead_position.time.to_seconds(),
            effective_bpm: state.transport.effective_bpm(),
        }
    }

    /// Emit a custom event
    async fn emit_event(&self, event: TransportEvent) {
        let _ = self.event_sender.send(event);
    }
}

/// Statistics about the transport application
#[derive(Debug, Clone, serde::Serialize)]
pub struct TransportStats {
    #[serde(serialize_with = "serialize_uuid")]
    pub session_id: Uuid,
    pub uptime: Duration,
    pub subscriber_count: usize,
    pub is_running: bool,
    pub current_position: f64,
    pub effective_bpm: f64,
}

fn serialize_uuid<S>(uuid: &Uuid, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    serializer.serialize_str(&uuid.to_string())
}

// Async transport actions for TransportApp (to avoid block_on issues)
impl TransportApp {
    /// Async version of play
    pub async fn async_play(&mut self) -> Result<String, crate::TransportError> {
        println!("🎵 TransportApp::async_play() called");
        let mut state = self.state.write().await;

        if !state.transport.is_playing() {
            println!("🎵 Starting playback from position: {:.3}s", state.transport.playhead_position.time.to_seconds());
            let now = Instant::now();
            state.transport.play_state = PlayState::Playing;
            state.playback_start_time = Some(now);
            state.playback_start_position = state.transport.playhead_position;

            let event = TransportEvent::PlaybackStarted {
                timestamp: chrono::Utc::now(),
                position: state.transport.playhead_position.time.to_seconds(),
            };
            let _ = self.event_sender.send(event);

            println!("✅ Playback started successfully");
            Ok("Playback started".to_string())
        } else {
            println!("⚠️ Already playing - ignoring play command");
            Err(crate::TransportError::InvalidState("Already playing".to_string()))
        }
    }

    /// Async version of pause
    pub async fn async_pause(&mut self) -> Result<String, crate::TransportError> {
        println!("⏸️ TransportApp::async_pause() called");
        let mut state = self.state.write().await;

        if state.transport.is_playing() {
            println!("⏸️ Pausing playback at position: {:.3}s", state.transport.playhead_position.time.to_seconds());
            state.update_position(); // Update position before pausing
            state.transport.play_state = PlayState::Paused;
            state.playback_start_time = None;

            let event = TransportEvent::PlaybackPaused {
                timestamp: chrono::Utc::now(),
                position: state.transport.playhead_position.time.to_seconds(),
            };
            let _ = self.event_sender.send(event);

            println!("✅ Playback paused successfully");
            Ok("Playback paused".to_string())
        } else {
            println!("⚠️ Not playing - ignoring pause command");
            Err(crate::TransportError::InvalidState("Not playing".to_string()))
        }
    }

    /// Async version of stop
    pub async fn async_stop(&mut self) -> Result<String, crate::TransportError> {
        println!("⏹️ TransportApp::async_stop() called");
        let mut state = self.state.write().await;

        let was_playing = state.transport.is_playing();
        println!("⏹️ Was playing: {}, stopping at position: {:.3}s", was_playing, state.transport.playhead_position.time.to_seconds());

        state.transport.play_state = PlayState::Stopped;
        state.transport.playhead_position = state.transport.edit_position;
        state.playback_start_time = None;

        if was_playing {
            let event = TransportEvent::PlaybackStopped {
                timestamp: chrono::Utc::now(),
                position: state.transport.playhead_position.time.to_seconds(),
            };
            let _ = self.event_sender.send(event);
        }

        println!("✅ Transport stopped successfully");
        Ok("Transport stopped".to_string())
    }

    /// Async version of play_pause
    pub async fn async_play_pause(&mut self) -> Result<String, crate::TransportError> {
        let is_playing = {
            let state = self.state.read().await;
            state.transport.is_playing()
        };

        if is_playing {
            self.async_pause().await
        } else {
            self.async_play().await
        }
    }

    /// Async version of play_stop
    pub async fn async_play_stop(&mut self) -> Result<String, crate::TransportError> {
        let is_playing = {
            let state = self.state.read().await;
            state.transport.is_playing()
        };

        if is_playing {
            self.async_stop().await
        } else {
            self.async_play().await
        }
    }

    /// Async version of start_recording
    pub async fn async_start_recording(&mut self) -> Result<String, crate::TransportError> {
        let mut state = self.state.write().await;

        let now = Instant::now();
        state.transport.play_state = PlayState::Recording;
        if state.playback_start_time.is_none() {
            state.playback_start_time = Some(now);
            state.playback_start_position = state.transport.playhead_position;
        }

        let event = TransportEvent::RecordingStarted {
            timestamp: chrono::Utc::now(),
            position: state.transport.playhead_position.time.to_seconds(),
        };
        let _ = self.event_sender.send(event);

        Ok("Recording started".to_string())
    }

    /// Async version of stop_recording
    pub async fn async_stop_recording(&mut self) -> Result<String, crate::TransportError> {
        let mut state = self.state.write().await;

        if state.transport.is_recording() {
            state.transport.play_state = PlayState::Playing;

            let event = TransportEvent::RecordingStopped {
                timestamp: chrono::Utc::now(),
                position: state.transport.playhead_position.time.to_seconds(),
            };
            let _ = self.event_sender.send(event);

            Ok("Recording stopped".to_string())
        } else {
            Err(crate::TransportError::InvalidState("Not currently recording".to_string()))
        }
    }

    /// Async version of set_tempo
    pub async fn async_set_tempo(&mut self, tempo: Tempo) -> Result<String, crate::TransportError> {
        let mut state = self.state.write().await;
        let old_tempo = state.transport.tempo.bpm;

        if !tempo.is_valid() {
            return Err(crate::TransportError::InvalidTempo(format!("{} BPM", tempo.bpm)));
        }

        state.transport.tempo = tempo;

        let event = TransportEvent::TempoChanged {
            timestamp: chrono::Utc::now(),
            old_tempo,
            new_tempo: tempo.bpm,
        };
        let _ = self.event_sender.send(event);

        Ok(format!("Tempo set to {} BPM", tempo.bpm))
    }

    /// Async version of set_time_signature
    pub async fn async_set_time_signature(&mut self, time_signature: TimeSignature) -> Result<String, crate::TransportError> {
        let mut state = self.state.write().await;
        state.transport.time_signature = time_signature;

        let event = TransportEvent::TimeSignatureChanged {
            timestamp: chrono::Utc::now(),
            numerator: time_signature.numerator as u8,
            denominator: time_signature.denominator as u8,
        };
        let _ = self.event_sender.send(event);

        Ok(format!("Time signature set to {}/{}", time_signature.numerator, time_signature.denominator))
    }

    /// Async version of set_position
    pub async fn async_set_position(&mut self, seconds: f64) -> Result<String, crate::TransportError> {
        let mut state = self.state.write().await;
        let old_position = state.transport.playhead_position.time.to_seconds();
        let new_position = Position::from_seconds(seconds);

        state.transport.playhead_position = new_position;
        state.transport.edit_position = new_position;

        // Reset playback timing if currently playing
        if state.transport.is_playing() {
            state.playback_start_time = Some(Instant::now());
            state.playback_start_position = new_position;
        }

        let event = TransportEvent::PositionChanged {
            timestamp: chrono::Utc::now(),
            old_position,
            new_position: seconds,
        };
        let _ = self.event_sender.send(event);

        Ok(format!("Position set to {} seconds", seconds))
    }

    /// Async version of get_transport
    pub async fn async_get_transport(&self) -> Result<Transport, crate::TransportError> {
        println!("📊 TransportApp::async_get_transport() called");
        let state = self.state.read().await;
        println!("📊 Returning transport state: play_state={:?}, position={:.3}s",
                 state.transport.play_state, state.transport.playhead_position.time.to_seconds());
        Ok(state.transport.clone())
    }

    /// Async version of get_tempo
    pub async fn async_get_tempo(&self) -> Result<Tempo, crate::TransportError> {
        let state = self.state.read().await;
        Ok(state.transport.tempo)
    }

    /// Async version of get_time_signature
    pub async fn async_get_time_signature(&self) -> Result<TimeSignature, crate::TransportError> {
        let state = self.state.read().await;
        Ok(state.transport.time_signature)
    }

    /// Async version of get_position
    pub async fn async_get_position(&self) -> Result<f64, crate::TransportError> {
        let state = self.state.read().await;
        Ok(state.calculate_current_position().time.to_seconds())
    }

    /// Async version of is_playing
    pub async fn async_is_playing(&self) -> Result<bool, crate::TransportError> {
        let state = self.state.read().await;
        Ok(state.transport.is_playing())
    }

    /// Async version of is_recording
    pub async fn async_is_recording(&self) -> Result<bool, crate::TransportError> {
        let state = self.state.read().await;
        Ok(state.transport.is_recording())
    }

    /// Async version of is_ready
    pub async fn async_is_ready(&self) -> Result<bool, crate::TransportError> {
        Ok(self.update_handle.is_some())
    }
}

// Implement TransportActions for TransportApp
impl TransportActions for TransportApp {
    fn play(&mut self) -> Result<String, TransportError> {
        let rt = tokio::runtime::Handle::try_current()
            .map_err(|_| TransportError::InvalidState("No tokio runtime".to_string()))?;

        rt.block_on(async {
            let mut state = self.state.write().await;

            if !state.transport.is_playing() {
                let now = Instant::now();
                state.transport.play_state = PlayState::Playing;
                state.playback_start_time = Some(now);
                state.playback_start_position = state.transport.playhead_position;

                let event = TransportEvent::PlaybackStarted {
                    timestamp: chrono::Utc::now(),
                    position: state.transport.playhead_position.time.to_seconds(),
                };
                let _ = self.event_sender.send(event);

                Ok("Playback started".to_string())
            } else {
                Err(TransportError::InvalidState("Already playing".to_string()))
            }
        })
    }

    fn pause(&mut self) -> Result<String, TransportError> {
        let rt = tokio::runtime::Handle::try_current()
            .map_err(|_| TransportError::InvalidState("No tokio runtime".to_string()))?;

        rt.block_on(async {
            let mut state = self.state.write().await;

            if state.transport.is_playing() {
                state.update_position(); // Update position before pausing
                state.transport.play_state = PlayState::Paused;
                state.playback_start_time = None;

                let event = TransportEvent::PlaybackPaused {
                    timestamp: chrono::Utc::now(),
                    position: state.transport.playhead_position.time.to_seconds(),
                };
                let _ = self.event_sender.send(event);

                Ok("Playback paused".to_string())
            } else {
                Err(TransportError::InvalidState("Not currently playing".to_string()))
            }
        })
    }

    fn stop(&mut self) -> Result<String, TransportError> {
        let rt = tokio::runtime::Handle::try_current()
            .map_err(|_| TransportError::InvalidState("No tokio runtime".to_string()))?;

        rt.block_on(async {
            let mut state = self.state.write().await;

            let was_playing = state.transport.is_playing();
            state.transport.play_state = PlayState::Stopped;
            state.transport.playhead_position = state.transport.edit_position;
            state.playback_start_time = None;

            if was_playing {
                let event = TransportEvent::PlaybackStopped {
                    timestamp: chrono::Utc::now(),
                    position: state.transport.playhead_position.time.to_seconds(),
                };
                let _ = self.event_sender.send(event);
            }

            Ok("Playback stopped".to_string())
        })
    }

    fn play_pause(&mut self) -> Result<String, TransportError> {
        let rt = tokio::runtime::Handle::try_current()
            .map_err(|_| TransportError::InvalidState("No tokio runtime".to_string()))?;

        rt.block_on(async {
            let is_playing = {
                let state = self.state.read().await;
                state.transport.is_playing()
            };

            if is_playing {
                self.pause()
            } else {
                self.play()
            }
        })
    }

    fn play_stop(&mut self) -> Result<String, TransportError> {
        let rt = tokio::runtime::Handle::try_current()
            .map_err(|_| TransportError::InvalidState("No tokio runtime".to_string()))?;

        let is_playing = rt.block_on(async {
            let state = self.state.read().await;
            state.transport.is_playing()
        });

        if is_playing {
            return <Self as TransportActions>::stop(self);
        } else {
            return <Self as TransportActions>::play(self);
        }
    }

    fn start_recording(&mut self) -> Result<String, TransportError> {
        let rt = tokio::runtime::Handle::try_current()
            .map_err(|_| TransportError::InvalidState("No tokio runtime".to_string()))?;

        rt.block_on(async {
            let mut state = self.state.write().await;

            let now = Instant::now();
            state.transport.play_state = PlayState::Recording;
            if state.playback_start_time.is_none() {
                state.playback_start_time = Some(now);
                state.playback_start_position = state.transport.playhead_position;
            }

            let event = TransportEvent::RecordingStarted {
                timestamp: chrono::Utc::now(),
                position: state.transport.playhead_position.time.to_seconds(),
            };
            let _ = self.event_sender.send(event);

            Ok("Recording started".to_string())
        })
    }

    fn stop_recording(&mut self) -> Result<String, TransportError> {
        let rt = tokio::runtime::Handle::try_current()
            .map_err(|_| TransportError::InvalidState("No tokio runtime".to_string()))?;

        rt.block_on(async {
            let mut state = self.state.write().await;

            if state.transport.is_recording() {
                state.transport.play_state = PlayState::Playing;

                let event = TransportEvent::RecordingStopped {
                    timestamp: chrono::Utc::now(),
                    position: state.transport.playhead_position.time.to_seconds(),
                };
                let _ = self.event_sender.send(event);

                Ok("Recording stopped".to_string())
            } else {
                Err(TransportError::InvalidState("Not currently recording".to_string()))
            }
        })
    }

    fn toggle_recording(&mut self) -> Result<String, TransportError> {
        let rt = tokio::runtime::Handle::try_current()
            .map_err(|_| TransportError::InvalidState("No tokio runtime".to_string()))?;

        rt.block_on(async {
            let is_recording = {
                let state = self.state.read().await;
                state.transport.is_recording()
            };

            if is_recording {
                self.stop_recording()
            } else {
                self.start_recording()
            }
        })
    }

    fn set_tempo(&mut self, tempo: Tempo) -> Result<String, TransportError> {
        let rt = tokio::runtime::Handle::try_current()
            .map_err(|_| TransportError::InvalidState("No tokio runtime".to_string()))?;

        rt.block_on(async {
            let mut state = self.state.write().await;
            let old_tempo = state.transport.tempo.bpm;

            if !tempo.is_valid() {
                return Err(TransportError::InvalidTempo(format!("{} BPM", tempo.bpm)));
            }

            state.transport.tempo = tempo;

            let event = TransportEvent::TempoChanged {
                timestamp: chrono::Utc::now(),
                old_tempo,
                new_tempo: tempo.bpm,
            };
            let _ = self.event_sender.send(event);

            Ok(format!("Tempo set to {} BPM", tempo.bpm))
        })
    }

    fn set_time_signature(&mut self, time_signature: TimeSignature) -> Result<String, TransportError> {
        let rt = tokio::runtime::Handle::try_current()
            .map_err(|_| TransportError::InvalidState("No tokio runtime".to_string()))?;

        rt.block_on(async {
            let mut state = self.state.write().await;
            state.transport.time_signature = time_signature;

            let event = TransportEvent::TimeSignatureChanged {
                timestamp: chrono::Utc::now(),
                numerator: time_signature.numerator as u8,
                denominator: time_signature.denominator as u8,
            };
            let _ = self.event_sender.send(event);

            Ok(format!("Time signature set to {}/{}", time_signature.numerator, time_signature.denominator))
        })
    }

    fn set_record_mode(&mut self, record_mode: RecordMode) -> Result<String, TransportError> {
        let rt = tokio::runtime::Handle::try_current()
            .map_err(|_| TransportError::InvalidState("No tokio runtime".to_string()))?;

        rt.block_on(async {
            let mut state = self.state.write().await;
            state.transport.record_mode = record_mode;
            Ok(format!("Record mode set to {:?}", record_mode))
        })
    }

    fn set_position(&mut self, seconds: f64) -> Result<String, TransportError> {
        let rt = tokio::runtime::Handle::try_current()
            .map_err(|_| TransportError::InvalidState("No tokio runtime".to_string()))?;

        rt.block_on(async {
            let mut state = self.state.write().await;
            let old_position = state.transport.playhead_position.time.to_seconds();
            let new_position = Position::from_seconds(seconds);

            state.transport.playhead_position = new_position;
            state.transport.edit_position = new_position;

            // Reset playback timing if currently playing
            if state.transport.is_playing() {
                state.playback_start_time = Some(Instant::now());
                state.playback_start_position = new_position;
            }

            let event = TransportEvent::PositionChanged {
                timestamp: chrono::Utc::now(),
                old_position,
                new_position: seconds,
            };
            let _ = self.event_sender.send(event);

            Ok(format!("Position set to {} seconds", seconds))
        })
    }

    fn get_tempo(&self) -> Result<Tempo, TransportError> {
        let rt = tokio::runtime::Handle::try_current()
            .map_err(|_| TransportError::InvalidState("No tokio runtime".to_string()))?;

        rt.block_on(async {
            let state = self.state.read().await;
            Ok(state.transport.tempo)
        })
    }

    fn get_time_signature(&self) -> Result<TimeSignature, TransportError> {
        let rt = tokio::runtime::Handle::try_current()
            .map_err(|_| TransportError::InvalidState("No tokio runtime".to_string()))?;

        rt.block_on(async {
            let state = self.state.read().await;
            Ok(state.transport.time_signature)
        })
    }

    fn get_record_mode(&self) -> Result<RecordMode, TransportError> {
        let rt = tokio::runtime::Handle::try_current()
            .map_err(|_| TransportError::InvalidState("No tokio runtime".to_string()))?;

        rt.block_on(async {
            let state = self.state.read().await;
            Ok(state.transport.record_mode)
        })
    }

    fn get_position(&self) -> Result<f64, TransportError> {
        let rt = tokio::runtime::Handle::try_current()
            .map_err(|_| TransportError::InvalidState("No tokio runtime".to_string()))?;

        rt.block_on(async {
            let state = self.state.read().await;
            Ok(state.calculate_current_position().time.to_seconds())
        })
    }

    fn is_playing(&self) -> Result<bool, TransportError> {
        let rt = tokio::runtime::Handle::try_current()
            .map_err(|_| TransportError::InvalidState("No tokio runtime".to_string()))?;

        rt.block_on(async {
            let state = self.state.read().await;
            Ok(state.transport.is_playing())
        })
    }

    fn is_recording(&self) -> Result<bool, TransportError> {
        let rt = tokio::runtime::Handle::try_current()
            .map_err(|_| TransportError::InvalidState("No tokio runtime".to_string()))?;

        rt.block_on(async {
            let state = self.state.read().await;
            Ok(state.transport.is_recording())
        })
    }

    fn get_transport(&self) -> Result<Transport, TransportError> {
        let rt = tokio::runtime::Handle::try_current()
            .map_err(|_| TransportError::InvalidState("No tokio runtime".to_string()))?;

        rt.block_on(async {
            let mut state = self.state.write().await;
            state.update_position(); // Ensure position is current
            Ok(state.transport.clone())
        })
    }

    fn is_ready(&self) -> Result<bool, TransportError> {
        Ok(self.update_handle.is_some())
    }
}

impl Drop for TransportApp {
    fn drop(&mut self) {
        self.stop();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tokio::time::{sleep, Duration};

    #[tokio::test]
    async fn test_transport_app_creation() {
        let app = TransportApp::with_defaults();
        let stats = app.get_stats().await;

        assert!(!stats.is_running);
        assert_eq!(stats.current_position, 0.0);
        assert_eq!(stats.effective_bpm, 120.0);
    }

    #[tokio::test]
    async fn test_transport_app_lifecycle() {
        let mut app = TransportApp::with_defaults();

        // Start the app
        assert!(app.start().await.is_ok());
        assert!(app.get_stats().await.is_running);

        // Stop the app
        app.stop();
        assert!(!app.get_stats().await.is_running);
    }

    #[tokio::test]
    async fn test_transport_playback() {
        let mut app = TransportApp::with_defaults();
        app.start().await.unwrap();

        // Test play
        assert!(app.play().is_ok());
        assert!(app.is_playing().unwrap());

        // Let it play for a short time
        sleep(Duration::from_millis(100)).await;

        // Position should have advanced
        let position = app.get_position().unwrap();
        assert!(position > 0.0);

        // Test stop
        assert!(app.stop().is_ok());
        assert!(!app.is_playing().unwrap());

        // Position should reset to 0
        let position = app.get_position().unwrap();
        assert_eq!(position, 0.0);
    }

    #[tokio::test]
    async fn test_transport_events() {
        let mut app = TransportApp::with_defaults();
        let mut receiver = app.subscribe();

        app.start().await.unwrap();

        // Play should generate an event
        app.play().unwrap();

        let event = receiver.recv().await.unwrap();
        match event {
            TransportEvent::PlaybackStarted { position, .. } => {
                assert_eq!(position, 0.0);
            }
            _ => panic!("Expected PlaybackStarted event"),
        }

        // Stop should generate an event
        app.stop().unwrap();

        let event = receiver.recv().await.unwrap();
        match event {
            TransportEvent::PlaybackStopped { .. } => {}
            _ => panic!("Expected PlaybackStopped event"),
        }
    }

    #[tokio::test]
    async fn test_tempo_changes() {
        let mut app = TransportApp::with_defaults();
        let mut receiver = app.subscribe();

        let new_tempo = Tempo::new(140.0);
        app.set_tempo(new_tempo).unwrap();

        let event = receiver.recv().await.unwrap();
        match event {
            TransportEvent::TempoChanged { old_tempo, new_tempo, .. } => {
                assert_eq!(old_tempo, 120.0);
                assert_eq!(new_tempo, 140.0);
            }
            _ => panic!("Expected TempoChanged event"),
        }

        assert_eq!(app.get_tempo().unwrap().bpm, 140.0);
    }

    #[tokio::test]
    async fn test_recording() {
        let mut app = TransportApp::with_defaults();

        // Start recording
        assert!(app.start_recording().is_ok());
        assert!(app.is_recording().unwrap());
        assert!(app.is_playing().unwrap()); // Recording implies playing

        // Stop recording
        assert!(app.stop_recording().is_ok());
        assert!(!app.is_recording().unwrap());
        assert!(app.is_playing().unwrap()); // Should still be playing

        // Toggle recording
        assert!(app.toggle_recording().is_ok());
        assert!(app.is_recording().unwrap());
    }
}
