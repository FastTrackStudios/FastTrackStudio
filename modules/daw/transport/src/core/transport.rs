//! Transport Domain Model
//!
//! This module defines the core Transport struct and related enums for managing
//! playback state, recording, and transport control in a DAW.

use primitives::{Position, TimeSelection, TimeSignature};

/// Playback state enumeration
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize )]
pub enum PlayState {
    /// Transport is stopped
    Stopped,
    /// Transport is playing
    Playing,
    /// Transport is paused (maintains position)
    Paused,
    /// Transport is recording (implies playing)
    Recording,
}

impl Default for PlayState {
    fn default() -> Self {
        Self::Stopped
    }
}

/// Recording mode enumeration
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize )]
pub enum RecordMode {
    Normal,
    TimeSelection,
    Item,
}

impl Default for RecordMode {
    fn default() -> Self {
        Self::Normal
    }
}

/// Tempo structure
#[derive(Debug, Clone, Copy, PartialEq, serde::Serialize, serde::Deserialize )]
pub struct Tempo {
    pub bpm: f64,
}

impl Tempo {
    pub fn new(bpm: f64) -> Self {
        Self { bpm }
    }
    pub fn is_valid(&self) -> bool {
        self.bpm > 0.0 && self.bpm <= 999.0
    }
}

impl Default for Tempo {
    fn default() -> Self {
        Self { bpm: 120.0 }
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize,  PartialEq)]
pub struct Transport {
    pub play_state: PlayState,
    pub record_mode: RecordMode,
    pub looping: bool,
    pub tempo: Tempo,
    pub playrate: f64,
    pub time_signature: TimeSignature,
    pub playhead_position: Position,
    pub edit_position: Position,
    pub time_selection: Option<TimeSelection>,
    pub loop_selection: Option<TimeSelection>,
    pub input_monitoring: bool,
    pub preroll: f64,
    pub postroll: f64,
}

impl Transport {
    pub fn new() -> Self {
        Self {
            play_state: PlayState::default(),
            record_mode: RecordMode::default(),
            looping: false,
            tempo: Tempo::default(),
            playrate: 1.0,
            time_signature: TimeSignature::default(),
            playhead_position: Position::start(),
            edit_position: Position::start(),
            time_selection: None,
            loop_selection: None,
            input_monitoring: false,
            preroll: 0.0,
            postroll: 0.0,
        }
    }

    pub fn is_playing(&self) -> bool {
        matches!(self.play_state, PlayState::Playing | PlayState::Recording)
    }

    pub fn is_recording(&self) -> bool {
        matches!(self.play_state, PlayState::Recording)
    }

    pub fn is_stopped(&self) -> bool {
        matches!(self.play_state, PlayState::Stopped)
    }

    pub fn is_paused(&self) -> bool {
        matches!(self.play_state, PlayState::Paused)
    }

    pub fn set_tempo(&mut self, tempo: Tempo) -> Result<(), String> {
        if !tempo.is_valid() {
            return Err(format!("Invalid tempo: {} BPM", tempo.bpm));
        }
        self.tempo = tempo;
        Ok(())
    }

    pub fn set_playrate(&mut self, rate: f64) -> Result<(), String> {
        if rate <= 0.0 || rate > 4.0 {
            return Err(format!("Invalid playrate: {}", rate));
        }
        self.playrate = rate;
        Ok(())
    }

    pub fn effective_bpm(&self) -> f64 {
        self.tempo.bpm * self.playrate
    }

    pub fn reset(&mut self) {
        self.play_state = PlayState::Stopped;
        self.playhead_position = Position::start();
        self.edit_position = Position::start();
        self.time_selection = None;
    }
}

impl Default for Transport {
    fn default() -> Self {
        Self::new()
    }
}

pub trait TransportActions: Send + Sync {
    fn play(&mut self) -> Result<String, crate::TransportError>;
    fn pause(&mut self) -> Result<String, crate::TransportError>;
    fn stop(&mut self) -> Result<String, crate::TransportError>;
    fn play_pause(&mut self) -> Result<String, crate::TransportError>;
    fn play_stop(&mut self) -> Result<String, crate::TransportError>;
    fn start_recording(&mut self) -> Result<String, crate::TransportError>;
    fn stop_recording(&mut self) -> Result<String, crate::TransportError>;
    fn toggle_recording(&mut self) -> Result<String, crate::TransportError>;
    fn set_tempo(&mut self, tempo: Tempo) -> Result<String, crate::TransportError>;
    fn set_time_signature(
        &mut self,
        time_signature: TimeSignature,
    ) -> Result<String, crate::TransportError>;
    fn set_record_mode(&mut self, record_mode: RecordMode)
    -> Result<String, crate::TransportError>;
    fn set_position(&mut self, seconds: f64) -> Result<String, crate::TransportError>;
    fn get_tempo(&self) -> Result<Tempo, crate::TransportError>;
    fn get_time_signature(&self) -> Result<TimeSignature, crate::TransportError>;
    fn get_record_mode(&self) -> Result<RecordMode, crate::TransportError>;
    fn get_position(&self) -> Result<f64, crate::TransportError>;
    fn is_playing(&self) -> Result<bool, crate::TransportError>;
    fn is_recording(&self) -> Result<bool, crate::TransportError>;
    fn get_transport(&self) -> Result<Transport, crate::TransportError>;
    fn is_ready(&self) -> Result<bool, crate::TransportError>;
}

// Implement TransportActions for Transport struct itself (for direct usage)
impl TransportActions for Transport {
    fn play(&mut self) -> Result<String, crate::TransportError> {
        self.play_state = PlayState::Playing;
        Ok("Playback started".to_string())
    }

    fn pause(&mut self) -> Result<String, crate::TransportError> {
        self.play_state = PlayState::Paused;
        Ok("Playback paused".to_string())
    }

    fn stop(&mut self) -> Result<String, crate::TransportError> {
        self.reset();
        Ok("Playback stopped".to_string())
    }

    fn play_pause(&mut self) -> Result<String, crate::TransportError> {
        if self.is_playing() {
            self.pause()
        } else {
            self.play()
        }
    }

    fn play_stop(&mut self) -> Result<String, crate::TransportError> {
        if self.is_playing() {
            self.stop()
        } else {
            self.play()
        }
    }

    fn start_recording(&mut self) -> Result<String, crate::TransportError> {
        self.play_state = PlayState::Recording;
        Ok("Recording started".to_string())
    }

    fn stop_recording(&mut self) -> Result<String, crate::TransportError> {
        self.play_state = PlayState::Playing;
        Ok("Recording stopped".to_string())
    }

    fn toggle_recording(&mut self) -> Result<String, crate::TransportError> {
        if self.is_recording() {
            self.stop_recording()
        } else {
            self.start_recording()
        }
    }

    fn set_tempo(&mut self, tempo: Tempo) -> Result<String, crate::TransportError> {
        if !tempo.is_valid() {
            return Err(crate::TransportError::InvalidTempo(format!(
                "{} BPM",
                tempo.bpm
            )));
        }
        self.tempo = tempo;
        Ok(format!("Tempo set to {} BPM", tempo.bpm))
    }

    fn set_time_signature(
        &mut self,
        time_signature: TimeSignature,
    ) -> Result<String, crate::TransportError> {
        self.time_signature = time_signature;
        Ok(format!(
            "Time signature set to {}/{}",
            time_signature.numerator, time_signature.denominator
        ))
    }

    fn set_record_mode(
        &mut self,
        record_mode: RecordMode,
    ) -> Result<String, crate::TransportError> {
        self.record_mode = record_mode;
        Ok(format!("Record mode set to {:?}", record_mode))
    }

    fn set_position(&mut self, seconds: f64) -> Result<String, crate::TransportError> {
        self.playhead_position = Position::from_seconds(seconds);
        Ok(format!("Position set to {} seconds", seconds))
    }

    fn get_tempo(&self) -> Result<Tempo, crate::TransportError> {
        Ok(self.tempo)
    }

    fn get_time_signature(&self) -> Result<TimeSignature, crate::TransportError> {
        Ok(self.time_signature)
    }

    fn get_record_mode(&self) -> Result<RecordMode, crate::TransportError> {
        Ok(self.record_mode)
    }

    fn get_position(&self) -> Result<f64, crate::TransportError> {
        Ok(self.playhead_position.time.to_seconds())
    }

    fn is_playing(&self) -> Result<bool, crate::TransportError> {
        Ok(self.is_playing())
    }

    fn is_recording(&self) -> Result<bool, crate::TransportError> {
        Ok(self.is_recording())
    }

    fn get_transport(&self) -> Result<Transport, crate::TransportError> {
        Ok(self.clone())
    }

    fn is_ready(&self) -> Result<bool, crate::TransportError> {
        Ok(true)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_transport_new() {
        let transport = Transport::new();
        assert_eq!(transport.play_state, PlayState::Stopped);
        assert_eq!(transport.tempo.bpm, 120.0);
        assert!(!transport.is_playing());
        assert!(!transport.is_recording());
    }

    #[test]
    fn test_play_state_queries() {
        let mut transport = Transport::new();

        // Test stopped state
        assert!(transport.is_stopped());
        assert!(!transport.is_playing());
        assert!(!transport.is_recording());

        // Test playing state
        transport.play_state = PlayState::Playing;
        assert!(!transport.is_stopped());
        assert!(transport.is_playing());
        assert!(!transport.is_recording());

        // Test recording state
        transport.play_state = PlayState::Recording;
        assert!(!transport.is_stopped());
        assert!(transport.is_playing());
        assert!(transport.is_recording());

        // Test paused state
        transport.play_state = PlayState::Paused;
        assert!(!transport.is_stopped());
        assert!(!transport.is_playing());
        assert!(!transport.is_recording());
        assert!(transport.is_paused());
    }

    #[test]
    fn test_tempo_validation() {
        let mut transport = Transport::new();

        // Valid tempo
        let valid_tempo = Tempo::new(140.0);
        assert!(transport.set_tempo(valid_tempo).is_ok());
        assert_eq!(transport.tempo.bpm, 140.0);

        // Invalid tempo - too high
        let invalid_tempo = Tempo::new(1000.0);
        assert!(transport.set_tempo(invalid_tempo).is_err());

        // Invalid tempo - negative
        let invalid_tempo = Tempo::new(-10.0);
        assert!(transport.set_tempo(invalid_tempo).is_err());
    }

    #[test]
    fn test_playrate() {
        let mut transport = Transport::new();

        // Valid playrate
        assert!(transport.set_playrate(0.5).is_ok());
        assert_eq!(transport.playrate, 0.5);

        // Test effective BPM
        transport.tempo = Tempo::new(120.0);
        transport.playrate = 2.0;
        assert_eq!(transport.effective_bpm(), 240.0);

        // Invalid playrate
        assert!(transport.set_playrate(0.0).is_err());
        assert!(transport.set_playrate(-1.0).is_err());
        assert!(transport.set_playrate(5.0).is_err());
    }

    #[test]
    fn test_reset() {
        let mut transport = Transport::new();

        // Modify state
        transport.play_state = PlayState::Playing;
        transport.playhead_position = Position::from_seconds(30.0);
        transport.time_selection = Some(TimeSelection::from_seconds(10.0, 20.0));

        // Reset
        transport.reset();

        // Verify reset
        assert_eq!(transport.play_state, PlayState::Stopped);
        assert_eq!(transport.playhead_position, Position::start());
        assert_eq!(transport.edit_position, Position::start());
        assert!(transport.time_selection.is_none());
    }

    #[tokio::test]
    async fn test_transport_actions_impl() {
        let mut transport = Transport::new();

        // Test sync methods
        assert!(transport.play().is_ok());
        assert!(transport.is_playing());

        assert!(transport.pause().is_ok());
        assert!(!transport.is_playing());

        assert!(transport.stop().is_ok());
        assert!(!transport.is_playing());

        // Test configuration methods
        let tempo = Tempo::new(140.0);
        assert!(transport.set_tempo(tempo).is_ok());
        assert_eq!(transport.get_tempo().unwrap().bpm, 140.0);
    }

    #[test]
    fn test_defaults() {
        let play_state = PlayState::default();
        assert_eq!(play_state, PlayState::Stopped);

        let record_mode = RecordMode::default();
        assert_eq!(record_mode, RecordMode::Normal);

        let tempo = Tempo::default();
        assert_eq!(tempo.bpm, 120.0);
        assert!(tempo.is_valid());
    }
}
