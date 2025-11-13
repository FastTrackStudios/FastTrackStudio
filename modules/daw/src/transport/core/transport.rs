//! Transport Domain Model
//!
//! This module defines the core Transport struct and related enums for managing
//! playback state, recording, and transport control in a DAW.

use crate::primitives::{Position, TimeSelection, TimeSignature};

/// Playback state enumeration
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum RecordMode {
    /// Normal recording mode
    Normal,
    /// Record only within time selection
    TimeSelection,
    /// Record into selected item/take
    Item,
}

impl Default for RecordMode {
    fn default() -> Self {
        Self::Normal
    }
}

/// Tempo structure
#[derive(Debug, Clone, Copy, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct Tempo {
    /// Beats per minute
    pub bpm: f64,
}

impl Tempo {
    /// Create a new tempo
    pub fn new(bpm: f64) -> Self {
        Self { bpm }
    }

    /// Validate tempo is within reasonable range
    pub fn is_valid(&self) -> bool {
        self.bpm > 0.0 && self.bpm <= 999.0
    }
}

impl Default for Tempo {
    fn default() -> Self {
        Self { bpm: 120.0 }
    }
}

/// Main Transport state structure
///
/// This is the core state container for all transport-related information
/// in the DAW. It manages playback state, position, timing, and recording.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[derive(PartialEq)]
pub struct Transport {
    /// Current playback state
    pub play_state: PlayState,

    /// Current recording mode
    pub record_mode: RecordMode,

    /// Whether transport is looping
    pub looping: bool,

    /// Current tempo (BPM)
    pub tempo: Tempo,

    /// Playback rate multiplier (1.0 = normal speed)
    pub playrate: f64,

    /// Current time signature
    pub time_signature: TimeSignature,

    /// Current playhead position
    pub playhead_position: Position,

    /// Edit cursor position (where edits will be inserted)
    pub edit_position: Position,

    /// Time selection range (if any)
    pub time_selection: Option<TimeSelection>,

    /// Loop points (if looping is enabled)
    pub loop_selection: Option<TimeSelection>,

    /// Whether input monitoring is enabled
    pub input_monitoring: bool,

    /// Pre-roll time in seconds
    pub preroll: f64,

    /// Post-roll time in seconds
    pub postroll: f64,
}

impl Transport {
    /// Create a new transport with default settings
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

    /// Check if transport is currently playing
    pub fn is_playing(&self) -> bool {
        matches!(self.play_state, PlayState::Playing | PlayState::Recording)
    }

    /// Check if transport is currently recording
    pub fn is_recording(&self) -> bool {
        matches!(self.play_state, PlayState::Recording)
    }

    /// Check if transport is stopped
    pub fn is_stopped(&self) -> bool {
        matches!(self.play_state, PlayState::Stopped)
    }

    /// Check if transport is paused
    pub fn is_paused(&self) -> bool {
        matches!(self.play_state, PlayState::Paused)
    }

    /// Set the tempo, validating it's within acceptable range
    pub fn set_tempo(&mut self, tempo: Tempo) -> Result<(), String> {
        if !tempo.is_valid() {
            return Err(format!("Invalid tempo: {} BPM", tempo.bpm));
        }
        self.tempo = tempo;
        Ok(())
    }

    /// Set the playback rate
    pub fn set_playrate(&mut self, rate: f64) -> Result<(), String> {
        if rate <= 0.0 || rate > 4.0 {
            return Err(format!("Invalid playrate: {}", rate));
        }
        self.playrate = rate;
        Ok(())
    }

    /// Get the effective BPM (accounting for playrate)
    pub fn effective_bpm(&self) -> f64 {
        self.tempo.bpm * self.playrate
    }

    /// Reset transport to initial state
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
