use crate::primitives::{Position, TimeSelection, TimeSignature};
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum PlayState {
    Stopped,
    Playing,
    Paused,
    Recording,
}

impl Default for PlayState {
    fn default() -> Self {
        Self::Stopped
    }
}

impl fmt::Display for PlayState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PlayState::Stopped => write!(f, "Stopped"),
            PlayState::Playing => write!(f, "Playing"),
            PlayState::Paused => write!(f, "Paused"),
            PlayState::Recording => write!(f, "Recording"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
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

impl fmt::Display for RecordMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RecordMode::Normal => write!(f, "Normal"),
            RecordMode::TimeSelection => write!(f, "Time Selection"),
            RecordMode::Item => write!(f, "Item"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, serde::Serialize, serde::Deserialize)]
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

    /// Parse tempo from string (e.g., "120bpm", "120")
    pub fn parse(s: &str) -> Option<Self> {
        let s = s.trim().to_lowercase();
        let s = s.strip_suffix("bpm").unwrap_or(&s).trim();

        s.parse::<f64>().ok().map(|bpm| Tempo { bpm })
    }
}

impl Default for Tempo {
    fn default() -> Self {
        Self { bpm: 120.0 }
    }
}

impl fmt::Display for Tempo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:.1} BPM", self.bpm)
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq)]
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

impl fmt::Display for Transport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Transport {{ state: {}, record_mode: {}, looping: {}, tempo: {}, playrate: {:.2}x, time_sig: {}, playhead: {} ({:.3}s), edit: {} ({:.3}s) }}",
            self.play_state,
            self.record_mode,
            self.looping,
            self.tempo,
            self.playrate,
            self.time_signature,
            self.playhead_position.musical_position_string(),
            self.playhead_position.time.to_seconds(),
            self.edit_position.musical_position_string(),
            self.edit_position.time.to_seconds()
        )
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
