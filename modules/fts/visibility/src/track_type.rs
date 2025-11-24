//! Track type domain model
//! 
//! Track types represent specific configurations within an instrument group.
//! For example, a Guitar group might have:
//! - "Mic Input" track type
//! - "Modeler Stereo" track type
//! - "DI" track type
//! - "Stereo No FX" track type

use super::version::VersionId;

/// Unique identifier for a track type
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TrackTypeId(pub String);

impl TrackTypeId {
    pub fn new(id: impl Into<String>) -> Self {
        Self(id.into())
    }
    
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<&str> for TrackTypeId {
    fn from(s: &str) -> Self {
        Self::new(s)
    }
}

/// Category classification for track types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TrackTypeCategory {
    /// Physical microphone input
    MicInput,
    
    /// Modeler/amp sim input (mono or stereo)
    ModelerInput,
    
    /// Direct instrument input (DI)
    DirectInput,
    
    /// Line input
    LineInput,
    
    /// MIDI track
    MidiTrack,
    
    /// Bus/Group track
    Bus,
    
    /// Sum/Aux track
    Sum,
    
    /// Send track
    Send,
    
    /// Other/Unspecified
    Other,
}

impl TrackTypeCategory {
    pub fn as_str(&self) -> &'static str {
        match self {
            TrackTypeCategory::MicInput => "Mic Input",
            TrackTypeCategory::ModelerInput => "Modeler Input",
            TrackTypeCategory::DirectInput => "Direct Input",
            TrackTypeCategory::LineInput => "Line Input",
            TrackTypeCategory::MidiTrack => "MIDI",
            TrackTypeCategory::Bus => "Bus",
            TrackTypeCategory::Sum => "Sum",
            TrackTypeCategory::Send => "Send",
            TrackTypeCategory::Other => "Other",
        }
    }
}

/// Represents a track type within an instrument group
#[derive(Debug, Clone)]
pub struct TrackType {
    /// Unique identifier
    pub id: TrackTypeId,
    
    /// Human-readable name
    pub name: String,
    
    /// Category classification
    pub category: TrackTypeCategory,
    
    /// Whether this is a stereo pair (needs intelligent grouping)
    pub is_stereo_pair: bool,
    
    /// Name pattern for the partner track (if stereo pair)
    pub stereo_partner_pattern: Option<String>,
    
    /// Default channel count (1 = mono, 2 = stereo, etc.)
    pub default_channels: u8,
    
    /// Available versions for this track type
    pub versions: Vec<VersionId>,
    
    /// Description/notes
    pub description: Option<String>,
}

impl TrackType {
    pub fn new(
        id: impl Into<TrackTypeId>,
        name: impl Into<String>,
        category: TrackTypeCategory,
    ) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            category,
            is_stereo_pair: false,
            stereo_partner_pattern: None,
            default_channels: 2, // Default to stereo
            versions: Vec::new(),
            description: None,
        }
    }
    
    pub fn add_version(&mut self, version_id: VersionId) {
        self.versions.push(version_id);
    }
    
    pub fn with_stereo_pair(mut self, partner_pattern: impl Into<String>) -> Self {
        self.is_stereo_pair = true;
        self.stereo_partner_pattern = Some(partner_pattern.into());
        self
    }
    
    pub fn with_channels(mut self, channels: u8) -> Self {
        self.default_channels = channels;
        self
    }
}

/// Example: Guitar track types
pub mod examples {
    use super::*;
    
    pub fn guitar_mic_input() -> TrackType {
        TrackType::new(
            "gtr-mic",
            "Guitar Mic Input",
            TrackTypeCategory::MicInput,
        )
        .with_channels(1)
    }
    
    pub fn guitar_modeler_stereo() -> TrackType {
        TrackType::new(
            "gtr-modeler-stereo",
            "Guitar Modeler Stereo",
            TrackTypeCategory::ModelerInput,
        )
        .with_channels(2)
    }
    
    pub fn guitar_modeler_with_di() -> TrackType {
        TrackType::new(
            "gtr-modeler-di",
            "Guitar Modeler with DI",
            TrackTypeCategory::ModelerInput,
        )
        .with_channels(2)
        .with_stereo_pair("DI") // Partner would be "DI" track
    }
    
    pub fn guitar_di() -> TrackType {
        TrackType::new(
            "gtr-di",
            "Guitar DI",
            TrackTypeCategory::DirectInput,
        )
        .with_channels(1)
    }
    
    pub fn guitar_stereo_no_fx() -> TrackType {
        TrackType::new(
            "gtr-stereo-no-fx",
            "Guitar Stereo No FX",
            TrackTypeCategory::ModelerInput,
        )
        .with_channels(2)
    }
}

