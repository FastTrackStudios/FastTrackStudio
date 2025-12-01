//! Component types for track naming convention

/// Enum representing the different components of a track name.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ComponentType {
    /// Recording tag (e.g., "PASS-01", "TAKE-02")
    RecTag,
    
    /// Group prefix (e.g., "D", "GTR", "Bass")
    GroupPrefix,
    
    /// Sub-types within the group (e.g., "Kick", "Snare", "Electric")
    SubType,
    
    /// Performer name (e.g., "Cody", "Joshua")
    Performer,
    
    /// Arrangement style (e.g., "Rhythm", "Solo", "Crunch")
    Arrangement,
    
    /// Section of the song (e.g., "Intro", "Verse", "Chorus")
    Section,
    
    /// Layer information (e.g., "DBL", "OCT", "L", "R")
    Layers,
    
    /// Multi-mic positions (e.g., "Top", "Bottom", "In", "Out")
    MultiMic,
    
    /// Effect/send indicators (e.g., "Verb", "Delay", "Chorus")
    Effect,
    
    /// Increment number for numbered instances (e.g., "1", "2")
    Increment,
    
    /// Channel information (e.g., "L", "R", "C", "Left", "Right")
    Channel,
    
    /// Playlist identifier (e.g., ".1", ".2", ".A")
    Playlist,
    
    /// Track type indicator (e.g., "BUS", "SUM", "MIDI", "DI")
    TrackType,
    
    /// Unknown/unrecognized component
    Unknown,
}

impl ComponentType {
    /// Get a string representation of the component type
    pub fn as_str(&self) -> &'static str {
        match self {
            ComponentType::RecTag => "RecTag",
            ComponentType::GroupPrefix => "GroupPrefix",
            ComponentType::SubType => "SubType",
            ComponentType::Performer => "Performer",
            ComponentType::Arrangement => "Arrangement",
            ComponentType::Section => "Section",
            ComponentType::Layers => "Layers",
            ComponentType::MultiMic => "MultiMic",
            ComponentType::Effect => "Effect",
            ComponentType::Increment => "Increment",
            ComponentType::Channel => "Channel",
            ComponentType::Playlist => "Playlist",
            ComponentType::TrackType => "TrackType",
            ComponentType::Unknown => "Unknown",
        }
    }
}

