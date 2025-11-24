//! Track name domain model
//! 
//! Represents the structured naming convention components for audio tracks and files.
//! This is a pure domain model with no parsing logic.

use crate::component::ComponentType;

/// Represents a parsed or constructed track name with all its components.
/// 
/// This structure follows the FTS naming convention and can represent both
/// DAW tracks and audio files.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct TrackName {
    /// Recording tag (e.g., "PASS-01", "TAKE-02", "REC-01")
    pub rec_tag: Option<String>,
    
    /// Group prefix (e.g., "D", "GTR", "Bass", "SY")
    pub group_prefix: Option<String>,
    
    /// Sub-types within the group (e.g., ["Kick"], ["SY"], ["Electric"])
    pub sub_type: Option<Vec<String>>,
    
    /// Performer name (e.g., "Cody", "Joshua", "Sarah")
    pub performer: Option<String>,
    
    /// Arrangement style (e.g., "Rhythm", "Solo", "Amb", "Crunch")
    pub arrangement: Option<String>,
    
    /// Section of the song (e.g., "Intro", "Verse", "Chorus", "Bridge")
    pub section: Option<String>,
    
    /// Layer information (e.g., "DBL", "OCT", "L", "R", "Stereo")
    pub layers: Option<String>,
    
    /// Multi-mic positions (e.g., ["Top", "Bottom"], ["In", "Out"], ["Close", "Room"])
    pub multi_mic: Option<Vec<String>>,
    
    /// Effect/send indicators (e.g., ["Verb"], ["Delay"], ["Chorus", "Verb"])
    pub effect: Option<Vec<String>>,
    
    /// Increment number for numbered instances (e.g., "1", "2" for Tom 1, Tom 2)
    pub increment: Option<String>,
    
    /// Channel information (e.g., "L", "R", "C", "Left", "Right")
    pub channel: Option<String>,
    
    /// Playlist identifier (e.g., ".1", ".2", ".3", ".A", ".B")
    pub playlist: Option<String>,
    
    /// Track type indicator (e.g., "BUS", "SUM", "MIDI", "DI", "NOFX")
    pub track_type: Option<String>,
    
    /// Words that didn't match any known patterns (for validation/debugging)
    pub unparsed_words: Option<Vec<String>>,
    
    /// File extension if parsed from a filename (e.g., ".wav", ".aiff", ".flac")
    /// This is typically not included in string formatting output
    pub file_extension: Option<String>,
}

impl TrackName {
    /// Creates a new, empty TrackName.
    pub fn new() -> Self {
        Self::default()
    }
    
    /// Check if the track name is empty (has no meaningful components)
    pub fn is_empty(&self) -> bool {
        self.rec_tag.is_none()
            && self.group_prefix.is_none()
            && self.sub_type.is_none()
            && self.performer.is_none()
            && self.arrangement.is_none()
            && self.section.is_none()
            && self.layers.is_none()
            && self.multi_mic.is_none()
            && self.effect.is_none()
            && self.increment.is_none()
            && self.channel.is_none()
            && self.playlist.is_none()
            && self.track_type.is_none()
            && self.unparsed_words.is_none()
    }
    
    /// Check if a specific component type is present
    pub fn has_component(&self, component_type: ComponentType) -> bool {
        match component_type {
            ComponentType::RecTag => self.rec_tag.is_some(),
            ComponentType::GroupPrefix => self.group_prefix.is_some(),
            ComponentType::SubType => self.sub_type.is_some(),
            ComponentType::Performer => self.performer.is_some(),
            ComponentType::Arrangement => self.arrangement.is_some(),
            ComponentType::Section => self.section.is_some(),
            ComponentType::Layers => self.layers.is_some(),
            ComponentType::MultiMic => self.multi_mic.is_some(),
            ComponentType::Effect => self.effect.is_some(),
            ComponentType::Increment => self.increment.is_some(),
            ComponentType::Channel => self.channel.is_some(),
            ComponentType::Playlist => self.playlist.is_some(),
            ComponentType::TrackType => self.track_type.is_some(),
            ComponentType::Unknown => false,
        }
    }
    
    /// Get the value of a component as a string (for display purposes)
    pub fn get_component_value(&self, component_type: ComponentType) -> Option<String> {
        match component_type {
            ComponentType::RecTag => self.rec_tag.clone(),
            ComponentType::GroupPrefix => self.group_prefix.clone(),
            ComponentType::SubType => self.sub_type.as_ref().map(|v| v.join(" ")),
            ComponentType::Performer => self.performer.clone(),
            ComponentType::Arrangement => self.arrangement.clone(),
            ComponentType::Section => self.section.clone(),
            ComponentType::Layers => self.layers.clone(),
            ComponentType::MultiMic => self.multi_mic.as_ref().map(|v| v.join(", ")),
            ComponentType::Effect => self.effect.as_ref().map(|v| v.join(", ")),
            ComponentType::Increment => self.increment.clone(),
            ComponentType::Channel => self.channel.clone(),
            ComponentType::Playlist => self.playlist.clone(),
            ComponentType::TrackType => self.track_type.clone(),
            ComponentType::Unknown => None,
        }
    }
    
    /// Count the number of components that are present
    pub fn component_count(&self) -> usize {
        let mut count = 0;
        if self.rec_tag.is_some() { count += 1; }
        if self.group_prefix.is_some() { count += 1; }
        if self.sub_type.is_some() { count += 1; }
        if self.performer.is_some() { count += 1; }
        if self.arrangement.is_some() { count += 1; }
        if self.section.is_some() { count += 1; }
        if self.layers.is_some() { count += 1; }
        if self.multi_mic.is_some() { count += 1; }
        if self.effect.is_some() { count += 1; }
        if self.increment.is_some() { count += 1; }
        if self.channel.is_some() { count += 1; }
        if self.playlist.is_some() { count += 1; }
        if self.track_type.is_some() { count += 1; }
        count
    }
    
    /// Basic validation: checks if the track name has at least one meaningful component
    /// 
    /// This is a simple validation that can be expanded by parsers with more
    /// domain-specific validation logic.
    pub fn is_valid(&self) -> bool {
        !self.is_empty()
    }
    
    /// Check if this track name represents a valid file (has extension or meaningful content)
    pub fn is_valid_file(&self) -> bool {
        self.file_extension.is_some() || !self.is_empty()
    }
}

