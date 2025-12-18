//! Room template generator and matcher implementation

use crate::smart_template::matching::matcher::{Matcher, MatchResult, MatchType};
use crate::smart_template::core::traits::TemplateGenerator;
use crate::smart_template::core::template::Template;
use crate::smart_template::helpers::track_helpers::create_track;
use crate::smart_template::naming::item_properties::ItemProperties;
use crate::smart_template::config::template_config::TemplateConfig;
use crate::smart_template::shared::GroupMode;

/// Room template generator
pub struct RoomTemplateGenerator;

impl RoomTemplateGenerator {
    /// Create a new Room template generator
    pub fn new() -> Self {
        Self
    }
    
    /// Generate the default Room track structure with mode assignments
    /// 
    /// Structure:
    /// ```
    /// Room (BUS)
    /// ├── Rooms
    /// ├── Rooms Far
    /// └── Rooms Mono
    /// ```
    pub fn generate_room_structure() -> Template {
        let mut tracks = Vec::new();
        
        // Room (BUS) - parent
        // Visible in all modes (empty modes = all modes)
        tracks.push(create_track("Room", Some("BUS"), None, &[]));
        
        // Rooms - child of Room (BUS)
        tracks.push(create_track(
            "Rooms",
            None,
            Some("Room"),
            &[GroupMode::Full, GroupMode::Recording],
        ));
        
        // Rooms Far - child of Room (BUS)
        tracks.push(create_track(
            "Rooms Far",
            None,
            Some("Room"),
            &[GroupMode::Full, GroupMode::Recording],
        ));
        
        // Rooms Mono - child of Room (BUS)
        tracks.push(create_track(
            "Rooms Mono",
            None,
            Some("Room"),
            &[GroupMode::Full, GroupMode::Recording],
        ));
        
        Template {
            name: "Room".to_string(),
            tracks,
        }
    }
}

impl TemplateGenerator for RoomTemplateGenerator {
    type Config = TemplateConfig;
    type Error = RoomGeneratorError;
    
    fn generate(&self, _config: &Self::Config) -> Result<Template, Self::Error> {
        Ok(Self::generate_room_structure())
    }
    
    fn name(&self) -> &str {
        "room"
    }
}

impl Default for RoomTemplateGenerator {
    fn default() -> Self {
        Self::new()
    }
}

/// Room generator error
#[derive(Debug, thiserror::Error)]
pub enum RoomGeneratorError {
    #[error("Generator error: {0}")]
    Other(String),
}

/// Room matcher
pub struct RoomMatcher {
    template: Template,
}

impl RoomMatcher {
    /// Create a new Room matcher with the default template
    pub fn new() -> Self {
        Self {
            template: RoomTemplateGenerator::generate_room_structure(),
        }
    }
    
    /// Create a matcher with a custom template
    pub fn with_template(template: Template) -> Self {
        Self { template }
    }
}

impl Matcher for RoomMatcher {
    type TrackName = ItemProperties;
    type Error = RoomMatchError;
    
    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        // Build search name from multi-mic descriptor
        let search_name = if let Some(multi_mic) = &track_name.multi_mic {
            if let Some(first) = multi_mic.first() {
                format!("Rooms {}", first)
            } else {
                "Rooms".to_string()
            }
        } else {
            "Rooms".to_string()
        };
        
        // Look for exact match
        if let Some(track) = self.template.tracks.iter()
            .find(|t| t.name.eq_ignore_ascii_case(&search_name)) {
            return Some(MatchResult {
                track_name: track.name.clone(),
                match_type: MatchType::Exact,
                score: 100,
                use_takes: false,
            });
        }
        
        // Try partial match
        if let Some(track) = self.template.tracks.iter()
            .find(|t| t.name.to_lowercase().contains(&search_name.to_lowercase())) {
            return Some(MatchResult {
                track_name: track.name.clone(),
                match_type: MatchType::Partial,
                score: 60,
                use_takes: false,
            });
        }
        
        // Check for playlist variation
        if track_name.playlist.is_some() {
            // If we have a playlist, try matching without it
            if let Some(original) = &track_name.original_name {
                let name_without_playlist = original
                    .trim_end_matches(|c: char| c == '.' || c.is_alphanumeric());
                
                if let Some(track) = self.template.tracks.iter()
                    .find(|t| t.name.to_lowercase() == name_without_playlist.to_lowercase()) {
                    return Some(MatchResult {
                        track_name: track.name.clone(),
                        match_type: MatchType::PlaylistVariation,
                        score: 90,
                        use_takes: true,
                    });
                }
            }
        }
        
        None
    }
    
    fn find_or_create_track(&mut self, track_name: &Self::TrackName, base_name: Option<&str>) -> Result<(String, bool), Self::Error> {
        // First try to find a match
        if let Some(result) = self.find_best_match(track_name) {
            return Ok((result.track_name, result.use_takes));
        }
        
        // No match found, create a new track name
        let new_track_name = base_name
            .map(|s| s.to_string())
            .unwrap_or_else(|| {
                if let Some(multi_mic) = &track_name.multi_mic {
                    if let Some(first) = multi_mic.first() {
                        format!("Rooms {}", first)
                    } else {
                        "Rooms".to_string()
                    }
                } else {
                    "Rooms".to_string()
                }
            });
        
        // Add the new track to the template
        self.template.tracks.push(create_track(
            &new_track_name,
            None,
            Some("Room"),
            &[], // Default to all modes
        ));
        
        Ok((new_track_name, false))
    }
    
    fn name(&self) -> &str {
        "room"
    }
}

impl Default for RoomMatcher {
    fn default() -> Self {
        Self::new()
    }
}

/// Room match error
#[derive(Debug, thiserror::Error)]
pub enum RoomMatchError {
    #[error("Match error: {0}")]
    Other(String),
}
