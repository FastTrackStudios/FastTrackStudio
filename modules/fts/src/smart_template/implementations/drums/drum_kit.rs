//! Drum Kit template generator and matcher implementation
//!
//! The drum_kit is a parent group that contains kick (and eventually snare, toms, etc.)
//! The visibility manager works at the drum_kit level.

use crate::smart_template::matching::matcher::{Matcher, MatchResult, MatchType};
use crate::smart_template::core::traits::TemplateGenerator;
use crate::smart_template::core::template::Template;
use crate::smart_template::helpers::track_helpers::{create_track, TrackExt};
use crate::smart_template::config::template_config::TemplateConfig;
use crate::smart_template::shared::GroupMode;
use crate::smart_template::implementations::drums::kick::KickTemplateGenerator;
use crate::smart_template::implementations::drums::snare::SnareTemplateGenerator;
use crate::smart_template::implementations::drums::tom::TomTemplateGenerator;
use crate::smart_template::implementations::drums::cymbals::CymbalsTemplateGenerator;
use crate::smart_template::implementations::drums::room::RoomTemplateGenerator;

/// Drum Kit template generator
pub struct DrumKitTemplateGenerator;

impl DrumKitTemplateGenerator {
    /// Create a new Drum Kit template generator
    pub fn new() -> Self {
        Self
    }
    
    /// Generate the default Drum Kit track structure
    /// This includes the drum_kit parent track and all kick tracks as children
    pub fn generate_drum_kit_structure() -> Template {
        let mut tracks = Vec::new();
        
        // Drum Kit (BUS) - top-level parent
        // Visible in all modes (empty modes = all modes)
        tracks.push(create_track("Drum Kit", Some("BUS"), None, &[]));
        
        // Get kick tracks and add them as children of Drum Kit
        let kick_template = KickTemplateGenerator::generate_kick_structure();
        
        // Add all kick tracks, but change their parent to "Drum Kit" instead of "Kick"
        // The Kick (BUS) becomes a child of Drum Kit (BUS)
        for mut kick_track in kick_template.tracks {
            // If the kick track has no parent, it's the Kick (BUS) - make it a child of Drum Kit
            if kick_track.parent_name().is_none() {
                kick_track.set_parent_name("Drum Kit");
            }
            // If the kick track's parent is "Kick", keep it as is (it's a child of Kick (BUS))
            // Otherwise, if it's already a child, keep the hierarchy
            
            tracks.push(kick_track);
        }
        
        // Get snare tracks and add them as children of Drum Kit
        let snare_template = SnareTemplateGenerator::generate_snare_structure();
        
        // Add all snare tracks, but change their parent to "Drum Kit" instead of "Snare"
        // The Snare (BUS) becomes a child of Drum Kit (BUS)
        for mut snare_track in snare_template.tracks {
            // If the snare track has no parent, it's the Snare (BUS) - make it a child of Drum Kit
            if snare_track.parent_name().is_none() {
                snare_track.set_parent_name("Drum Kit");
            }
            // If the snare track's parent is "Snare", keep it as is (it's a child of Snare (BUS))
            
            tracks.push(snare_track);
        }
        
        // Get tom tracks and add them as children of Drum Kit
        let tom_template = TomTemplateGenerator::generate_tom_structure();
        
        // Add all tom tracks, but change their parent to "Drum Kit" instead of "Tom"
        // The Tom (BUS) becomes a child of Drum Kit (BUS)
        for mut tom_track in tom_template.tracks {
            // If the tom track has no parent, it's the Tom (BUS) - make it a child of Drum Kit
            if tom_track.parent_name().is_none() {
                tom_track.set_parent_name("Drum Kit");
            }
            // If the tom track's parent is "Tom", keep it as is (it's a child of Tom (BUS))
            
            tracks.push(tom_track);
        }
        
        // Get cymbals tracks and add them as children of Drum Kit
        let cymbals_template = CymbalsTemplateGenerator::generate_cymbals_structure();
        
        // Add all cymbals tracks, but change their parent to "Drum Kit" instead of "Cymbals"
        // The Cymbals (BUS) becomes a child of Drum Kit (BUS)
        for mut cymbals_track in cymbals_template.tracks {
            // If the cymbals track has no parent, it's the Cymbals (BUS) - make it a child of Drum Kit
            if cymbals_track.parent_name().is_none() {
                cymbals_track.set_parent_name("Drum Kit");
            }
            // If the cymbals track's parent is "Cymbals", keep it as is (it's a child of Cymbals (BUS))
            
            tracks.push(cymbals_track);
        }
        
        // Get room tracks and add them as children of Drum Kit
        let room_template = RoomTemplateGenerator::generate_room_structure();
        
        // Add all room tracks, but change their parent to "Drum Kit" instead of "Room"
        // The Room (BUS) becomes a child of Drum Kit (BUS)
        for mut room_track in room_template.tracks {
            // If the room track has no parent, it's the Room (BUS) - make it a child of Drum Kit
            if room_track.parent_name().is_none() {
                room_track.set_parent_name("Drum Kit");
            }
            // If the room track's parent is "Room", keep it as is (it's a child of Room (BUS))
            
            tracks.push(room_track);
        }
        
        Template {
            name: "Drum Kit".to_string(),
            tracks,
        }
    }
}

impl TemplateGenerator for DrumKitTemplateGenerator {
    type Config = TemplateConfig;
    type Error = DrumKitGeneratorError;
    
    fn generate(&self, _config: &Self::Config) -> Result<Template, Self::Error> {
        Ok(Self::generate_drum_kit_structure())
    }
    
    fn name(&self) -> &str {
        "drum_kit"
    }
}

impl Default for DrumKitTemplateGenerator {
    fn default() -> Self {
        Self::new()
    }
}

/// Drum Kit generator error
#[derive(Debug, thiserror::Error)]
pub enum DrumKitGeneratorError {
    #[error("Generator error: {0}")]
    Other(String),
}

/// Drum Kit matcher
/// 
/// For now, this is a placeholder. The visibility manager works at the drum_kit level,
/// so matching can be done at that level or delegated to child groups.
pub struct DrumKitMatcher {
    template: Template,
}

impl DrumKitMatcher {
    /// Create a new Drum Kit matcher with the default template
    pub fn new() -> Self {
        Self {
            template: DrumKitTemplateGenerator::generate_drum_kit_structure(),
        }
    }
    
    /// Create a matcher with a custom template
    pub fn with_template(template: Template) -> Self {
        Self { template }
    }
}

impl Matcher for DrumKitMatcher {
    type TrackName = crate::smart_template::naming::item_properties::ItemProperties;
    type Error = DrumKitMatchError;
    
    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        // Use original_name for matching
        let search_name = track_name.original_name.as_deref().unwrap_or("");
        
        // Simple name-based matching for now
        if let Some(track) = self.template.tracks.iter()
            .find(|t| t.name.eq_ignore_ascii_case(search_name)) {
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
                track_name.original_name.clone().unwrap_or_else(|| "Unknown".to_string())
            });
        
        // Add the new track to the template as a child of Drum Kit
        self.template.tracks.push(create_track(
            &new_track_name,
            None,
            Some("Drum Kit"),
            &[], // Default to all modes
        ));
        
        Ok((new_track_name, false))
    }
    
    fn name(&self) -> &str {
        "drum_kit"
    }
}

impl Default for DrumKitMatcher {
    fn default() -> Self {
        Self::new()
    }
}

/// Drum Kit match error
#[derive(Debug, thiserror::Error)]
pub enum DrumKitMatchError {
    #[error("Match error: {0}")]
    Other(String),
}
