//! Cymbals template generator and matcher implementation

use crate::smart_template::matching::matcher::{Matcher, MatchResult, MatchType};
use crate::smart_template::core::traits::TemplateGenerator;
use crate::smart_template::core::template::Template;
use crate::smart_template::helpers::track_helpers::create_track;
use crate::smart_template::naming::item_properties::ItemProperties;
use crate::smart_template::config::template_config::TemplateConfig;
use crate::smart_template::shared::GroupMode;

/// Cymbals template generator
pub struct CymbalsTemplateGenerator;

impl CymbalsTemplateGenerator {
    /// Create a new Cymbals template generator
    pub fn new() -> Self {
        Self
    }
    
    /// Generate the default Cymbals track structure with mode assignments
    /// 
    /// Structure:
    /// ```
    /// Cymbals (BUS)
    /// ├── Hi-Hat
    /// ├── Ride
    /// └── OH
    /// ```
    pub fn generate_cymbals_structure() -> Template {
        let mut tracks = Vec::new();
        
        // Cymbals (BUS) - parent
        // Visible in all modes (empty modes = all modes)
        tracks.push(create_track("Cymbals", Some("BUS"), None, &[]));
        
        // Hi-Hat - child of Cymbals (BUS)
        tracks.push(create_track(
            "Hi-Hat",
            None,
            Some("Cymbals"),
            &[GroupMode::Full, GroupMode::Recording],
        ));
        
        // Ride - child of Cymbals (BUS)
        tracks.push(create_track(
            "Ride",
            None,
            Some("Cymbals"),
            &[GroupMode::Full, GroupMode::Recording],
        ));
        
        // OH (Overheads) - child of Cymbals (BUS)
        tracks.push(create_track(
            "OH",
            None,
            Some("Cymbals"),
            &[GroupMode::Full, GroupMode::Recording],
        ));
        
        Template {
            name: "Cymbals".to_string(),
            tracks,
        }
    }
}

impl TemplateGenerator for CymbalsTemplateGenerator {
    type Config = TemplateConfig;
    type Error = CymbalsGeneratorError;
    
    fn generate(&self, _config: &Self::Config) -> Result<Template, Self::Error> {
        Ok(Self::generate_cymbals_structure())
    }
    
    fn name(&self) -> &str {
        "cymbals"
    }
}

impl Default for CymbalsTemplateGenerator {
    fn default() -> Self {
        Self::new()
    }
}

/// Cymbals generator error
#[derive(Debug, thiserror::Error)]
pub enum CymbalsGeneratorError {
    #[error("Generator error: {0}")]
    Other(String),
}

/// Cymbals matcher
pub struct CymbalsMatcher {
    template: Template,
}

impl CymbalsMatcher {
    /// Create a new Cymbals matcher with the default template
    pub fn new() -> Self {
        Self {
            template: CymbalsTemplateGenerator::generate_cymbals_structure(),
        }
    }
    
    /// Create a matcher with a custom template
    pub fn with_template(template: Template) -> Self {
        Self { template }
    }
}

impl Matcher for CymbalsMatcher {
    type TrackName = ItemProperties;
    type Error = CymbalsMatchError;
    
    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        // Build search name from sub-type
        let search_name = if let Some(sub_type) = &track_name.sub_type {
            if let Some(first) = sub_type.first() {
                // Map sub-type names to track names
                match first.as_str() {
                    "Hi Hat" => "Hi-Hat".to_string(),
                    "Overheads" => "OH".to_string(),
                    _ => first.clone(),
                }
            } else {
                "Cymbals".to_string()
            }
        } else {
            "Cymbals".to_string()
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
                if let Some(sub_type) = &track_name.sub_type {
                    if let Some(first) = sub_type.first() {
                        match first.as_str() {
                            "Hi Hat" => "Hi-Hat".to_string(),
                            "Overheads" => "OH".to_string(),
                            _ => first.clone(),
                        }
                    } else {
                        "Cymbals".to_string()
                    }
                } else {
                    "Cymbals".to_string()
                }
            });
        
        // Add the new track to the template
        self.template.tracks.push(create_track(
            &new_track_name,
            None,
            Some("Cymbals"),
            &[], // Default to all modes
        ));
        
        Ok((new_track_name, false))
    }
    
    fn name(&self) -> &str {
        "cymbals"
    }
}

impl Default for CymbalsMatcher {
    fn default() -> Self {
        Self::new()
    }
}

/// Cymbals match error
#[derive(Debug, thiserror::Error)]
pub enum CymbalsMatchError {
    #[error("Match error: {0}")]
    Other(String),
}
