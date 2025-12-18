//! Snare template generator and matcher implementation

use crate::smart_template::matching::matcher::{Matcher, MatchResult, MatchType};
use crate::smart_template::core::traits::TemplateGenerator;
use crate::smart_template::core::template::Template;
use crate::smart_template::helpers::track_helpers::create_track;
use crate::smart_template::naming::item_properties::ItemProperties;
use crate::smart_template::config::template_config::TemplateConfig;
use crate::smart_template::shared::GroupMode;

/// Snare template generator
pub struct SnareTemplateGenerator;

impl SnareTemplateGenerator {
    /// Create a new Snare template generator
    pub fn new() -> Self {
        Self
    }
    
    /// Generate the default Snare track structure with mode assignments
    pub fn generate_snare_structure() -> Template {
        let mut tracks = Vec::new();
        
        // Snare (BUS) - parent
        // Visible in all modes (empty modes = all modes)
        tracks.push(create_track("Snare", Some("BUS"), None, &[]));
        
        // Snare (SUM) - child of Snare (BUS)
        // Visible in Full and Minimal
        // Use a unique identifier for the SUM track so children can reference it
        let snare_sum_name = "Snare (SUM)".to_string();
        tracks.push(create_track(
            &snare_sum_name,
            Some("SUM"),
            Some("Snare"),
            &[GroupMode::Full, GroupMode::Minimal],
        ));
        
        // Multi-mic tracks - children of Snare (SUM)
        // Top, Bottom: Recording mode (audio inputs)
        // Trig: Midi mode (MIDI trigger)
        for multi_mic in &["Top", "Bottom"] {
            tracks.push(create_track(
                format!("Snare {}", multi_mic),
                None,
                Some(&snare_sum_name),
                &[GroupMode::Full, GroupMode::Recording],
            ));
        }
        
        // Trig is MIDI-related
        tracks.push(create_track(
            "Snare Trig",
            None,
            Some(&snare_sum_name),
            &[GroupMode::Full, GroupMode::Midi],
        ));
        
        // Snare Verb - child of Snare (BUS), not SUM
        // Full mode only (processing track)
        tracks.push(create_track(
            "Snare Verb",
            None,
            Some("Snare"),
            &[GroupMode::Full],
        ));
        
        Template {
            name: "Snare".to_string(),
            tracks,
        }
    }
}

impl TemplateGenerator for SnareTemplateGenerator {
    type Config = TemplateConfig;
    type Error = SnareGeneratorError;
    
    fn generate(&self, _config: &Self::Config) -> Result<Template, Self::Error> {
        Ok(Self::generate_snare_structure())
    }
    
    fn name(&self) -> &str {
        "snare"
    }
}

impl Default for SnareTemplateGenerator {
    fn default() -> Self {
        Self::new()
    }
}

/// Snare generator error
#[derive(Debug, thiserror::Error)]
pub enum SnareGeneratorError {
    #[error("Generator error: {0}")]
    Other(String),
}

/// Snare matcher
pub struct SnareMatcher {
    template: Template,
}

impl SnareMatcher {
    /// Create a new Snare matcher with the default template
    pub fn new() -> Self {
        Self {
            template: SnareTemplateGenerator::generate_snare_structure(),
        }
    }
    
    /// Create a matcher with a custom template
    pub fn with_template(template: Template) -> Self {
        Self { template }
    }
}

impl Matcher for SnareMatcher {
    type TrackName = ItemProperties;
    type Error = SnareMatchError;
    
    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        // Try to find exact match first
        let search_name = if let Some(sub_type) = &track_name.sub_type {
            if let Some(first) = sub_type.first() {
                format!("Snare {}", first)
            } else {
                "Snare".to_string()
            }
        } else {
            "Snare".to_string()
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
                        format!("Snare {}", first)
                    } else {
                        "Snare".to_string()
                    }
                } else {
                    "Snare".to_string()
                }
            });
        
        // Add the new track to the template
        self.template.tracks.push(create_track(
            &new_track_name,
            None,
            Some("Snare"),
            &[], // Default to all modes
        ));
        
        Ok((new_track_name, false))
    }
    
    fn name(&self) -> &str {
        "snare"
    }
}

impl Default for SnareMatcher {
    fn default() -> Self {
        Self::new()
    }
}

/// Snare match error
#[derive(Debug, thiserror::Error)]
pub enum SnareMatchError {
    #[error("Match error: {0}")]
    Other(String),
}
