//! Kick template generator and matcher implementation

use crate::smart_template::matching::matcher::{Matcher, MatchResult, MatchType};
use crate::smart_template::core::traits::TemplateGenerator;
use crate::smart_template::core::template::Template;
use crate::smart_template::helpers::track_helpers::create_track;
use crate::smart_template::naming::item_properties::ItemProperties;
use crate::smart_template::config::template_config::TemplateConfig;
use crate::smart_template::shared::GroupMode;

/// Kick template generator
pub struct KickTemplateGenerator;

impl KickTemplateGenerator {
    /// Create a new Kick template generator
    pub fn new() -> Self {
        Self
    }
    
    /// Generate the default Kick track structure with mode assignments
    pub fn generate_kick_structure() -> Template {
        let mut tracks = Vec::new();
        
        // Kick (BUS) - parent
        // Visible in all modes (empty modes = all modes)
        tracks.push(create_track("Kick", Some("BUS"), None, &[]));
        
        // Kick (SUM) - child of Kick (BUS)
        // Visible in Full and Minimal
        // Use a unique identifier for the SUM track so children can reference it
        let kick_sum_name = "Kick (SUM)".to_string();
        tracks.push(create_track(
            &kick_sum_name,
            Some("SUM"),
            Some("Kick"),
            &[GroupMode::Full, GroupMode::Minimal],
        ));
        
        // Multi-mic tracks - children of Kick (SUM)
        // In, Out: Recording mode (audio inputs)
        // Trig: Midi mode (MIDI trigger)
        for multi_mic in &["In", "Out"] {
            tracks.push(create_track(
                format!("Kick {}", multi_mic),
                None,
                Some(&kick_sum_name),
                &[GroupMode::Full, GroupMode::Recording],
            ));
        }
        
        // Trig is MIDI-related
        tracks.push(create_track(
            "Kick Trig",
            None,
            Some(&kick_sum_name),
            &[GroupMode::Full, GroupMode::Midi],
        ));
        
        // Kick Sub - child of Kick (BUS), not SUM
        // Full mode only (processing track)
        tracks.push(create_track(
            "Kick Sub",
            None,
            Some("Kick"),
            &[GroupMode::Full],
        ));
        
        // Kick Ambient - child of Kick (BUS), not SUM
        // Full mode only (processing track)
        tracks.push(create_track(
            "Kick Ambient",
            None,
            Some("Kick"),
            &[GroupMode::Full],
        ));
        
        Template {
            name: "Kick".to_string(),
            tracks,
        }
    }
}

impl TemplateGenerator for KickTemplateGenerator {
    type Config = TemplateConfig;
    type Error = KickGeneratorError;
    
    fn generate(&self, _config: &Self::Config) -> Result<Template, Self::Error> {
        Ok(Self::generate_kick_structure())
    }
    
    fn name(&self) -> &str {
        "kick"
    }
}

impl Default for KickTemplateGenerator {
    fn default() -> Self {
        Self::new()
    }
}

/// Kick generator error
#[derive(Debug, thiserror::Error)]
pub enum KickGeneratorError {
    #[error("Generator error: {0}")]
    Other(String),
}

/// Kick matcher
pub struct KickMatcher {
    template: Template,
}

impl KickMatcher {
    /// Create a new Kick matcher with the default template
    pub fn new() -> Self {
        Self {
            template: KickTemplateGenerator::generate_kick_structure(),
        }
    }
    
    /// Create a matcher with a custom template
    pub fn with_template(template: Template) -> Self {
        Self { template }
    }
}

impl Matcher for KickMatcher {
    type TrackName = ItemProperties;
    type Error = KickMatchError;
    
    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        // Try to find exact match first
        let search_name = if let Some(sub_type) = &track_name.sub_type {
            if let Some(first) = sub_type.first() {
                format!("Kick {}", first)
            } else {
                "Kick".to_string()
            }
        } else {
            "Kick".to_string()
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
                        format!("Kick {}", first)
                    } else {
                        "Kick".to_string()
                    }
                } else {
                    "Kick".to_string()
                }
            });
        
        // Add the new track to the template
        self.template.tracks.push(create_track(
            &new_track_name,
            None,
            Some("Kick"),
            &[], // Default to all modes
        ));
        
        Ok((new_track_name, false))
    }
    
    fn name(&self) -> &str {
        "kick"
    }
}

impl Default for KickMatcher {
    fn default() -> Self {
        Self::new()
    }
}

/// Kick match error
#[derive(Debug, thiserror::Error)]
pub enum KickMatchError {
    #[error("Match error: {0}")]
    Other(String),
}
