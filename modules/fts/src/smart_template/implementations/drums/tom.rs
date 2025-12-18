//! Tom template generator and matcher implementation

use crate::smart_template::matching::matcher::{Matcher, MatchResult, MatchType};
use crate::smart_template::core::traits::TemplateGenerator;
use crate::smart_template::core::template::Template;
use crate::smart_template::helpers::track_helpers::create_track;
use crate::smart_template::naming::item_properties::ItemProperties;
use crate::smart_template::config::template_config::TemplateConfig;
use crate::smart_template::shared::GroupMode;
use crate::smart_template::implementations::drums::tom_mapper::TomMapper;
use crate::smart_template::matching::group_mapper::NamePreservation;
use std::sync::Mutex;

/// Tom template generator
pub struct TomTemplateGenerator;

impl TomTemplateGenerator {
    /// Create a new Tom template generator
    pub fn new() -> Self {
        Self
    }
    
    /// Generate the default Tom track structure with mode assignments
    /// 
    /// Structure:
    /// ```
    /// Tom (BUS)
    /// └── ... (dynamically created based on discovered toms)
    /// ```
    /// 
    /// This creates an empty template with just the BUS track.
    /// Tracks are added dynamically as they are discovered.
    pub fn generate_tom_structure() -> Template {
        let mut tracks = Vec::new();
        
        // Tom (BUS) - parent
        // Visible in all modes (empty modes = all modes)
        tracks.push(create_track("Tom", Some("BUS"), None, &[]));
        
        Template {
            name: "Tom".to_string(),
            tracks,
        }
    }
    
    /// Add a tom track to the template dynamically
    pub fn add_tom_track(template: &mut Template, tom_number: u32) {
        let track_name = format!("Tom {}", tom_number);
        
        // Check if track already exists
        if template.tracks.iter().any(|t| t.name == track_name) {
            return;
        }
        
        template.tracks.push(create_track(
            track_name,
            None,
            Some("Tom"),
            &[GroupMode::Full, GroupMode::Recording],
        ));
    }
}

impl TemplateGenerator for TomTemplateGenerator {
    type Config = TemplateConfig;
    type Error = TomGeneratorError;
    
    fn generate(&self, _config: &Self::Config) -> Result<Template, Self::Error> {
        Ok(Self::generate_tom_structure())
    }
    
    fn name(&self) -> &str {
        "tom"
    }
}

impl Default for TomTemplateGenerator {
    fn default() -> Self {
        Self::new()
    }
}

/// Tom generator error
#[derive(Debug, thiserror::Error)]
pub enum TomGeneratorError {
    #[error("Generator error: {0}")]
    Other(String),
}

/// Tom matcher
pub struct TomMatcher {
    template: Mutex<Template>,
    mapper: Mutex<TomMapper>,
}

impl TomMatcher {
    /// Create a new Tom matcher with the default template
    /// Uses unified naming (always "Tom 1", "Tom 2", etc.)
    pub fn new() -> Self {
        Self {
            template: Mutex::new(TomTemplateGenerator::generate_tom_structure()),
            mapper: Mutex::new(TomMapper::new()),
        }
    }
    
    /// Create a matcher with name preservation strategy
    pub fn with_preservation(preservation: NamePreservation) -> Self {
        Self {
            template: Mutex::new(TomTemplateGenerator::generate_tom_structure()),
            mapper: Mutex::new(TomMapper::with_preservation(preservation)),
        }
    }
    
    /// Create a matcher with a custom template
    pub fn with_template(template: Template) -> Self {
        Self { 
            template: Mutex::new(template),
            mapper: Mutex::new(TomMapper::new()),
        }
    }
    
    /// Get the tom mapper (for external access if needed)
    pub fn mapper(&self) -> std::sync::MutexGuard<'_, TomMapper> {
        self.mapper.lock().unwrap()
    }
}

impl Matcher for TomMatcher {
    type TrackName = ItemProperties;
    type Error = TomMatchError;
    
    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        // Map the track name (gets unified name and display name)
        let mapping_result = {
            let mut mapper = self.mapper.lock().unwrap();
            mapper.map_track_name(track_name)
        };
        
        // Extract unified increment from unified name (e.g., "Tom 1" → 1)
        let unified_tom_number = mapping_result.unified_name
            .split_whitespace()
            .last()
            .and_then(|s| s.parse::<u32>().ok())
            .unwrap_or(1);
        
        // Use display name for the track (may preserve sub-type)
        let display_track_name = mapping_result.display_name;
        
        // Ensure the track exists in the template
        let mut template = self.template.lock().unwrap();
        if !template.tracks.iter().any(|t| t.name == display_track_name) {
            // Add track with display name
            template.tracks.push(create_track(
                &display_track_name,
                None,
                Some("Tom"),
                &[GroupMode::Full, GroupMode::Recording],
            ));
        }
        
        // Look for the track by display name
        if let Some(track) = template.tracks.iter()
            .find(|t| t.name == display_track_name) {
            return Some(MatchResult {
                track_name: track.name.clone(),
                match_type: MatchType::Exact,
                score: 100,
                use_takes: false,
            });
        }
        
        None
    }
    
    fn find_or_create_track(&mut self, track_name: &Self::TrackName, _base_name: Option<&str>) -> Result<(String, bool), Self::Error> {
        // Map the track name (gets unified name and display name)
        let mapping_result = {
            let mut mapper = self.mapper.lock().unwrap();
            mapper.map_track_name(track_name)
        };
        
        // Use display name (may preserve sub-type based on strategy)
        let display_track_name = mapping_result.display_name;
        
        // Ensure the track exists in the template
        let mut template = self.template.lock().unwrap();
        if !template.tracks.iter().any(|t| t.name == display_track_name) {
            template.tracks.push(create_track(
                &display_track_name,
                None,
                Some("Tom"),
                &[GroupMode::Full, GroupMode::Recording],
            ));
        }
        
        Ok((display_track_name, false))
    }
    
    fn name(&self) -> &str {
        "tom"
    }
}

impl Default for TomMatcher {
    fn default() -> Self {
        Self::new()
    }
}

/// Tom match error
#[derive(Debug, thiserror::Error)]
pub enum TomMatchError {
    #[error("Match error: {0}")]
    Other(String),
}
