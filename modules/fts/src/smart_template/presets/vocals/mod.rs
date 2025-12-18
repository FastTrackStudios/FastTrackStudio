//! Vocals Preset
//!
//! Consolidates all vocal-related logic: naming conventions, template generators, and track lists.

pub mod naming;
pub mod bgvs;

use daw::tracks::{Track, TrackName};
use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::models::group_config::GroupConfig;
use crate::smart_template::core::traits::{Group, TemplateSource, Parser, Matcher};
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::matching::matcher::MatchResult;

pub use naming::*;
pub use bgvs::BGVs;

/// Vocals instrument consolidated struct
pub struct Vocals {
    pub config: GroupConfig,
    pub template: Template,
}

impl Vocals {
    /// Create a new Vocals instrument with default config and template
    pub fn new() -> Self {
        let config = naming::default_vocals_config();
        let template = Template {
            name: TrackName::from("Vocals"),
            tracks: vec![
                crate::smart_template::utils::track_helpers::create_track("Lead Vocal", None, None, &[]),
            ],
        };
        Self {
            config,
            template,
        }
    }
}

impl Default for Vocals {
    fn default() -> Self {
        Self::new()
    }
}

impl Group for Vocals {
    fn name(&self) -> &str {
        "Vocals"
    }

    fn config(&self) -> &GroupConfig {
        &self.config
    }

    fn default_tracklist(&self) -> Vec<Track> {
        self.template.tracks.clone()
    }
}

impl TemplateSource for Vocals {
    fn template(&self) -> Template {
        self.template.clone()
    }
}

impl Parser for Vocals {
    type Output = ItemProperties;
    type Error = naming::VocalsParseError;

    fn parse(&self, name: &str) -> Result<Self::Output, Self::Error> {
        naming::parse_vocals(self, name)
    }
}

impl Matcher for Vocals {
    type TrackName = ItemProperties;
    type Error = VocalsMatchError;

    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        let search_name = "Lead Vocal";
        crate::smart_template::features::matching::matcher::helpers::instrument_find_best_match(
            &self.template,
            track_name,
            search_name,
        )
    }

    fn find_or_create_track(&mut self, track_name: &Self::TrackName, base_name: Option<&str>) -> Result<(TrackName, bool), Self::Error> {
        if let Some(result) = self.find_best_match(track_name) {
            return Ok((result.track_name, result.use_takes));
        }
        
        let new_track_name = TrackName::from(base_name.map(|s| s.to_string()).unwrap_or_else(|| "Lead Vocal".to_string()));
        self.template.tracks.push(crate::smart_template::utils::track_helpers::create_track(&new_track_name.0, None, Some("Vocals"), &[]));
        Ok((new_track_name, false))
    }
}

#[derive(Debug, thiserror::Error)]
pub enum VocalsMatchError {
    #[error("Match error: {0}")]
    Other(String),
}

/// Returns the default vocals track list as a Vec<Track>
pub fn default_tracklist() -> Vec<Track> {
    let mut all_tracks = Vec::new();
    
    all_tracks.extend(Vocals::new().default_tracklist());
    all_tracks.extend(BGVs::new().default_tracklist());
    
    all_tracks
}

/// Returns the default vocals track list as a Template
pub fn default_tracks() -> Vec<Template> {
    vec![Vocals::new().template()]
}
