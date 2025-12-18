//! Bass Preset
//!
//! Consolidates all bass-related logic: naming conventions, templates, and track lists.

pub mod naming;

use daw::tracks::{Track, TrackName};
use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::models::group_config::GroupConfig;
use crate::smart_template::core::traits::{Group, TemplateSource, Parser, Matcher};
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::matching::matcher::MatchResult;

pub use naming::*;

/// Bass instrument consolidated struct
pub struct Bass {
    pub config: GroupConfig,
    pub template: Template,
}

impl Bass {
    /// Create a new Bass instrument with default config and template
    pub fn new() -> Self {
        let config = naming::default_bass_config();
        let template = Template {
            name: TrackName::from("Bass"),
            tracks: vec![
                crate::smart_template::utils::track_helpers::create_track("DI", None, None, &[]),
                crate::smart_template::utils::track_helpers::create_track("Amp", None, None, &[]),
            ],
        };
        Self {
            config,
            template,
        }
    }
}

impl Default for Bass {
    fn default() -> Self {
        Self::new()
    }
}

impl Group for Bass {
    fn name(&self) -> &str {
        "Bass"
    }

    fn config(&self) -> &GroupConfig {
        &self.config
    }

    fn default_tracklist(&self) -> Vec<Track> {
        self.template.tracks.clone()
    }
}

impl TemplateSource for Bass {
    fn template(&self) -> Template {
        self.template.clone()
    }
}

impl Parser for Bass {
    type Output = ItemProperties;
    type Error = naming::BassParseError;

    fn parse(&self, name: &str) -> Result<Self::Output, Self::Error> {
        naming::parse_bass(self, name)
    }
}

impl Matcher for Bass {
    type TrackName = ItemProperties;
    type Error = BassMatchError;

    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        let search_name = "Bass";
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
        
        let new_track_name = TrackName::from(base_name.map(|s| s.to_string()).unwrap_or_else(|| "Bass".to_string()));
        self.template.tracks.push(crate::smart_template::utils::track_helpers::create_track(&new_track_name.0, None, Some("Bass"), &[]));
        Ok((new_track_name, false))
    }
}

#[derive(Debug, thiserror::Error)]
pub enum BassMatchError {
    #[error("Match error: {0}")]
    Other(String),
}

/// Returns the default bass track list as a Vec<Track>
pub fn default_tracklist() -> Vec<Track> {
    Bass::new().default_tracklist()
}

/// Returns the default bass track list as a Template
pub fn default_tracks() -> Vec<Template> {
    vec![Bass::new().template()]
}
