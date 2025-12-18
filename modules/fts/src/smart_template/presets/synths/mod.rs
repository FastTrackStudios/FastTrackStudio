use crate::smart_template::core::models::group_config::GroupConfig;
use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::traits::{Group, TemplateSource, Parser, Matcher};
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::matching::matcher::MatchResult;
use daw::tracks::{TrackName, Track};

pub mod naming;

pub use naming::*;

/// Synths instrument consolidated struct
pub struct Synths {
    pub config: GroupConfig,
    pub template: Template,
}

impl Synths {
    /// Create a new Synths instrument with default config and template
    pub fn new() -> Self {
        let config = naming::default_synths_config();
        let template = Template {
            name: TrackName::from("Synths"),
            tracks: vec![
                crate::smart_template::utils::track_helpers::create_track("L", None, None, &[]),
                crate::smart_template::utils::track_helpers::create_track("R", None, None, &[]),
            ],
        };
        Self {
            config,
            template,
        }
    }
}

impl Default for Synths {
    fn default() -> Self {
        Self::new()
    }
}

impl Group for Synths {
    fn name(&self) -> &str {
        "Synths"
    }

    fn config(&self) -> &GroupConfig {
        &self.config
    }

    fn default_tracklist(&self) -> Vec<Track> {
        self.template.tracks.clone()
    }
}

impl TemplateSource for Synths {
    fn template(&self) -> Template {
        self.template.clone()
    }
}

impl Parser for Synths {
    type Output = ItemProperties;
    type Error = naming::SynthsParseError;

    fn parse(&self, name: &str) -> Result<Self::Output, Self::Error> {
        naming::parse_synths(self, name)
    }
}

impl Matcher for Synths {
    type TrackName = ItemProperties;
    type Error = SynthsMatchError;

    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        let search_name = "Synths";
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
        
        let new_track_name = TrackName::from(base_name.map(|s| s.to_string()).unwrap_or_else(|| "Synths".to_string()));
        self.template.tracks.push(crate::smart_template::utils::track_helpers::create_track(&new_track_name.0, None, Some("Synths"), &[]));
        Ok((new_track_name, false))
    }
}

#[derive(Debug, thiserror::Error)]
pub enum SynthsMatchError {
    #[error("Match error: {0}")]
    Other(String),
}

/// Returns the default synths track list as a Vec<Track>
pub fn default_tracklist() -> Vec<Track> {
    Synths::new().default_tracklist()
}

/// Returns the default synths track list as a Template
pub fn default_tracks() -> Vec<Template> {
    vec![Synths::new().template()]
}
