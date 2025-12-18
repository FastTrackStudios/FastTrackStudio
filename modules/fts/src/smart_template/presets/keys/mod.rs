use crate::smart_template::core::models::group_config::GroupConfig;
use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::traits::{Group, TemplateSource, Parser, Matcher};
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::matching::matcher::MatchResult;
use daw::tracks::{TrackName, Track};

pub mod naming;

pub use naming::*;

/// Keys instrument consolidated struct
pub struct Keys {
    pub config: GroupConfig,
    pub template: Template,
}

impl Keys {
    /// Create a new Keys instrument with default config and template
    pub fn new() -> Self {
        let config = naming::default_keys_config();
        let template = Template {
            name: TrackName::from("Keys"),
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

impl Default for Keys {
    fn default() -> Self {
        Self::new()
    }
}

impl Group for Keys {
    fn name(&self) -> &str {
        "Keys"
    }

    fn config(&self) -> &GroupConfig {
        &self.config
    }

    fn default_tracklist(&self) -> Vec<Track> {
        self.template.tracks.clone()
    }
}

impl TemplateSource for Keys {
    fn template(&self) -> Template {
        self.template.clone()
    }
}

impl Parser for Keys {
    type Output = ItemProperties;
    type Error = naming::KeysParseError;

    fn parse(&self, name: &str) -> Result<Self::Output, Self::Error> {
        naming::parse_keys(self, name)
    }
}

impl Matcher for Keys {
    type TrackName = ItemProperties;
    type Error = KeysMatchError;

    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        let search_name = "Keys";
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
        
        let new_track_name = TrackName::from(base_name.map(|s| s.to_string()).unwrap_or_else(|| "Keys".to_string()));
        self.template.tracks.push(crate::smart_template::utils::track_helpers::create_track(&new_track_name.0, None, Some("Keys"), &[]));
        Ok((new_track_name, false))
    }
}

#[derive(Debug, thiserror::Error)]
pub enum KeysMatchError {
    #[error("Match error: {0}")]
    Other(String),
}
