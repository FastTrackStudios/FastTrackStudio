use crate::smart_template::core::models::group_config::GroupConfig;
use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::traits::{Group, TemplateSource, Parser, Matcher};
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::matching::matcher::MatchResult;
use daw::tracks::{TrackName, Track};

pub mod naming;
pub mod template;

pub use naming::*;
pub use template::*;

/// Snare instrument consolidated struct
pub struct Snare {
    pub config: GroupConfig,
    pub template: Template,
}

impl Snare {
    /// Create a new Snare instrument with default config and template
    pub fn new() -> Self {
        let config = naming::default_snare_config();
        let template = template::generate_snare_structure();
        Self {
            config,
            template,
        }
    }
}

impl Default for Snare {
    fn default() -> Self {
        Self::new()
    }
}

impl Group for Snare {
    fn name(&self) -> &str {
        "Snare"
    }

    fn config(&self) -> &GroupConfig {
        &self.config
    }

    fn default_tracklist(&self) -> Vec<Track> {
        self.template.tracks.clone()
    }
}

impl TemplateSource for Snare {
    fn template(&self) -> Template {
        self.template.clone()
    }
}

impl Parser for Snare {
    type Output = ItemProperties;
    type Error = naming::SnareParseError;

    fn parse(&self, name: &str) -> Result<Self::Output, Self::Error> {
        naming::parse_snare(self, name)
    }
}

impl Matcher for Snare {
    type TrackName = ItemProperties;
    type Error = template::SnareMatchError;

    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        template::find_best_match(self, track_name)
    }

    fn find_or_create_track(&mut self, track_name: &Self::TrackName, base_name: Option<&str>) -> Result<(TrackName, bool), Self::Error> {
        template::find_or_create_track(self, track_name, base_name)
    }
}
