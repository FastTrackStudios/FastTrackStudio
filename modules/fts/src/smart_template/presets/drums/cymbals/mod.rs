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

/// Cymbals instrument consolidated struct
pub struct Cymbals {
    pub config: GroupConfig,
    pub template: Template,
}

impl Cymbals {
    /// Create a new Cymbals instrument with default config and template
    pub fn new() -> Self {
        let config = naming::default_cymbals_config();
        let template = template::generate_cymbals_structure();
        Self {
            config,
            template,
        }
    }
}

impl Default for Cymbals {
    fn default() -> Self {
        Self::new()
    }
}

impl Group for Cymbals {
    fn name(&self) -> &str {
        "Cymbals"
    }

    fn config(&self) -> &GroupConfig {
        &self.config
    }

    fn default_tracklist(&self) -> Vec<Track> {
        self.template.tracks.clone()
    }
}

impl TemplateSource for Cymbals {
    fn template(&self) -> Template {
        self.template.clone()
    }
}

impl Parser for Cymbals {
    type Output = ItemProperties;
    type Error = naming::CymbalsParseError;

    fn parse(&self, name: &str) -> Result<Self::Output, Self::Error> {
        naming::parse_cymbals(self, name)
    }
}

impl Matcher for Cymbals {
    type TrackName = ItemProperties;
    type Error = template::CymbalsMatchError;

    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        template::find_best_match(self, track_name)
    }

    fn find_or_create_track(&mut self, track_name: &Self::TrackName, base_name: Option<&str>) -> Result<(TrackName, bool), Self::Error> {
        template::find_or_create_track(self, track_name, base_name)
    }
}
