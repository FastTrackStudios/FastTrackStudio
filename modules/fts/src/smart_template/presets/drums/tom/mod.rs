use crate::smart_template::core::models::group_config::GroupConfig;
use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::traits::{Group, TemplateSource, Parser, Matcher};
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::matching::matcher::MatchResult;
use daw::tracks::{TrackName, Track};
use std::sync::Mutex;

pub mod naming;
pub mod template;
pub mod tom_mapper;

pub use naming::*;
pub use template::*;
pub use tom_mapper::*;

/// Tom instrument consolidated struct
pub struct Tom {
    pub config: GroupConfig,
    pub template: Mutex<Template>,
    pub mapper: Mutex<TomMapper>,
}

impl Tom {
    /// Create a new Tom instrument with default config and template
    pub fn new() -> Self {
        let config = naming::default_tom_config();
        let template = template::generate_tom_structure();
        Self {
            config,
            template: Mutex::new(template),
            mapper: Mutex::new(TomMapper::new()),
        }
    }
}

impl Default for Tom {
    fn default() -> Self {
        Self::new()
    }
}

impl Group for Tom {
    fn name(&self) -> &str {
        "Tom"
    }

    fn config(&self) -> &GroupConfig {
        &self.config
    }

    fn default_tracklist(&self) -> Vec<Track> {
        self.template.lock().unwrap().tracks.clone()
    }
}

impl TemplateSource for Tom {
    fn template(&self) -> Template {
        self.template.lock().unwrap().clone()
    }
}

impl Parser for Tom {
    type Output = ItemProperties;
    type Error = naming::TomParseError;

    fn parse(&self, name: &str) -> Result<Self::Output, Self::Error> {
        naming::parse_tom(self, name)
    }
}

impl Matcher for Tom {
    type TrackName = ItemProperties;
    type Error = template::TomMatchError;

    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        template::find_best_match(self, track_name)
    }

    fn find_or_create_track(&mut self, track_name: &Self::TrackName, base_name: Option<&str>) -> Result<(TrackName, bool), Self::Error> {
        template::find_or_create_track(self, track_name, base_name)
    }
}
