use crate::smart_template::core::models::group_config::GroupConfig;
use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::traits::{Group, TemplateSource, Parser, Matcher};
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::matching::matcher::MatchResult;
use daw::tracks::{TrackName, Track};

pub mod naming;

pub use naming::*;

/// Guitar Acoustic instrument consolidated struct
pub struct GuitarAcoustic {
    pub config: GroupConfig,
    pub template: Template,
}

impl GuitarAcoustic {
    /// Create a new Guitar Acoustic instrument with default config and template
    pub fn new() -> Self {
        let config = naming::default_guitar_acoustic_config();
        let template = Template {
            name: TrackName::from("Guitar Acoustic"),
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

impl Default for GuitarAcoustic {
    fn default() -> Self {
        Self::new()
    }
}

impl Group for GuitarAcoustic {
    fn name(&self) -> &str {
        "Guitar Acoustic"
    }

    fn config(&self) -> &GroupConfig {
        &self.config
    }

    fn default_tracklist(&self) -> Vec<Track> {
        self.template.tracks.clone()
    }
}

impl TemplateSource for GuitarAcoustic {
    fn template(&self) -> Template {
        self.template.clone()
    }
}

impl Parser for GuitarAcoustic {
    type Output = ItemProperties;
    type Error = naming::GuitarAcousticParseError;

    fn parse(&self, name: &str) -> Result<Self::Output, Self::Error> {
        naming::parse_guitar_acoustic(self, name)
    }
}

impl Matcher for GuitarAcoustic {
    type TrackName = ItemProperties;
    type Error = GuitarAcousticMatchError;

    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        let search_name = "Guitar Acoustic";
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
        
        let new_track_name = TrackName::from(base_name.map(|s| s.to_string()).unwrap_or_else(|| "Guitar Acoustic".to_string()));
        self.template.tracks.push(crate::smart_template::utils::track_helpers::create_track(&new_track_name.0, None, Some("Guitar Acoustic"), &[]));
        Ok((new_track_name, false))
    }
}

#[derive(Debug, thiserror::Error)]
pub enum GuitarAcousticMatchError {
    #[error("Match error: {0}")]
    Other(String),
}
