use crate::smart_template::core::models::group_config::{GroupConfig, InsertMode};
use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::traits::{Group, TemplateSource, Parser, Matcher};
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::features::matching::matcher::MatchResult;
use daw::tracks::{TrackName, Track};

/// BGVs instrument consolidated struct
pub struct BGVs {
    pub config: GroupConfig,
    pub template: Template,
}

impl BGVs {
    /// Create a new BGVs instrument with default config and template
    pub fn new() -> Self {
        let config = default_bgvs_config();
        let template = Template {
            name: TrackName::from("BGVs"),
            tracks: vec![
                crate::smart_template::utils::track_helpers::create_track("BGVs", None, None, &[]),
            ],
        };
        Self {
            config,
            template,
        }
    }
}

impl Default for BGVs {
    fn default() -> Self {
        Self::new()
    }
}

impl Group for BGVs {
    fn name(&self) -> &str {
        "BGVs"
    }

    fn config(&self) -> &GroupConfig {
        &self.config
    }

    fn default_tracklist(&self) -> Vec<Track> {
        self.template.tracks.clone()
    }
}

impl TemplateSource for BGVs {
    fn template(&self) -> Template {
        self.template.clone()
    }
}

impl Parser for BGVs {
    type Output = ItemProperties;
    type Error = BGVsParseError;

    fn parse(&self, name: &str) -> Result<Self::Output, Self::Error> {
        parse_bgvs(self, name)
    }
}

impl Matcher for BGVs {
    type TrackName = ItemProperties;
    type Error = BGVsMatchError;

    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        let search_name = "BGVs";
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
        
        let new_track_name = TrackName::from(base_name.map(|s| s.to_string()).unwrap_or_else(|| "BGVs".to_string()));
        self.template.tracks.push(crate::smart_template::utils::track_helpers::create_track(&new_track_name.0, None, Some("BGVs"), &[]));
        Ok((new_track_name, false))
    }
}

/// Get the default BGVs group configuration
pub fn default_bgvs_config() -> GroupConfig {
    GroupConfig {
        name: "BGVs".to_string(),
        prefix: "BGV".to_string(),
        patterns: vec!["bgv".to_string(), "bkg".to_string(), "back".to_string(), "backing".to_string()],
        negative_patterns: vec![],
        parent_track: None,
        destination_track: None,
        insert_mode: Some(InsertMode::Increment),
        increment_start: Some(1),
        only_number_when_multiple: Some(true),
        create_if_missing: Some(true),
        ..Default::default()
    }
}

/// Parse a track name into BGVs properties
pub fn parse_bgvs(bgvs: &BGVs, name: &str) -> Result<ItemProperties, BGVsParseError> {
    let parser = ItemPropertiesParser::new();
    let props = parser.parse(name);
    
    let is_bgvs = props.group_prefix.as_deref() == Some("BGVs")
        || props.sub_type.as_ref()
            .map(|st| st.iter().any(|s| s.eq_ignore_ascii_case("BGV") || s.eq_ignore_ascii_case("BGVs")))
            .unwrap_or(false)
        || props.original_name.as_ref()
            .map(|n| {
                let name_lower = n.to_lowercase();
                bgvs.config.patterns.iter()
                    .any(|p| name_lower.contains(&p.to_lowercase()))
            })
            .unwrap_or(false);
    
    if !is_bgvs {
        return Err(BGVsParseError::NotBGVsTrack);
    }
    
    Ok(props)
}

#[derive(Debug, thiserror::Error)]
pub enum BGVsParseError {
    #[error("Track name does not match BGVs patterns")]
    NotBGVsTrack,
    #[error("Parse error: {0}")]
    Other(String),
}

#[derive(Debug, thiserror::Error)]
pub enum BGVsMatchError {
    #[error("Match error: {0}")]
    Other(String),
}
