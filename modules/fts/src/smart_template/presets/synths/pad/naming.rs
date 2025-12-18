use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::core::models::group_config::GroupConfig;
use super::Pad;

use crate::smart_template::core::traits::{Group, Parser, Matcher, TemplateSource};
use crate::smart_template::features::matching::matcher::MatchResult;
use crate::smart_template::core::errors::{TemplateParseError, TemplateMatchError};
use daw::tracks::{Track, TrackName};

impl Group for Pad {
    fn group_name(&self) -> &str {
        "Pad"
    }

    fn group_config(&self) -> GroupConfig {
        GroupConfig {
            name: "Pad".to_string(),
            prefix: "PAD".to_string(),
            patterns: vec!["pad".to_string(), "synth pad".to_string()],
            ..Default::default()
        }
    }

    fn default_tracklist(&self) -> Vec<Track> {
        self.template().tracks
    }
}

impl Parser for Pad {
    type Output = ItemProperties;
    type Error = TemplateParseError;

    fn parse_item_properties(&self, name: &str) -> Result<Self::Output, Self::Error> {
        let parser = ItemPropertiesParser::new();
        let props = parser.parse_item_properties(name);
        
        let config = self.group_config();
        let is_pad = props.group_prefix.as_deref() == Some("Pad")
            || props.sub_type.as_ref()
                .map(|st| st.iter().any(|s| s.eq_ignore_ascii_case("Pad")))
                .unwrap_or(false)
            || props.original_name.as_ref()
                .map(|n| {
                    let name_lower = n.to_lowercase();
                    config.patterns.iter()
                        .any(|p| name_lower.contains(&p.to_lowercase()))
                })
                .unwrap_or(false);
        
        if !is_pad {
            return Err(TemplateParseError::NotMatch("Pad".to_string()));
        }
        
        Ok(props)
    }
}
