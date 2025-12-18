use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::core::models::group_config::GroupConfig;
use super::Arp;

use crate::smart_template::core::traits::{Group, Parser, Matcher, TemplateSource};
use crate::smart_template::features::matching::matcher::MatchResult;
use crate::smart_template::core::errors::{TemplateParseError, TemplateMatchError};
use daw::tracks::{Track, TrackName};

impl Group for Arp {
    fn group_name(&self) -> &str {
        "Arp"
    }

    fn group_config(&self) -> GroupConfig {
        GroupConfig {
            name: "Arp".to_string(),
            prefix: "ARP".to_string(),
            patterns: vec!["arp".to_string(), "arpeggio".to_string(), "synth arp".to_string()],
            ..Default::default()
        }
    }

    fn default_tracklist(&self) -> Vec<Track> {
        self.template().tracks
    }
}

impl Parser for Arp {
    type Output = ItemProperties;
    type Error = TemplateParseError;

    fn parse_item_properties(&self, name: &str) -> Result<Self::Output, Self::Error> {
        let parser = ItemPropertiesParser::new();
        let props = parser.parse_item_properties(name);
        
        let config = self.group_config();
        let is_arp = props.group_prefix.as_deref() == Some("Arp")
            || props.sub_type.as_ref()
                .map(|st| st.iter().any(|s| s.eq_ignore_ascii_case("Arp")))
                .unwrap_or(false)
            || props.original_name.as_ref()
                .map(|n| {
                    let name_lower = n.to_lowercase();
                    config.patterns.iter()
                        .any(|p| name_lower.contains(&p.to_lowercase()))
                })
                .unwrap_or(false);
        
        if !is_arp {
            return Err(TemplateParseError::NotMatch("Arp".to_string()));
        }
        
        Ok(props)
    }
}

