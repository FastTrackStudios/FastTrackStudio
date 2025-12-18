//! Tom template implementation

use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::models::GroupMode;
use crate::smart_template::core::traits::{TemplateSource, Matcher};
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::matching::matcher::MatchResult;
use crate::smart_template::core::errors::TemplateMatchError;
use daw::tracks::{TrackName};
use super::Tom;
use std::sync::{Mutex, OnceLock};

static TEMPLATE: OnceLock<Mutex<Template>> = OnceLock::new();

impl TemplateSource for Tom {
    fn full_template(&self) -> Template {
        Template::builder("Tom Full")
            .bus("Tom")
                .track("T1")
                .track("T2")
                .track("T3")
            .end()
            .build()
    }

    fn default_template(&self) -> Template {
        Template::builder("Tom Default")
            .bus("Tom")
                .track("T1")
                .track("T2")
                .track("T3")
            .end()
            .build()
    }

    fn minimal_template(&self) -> Template {
        Template::builder("Tom Minimal")
            .track("T1")
            .track("T2")
            .track("T3")
            .build()
    }

    fn template(&self) -> Template {
        self.full_template()
    }
}

/// Generate the default Tom track structure
pub fn generate_tom_structure() -> Template {
    Template::builder("Tom")
        .bus("Tom")
            .track("Tom 1").modes(&[GroupMode::Full, GroupMode::Recording])
            .track("Tom 2").modes(&[GroupMode::Full, GroupMode::Recording])
            .track("Tom 3").modes(&[GroupMode::Full, GroupMode::Recording])
        .end()
        .build()
}


impl Matcher for Tom {
    type TrackName = ItemProperties;
    type Error = TemplateMatchError;

    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        let search_name = match track_name.increment.as_ref() {
            Some(inc) => format!("Tom {}", inc),
            None => "Tom".to_string(),
        };
        
        crate::smart_template::features::matching::matcher::helpers::instrument_find_best_match(
            &self.template(),
            track_name,
            &search_name,
        )
    }

    fn find_or_create_track(&mut self, track_name: &Self::TrackName, base_name: Option<&str>) -> Result<(TrackName, bool), Self::Error> {
        if let Some(result) = self.find_best_match(track_name) {
            return Ok((result.track_name, result.use_takes));
        }
        
        let new_track_name = TrackName::from(base_name
            .map(|s| s.to_string())
            .unwrap_or_else(|| {
                match track_name.increment.as_ref() {
                    Some(inc) => format!("Tom {}", inc),
                    None => "Tom".to_string(),
                }
            }));
        
        Ok((new_track_name, false))
    }
}
