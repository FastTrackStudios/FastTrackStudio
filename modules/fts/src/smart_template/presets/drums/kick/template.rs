//! Kick template implementation

use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::models::GroupMode;
use crate::smart_template::core::traits::{TemplateSource, Matcher};
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::matching::matcher::MatchResult;
use crate::smart_template::core::errors::TemplateMatchError;
use crate::smart_template::utils::track_helpers::create_track;
use daw::tracks::TrackName;
use super::Kick;
use std::sync::{Mutex, OnceLock};

static TEMPLATE: OnceLock<Mutex<Template>> = OnceLock::new();

impl TemplateSource for Kick {
    fn full_template(&self) -> Template {
        Template::builder("Kick Full")
            .bus("Kick")
                .sum("Kick (SUM)")
                    .track("Kick In")
                    .track("Kick Out")
                    .track("Kick Trig")
                .end()
                .track("Kick Sub")
                .track("Kick Ambient")
            .end()
            .build()
    }

    fn default_template(&self) -> Template {
        Template::builder("Kick Default")
            .bus("Kick")
                .track("Kick In")
                .track("Kick Out")
                .track("Kick Trig")
            .end()
            .build()
    }

    fn minimal_template(&self) -> Template {
        Template::builder("Kick Minimal")
            .track("Kick In")
            .track("Kick Out")
            .track("Kick Trig")
            .build()
    }

    fn template(&self) -> Template {
        self.full_template()
    }
}

impl Matcher for Kick {
    type TrackName = ItemProperties;
    type Error = TemplateMatchError;

    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        let search_name = match track_name.sub_type.as_ref().and_then(|s| s.first()) {
            Some(first) => format!("Kick {}", first),
            None => "Kick".to_string(),
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
                match track_name.sub_type.as_ref().and_then(|s| s.first()) {
                    Some(first) => format!("Kick {}", first),
                    None => "Kick".to_string(),
                }
            }));
        
        let mut template_lock = TEMPLATE.get_or_init(|| Mutex::new(generate_kick_structure())).lock().unwrap();
        template_lock.tracks.push(create_track(
            &new_track_name.0,
            None,
            Some("Kick"),
            &[],
        ));
        
        Ok((new_track_name, false))
    }
}

/// Generate the default Kick track structure
pub fn generate_kick_structure() -> Template {
    Template::builder("Kick")
        .bus("Kick")
            .sum("Kick (SUM)").modes(&[GroupMode::Full, GroupMode::Minimal])
                .track("Kick In").modes(&[GroupMode::Full, GroupMode::Recording])
                .track("Kick Out").modes(&[GroupMode::Full, GroupMode::Recording])
                .track("Kick Trig").modes(&[GroupMode::Full, GroupMode::Midi])
            .end()
            .track("Kick Sub").modes(&[GroupMode::Full])
            .track("Kick Ambient").modes(&[GroupMode::Full])
        .end()
        .build()
}
