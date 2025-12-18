use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::models::GroupMode;
use crate::smart_template::core::traits::{TemplateSource, Matcher};
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::matching::matcher::MatchResult;
use crate::smart_template::core::errors::TemplateMatchError;
use crate::smart_template::utils::track_helpers::create_track;
use daw::tracks::TrackName;
use super::Cymbals;
use std::sync::{Mutex, OnceLock};

static TEMPLATE: OnceLock<Mutex<Template>> = OnceLock::new();

impl TemplateSource for Cymbals {
    fn full_template(&self) -> Template {
        Template::builder("Cymbals Full")
            .bus("Cymbals")
                .track("Hi-Hat")
                .track("Ride")
                .track("OH")
            .end()
            .build()
    }

    fn default_template(&self) -> Template {
        Template::builder("Cymbals Default")
            .bus("Cymbals")
                .track("Hi-Hat")
                .track("Ride")
                .track("OH")
            .end()
            .build()
    }

    fn minimal_template(&self) -> Template {
        Template::builder("Cymbals Minimal")
            .track("Hi-Hat")
            .track("Ride")
            .track("OH")
            .build()
    }

    fn template(&self) -> Template {
        self.full_template()
    }
}

impl Matcher for Cymbals {
    type TrackName = ItemProperties;
    type Error = TemplateMatchError;

    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        let search_name = match track_name.sub_type.as_ref().and_then(|s| s.first()) {
            Some(first) => match first.as_str() {
                "Hi Hat" => "Hi-Hat".to_string(),
                "Overheads" => "OH".to_string(),
                _ => first.clone(),
            },
            None => "Cymbals".to_string(),
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
                    Some(first) => match first.as_str() {
                        "Hi Hat" => "Hi-Hat".to_string(),
                        "Overheads" => "OH".to_string(),
                        _ => first.clone(),
                    },
                    None => "Cymbals".to_string(),
                }
            }));
        
        let mut template_lock = TEMPLATE.get_or_init(|| Mutex::new(generate_cymbals_structure())).lock().unwrap();
        template_lock.tracks.push(create_track(
            &new_track_name.0,
            None,
            Some("Cymbals"),
            &[],
        ));
        
        Ok((new_track_name, false))
    }
}

/// Generate the default Cymbals track structure
pub fn generate_cymbals_structure() -> Template {
    Template::builder("Cymbals")
        .bus("Cymbals")
            .track("Hi-Hat").modes(&[GroupMode::Full, GroupMode::Recording])
            .track("Ride").modes(&[GroupMode::Full, GroupMode::Recording])
            .track("OH").modes(&[GroupMode::Full, GroupMode::Recording])
        .end()
        .build()
}
