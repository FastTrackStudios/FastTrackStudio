use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::models::GroupMode;
use crate::smart_template::core::traits::{TemplateSource, Matcher};
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::matching::matcher::MatchResult;
use crate::smart_template::core::errors::TemplateMatchError;
use crate::smart_template::utils::track_helpers::create_track;
use daw::tracks::TrackName;
use super::Snare;
use std::sync::{Mutex, OnceLock};

static TEMPLATE: OnceLock<Mutex<Template>> = OnceLock::new();

impl TemplateSource for Snare {
    fn template(&self) -> Template {
        TEMPLATE.get_or_init(|| {
            Mutex::new(generate_snare_structure())
        }).lock().unwrap().clone()
    }
}

impl Matcher for Snare {
    type TrackName = ItemProperties;
    type Error = TemplateMatchError;

    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        let search_name = match track_name.sub_type.as_ref().and_then(|s| s.first()) {
            Some(first) => format!("Snare {}", first),
            None => "Snare".to_string(),
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
                    Some(first) => format!("Snare {}", first),
                    None => "Snare".to_string(),
                }
            }));
        
        let mut template_lock = TEMPLATE.get_or_init(|| Mutex::new(generate_snare_structure())).lock().unwrap();
        template_lock.tracks.push(create_track(
            &new_track_name.0,
            None,
            Some("Snare"),
            &[],
        ));
        
        Ok((new_track_name, false))
    }
}

/// Generate the default Snare track structure
pub fn generate_snare_structure() -> Template {
    Template::builder("Snare")
        .bus("Snare")
            .sum("Snare (SUM)").modes(&[GroupMode::Full, GroupMode::Minimal])
                .track("Snare Top").modes(&[GroupMode::Full, GroupMode::Recording])
                .track("Snare Bottom").modes(&[GroupMode::Full, GroupMode::Recording])
                .track("Snare Trig").modes(&[GroupMode::Full, GroupMode::Midi])
            .end()
        .end()
        .build()
}
