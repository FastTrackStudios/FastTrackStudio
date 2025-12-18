use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::traits::{TemplateSource, Matcher};
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::matching::matcher::MatchResult;
use crate::smart_template::core::errors::TemplateMatchError;
use daw::tracks::TrackName;
use super::Bass;

impl TemplateSource for Bass {
    fn full_template(&self) -> Template {
        Template::builder("Bass Full")
            .bus("Bass")
                .bus("Guitar")
                    .track("DI")
                    .track("Amp")
                .end()
                .bus("Synth")
                    .track("Bass Synth")
                .end()
            .end()
            .build()
    }

    fn default_template(&self) -> Template {
        Template::builder("Bass Default")
            .bus("Bass")
                .track("DI")
                .track("Amp")
            .end()
            .build()
    }

    fn minimal_template(&self) -> Template {
        Template::builder("Bass Minimal")
            .track("DI")
            .track("Amp")
            .build()
    }

    fn template(&self) -> Template {
        self.full_template()
    }
}

impl Matcher for Bass {
    type TrackName = ItemProperties;
    type Error = TemplateMatchError;

    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        crate::smart_template::features::matching::matcher::helpers::instrument_find_best_match(
            &self.template(),
            track_name,
            "Bass",
        )
    }

    fn find_or_create_track(&mut self, track_name: &Self::TrackName, base_name: Option<&str>) -> Result<(TrackName, bool), Self::Error> {
        if let Some(result) = self.find_best_match(track_name) {
            return Ok((result.track_name, result.use_takes));
        }
        
        let new_track_name = TrackName::from(base_name
            .map(|s| s.to_string())
            .unwrap_or_else(|| {
                track_name.original_name.clone().unwrap_or_else(|| "Bass".to_string())
            }));
        
        Ok((new_track_name, false))
    }
}
