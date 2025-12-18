use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::traits::{TemplateSource, Matcher};
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::matching::matcher::MatchResult;
use crate::smart_template::core::errors::TemplateMatchError;
use daw::tracks::TrackName;
use super::Lead;

impl TemplateSource for Lead {
    fn full_template(&self) -> Template {
        Template::builder("Lead Full")
            .track("Lead")
            .build()
    }

    fn default_template(&self) -> Template {
        Template::builder("Lead Default")
            .track("Lead")
            .build()
    }

    fn minimal_template(&self) -> Template {
        Template::builder("Lead Minimal")
            .track("Lead")
            .build()
    }

    fn template(&self) -> Template {
        self.full_template()
    }
}

impl Matcher for Lead {
    type TrackName = ItemProperties;
    type Error = TemplateMatchError;

    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        crate::smart_template::features::matching::matcher::helpers::instrument_find_best_match(
            &self.template(),
            track_name,
            "Lead",
        )
    }

    fn find_or_create_track(&mut self, track_name: &Self::TrackName, base_name: Option<&str>) -> Result<(TrackName, bool), Self::Error> {
        if let Some(result) = self.find_best_match(track_name) {
            return Ok((result.track_name, result.use_takes));
        }
        let name = base_name.unwrap_or("Lead");
        Ok((TrackName::from(name), false))
    }
}
