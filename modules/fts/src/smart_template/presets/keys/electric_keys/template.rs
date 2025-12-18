use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::traits::{TemplateSource, Matcher};
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::matching::matcher::MatchResult;
use crate::smart_template::core::errors::TemplateMatchError;
use daw::tracks::TrackName;
use super::ElectricKeys;

impl TemplateSource for ElectricKeys {
    fn template(&self) -> Template {
        Template::builder("Electric Keys")
            .bus("Electric Keys")
                .track("Rhodes")
                .track("Wurli")
                .track("Digital")
                .track("Keys")
            .end()
            .build()
    }
}

impl Matcher for ElectricKeys {
    type TrackName = ItemProperties;
    type Error = TemplateMatchError;

    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        crate::smart_template::features::matching::matcher::helpers::instrument_find_best_match(
            &self.template(),
            track_name,
            "Electric Keys",
        )
    }

    fn find_or_create_track(&mut self, track_name: &Self::TrackName, base_name: Option<&str>) -> Result<(TrackName, bool), Self::Error> {
        if let Some(result) = self.find_best_match(track_name) {
            return Ok((result.track_name, result.use_takes));
        }
        let name = base_name.unwrap_or("Electric Keys");
        Ok((TrackName::from(name), false))
    }
}
