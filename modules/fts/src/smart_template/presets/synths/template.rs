use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::traits::{TemplateSource, Matcher};
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::matching::matcher::MatchResult;
use crate::smart_template::core::errors::TemplateMatchError;
use daw::tracks::TrackName;
use super::Synths;
use super::lead::Lead;
use super::chord::Chord;
use super::pad::Pad;
use super::arp::Arp;
use super::fx::FX;

impl TemplateSource for Synths {
    fn template(&self) -> Template {
        Template::builder("Synths")
            .bus("Synths")
                .add_template(Lead::new().template())
                .add_template(Chord::new().template())
                .add_template(Pad::new().template())
                .add_template(Arp::new().template())
                .add_template(FX::new().template())
            .end()
            .build()
    }
}

impl Matcher for Synths {
    type TrackName = ItemProperties;
    type Error = TemplateMatchError;

    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        crate::smart_template::features::matching::matcher::helpers::instrument_find_best_match(
            &self.template(),
            track_name,
            "Synths",
        )
    }

    fn find_or_create_track(&mut self, track_name: &Self::TrackName, base_name: Option<&str>) -> Result<(TrackName, bool), Self::Error> {
        if let Some(result) = self.find_best_match(track_name) {
            return Ok((result.track_name, result.use_takes));
        }
        let name = base_name.unwrap_or("Synths");
        Ok((TrackName::from(name), false))
    }
}
