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
use super::keys::Keys;

impl TemplateSource for Synths {
    fn full_template(&self) -> Template {
        Template::builder("Synths Full")
            .bus("Synths")
                .add_template(Lead::new().full_template())
                .add_template(Chord::new().full_template())
                .add_template(Pad::new().full_template())
                .add_template(Arp::new().full_template())
                .add_template(Keys::new().full_template())
                .add_template(FX::new().full_template())
            .end()
            .build()
    }

    fn default_template(&self) -> Template {
        Template::builder("Synths Default")
            .bus("Synths")
                .add_template(Lead::new().default_template())
                .add_template(Chord::new().default_template())
                .add_template(Pad::new().default_template())
                .add_template(Arp::new().default_template())
                .add_template(Keys::new().default_template())
                .add_template(FX::new().default_template())
            .end()
            .build()
    }

    fn minimal_template(&self) -> Template {
        Template::builder("Synths Minimal")
            .add_template(Lead::new().minimal_template())
            .add_template(Chord::new().minimal_template())
            .add_template(Pad::new().minimal_template())
            .add_template(Arp::new().minimal_template())
            .add_template(Keys::new().minimal_template())
            .add_template(FX::new().minimal_template())
            .build()
    }

    fn template(&self) -> Template {
        self.full_template()
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
