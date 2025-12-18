use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::traits::{TemplateSource, Matcher};
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::matching::matcher::MatchResult;
use crate::smart_template::core::errors::TemplateMatchError;
use daw::tracks::TrackName;
use super::Keys;
use super::piano::Piano;
use super::electric_keys::ElectricKeys;
use super::organ::Organ;
use super::harpsichord::Harpsichord;
use super::clavichord::Clavichord;

impl TemplateSource for Keys {
    fn full_template(&self) -> Template {
        Template::builder("Keys Full")
            .bus("Keys")
                .add_template(Piano::new().full_template())
                .add_template(ElectricKeys::new().full_template())
                .add_template(Organ::new().full_template())
                .add_template(Harpsichord::new().full_template())
                .add_template(Clavichord::new().full_template())
            .end()
            .build()
    }

    fn default_template(&self) -> Template {
        Template::builder("Keys Default")
            .bus("Keys")
                .add_template(Piano::new().default_template())
                .add_template(ElectricKeys::new().default_template())
                .add_template(Organ::new().default_template())
                .add_template(Harpsichord::new().default_template())
                .add_template(Clavichord::new().default_template())
            .end()
            .build()
    }

    fn minimal_template(&self) -> Template {
        Template::builder("Keys Minimal")
            .add_template(Piano::new().minimal_template())
            .add_template(ElectricKeys::new().minimal_template())
            .add_template(Organ::new().minimal_template())
            .add_template(Harpsichord::new().minimal_template())
            .add_template(Clavichord::new().minimal_template())
            .build()
    }

    fn template(&self) -> Template {
        self.full_template()
    }
}

impl Matcher for Keys {
    type TrackName = ItemProperties;
    type Error = TemplateMatchError;

    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        crate::smart_template::features::matching::matcher::helpers::instrument_find_best_match(
            &self.template(),
            track_name,
            "Keys",
        )
    }

    fn find_or_create_track(&mut self, track_name: &Self::TrackName, base_name: Option<&str>) -> Result<(TrackName, bool), Self::Error> {
        if let Some(result) = self.find_best_match(track_name) {
            return Ok((result.track_name, result.use_takes));
        }
        let name = base_name.unwrap_or("Keys");
        Ok((TrackName::from(name), false))
    }
}
