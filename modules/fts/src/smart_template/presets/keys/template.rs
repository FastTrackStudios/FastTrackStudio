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
    fn template(&self) -> Template {
        Template::builder("Keys")
            .bus("Keys")
                .add_template(Piano::new().template())
                .add_template(ElectricKeys::new().template())
                .add_template(Organ::new().template())
                .add_template(Harpsichord::new().template())
                .add_template(Clavichord::new().template())
            .end()
            .build()
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
