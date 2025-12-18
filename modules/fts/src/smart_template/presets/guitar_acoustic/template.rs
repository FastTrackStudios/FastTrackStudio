use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::models::organization::OrganizationMode;
use crate::smart_template::core::traits::{TemplateSource, Matcher};
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::matching::matcher::MatchResult;
use crate::smart_template::core::errors::TemplateMatchError;
use daw::tracks::TrackName;
use super::GuitarAcoustic;

impl TemplateSource for GuitarAcoustic {
    fn template(&self) -> Template {
        let mut builder = Template::builder("Guitar Acoustic")
            .with_mode(self.mode)
            .bus("GTR Acoustic");

        match self.mode {
            OrganizationMode::ByPerformer => {
                builder = builder
                    .performer("Cody")
                        .entry("Strum", &[], &["Mic L", "Mic R", "DI"])
                    .end()
                    .performer("Luke")
                        .source("Mic")
                        .source("DI")
                    .end();
            }
            OrganizationMode::ByArrangement => {
                builder = builder
                    .with_performer("Cody")
                    .entry("Strum", &[], &["Mic L", "Mic R", "DI"])
                    .with_performer("Luke")
                    .bus("Luke")
                        .source("Mic")
                        .source("DI")
                    .end();
            }
        }

        builder.build()
    }
}

impl Matcher for GuitarAcoustic {
    type TrackName = ItemProperties;
    type Error = TemplateMatchError;

    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        crate::smart_template::features::matching::matcher::helpers::instrument_find_best_match(
            &self.template(),
            track_name,
            "GTR Acoustic",
        )
    }

    fn find_or_create_track(&mut self, track_name: &Self::TrackName, base_name: Option<&str>) -> Result<(TrackName, bool), Self::Error> {
        if let Some(result) = self.find_best_match(track_name) {
            return Ok((result.track_name, result.use_takes));
        }
        
        let name = base_name.unwrap_or("GTR Acoustic");
        Ok((TrackName::from(name), false))
    }
}
