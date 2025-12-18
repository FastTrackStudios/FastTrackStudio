use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::models::organization::OrganizationMode;
use crate::smart_template::core::traits::{TemplateSource, Matcher};
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::matching::matcher::MatchResult;
use crate::smart_template::core::errors::TemplateMatchError;
use daw::tracks::TrackName;
use super::GuitarElectric;

impl TemplateSource for GuitarElectric {
    fn template(&self) -> Template {
        let mut builder = Template::builder("Guitar Electric")
            .with_mode(self.mode)
            .bus("GTR Electric");

        match self.mode {
            OrganizationMode::ByPerformer => {
                builder = builder
                    .performer("Cody")
                        .entry("Drive", &["Main", "DBL"], &["NO-FX", "DI"])
                        .entry("Clean", &[], &["NO-FX", "DI"])
                    .end()
                    .performer("Luke")
                        .entry("Lead", &[], &["NO-FX", "DI"])
                    .end();
            }
            OrganizationMode::ByArrangement => {
                builder = builder
                    .with_performer("Cody")
                    .entry("Drive", &["Main", "DBL"], &["NO-FX", "DI"])
                    .with_performer("Cody")
                    .entry("Clean", &[], &["NO-FX", "DI"])
                    .with_performer("Luke")
                    .entry("Lead", &[], &["NO-FX", "DI"])
                    .with_performer("Luke")
                    .bus("Luke")
                        .source("GTR")
                        .source("GTR")
                    .end();
            }
        }

        builder.build()
    }
}

impl Matcher for GuitarElectric {
    type TrackName = ItemProperties;
    type Error = TemplateMatchError;

    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        crate::smart_template::features::matching::matcher::helpers::instrument_find_best_match(
            &self.template(),
            track_name,
            "GTR Electric",
        )
    }

    fn find_or_create_track(&mut self, track_name: &Self::TrackName, base_name: Option<&str>) -> Result<(TrackName, bool), Self::Error> {
        if let Some(result) = self.find_best_match(track_name) {
            return Ok((result.track_name, result.use_takes));
        }
        
        let name = base_name.unwrap_or("GTR Electric");
        Ok((TrackName::from(name), false))
    }
}
