use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::models::GroupMode;
use crate::smart_template::core::traits::{TemplateSource, Matcher};
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::matching::matcher::MatchResult;
use crate::smart_template::core::errors::TemplateMatchError;
use daw::tracks::TrackName;
use super::Room;

impl TemplateSource for Room {
    fn full_template(&self) -> Template {
        Template::builder("Rooms Full")
            .bus("Rooms")
                .track("Rooms Close")
                .track("Rooms Far")
                .track("Room Mono")
            .end()
            .build()
    }

    fn default_template(&self) -> Template {
        Template::builder("Rooms Default")
            .bus("Rooms")
                .track("Rooms Close")
                .track("Rooms Far")
                .track("Room Mono")
            .end()
            .build()
    }

    fn minimal_template(&self) -> Template {
        Template::builder("Rooms Minimal")
            .track("Rooms Close")
            .track("Rooms Far")
            .track("Room Mono")
            .build()
    }

    fn template(&self) -> Template {
        self.full_template()
    }
}

impl Matcher for Room {
    type TrackName = ItemProperties;
    type Error = TemplateMatchError;

    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        crate::smart_template::features::matching::matcher::helpers::instrument_find_best_match(
            &self.template(),
            track_name,
            "Rooms",
        )
    }

    fn find_or_create_track(&mut self, track_name: &Self::TrackName, base_name: Option<&str>) -> Result<(TrackName, bool), Self::Error> {
        if let Some(result) = self.find_best_match(track_name) {
            return Ok((result.track_name, result.use_takes));
        }
        
        let name = base_name.unwrap_or("Rooms");
        Ok((TrackName::from(name), false))
    }
}

pub fn generate_room_structure() -> Template {
    Template::builder("Rooms")
        .bus("Rooms")
            .track("Rooms Close").modes(&[GroupMode::Full, GroupMode::Recording])
            .track("Rooms Far").modes(&[GroupMode::Full, GroupMode::Recording])
            .track("Room Mono").modes(&[GroupMode::Full, GroupMode::Recording])
        .end()
        .build()
}
