//! Drum Kit template generator and matcher implementation

use crate::smart_template::core::traits::{Group, TemplateSource, Matcher};
use crate::smart_template::features::matching::matcher::MatchResult;
use crate::smart_template::core::models::template::Template;
use crate::smart_template::utils::track_helpers::TrackExt;
use crate::smart_template::core::models::group_config::GroupConfig;
use crate::smart_template::presets::drums::kick::Kick;
use crate::smart_template::presets::drums::snare::Snare;
use crate::smart_template::presets::drums::tom::Tom;
use crate::smart_template::presets::drums::cymbals::Cymbals;
use crate::smart_template::presets::drums::room::Room;
use crate::smart_template::core::errors::{TemplateMatchError};
use daw::tracks::{TrackName, Track};

/// Drum Kit consolidated struct
pub struct DrumKit {}

impl DrumKit {
    /// Create a new Drum Kit instrument
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for DrumKit {
    fn default() -> Self {
        Self::new()
    }
}

impl Group for DrumKit {
    fn group_name(&self) -> &str {
        "Drum Kit"
    }

    fn group_config(&self) -> GroupConfig {
        GroupConfig {
            name: "Drum Kit".to_string(),
            ..Default::default()
        }
    }

    fn default_tracklist(&self) -> Vec<Track> {
        self.template().tracks
    }
}

impl TemplateSource for DrumKit {
    fn full_template(&self) -> Template {
        Template::builder("Drum Kit Full")
            .bus("Drum Kit")
                .add_template(Kick::new().full_template())
                .add_template(Snare::new().full_template())
                .add_template(Tom::new().full_template())
                .add_template(Cymbals::new().full_template())
                .add_template(Room::new().full_template())
            .end()
            .build()
    }

    fn default_template(&self) -> Template {
        Template::builder("Drum Kit Default")
            .bus("Drum Kit")
                .add_template(Kick::new().default_template())
                .add_template(Snare::new().default_template())
                .add_template(Tom::new().default_template())
                .add_template(Cymbals::new().default_template())
                .add_template(Room::new().default_template())
            .end()
            .build()
    }

    fn minimal_template(&self) -> Template {
        Template::builder("Drum Kit Minimal")
            .add_template(Kick::new().minimal_template())
            .add_template(Snare::new().minimal_template())
            .add_template(Tom::new().minimal_template())
            .add_template(Cymbals::new().minimal_template())
            .add_template(Room::new().minimal_template())
            .build()
    }

    fn template(&self) -> Template {
        self.full_template()
    }
}

impl Matcher for DrumKit {
    type TrackName = crate::smart_template::features::naming::item_properties::ItemProperties;
    type Error = TemplateMatchError;
    
    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        let search_name = track_name.original_name.as_deref().unwrap_or("");
        let template = self.template();
        
        if let Some(track) = template.tracks.iter()
            .find(|t| t.name.eq_ignore_case(search_name)) {
            return Some(MatchResult {
                track_name: track.name.clone(),
                match_type: crate::smart_template::features::matching::matcher::MatchType::Exact,
                score: 100,
                use_takes: false,
            });
        }
        
        if let Some(track) = template.tracks.iter()
            .find(|t| t.name.contains_ignore_case(search_name)) {
            return Some(MatchResult {
                track_name: track.name.clone(),
                match_type: crate::smart_template::features::matching::matcher::MatchType::Partial,
                score: 60,
                use_takes: false,
            });
        }
        
        None
    }
    
    fn find_or_create_track(&mut self, track_name: &Self::TrackName, base_name: Option<&str>) -> Result<(TrackName, bool), Self::Error> {
        if let Some(result) = self.find_best_match(track_name) {
            return Ok((result.track_name, result.use_takes));
        }
        
        let new_track_name = TrackName::from(base_name
            .map(|s| s.to_string())
            .unwrap_or_else(|| {
                track_name.original_name.clone().unwrap_or_else(|| "Unknown".to_string())
            }));
        
        Ok((new_track_name, false))
    }
}
