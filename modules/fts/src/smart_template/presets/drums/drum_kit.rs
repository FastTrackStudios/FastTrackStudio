//! Drum Kit template generator and matcher implementation

use crate::smart_template::core::traits::{Group, TemplateSource, Matcher};
use crate::smart_template::features::matching::matcher::{MatchResult, MatchType};
use crate::smart_template::core::models::template::Template;
use crate::smart_template::utils::track_helpers::{create_track, TrackExt};
use crate::smart_template::core::models::group_config::GroupConfig;
use crate::smart_template::presets::drums::kick::Kick;
use crate::smart_template::presets::drums::snare::Snare;
use crate::smart_template::presets::drums::tom::Tom;
use crate::smart_template::presets::drums::cymbals::Cymbals;
use crate::smart_template::presets::drums::room::Room;
use daw::tracks::{TrackName, Track};

/// Drum Kit consolidated struct
pub struct DrumKit {
    pub config: GroupConfig,
    pub template: Template,
}

impl DrumKit {
    /// Create a new Drum Kit instrument
    pub fn new() -> Self {
        let config = GroupConfig {
            name: "Drum Kit".to_string(),
            ..Default::default()
        };
        let mut drum_kit = Self {
            config,
            template: Template {
                name: TrackName::from("Drum Kit"),
                tracks: Vec::new(),
            },
        };
        drum_kit.template = drum_kit.generate_template();
        drum_kit
    }

    fn generate_template(&self) -> Template {
        let mut tracks = Vec::new();
        
        // Drum Kit (BUS) - top-level parent
        tracks.push(create_track("Drum Kit", Some("BUS"), None, &[]));
        
        // Combine all instrument templates
        let instrument_templates = vec![
            Kick::new().template(),
            Snare::new().template(),
            Tom::new().template(),
            Cymbals::new().template(),
            Room::new().template(),
        ];
        
        for inst_template in instrument_templates {
            for mut track in inst_template.tracks {
                // If the track has no parent, make it a child of Drum Kit
                if track.parent_name().is_none() {
                    track.set_parent_name("Drum Kit");
                }
                tracks.push(track);
            }
        }
        
        Template {
            name: TrackName::from("Drum Kit"),
            tracks,
        }
    }
}

impl Default for DrumKit {
    fn default() -> Self {
        Self::new()
    }
}

impl Group for DrumKit {
    fn name(&self) -> &str {
        "Drum Kit"
    }

    fn config(&self) -> &GroupConfig {
        &self.config
    }

    fn default_tracklist(&self) -> Vec<Track> {
        self.template.tracks.clone()
    }
}

impl TemplateSource for DrumKit {
    fn template(&self) -> Template {
        self.template.clone()
    }
}

impl Matcher for DrumKit {
    type TrackName = crate::smart_template::features::naming::item_properties::ItemProperties;
    type Error = DrumKitMatchError;
    
    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult> {
        let search_name = track_name.original_name.as_deref().unwrap_or("");
        
        if let Some(track) = self.template.tracks.iter()
            .find(|t| t.name.eq_ignore_case(search_name)) {
            return Some(MatchResult {
                track_name: track.name.clone(),
                match_type: MatchType::Exact,
                score: 100,
                use_takes: false,
            });
        }
        
        if let Some(track) = self.template.tracks.iter()
            .find(|t| t.name.contains_ignore_case(search_name)) {
            return Some(MatchResult {
                track_name: track.name.clone(),
                match_type: MatchType::Partial,
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
        
        self.template.tracks.push(create_track(
            &new_track_name.0,
            None,
            Some("Drum Kit"),
            &[],
        ));
        
        Ok((new_track_name, false))
    }
}

#[derive(Debug, thiserror::Error)]
pub enum DrumKitMatchError {
    #[error("Match error: {0}")]
    Other(String),
}
