//! Tom template implementation

use crate::smart_template::features::matching::matcher::{MatchResult, MatchType};
use crate::smart_template::core::models::template::Template;
use crate::smart_template::utils::track_helpers::create_track;
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::core::models::GroupMode;
use daw::tracks::TrackName;
use super::Tom;

/// Generate the default Tom track structure
pub fn generate_tom_structure() -> Template {
    Template::builder("Tom")
        .bus("Tom")
        .end()
        .build()
}

/// Find best match for Tom
pub fn find_best_match(tom: &Tom, track_name: &ItemProperties) -> Option<MatchResult> {
    let mapping_result = {
        let mut mapper = tom.mapper.lock().unwrap();
        mapper.map_track_name(track_name)
    };
    
    let display_track_name = TrackName::from(mapping_result.display_name);
    
    let mut template = tom.template.lock().unwrap();
    if !template.tracks.iter().any(|t| t.name == display_track_name) {
        template.tracks.push(create_track(
            &display_track_name.0,
            None,
            Some("Tom"),
            &[GroupMode::Full, GroupMode::Recording],
        ));
    }
    
    if let Some(track) = template.tracks.iter()
        .find(|t| t.name == display_track_name) {
        return Some(MatchResult {
            track_name: track.name.clone(),
            match_type: MatchType::Exact,
            score: 100,
            use_takes: false,
        });
    }
    
    None
}

/// Find or create track for Tom
pub fn find_or_create_track(tom: &mut Tom, track_name: &ItemProperties, _base_name: Option<&str>) -> Result<(TrackName, bool), TomMatchError> {
    let mapping_result = {
        let mut mapper = tom.mapper.lock().unwrap();
        mapper.map_track_name(track_name)
    };
    
    let display_track_name = TrackName::from(mapping_result.display_name);
    
    let mut template = tom.template.lock().unwrap();
    if !template.tracks.iter().any(|t| t.name == display_track_name) {
        template.tracks.push(create_track(
            &display_track_name.0,
            None,
            Some("Tom"),
            &[GroupMode::Full, GroupMode::Recording],
        ));
    }
    
    Ok((display_track_name, false))
}

#[derive(Debug, thiserror::Error)]
pub enum TomMatchError {
    #[error("Match error: {0}")]
    Other(String),
}
