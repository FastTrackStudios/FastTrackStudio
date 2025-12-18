//! Cymbals template implementation

use crate::smart_template::features::matching::matcher::{MatchResult, MatchType};
use crate::smart_template::core::models::template::Template;
use crate::smart_template::utils::track_helpers::create_track;
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::core::models::GroupMode;
use daw::tracks::TrackName;
use super::Cymbals;

/// Generate the default Cymbals track structure
pub fn generate_cymbals_structure() -> Template {
    Template::builder("Cymbals")
        .bus("Cymbals")
            .track("Hi-Hat").modes(&[GroupMode::Full, GroupMode::Recording])
            .track("Ride").modes(&[GroupMode::Full, GroupMode::Recording])
            .track("OH").modes(&[GroupMode::Full, GroupMode::Recording])
        .end()
        .build()
}

/// Find best match for Cymbals
pub fn find_best_match(cymbals: &Cymbals, track_name: &ItemProperties) -> Option<MatchResult> {
    let search_name = match track_name.sub_type.as_ref().and_then(|s| s.first()) {
        Some(first) => match first.as_str() {
            "Hi Hat" => "Hi-Hat".to_string(),
            "Overheads" => "OH".to_string(),
            _ => first.clone(),
        },
        None => "Cymbals".to_string(),
    };
    
    crate::smart_template::features::matching::matcher::helpers::instrument_find_best_match(
        &cymbals.template,
        track_name,
        &search_name,
    )
}

/// Find or create track for Cymbals
pub fn find_or_create_track(cymbals: &mut Cymbals, track_name: &ItemProperties, base_name: Option<&str>) -> Result<(TrackName, bool), CymbalsMatchError> {
    if let Some(result) = find_best_match(cymbals, track_name) {
        return Ok((result.track_name, result.use_takes));
    }
    
    let new_track_name = TrackName::from(base_name
        .map(|s| s.to_string())
        .unwrap_or_else(|| {
            match track_name.sub_type.as_ref().and_then(|s| s.first()) {
                Some(first) => match first.as_str() {
                    "Hi Hat" => "Hi-Hat".to_string(),
                    "Overheads" => "OH".to_string(),
                    _ => first.clone(),
                },
                None => "Cymbals".to_string(),
            }
        }));
    
    cymbals.template.tracks.push(create_track(
        &new_track_name.0,
        None,
        Some("Cymbals"),
        &[],
    ));
    
    Ok((new_track_name, false))
}

#[derive(Debug, thiserror::Error)]
pub enum CymbalsMatchError {
    #[error("Match error: {0}")]
    Other(String),
}
