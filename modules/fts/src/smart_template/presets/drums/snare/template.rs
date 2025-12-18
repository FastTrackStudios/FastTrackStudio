//! Snare template implementation

use crate::smart_template::features::matching::matcher::MatchResult;
use crate::smart_template::core::models::template::Template;
use crate::smart_template::utils::track_helpers::create_track;
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::core::models::GroupMode;
use daw::tracks::TrackName;
use super::Snare;

/// Generate the default Snare track structure
pub fn generate_snare_structure() -> Template {
    Template::builder("Snare")
        .bus("Snare")
            .sum("Snare (SUM)").modes(&[GroupMode::Full, GroupMode::Minimal])
                .track("Snare Top").modes(&[GroupMode::Full, GroupMode::Recording])
                .track("Snare Bottom").modes(&[GroupMode::Full, GroupMode::Recording])
                .track("Snare Trig").modes(&[GroupMode::Full, GroupMode::Midi])
            .end()
            .track("Snare Verb").modes(&[GroupMode::Full])
        .end()
        .build()
}

/// Find best match for Snare
pub fn find_best_match(snare: &Snare, track_name: &ItemProperties) -> Option<MatchResult> {
    let search_name = match track_name.sub_type.as_ref().and_then(|s| s.first()) {
        Some(first) => format!("Snare {}", first),
        None => "Snare".to_string(),
    };
    
    crate::smart_template::features::matching::matcher::helpers::instrument_find_best_match(
        &snare.template,
        track_name,
        &search_name,
    )
}

/// Find or create track for Snare
pub fn find_or_create_track(snare: &mut Snare, track_name: &ItemProperties, base_name: Option<&str>) -> Result<(TrackName, bool), SnareMatchError> {
    if let Some(result) = find_best_match(snare, track_name) {
        return Ok((result.track_name, result.use_takes));
    }
    
    let new_track_name = TrackName::from(base_name
        .map(|s| s.to_string())
        .unwrap_or_else(|| {
            match track_name.sub_type.as_ref().and_then(|s| s.first()) {
                Some(first) => format!("Snare {}", first),
                None => "Snare".to_string(),
            }
        }));
    
    snare.template.tracks.push(create_track(
        &new_track_name.0,
        None,
        Some("Snare"),
        &[],
    ));
    
    Ok((new_track_name, false))
}

#[derive(Debug, thiserror::Error)]
pub enum SnareMatchError {
    #[error("Match error: {0}")]
    Other(String),
}
