//! Kick template implementation

use crate::smart_template::features::matching::matcher::MatchResult;
use crate::smart_template::core::models::template::Template;
use crate::smart_template::utils::track_helpers::create_track;
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::core::models::GroupMode;
use daw::tracks::TrackName;
use super::Kick;

/// Generate the default Kick track structure
pub fn generate_kick_structure() -> Template {
    Template::builder("Kick")
        .bus("Kick")
            .sum("Kick (SUM)").modes(&[GroupMode::Full, GroupMode::Minimal])
                .track("Kick In").modes(&[GroupMode::Full, GroupMode::Recording])
                .track("Kick Out").modes(&[GroupMode::Full, GroupMode::Recording])
                .track("Kick Trig").modes(&[GroupMode::Full, GroupMode::Midi])
            .end()
            .track("Kick Sub").modes(&[GroupMode::Full])
            .track("Kick Ambient").modes(&[GroupMode::Full])
        .end()
        .build()
}

/// Find best match for Kick
pub fn find_best_match(kick: &Kick, track_name: &ItemProperties) -> Option<MatchResult> {
    let search_name = match track_name.sub_type.as_ref().and_then(|s| s.first()) {
        Some(first) => format!("Kick {}", first),
        None => "Kick".to_string(),
    };
    
    crate::smart_template::features::matching::matcher::helpers::instrument_find_best_match(
        &kick.template,
        track_name,
        &search_name,
    )
}

/// Find or create track for Kick
pub fn find_or_create_track(kick: &mut Kick, track_name: &ItemProperties, base_name: Option<&str>) -> Result<(TrackName, bool), KickMatchError> {
    if let Some(result) = find_best_match(kick, track_name) {
        return Ok((result.track_name, result.use_takes));
    }
    
    let new_track_name = TrackName::from(base_name
        .map(|s| s.to_string())
        .unwrap_or_else(|| {
            match track_name.sub_type.as_ref().and_then(|s| s.first()) {
                Some(first) => format!("Kick {}", first),
                None => "Kick".to_string(),
            }
        }));
    
    kick.template.tracks.push(create_track(
        &new_track_name.0,
        None,
        Some("Kick"),
        &[],
    ));
    
    Ok((new_track_name, false))
}

#[derive(Debug, thiserror::Error)]
pub enum KickMatchError {
    #[error("Match error: {0}")]
    Other(String),
}
