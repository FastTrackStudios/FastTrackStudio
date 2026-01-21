//! Track classification using dynamic-template patterns
//!
//! This module provides track classification by reusing the pattern matching
//! from dynamic-template without the full sorting/organization logic.

use dynamic_template::{DynamicTemplateConfig, OrganizeIntoTracks, default_config};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Classification result for a single track
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct TrackClassification {
    /// The primary group this track belongs to (e.g., "Drums", "Guitars", "Vocals")
    pub primary_group: Option<String>,

    /// Sub-group within the primary group (e.g., "Kick", "Snare" within Drums)
    pub sub_group: Option<String>,

    /// Tertiary classification (e.g., "In", "Out" for Kick In/Out)
    pub tertiary: Option<String>,

    /// The full classification path (e.g., ["Drums", "Kick", "In"])
    pub path: Vec<String>,

    /// Confidence score (0.0 to 1.0) - how well the track name matches patterns
    pub confidence: f32,
}

impl TrackClassification {
    /// Create a new classification with a primary group
    pub fn new(primary: impl Into<String>) -> Self {
        let primary = primary.into();
        Self {
            primary_group: Some(primary.clone()),
            path: vec![primary],
            confidence: 1.0,
            ..Default::default()
        }
    }

    /// Add a sub-group to the classification
    pub fn with_sub_group(mut self, sub: impl Into<String>) -> Self {
        let sub = sub.into();
        self.sub_group = Some(sub.clone());
        self.path.push(sub);
        self
    }

    /// Add a tertiary classification
    pub fn with_tertiary(mut self, tertiary: impl Into<String>) -> Self {
        let tertiary = tertiary.into();
        self.tertiary = Some(tertiary.clone());
        self.path.push(tertiary);
        self
    }

    /// Check if this track is classified (has at least a primary group)
    pub fn is_classified(&self) -> bool {
        self.primary_group.is_some()
    }

    /// Get the full path as a string (e.g., "Drums/Kick/In")
    pub fn path_string(&self) -> String {
        self.path.join("/")
    }
}

/// Classify a single track by its name
///
/// Uses dynamic-template's pattern matching to determine what instrument category
/// a track belongs to.
///
/// # Example
/// ```
/// use visibility_manager::classify_track;
///
/// let classification = classify_track("Kick In");
/// assert_eq!(classification.primary_group.as_deref(), Some("Drums"));
/// ```
pub fn classify_track(track_name: &str) -> TrackClassification {
    classify_track_with_config(track_name, &default_config())
}

/// Classify a track using a custom configuration
pub fn classify_track_with_config(
    track_name: &str,
    config: &DynamicTemplateConfig,
) -> TrackClassification {
    // Use dynamic-template's sorting to classify a single item
    let items = vec![track_name.to_string()];

    match items.organize_into_tracks(config, None) {
        Ok(tracks) => {
            // Extract classification from the resulting track structure
            extract_classification_from_tracks(&tracks, track_name)
        }
        Err(_) => TrackClassification::default(),
    }
}

/// Classify multiple tracks at once (more efficient than classifying one at a time)
///
/// Returns a map from track name to classification
pub fn classify_tracks<'a>(
    track_names: impl IntoIterator<Item = &'a str>,
) -> HashMap<String, TrackClassification> {
    classify_tracks_with_config(track_names, &default_config())
}

/// Classify multiple tracks with a custom configuration
pub fn classify_tracks_with_config<'a>(
    track_names: impl IntoIterator<Item = &'a str>,
    config: &DynamicTemplateConfig,
) -> HashMap<String, TrackClassification> {
    let names: Vec<String> = track_names.into_iter().map(|s| s.to_string()).collect();

    if names.is_empty() {
        return HashMap::new();
    }

    // Store original names for lookup
    let original_names: Vec<String> = names.clone();

    match names.organize_into_tracks(config, None) {
        Ok(tracks) => {
            let mut classifications = HashMap::new();

            // Extract classification for each original name
            for name in &original_names {
                let classification = extract_classification_from_tracks(&tracks, name);
                classifications.insert(name.clone(), classification);
            }

            classifications
        }
        Err(_) => {
            // Return empty classifications on error
            original_names
                .into_iter()
                .map(|name| (name, TrackClassification::default()))
                .collect()
        }
    }
}

/// Extract classification from the organized track structure
fn extract_classification_from_tracks(
    tracks: &[daw::tracks::Track],
    item_name: &str,
) -> TrackClassification {
    // Walk through the track structure to find where this item ended up
    let mut classification = TrackClassification::default();
    let mut depth = 0i32;
    let mut path_stack: Vec<String> = Vec::new();

    for track in tracks {
        // Update path based on depth
        let current_depth = depth.max(0) as usize;

        // Trim path_stack to current depth
        while path_stack.len() > current_depth {
            path_stack.pop();
        }

        // Add current track to path if it's a folder
        if track.is_folder || !track.items.is_empty() {
            if path_stack.len() == current_depth {
                path_stack.push(track.name.0.clone());
            }
        }

        // Check if this track contains our item
        let contains_item = track.items.iter().any(|item| item.name == item_name);

        if contains_item {
            classification.path = path_stack.clone();
            classification.confidence = 1.0;

            // Set primary, sub, tertiary based on path depth
            if let Some(primary) = classification.path.first() {
                classification.primary_group = Some(primary.clone());
            }
            if classification.path.len() > 1 {
                classification.sub_group = classification.path.get(1).cloned();
            }
            if classification.path.len() > 2 {
                classification.tertiary = classification.path.get(2).cloned();
            }

            return classification;
        }

        // Update depth for next iteration
        depth += track.folder_depth_change.to_reaper_value();
    }

    // Check if item landed in "Unsorted" or wasn't found
    classification
}

/// Get all known group names from the default configuration
///
/// Returns the list of top-level instrument categories that can be used
/// for visibility grouping.
pub fn known_groups() -> Vec<&'static str> {
    vec![
        "Drums",
        "Percussion",
        "Bass",
        "Guitars",
        "Keys",
        "Synths",
        "Horns",
        "Harmonica",
        "Vocals",
        "Choir",
        "Orchestra",
        "SFX",
        "Guide",
        "Reference",
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_classify_kick() {
        let classification = classify_track("Kick In");
        assert!(classification.is_classified());
        assert_eq!(classification.primary_group.as_deref(), Some("Drums"));
    }

    #[test]
    fn test_classify_guitar() {
        let classification = classify_track("Electric Guitar");
        assert!(classification.is_classified());
        assert_eq!(classification.primary_group.as_deref(), Some("Guitars"));
    }

    #[test]
    fn test_classify_lead_vocal() {
        let classification = classify_track("Lead Vox");
        assert!(classification.is_classified());
        assert_eq!(classification.primary_group.as_deref(), Some("Vocals"));
    }

    #[test]
    fn test_classify_multiple() {
        let names = vec!["Kick In", "Snare Top", "Bass DI", "Lead Vox"];
        let classifications = classify_tracks(names);

        assert_eq!(
            classifications
                .get("Kick In")
                .and_then(|c| c.primary_group.as_deref()),
            Some("Drums")
        );
        assert_eq!(
            classifications
                .get("Bass DI")
                .and_then(|c| c.primary_group.as_deref()),
            Some("Bass")
        );
    }

    #[test]
    fn test_known_groups() {
        let groups = known_groups();
        assert!(groups.contains(&"Drums"));
        assert!(groups.contains(&"Vocals"));
        assert!(groups.contains(&"Guitars"));
    }
}
