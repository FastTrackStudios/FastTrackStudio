//! Item properties for FTS naming convention
//!
//! This module provides the ItemProperties struct which represents
//! parsed or constructed track/item names with all their components.

use derive_builder::Builder;
use serde::{Deserialize, Serialize};

/// Represents a parsed or constructed track name with all its components.
/// This is stored in Track.ext_state as JSON and represents the full FTS naming convention.
#[derive(Debug, Default, Clone, PartialEq, Eq, Serialize, Deserialize, Builder)]
#[builder(setter(into, strip_option), default)]
pub struct ItemProperties {
    /// Recording tag (e.g., "PASS-01", "TAKE-02", "REC-01")
    pub rec_tag: Option<String>,

    /// Group prefix (e.g., "D", "GTR", "Bass", "SY")
    pub group_prefix: Option<String>,

    /// Sub-types within the group (e.g., ["Kick"], ["SY"], ["Electric"])
    pub sub_type: Option<Vec<String>>,

    /// Performer name (e.g., "Cody", "Joshua", "Sarah")
    pub performer: Option<String>,

    /// Arrangement style (e.g., "Rhythm", "Solo", "Amb", "Crunch")
    pub arrangement: Option<String>,

    /// Section of the song (e.g., "Intro", "Verse", "Chorus", "Bridge")
    pub section: Option<String>,

    /// Layer information (e.g., "DBL", "OCT", "L", "R", "Stereo")
    pub layers: Option<String>,

    /// Multi-mic positions (e.g., ["Top", "Bottom"], ["In", "Out"], ["Close", "Room"])
    pub multi_mic: Option<Vec<String>>,

    /// Effect/send indicators (e.g., ["Verb"], ["Delay"], ["Chorus", "Verb"])
    pub effect: Option<Vec<String>>,

    /// Increment number for numbered instances (e.g., "1", "2" for Tom 1, Tom 2)
    pub increment: Option<String>,

    /// Channel information (e.g., "L", "R", "C", "Left", "Right")
    pub channel: Option<String>,

    /// Playlist identifier (e.g., ".1", ".2", ".3", ".A", ".B")
    pub playlist: Option<String>,

    /// Track type indicator (e.g., "BUS", "SUM", "MIDI", "DI", "NOFX")
    pub track_type: Option<String>,

    /// Words that didn't match any known patterns (for validation/debugging)
    pub unparsed_words: Option<Vec<String>>,

    /// Original input string before parsing (preserves full context for later analysis)
    /// This allows removing context based on matched group, but still checking the original context later
    pub original_name: Option<String>,

    /// File extension if parsed from a filename (e.g., ".wav", ".aiff", ".flac")
    /// This is typically not included in string formatting output
    pub file_extension: Option<String>,
}

impl ItemProperties {
    /// Create a new ItemProperties with just an original name
    pub fn from_name(name: impl Into<String>) -> Self {
        Self {
            original_name: Some(name.into()),
            ..Default::default()
        }
    }

    /// Create an ItemProperties for a kick drum
    pub fn kick(name: impl Into<String>) -> Self {
        Self {
            original_name: Some(name.into()),
            group_prefix: Some("D".to_string()),
            sub_type: Some(vec!["Kick".to_string()]),
            ..Default::default()
        }
    }

    /// Create an ItemProperties for a snare drum
    pub fn snare(name: impl Into<String>) -> Self {
        Self {
            original_name: Some(name.into()),
            group_prefix: Some("D".to_string()),
            sub_type: Some(vec!["Snare".to_string()]),
            ..Default::default()
        }
    }

    /// Create an ItemProperties for a guitar
    pub fn guitar(name: impl Into<String>) -> Self {
        Self {
            original_name: Some(name.into()),
            group_prefix: Some("GTR".to_string()),
            ..Default::default()
        }
    }

    /// Create an ItemProperties for bass
    pub fn bass(name: impl Into<String>) -> Self {
        Self {
            original_name: Some(name.into()),
            group_prefix: Some("Bass".to_string()),
            ..Default::default()
        }
    }

    /// Create an ItemProperties for vocals
    pub fn vocal(name: impl Into<String>) -> Self {
        Self {
            original_name: Some(name.into()),
            group_prefix: Some("Vox".to_string()),
            ..Default::default()
        }
    }

    /// Create an ItemProperties for synth
    pub fn synth(name: impl Into<String>) -> Self {
        Self {
            original_name: Some(name.into()),
            group_prefix: Some("SY".to_string()),
            ..Default::default()
        }
    }
}

/// Extension trait to easily create Items from ItemProperties
pub trait IntoItem {
    /// Create an Item with these properties and default values for everything else
    fn into_item(self) -> crate::tracks::item::Item;
}

impl IntoItem for ItemProperties {
    fn into_item(self) -> crate::tracks::item::Item {
        use crate::tracks::item::ItemBuilder;

        ItemBuilder::default()
            .properties(self)
            .build()
            .expect("Failed to build Item from ItemProperties")
    }
}

impl IntoItem for ItemPropertiesBuilder {
    fn into_item(self) -> crate::tracks::item::Item {
        use crate::tracks::item::ItemBuilder;

        let properties = self.build().expect("Failed to build ItemProperties");
        ItemBuilder::default()
            .properties(properties)
            .build()
            .expect("Failed to build Item from ItemProperties")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_item_properties_builder() {
        let props = ItemPropertiesBuilder::default()
            .original_name("kick_in.wav")
            .group_prefix("D")
            .sub_type(vec!["Kick"])
            .build()
            .unwrap();

        assert_eq!(props.original_name, Some("kick_in.wav".to_string()));
        assert_eq!(props.group_prefix, Some("D".to_string()));
        assert_eq!(props.sub_type, Some(vec!["Kick".to_string()]));
    }

    #[test]
    fn test_convenience_constructors() {
        let kick = ItemProperties::kick("kick_out");
        assert_eq!(kick.group_prefix, Some("D".to_string()));
        assert_eq!(kick.sub_type, Some(vec!["Kick".to_string()]));

        let guitar = ItemProperties::guitar("GTR_Rhythm");
        assert_eq!(guitar.group_prefix, Some("GTR".to_string()));
    }

    #[test]
    fn test_into_item() {
        // Create an Item directly from ItemProperties
        let item = ItemProperties::kick("kick_in.wav").into_item();
        assert_eq!(
            item.properties.original_name,
            Some("kick_in.wav".to_string())
        );
        assert_eq!(item.properties.group_prefix, Some("D".to_string()));

        // Create an Item directly from ItemPropertiesBuilder
        let item2 = ItemPropertiesBuilder::default()
            .original_name("snare.wav")
            .group_prefix("D")
            .sub_type(vec!["Snare"])
            .into_item();
        assert_eq!(
            item2.properties.original_name,
            Some("snare.wav".to_string())
        );
    }
}
