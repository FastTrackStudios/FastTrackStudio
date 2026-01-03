use monarchy::{Group, GroupBuilder, Metadata, ToDisplayName};
use serde::{Deserialize, Serialize};

/// Represents a parsed or constructed track name with all its components.
/// This is stored in Track.ext_state as JSON and represents the full FTS naming convention.
/// 
/// Note: `Metadata` derive automatically includes `MetadataBuilder` functionality,
/// so you only need to derive `Metadata` to get both the trait implementation
/// and the GroupBuilder extension methods (like `.multi_mic()`, `.layers()`, etc.).
#[derive(Debug, Default, Clone, PartialEq, Eq, Serialize, Deserialize, Metadata)]
pub struct ItemMetadata {
    /// Recording tag (e.g., "PASS-01", "TAKE-02", "REC-01")
    pub rec_tag: Option<String>,

    /// Full hierarchy of groups that matched, from top-level to most specific
    /// Example: ["Drums", "Drum_Kit", "Kick"]
    /// The last element is the most specific group that matched
    pub group: Option<Vec<String>>,

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

    /// Tagged collections that this item matches (e.g., ["SUM"] if it matches a tagged collection's patterns)
    /// An item can match multiple tagged collections
    pub tagged_collection: Option<Vec<String>>,

    /// Words that didn't match any known patterns (for validation/debugging)
    pub unparsed_words: Option<Vec<String>>,

    /// Original input string before parsing (preserves full context for later analysis)
    /// This allows removing context based on matched group, but still checking the original context later
    pub original_name: Option<String>,

    /// File extension if parsed from a filename (e.g., ".wav", ".aiff", ".flac")
    /// This is typically not included in string formatting output
    pub file_extension: Option<String>,
}

// Type alias for convenience
/// Type alias for `Group<ItemMetadata>`
pub type ItemMetadataGroup = Group<ItemMetadata>;

/// Builder for ItemMetadata with flexible input types
/// 
/// Allows passing strings, vectors, or arrays directly without explicit conversions.
#[derive(Default, Clone)]
pub struct ItemMetadataBuilder {
    rec_tag: Option<String>,
    group: Option<Vec<String>>,
    performer: Option<String>,
    arrangement: Option<String>,
    section: Option<String>,
    layers: Option<String>,
    multi_mic: Option<Vec<String>>,
    effect: Option<Vec<String>>,
    increment: Option<String>,
    channel: Option<String>,
    playlist: Option<String>,
    track_type: Option<String>,
    tagged_collection: Option<Vec<String>>,
    unparsed_words: Option<Vec<String>>,
    original_name: Option<String>,
    file_extension: Option<String>,
}

impl ItemMetadataBuilder {
    /// Create a new builder with default values
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the recording tag
    pub fn rec_tag(mut self, value: impl Into<String>) -> Self {
        self.rec_tag = Some(value.into());
        self
    }

    /// Set the group trail (accepts single string, Vec<String>, array, etc.)
    /// Single string will be converted to a single-element vector
    pub fn group<T>(mut self, value: T) -> Self
    where
        T: IntoVec<String>,
    {
        self.group = Some(value.into_vec());
        self
    }

    /// Set the last group in the trail (most specific group)
    /// This is a convenience method for tests where you only care about the final group,
    /// not the full hierarchy. The group trail will be set to a single-element vector.
    pub fn last_group(mut self, group_name: impl Into<String>) -> Self {
        self.group = Some(vec![group_name.into()]);
        self
    }

    /// Set the performer name
    pub fn performer(mut self, value: impl Into<String>) -> Self {
        self.performer = Some(value.into());
        self
    }

    /// Set the arrangement style
    pub fn arrangement(mut self, value: impl Into<String>) -> Self {
        self.arrangement = Some(value.into());
        self
    }

    /// Set the section
    pub fn section(mut self, value: impl Into<String>) -> Self {
        self.section = Some(value.into());
        self
    }

    /// Set the layers
    pub fn layers(mut self, value: impl Into<String>) -> Self {
        self.layers = Some(value.into());
        self
    }

    /// Set multi-mic positions (accepts single string, Vec<String>, array, etc.)
    pub fn multi_mic<T>(mut self, value: T) -> Self
    where
        T: IntoVec<String>,
    {
        self.multi_mic = Some(value.into_vec());
        self
    }

    /// Set effect indicators (accepts single string, Vec<String>, array, etc.)
    pub fn effect<T>(mut self, value: T) -> Self
    where
        T: IntoVec<String>,
    {
        self.effect = Some(value.into_vec());
        self
    }

    /// Set the increment number
    pub fn increment(mut self, value: impl Into<String>) -> Self {
        self.increment = Some(value.into());
        self
    }

    /// Set the channel
    pub fn channel(mut self, value: impl Into<String>) -> Self {
        self.channel = Some(value.into());
        self
    }

    /// Set the playlist identifier
    pub fn playlist(mut self, value: impl Into<String>) -> Self {
        self.playlist = Some(value.into());
        self
    }

    /// Set the track type
    pub fn track_type(mut self, value: impl Into<String>) -> Self {
        self.track_type = Some(value.into());
        self
    }

    /// Set tagged collection names (accepts single string, Vec<String>, array, etc.)
    /// An item can match multiple tagged collections
    pub fn tagged_collection<T>(mut self, value: T) -> Self
    where
        T: IntoVec<String>,
    {
        self.tagged_collection = Some(value.into_vec());
        self
    }

    /// Set unparsed words (accepts single string, Vec<String>, array, etc.)
    pub fn unparsed_words<T>(mut self, value: T) -> Self
    where
        T: IntoVec<String>,
    {
        self.unparsed_words = Some(value.into_vec());
        self
    }

    /// Set the original name
    pub fn original_name(mut self, value: impl Into<String>) -> Self {
        self.original_name = Some(value.into());
        self
    }

    /// Set the file extension
    pub fn file_extension(mut self, value: impl Into<String>) -> Self {
        self.file_extension = Some(value.into());
        self
    }

    /// Build the ItemMetadata
    pub fn build(self) -> ItemMetadata {
        ItemMetadata {
            rec_tag: self.rec_tag,
            group: self.group,
            performer: self.performer,
            arrangement: self.arrangement,
            section: self.section,
            layers: self.layers,
            multi_mic: self.multi_mic,
            effect: self.effect,
            increment: self.increment,
            channel: self.channel,
            playlist: self.playlist,
            track_type: self.track_type,
            tagged_collection: self.tagged_collection,
            unparsed_words: self.unparsed_words,
            original_name: self.original_name,
            file_extension: self.file_extension,
        }
    }
}

// Re-export IntoVec from monarchy for convenience
pub use monarchy::IntoVec;

/// Prelude module that exports everything needed to work with ItemMetadata groups
/// 
/// Import this to get all types and the extension trait in scope:
/// ```ignore
/// use dynamic_template::item_metadata::prelude::*;
/// ```
pub mod prelude {
    pub use super::ItemMetadata;
    pub use super::ItemMetadataGroup;
    pub use super::ItemMetadataBuilder;
    pub use monarchy::{Group, GroupBuilder};
    // The extension trait is generated by the Metadata derive macro
    // Bringing it into scope makes methods like .multi_mic() available
    pub use super::ItemMetadataGroupExt;
}

impl ToDisplayName for ItemMetadata {
    /// Generate a full canonical display name from metadata
    /// 
    /// Build order: group → performer → section → arrangement → layers → increment → channel → multi_mic → track_type
    /// 
    /// If we have meaningful metadata (beyond just group name), use that.
    /// If we only have group name and no other metadata, return empty string to trigger
    /// fallback to item.original in derive_display_name().
    /// 
    /// "Meaningful metadata" means metadata that describes WHAT the item is, not just
    /// version/layer information. Increment alone (".2") or playlist alone are not
    /// considered meaningful - they only make sense when combined with identifying info.
    /// 
    /// The prefixes and group_names from matched_groups provide context that can be used
    /// to strip redundant information in a later cleanup pass.
    /// 
    /// # Examples
    /// - Kick with multi_mic=["In"] → "Kick In"
    /// - Electric Guitar with performer="Ed", arrangement="Crunch" → "Ed Crunch"
    /// - Snare with increment="1" → "Snare 1" (but only if has mic info too)
    /// - DX7 .2_03.wav → "" (triggers fallback to original)
    fn to_display_name(&self, _prefixes: &[String], group_names: &[String]) -> String {
        let mut parts = Vec::new();
        
        // Track if we have any PRIMARY metadata (identifies what the item is)
        // vs SECONDARY metadata (version/layer info that only makes sense with primary)
        let mut has_primary_metadata = false;
        
        // PRIMARY metadata fields - these identify WHAT the item is
        
        // Add performer (e.g., "Ed", "Johny")
        if let Some(ref performer) = self.performer {
            parts.push(performer.clone());
            has_primary_metadata = true;
        }
        
        // Add section (e.g., "Verse", "Chorus")
        if let Some(ref section) = self.section {
            parts.push(section.clone());
            has_primary_metadata = true;
        }
        
        // Add arrangement (e.g., "Crunch", "Clean", "Lead")
        if let Some(ref arrangement) = self.arrangement {
            parts.push(arrangement.clone());
            has_primary_metadata = true;
        }
        
        // Add multi-mic positions (e.g., "In", "Out", "Top", "Bottom")
        // This is primary because it identifies mic placement
        if let Some(ref multi_mic) = self.multi_mic {
            for mic in multi_mic {
                parts.push(mic.clone());
                has_primary_metadata = true;
            }
        }
        
        // Add track type (e.g., "BUS", "SUM", "MIDI", "DI")
        // This is primary because it identifies the track's role
        if let Some(ref track_type) = self.track_type {
            parts.push(track_type.clone());
            has_primary_metadata = true;
        }
        
        // SECONDARY metadata fields - only include if we have primary metadata
        // These provide additional context but don't identify the item alone
        
        if has_primary_metadata {
            // Add layers (e.g., "DBL", "OCT", "L", "R")
            if let Some(ref layers) = self.layers {
                parts.push(layers.clone());
            }
            
            // Add increment (e.g., "1", "2" for Tom 1, Tom 2)
            if let Some(ref increment) = self.increment {
                parts.push(increment.clone());
            }
            
            // Add channel (e.g., "L", "R", "C")
            if let Some(ref channel) = self.channel {
                parts.push(channel.clone());
            }
        }
        
        // If we have meaningful (primary) metadata, prepend the group name
        if has_primary_metadata {
            if let Some(last_group) = group_names.last() {
                parts.insert(0, last_group.clone());
            }
            return parts.join(" ");
        }
        
        // No primary metadata - return empty string to trigger fallback to item.original
        // This allows tracks like "DX7", "Mellotron" to keep their original names
        // rather than being reduced to just the group name or version number
        String::new()
    }
}

