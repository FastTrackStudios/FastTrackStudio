//! ItemProperties formatter
//!
//! Generic formatter that works with ItemProperties to format track names.
//! This replaces group-specific formatters since all groups now use ItemProperties.

use crate::smart_template::naming::item_properties::ItemProperties;
use crate::smart_template::naming::formatter::Formatter;

/// Generic formatter for ItemProperties
/// 
/// Formats track names based on the properties themselves, following the FTS naming convention.
/// The format order is:
/// 1. Group prefix or sub-type (group name)
/// 2. Sub-type (if different from group)
/// 3. Multi-mic descriptors
/// 4. Arrangement
/// 5. Layers
/// 6. Increment
/// 7. Channel
/// 8. Playlist
pub struct ItemPropertiesFormatter {
    /// Whether to use group prefix or sub-type for the base name
    prefer_sub_type: bool,
}

impl ItemPropertiesFormatter {
    /// Create a new formatter
    pub fn new() -> Self {
        Self {
            prefer_sub_type: true,
        }
    }
    
    /// Create a formatter that prefers group prefix over sub-type
    pub fn prefer_prefix() -> Self {
        Self {
            prefer_sub_type: false,
        }
    }
    
    /// Format ItemProperties into a track name string
    pub fn format_properties(&self, props: &ItemProperties) -> String {
        let mut parts = Vec::new();
        
        // 1. Determine base name (group name)
        // Use sub-type if available and prefer_sub_type is true, otherwise use group_prefix
        let base_name = if self.prefer_sub_type {
            if let Some(sub_types) = &props.sub_type {
                if let Some(first) = sub_types.first() {
                    Some(first.clone())
                } else {
                    None
                }
            } else {
                None
            }
        } else {
            None
        }.or_else(|| {
            // Fallback to group prefix
            props.group_prefix.clone()
        });
        
        if let Some(base) = base_name {
            parts.push(base);
        }
        
        // 2. Add additional sub-types (if multiple)
        if let Some(sub_types) = &props.sub_type {
            if sub_types.len() > 1 {
                for sub_type in sub_types.iter().skip(1) {
                    parts.push(sub_type.clone());
                }
            }
        }
        
        // 3. Add multi-mic descriptors
        if let Some(multi_mics) = &props.multi_mic {
            for multi_mic in multi_mics {
                parts.push(multi_mic.clone());
            }
        }
        
        // 4. Add arrangement
        if let Some(arrangement) = &props.arrangement {
            parts.push(arrangement.clone());
        }
        
        // 5. Add layers
        if let Some(layers) = &props.layers {
            parts.push(layers.clone());
        }
        
        // 6. Add increment
        if let Some(increment) = &props.increment {
            parts.push(increment.clone());
        }
        
        // 7. Add channel
        if let Some(channel) = &props.channel {
            parts.push(channel.clone());
        }
        
        // 8. Add playlist
        if let Some(playlist) = &props.playlist {
            parts.push(playlist.clone());
        }
        
        // 9. Add track type in parentheses if present
        if let Some(track_type) = &props.track_type {
            if !parts.is_empty() {
                let base = parts.join(" ");
                return format!("{} ({})", base, track_type);
            }
        }
        
        if parts.is_empty() {
            // Fallback to original name or a default
            props.original_name.as_ref()
                .map(|s| s.clone())
                .unwrap_or_else(|| "Track".to_string())
        } else {
            parts.join(" ")
        }
    }
}

impl Formatter for ItemPropertiesFormatter {
    type TrackName = ItemProperties;
    
    fn format(&self, track_name: &Self::TrackName) -> String {
        self.format_properties(track_name)
    }
    
    fn name(&self) -> &str {
        "item_properties"
    }
}

impl Default for ItemPropertiesFormatter {
    fn default() -> Self {
        Self::new()
    }
}
