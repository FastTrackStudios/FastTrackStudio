//! Formatting for TrackName according to FTS naming convention
//! 
//! This module handles converting TrackName structs back to strings
//! following the naming convention order and rules.

use super::TrackName;
use super::component_order::{ComponentOrder, ComponentOrderType};

/// Format a TrackName into a string according to the FTS naming convention.
/// 
/// The formatting follows the standard order:
/// RecTag → GroupPrefix → SubType → Performer → Arrangement → Section → 
/// Layers → MultiMic → Effect → TrackType → Increment → Channel → Playlist
/// 
/// # Arguments
/// 
/// * `track_name` - The TrackName to format
/// * `order` - Optional component order (uses default if None)
pub fn format_track_name(track_name: &TrackName, order: Option<&ComponentOrder>) -> String {
    let default_order = ComponentOrder::default();
    let order = order.unwrap_or(&default_order);
    let mut parts = Vec::new();

    // Add each component in the correct order
    for component_type in &order.order {
        match component_type {
            ComponentOrderType::RecTag => {
                if let Some(rec_tag) = &track_name.rec_tag {
                    parts.push(rec_tag.clone());
                }
            }
            ComponentOrderType::GroupPrefix => {
                if let Some(group_prefix) = &track_name.group_prefix {
                    // Group prefix may contain multiple prefixes (e.g., "B D K" for Band -> Drums -> Kick)
                    // Split and add each prefix separately
                    let prefixes: Vec<&str> = group_prefix.split_whitespace().collect();
                    for prefix in prefixes {
                        parts.push(prefix.to_string());
                    }
                }
            }
            ComponentOrderType::SubType => {
                if let Some(sub_types) = &track_name.sub_type {
                    // Sub-types are the nested group names (e.g., ["Kick"] or ["Drums", "Kick"])
                    // We only want the last one (the most specific child) - this preserves original case
                    if let Some(last_sub_type) = sub_types.last() {
                        parts.push(last_sub_type.clone());
                    }
                }
            }
            ComponentOrderType::Performer => {
                if let Some(performer) = &track_name.performer {
                    parts.push(performer.clone());
                }
            }
            ComponentOrderType::Arrangement => {
                // Only add arrangement if it doesn't match any sub-type
                if let Some(arrangement) = &track_name.arrangement {
                    let should_include = if let Some(ref sub_types) = track_name.sub_type {
                        // Don't include arrangement if it matches any sub-type
                        !sub_types.iter().any(|st| st.eq_ignore_ascii_case(arrangement))
                    } else {
                        // No sub-types, so include arrangement
                        true
                    };
                    
                    if should_include {
                        parts.push(arrangement.clone());
                    }
                }
            }
            ComponentOrderType::Section => {
                if let Some(section) = &track_name.section {
                    parts.push(section.clone());
                }
            }
            ComponentOrderType::Layers => {
                if let Some(layers) = &track_name.layers {
                    parts.push(layers.clone());
                }
            }
            ComponentOrderType::MultiMic => {
                if let Some(multi_mic) = &track_name.multi_mic {
                    // Join multiple mic positions with ", "
                    parts.push(multi_mic.join(", "));
                }
            }
            ComponentOrderType::Effect => {
                if let Some(effect) = &track_name.effect {
                    // Join multiple effects with ", "
                    parts.push(effect.join(", "));
                }
            }
            ComponentOrderType::Increment => {
                if let Some(increment) = &track_name.increment {
                    parts.push(increment.clone());
                }
            }
            ComponentOrderType::Channel => {
                if let Some(channel) = &track_name.channel {
                    parts.push(channel.clone());
                }
            }
            ComponentOrderType::Playlist => {
                if let Some(playlist) = &track_name.playlist {
                    parts.push(playlist.clone());
                }
            }
            ComponentOrderType::TrackType => {
                if let Some(track_type) = &track_name.track_type {
                    // Track type is formatted in parentheses
                    parts.push(format!("({})", track_type));
                }
            }
        }
    }
    
    // Add unparsed words if present (these should be included in output)
    if let Some(unparsed_words) = &track_name.unparsed_words {
        for word in unparsed_words {
            parts.push(word.clone());
        }
    }

    parts.join(" ")
}

/// Format a TrackName using the default component order
pub fn format_track_name_default(track_name: &TrackName) -> String {
    format_track_name(track_name, None)
}

