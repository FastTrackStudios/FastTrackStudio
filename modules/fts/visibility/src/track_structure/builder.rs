//! Build track structures from flat lists of tracks
//!
//! This module provides utilities to build TrackNode hierarchies from
//! a flat list of tracks with parent-child relationships, which is what
//! we get from REAPER.

use super::{
    TrackNode, TrackRole, TrackCategory,
    TrackStructure, GroupingStrategy,
};
use crate::track_scope::TrackIdentifier;

/// A flat track representation from REAPER
/// This is what we get from the DAW - just track info with parent reference
#[derive(Debug, Clone)]
pub struct FlatTrack {
    /// Track identifier
    pub identifier: TrackIdentifier,

    /// Track name
    pub name: String,

    /// Parent track identifier (None if root)
    pub parent: Option<TrackIdentifier>,

    /// Track depth (0 = top level, 1 = nested, etc.)
    pub depth: usize,

    /// Whether this is a folder/bus track
    pub is_folder: bool,

    /// Whether this is MIDI
    pub is_midi: bool,

    /// Whether this is audio
    pub is_audio: bool,
}

impl FlatTrack {
    /// Create a new flat track
    pub fn new(
        identifier: TrackIdentifier,
        name: String,
        parent: Option<TrackIdentifier>,
        depth: usize,
    ) -> Self {
        // Infer category from name for now
        let is_folder = name.to_lowercase().contains("bus")
            || name.to_lowercase().contains("sum")
            || name.to_lowercase().contains("folder");

        Self {
            identifier,
            name,
            parent,
            depth,
            is_folder,
            is_midi: false, // Would need to check from REAPER
            is_audio: !is_folder, // Assume audio if not folder
        }
    }

    /// Create a root track (no parent)
    pub fn root(identifier: TrackIdentifier, name: String) -> Self {
        Self::new(identifier, name, None, 0)
    }

    /// Create a child track
    pub fn child(
        identifier: TrackIdentifier,
        name: String,
        parent: TrackIdentifier,
        depth: usize,
    ) -> Self {
        Self::new(identifier, name, Some(parent), depth)
    }
}

/// Build a track hierarchy from a flat list of tracks
///
/// Takes a list of tracks with parent references and builds a tree structure.
/// The root track should have `parent: None`.
pub fn build_from_flat_list(tracks: Vec<FlatTrack>) -> Option<TrackNode> {
    if tracks.is_empty() {
        return None;
    }

    // Find root track (one with no parent or lowest depth)
    let root = tracks
        .iter()
        .find(|t| t.parent.is_none())
        .or_else(|| tracks.iter().min_by_key(|t| t.depth))?;

    // Build a map of tracks by identifier for quick lookup
    let mut track_map: std::collections::HashMap<&TrackIdentifier, &FlatTrack> =
        tracks.iter().map(|t| (&t.identifier, t)).collect();

    // Build a map of children by parent
    let mut children_map: std::collections::HashMap<&TrackIdentifier, Vec<&FlatTrack>> =
        std::collections::HashMap::new();

    for track in &tracks {
        if let Some(parent_id) = &track.parent {
            children_map
                .entry(parent_id)
                .or_insert_with(Vec::new)
                .push(track);
        }
    }

    // Build the tree recursively
    build_node_recursive(root, &children_map)
}

/// Recursively build a node and its children
fn build_node_recursive(
    track: &FlatTrack,
    children_map: &std::collections::HashMap<&TrackIdentifier, Vec<&FlatTrack>>,
) -> Option<TrackNode> {
    let role = infer_role(&track.name);
    let category = infer_category(track);

    let mut node = TrackNode::new(
        track.identifier.clone(),
        track.name.clone(),
        role,
        category,
    );

    node.parent = track.parent.clone();

    // Add children
    if let Some(children) = children_map.get(&track.identifier) {
        // Sort children by depth to maintain order
        let mut sorted_children: Vec<&FlatTrack> = children.to_vec();
        sorted_children.sort_by_key(|c| c.depth);

        for child_track in sorted_children {
            if let Some(child_node) = build_node_recursive(child_track, children_map) {
                node.add_child(child_node);
            }
        }
    }

    Some(node)
}

/// Build a TrackStructure from a flat list, assuming the first track is the root
pub fn build_structure_from_flat_list(
    tracks: Vec<FlatTrack>,
    entry_types: Vec<super::EntryType>,
    grouping_strategy: GroupingStrategy,
) -> Option<TrackStructure> {
    let root = build_from_flat_list(tracks)?;

    Some(TrackStructure {
        root,
        entry_types,
        grouping_strategy,
        filter_rules: Vec::new(),
    })
}

/// Infer track role from name
fn infer_role(name: &str) -> TrackRole {
    let name_lower = name.to_lowercase();

    if name_lower.contains("bus") || name_lower.contains("folder") {
        TrackRole::Bus
    } else if name_lower.contains("sum") {
        TrackRole::Sum
    } else if name_lower == "in" || name_lower.contains(" in") || name_lower.ends_with(" in") {
        TrackRole::Input
    } else if name_lower == "out" || name_lower.contains(" out") || name_lower.ends_with(" out") {
        TrackRole::Output
    } else if name_lower.contains("trig") || name_lower.contains("trigger") {
        TrackRole::Trigger
    } else if name_lower.contains("di") || name_lower.contains("direct") {
        TrackRole::DirectInput
    } else if name_lower.contains("no fx") || name_lower.contains("nofx") {
        TrackRole::NoFx
    } else if name_lower.contains("ambient") {
        TrackRole::Ambient
    } else if name_lower.contains("sub") {
        TrackRole::Sub
    } else {
        TrackRole::Custom(name.to_string())
    }
}

/// Infer track category from track info
fn infer_category(track: &FlatTrack) -> TrackCategory {
    if track.is_midi {
        TrackCategory::Midi
    } else if track.is_folder {
        TrackCategory::Folder
    } else if track.is_audio {
        TrackCategory::Audio
    } else {
        TrackCategory::Other
    }
}
