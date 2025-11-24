//! Track structure and hierarchy definitions for sorting groups
//!
//! Defines how tracks are organized within groups, including parent-child relationships,
//! entry types, and grouping strategies.
//!
//! Key concepts:
//! - **Track Structure**: Defines the hierarchy (parent-child relationships)
//! - **Entry Types**: Different track configurations (Stereo, Stereo+DI, Doubled, etc.)
//! - **Grouping Strategy**: How entries are organized (by Performer, Arrangement, etc.)
//! - **Filter Rules**: What tracks to include/exclude

use super::track_scope::TrackIdentifier;
use std::fmt;

pub mod discovery;
pub mod examples;
pub mod builder;

#[cfg(test)]
mod tests;

/// Represents a track entry within a group
/// An entry is a collection of related tracks that form a logical unit
#[derive(Debug, Clone)]
pub struct TrackEntry {
    /// Entry identifier (e.g., performer name, arrangement descriptor, or track name)
    pub id: String,
    
    /// Display name for this entry
    pub name: String,
    
    /// Type of entry (determines what tracks are included)
    pub entry_type: EntryType,
    
    /// Tracks that belong to this entry
    pub tracks: Vec<TrackNode>,
    
    /// Metadata about this entry
    pub metadata: EntryMetadata,
}

/// Metadata about a track entry
#[derive(Debug, Clone, Default)]
pub struct EntryMetadata {
    /// Performer name (if applicable)
    pub performer: Option<String>,
    
    /// Arrangement descriptor (if applicable)
    pub arrangement: Option<String>,
    
    /// Custom tags or labels
    pub tags: Vec<String>,
}

/// Type of entry - defines the track configuration pattern
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EntryType {
    /// Single mono track
    Mono,
    
    /// Single stereo track
    Stereo,
    
    /// Stereo track with a DI track
    StereoWithDi,
    
    /// Two doubled tracks (stereo pair)
    Doubled,
    
    /// Two doubled tracks + DI track
    DoubledWithDi,
    
    /// Two doubled tracks + DI + no FX track
    DoubledWithDiAndNoFx,
    
    /// Custom entry type (for flexible structures)
    Custom {
        /// Description of the custom pattern
        description: String,
        /// Expected track count
        track_count: usize,
    },
}

/// Represents a track node in the hierarchy
#[derive(Debug, Clone)]
pub struct TrackNode {
    /// Track identifier
    pub track: TrackIdentifier,
    
    /// Track name
    pub name: String,
    
    /// Track role/type (e.g., "In", "Out", "Trig", "DI", "No FX")
    pub role: TrackRole,
    
    /// Parent track (if this is a child track)
    pub parent: Option<TrackIdentifier>,
    
    /// Child tracks (if this is a parent track)
    pub children: Vec<TrackNode>,
    
    /// Track type category (for filtering)
    pub category: TrackCategory,
}

/// Role of a track within the structure
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TrackRole {
    /// Bus/Folder track
    Bus,
    
    /// Sum/Group track
    Sum,
    
    /// Input track
    Input,
    
    /// Output track
    Output,
    
    /// Trigger track
    Trigger,
    
    /// DI (Direct Input) track
    DirectInput,
    
    /// No FX track
    NoFx,
    
    /// Ambient track
    Ambient,
    
    /// Sub track
    Sub,
    
    /// Custom role
    Custom(String),
}

/// Category for filtering tracks
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TrackCategory {
    /// Audio track
    Audio,
    
    /// MIDI track
    Midi,
    
    /// Bus/Group track
    Bus,
    
    /// Folder track
    Folder,
    
    /// Other
    Other,
}

/// Grouping strategy for organizing entries within a group
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GroupingStrategy {
    /// Organize by performer name
    ByPerformer,
    
    /// Organize by arrangement descriptor
    ByArrangement,
    
    /// Organize by track type/version
    ByTrackType,
    
    /// Flat structure (no grouping)
    Flat,
    
    /// Custom strategy
    Custom(String),
}

/// Track structure template for a group
/// Defines the expected hierarchy and patterns for a group
#[derive(Debug, Clone)]
pub struct TrackStructure {
    /// Root/top-level track (e.g., "KICK BUS")
    pub root: TrackNode,
    
    /// Expected entry types for this group
    pub entry_types: Vec<EntryType>,
    
    /// Grouping strategy for entries
    pub grouping_strategy: GroupingStrategy,
    
    /// Filter rules for what tracks to include
    pub filter_rules: Vec<FilterRule>,
}

/// Filter rule for selecting tracks
#[derive(Debug, Clone)]
pub struct FilterRule {
    /// What to filter by
    pub filter_type: FilterType,
    
    /// Whether to include or exclude matching tracks
    pub include: bool,
}

/// Type of filter
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FilterType {
    /// Filter by track category
    ByCategory(TrackCategory),
    
    /// Filter by track role
    ByRole(TrackRole),
    
    /// Filter by track name pattern
    ByNamePattern(String),
    
    /// Filter audio tracks only
    AudioOnly,
    
    /// Filter MIDI tracks only
    MidiOnly,
}

impl TrackNode {
    /// Create a new track node
    pub fn new(track: TrackIdentifier, name: String, role: TrackRole, category: TrackCategory) -> Self {
        Self {
            track,
            name,
            role,
            parent: None,
            children: Vec::new(),
            category,
        }
    }
    
    /// Add a child track node
    pub fn add_child(&mut self, child: TrackNode) {
        self.children.push(child);
    }
    
    /// Check if this node has children
    pub fn has_children(&self) -> bool {
        !self.children.is_empty()
    }
    
    /// Get all descendant nodes (recursive)
    pub fn all_descendants(&self) -> Vec<&TrackNode> {
        let mut descendants = Vec::new();
        for child in &self.children {
            descendants.push(child);
            descendants.extend(child.all_descendants());
        }
        descendants
    }
    
    /// Format the node tree with indentation
    pub fn format_tree(&self, indent: usize) -> String {
        let mut result = String::new();
        let prefix = "  ".repeat(indent);
        result.push_str(&format!("{}{} ({})\n", prefix, self.name, self.role));
        
        for child in &self.children {
            result.push_str(&child.format_tree(indent + 1));
        }
        
        result
    }
}

impl fmt::Display for TrackNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.format_tree(0).trim_end())
    }
}

impl fmt::Display for TrackStructure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Track Structure:")?;
        writeln!(f, "  Entry Types: {:?}", self.entry_types)?;
        writeln!(f, "  Grouping Strategy: {:?}", self.grouping_strategy)?;
        writeln!(f, "  Hierarchy:")?;
        write!(f, "{}", self.root.format_tree(2))
    }
}

impl fmt::Display for TrackRole {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TrackRole::Bus => write!(f, "Bus"),
            TrackRole::Sum => write!(f, "Sum"),
            TrackRole::Input => write!(f, "Input"),
            TrackRole::Output => write!(f, "Output"),
            TrackRole::Trigger => write!(f, "Trigger"),
            TrackRole::DirectInput => write!(f, "DI"),
            TrackRole::NoFx => write!(f, "No FX"),
            TrackRole::Ambient => write!(f, "Ambient"),
            TrackRole::Sub => write!(f, "Sub"),
            TrackRole::Custom(s) => write!(f, "{}", s),
        }
    }
}

impl fmt::Display for TrackCategory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TrackCategory::Audio => write!(f, "Audio"),
            TrackCategory::Midi => write!(f, "MIDI"),
            TrackCategory::Bus => write!(f, "Bus"),
            TrackCategory::Folder => write!(f, "Folder"),
            TrackCategory::Other => write!(f, "Other"),
        }
    }
}

impl fmt::Display for EntryType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.description())
    }
}

impl fmt::Display for TrackEntry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Entry: {} ({}) - {} tracks", self.name, self.entry_type, self.tracks.len())
    }
}

impl fmt::Display for GroupingStrategy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GroupingStrategy::ByPerformer => write!(f, "By Performer"),
            GroupingStrategy::ByArrangement => write!(f, "By Arrangement"),
            GroupingStrategy::ByTrackType => write!(f, "By Track Type"),
            GroupingStrategy::Flat => write!(f, "Flat"),
            GroupingStrategy::Custom(s) => write!(f, "{}", s),
        }
    }
}

impl EntryType {
    /// Get expected track count for this entry type
    pub fn track_count(&self) -> usize {
        match self {
            EntryType::Mono => 1,
            EntryType::Stereo => 1,
            EntryType::StereoWithDi => 2,
            EntryType::Doubled => 2,
            EntryType::DoubledWithDi => 3,
            EntryType::DoubledWithDiAndNoFx => 4,
            EntryType::Custom { track_count, .. } => *track_count,
        }
    }
    
    /// Get description of this entry type
    pub fn description(&self) -> &str {
        match self {
            EntryType::Mono => "Single mono track",
            EntryType::Stereo => "Single stereo track",
            EntryType::StereoWithDi => "Stereo track with DI",
            EntryType::Doubled => "Two doubled tracks",
            EntryType::DoubledWithDi => "Two doubled tracks with DI",
            EntryType::DoubledWithDiAndNoFx => "Two doubled tracks with DI and no FX",
            EntryType::Custom { description, .. } => description,
        }
    }
}

/// Filter tracks based on filter rules
pub fn filter_tracks<'a>(nodes: &'a [TrackNode], rules: &[FilterRule]) -> Vec<&'a TrackNode> {
    let mut filtered: Vec<&TrackNode> = nodes.iter().collect();
    
    for rule in rules {
        filtered = filtered
            .into_iter()
            .filter(|node| {
                let matches = match &rule.filter_type {
                    FilterType::ByCategory(cat) => node.category == *cat,
                    FilterType::ByRole(role) => node.role == *role,
                    FilterType::ByNamePattern(pattern) => {
                        node.name.to_lowercase().contains(&pattern.to_lowercase())
                    }
                    FilterType::AudioOnly => node.category == TrackCategory::Audio,
                    FilterType::MidiOnly => node.category == TrackCategory::Midi,
                };
                
                if rule.include {
                    matches
                } else {
                    !matches
                }
            })
            .collect();
    }
    
    filtered
}

