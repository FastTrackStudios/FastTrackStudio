//! Track scope identification and management

use std::fmt;

/// Identifies a track in a DAW-agnostic way
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TrackIdentifier {
    /// Global Unique Identifier (most reliable)
    Guid(String),
    
    /// Track name (fallback, less reliable)
    Name(String),
    
    /// Index-based (temporary, not persistent)
    Index(usize),
}

impl TrackIdentifier {
    pub fn guid(guid: impl Into<String>) -> Self {
        Self::Guid(guid.into())
    }
    
    pub fn name(name: impl Into<String>) -> Self {
        Self::Name(name.into())
    }
    
    pub fn index(index: usize) -> Self {
        Self::Index(index)
    }
    
    /// Get GUID if available
    pub fn as_guid(&self) -> Option<&str> {
        match self {
            Self::Guid(g) => Some(g),
            _ => None,
        }
    }
    
    /// Get name if available
    pub fn as_name(&self) -> Option<&str> {
        match self {
            Self::Name(n) => Some(n),
            _ => None,
        }
    }
}

impl fmt::Display for TrackIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Guid(g) => write!(f, "GUID:{}", g),
            Self::Name(n) => write!(f, "Name:{}", n),
            Self::Index(i) => write!(f, "Index:{}", i),
        }
    }
}

/// Defines the scope of tracks included in a group
#[derive(Debug, Clone)]
pub struct TrackScope {
    /// Primary parent track (if any)
    pub parent_track: Option<TrackIdentifier>,
    
    /// Additional tracks explicitly included in scope
    pub additional_tracks: Vec<TrackIdentifier>,
    
    /// Whether to include all children of parent track
    pub include_children: bool,
    
    /// Whether to recursively include nested children
    pub recursive_children: bool,
}

impl TrackScope {
    pub fn new() -> Self {
        Self {
            parent_track: None,
            additional_tracks: Vec::new(),
            include_children: true,
            recursive_children: true,
        }
    }
    
    pub fn with_parent(parent: TrackIdentifier) -> Self {
        Self {
            parent_track: Some(parent),
            additional_tracks: Vec::new(),
            include_children: true,
            recursive_children: true,
        }
    }
    
    pub fn add_track(&mut self, track: TrackIdentifier) {
        self.additional_tracks.push(track);
    }
    
    pub fn is_empty(&self) -> bool {
        self.parent_track.is_none() && self.additional_tracks.is_empty()
    }
}

impl Default for TrackScope {
    fn default() -> Self {
        Self::new()
    }
}

