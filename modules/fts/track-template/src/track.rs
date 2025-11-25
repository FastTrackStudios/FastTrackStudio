//! Track representation
//!
//! A Track represents a single track in a DAW template, with support for:
//! - Name and metadata
//! - Parent-child relationships (folder hierarchy)
//! - Automatic parent send (tracks can auto-send to their parent, enabled by default)
//! - Takes (item lanes)
//! - Explicit send/receive relationships (for tracks without parent-child relationships)
//! - Sorting status (to track whether tracks are sorted, need confirmation, or are unsorted)

use std::collections::HashMap;
use std::fmt;

/// Status of track sorting
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SortStatus {
    /// Track is sorted and confirmed
    Sorted,
    /// Track needs confirmation - we're not 100% sure about the match
    NeedsConfirmation,
    /// Track could not be sorted - no way to know for sure where it belongs
    NotSorted,
}

impl Default for SortStatus {
    fn default() -> Self {
        Self::Sorted
    }
}

impl fmt::Display for SortStatus {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SortStatus::Sorted => write!(f, "Sorted"),
            SortStatus::NeedsConfirmation => write!(f, "Needs Confirmation"),
            SortStatus::NotSorted => write!(f, "Not Sorted"),
        }
    }
}

/// A single track in a template
#[derive(Debug, Clone, PartialEq)]
pub struct Track {
    /// The name of the track
    pub name: String,
    
    /// Optional track type (e.g., "BUS", "SUM", "AUX")
    pub track_type: Option<String>,
    
    /// Parent track name (for folder hierarchy)
    pub parent: Option<String>,
    
    /// Whether this track automatically sends to its parent track
    /// When true, audio is automatically routed to the parent track.
    /// When false, no automatic send is created (useful for tracks that
    /// need explicit routing or are not part of a folder hierarchy).
    /// Defaults to true.
    pub parent_send: bool,
    
    /// Takes (item lanes) on this track
    pub takes: Vec<Take>,
    
    /// Send relationships (tracks this track sends to)
    /// These are for explicit sends to tracks that don't have a parent-child
    /// relationship (e.g., "Snare Top" sending to "Snare Trig").
    pub sends: Vec<SendReceive>,
    
    /// Receive relationships (tracks this track receives from)
    pub receives: Vec<SendReceive>,
    
    /// Sorting status - indicates whether the track is sorted, needs confirmation, or is unsorted
    pub sort_status: SortStatus,
    
    /// Additional metadata
    pub metadata: HashMap<String, String>,
}

impl Track {
    /// Create a new track with a name
    /// Parent send is enabled by default.
    /// Sort status defaults to Sorted.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            track_type: None,
            parent: None,
            parent_send: true, // Default: tracks send to parent
            takes: Vec::new(),
            sends: Vec::new(),
            receives: Vec::new(),
            sort_status: SortStatus::Sorted,
            metadata: HashMap::new(),
        }
    }
    
    /// Create a new track with a track type
    /// Parent send is enabled by default.
    /// Sort status defaults to Sorted.
    pub fn with_type(name: impl Into<String>, track_type: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            track_type: Some(track_type.into()),
            parent: None,
            parent_send: true, // Default: tracks send to parent
            takes: Vec::new(),
            sends: Vec::new(),
            receives: Vec::new(),
            sort_status: SortStatus::Sorted,
            metadata: HashMap::new(),
        }
    }
    
    /// Set the sorting status
    pub fn set_sort_status(&mut self, status: SortStatus) {
        self.sort_status = status;
    }
    
    /// Get the sorting status
    pub fn sort_status(&self) -> SortStatus {
        self.sort_status
    }
    
    /// Set whether this track should send to its parent
    pub fn set_parent_send(&mut self, enabled: bool) {
        self.parent_send = enabled;
    }
    
    /// Get whether this track sends to its parent
    pub fn parent_send(&self) -> bool {
        self.parent_send
    }
    
    /// Set the parent track
    pub fn set_parent(&mut self, parent: impl Into<String>) {
        self.parent = Some(parent.into());
    }
    
    /// Add a take (item lane) to this track
    pub fn add_take(&mut self, take: Take) {
        self.takes.push(take);
    }
    
    /// Add a send relationship
    pub fn add_send(&mut self, send: SendReceive) {
        self.sends.push(send);
    }
    
    /// Add a receive relationship
    pub fn add_receive(&mut self, receive: SendReceive) {
        self.receives.push(receive);
    }
    
    /// Set metadata
    pub fn set_metadata(&mut self, key: impl Into<String>, value: impl Into<String>) {
        self.metadata.insert(key.into(), value.into());
    }
    
    /// Get metadata
    pub fn get_metadata(&self, key: &str) -> Option<&String> {
        self.metadata.get(key)
    }
    
    /// Format this track as a tree node with the given prefix
    /// This is used internally by Display implementations
    pub(crate) fn fmt_tree(&self, f: &mut fmt::Formatter, prefix: &str, is_last: bool) -> fmt::Result {
        // Draw the tree connector
        let connector = if is_last { "└── " } else { "├── " };
        write!(f, "{}{}", prefix, connector)?;
        
        // Track name and type
        write!(f, "{}", self.name)?;
        if let Some(track_type) = &self.track_type {
            write!(f, " ({})", track_type)?;
        }
        // Show sort status if not sorted
        match self.sort_status {
            SortStatus::NeedsConfirmation => write!(f, " [Needs Confirmation]")?,
            SortStatus::NotSorted => write!(f, " [Not Sorted]")?,
            SortStatus::Sorted => {}
        }
        
        writeln!(f)?;
        
        // Calculate prefix for children
        let child_prefix = if is_last {
            format!("{}    ", prefix)
        } else {
            format!("{}│   ", prefix)
        };
        
        // Takes
        for (i, take) in self.takes.iter().enumerate() {
            let is_last_take = i == self.takes.len() - 1 && self.sends.is_empty() && self.receives.is_empty();
            let take_connector = if is_last_take { "└── " } else { "├── " };
            write!(f, "{}{}Take: {}", child_prefix, take_connector, take.name)?;
            if let Some(index) = take.index {
                write!(f, " [{}]", index)?;
            }
            writeln!(f)?;
        }
        
        // Sends
        for (i, send) in self.sends.iter().enumerate() {
            let is_last_send = i == self.sends.len() - 1 && self.receives.is_empty();
            let send_connector = if is_last_send { "└── " } else { "├── " };
            write!(f, "{}{}Send → {}", child_prefix, send_connector, send.target_track)?;
            if let Some(level) = send.level {
                write!(f, " ({:.2})", level)?;
            }
            writeln!(f)?;
        }
        
        // Receives
        for (i, receive) in self.receives.iter().enumerate() {
            let is_last_receive = i == self.receives.len() - 1;
            let receive_connector = if is_last_receive { "└── " } else { "├── " };
            write!(f, "{}{}Receive ← {}", child_prefix, receive_connector, receive.target_track)?;
            if let Some(level) = receive.level {
                write!(f, " ({:.2})", level)?;
            }
            writeln!(f)?;
        }
        
        Ok(())
    }
}

impl fmt::Display for Track {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fmt_tree(f, "", true)
    }
}

/// A take (item lane) on a track
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Take {
    /// The name of the take
    pub name: String,
    
    /// Optional take index (for ordering)
    pub index: Option<usize>,
    
    /// Additional metadata
    pub metadata: HashMap<String, String>,
}

impl Take {
    /// Create a new take with a name
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            index: None,
            metadata: HashMap::new(),
        }
    }
    
    /// Create a new take with an index
    pub fn with_index(name: impl Into<String>, index: usize) -> Self {
        Self {
            name: name.into(),
            index: Some(index),
            metadata: HashMap::new(),
        }
    }
    
    /// Set metadata
    pub fn set_metadata(&mut self, key: impl Into<String>, value: impl Into<String>) {
        self.metadata.insert(key.into(), value.into());
    }
}

/// A send or receive relationship between tracks
#[derive(Debug, Clone, PartialEq)]
pub struct SendReceive {
    /// The target track name
    pub target_track: String,
    
    /// Optional send level (0.0 to 1.0, or dB value)
    pub level: Option<f64>,
    
    /// Optional send type (e.g., "pre-fader", "post-fader")
    pub send_type: Option<String>,
    
    /// Additional metadata
    pub metadata: HashMap<String, String>,
}

impl SendReceive {
    /// Create a new send/receive relationship
    pub fn new(target_track: impl Into<String>) -> Self {
        Self {
            target_track: target_track.into(),
            level: None,
            send_type: None,
            metadata: HashMap::new(),
        }
    }
    
    /// Create a new send/receive with a level
    pub fn with_level(target_track: impl Into<String>, level: f64) -> Self {
        Self {
            target_track: target_track.into(),
            level: Some(level),
            send_type: None,
            metadata: HashMap::new(),
        }
    }
    
    /// Set the send type
    pub fn set_send_type(&mut self, send_type: impl Into<String>) {
        self.send_type = Some(send_type.into());
    }
    
    /// Set metadata
    pub fn set_metadata(&mut self, key: impl Into<String>, value: impl Into<String>) {
        self.metadata.insert(key.into(), value.into());
    }
}

