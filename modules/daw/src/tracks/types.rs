use serde::{Serialize, Deserialize};
use std::fmt;

/// A newtype for track names to provide type safety and prevent "string-ly typed" bugs.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Default)]
pub struct TrackName(pub String);

impl From<String> for TrackName {
    fn from(s: String) -> Self {
        Self(s)
    }
}

impl From<&str> for TrackName {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl fmt::Display for TrackName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl AsRef<str> for TrackName {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl TrackName {
    /// Create a new TrackName
    pub fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }

    /// Convert to lowercase for comparison
    pub fn to_lowercase(&self) -> String {
        self.0.to_lowercase()
    }

    /// Check if the name contains a substring (case-insensitive)
    pub fn contains_ignore_case(&self, other: &str) -> bool {
        self.0.to_lowercase().contains(&other.to_lowercase())
    }

    /// Check for equality (case-insensitive)
    pub fn eq_ignore_case(&self, other: &str) -> bool {
        self.0.eq_ignore_ascii_case(other)
    }
}

/// A newtype for track GUIDs (typically REAPER's {GUID} format).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TrackGuid(pub String);

impl From<String> for TrackGuid {
    fn from(s: String) -> Self {
        Self(s)
    }
}

impl From<&str> for TrackGuid {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl fmt::Display for TrackGuid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl AsRef<str> for TrackGuid {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

/// A newtype for metadata keys to prevent typos when accessing track metadata.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct MetadataKey(pub String);

impl From<String> for MetadataKey {
    fn from(s: String) -> Self {
        Self(s)
    }
}

impl From<&str> for MetadataKey {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

impl fmt::Display for MetadataKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl AsRef<str> for MetadataKey {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

// Common metadata keys as constants to further prevent typos
impl MetadataKey {
    pub const GROUP: &'static str = "fts.group";
    pub const TRACK_TYPE: &'static str = "fts.track_type";
    pub const PARENT: &'static str = "fts.parent";
    pub const MODES: &'static str = "fts.modes";
    pub const USE_PLAYLIST: &'static str = "fts.use_playlist";
}

