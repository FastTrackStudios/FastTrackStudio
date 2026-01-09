/// Folder state for TCP (Track Control Panel)
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum TcpFolderState {
    /// Normal folder state
    Normal,
    /// Small folder state
    Small,
    /// Collapsed folder state
    Collapsed,
}

/// Folder state for MCP (Mixer Control Panel)
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum McpFolderState {
    /// Normal folder state
    Normal,
    /// Collapsed folder state
    Collapsed,
}

/// Folder depth change - relative change in folder depth from the previous track
///
/// This represents the `folder_depth_change()` value from REAPER:
/// - `0` = normal track (no change)
/// - `1` = track is a folder parent (starts a new folder)
/// - `-1` = track is the last in the innermost folder (closes one level)
/// - `-2` = track is the last in the innermost and next-innermost folders (closes two levels)
/// - etc.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize,
)]
pub enum FolderDepthChange {
    /// Closes multiple folder levels (e.g., -2, -3, -4, etc.)
    ClosesLevels(i32),
    /// Normal track (no change in folder depth)
    Normal,
    /// Starts a new folder (opens one level)
    FolderStart,
}

impl FolderDepthChange {
    /// Create from REAPER's folder_depth_change value
    pub fn from_reaper_value(value: i32) -> Self {
        match value {
            0 => Self::Normal,
            1 => Self::FolderStart,
            n if n < 0 => Self::ClosesLevels(n),
            _ => Self::Normal, // Unexpected positive values > 1, treat as normal
        }
    }

    /// Convert to REAPER's folder_depth_change value
    pub fn to_reaper_value(&self) -> i32 {
        match self {
            Self::Normal => 0,
            Self::FolderStart => 1,
            Self::ClosesLevels(n) => *n,
        }
    }

    /// Check if this is a folder start
    pub fn is_folder_start(&self) -> bool {
        matches!(self, Self::FolderStart)
    }

    /// Check if this closes any folder levels
    pub fn closes_levels(&self) -> bool {
        matches!(self, Self::ClosesLevels(_))
    }

    /// Get the number of levels closed (0 if none)
    pub fn levels_closed(&self) -> i32 {
        match self {
            Self::ClosesLevels(n) => -n, // n is negative, so negate to get positive count
            _ => 0,
        }
    }
}

impl From<i32> for FolderDepthChange {
    fn from(value: i32) -> Self {
        Self::from_reaper_value(value)
    }
}

impl From<FolderDepthChange> for i32 {
    fn from(value: FolderDepthChange) -> Self {
        value.to_reaper_value()
    }
}

/// Track depth - absolute cumulative folder nesting level
///
/// This represents how many folder levels deep a track is:
/// - `0` = top-level track (not in any folder)
/// - `1` = inside one folder
/// - `2` = inside two nested folders
/// - etc.
///
/// This is calculated by summing all `FolderDepthChange` values from track 0 to the current track.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize,
)]
pub struct TrackDepth(i32);

impl TrackDepth {
    /// Create a new track depth
    pub fn new(depth: i32) -> Self {
        Self(depth.max(0)) // Ensure non-negative
    }

    /// Get the depth value
    pub fn value(&self) -> i32 {
        self.0
    }

    /// Check if this is a top-level track (depth 0)
    pub fn is_top_level(&self) -> bool {
        self.0 == 0
    }

    /// Check if this track is nested (depth > 0)
    pub fn is_nested(&self) -> bool {
        self.0 > 0
    }

    /// Get the nesting level (0-based)
    pub fn level(&self) -> usize {
        self.0.max(0) as usize
    }

    /// Calculate depth after applying a folder depth change
    pub fn apply_change(&self, change: FolderDepthChange) -> Self {
        let new_depth = self.0 + change.to_reaper_value();
        Self::new(new_depth)
    }
}

impl From<i32> for TrackDepth {
    fn from(value: i32) -> Self {
        Self::new(value)
    }
}

impl From<TrackDepth> for i32 {
    fn from(value: TrackDepth) -> Self {
        value.value()
    }
}

impl Default for TrackDepth {
    fn default() -> Self {
        Self::new(0)
    }
}

impl std::fmt::Display for TrackDepth {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
