//! Panel registry — all known panel types in the application.
//!
//! Each variant maps to a specific UI component that the dock renderer
//! will display. Adding a new panel = adding a variant here + adding a
//! match arm in the app's panel renderer callback.

use serde::{Deserialize, Serialize};

/// All known panel types in the application.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PanelId {
    // Session panels
    Performance,
    ChartEditor,
    ChartPreview,
    Setlist,
    Settings,

    // Signal/Rig panels
    RigGrid,
    RigNodeGraph,
    PresetBrowser,
    ProfileBrowser,
    SongParts,
    SongSelector,

    // DAW panels
    Transport,
    Mixer,

    // Utility panels
    Navigator,
    Inspector,
}

impl PanelId {
    /// Human-readable display name for the panel.
    pub const fn display_name(self) -> &'static str {
        match self {
            Self::Performance => "Performance",
            Self::ChartEditor => "Chart Editor",
            Self::ChartPreview => "Chart Preview",
            Self::Setlist => "Setlist",
            Self::Settings => "Settings",
            Self::RigGrid => "Rig Grid",
            Self::RigNodeGraph => "Rig Node Graph",
            Self::PresetBrowser => "Preset Browser",
            Self::ProfileBrowser => "Profile Browser",
            Self::SongParts => "Song Parts",
            Self::SongSelector => "Song Selector",
            Self::Transport => "Transport",
            Self::Mixer => "Mixer",
            Self::Navigator => "Navigator",
            Self::Inspector => "Inspector",
        }
    }

    /// Icon identifier (for lucide-dioxus or similar).
    pub const fn icon_name(self) -> &'static str {
        match self {
            Self::Performance => "play",
            Self::ChartEditor => "music",
            Self::ChartPreview => "music-2",
            Self::Setlist => "list-music",
            Self::Settings => "settings",
            Self::RigGrid => "grid-3x3",
            Self::RigNodeGraph => "workflow",
            Self::PresetBrowser => "folder-open",
            Self::ProfileBrowser => "user",
            Self::SongParts => "layers",
            Self::SongSelector => "list-music",
            Self::Transport => "disc",
            Self::Mixer => "sliders-horizontal",
            Self::Navigator => "compass",
            Self::Inspector => "search",
        }
    }

    /// Get all panels in display order.
    pub fn all() -> &'static [PanelId] {
        &[
            Self::Performance,
            Self::ChartEditor,
            Self::ChartPreview,
            Self::Setlist,
            Self::RigGrid,
            Self::RigNodeGraph,
            Self::PresetBrowser,
            Self::ProfileBrowser,
            Self::SongParts,
            Self::SongSelector,
            Self::Transport,
            Self::Mixer,
            Self::Navigator,
            Self::Inspector,
            Self::Settings,
        ]
    }
}
