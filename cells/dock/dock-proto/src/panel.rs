//! Panel registry — all known panel types in the application.
//!
//! Each variant maps to a specific UI component that the dock renderer
//! will display. Adding a new panel = adding a variant here + adding a
//! match arm in the app's panel renderer callback.

use facet::Facet;

/// All known panel types in the application.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Facet)]
#[repr(u8)]
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
    SceneGrid,

    // DAW panels
    Transport,
    Mixer,

    // Utility panels
    Navigator,
    Inspector,
}

impl std::fmt::Display for PanelId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.display_name())
    }
}

impl From<PanelId> for String {
    fn from(panel: PanelId) -> Self {
        panel.as_str().to_owned()
    }
}

impl PanelId {
    /// Stable string identifier for this panel (used as registry key).
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Performance => "performance",
            Self::ChartEditor => "chart-editor",
            Self::ChartPreview => "chart-preview",
            Self::Setlist => "setlist",
            Self::Settings => "settings",
            Self::RigGrid => "rig-grid",
            Self::RigNodeGraph => "rig-node-graph",
            Self::PresetBrowser => "preset-browser",
            Self::ProfileBrowser => "profile-browser",
            Self::SongParts => "song-parts",
            Self::SongSelector => "song-selector",
            Self::SceneGrid => "scene-grid",
            Self::Transport => "transport",
            Self::Mixer => "mixer",
            Self::Navigator => "navigator",
            Self::Inspector => "inspector",
        }
    }

    /// Look up a PanelId from its string identifier. Returns `None` for
    /// unrecognized strings (which may be dynamic panels from the registry).
    pub fn from_str_id(s: &str) -> Option<Self> {
        match s {
            "performance" => Some(Self::Performance),
            "chart-editor" => Some(Self::ChartEditor),
            "chart-preview" => Some(Self::ChartPreview),
            "setlist" => Some(Self::Setlist),
            "settings" => Some(Self::Settings),
            "rig-grid" => Some(Self::RigGrid),
            "rig-node-graph" => Some(Self::RigNodeGraph),
            "preset-browser" => Some(Self::PresetBrowser),
            "profile-browser" => Some(Self::ProfileBrowser),
            "song-parts" => Some(Self::SongParts),
            "song-selector" => Some(Self::SongSelector),
            "scene-grid" => Some(Self::SceneGrid),
            "transport" => Some(Self::Transport),
            "mixer" => Some(Self::Mixer),
            "navigator" => Some(Self::Navigator),
            "inspector" => Some(Self::Inspector),
            _ => None,
        }
    }

    /// Register all built-in panels into a [`PanelRegistry`](crate::registry::PanelRegistry).
    ///
    /// This seeds the registry with descriptors for all `PanelId` variants,
    /// preserving backward compatibility while enabling dynamic registration.
    pub fn register_all(registry: &mut crate::registry::PanelRegistry) {
        use crate::registry::{DockPosition, PanelDescriptor};

        for &panel in Self::all() {
            let category = match panel {
                Self::Performance
                | Self::ChartEditor
                | Self::ChartPreview
                | Self::Setlist
                | Self::Settings => "Session",
                Self::RigGrid
                | Self::RigNodeGraph
                | Self::PresetBrowser
                | Self::ProfileBrowser
                | Self::SongParts
                | Self::SongSelector
                | Self::SceneGrid => "Signal",
                Self::Transport | Self::Mixer => "DAW",
                Self::Navigator | Self::Inspector => "Utility",
            };

            let default_position = match panel {
                Self::Navigator | Self::Setlist | Self::PresetBrowser | Self::ProfileBrowser => {
                    DockPosition::Left
                }
                Self::Inspector => DockPosition::Right,
                Self::Transport | Self::Mixer | Self::SceneGrid => DockPosition::Bottom,
                _ => DockPosition::Center,
            };

            registry.register(
                PanelDescriptor::new(panel.as_str(), panel.display_name())
                    .with_icon(panel.icon_name())
                    .with_category(category)
                    .with_default_position(default_position),
            );
        }
    }

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
            Self::SceneGrid => "Scene Grid",
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
            Self::SceneGrid => "layout-grid",
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
            Self::SceneGrid,
            Self::Transport,
            Self::Mixer,
            Self::Navigator,
            Self::Inspector,
            Self::Settings,
        ]
    }
}
