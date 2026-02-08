//! View mode types for the rig grid UI.

/// Page-level view mode for the guitar rig interface.
///
/// Determines which sidebars are shown and how the layout is organized.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum RigViewMode {
    /// Preset browsing mode.
    ///
    /// - Left: Presets (full height)
    /// - Right: Hidden
    Preset,

    /// Profile browsing mode.
    ///
    /// - Left: Presets (full height)
    /// - Right: Profiles (full height)
    Profile,

    /// Song/performance mode (default).
    ///
    /// - Left: Split view (Presets 60%, Profiles 40%)
    /// - Right: Songs and Scenes
    #[default]
    Song,
}

impl RigViewMode {
    /// Get the display name for this view mode.
    pub const fn display_name(self) -> &'static str {
        match self {
            Self::Preset => "Presets",
            Self::Profile => "Profiles",
            Self::Song => "Songs",
        }
    }

    /// Get all view modes in display order.
    pub const fn all() -> &'static [RigViewMode] {
        &[RigViewMode::Preset, RigViewMode::Profile, RigViewMode::Song]
    }
}

/// View mode for the node graph display.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum ModuleViewMode {
    /// Full node graph with pan/zoom and widget visualizations.
    ///
    /// Shows modules with child nodes containing audio widget previews
    /// (EQ graphs, compressor curves, amp/cab, etc). Interactive pan
    /// and zoom with fit-to-view controls.
    #[default]
    Flow,

    /// Compact node graph — auto-fitted overview.
    ///
    /// Same full-size graph with all widget visualizations, auto-zoomed
    /// to fit all modules on screen simultaneously. Modules are shown as
    /// large as possible while ensuring everything is visible. Good for
    /// overview without scrolling or manual zoom.
    FlowCompact,
}

impl ModuleViewMode {
    /// Get the display name for this view mode.
    pub const fn display_name(self) -> &'static str {
        match self {
            Self::Flow => "Flow",
            Self::FlowCompact => "Compact",
        }
    }

    /// Get a short icon/symbol for this view mode.
    pub const fn icon(self) -> &'static str {
        match self {
            Self::Flow => "⬡",
            Self::FlowCompact => "⬢",
        }
    }

    /// Get all view modes in display order.
    pub const fn all() -> &'static [ModuleViewMode] {
        &[ModuleViewMode::Flow, ModuleViewMode::FlowCompact]
    }
}
