//! View mode types for the rig grid UI.
//!
//! Each module can display in one of three view modes. Users can set a global
//! override that forces all modules to the same mode, or let each module
//! remember its preferred view mode.

/// View mode for displaying the rig.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum ModuleViewMode {
    /// Full parameter editing view.
    ///
    /// Shows all blocks with their complete parameter lists using audio-controls
    /// widgets. Width: ~320px per module.
    Detail,

    /// Quick-access macro knobs only.
    ///
    /// Shows only the `ModuleMacro` knobs that have been configured for rapid
    /// access. Width: ~192px per module.
    Macro,

    /// Signal flow grid (Quad Cortex style).
    ///
    /// Shows blocks on a 14x6 grid with routing lines and I/O jacks.
    /// This is the primary compact view mode.
    #[default]
    Grid,
}

impl ModuleViewMode {
    /// Get the display name for this view mode.
    pub const fn display_name(self) -> &'static str {
        match self {
            Self::Detail => "Detail",
            Self::Macro => "Macro",
            Self::Grid => "Grid",
        }
    }

    /// Get a short icon/symbol for this view mode.
    pub const fn icon(self) -> &'static str {
        match self {
            Self::Detail => "⚙",  // Gear for full settings
            Self::Macro => "◉",  // Circle for knobs
            Self::Grid => "⊞",   // Grid for signal flow
        }
    }

    /// Get the recommended width in pixels for this view mode.
    /// Note: Grid mode uses full width (signal flow layout).
    pub const fn width(self) -> u32 {
        match self {
            Self::Detail => 320,
            Self::Macro => 192,
            Self::Grid => 800, // Full signal flow grid
        }
    }

    /// Get all view modes in display order.
    pub const fn all() -> &'static [ModuleViewMode] {
        &[
            ModuleViewMode::Grid,
            ModuleViewMode::Macro,
            ModuleViewMode::Detail,
        ]
    }
}

/// Global view override state.
///
/// When `Override` is active, all modules display in the specified mode
/// regardless of their individual preferences.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum GlobalViewOverride {
    /// No override - each module uses its own preferred view mode.
    #[default]
    None,

    /// Force all modules to display in the specified view mode.
    Override(ModuleViewMode),
}

impl GlobalViewOverride {
    /// Get the effective view mode for a module.
    ///
    /// If there's a global override, it takes precedence. Otherwise,
    /// the module's individual preference is used.
    pub fn effective_mode(self, module_preference: ModuleViewMode) -> ModuleViewMode {
        match self {
            Self::None => module_preference,
            Self::Override(mode) => mode,
        }
    }

    /// Check if a global override is active.
    pub const fn is_active(self) -> bool {
        matches!(self, Self::Override(_))
    }

    /// Get the override mode, if active.
    pub const fn mode(self) -> Option<ModuleViewMode> {
        match self {
            Self::None => None,
            Self::Override(mode) => Some(mode),
        }
    }
}
