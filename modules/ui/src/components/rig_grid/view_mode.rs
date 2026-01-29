//! View mode types for the rig grid UI.
//!
//! Each module can display in one of three view modes. Users can set a global
//! override that forces all modules to the same mode, or let each module
//! remember its preferred view mode.

/// View mode for displaying a single module in the rig grid.
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
    #[default]
    Macro,

    /// Minimal colored pill display.
    ///
    /// Shows blocks as compact colored buttons (Quad Cortex style) with bypass
    /// indicators. Width: ~128px per module.
    Compact,
}

impl ModuleViewMode {
    /// Get the display name for this view mode.
    pub const fn display_name(self) -> &'static str {
        match self {
            Self::Detail => "Detail",
            Self::Macro => "Macro",
            Self::Compact => "Compact",
        }
    }

    /// Get a short icon/symbol for this view mode.
    pub const fn icon(self) -> &'static str {
        match self {
            Self::Detail => "⚙",  // Gear for full settings
            Self::Macro => "◉",  // Circle for knobs
            Self::Compact => "▪", // Square for pills
        }
    }

    /// Get the recommended width in pixels for this view mode.
    pub const fn width(self) -> u32 {
        match self {
            Self::Detail => 320,
            Self::Macro => 192,
            Self::Compact => 128,
        }
    }

    /// Get all view modes in display order.
    pub const fn all() -> &'static [ModuleViewMode] {
        &[
            ModuleViewMode::Compact,
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
