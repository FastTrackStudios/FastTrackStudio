//! Theme definitions for consistent styling across plugins.

use serde::{Deserialize, Serialize};

/// Color theme for plugin UIs.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Theme {
    /// Background color (main)
    pub bg_primary: &'static str,
    /// Background color (secondary/panels)
    pub bg_secondary: &'static str,
    /// Background color (tertiary/inputs)
    pub bg_tertiary: &'static str,

    /// Border color
    pub border: &'static str,

    /// Text color (primary)
    pub text_primary: &'static str,
    /// Text color (secondary/muted)
    pub text_secondary: &'static str,
    /// Text color (tertiary/disabled)
    pub text_tertiary: &'static str,

    /// Accent color (primary - for highlights, active states)
    pub accent_primary: &'static str,
    /// Accent color (secondary)
    pub accent_secondary: &'static str,

    /// Meter colors
    pub meter_green: &'static str,
    pub meter_yellow: &'static str,
    pub meter_red: &'static str,

    /// Gain reduction meter color
    pub meter_gr: &'static str,
}

/// Default dark theme for FTS plugins.
pub const DARK_THEME: Theme = Theme {
    bg_primary: "#0f0f12",
    bg_secondary: "#1a1a1e",
    bg_tertiary: "#242428",

    border: "#2a2a2e",

    text_primary: "#ffffff",
    text_secondary: "#888888",
    text_tertiary: "#666666",

    accent_primary: "#3b82f6",
    accent_secondary: "#60a5fa",

    meter_green: "#22c55e",
    meter_yellow: "#eab308",
    meter_red: "#ef4444",

    meter_gr: "#f97316",
};
