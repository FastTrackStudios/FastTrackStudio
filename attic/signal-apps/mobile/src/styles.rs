//! Design tokens for the Signal mobile UI.
//!
//! All colours, radii, and spacing in one place so components stay consistent
//! without needing Tailwind (which requires a build step / node toolchain).

pub const BG_APP: &str = "#0a0a0b";
pub const BG_PANEL: &str = "#141416";
pub const BG_SURFACE: &str = "#1c1c20";
pub const BG_ACTIVE: &str = "#23232a";
pub const BORDER: &str = "#2a2a32";
pub const BORDER_SUBTLE: &str = "#1e1e24";

pub const TEXT: &str = "#e4e4e7";
pub const TEXT_DIM: &str = "#71717a";
pub const TEXT_BRIGHT: &str = "#ffffff";

pub const ACCENT: &str = "#3b82f6"; // blue-500
pub const ACCENT_DIM: &str = "#1d4ed8"; // blue-700
pub const WARN: &str = "#f59e0b"; // amber-500
pub const DANGER: &str = "#ef4444"; // red-500
pub const SUCCESS: &str = "#22c55e"; // green-500

pub const RADIUS_SM: &str = "6px";
pub const RADIUS_MD: &str = "10px";
pub const RADIUS_LG: &str = "16px";

pub const FONT: &str =
    "-apple-system, BlinkMacSystemFont, 'SF Pro Display', 'Helvetica Neue', sans-serif";
pub const FONT_MONO: &str = "'SF Mono', 'Courier New', monospace";

/// Minimum touch target size per Apple HIG (44×44 pt).
pub const TOUCH_TARGET: &str = "44px";
