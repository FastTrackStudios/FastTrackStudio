//! FTS plugin theme — color constants for inline-styled Blitz components.
//!
//! All FTS plugin UIs share these constants for visual consistency.
//! Blitz doesn't support CSS custom properties or Tailwind, so every
//! component uses `format!()` with these constants.

// ── Background & Surface ────────────────────────────────────────────
pub const BG: &str = "#141414";
pub const CARD_BG: &str = "#1c1c1c";
pub const SURFACE: &str = "#0e0e0e";

// ── Text ────────────────────────────────────────────────────────────
pub const TEXT: &str = "#d4d4d4";
pub const TEXT_DIM: &str = "#777";

// ── Accent ──────────────────────────────────────────────────────────
pub const ACCENT: &str = "#e0e0e0";
pub const ACCENT_HOVER: &str = "#ffffff";
pub const ACCENT_DIM: &str = "rgba(224,224,224,0.25)";

// ── Signal metering ─────────────────────────────────────────────────
pub const SIGNAL_SAFE: &str = "#4ade80";
pub const SIGNAL_WARN: &str = "#facc15";
pub const SIGNAL_DANGER: &str = "#f87171";
pub const SIGNAL_MOD: &str = "#8b5cf6";

// ── Controls ────────────────────────────────────────────────────────
pub const TOGGLE_OFF: &str = "#3a3a3a";
pub const BORDER: &str = "#2a2a2a";
pub const GRID_LINE: &str = "rgba(255,255,255,0.05)";
pub const CROSSHAIR: &str = "rgba(255,100,100,0.3)";
pub const REFERENCE_DOT: &str = "rgba(255,255,255,0.10)";

// ── CSS reset + root ────────────────────────────────────────────────

/// CSS reset. Inject via `document::Style` in your root component.
pub const BASE_CSS: &str = concat!(
    "*, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; } ",
    "html, body { background: #141414; width: 100%; height: 100%; overflow: hidden; }"
);

/// Root container style — fills viewport, scrolls vertically.
pub const ROOT_STYLE: &str = "\
    width:100vw; height:100vh; padding:12px 16px; \
    background:#141414; color:#d4d4d4; \
    font-family:system-ui,sans-serif; font-size:13px; user-select:none; \
    overflow-y:auto; position:relative;";
