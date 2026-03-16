//! FTS plugin theme — color constants for inline-styled Blitz components.

pub const BG: &str = "#1a1a2e";
pub const CARD_BG: &str = "#222240";
pub const TEXT: &str = "#e0e0e0";
pub const TEXT_DIM: &str = "#888";
pub const ACCENT: &str = "#6c63ff";
pub const ACCENT_HOVER: &str = "#7c74ff";
pub const GREEN: &str = "#4ade80";
pub const TOGGLE_OFF: &str = "#444";
pub const BORDER: &str = "#333";

/// CSS reset + body background. Inject via `document::Style` in your root component.
pub const BASE_CSS: &str = concat!(
    "*, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; } ",
    "html, body { background: #1a1a2e; width: 100%; height: 100%; overflow: hidden; }"
);

/// Root container style — fills viewport, scrolls vertically.
pub const ROOT_STYLE: &str = "\
    width:100vw; height:100vh; padding:12px 16px; \
    background:#1a1a2e; color:#e0e0e0; \
    font-family:system-ui,sans-serif; font-size:13px; user-select:none; \
    overflow-y:auto; position:relative;";
