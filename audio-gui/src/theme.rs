//! FTS plugin design system — warm hybrid skeuomorphic theme.
//!
//! All FTS plugin UIs share these constants for visual consistency.
//! Blitz doesn't support CSS custom properties, so every component
//! uses `format!()` with these constants.
//!
//! # Design philosophy
//!
//! Warm hybrid aesthetic: textured depth and analog warmth without
//! full photorealism. Inspired by u-he Diva, Soundtoys, and the
//! interactive clarity of FabFilter.

// ═══════════════════════════════════════════════════════════════════
//  COLORS
// ═══════════════════════════════════════════════════════════════════

// ── Background & Surface ────────────────────────────────────────────
/// Deepest background — plugin window fill.
pub const BG: &str = "#111114";
/// Card / panel background — slightly raised.
pub const CARD_BG: &str = "#1a1a1e";
/// Recessed surface — inset areas, meter troughs, input fields.
pub const SURFACE: &str = "#0c0c0f";
/// Subtle raised panel — section headers, toolbars.
pub const SURFACE_RAISED: &str = "#222228";
/// Hover highlight on interactive surfaces.
pub const SURFACE_HOVER: &str = "#28282e";

// ── Text ────────────────────────────────────────────────────────────
/// Primary text — labels, values.
pub const TEXT: &str = "#d4d4d8";
/// Secondary text — units, hints, dim labels.
pub const TEXT_DIM: &str = "#737380";
/// Bright text — active values, highlighted labels.
pub const TEXT_BRIGHT: &str = "#eeeeef";

// ── Accent ──────────────────────────────────────────────────────────
/// Primary accent — active arcs, fills, focus rings.
pub const ACCENT: &str = "#c8a86e";
/// Accent hover — brighter gold on mouseover.
pub const ACCENT_HOVER: &str = "#dfc088";
/// Accent dim — subtle tints, modulation overlays.
pub const ACCENT_DIM: &str = "rgba(200,168,110,0.25)";
/// Accent glow — box-shadow / text-shadow color for active elements.
pub const ACCENT_GLOW: &str = "rgba(200,168,110,0.35)";

// ── Signal metering ─────────────────────────────────────────────────
/// Safe / nominal level — green.
pub const SIGNAL_SAFE: &str = "#4ade80";
/// Warning level — amber.
pub const SIGNAL_WARN: &str = "#f0c040";
/// Danger / clip — red.
pub const SIGNAL_DANGER: &str = "#ef5350";
/// Modulation overlay — purple.
pub const SIGNAL_MOD: &str = "#8b5cf6";
/// Glow for safe meter segments.
pub const SIGNAL_SAFE_GLOW: &str = "rgba(74,222,128,0.3)";
/// Glow for warning meter segments.
pub const SIGNAL_WARN_GLOW: &str = "rgba(240,192,64,0.3)";
/// Glow for danger meter segments.
pub const SIGNAL_DANGER_GLOW: &str = "rgba(239,83,80,0.4)";

// ── Controls ────────────────────────────────────────────────────────
/// Toggle track — off state.
pub const TOGGLE_OFF: &str = "#333338";
/// Border — cards, panels, dividers.
pub const BORDER: &str = "#2a2a30";
/// Subtle border — inner separators, grid lines.
pub const BORDER_SUBTLE: &str = "#222228";
/// Grid lines in visualizations.
pub const GRID_LINE: &str = "rgba(255,255,255,0.05)";
/// Crosshair overlay (e.g. threshold marker).
pub const CROSSHAIR: &str = "rgba(255,100,100,0.3)";
/// Reference dots (e.g. 1:1 line on transfer curve).
pub const REFERENCE_DOT: &str = "rgba(255,255,255,0.10)";
/// Knob track — the unlit arc behind the value arc.
pub const KNOB_TRACK: &str = "#252528";
/// Knob body gradient start (top-left highlight).
pub const KNOB_BODY_LIGHT: &str = "#3a3a42";
/// Knob body gradient end (bottom-right shadow).
pub const KNOB_BODY_DARK: &str = "#1a1a1e";
/// Knob indicator dot / line.
pub const KNOB_INDICATOR: &str = "#d4d4d8";

// ═══════════════════════════════════════════════════════════════════
//  SHADOWS & DEPTH
// ═══════════════════════════════════════════════════════════════════

/// Raised panel shadow — cards, sections.
pub const SHADOW_RAISED: &str = "0 2px 8px rgba(0,0,0,0.4), 0 1px 2px rgba(0,0,0,0.3)";
/// Inset shadow — recessed areas, meter troughs, tracks.
pub const SHADOW_INSET: &str = "inset 0 1px 3px rgba(0,0,0,0.5), inset 0 0 1px rgba(0,0,0,0.3)";
/// Knob shadow — drop shadow under knobs.
pub const SHADOW_KNOB: &str = "0 2px 6px rgba(0,0,0,0.5), 0 1px 2px rgba(0,0,0,0.3)";
/// Subtle raised — toggles, segment buttons.
pub const SHADOW_SUBTLE: &str = "0 1px 3px rgba(0,0,0,0.3)";
/// Glow shadow for active/lit elements.
pub const SHADOW_GLOW: &str = "0 0 8px rgba(200,168,110,0.3)";
/// Inner highlight — top edge light catch on raised elements.
pub const HIGHLIGHT_TOP: &str = "inset 0 1px 0 rgba(255,255,255,0.06)";
/// Combined raised + top highlight (most panels use this).
pub const SHADOW_PANEL: &str =
    "0 2px 8px rgba(0,0,0,0.4), 0 1px 2px rgba(0,0,0,0.3), inset 0 1px 0 rgba(255,255,255,0.06)";

// ═══════════════════════════════════════════════════════════════════
//  TYPOGRAPHY
// ═══════════════════════════════════════════════════════════════════

/// Primary font stack — clean sans-serif for labels and body text.
pub const FONT_FAMILY: &str = "system-ui, -apple-system, 'Segoe UI', sans-serif";
/// Monospace font stack — numeric values, readouts, meters.
pub const FONT_MONO: &str = "'SF Mono', 'Cascadia Code', 'JetBrains Mono', ui-monospace, monospace";

/// Plugin title in header.
pub const FONT_SIZE_TITLE: &str = "16px";
/// Primary control values.
pub const FONT_SIZE_VALUE: &str = "12px";
/// Control labels, section headers.
pub const FONT_SIZE_LABEL: &str = "10px";
/// Tiny annotations — units, ranges, axis labels.
pub const FONT_SIZE_TINY: &str = "9px";
/// Large readout — e.g. gain reduction number.
pub const FONT_SIZE_READOUT: &str = "20px";

/// Letter spacing for uppercase labels.
pub const LETTER_SPACING_LABEL: &str = "0.6px";

// ═══════════════════════════════════════════════════════════════════
//  SPACING
// ═══════════════════════════════════════════════════════════════════

/// Root padding around the entire plugin UI.
pub const SPACING_ROOT: &str = "10px 14px";
/// Gap between major sections (e.g. header to body).
pub const SPACING_SECTION: &str = "10px";
/// Gap between controls within a group.
pub const SPACING_CONTROL: &str = "14px";
/// Internal padding of cards/panels.
pub const SPACING_CARD: &str = "10px 12px";
/// Gap between a label and its control.
pub const SPACING_LABEL: &str = "4px";
/// Tight gap — e.g. between meter segments.
pub const SPACING_TIGHT: &str = "2px";

// ═══════════════════════════════════════════════════════════════════
//  BORDER RADIUS
// ═══════════════════════════════════════════════════════════════════

/// Cards, panels, visualization containers.
pub const RADIUS_CARD: &str = "6px";
/// Buttons, toggles, segment items.
pub const RADIUS_BUTTON: &str = "4px";
/// Small elements — tags, indicators, badges.
pub const RADIUS_SMALL: &str = "3px";
/// Fully round — knob bodies, indicator dots.
pub const RADIUS_ROUND: &str = "50%";

// ═══════════════════════════════════════════════════════════════════
//  TRANSITIONS
// ═══════════════════════════════════════════════════════════════════

/// Default transition for interactive element state changes.
pub const TRANSITION_FAST: &str = "all 0.12s ease";
/// Slower transition — expanding sections, mode switches.
pub const TRANSITION_NORMAL: &str = "all 0.2s ease";

// ═══════════════════════════════════════════════════════════════════
//  CSS RESET & ROOT STYLES
// ═══════════════════════════════════════════════════════════════════

/// CSS reset. Inject via `document::Style` in your root component.
pub const BASE_CSS: &str = concat!(
    "*, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; } ",
    "html, body { width: 100%; height: 100%; overflow: hidden; ",
    "background: #111114; color: #d4d4d8; ",
    "font-family: system-ui, -apple-system, 'Segoe UI', sans-serif; ",
    "font-size: 13px; }",
);

/// Root container style — fills viewport, applies base theme.
pub const ROOT_STYLE: &str = "\
    width:100vw; height:100vh; \
    padding:10px 14px; \
    background:#111114; color:#d4d4d8; \
    font-family:system-ui,-apple-system,'Segoe UI',sans-serif; \
    font-size:13px; user-select:none; \
    overflow-y:auto; position:relative;";

// ═══════════════════════════════════════════════════════════════════
//  COMPOSITE STYLE HELPERS
// ═══════════════════════════════════════════════════════════════════

/// Style for a raised card/panel section.
///
/// Usage: `style: "{} {extra_styles}", theme::STYLE_CARD`
pub const STYLE_CARD: &str = "\
    background: #1a1a1e; \
    border: 1px solid #2a2a30; \
    border-radius: 6px; \
    box-shadow: 0 2px 8px rgba(0,0,0,0.4), 0 1px 2px rgba(0,0,0,0.3), inset 0 1px 0 rgba(255,255,255,0.06);";

/// Style for a recessed / inset trough (meter tracks, slider tracks).
///
/// Usage: `style: "{} {extra_styles}", theme::STYLE_INSET`
pub const STYLE_INSET: &str = "\
    background: #0c0c0f; \
    border: 1px solid #222228; \
    border-radius: 4px; \
    box-shadow: inset 0 1px 3px rgba(0,0,0,0.5), inset 0 0 1px rgba(0,0,0,0.3);";

/// Style for uppercase dim labels (control labels, section headers).
///
/// Usage: `style: "{} {extra_styles}", theme::STYLE_LABEL`
pub const STYLE_LABEL: &str = "\
    font-size: 10px; \
    font-weight: 600; \
    color: #737380; \
    text-transform: uppercase; \
    letter-spacing: 0.6px;";

/// Style for numeric value readouts (tabular figures, mono font).
///
/// Usage: `style: "{} {extra_styles}", theme::STYLE_VALUE`
pub const STYLE_VALUE: &str = "\
    font-family: 'SF Mono','Cascadia Code','JetBrains Mono',ui-monospace,monospace; \
    font-size: 12px; \
    font-variant-numeric: tabular-nums; \
    color: #d4d4d8;";

/// Style for a large numeric readout (e.g. GR meter value).
///
/// Usage: `style: "{} {extra_styles}", theme::STYLE_READOUT`
pub const STYLE_READOUT: &str = "\
    font-family: 'SF Mono','Cascadia Code','JetBrains Mono',ui-monospace,monospace; \
    font-size: 20px; \
    font-variant-numeric: tabular-nums; \
    font-weight: 300; \
    color: #d4d4d8;";
