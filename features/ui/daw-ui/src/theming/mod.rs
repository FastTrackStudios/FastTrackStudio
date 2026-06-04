//! Theming system for audio controls.
//!
//! Provides customizable styling through the `StyleSheet` trait and preset themes.

pub mod context;
/// The MCP strip context — REAPER's `mcp.*` element vocabulary as typed layouts.
pub mod mcp;
pub mod presets;
pub mod style;
pub mod svg_texture;
/// Token-based theme model (vector-first; Phase 1 of the themeable-UI plan).
pub mod theme;
/// WALTER layout primitives (8-value anchor coords, margins, fonts, params).
pub mod walter;

pub use context::{ControlConfig, ThemeContext, ThemeProvider, use_theme};
pub use mcp::{McpColors, McpLayout, McpTheme};
pub use style::{ControlState, ControlVariant, KnobStyle, SliderStyle, StyleSheet, XYPadStyle};
pub use svg_texture::SvgTexture;
pub use theme::{
    Color, ElementRole, FaderStyle, MeterStyle, Metrics, PanelStyle, StripStyle, Theme, ThemeState,
    ToggleKind, ToggleStyle, Tokens,
};
pub use walter::{ColorPair, Coord, FaderMode, FontSpec, Margin, Rect, ThemeParam};
