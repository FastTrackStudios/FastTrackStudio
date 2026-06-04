//! REAPER theme → FTS [`Theme`] mapping (the `reaper-theme` feature).
//!
//! Converts a loaded [`daw_theme_reaper::ReaperTheme`] into our token model:
//! `[color theme]` palette keys feed [`Tokens`], the meter scale colours feed
//! [`McpColors`], and `define_parameter` knobs surface as [`ThemeParam`]s.
//! Keys a theme doesn't carry fall back to the FTS dark defaults, so a
//! partial palette still yields a complete theme.
//!
//! Key choices (verified against the Anti-Theme / REAPER 7 default):
//! - `col_main_bg2` → app surface, `col_tr1_bg` → raised strip surface,
//!   `col_main_3dsh` → borders, `col_main_text`/`col_main_text2` → text.
//! - `col_cursor` → accent (the edit cursor is REAPER's de-facto accent).
//! - `col_vutop`/`col_vumid`/`col_vubot` → meter zones **and** the
//!   `mcp.meter.scale` lit gradient; `col_vuclip` → danger.
//! - Button colours (mute/solo/arm) stay FTS defaults: REAPER themes carry
//!   them as images, not palette keys (the image skin is the next layer).

use daw_theme_reaper::Rgba;
pub use daw_theme_reaper::{ReaperTheme, ThemeError};

use super::theme::{Color, Theme};
use super::walter::ThemeParam;

/// Load an unpacked REAPER theme directory and map it to an FTS [`Theme`]
/// in one step (apps depend only on the facade; the parser stays internal).
pub fn theme_from_dir(dir: impl AsRef<std::path::Path>) -> Result<Theme, ThemeError> {
    Ok(theme_from_reaper(&ReaperTheme::load_dir(dir)?))
}

fn color(c: Rgba) -> Color {
    Color::rgba(c.r, c.g, c.b, c.a)
}

/// Build an FTS [`Theme`] from a loaded REAPER theme (colors + params).
///
/// Image-skin extraction is a separate layer; this covers the palette,
/// rtconfig globals and adjuster knobs.
pub fn theme_from_reaper(rt: &ReaperTheme) -> Theme {
    let mut theme = Theme::dark();
    let pal = |key: &str| rt.palette.color(key).map(color);

    // ── semantic tokens ──
    let t = &mut theme.tokens;
    if let Some(c) = pal("col_main_bg2").or_else(|| pal("col_tracklistbg")) {
        t.surface = c;
        t.surface_sunken = c.darken(0.25);
    }
    if let Some(c) = pal("col_tr1_bg").or_else(|| pal("col_seltrack2")) {
        t.surface_raised = c;
    }
    if let Some(c) = pal("col_main_3dsh").or_else(|| pal("col_tr1_divline")) {
        t.border = c;
    }
    if let Some(c) = pal("col_main_text") {
        t.text = c;
    }
    if let Some(c) = pal("col_main_text2") {
        t.text_dim = c;
        t.text_faint = c.darken(0.3);
        t.neutral_track = c;
    }
    if let Some(c) = pal("col_cursor") {
        t.accent = c;
    }
    if let Some(c) = pal("col_vubot") {
        t.meter_safe = c;
    }
    if let Some(c) = pal("col_vumid") {
        t.meter_warn = c;
    }
    if let Some(c) = pal("col_vuclip").or_else(|| pal("col_vutop")) {
        t.meter_danger = c;
    }

    // ── MCP context: meter scale gradient (mcp.meter.scale.color.lit.*) ──
    let mcp = &mut theme.mcp;
    mcp.colors.meter_lit_top = pal("col_vutop");
    mcp.colors.meter_lit_bottom = pal("col_vubot");

    // Fader zero-line → volume accent, when the theme pins one.
    if let Some(zl) = rt.rtconfig.global_color("mcp_vol_zeroline") {
        mcp.colors.volume = Some(color(zl));
    }

    // ── define_parameter knobs ──
    // Imported knobs are appended after the FTS ones; same-name knobs from
    // the theme replace ours.
    for p in &rt.rtconfig.params {
        let knob = ThemeParam {
            name: p.name.clone(),
            desc: p.desc.clone(),
            value: p.default,
            default: p.default,
            min: p.min,
            max: p.max,
        };
        match mcp.params.iter_mut().find(|e| e.name == p.name) {
            Some(existing) => *existing = knob,
            None => mcp.params.push(knob),
        }
    }

    theme
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The unpacked Anti-Theme (skips silently when the corpus is absent).
    fn antitheme() -> Option<ReaperTheme> {
        let dir = std::env::var("REAPER_ANTITHEME_DIR").unwrap_or_else(|_| {
            "/home/cody/Development/FastTrackStudio/reaper-theme/extracted/antitheme".to_string()
        });
        ReaperTheme::load_dir(&dir).ok()
    }

    #[test]
    fn maps_anti_theme_palette_onto_tokens() {
        let Some(rt) = antitheme() else {
            eprintln!("anti-theme not found — skipping");
            return;
        };
        let theme = theme_from_reaper(&rt);
        let t = &theme.tokens;

        // col_main_bg2 = 3355443 = 0x333333.
        assert_eq!((t.surface.r, t.surface.g, t.surface.b), (0x33, 0x33, 0x33));
        // col_main_text = 12566463 = 0xBFBFBF.
        assert_eq!((t.text.r, t.text.g, t.text.b), (0xbf, 0xbf, 0xbf));
        // col_cursor = 8886323 = 0x879833 → COLORREF r=0x33 g=0x98 b=0x87.
        assert_eq!((t.accent.r, t.accent.g, t.accent.b), (0x33, 0x98, 0x87));

        // Meter gradient pinned from col_vutop/col_vubot.
        assert!(theme.mcp.colors.meter_lit_top.is_some());
        assert!(theme.mcp.colors.meter_lit_bottom.is_some());

        // Anti-Theme adjuster knobs surfaced.
        assert!(theme.mcp.params.iter().any(|p| p.name == "textBrightness"));
        // FTS knobs are kept too.
        assert!(theme.mcp.params.iter().any(|p| p.name == "mcp_show_pan"));
    }
}
