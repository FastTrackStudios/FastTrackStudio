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

use daw_theme_reaper::{ImageCatalog, Rgba};
pub use daw_theme_reaper::{ReaperTheme, ThemeError};

use super::mcp::{ButtonSkin, ButtonStateSkin, McpSkin, SkinImage};
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

    // ── image skin ──
    mcp.skin = extract_skin(rt);

    // REAPER's MCP pan is a horizontal slider (`mcp.pan.fadermode` resolves
    // to 1 in the default theme's WALTER, knob only for dual-pan). When the
    // theme ships pan slider art, switch visible pan elements over.
    if mcp.skin.as_ref().is_some_and(|s| s.panbg.is_some()) {
        for layout in &mut mcp.layouts {
            if !layout.pan.is_hidden() {
                layout.pan = super::walter::Coord::new(6.0, 9.0, 52.0, 19.0, 0.0, 0.0, 1.0, 0.0);
                layout.pan_fadermode = super::walter::FaderMode::Horizontal;
            }
        }
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

/// Slice the theme's button/fader atlases into an [`McpSkin`] (data-URI PNGs).
///
/// Image lookup walks REAPER's fallback chain: a context image (`mcp_X`),
/// then the shared track vocabulary (`track_X`), then the general fallback
/// (`gen_X`) — the Anti-Theme, like the stock default, ships most strip
/// buttons as `track_*`/`gen_*`.
fn extract_skin(rt: &ReaperTheme) -> Option<McpSkin> {
    let imgs = &rt.images;

    // First catalog name present along the fallback chain.
    let find = |base: &str| -> Option<String> {
        ["mcp_", "track_", "gen_"]
            .iter()
            .map(|p| format!("{p}{base}"))
            .find(|n| imgs.has(n))
    };

    // All three interaction states of a 3-slice button, with the `*_ol`
    // overlay composited per state — `use_overlays 1` themes (incl. the
    // default) keep the base states transparent and ship the visible art in
    // the overlay.
    let button = |base: &str| -> Option<ButtonStateSkin> {
        let name = find(base)?;
        let s = imgs.button3(&name).ok()?;
        let ol = imgs.button3(&format!("{name}_ol")).ok();
        let state = |b: &daw_theme_reaper::image::RgbaImage,
                     o: Option<&daw_theme_reaper::image::RgbaImage>| {
            let img = match o {
                Some(o) => daw_theme_reaper::images::alpha_over(b, o),
                None => b.clone(),
            };
            SkinImage {
                url: ImageCatalog::data_uri(&img),
                w: img.width(),
                h: img.height(),
            }
        };
        Some(ButtonStateSkin {
            normal: state(&s.normal, ol.as_ref().map(|o| &o.normal)),
            hover: state(&s.hover, ol.as_ref().map(|o| &o.hover)),
            pressed: state(&s.pressed, ol.as_ref().map(|o| &o.pressed)),
        })
    };
    let toggle = |base_off: &str, base_on: &str| -> Option<ButtonSkin> {
        Some(ButtonSkin {
            off: button(base_off)?,
            on: button(base_on)?,
        })
    };
    // A plain (marker-stripped) image.
    let plain = |name: &str| -> Option<SkinImage> {
        let s = imgs.load(name).ok()?;
        Some(SkinImage {
            url: ImageCatalog::data_uri(&s.image),
            w: s.image.width(),
            h: s.image.height(),
        })
    };

    let skin = McpSkin {
        mute: toggle("mute_off", "mute_on"),
        solo: toggle("solo_off", "solo_on"),
        recarm: toggle("recarm_off", "recarm_on"),
        io: button("io"),
        volbg: plain("mcp_volbg"),
        volthumb: plain("mcp_volthumb"),
        panbg: plain("mcp_panbg"),
        panthumb: plain("mcp_panthumb"),
        meter_strip: plain("meter_strip_v"),
        meter_bg: plain("meter_bg_v"),
    };

    // No images at all → stay vector.
    let any = skin.mute.is_some()
        || skin.solo.is_some()
        || skin.recarm.is_some()
        || skin.io.is_some()
        || skin.volbg.is_some()
        || skin.volthumb.is_some();
    any.then_some(skin)
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

        // Image skin extracted: mute/solo via the track_* fallback, fader
        // bg/thumb from mcp_volbg/mcp_volthumb — all as data URIs.
        let skin = theme.mcp.skin.as_ref().expect("anti-theme yields a skin");
        let mute = skin.mute.as_ref().expect("mute skin");
        assert!(mute.off.normal.url.starts_with("data:image/png;base64,"));
        // 20 wide from the base 3-slice; height includes the _ol overlay's
        // shadow rows (the visible art — the base off-state is transparent).
        assert_eq!(mute.off.normal.w, 20);
        assert!(mute.off.normal.h >= 20);
        assert!(mute.off.hover.w > 0 && mute.off.pressed.w > 0);
        assert!(skin.solo.is_some());
        assert!(skin.io.is_some());
        let thumb = skin.volthumb.as_ref().expect("volthumb");
        assert_eq!((thumb.w, thumb.h), (23, 53));

        // Pan slider art + meter strips extracted; pan switched horizontal.
        assert!(skin.panbg.is_some() && skin.panthumb.is_some());
        assert!(skin.meter_strip.is_some() && skin.meter_bg.is_some());
        use crate::theming::FaderMode;
        let vertical = theme.mcp.layout(Some("vertical"));
        assert_eq!(vertical.pan_fadermode, FaderMode::Horizontal);
    }
}
