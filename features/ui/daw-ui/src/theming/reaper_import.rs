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
/// Scale comes from `FTS_THEME_SCALE` (default 1.0).
pub fn theme_from_dir(dir: impl AsRef<std::path::Path>) -> Result<Theme, ThemeError> {
    let scale = std::env::var("FTS_THEME_SCALE")
        .ok()
        .and_then(|s| s.parse::<f32>().ok())
        .unwrap_or(1.0);
    theme_from_dir_scaled(dir, scale)
}

/// [`theme_from_dir`] at an explicit DPI scale: ≥1.25 selects the theme's
/// `150%_*` layout variants + `150/` images (≥1.75 → `200%_*`/`200/`),
/// mirroring REAPER's `misc_dpi_translate` behaviour.
pub fn theme_from_dir_scaled(
    dir: impl AsRef<std::path::Path>,
    scale: f32,
) -> Result<Theme, ThemeError> {
    Ok(theme_from_reaper_scaled(
        &ReaperTheme::load_dir(dir)?,
        scale,
    ))
}

fn color(c: Rgba) -> Color {
    Color::rgba(c.r, c.g, c.b, c.a)
}

/// Build an FTS [`Theme`] from a loaded REAPER theme at 100% scale.
pub fn theme_from_reaper(rt: &ReaperTheme) -> Theme {
    theme_from_reaper_scaled(rt, 1.0)
}

/// Build an FTS [`Theme`] from a loaded REAPER theme (colors + params) at a
/// DPI scale (1.0 / 1.5 / 2.0 — the theme's layout + image variants).
pub fn theme_from_reaper_scaled(rt: &ReaperTheme, scale: f32) -> Theme {
    let mut theme = Theme::dark();
    let pal = |key: &str| rt.palette.color(key).map(color);

    // The per-DPI image folder + layout-name prefix REAPER would pick for
    // this scale (`misc_dpi_translate 134 150` → folder "150" at ≥134%).
    let dpi_folder: Option<String> = rt
        .rtconfig
        .dpi_translate
        .iter()
        .filter(|(min_dpi, _)| scale * 100.0 >= *min_dpi)
        .max_by(|a, b| a.0.total_cmp(&b.0))
        .map(|(_, folder)| folder.clone());

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

    // ── image skin ── (per-DPI images override the base set)
    let mut imgs = rt.images.clone();
    if let Some(folder) = &dpi_folder {
        let _ = imgs.overlay_subdir(folder);
    }
    mcp.skin = extract_skin(&imgs);

    // REAPER's MCP pan is a horizontal slider (`mcp.pan.fadermode` resolves
    // to 1 in the default theme's WALTER, knob only for dual-pan). When the
    // theme ships pan slider art, switch the *fallback* layouts' visible pan
    // elements over (the WALTER-evaluated layouts below carry their own pan
    // geometry).
    if mcp.skin.as_ref().is_some_and(|s| s.panbg.is_some()) {
        for layout in &mut mcp.layouts {
            if !layout.pan.is_hidden() {
                layout.pan = super::walter::Coord::new(6.0, 9.0, 52.0, 19.0, 0.0, 0.0, 1.0, 0.0);
                layout.pan_fadermode = super::walter::FaderMode::Horizontal;
            }
        }
    }

    // ── WALTER-evaluated layouts ──
    // Run the theme's own rtconfig program per named layout and convert the
    // resolved `mcp.*` geometry into McpLayouts. These take precedence (the
    // first layout is the default); the FTS fallbacks stay reachable by name.
    let walter_layouts = walter_mcp_layouts(rt, scale, dpi_folder.as_deref());
    if !walter_layouts.is_empty() {
        let fallbacks = std::mem::take(&mut mcp.layouts);
        mcp.layouts = walter_layouts;
        mcp.layouts.extend(fallbacks);
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

/// Evaluate the theme's WALTER program per named layout and convert the
/// resolved `mcp.*` attributes into [`McpLayout`]s.
///
/// The 8-value anchor model is recovered by **finite differences**: WALTER
/// output is piecewise-linear in the panel size within a form, so evaluating
/// at the natural size and at `+Δw`/`+Δh` yields each element's edge-attach
/// scales exactly. Natural width comes from the theme's own `mcpWidth`
/// (two-pass: probe, then evaluate at that width).
fn walter_mcp_layouts(
    rt: &ReaperTheme,
    scale: f32,
    dpi_folder: Option<&str>,
) -> Vec<super::mcp::McpLayout> {
    use daw_theme_reaper::walter::{Env, Output, evaluate};

    let src = &rt.rtconfig_src;
    let make_env = |w: f32, h: f32| -> Env {
        let mut env = Env::reaper_defaults(w, h);
        env.set("Scale", scale);
        // Bake with the track-colour *sentinel* so colours the theme derives
        // from it stay dynamic (renderers substitute the live accent).
        env.set("trackcolor_valid", 1.0);
        env.set("trackcolor_r", Color::TRACK.r as f32);
        env.set("trackcolor_g", Color::TRACK.g as f32);
        env.set("trackcolor_b", Color::TRACK.b as f32);
        for p in &rt.rtconfig.params {
            env.set(&p.name, p.default);
        }
        env
    };

    let h0 = 600.0 * scale;
    const DW: f32 = 16.0;
    const DH: f32 = 32.0;
    let (h0, dw, dh) = (h0, DW, DH);

    // Which layouts exist? Base names only — DPI variants (`150%_A`) are the
    // same layout at another Scale and are selected below per `scale`.
    let probe = evaluate(src, None, &make_env(100.0, h0));
    let names: Vec<String> = probe
        .layouts
        .iter()
        .filter(|n| !n.contains('%'))
        .cloned()
        .collect();
    // Evaluate the DPI variant when the theme ships one for this scale
    // (`Layout "150%_A" "150"` runs the same macros at Scale 1.5 with the
    // `150/` images); the result is exposed under the base name.
    let variant_of = |base: &str| -> String {
        match dpi_folder {
            Some(folder) => {
                let v = format!("{folder}%_{base}");
                if probe.layouts.iter().any(|l| l == &v) {
                    v
                } else {
                    base.to_string()
                }
            }
            None => base.to_string(),
        }
    };

    let mut layouts = Vec::new();
    for name in names {
        let eval_name = variant_of(&name);
        // Pass 1: the theme's own strip width for this layout.
        let pass1 = evaluate(src, Some(&eval_name), &make_env(100.0, h0));
        let w0 = match pass1
            .get("mcpWidth")
            .map(|v| v.first().copied().unwrap_or(0.0))
        {
            Some(w) if w >= 24.0 => w,
            _ => continue,
        };

        // Pass 2: natural + finite-difference evaluations. Strip width is
        // theme-driven (`mcpWidth` reads the layout's width knob, not env
        // `w`), so the +Δw pass bumps the matching `define_parameter` too.
        // Track-state variants re-run the program with the state scalar set
        // (`@armed` = recarm 1; themes resize/show elements per state).
        let bake = |state: &[(&str, f32)], label: &str| {
            let with_state = |mut env: Env| -> Env {
                for (k, v) in state {
                    env.set(k, *v);
                }
                env
            };
            let pass1 = evaluate(src, Some(&eval_name), &with_state(make_env(100.0, h0)));
            let w0s = pass1
                .get("mcpWidth")
                .and_then(|v| v.first().copied())
                .filter(|w| *w >= 24.0)
                .unwrap_or(w0);
            let out0 = evaluate(src, Some(&eval_name), &with_state(make_env(w0s, h0)));
            let mut env_w = with_state(make_env(w0s + dw, h0));
            // Bump every width knob this state can select.
            for suffix in ["", "Sel", "Recarm"] {
                let knob = format!("Layout{name}-mcpWidth{suffix}");
                if let Some(p) = rt.rtconfig.params.iter().find(|p| p.name == knob) {
                    env_w.set(&p.name, p.default + dw);
                }
            }
            let out_w = evaluate(src, Some(&eval_name), &env_w);
            let out_h = evaluate(src, Some(&eval_name), &with_state(make_env(w0s, h0 + dh)));
            layout_from_walter(label, w0s, h0, scale, &out0, &out_w, &out_h, rt)
        };

        let base = bake(&[], &name);
        let armed = bake(&[("recarm", 1.0)], &format!("{name}@armed"));
        let differs =
            armed.size != base.size || armed.recarm != base.recarm || armed.customs != base.customs;
        layouts.push(base);
        if differs {
            layouts.push(armed);
        }
    }
    layouts
}

/// Convert one evaluated layout into an [`McpLayout`].
#[allow(clippy::too_many_arguments)]
fn layout_from_walter(
    name: &str,
    w0: f32,
    h0: f32,
    scale: f32,
    out0: &daw_theme_reaper::walter::Output,
    out_w: &daw_theme_reaper::walter::Output,
    out_h: &daw_theme_reaper::walter::Output,
    rt: &ReaperTheme,
) -> super::mcp::McpLayout {
    use super::mcp::McpLayout;
    use super::walter::{Coord, FaderMode, Margin};

    const DW: f32 = 16.0;
    const DH: f32 = 32.0;

    // An element's anchor coord from the three evaluations.
    let coord = |attr: &str| -> Coord {
        let Some(c0) = out0.coord(attr) else {
            return Coord::hidden();
        };
        let (x, y, w, h) = (c0[0], c0[1], c0[2], c0[3]);
        if w <= 0.0 || h <= 0.0 {
            return Coord::hidden();
        }
        // Edge-attach scales via finite differences (fall back to the
        // natural-size values when a perturbed run hides the element).
        let cw = out_w.coord(attr).unwrap_or(c0);
        let ch = out_h.coord(attr).unwrap_or(c0);
        let ls = (cw[0] - x) / DW;
        let rs = ((cw[0] + cw[2]) - (x + w)) / DW;
        let ts = (ch[1] - y) / DH;
        let bs = ((ch[1] + ch[3]) - (y + h)) / DH;
        Coord::new(x, y, w, h, ls, ts, rs, bs)
    };

    // `*.margin` = [l t r b justify].
    let margin = |attr: &str, fallback: Margin| -> Margin {
        match out0.get(attr) {
            Some(v) => Margin::new(
                v.first().copied().unwrap_or(0.0),
                v.get(1).copied().unwrap_or(0.0),
                v.get(2).copied().unwrap_or(0.0),
                v.get(3).copied().unwrap_or(0.0),
                v.get(4).copied().unwrap_or(0.5),
            ),
            None => fallback,
        }
    };

    // Sliders read their orientation from the resolved geometry (REAPER's
    // `.fadermode` values are theme-convention; the box shape is ground truth).
    let fadermode = |c: &Coord| -> FaderMode {
        if c.w > c.h {
            FaderMode::Horizontal
        } else {
            FaderMode::Vertical
        }
    };

    // `*.color` = [fg r g b a, optional bg r g b a]. Alpha 0/absent → opaque.
    let color_at = |v: &[f32], i: usize| -> Option<Color> {
        let r = *v.get(i)? as u8;
        let g = v.get(i + 1).copied().unwrap_or(0.0) as u8;
        let b = v.get(i + 2).copied().unwrap_or(0.0) as u8;
        let a = match v.get(i + 3).copied() {
            Some(a) if a > 0.0 => a as u8,
            _ => 255,
        };
        Some(Color::rgba(r, g, b, a))
    };
    let color_pair = |attr: &str| -> Option<super::walter::ColorPair> {
        let v = out0.get(attr)?;
        let fg = color_at(v, 0)?;
        Some(super::walter::ColorPair {
            fg,
            bg: if v.len() >= 8 { color_at(v, 4) } else { None },
        })
    };

    // Per-layout colour overrides. Colours the theme derives from the track
    // colour come back as the sentinel (the bake env paints the track with
    // `Color::TRACK`) and resolve to the live accent at render time.
    let mut colors = super::mcp::McpColors::default();
    colors.label = color_pair("mcp.label.color");
    colors.trackidx = color_pair("mcp.trackidx.color");
    colors.volume_label = color_pair("mcp.volume.label.color");
    colors.pan_label = color_pair("mcp.pan.label.color");
    colors.meter_readout = color_pair("mcp.meter.readout.color");

    // Theme-drawn chrome: every `mcp.custom.*` box, in WALTER z-order.
    let customs: Vec<super::mcp::McpCustom> = out0
        .set_order
        .iter()
        .filter(|n| n.starts_with("mcp.custom.") && !n.ends_with(".color"))
        .filter_map(|n| {
            let c = coord(n);
            if c.is_hidden() {
                return None;
            }
            let pair = color_pair(&format!("{n}.color"));
            Some(super::mcp::McpCustom {
                name: n.clone(),
                coord: c,
                fg: pair.map(|p| p.fg),
                bg: pair.and_then(|p| p.bg),
            })
        })
        .collect();

    // `mcp.size` = [default w, default h, min w, min h].
    let size_attr = out0.coord("mcp.size");
    let min_h = size_attr
        .map(|s| s[3])
        .filter(|h| *h > 0.0)
        .or_else(|| rt.rtconfig.global_f32("mcp_min_height"))
        .unwrap_or(180.0);
    let mut base = McpLayout::vertical();
    // Fonts scale with the DPI variant (geometry already comes scaled from
    // the evaluation).
    for f in [
        &mut base.trackidx_font,
        &mut base.label_font,
        &mut base.volume_label_font,
        &mut base.pan_label_font,
    ] {
        f.size *= scale;
    }

    let volume = coord("mcp.volume");
    let pan = coord("mcp.pan");
    let width = coord("mcp.width");
    McpLayout {
        name: name.to_string(),
        size: (w0, h0),
        min_size: (24.0, min_h),
        margin: margin("mcp.margin", base.margin),

        trackidx: coord("mcp.trackidx"),
        trackidx_font: base.trackidx_font,
        trackidx_margin: margin("mcp.trackidx.margin", base.trackidx_margin),
        label: coord("mcp.label"),
        label_font: base.label_font,
        label_margin: margin("mcp.label.margin", base.label_margin),

        volume_fadermode: fadermode(&volume),
        volume,
        volume_label: coord("mcp.volume.label"),
        volume_label_font: base.volume_label_font,
        volume_label_margin: margin("mcp.volume.label.margin", base.volume_label_margin),
        pan_fadermode: fadermode(&pan),
        pan,
        pan_label: coord("mcp.pan.label"),
        pan_label_font: base.pan_label_font,
        pan_label_margin: margin("mcp.pan.label.margin", base.pan_label_margin),
        width_fadermode: fadermode(&width),
        width,

        meter: coord("mcp.meter"),

        width_label: coord("mcp.width.label"),
        width_label_margin: margin("mcp.width.label.margin", base.width_label_margin),

        mute: coord("mcp.mute"),
        solo: coord("mcp.solo"),
        recarm: coord("mcp.recarm"),
        phase: coord("mcp.phase"),
        fx: coord("mcp.fx"),
        fxbyp: coord("mcp.fxbyp"),
        io: coord("mcp.io"),
        env: coord("mcp.env"),
        folder: coord("mcp.folder"),

        recinput: coord("mcp.recinput"),
        recinput_margin: margin("mcp.recinput.margin", base.recinput_margin),
        recmode: coord("mcp.recmode"),
        recmon: coord("mcp.recmon"),
        fxin: coord("mcp.fxin"),

        customs,
        colors,
    }
}

/// Slice the theme's button/fader atlases into an [`McpSkin`] (data-URI PNGs).
///
/// Image lookup walks REAPER's fallback chain: a context image (`mcp_X`),
/// then the shared track vocabulary (`track_X`), then the general fallback
/// (`gen_X`) — the Anti-Theme, like the stock default, ships most strip
/// buttons as `track_*`/`gen_*`.
fn extract_skin(imgs: &ImageCatalog) -> Option<McpSkin> {
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
    // A knob filmstrip along the fallback chain (`mcp_X` → `tcp_X` → `gen_X`
    // — knob stacks commonly ship as `tcp_*` and are shared).
    let knob = |base: &str| -> Option<super::mcp::KnobSkin> {
        let name = ["mcp_", "tcp_", "gen_", ""]
            .iter()
            .map(|p| format!("{p}{base}"))
            .find(|n| imgs.has(n))?;
        let stack = imgs.knob_stack(&name).ok()?;
        Some(super::mcp::KnobSkin {
            url: ImageCatalog::data_uri(&stack.image),
            frames: stack.frames,
            frame_w: stack.frame_w,
            frame_h: stack.frame_h,
        })
    };

    let skin = McpSkin {
        mute: toggle("mute_off", "mute_on"),
        solo: toggle("solo_off", "solo_on"),
        recarm: toggle("recarm_off", "recarm_on"),
        io: button("io"),
        fx: toggle("fx_empty", "fx_norm"),
        fxbyp: toggle("fxoff_h", "fxon_h"),
        env: toggle("env", "env"),
        phase: toggle("phase_norm", "phase_inv"),
        recmode: toggle("recmode_off", "recmode_in"),
        folder: toggle("folder_off", "folder_on"),
        fxin: toggle("fx_in_empty", "fx_in_norm"),
        recinput_bg: plain("mcp_recinput"),
        pan_knob: knob("pan_knob_stack"),
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

    #[test]
    fn walter_layouts_drive_the_strip_geometry() {
        let Some(rt) = antitheme() else {
            eprintln!("anti-theme not found — skipping");
            return;
        };
        let theme = theme_from_reaper(&rt);

        // The theme's own layouts (A/B/C) lead; FTS fallbacks stay reachable.
        let a = theme.mcp.layout(None);
        assert_eq!(a.name, "A");
        assert_eq!(theme.mcp.layout(Some("vertical")).name, "vertical");

        // Layout A at its natural width (LayoutA-mcpWidth = 88): the REAPER 7
        // default geometry, with sensible edge-attach scales recovered.
        assert_eq!(a.size.0, 88.0);
        assert_eq!(
            (a.mute.x, a.mute.y, a.mute.w, a.mute.h),
            (62., 86., 20., 20.)
        );
        // The right-hand button column tracks the strip width 1:1.
        assert!((a.mute.ls - 1.0).abs() < 0.01, "mute.ls = {}", a.mute.ls);
        assert!((a.mute.rs - 1.0).abs() < 0.01);
        // The fader stretches with panel height (x = 30 at the natural
        // 88px width: meter block, gap, fader, button column).
        assert_eq!((a.volume.x, a.volume.w), (30., 25.));
        assert!(
            (a.volume.bs - 1.0).abs() < 0.01,
            "volume.bs = {}",
            a.volume.bs
        );
        assert!(a.volume.ts.abs() < 0.01);
        // Vertical fader, horizontal-ish pan box.
        use crate::theming::FaderMode;
        assert_eq!(a.volume_fadermode, FaderMode::Vertical);
        // Name bar pinned to the bottom, spanning the strip.
        assert!((a.label.ts - 1.0).abs() < 0.01 && (a.label.bs - 1.0).abs() < 0.01);
        assert_eq!(a.label.w, 88.0);
    }
}
