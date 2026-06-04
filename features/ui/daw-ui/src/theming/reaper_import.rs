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

use super::mcp::{ButtonSkin, ButtonStateSkin, LayoutEngine, McpSkin, SkinImage};
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
    mcp.skin = extract_skin(&imgs, "mcp");

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
    let walter_layouts = walter_strip_layouts(rt, scale, dpi_folder.as_deref(), "mcp");
    let has_walter_mcp = !walter_layouts.is_empty();
    if has_walter_mcp {
        let fallbacks = std::mem::take(&mut mcp.layouts);
        mcp.layouts = walter_layouts;
        mcp.layouts.extend(fallbacks);
    }

    // ── TCP context ── (same vocabulary, laid out as a track-control row)
    let tcp = &mut theme.tcp;
    tcp.colors.meter_lit_top = pal("col_vutop");
    tcp.colors.meter_lit_bottom = pal("col_vubot");
    if let Some(zl) = rt.rtconfig.global_color("tcp_vol_zeroline") {
        tcp.colors.volume = Some(color(zl));
    }
    tcp.skin = extract_skin(&imgs, "tcp");
    let tcp_layouts = walter_strip_layouts(rt, scale, dpi_folder.as_deref(), "tcp");
    let has_walter_tcp = !tcp_layouts.is_empty();
    if has_walter_tcp {
        let fallbacks = std::mem::take(&mut tcp.layouts);
        tcp.layouts = tcp_layouts;
        tcp.layouts.extend(fallbacks);
    }

    // ── runtime layout engine ──
    // Renderers that know their actual px box (TCP rows) re-evaluate the
    // theme at that size — REAPER's resize model — instead of springing the
    // baked anchors. Only worth installing when the theme has WALTER layouts.
    if has_walter_tcp || has_walter_mcp {
        theme.engine = Some(make_layout_engine(rt, scale, dpi_folder.clone()));
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

/// Build the runtime [`LayoutEngine`]: re-evaluates the theme's WALTER at an
/// **exact panel size**, the way REAPER does on every resize. Flow-based
/// themes (Reapertips' `then`-macro chain) wrap elements to the next row,
/// shrink label/volume toward minimums and cull what doesn't fit — all
/// functions of the actual `(w, h)` that a one-shot anchor bake cannot
/// reproduce. Evaluated at the real size, the anchors collapse to zero and
/// the geometry is exact; results are memoized per `(ctx, layout, w, h,
/// armed)`.
fn make_layout_engine(rt: &ReaperTheme, scale: f32, dpi_folder: Option<String>) -> LayoutEngine {
    use daw_theme_reaper::walter::{Env, evaluate};
    use std::collections::HashMap;
    use std::sync::Mutex;

    let src = rt.rtconfig_src.clone();
    let rtc = rt.rtconfig.clone();
    // Layout names, probed once (DPI variants resolve per `dpi_folder`).
    let probe = evaluate(&src, None, &Env::reaper_defaults(100.0, 100.0));
    let layout_names = probe.layouts;

    type Key = (String, String, u32, u32, bool);
    let cache: Mutex<HashMap<Key, Option<super::mcp::McpLayout>>> = Mutex::new(HashMap::new());

    LayoutEngine(std::sync::Arc::new(move |ctx, name, w, h, armed| {
        let key = (ctx.to_string(), name.to_string(), w as u32, h as u32, armed);
        if let Some(hit) = cache.lock().unwrap().get(&key) {
            return hit.clone();
        }

        // Unknown layout name (an FTS fallback layout's) → no WALTER run.
        if !layout_names.iter().any(|l| l == name) {
            cache.lock().unwrap().insert(key, None);
            return None;
        }

        // `Layout "150%_A" "150"` is layout A at another Scale.
        let eval_name = match &dpi_folder {
            Some(folder) => {
                let v = format!("{folder}%_{name}");
                if layout_names.iter().any(|l| l == &v) {
                    v
                } else {
                    name.to_string()
                }
            }
            None => name.to_string(),
        };

        let mut env = Env::reaper_defaults(w, h);
        env.set("Scale", scale);
        env.set("trackcolor_valid", 1.0);
        env.set("trackcolor_r", Color::TRACK.r as f32);
        env.set("trackcolor_g", Color::TRACK.g as f32);
        env.set("trackcolor_b", Color::TRACK.b as f32);
        for p in &rtc.params {
            env.set(&p.name, p.default);
        }
        if armed {
            env.set("recarm", 1.0);
        }

        let out = evaluate(&src, Some(&eval_name), &env);
        // A context the theme doesn't program (no `{ctx}.` attrs) → None.
        let prefix = format!("{ctx}.");
        let layout = out.attrs.keys().any(|k| k.starts_with(&prefix)).then(|| {
            // Same output for all three passes → zero attach scales:
            // positions are already exact for this size (re-evaluation
            // replaces springing).
            layout_from_walter(name, w, h, scale, &out, &out, &out, &rtc, ctx)
        });
        cache.lock().unwrap().insert(key, layout.clone());
        layout
    }))
}

/// Evaluate the theme's WALTER program per named layout and convert the
/// resolved `{ctx}.*` attributes into [`McpLayout`]s (`ctx` = `"mcp"` for
/// mixer strips, `"tcp"` for track-control rows — the same vocabulary).
///
/// The 8-value anchor model is recovered by **finite differences**: WALTER
/// output is piecewise-linear in the panel size within a form, so evaluating
/// at the natural size and at `+Δw`/`+Δh` yields each element's edge-attach
/// scales exactly. Natural size comes from `{ctx}.size` (two-pass: probe,
/// then evaluate at that size); the Anti-Theme's `mcpWidth` variable is the
/// MCP fallback.
fn walter_strip_layouts(
    rt: &ReaperTheme,
    scale: f32,
    dpi_folder: Option<&str>,
    ctx: &str,
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

    // Probe panel height: a mixer strip is tall, a track row is short.
    let h0 = if ctx == "tcp" { 100.0 } else { 600.0 } * scale;
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
        // Pass 1: the layout's natural size. `{ctx}.size` ([default w,
        // default h, min w, min h]) is REAPER's mechanism and holds across
        // themes (Anti/Reapertips/Neptune/Imperial); the Anti-Theme's
        // `mcpWidth` variable is the MCP fallback.
        let pass1 = evaluate(src, Some(&eval_name), &make_env(100.0, h0));
        let size = pass1.coord(&format!("{ctx}.size"));
        let w0 = match size.map(|s| s[0]).filter(|w| *w >= 24.0).or_else(|| {
            (ctx == "mcp")
                .then(|| pass1.get("mcpWidth").and_then(|v| v.first().copied()))
                .flatten()
        }) {
            Some(w) if w >= 24.0 => w,
            _ => continue,
        };
        // A strip is at least ~100px tall; a track row can be much shorter.
        let min_nat_h = if ctx == "tcp" { 16.0 } else { 100.0 };
        let h0 = size.map(|s| s[1]).filter(|h| *h >= min_nat_h).unwrap_or(h0);

        // Pass 2: natural + finite-difference evaluations. MCP strip width is
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
            let w0s = (ctx == "mcp")
                .then(|| pass1.get("mcpWidth").and_then(|v| v.first().copied()))
                .flatten()
                .filter(|w| *w >= 24.0)
                .unwrap_or(w0);
            let out0 = evaluate(src, Some(&eval_name), &with_state(make_env(w0s, h0)));
            let mut env_w = with_state(make_env(w0s + dw, h0));
            // Bump every width knob this state can select.
            if ctx == "mcp" {
                for suffix in ["", "Sel", "Recarm"] {
                    let knob = format!("Layout{name}-mcpWidth{suffix}");
                    if let Some(p) = rt.rtconfig.params.iter().find(|p| p.name == knob) {
                        env_w.set(&p.name, p.default + dw);
                    }
                }
            }
            let out_w = evaluate(src, Some(&eval_name), &env_w);
            let out_h = evaluate(src, Some(&eval_name), &with_state(make_env(w0s, h0 + dh)));
            layout_from_walter(
                label,
                w0s,
                h0,
                scale,
                &out0,
                &out_w,
                &out_h,
                &rt.rtconfig,
                ctx,
            )
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
    rtc: &daw_theme_reaper::RtConfig,
    ctx: &str,
) -> super::mcp::McpLayout {
    use super::mcp::McpLayout;
    use super::walter::{Coord, FaderMode, Margin};

    const DW: f32 = 16.0;
    const DH: f32 = 32.0;

    // Context-qualified attribute name (`mcp.volume` / `tcp.volume`).
    let at = |el: &str| format!("{ctx}.{el}");

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

    // `*.fadermode` (SDK): first coordinate `1` forces a knob, `-1` prevents
    // one, `0` is REAPER's default. Slider orientation comes from the
    // resolved box shape (ground truth).
    let fadermode = |c: &Coord, attr: &str| -> FaderMode {
        let mode = out0
            .get(attr)
            .and_then(|v| v.first().copied())
            .unwrap_or(0.0);
        if mode > 0.5 {
            FaderMode::Knob
        } else if c.w > c.h {
            FaderMode::Horizontal
        } else {
            FaderMode::Vertical
        }
    };

    // `*.color` = [fg r g b a, optional bg r g b a]. WALTER lists pad with
    // zeros, so a 7-long value still carries a bg (its alpha is implied
    // opaque); an *explicit* alpha 0 means transparent.
    let color_at = |v: &[f32], i: usize| -> Option<Color> {
        let r = *v.get(i)? as u8;
        let g = v.get(i + 1).copied().unwrap_or(0.0) as u8;
        let b = v.get(i + 2).copied().unwrap_or(0.0) as u8;
        let a = match v.get(i + 3).copied() {
            Some(a) => a as u8,
            None => 255,
        };
        Some(Color::rgba(r, g, b, a))
    };
    let color_pair = |attr: &str| -> Option<super::walter::ColorPair> {
        let v = out0.get(attr)?;
        let fg = color_at(v, 0)?;
        Some(super::walter::ColorPair {
            fg,
            bg: color_at(v, 4),
        })
    };

    // Per-layout colour overrides. Colours the theme derives from the track
    // colour come back as the sentinel (the bake env paints the track with
    // `Color::TRACK`) and resolve to the live accent at render time.
    let mut colors = super::mcp::McpColors::default();
    colors.label = color_pair(&at("label.color"));
    colors.trackidx = color_pair(&at("trackidx.color"));
    colors.volume_label = color_pair(&at("volume.label.color"));
    colors.pan_label = color_pair(&at("pan.label.color"));
    colors.meter_readout = color_pair(&at("meter.readout.color"));

    // Theme-drawn chrome: every `{ctx}.custom.*` box.
    //
    // Z-order (verified against the Anti-Theme in REAPER 7): custom elements
    // stack in **reverse declaration order** — the last-declared custom
    // (`tcp.custom.tcpBgBox`, the full-panel background) draws at the
    // bottom, the first-declared (`tcpDiv`, the hairline divider) on top.
    // `front` statements then lift named elements above everything (we apply
    // the ones naming customs; standard elements render above customs
    // anyway). First occurrence dedupes `clear` + re-set repeats.
    let custom_prefix = at("custom.");
    let mut names: Vec<&String> = Vec::new();
    for n in &out0.set_order {
        if n.starts_with(&custom_prefix) && !n.ends_with(".color") && !names.contains(&n) {
            names.push(n);
        }
    }
    names.reverse();
    for f in &out0.fronts {
        if let Some(pos) = names.iter().position(|n| *n == f) {
            let n = names.remove(pos);
            names.push(n);
        }
    }
    let customs: Vec<super::mcp::McpCustom> = names
        .into_iter()
        .filter_map(|n| {
            let c = coord(n);
            if c.is_hidden() {
                return None;
            }
            // A fill colour is usable only with nonzero alpha (themes write
            // `[0 0 0 0 …]` for a transparent fg + bg-only box).
            let usable = |c: Color| (c.a > 0).then_some(c);
            let pair = color_pair(&format!("{n}.color"));
            Some(super::mcp::McpCustom {
                name: n.clone(),
                coord: c,
                fg: pair.map(|p| p.fg).and_then(usable),
                bg: pair.and_then(|p| p.bg).and_then(usable),
            })
        })
        .collect();

    // `{ctx}.size` = [default w, default h, min w, min h].
    let size_attr = out0.coord(&at("size"));
    let min_w = size_attr.map(|s| s[2]).filter(|w| *w > 0.0).unwrap_or(24.0);
    let min_h = size_attr
        .map(|s| s[3])
        .filter(|h| *h > 0.0)
        .or_else(|| rtc.global_f32(&format!("{ctx}_min_height")))
        .unwrap_or(if ctx == "tcp" { 24.0 } else { 180.0 });
    let mut base = if ctx == "tcp" {
        McpLayout::tcp_row()
    } else {
        McpLayout::vertical()
    };
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

    let volume = coord(&at("volume"));
    let pan = coord(&at("pan"));
    let width = coord(&at("width"));
    McpLayout {
        name: name.to_string(),
        size: (w0, h0),
        min_size: (min_w, min_h),
        margin: margin(&at("margin"), base.margin),

        trackidx: coord(&at("trackidx")),
        trackidx_font: base.trackidx_font,
        trackidx_margin: margin(&at("trackidx.margin"), base.trackidx_margin),
        label: coord(&at("label")),
        label_font: base.label_font,
        label_margin: margin(&at("label.margin"), base.label_margin),

        volume_fadermode: fadermode(&volume, &at("volume.fadermode")),
        volume,
        volume_label: coord(&at("volume.label")),
        volume_label_font: base.volume_label_font,
        volume_label_margin: margin(&at("volume.label.margin"), base.volume_label_margin),
        pan_fadermode: fadermode(&pan, &at("pan.fadermode")),
        pan,
        pan_label: coord(&at("pan.label")),
        pan_label_font: base.pan_label_font,
        pan_label_margin: margin(&at("pan.label.margin"), base.pan_label_margin),
        width_fadermode: fadermode(&width, &at("width.fadermode")),
        width,

        meter: coord(&at("meter")),

        width_label: coord(&at("width.label")),
        width_label_margin: margin(&at("width.label.margin"), base.width_label_margin),

        mute: coord(&at("mute")),
        solo: coord(&at("solo")),
        recarm: coord(&at("recarm")),
        phase: coord(&at("phase")),
        fx: coord(&at("fx")),
        fxbyp: coord(&at("fxbyp")),
        io: coord(&at("io")),
        env: coord(&at("env")),
        folder: coord(&at("folder")),

        recinput: coord(&at("recinput")),
        recinput_margin: margin(&at("recinput.margin"), base.recinput_margin),
        recmode: coord(&at("recmode")),
        recmon: coord(&at("recmon")),
        fxin: coord(&at("fxin")),

        customs,
        colors,
    }
}

/// Slice the theme's button/fader atlases into an [`McpSkin`] (data-URI PNGs).
///
/// Image lookup walks REAPER's fallback chain: a context image (`{ctx}_X` —
/// `mcp_X`/`tcp_X`), then the shared track vocabulary (`track_X`), then the
/// general fallback (`gen_X`) — the Anti-Theme, like the stock default, ships
/// most strip buttons as `track_*`/`gen_*`.
fn extract_skin(imgs: &ImageCatalog, ctx: &str) -> Option<McpSkin> {
    let ctx_prefix = format!("{ctx}_");
    // First catalog name present along the fallback chain.
    let find = |base: &str| -> Option<String> {
        [ctx_prefix.as_str(), "track_", "gen_"]
            .iter()
            .map(|p| format!("{p}{base}"))
            .find(|n| imgs.has(n))
    };

    // Build a SkinImage, pre-slicing a 9-patch grid when the art carries
    // pink fixed margins (the "Pink Line Crush" technique: fixed margins
    // render 1:1, only the unmarked bands stretch/crush).
    let skin_image = |img: &daw_theme_reaper::image::RgbaImage,
                      markers: &daw_theme_reaper::images::Markers| {
        use daw_theme_reaper::image::GenericImageView;
        let (w, h) = img.dimensions();
        let plain_img = |i: daw_theme_reaper::image::RgbaImage| SkinImage {
            url: ImageCatalog::data_uri(&i),
            w: i.width(),
            h: i.height(),
            slices: None,
        };
        // Per-axis margins, dropped when degenerate (margins >= dimension).
        let (ml, mr) = if markers.fixed_left + markers.fixed_right < w {
            (markers.fixed_left, markers.fixed_right)
        } else {
            (0, 0)
        };
        let (mt, mb) = if markers.fixed_top + markers.fixed_bottom < h {
            (markers.fixed_top, markers.fixed_bottom)
        } else {
            (0, 0)
        };
        let slices = (ml + mr + mt + mb > 0).then(|| {
            let xs = [
                (0, ml.max(1)),
                (ml, w - ml - mr),
                (w - mr.max(1), mr.max(1)),
            ];
            let ys = [
                (0, mt.max(1)),
                (mt, h - mt - mb),
                (h - mb.max(1), mb.max(1)),
            ];
            let patches = ys
                .iter()
                .flat_map(|&(y, ph)| xs.iter().map(move |&(x, pw)| (x, y, pw, ph)))
                .map(|(x, y, pw, ph)| plain_img(img.view(x, y, pw, ph).to_image()))
                .collect();
            Box::new(super::mcp::NineSlice {
                l: ml,
                t: mt,
                r: mr,
                b: mb,
                patches,
            })
        });
        SkinImage {
            url: ImageCatalog::data_uri(img),
            w,
            h,
            slices,
        }
    };

    // All three interaction states of a 3-slice button, with the `*_ol`
    // overlay composited per state — `use_overlays 1` themes (incl. the
    // default) keep the base states transparent and ship the visible art in
    // the overlay. Stretch margins come from the *parent* image (the guide:
    // overlays belong to a parent).
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
            skin_image(&img, &s.markers)
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
    // A plain (marker-stripped) image, banded when pink-marked.
    let plain = |name: &str| -> Option<SkinImage> {
        let s = imgs.load(name).ok()?;
        Some(skin_image(&s.image, &s.markers))
    };
    // A knob filmstrip along the fallback chain (context first, then the
    // other strip context, then general — knob stacks commonly ship as
    // `tcp_*` and are shared).
    let knob = |base: &str| -> Option<super::mcp::KnobSkin> {
        let name = [ctx_prefix.as_str(), "tcp_", "mcp_", "gen_", ""]
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
    // Context-owned (non-falling-back) images: `mcp_volbg`, `tcp_panbg`, …
    let ctx_plain = |base: &str| plain(&format!("{ctx_prefix}{base}"));

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
        recinput_bg: ctx_plain("recinput"),
        pan_knob: knob("pan_knob_stack"),
        vol_knob: knob("vol_knob_stack"),
        volbg: ctx_plain("volbg"),
        volthumb: ctx_plain("volthumb"),
        panbg: ctx_plain("panbg"),
        panthumb: ctx_plain("panthumb"),
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

#[cfg(test)]
mod tcp_tests {
    use super::*;
    use crate::theming::FaderMode;

    fn antitheme() -> Option<ReaperTheme> {
        let dir = std::env::var("REAPER_ANTITHEME_DIR").unwrap_or_else(|_| {
            "/home/cody/Development/FastTrackStudio/reaper-theme/extracted/antitheme".to_string()
        });
        ReaperTheme::load_dir(&dir).ok()
    }

    #[test]
    fn tcp_context_imports_from_walter() {
        let Some(rt) = antitheme() else {
            eprintln!("anti-theme not found — skipping");
            return;
        };
        let theme = theme_from_reaper(&rt);

        // The theme's TCP layouts lead; natural size from `tcp.size`.
        let a = theme.tcp.layout(None);
        assert_eq!(a.name, "A");
        assert_eq!(a.size, (300.0, 100.0));
        assert_eq!(a.min_size.0, 212.0);

        // REAPER 7 default TCP: volume is a knob (`tcp.volume.fadermode 1`).
        assert_eq!(a.volume_fadermode, FaderMode::Knob);
        assert!(!a.volume.is_hidden());
        assert!(!a.label.is_hidden());
        assert!(!a.mute.is_hidden());
        assert!(!a.meter.is_hidden());

        // TCP skin: volume knob stack + the tcp fader art.
        let skin = theme.tcp.skin.as_ref().expect("tcp skin");
        assert!(skin.vol_knob.is_some());
        assert!(skin.pan_knob.is_some());
    }

    #[test]
    fn engine_reevaluates_at_exact_size() {
        let Some(rt) = antitheme() else {
            eprintln!("anti-theme not found — skipping");
            return;
        };
        let theme = theme_from_reaper(&rt);
        let engine = theme.engine.as_ref().expect("walter engine installed");

        // Evaluated at the actual row box: geometry is exact px (no anchor
        // springing — attach scales are zero) and stays inside the box.
        let l = engine
            .layout_at("tcp", "A", 260.0, 64.0, false)
            .expect("tcp layout");
        assert_eq!(l.size, (260.0, 64.0));
        for (name, c) in [("label", &l.label), ("mute", &l.mute), ("meter", &l.meter)] {
            assert!(!c.is_hidden(), "{name} hidden at 260x64");
            assert!(
                c.x >= -1.0 && c.x + c.w <= 261.0,
                "{name} out of bounds: x={} w={}",
                c.x,
                c.w
            );
            assert_eq!((c.ls, c.ts, c.rs, c.bs), (0.0, 0.0, 0.0, 0.0));
        }

        // Unknown layout name → None (callers fall back to bakes).
        assert!(engine.layout_at("tcp", "row", 260.0, 64.0, false).is_none());
    }

    /// Reapertips' TCP is a WALTER flow engine (`then`-macro chain): elements
    /// wrap/shrink/cull per the actual width. Re-evaluating at two widths
    /// must yield different geometry — the behavior anchors can't reproduce.
    #[test]
    fn reapertips_flow_responds_to_width() {
        let dir = "/home/cody/Development/FastTrackStudio/reaper-theme/extracted/reapertips/_reapertips_theme";
        let Ok(rt) = ReaperTheme::load_dir(dir) else {
            eprintln!("reapertips not found — skipping");
            return;
        };
        let theme = theme_from_reaper(&rt);
        let engine = theme.engine.as_ref().expect("walter engine installed");

        let wide = engine
            .layout_at("tcp", "A", 420.0, 60.0, false)
            .expect("wide tcp layout");
        let narrow = engine
            .layout_at("tcp", "A", 150.0, 60.0, false)
            .expect("narrow tcp layout");

        // The label survives at both widths but the flow repacks the row.
        assert!(!wide.label.is_hidden());
        assert!(!narrow.label.is_hidden());
        assert!(
            wide.label.w != narrow.label.w || wide.volume != narrow.volume,
            "flow theme should repack elements with width"
        );
        // Wide rows fit more flow elements than narrow ones.
        let visible = |l: &crate::theming::McpLayout| {
            [&l.recarm, &l.label, &l.volume, &l.pan, &l.io, &l.fx, &l.env]
                .iter()
                .filter(|c| !c.is_hidden())
                .count()
        };
        assert!(
            visible(&wide) >= visible(&narrow),
            "wide {} < narrow {}",
            visible(&wide),
            visible(&narrow)
        );
    }
}

#[cfg(test)]
mod nine_slice_tests {
    use super::*;

    #[test]
    fn io_slices_into_patches() {
        let dir = std::env::var("REAPER_ANTITHEME_DIR").unwrap_or_else(|_| {
            "/home/cody/Development/FastTrackStudio/reaper-theme/extracted/antitheme".to_string()
        });
        let Ok(rt) = ReaperTheme::load_dir(&dir) else {
            eprintln!("anti-theme not found — skipping");
            return;
        };
        let theme = theme_from_reaper(&rt);
        let skin = theme.mcp.skin.as_ref().unwrap();
        let io = skin.io.as_ref().expect("io skin");
        let n = io.normal.slices.as_ref().expect("io has pink margins");
        assert_eq!((n.l, n.t, n.r, n.b), (0, 23, 0, 0));
        assert_eq!(n.patches.len(), 9);
        // top-middle patch = the fixed icon cap.
        assert_eq!(n.patches[1].h, 23);
        assert!(n.patches[1].w > 0);
        // centre patch = the stretch band.
        assert_eq!(n.patches[4].h, io.normal.h - 23);
    }
}
