//! The WALTER-themeable MCP strip.
//!
//! Replaces the flex-stacked `ChannelStrip` for the mixer console: every
//! element is positioned by the active [`McpLayout`]'s 8-value anchor coords
//! (`theme.mcp`), colours resolve through the per-element overrides
//! ([`McpColors`]) with token fallbacks, and `define_parameter`-style knobs
//! (`mcp_show_pan`, `mcp_strip_width`) bend the layout — so a REAPER
//! `Layout … EndLayout` block can drive this strip 1:1.
//!
//! Element ids rendered (REAPER vocabulary): `mcp.trackidx mcp.label
//! mcp.volume mcp.volume.label mcp.pan mcp.meter mcp.mute mcp.solo mcp.recarm
//! mcp.phase mcp.fx mcp.fxbyp mcp.io mcp.env mcp.folder`. Hidden elements are
//! zero-sized coords, the WALTER idiom.

use crate::core::sensitivity::{DragSensitivity, ModifierKeys};
use crate::panels::model::TrackView;
use crate::prelude::*;
use crate::theming::{Color, FaderMode, McpLayout, ThemeState, ToggleKind, use_theme};
use crate::widgets::knob::{Knob, KnobVariant};
use crate::widgets::mixer::RoutingButton;

/// One mixer strip, laid out by the theme's MCP context.
#[component]
pub fn McpStrip(
    track: TrackView,
    /// Named layout to use (`None` = the theme's first/default layout).
    #[props(default)]
    layout: Option<String>,
    #[props(default)] disabled: bool,
) -> Element {
    let theme = use_theme().theme;
    let mcp = theme.mcp.clone();
    let l = mcp.layout(layout.as_deref()).clone();
    let nat = l.size;

    let st = ThemeState::new().track(track.color.as_deref().and_then(Color::hex));
    let strip = theme.strip(&st);
    let accent = strip.accent;

    // `mcp_strip_width` param overrides the layout's natural width (0 = off).
    let strip_w = match mcp.param("mcp_strip_width") {
        Some(w) if w > 0.0 => w,
        _ => l.size.0,
    };
    let show_pan = mcp.param("mcp_show_pan") != Some(0.0);

    let opacity = if disabled { "0.5" } else { "1.0" };

    // ── element styles ──
    let label_fg = mcp.colors.label.map(|c| c.fg).unwrap_or(accent.on());
    let label_bg = mcp.colors.label.and_then(|c| c.bg).unwrap_or(accent);
    let vol_label_fg = mcp
        .colors
        .volume_label
        .map(|c| c.fg)
        .unwrap_or(strip.value_text);
    let idx_fg = mcp
        .colors
        .trackidx
        .map(|c| c.fg)
        .unwrap_or(theme.tokens.text_faint);

    let fader_accent = mcp.colors.volume.unwrap_or(accent);
    let pan_color = mcp.colors.pan; // Knob themes itself; reserved for importer use.
    let _ = pan_color;

    let display_pct = format!("{:.0}%", (track.fader)().clamp(0.0, 1.0) * 100.0);

    rsx! {
        div {
            style: format!(
                "position:relative; flex:0 0 auto; width:{strip_w}px; height:100%; \
                 min-width:{minw}px; min-height:{minh}px; overflow:hidden; \
                 margin-right:-1px; opacity:{opacity}; user-select:none; \
                 background:{body}; border:1px solid {border};",
                minw = l.min_size.0,
                minh = l.min_size.1,
                body = strip.body.css(),
                border = strip.border.css(),
            ),

            // mcp.trackidx
            if !l.trackidx.is_hidden() {
                div {
                    style: format!(
                        "{pos} display:flex; align-items:center; justify-content:{justify}; \
                         color:{fg}; font-size:{fs}px; font-weight:{fw}; pointer-events:none;",
                        pos = l.trackidx.css_position(nat),
                        justify = flex_justify(&l.trackidx_margin),
                        fg = idx_fg.css(),
                        fs = l.trackidx_font.size,
                        fw = l.trackidx_font.weight,
                    ),
                    "{track.id + 1}"
                }
            }

            // mcp.pan
            if !l.pan.is_hidden() && show_pan {
                div {
                    style: format!(
                        "{pos} display:flex; align-items:center; justify-content:center;",
                        pos = l.pan.css_position(nat),
                    ),
                    Knob {
                        value: track.pan,
                        min: 0.0,
                        max: 1.0,
                        default: 0.5,
                        variant: KnobVariant::ArcBipolar,
                        size: l.pan.w.min(l.pan.h) as u32,
                        disabled,
                    }
                }
            }

            // mcp.solo / mcp.mute / mcp.recarm
            if !l.solo.is_hidden() {
                McpToggle { pos: l.solo.css_position(nat), active: track.solo, kind: ToggleKind::Solo, disabled }
            }
            if !l.mute.is_hidden() {
                McpToggle { pos: l.mute.css_position(nat), active: track.mute, kind: ToggleKind::Mute, disabled }
            }
            if !l.recarm.is_hidden() {
                McpToggle { pos: l.recarm.css_position(nat), active: track.record_arm, kind: ToggleKind::RecArm, disabled }
            }

            // mcp.volume
            if !l.volume.is_hidden() {
                McpFader {
                    pos: l.volume.css_position(nat),
                    value: track.fader,
                    mode: l.volume_fadermode,
                    accent: fader_accent,
                    disabled,
                }
            }

            // mcp.meter
            if !l.meter.is_hidden() {
                McpMeter {
                    pos: l.meter.css_position(nat),
                    level: (track.level)(),
                    level_right: track.stereo.then(|| (track.level_right)()),
                    peak: (track.peak)(),
                }
            }

            // mcp.volume.label — fader readout.
            if !l.volume_label.is_hidden() {
                div {
                    style: format!(
                        "{pos} display:flex; align-items:center; justify-content:{justify}; \
                         color:{fg}; font-size:{fs}px; font-weight:{fw}; \
                         font-variant-numeric:tabular-nums; pointer-events:none;",
                        pos = l.volume_label.css_position(nat),
                        justify = flex_justify(&l.volume_label_margin),
                        fg = vol_label_fg.css(),
                        fs = l.volume_label_font.size,
                        fw = l.volume_label_font.weight,
                    ),
                    "{display_pct}"
                }
            }

            // mcp.io — routing.
            if !l.io.is_hidden() {
                div {
                    style: format!(
                        "{pos} display:flex; align-items:center; justify-content:center;",
                        pos = l.io.css_position(nat),
                    ),
                    RoutingButton {
                        parent_send: true,
                        sends: track.sends,
                        receives: track.receives,
                        disabled,
                        on_click: move |_| {},
                    }
                }
            }

            // mcp.phase / mcp.fx / mcp.fxbyp / mcp.env / mcp.folder — inert
            // until the view-model carries their state; themed + positionable.
            if !l.phase.is_hidden() {
                McpFlag { pos: l.phase.css_position(nat), glyph: "ø", title: "Phase" }
            }
            if !l.fx.is_hidden() {
                McpFlag { pos: l.fx.css_position(nat), glyph: "FX", title: "FX chain" }
            }
            if !l.fxbyp.is_hidden() {
                McpFlag { pos: l.fxbyp.css_position(nat), glyph: "BYP", title: "FX bypass" }
            }
            if !l.env.is_hidden() {
                McpFlag { pos: l.env.css_position(nat), glyph: "ENV", title: "Envelopes" }
            }
            if !l.folder.is_hidden() {
                McpFlag { pos: l.folder.css_position(nat), glyph: "▼", title: "Folder" }
            }

            // mcp.label — track-name footer.
            if !l.label.is_hidden() {
                div {
                    style: format!(
                        "{pos} display:flex; align-items:center; \
                         justify-content:{justify}; padding:{pad}; \
                         background:{bg}; color:{fg}; font-size:{fs}px; \
                         font-weight:{fw}; letter-spacing:0.02em; \
                         white-space:nowrap; overflow:hidden;",
                        pos = l.label.css_position(nat),
                        justify = flex_justify(&l.label_margin),
                        pad = l.label_margin.css_padding(),
                        bg = label_bg.css(),
                        fg = label_fg.css(),
                        fs = l.label_font.size,
                        fw = l.label_font.weight,
                    ),
                    "{track.name}"
                }
            }
        }
    }
}

/// Map a WALTER margin justification to flex `justify-content`.
fn flex_justify(m: &crate::theming::Margin) -> &'static str {
    match m.text_align() {
        "left" => "flex-start",
        "right" => "flex-end",
        _ => "center",
    }
}

// ── element widgets ───────────────────────────────────────────────────────────

/// A toggle button filling its anchor box (mute/solo/recarm).
#[component]
fn McpToggle(pos: String, active: Signal<bool>, kind: ToggleKind, disabled: bool) -> Element {
    let t = use_theme().theme.toggle(kind);
    let (glyph, title) = match kind {
        ToggleKind::Solo => ("S", "Solo"),
        ToggleKind::Mute => ("M", "Mute"),
        ToggleKind::RecArm => ("\u{25cf}", "Record arm"),
    };
    let on = active();
    let cursor = if disabled { "not-allowed" } else { "pointer" };
    let colors = if on {
        format!(
            "background:{fill}; color:{fg}; border:1px solid {fill}; \
             box-shadow:0 0 8px {glow}, inset 0 1px 0 rgba(255,255,255,0.3);",
            fill = t.on_fill.css(),
            fg = t.on_text.css(),
            glow = t.on_fill.with_alpha(0x99).css(),
        )
    } else {
        format!(
            "background:{bg}; color:{txt}; border:1px solid {border};",
            bg = t.off_bg.css(),
            txt = t.off_text.css(),
            border = t.off_border.css(),
        )
    };
    rsx! {
        button {
            r#type: "button",
            title,
            style: format!(
                "{pos} display:flex; align-items:center; justify-content:center; \
                 font-size:12px; font-weight:800; line-height:1; border-radius:5px; \
                 cursor:{cursor}; {colors}"
            ),
            onclick: move |_| {
                if disabled { return; }
                let next = !active();
                active.set(next);
            },
            "{glyph}"
        }
    }
}

/// An inert, themed flag button (phase/fx/fxbyp/env/folder) — positionable by
/// layouts today, wired to state when the view-model carries it.
#[component]
fn McpFlag(pos: String, glyph: &'static str, title: &'static str) -> Element {
    let tk = use_theme().theme.tokens;
    rsx! {
        div {
            title,
            style: format!(
                "{pos} display:flex; align-items:center; justify-content:center; \
                 font-size:9px; font-weight:800; border-radius:4px; \
                 background:{bg}; color:{fg}; border:1px solid {border};",
                bg = tk.surface_sunken.css(),
                fg = tk.text_faint.css(),
                border = tk.border.css(),
            ),
            "{glyph}"
        }
    }
}

/// The volume fader, vertical or horizontal per `mcp.volume.fadermode`.
#[component]
fn McpFader(
    pos: String,
    value: Signal<f32>,
    mode: FaderMode,
    accent: Color,
    disabled: bool,
) -> Element {
    let mut is_dragging = use_signal(|| false);
    let mut drag_start = use_signal(|| 0.0f32);
    let mut drag_start_value = use_signal(|| 0.0f32);

    let sensitivity = DragSensitivity::new(180.0, 0.1);
    let v = value().clamp(0.0, 1.0);
    let fill_pct = v * 100.0;
    let cursor = if disabled {
        "not-allowed"
    } else if mode == FaderMode::Horizontal {
        "ew-resize"
    } else {
        "ns-resize"
    };

    let theme = use_theme().theme;
    let f = theme.fader(&ThemeState::new());
    let cap = match mode {
        FaderMode::Vertical => format!(
            "position:absolute; left:-3px; right:-3px; bottom:calc({fill_pct}% - 6px); \
             height:12px; border-radius:3px; \
             background:linear-gradient(180deg,{top},{bottom}); \
             border:1px solid #15151a; pointer-events:none; \
             box-shadow:0 1px 2px rgba(0,0,0,0.6);",
            top = f.cap_top.css(),
            bottom = f.cap_bottom.css(),
        ),
        FaderMode::Horizontal => format!(
            "position:absolute; top:-3px; bottom:-3px; left:calc({fill_pct}% - 6px); \
             width:12px; border-radius:3px; \
             background:linear-gradient(90deg,{top},{bottom}); \
             border:1px solid #15151a; pointer-events:none; \
             box-shadow:0 1px 2px rgba(0,0,0,0.6);",
            top = f.cap_top.css(),
            bottom = f.cap_bottom.css(),
        ),
    };
    let cap_line = match mode {
        FaderMode::Vertical => format!(
            "position:absolute; left:3px; right:3px; top:5px; height:2px; \
             background:{}; border-radius:1px;",
            accent.css()
        ),
        FaderMode::Horizontal => format!(
            "position:absolute; top:3px; bottom:3px; left:5px; width:2px; \
             background:{}; border-radius:1px;",
            accent.css()
        ),
    };

    rsx! {
        div {
            style: format!(
                "{pos} background:{well}; border-radius:5px; border:1px solid {border}; \
                 cursor:{cursor}; box-shadow:inset 0 1px 3px rgba(0,0,0,0.5);",
                well = f.well.css(),
                border = f.border.css(),
            ),
            div { style: cap, div { style: cap_line } }

            // Drag overlay.
            div {
                style: "position:absolute; inset:0;",
                onmousedown: move |evt: MouseEvent| {
                    if disabled { return; }
                    is_dragging.set(true);
                    let p = evt.client_coordinates();
                    drag_start.set(if mode == FaderMode::Horizontal { p.x as f32 } else { p.y as f32 });
                    drag_start_value.set(value().clamp(0.0, 1.0));
                },
                onmousemove: move |evt: MouseEvent| {
                    if !*is_dragging.read() || disabled { return; }
                    let p = evt.client_coordinates();
                    let delta_px = match mode {
                        // Up = increase.
                        FaderMode::Vertical => drag_start() - p.y as f32,
                        // Right = increase.
                        FaderMode::Horizontal => p.x as f32 - drag_start(),
                    };
                    let modifiers = ModifierKeys::new(
                        evt.modifiers().shift(), evt.modifiers().ctrl(), evt.modifiers().alt(),
                    );
                    let delta = sensitivity.calculate_delta(delta_px, modifiers);
                    value.set((drag_start_value() + delta).clamp(0.0, 1.0));
                },
                onmouseup: move |_| { is_dragging.set(false); },
                onmouseleave: move |_| { is_dragging.set(false); },
            }
        }
    }
}

/// The level meter. Fill colour: `mcp.meter.scale.color.lit.*` gradient when
/// the theme pins it, else the token zone colours; well from `…unlit.*` or the
/// meter style.
#[component]
fn McpMeter(pos: String, level: f32, level_right: Option<f32>, peak: f32) -> Element {
    let theme = use_theme().theme;
    let m = theme.meter();
    let c = theme.mcp.colors;

    let well = match (c.meter_unlit_top, c.meter_unlit_bottom) {
        (Some(top), Some(bottom)) => {
            format!("linear-gradient(180deg,{},{})", top.css(), bottom.css())
        }
        _ => m.well.css(),
    };

    // One meter column. Zero/non-finite renders nothing (vello NaN guard).
    let column = |lvl: f32, left: f32, w: f32| {
        let lvl = if lvl.is_finite() {
            lvl.clamp(0.0, 1.0)
        } else {
            0.0
        };
        let fill = match (c.meter_lit_top, c.meter_lit_bottom) {
            (Some(top), Some(bottom)) => format!(
                "background:linear-gradient(180deg,{},{});",
                top.css(),
                bottom.css()
            ),
            _ => format!("background:{};", theme.meter_zone(lvl).css()),
        };
        rsx! {
            if lvl > 0.0 {
                div {
                    style: format!(
                        "position:absolute; bottom:0; left:{left}%; width:{w}%; height:{h}%; \
                         {fill} opacity:0.9; pointer-events:none; border-radius:1px;",
                        h = lvl * 100.0,
                    ),
                }
            }
        }
    };

    let peak = if peak.is_finite() {
        peak.clamp(0.0, 1.0)
    } else {
        0.0
    };

    rsx! {
        div {
            style: format!(
                "{pos} background:{well}; border-radius:3px; overflow:hidden; \
                 border:1px solid {border};",
                border = m.border.css(),
            ),
            if let Some(right) = level_right {
                {column(level, 6.0, 41.0)}
                {column(right, 53.0, 41.0)}
            } else {
                {column(level, 10.0, 80.0)}
            }
            if peak > 0.0 {
                div {
                    style: format!(
                        "position:absolute; left:0; right:0; bottom:{p}%; height:2px; \
                         background:{col}; pointer-events:none;",
                        p = peak * 100.0,
                        col = m.peak.css(),
                    ),
                }
            }
        }
    }
}
