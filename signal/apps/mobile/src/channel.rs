//! Channel strip component — a single input channel (drums, vocals, guitar, etc.)
//!
//! Each channel has:
//! - Colour-coded label
//! - Peak level meter (VU-style)
//! - Fader (touch-draggable, 44pt minimum)
//! - Mute / Solo buttons
//! - Inline EQ/Comp indicators (tap to expand to a detail sheet)

use dioxus::prelude::*;

use crate::styles as s;

/// The type of input source on a channel.
#[derive(Clone, PartialEq, Debug)]
pub enum ChannelKind {
    Drums,
    Vocals,
    Guitar,
    Bass,
    Keys,
    Aux(String),
}

impl ChannelKind {
    pub fn label(&self) -> &str {
        match self {
            ChannelKind::Drums => "DRUMS",
            ChannelKind::Vocals => "VOCAL",
            ChannelKind::Guitar => "GTR",
            ChannelKind::Bass => "BASS",
            ChannelKind::Keys => "KEYS",
            ChannelKind::Aux(s) => s.as_str(),
        }
    }

    pub fn color(&self) -> &str {
        match self {
            ChannelKind::Drums => "#f59e0b",  // amber
            ChannelKind::Vocals => "#a78bfa", // violet
            ChannelKind::Guitar => "#34d399", // emerald
            ChannelKind::Bass => "#fb923c",   // orange
            ChannelKind::Keys => "#38bdf8",   // sky
            ChannelKind::Aux(_) => "#94a3b8", // slate
        }
    }
}

/// State for a single channel strip.
#[derive(Clone, PartialEq)]
pub struct ChannelState {
    pub kind: ChannelKind,
    pub fader_db: f32, // −∞ to +6 dB, 0 dB = unity
    pub peak_db: f32,  // live peak level (updated from audio thread)
    pub muted: bool,
    pub soloed: bool,
    pub eq_active: bool,
    pub comp_active: bool,
}

impl ChannelState {
    pub fn new(kind: ChannelKind) -> Self {
        Self {
            kind,
            fader_db: 0.0,
            peak_db: -60.0,
            muted: false,
            soloed: false,
            eq_active: false,
            comp_active: false,
        }
    }
}

// ── Peak meter helper ─────────────────────────────────────────────────────────

/// Map dB value (−60 … +6) to a 0…1 fill fraction with log scaling.
fn db_to_meter(db: f32) -> f32 {
    if db <= -60.0 {
        return 0.0;
    }
    // Log scale: −60 dB → 0.0, 0 dB → 0.75, +6 dB → 1.0
    let t = (db + 60.0) / 66.0;
    t.powf(0.5).clamp(0.0, 1.0)
}

fn meter_color(db: f32) -> &'static str {
    if db > 0.0 {
        s::DANGER
    } else if db > -6.0 {
        s::WARN
    } else {
        s::SUCCESS
    }
}

// ── Channel strip component ───────────────────────────────────────────────────

#[component]
pub fn ChannelStrip(
    state: ChannelState,
    /// Live gain reduction from the audio thread (positive = reducing).
    gr_db: f32,
    on_fader: EventHandler<f32>,
    on_mute: EventHandler<()>,
    on_solo: EventHandler<()>,
    on_tap: EventHandler<()>,
) -> Element {
    let color = state.kind.color();
    let label = state.kind.label().to_string();
    let fill = db_to_meter(state.peak_db);
    let meter_col = meter_color(state.peak_db);
    let muted = state.muted;
    let soloed = state.soloed;
    let eq_active = state.eq_active;
    let comp_active = state.comp_active;
    let fader_db = state.fader_db;

    // Fader fill 0…1 from −60…+6
    let fader_fill = ((fader_db + 60.0) / 66.0).clamp(0.0, 1.0);

    rsx! {
        div {
            onclick: move |_| on_tap.call(()),
            style: format!(
                "display:flex; flex-direction:column; align-items:center; gap:6px; \
                 width:72px; padding:10px 4px 8px 4px; \
                 background:{bg}; border-radius:{r}; \
                 border:1px solid {border}; position:relative; \
                 -webkit-tap-highlight-color: transparent; cursor:pointer;",
                bg = s::BG_PANEL,
                r = s::RADIUS_MD,
                border = s::BORDER_SUBTLE,
            ),

            // Colour accent bar at top
            div {
                style: format!(
                    "position:absolute; top:0; left:8px; right:8px; height:3px; \
                     border-radius:2px; background:{color};",
                ),
            }

            // Label
            div {
                style: format!(
                    "font-size:10px; font-weight:700; letter-spacing:0.08em; \
                     color:{color}; font-family:{font}; margin-top:4px;",
                    font = s::FONT,
                ),
                "{label}"
            }

            // Peak meter + fader side by side
            div {
                style: "display:flex; flex-direction:row; gap:4px; align-items:flex-end; \
                        height:140px; width:100%;",

                // Peak meter (narrow vertical bar)
                div {
                    style: "width:6px; height:140px; background:#222; border-radius:3px; \
                            overflow:hidden; position:relative; flex-shrink:0;",
                    div {
                        style: format!(
                            "position:absolute; bottom:0; left:0; right:0; \
                             height:{pct}%; background:{col}; transition:height 0.05s;",
                            pct = fill * 100.0,
                            col = meter_col,
                        ),
                    }
                }

                // Fader track
                div {
                    style: "flex:1; height:140px; background:#1a1a1e; border-radius:4px; \
                            overflow:hidden; position:relative;",
                    // Fader fill
                    div {
                        style: format!(
                            "position:absolute; bottom:0; left:0; right:0; \
                             height:{pct}%; background:{col}; opacity:0.6;",
                            pct = fader_fill * 100.0,
                            col = color,
                        ),
                    }
                    // Unity line at 0 dB (75% height)
                    div {
                        style: "position:absolute; bottom:75%; left:0; right:0; \
                                height:1px; background:#444;",
                    }
                    // Fader level label
                    div {
                        style: format!(
                            "position:absolute; bottom:2px; left:0; right:0; \
                             text-align:center; font-size:9px; color:{dim}; font-family:{font};",
                            dim = s::TEXT_DIM,
                            font = s::FONT_MONO,
                        ),
                        {
                            if fader_db <= -59.0 { "−∞".to_string() }
                            else { format!("{:+.0}", fader_db) }
                        }
                    }
                }
            }

            // Mute / Solo row
            div {
                style: "display:flex; flex-direction:row; gap:4px; width:100%;",

                // Mute
                button {
                    onclick: move |e| { e.stop_propagation(); on_mute.call(()); },
                    style: format!(
                        "flex:1; height:{h}; border-radius:{r}; border:none; \
                         font-size:9px; font-weight:700; font-family:{font}; \
                         background:{bg}; color:{col}; cursor:pointer;",
                        h = s::TOUCH_TARGET,
                        r = s::RADIUS_SM,
                        font = s::FONT,
                        bg = if muted { s::DANGER } else { s::BG_SURFACE },
                        col = if muted { s::TEXT_BRIGHT } else { s::TEXT_DIM },
                    ),
                    "M"
                }

                // Solo
                button {
                    onclick: move |e| { e.stop_propagation(); on_solo.call(()); },
                    style: format!(
                        "flex:1; height:{h}; border-radius:{r}; border:none; \
                         font-size:9px; font-weight:700; font-family:{font}; \
                         background:{bg}; color:{col}; cursor:pointer;",
                        h = s::TOUCH_TARGET,
                        r = s::RADIUS_SM,
                        font = s::FONT,
                        bg = if soloed { s::WARN } else { s::BG_SURFACE },
                        col = if soloed { "#000" } else { s::TEXT_DIM },
                    ),
                    "S"
                }
            }

            // FX indicators
            div {
                style: "display:flex; flex-direction:row; gap:3px;",
                FxBadge { label: "EQ", active: eq_active }
                FxBadge { label: "C", active: comp_active }
            }
        }
    }
}

/// Small badge indicating an FX stage is active/bypassed.
#[component]
fn FxBadge(label: &'static str, active: bool) -> Element {
    rsx! {
        div {
            style: format!(
                "padding:1px 4px; border-radius:3px; font-size:8px; font-weight:600; \
                 font-family:{font}; background:{bg}; color:{col};",
                font = s::FONT,
                bg = if active { s::ACCENT_DIM } else { s::BG_SURFACE },
                col = if active { s::TEXT_BRIGHT } else { s::TEXT_DIM },
            ),
            "{label}"
        }
    }
}
