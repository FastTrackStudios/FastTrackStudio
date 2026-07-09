//! Mixer view — horizontally scrollable channel strips + channel detail panel.

use dioxus::prelude::*;

use signal_ui::ProcessingChain;

use crate::channel::{ChannelKind, ChannelState, ChannelStrip};
use crate::styles as s;

#[component]
pub fn MixerView(
    chain: ProcessingChain,
    channels: Signal<Vec<ChannelState>>,
    active_channel: Signal<Option<usize>>,
) -> Element {
    // Poll GR from the audio thread (~60 Hz via Dioxus re-render).
    let gr = chain.gain_reduction_db();

    rsx! {
        div {
            style: format!(
                "flex:1; display:flex; flex-direction:column; min-height:0; background:{bg};",
                bg = s::BG_APP,
            ),

            // Channel strip row
            div {
                style: "flex:1; display:flex; flex-direction:row; gap:8px; \
                        padding:12px 16px; overflow-x:auto; align-items:flex-start; \
                        -webkit-overflow-scrolling:touch;",

                for (idx, ch) in channels.read().iter().enumerate() {
                    ChannelStrip {
                        key: "{idx}",
                        state: ch.clone(),
                        gr_db: if idx == 0 { gr } else { 0.0 },
                        on_fader: {
                            let mut channels = channels.clone();
                            move |v: f32| {
                                if let Some(c) = channels.write().get_mut(idx) { c.fader_db = v; }
                            }
                        },
                        on_mute: {
                            let mut channels = channels.clone();
                            move |_| {
                                if let Some(c) = channels.write().get_mut(idx) { c.muted = !c.muted; }
                            }
                        },
                        on_solo: {
                            let mut channels = channels.clone();
                            move |_| {
                                if let Some(c) = channels.write().get_mut(idx) { c.soloed = !c.soloed; }
                            }
                        },
                        on_tap: {
                            let mut active_channel = active_channel.clone();
                            move |_| {
                                let cur = *active_channel.read();
                                *active_channel.write() = if cur == Some(idx) { None } else { Some(idx) };
                            }
                        },
                    }
                }

                AddChannelButton {
                    on_add: {
                        let mut channels = channels.clone();
                        move |kind: ChannelKind| { channels.write().push(ChannelState::new(kind)); }
                    }
                }
            }

            // Detail panel slides up when a channel is selected
            if let Some(idx) = *active_channel.read() {
                if let Some(ch) = channels.read().get(idx).cloned() {
                    ChannelDetail {
                        chain: chain.clone(),
                        state: ch,
                        on_close: {
                            let mut active_channel = active_channel.clone();
                            move |_| { *active_channel.write() = None; }
                        },
                    }
                }
            }
        }
    }
}

// ── Add channel button ────────────────────────────────────────────────────────

#[component]
fn AddChannelButton(on_add: EventHandler<ChannelKind>) -> Element {
    let mut open = use_signal(|| false);

    rsx! {
        div { style: "position:relative;",
            button {
                onclick: move |_| { let v = *open.read(); *open.write() = !v; },
                style: format!(
                    "width:72px; height:72px; border-radius:{r}; \
                     border:1px dashed {border}; background:transparent; \
                     color:{dim}; font-size:24px; cursor:pointer;",
                    r = s::RADIUS_MD, border = s::BORDER, dim = s::TEXT_DIM,
                ),
                "+"
            }

            if *open.read() {
                div {
                    style: format!(
                        "position:absolute; top:80px; left:0; z-index:100; \
                         background:{bg}; border:1px solid {border}; \
                         border-radius:{r}; overflow:hidden; min-width:110px;",
                        bg = s::BG_SURFACE, border = s::BORDER, r = s::RADIUS_MD,
                    ),
                    for (kind, label) in [
                        (ChannelKind::Drums,            "Drums"),
                        (ChannelKind::Vocals,           "Vocals"),
                        (ChannelKind::Guitar,           "Guitar"),
                        (ChannelKind::Bass,             "Bass"),
                        (ChannelKind::Keys,             "Keys"),
                        (ChannelKind::Aux("AUX".into()), "Aux"),
                    ] {
                        button {
                            onclick: {
                                let kind = kind.clone();
                                let mut open = open.clone();
                                move |_| { on_add.call(kind.clone()); *open.write() = false; }
                            },
                            style: format!(
                                "width:100%; padding:12px 16px; background:transparent; \
                                 border:none; border-bottom:1px solid {border}; \
                                 color:{text}; font-size:13px; text-align:left; cursor:pointer;",
                                border = s::BORDER_SUBTLE, text = s::TEXT,
                            ),
                            "{label}"
                        }
                    }
                }
            }
        }
    }
}

// ── Channel detail panel ──────────────────────────────────────────────────────

#[component]
fn ChannelDetail(
    chain: ProcessingChain,
    state: ChannelState,
    on_close: EventHandler<()>,
) -> Element {
    let color = state.kind.color();
    let label = state.kind.label().to_string();

    // Compressor knobs — display-only. Dynamics processing is disabled in
    // `ProcessingChain` (it now exposes only `gr_db`), so these are local UI
    // state with no audio binding until a compressor is reintroduced.
    let _ = &chain;
    let mut threshold = use_signal(|| -18.0f32);
    let mut ratio = use_signal(|| 4.0f32);

    rsx! {
        div {
            style: format!(
                "background:{bg}; border-top:1px solid {border}; \
                 padding:16px; display:flex; flex-direction:column; gap:14px; flex-shrink:0;",
                bg = s::BG_PANEL, border = s::BORDER,
            ),

            // Header row
            div {
                style: "display:flex; justify-content:space-between; align-items:center;",
                span {
                    style: format!(
                        "font-size:14px; font-weight:700; color:{color}; letter-spacing:0.06em;",
                    ),
                    "{label}"
                }
                button {
                    onclick: move |_| on_close.call(()),
                    style: format!(
                        "width:{h}; height:{h}; border-radius:50%; border:none; \
                         background:{bg}; color:{dim}; font-size:20px; cursor:pointer;",
                        h = s::TOUCH_TARGET, bg = s::BG_SURFACE, dim = s::TEXT_DIM,
                    ),
                    "×"
                }
            }

            // EQ quick shelves + comp row
            div {
                style: "display:flex; flex-direction:row; gap:8px; flex-wrap:wrap;",
                QuickKnob { label: "HI SHELF", value: 0.0 }
                QuickKnob { label: "HI MID",   value: 0.0 }
                QuickKnob { label: "LO MID",   value: 0.0 }
                QuickKnob { label: "LO SHELF", value: 0.0 }

                // Threshold — display-only (no compressor backing).
                CompKnob {
                    label: "THRESH",
                    value: *threshold.read(),
                    min: -60.0,
                    max: 0.0,
                    on_change: move |v: f32| {
                        threshold.set(v);
                    },
                }

                // Ratio — display-only (no compressor backing).
                CompKnob {
                    label: "RATIO",
                    value: *ratio.read(),
                    min: 1.0,
                    max: 20.0,
                    on_change: move |v: f32| {
                        ratio.set(v);
                    },
                }
            }
        }
    }
}

// ── Knob helpers ──────────────────────────────────────────────────────────────

/// Passive display knob (no interaction yet — touch drag coming in Phase 4).
#[component]
fn QuickKnob(label: &'static str, value: f32) -> Element {
    let fill = value.clamp(0.0, 1.0);
    rsx! {
        div {
            style: "display:flex; flex-direction:column; align-items:center; gap:4px; flex:1; min-width:56px;",
            div {
                style: format!(
                    "width:48px; height:48px; border-radius:50%; \
                     background:conic-gradient({accent} 0% {pct}%, {bg} {pct}% 100%); \
                     display:flex; align-items:center; justify-content:center;",
                    accent = s::ACCENT,
                    bg = s::BG_ACTIVE,
                    pct = fill * 75.0,  // 270° sweep → 75% of 360°
                ),
                div {
                    style: format!(
                        "width:34px; height:34px; border-radius:50%; background:{bg};",
                        bg = s::BG_PANEL,
                    ),
                }
            }
            span {
                style: format!(
                    "font-size:8px; font-weight:700; color:{dim}; letter-spacing:0.07em;",
                    dim = s::TEXT_DIM,
                ),
                "{label}"
            }
        }
    }
}

/// Interactive knob for compressor params.
#[component]
fn CompKnob(
    label: &'static str,
    value: f32,
    min: f32,
    max: f32,
    on_change: EventHandler<f32>,
) -> Element {
    let t = ((value - min) / (max - min)).clamp(0.0, 1.0);
    rsx! {
        div {
            style: "display:flex; flex-direction:column; align-items:center; gap:4px; flex:1; min-width:56px;",
            div {
                style: format!(
                    "width:48px; height:48px; border-radius:50%; \
                     background:conic-gradient({accent} 0% {pct}%, {bg} {pct}% 100%); \
                     display:flex; align-items:center; justify-content:center;",
                    accent = s::WARN,
                    bg = s::BG_ACTIVE,
                    pct = t * 75.0,
                ),
                div {
                    style: format!(
                        "width:34px; height:34px; border-radius:50%; background:{bg}; \
                         display:flex; align-items:center; justify-content:center; \
                         font-size:9px; color:{text}; font-weight:700;",
                        bg = s::BG_PANEL, text = s::TEXT,
                    ),
                    { format!("{:.0}", value) }
                }
            }
            span {
                style: format!(
                    "font-size:8px; font-weight:700; color:{dim}; letter-spacing:0.07em;",
                    dim = s::TEXT_DIM,
                ),
                "{label}"
            }
        }
    }
}
