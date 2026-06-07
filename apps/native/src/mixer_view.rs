//! Drum-kit mixer panel for the native harness.
//!
//! Renders the send-based mixer of a loaded drum preset: a MASTER strip, the
//! shared buses (Overhead, Room…), and per-engine groups (collapsible) with a
//! fader + mute + solo + meter per close mic and per bus send.
//!
//! Faders drag vertically; `−`/`+` nudge 1 dB; `M` mutes; `S` solos.
//!
//! **Anti-flash design:** only the meters animate (~20 fps). The panel renders
//! its structure once per load; a small [`MeterBar`] child reads the meter tick
//! + handle from context, so meter repaints don't re-render the whole tree.
//! `SamplerPlayer`/`Arc<MixerMeters>` aren't `PartialEq`, so they can't be
//! props — the player comes from context, meters via the [`MeterCtx`] context.

use std::collections::HashSet;
use std::sync::Arc;

use dioxus::prelude::*;
use signal_sampler::{MixerLayout, MixerMeters, SamplerPlayer};

const DB_MIN: f32 = -60.0;
const DB_MAX: f32 = 12.0;
const TRACK_H: f32 = 116.0;
/// Drag sensitivity: dB change per pixel of vertical drag.
const DB_PER_PX: f32 = 0.4;

/// Which mixer control a strip drives.
#[derive(Clone, Copy, PartialEq)]
pub enum Target {
    Channel(usize),
    Send(usize),
    Bus(usize),
    Master,
}

/// Meter polling tick + live meters handle, shared via context so only
/// [`MeterBar`]s re-render on the tick (not the whole panel).
#[derive(Clone, Copy)]
struct MeterCtx {
    tick: Signal<u64>,
    meters: Signal<Option<Arc<MixerMeters>>>,
}

fn peak_to_dbfs(peak: f32) -> f32 {
    if peak > 1e-6 {
        20.0 * peak.log10()
    } else {
        -120.0
    }
}

fn target_peak(m: &MixerMeters, target: Target) -> f32 {
    match target {
        Target::Channel(i) => m.channel_peak(i),
        Target::Send(i) => m.send_peak(i),
        Target::Bus(i) => m.bus_peak(i),
        Target::Master => m.master_peak(),
    }
}

/// The mixer panel. `reload` (bumped by the app after each load) triggers a
/// fresh layout/meters fetch.
#[component]
pub fn MixerPanel(reload: u64) -> Element {
    let sampler = use_context::<SamplerPlayer>();
    let layout = use_signal(|| Option::<MixerLayout>::None);
    let meters = use_signal(|| Option::<Arc<MixerMeters>>::None);
    let tick = use_signal(|| 0u64);
    let mut collapsed = use_signal(HashSet::<usize>::new);
    let mut meters_live = use_signal(|| true);

    // Share meters + tick with MeterBar children (so only they animate).
    use_context_provider(|| MeterCtx { tick, meters });

    // Re-fetch structure whenever a new preset loads.
    use_effect(use_reactive((&reload,), {
        let sampler = sampler.clone();
        let mut layout = layout;
        let mut meters = meters;
        move |(_reload,)| {
            layout.set(sampler.drum_mixer_layout(super::INSTRUMENT_ID));
            meters.set(sampler.drum_mixer_meters(super::INSTRUMENT_ID));
        }
    }));

    // Poll to redraw meters (~8 fps). Only MeterBars read `tick`; a steady
    // meter rounds to the same px and diffs to a no-op, so idle = no repaint.
    // When the user freezes meters, stop bumping the tick → no repaints at all.
    use_future(move || {
        let mut tick = tick;
        let meters_live = meters_live;
        async move {
            let mut n = 0u64;
            loop {
                futures_timer::Delay::new(std::time::Duration::from_millis(120)).await;
                if meters_live() {
                    n += 1;
                    tick.set(n);
                }
            }
        }
    });

    let Some(lay) = layout() else {
        return rsx! {
            div { style: "padding:16px; color:#888; font-size:13px;",
                "No drum mixer for the loaded instrument. Load a multi-mic drum preset (e.g. Metal Monster)."
            }
        };
    };

    let n_engines = lay.engines.len();
    let all_collapsed = collapsed().len() >= n_engines && n_engines > 0;

    rsx! {
        div {
            style: "display:flex; flex-direction:column; gap:10px; padding:10px; \
                    overflow:auto; height:100%; box-sizing:border-box;",

            // ── Master + shared buses ──
            div {
                style: "display:flex; gap:14px; align-items:stretch; padding:8px; \
                        background:#191a1e; border:1px solid #3a3a3c; border-radius:6px;",
                div {
                    style: "display:flex; flex-direction:column; gap:4px;",
                    div { style: "color:#e8813a; font-weight:700; font-size:12px;", "MASTER" }
                    Strip {
                        target: Target::Master,
                        label: "Master".to_string(),
                        init_db: lay.master_gain_db,
                        init_muted: lay.master_muted,
                        init_soloed: false,
                        accent: "#e8813a".to_string(),
                        show_solo: false,
                        wide: true,
                    }
                }
                div { style: "width:1px; background:#3a3a3c;" }
                div {
                    style: "display:flex; flex-direction:column; gap:4px; flex:1; min-width:0;",
                    div { style: "color:#5a8cff; font-weight:700; font-size:12px;", "BUSES" }
                    div {
                        style: "display:flex; flex-wrap:wrap; gap:6px;",
                        for b in lay.buses.iter().cloned() {
                            Strip {
                                key: "{reload}-bus-{b.bus_idx}",
                                target: Target::Bus(b.bus_idx),
                                label: b.label.clone(),
                                init_db: b.gain_db,
                                init_muted: b.muted,
                                init_soloed: b.soloed,
                                accent: "#5a8cff".to_string(),
                                show_solo: true,
                                wide: false,
                            }
                        }
                    }
                }
            }

            // ── Engine collapse toolbar ──
            div {
                style: "display:flex; gap:8px; align-items:center;",
                div { style: "color:#888; font-size:12px; font-weight:600;", "PIECES" }
                button {
                    style: if meters_live() {
                        "margin-left:auto; padding:2px 10px; font-size:11px; background:#2a4a2e; \
                         color:#9be0a8; border:1px solid #3a6a44; border-radius:4px; cursor:pointer;"
                    } else {
                        "margin-left:auto; padding:2px 10px; font-size:11px; background:#2a2a2e; \
                         color:#999; border:1px solid #444; border-radius:4px; cursor:pointer;"
                    },
                    onclick: move |_| { let v = !meters_live(); meters_live.set(v); },
                    if meters_live() { "Meters: live" } else { "Meters: frozen" }
                }
                button {
                    style: "padding:2px 10px; font-size:11px; background:#2a2a2e; \
                            color:#ccc; border:1px solid #444; border-radius:4px; cursor:pointer;",
                    onclick: move |_| {
                        if all_collapsed {
                            collapsed.set(HashSet::new());
                        } else {
                            collapsed.set((0..n_engines).collect());
                        }
                    },
                    if all_collapsed { "Expand all" } else { "Collapse all" }
                }
            }

            // ── Per-engine groups ──
            for eng in lay.engines.iter().cloned() {
                {
                    let eidx = eng.engine_idx;
                    let is_collapsed = collapsed().contains(&eidx);
                    let n_ch = eng.channels.len();
                    let n_sn = eng.sends.len();
                    rsx! {
                        div {
                            style: "display:flex; flex-direction:column; \
                                    background:#161618; border:1px solid #2c2c2e; border-radius:6px;",
                            // Header (click toggles collapse).
                            div {
                                style: "display:flex; align-items:center; gap:8px; padding:6px 10px; cursor:pointer;",
                                onclick: move |_| {
                                    let mut c = collapsed();
                                    if !c.insert(eidx) { c.remove(&eidx); }
                                    collapsed.set(c);
                                },
                                span { style: "color:#888; width:12px;", if is_collapsed { "▸" } else { "▾" } }
                                span { style: "color:#e0e0e0; font-weight:600; font-size:12px;", "{eng.label}" }
                                span { style: "color:#666; font-size:10px;", "{n_ch} mic · {n_sn} send" }
                            }
                            if !is_collapsed {
                                div {
                                    style: "display:flex; flex-wrap:wrap; gap:6px; padding:2px 8px 8px;",
                                    for c in eng.channels.iter().cloned() {
                                        Strip {
                                            key: "{reload}-ch-{c.channel_idx}",
                                            target: Target::Channel(c.channel_idx),
                                            label: c.mic_label.clone(),
                                            init_db: c.gain_db,
                                            init_muted: c.muted,
                                            init_soloed: c.soloed,
                                            accent: "#3abe5a".to_string(),
                                            show_solo: true,
                                            wide: false,
                                        }
                                    }
                                    if !eng.sends.is_empty() {
                                        div { style: "width:1px; background:#2c2c2e; margin:0 2px;" }
                                    }
                                    for s in eng.sends.iter().cloned() {
                                        Strip {
                                            key: "{reload}-snd-{s.send_idx}",
                                            target: Target::Send(s.send_idx),
                                            label: format!("{} → {}", s.mic_label, s.bus_label),
                                            init_db: s.level_db,
                                            init_muted: s.muted,
                                            init_soloed: s.soloed,
                                            accent: "#9a7bff".to_string(),
                                            show_solo: true,
                                            wide: false,
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// A single channel strip: label, vertical fader (drag), meter, dB readout,
/// `−`/`+` nudge, `M` mute, optional `S` solo.
#[component]
fn Strip(
    target: Target,
    label: String,
    init_db: f32,
    init_muted: bool,
    init_soloed: bool,
    accent: String,
    show_solo: bool,
    wide: bool,
) -> Element {
    let sampler = use_context::<SamplerPlayer>();
    let mut db = use_signal(|| init_db);
    let mut muted = use_signal(|| init_muted);
    let mut soloed = use_signal(|| init_soloed);
    let mut drag = use_signal(|| Option::<(f32, f32)>::None);

    let apply_db = {
        let sampler = sampler.clone();
        move |new_db: f32| {
            let v = new_db.clamp(DB_MIN, DB_MAX);
            db.set(v);
            match target {
                Target::Channel(i) => sampler.set_mixer_channel_gain_db(super::INSTRUMENT_ID, i, v),
                Target::Send(i) => sampler.set_mixer_send_level_db(super::INSTRUMENT_ID, i, v),
                Target::Bus(i) => sampler.set_mixer_bus_gain_db(super::INSTRUMENT_ID, i, v),
                Target::Master => sampler.set_mixer_master_gain_db(super::INSTRUMENT_ID, v),
            }
        }
    };
    let toggle_mute = {
        let sampler = sampler.clone();
        move |_| {
            let m = !muted();
            muted.set(m);
            match target {
                Target::Channel(i) => sampler.set_mixer_channel_mute(super::INSTRUMENT_ID, i, m),
                Target::Send(i) => sampler.set_mixer_send_mute(super::INSTRUMENT_ID, i, m),
                Target::Bus(i) => sampler.set_mixer_bus_mute(super::INSTRUMENT_ID, i, m),
                Target::Master => sampler.set_mixer_master_mute(super::INSTRUMENT_ID, m),
            }
        }
    };
    let toggle_solo = {
        let sampler = sampler.clone();
        move |_| {
            let s = !soloed();
            soloed.set(s);
            match target {
                Target::Channel(i) => sampler.set_mixer_channel_solo(super::INSTRUMENT_ID, i, s),
                Target::Send(i) => sampler.set_mixer_send_solo(super::INSTRUMENT_ID, i, s),
                Target::Bus(i) => sampler.set_mixer_bus_solo(super::INSTRUMENT_ID, i, s),
                Target::Master => {}
            }
        }
    };

    // Fader cap position (top offset). Static between user changes.
    let cur_db = {
        let v = db();
        if v.is_finite() { v } else { 0.0 }
    };
    let fader_frac = ((cur_db - DB_MIN) / (DB_MAX - DB_MIN)).clamp(0.0, 1.0);
    let cap_top = ((1.0 - fader_frac) * TRACK_H).round() as i32;
    let db_label = if cur_db <= DB_MIN + 0.05 {
        "-inf".to_string()
    } else {
        format!("{cur_db:+.1}")
    };
    let strip_bg = if muted() { "#241516" } else { "#1d1d20" };
    let width = if wide { 60.0 } else { 52.0 };

    let mut apply_for_move = apply_db.clone();
    let mut apply_dn = apply_db.clone();
    let mut apply_up = apply_db;

    rsx! {
        div {
            style: "display:flex; flex-direction:column; align-items:center; gap:3px; \
                    width:{width}px; padding:5px 3px; background:{strip_bg}; border-radius:5px;",

            div {
                style: "font-size:9px; color:#bbb; height:24px; line-height:11px; \
                        overflow:hidden; text-align:center; width:100%;",
                "{label}"
            }

            // Fader track (drag to set) + meter (MeterBar animates on its own).
            div {
                style: "position:relative; width:{width - 18.0}px; height:{TRACK_H}px; \
                        background:#0e0e10; border:1px solid #333; border-radius:3px; \
                        touch-action:none; cursor:ns-resize;",
                onpointerdown: move |e| {
                    e.stop_propagation();
                    drag.set(Some((e.client_coordinates().y as f32, db())));
                },
                onpointermove: move |e| {
                    if let Some((start_y, start_db)) = drag() {
                        let y = e.client_coordinates().y as f32;
                        apply_for_move((start_db + (start_y - y) * DB_PER_PX).clamp(DB_MIN, DB_MAX));
                    }
                },
                onpointerup: move |_| drag.set(None),
                onpointerleave: move |_| drag.set(None),

                MeterBar { target }
                // Fader cap (right side).
                div {
                    style: "position:absolute; right:2px; top:{cap_top}px; width:20px; \
                            height:8px; margin-top:-4px; background:{accent}; \
                            border:1px solid #000; border-radius:2px;",
                }
            }

            div { style: "font-size:9px; color:#ddd;", "{db_label}" }

            div {
                style: "display:flex; gap:2px;",
                button {
                    style: "font-size:11px; width:20px; height:17px; padding:0; \
                            background:#2c2c2e; color:#ddd; border:1px solid #444; border-radius:3px;",
                    onclick: move |_| { let v = db() - 1.0; apply_dn(v); },
                    "−"
                }
                button {
                    style: "font-size:11px; width:20px; height:17px; padding:0; \
                            background:#2c2c2e; color:#ddd; border:1px solid #444; border-radius:3px;",
                    onclick: move |_| { let v = db() + 1.0; apply_up(v); },
                    "+"
                }
            }

            // Mute + (optional) Solo.
            div {
                style: "display:flex; gap:2px;",
                button {
                    style: if muted() {
                        "font-size:10px; width:20px; height:17px; padding:0; background:#e5484d; \
                         color:#fff; border:none; border-radius:3px; font-weight:600;"
                    } else {
                        "font-size:10px; width:20px; height:17px; padding:0; background:#2c2c2e; \
                         color:#aaa; border:1px solid #444; border-radius:3px;"
                    },
                    onclick: toggle_mute,
                    "M"
                }
                if show_solo {
                    button {
                        style: if soloed() {
                            "font-size:10px; width:20px; height:17px; padding:0; background:#e8c43a; \
                             color:#111; border:none; border-radius:3px; font-weight:700;"
                        } else {
                            "font-size:10px; width:20px; height:17px; padding:0; background:#2c2c2e; \
                             color:#aaa; border:1px solid #444; border-radius:3px;"
                        },
                        onclick: toggle_solo,
                        "S"
                    }
                }
            }
        }
    }
}

/// The animated meter fill for one strip. Reads the meter tick + handle from
/// context, so it repaints WITHOUT re-rendering its parent strip.
///
/// Height is rounded to whole pixels so a steady/idle meter produces identical
/// markup tick-to-tick (dioxus diffs to a no-op → no repaint). And when the
/// level is near zero we render NOTHING: a 0-height rounded rect makes Vello
/// emit NaN paths (`vello_common::flatten: A path contains NaN`), which was the
/// flashing.
#[component]
fn MeterBar(target: Target) -> Element {
    let ctx = use_context::<MeterCtx>();
    let _ = (ctx.tick)(); // subscribe → re-evaluate on each poll tick
    let peak = ctx
        .meters
        .read()
        .as_ref()
        .map(|m| target_peak(m, target))
        .unwrap_or(0.0);
    let peak = if peak.is_finite() { peak } else { 0.0 };
    let dbfs = peak_to_dbfs(peak);
    let frac = ((dbfs + 60.0) / 66.0).clamp(0.0, 1.0); // -60..+6 dB
    // Integer height so a steady/idle meter diffs to a no-op (no repaint).
    // NO border-radius: a rounded rect with ~0 height makes Vello emit NaN
    // paths. The element is ALWAYS present (just height 0 when idle) so it
    // never adds/removes a node — no reflow "jump".
    let h = (frac * TRACK_H).round().clamp(0.0, TRACK_H) as i32;
    let color = if dbfs > 0.0 {
        "#e5484d"
    } else if dbfs > -6.0 {
        "#e8a13a"
    } else {
        "#3abe5a"
    };
    rsx! {
        div {
            style: "position:absolute; left:3px; bottom:1px; width:11px; \
                    height:{h}px; background:{color};",
        }
    }
}
