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
use signal_plugin_host::PluginParamInfo;
use signal_sampler::mixer::FxTarget;
use signal_sampler::{MixerLayout, MixerMeters, SamplerPlayer};

/// PartialEq-friendly mirror of [`PluginParamInfo`] (the upstream type from
/// the `daw::plugin` trait doesn't derive `PartialEq`; Dioxus `#[component]`
/// props require it). Same fields, populated by `from_info`.
#[derive(Clone, Debug, PartialEq)]
pub struct ParamInfo {
    pub id: u32,
    pub name: String,
    pub min: f64,
    pub max: f64,
    pub default: f64,
}

impl ParamInfo {
    pub fn from_info(p: &PluginParamInfo) -> Self {
        Self {
            id: p.id,
            name: p.name.clone(),
            min: p.min,
            max: p.max,
            default: p.default,
        }
    }
    pub fn vec_from(ps: Vec<PluginParamInfo>) -> Vec<Self> {
        ps.iter().map(Self::from_info).collect()
    }
}

use crate::plugin_picker::PluginPicker;

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
    // FX rack state: an internal version counter is bumped every time we
    // install / remove a plugin so the layout `use_effect` re-runs without
    // needing the parent to bump `reload`.
    let mut picker_open = use_signal(|| false);
    let mut fx_version = use_signal(|| 0u64);
    // Currently-open params editor: which slot, and the param list snapshot
    // taken when it was opened. `None` = editor closed.
    let mut params_open = use_signal(|| Option::<(FxTarget, usize, Vec<ParamInfo>)>::None);
    let v = fx_version();

    // Share meters + tick with MeterBar children (so only they animate).
    use_context_provider(|| MeterCtx { tick, meters });

    // Re-fetch structure whenever a new preset loads OR an FX-rack mutation
    // bumps `fx_version`.
    use_effect(use_reactive((&reload, &v), {
        let sampler = sampler.clone();
        let mut layout = layout;
        let mut meters = meters;
        move |(_reload, _v)| {
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

            // ── Master FX rack ──
            // REAPER-style serial FX chain on the preset master. "+ FX" loads
            // a CLAP / VST3 plugin from disk; each row gets bypass + remove +
            // "params" to open the slider editor below.
            div {
                style: "display:flex; flex-direction:column; gap:6px; padding:8px; \
                        background:#16161a; border:1px solid #2c2c2e; border-radius:6px;",
                div {
                    style: "display:flex; align-items:center; gap:8px;",
                    div { style: "color:#9a7bff; font-weight:700; font-size:12px;", "MASTER FX" }
                    span { style: "color:#666; font-size:10px;", "{lay.master_fx.len()} slot" }
                    button {
                        style: "margin-left:auto; padding:3px 10px; font-size:11px; \
                                background:#9a7bff; color:#111; border:none; \
                                border-radius:4px; cursor:pointer; font-weight:600;",
                        onclick: move |_| picker_open.set(true),
                        "+ FX"
                    }
                }
                if lay.master_fx.is_empty() {
                    div {
                        style: "padding:8px 4px; color:#666; font-size:11px; \
                                font-style:italic;",
                        "No FX loaded. Click + FX to load a CLAP / VST3 plugin."
                    }
                } else {
                    for slot in lay.master_fx.iter().cloned() {
                        FxSlotRow {
                            key: "{v}-master-{slot.slot_idx}",
                            target: FxTarget::Master,
                            slot_idx: slot.slot_idx,
                            display_name: slot.display_name.clone(),
                            bypassed: slot.bypassed,
                            params_open: params_open,
                            fx_version: fx_version,
                        }
                    }
                }
            }

            // ── Params editor (visible only when a slot is open for editing) ──
            if let Some((target, slot_idx, params)) = params_open() {
                ParamsEditor {
                    target,
                    slot_idx,
                    params,
                    on_close: move |_| params_open.set(None),
                }
            }

            // ── Plugin picker overlay ──
            //
            // Installs onto the drum mixer's master FX chain (visible via
            // `MixerLayout::master_fx`). Non-drum presets need a different
            // path (PresetRuntime::master_fx) — wire that in once non-drum
            // presets get their own panel.
            PluginPicker {
                open: picker_open,
                on_pick: move |path: std::path::PathBuf| {
                    match sampler.load_mixer_plugin(
                        super::INSTRUMENT_ID,
                        FxTarget::Master,
                        &path,
                    ) {
                        Ok(Some(_idx)) => {
                            // Trigger layout re-fetch so the new slot appears.
                            fx_version.set(fx_version() + 1);
                        }
                        Ok(None) => {
                            tracing::warn!("plugin load returned synthetic / unknown format: {path:?}");
                        }
                        Err(e) => {
                            tracing::error!("plugin load failed: {e}");
                        }
                    }
                },
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

/// One row in the FX rack — display name, bypass toggle, "params" button to
/// pop open the slider editor, and a remove [×]. Targets either the master
/// FX chain or a future per-strip chain.
#[component]
fn FxSlotRow(
    target: FxTarget,
    slot_idx: usize,
    display_name: String,
    bypassed: bool,
    params_open: Signal<Option<(FxTarget, usize, Vec<ParamInfo>)>>,
    fx_version: Signal<u64>,
) -> Element {
    let sampler = use_context::<SamplerPlayer>();
    let mut by = use_signal(|| bypassed);
    let mut p_open = params_open;
    let mut fxv = fx_version;
    let is_open_for_edit = matches!(params_open(), Some((t, i, _)) if t == target && i == slot_idx);

    let toggle_bypass = {
        let sampler = sampler.clone();
        move |_| {
            let new_b = !by();
            by.set(new_b);
            match target {
                FxTarget::Master => {
                    sampler.set_mixer_slot_bypass(super::INSTRUMENT_ID, target, slot_idx, new_b);
                }
                FxTarget::Channel(_) | FxTarget::Bus(_) => {
                    sampler.set_mixer_slot_bypass(super::INSTRUMENT_ID, target, slot_idx, new_b);
                }
            }
        }
    };
    let open_params = {
        let sampler = sampler.clone();
        move |_| {
            if is_open_for_edit {
                p_open.set(None);
                return;
            }
            // Master FX lives on PresetRuntime, but mixer_slot_params only
            // covers the drum mixer. For Master we synthesize the call via
            // the drum-mixer master path (FxTarget::Master) if a drum mixer
            // exists; for non-drum presets this returns None today and the
            // editor stays closed. (TODO: surface preset-master params.)
            if let Some(params) = sampler.mixer_slot_params(super::INSTRUMENT_ID, target, slot_idx)
            {
                p_open.set(Some((target, slot_idx, ParamInfo::vec_from(params))));
            }
        }
    };
    let remove = {
        let sampler = sampler.clone();
        move |_| {
            // The picker installs to the drum mixer's chain (see MixerPanel's
            // on_pick), so removal routes through the same path for all
            // targets including Master.
            sampler.remove_mixer_plugin(super::INSTRUMENT_ID, target, slot_idx);
            if is_open_for_edit {
                p_open.set(None);
            }
            fxv.set(fxv() + 1);
        }
    };

    let row_bg = if by() { "#1f1a1a" } else { "#1d1d22" };

    rsx! {
        div {
            style: "display:flex; align-items:center; gap:8px; padding:5px 8px; \
                    background:{row_bg}; border:1px solid #2c2c2e; border-radius:4px;",
            span {
                style: "color:#888; font-size:10px; font-weight:600; width:18px;",
                "{slot_idx + 1}"
            }
            span {
                style: "flex:1; min-width:0; color:#e0e0e0; font-size:12px; \
                        white-space:nowrap; overflow:hidden; text-overflow:ellipsis;",
                "{display_name}"
            }
            button {
                style: if by() {
                    "padding:3px 8px; font-size:10px; background:#e5484d; color:#fff; \
                     border:none; border-radius:3px; cursor:pointer; font-weight:600;"
                } else {
                    "padding:3px 8px; font-size:10px; background:#2a2a2e; color:#aaa; \
                     border:1px solid #444; border-radius:3px; cursor:pointer;"
                },
                onclick: toggle_bypass,
                "BYP"
            }
            button {
                style: if is_open_for_edit {
                    "padding:3px 10px; font-size:10px; background:#9a7bff; color:#111; \
                     border:none; border-radius:3px; cursor:pointer; font-weight:600;"
                } else {
                    "padding:3px 10px; font-size:10px; background:#2a2a2e; color:#ccc; \
                     border:1px solid #444; border-radius:3px; cursor:pointer;"
                },
                onclick: open_params,
                "Params"
            }
            button {
                style: "padding:3px 8px; font-size:10px; background:#2a2a2e; \
                        color:#999; border:1px solid #444; border-radius:3px; cursor:pointer;",
                onclick: remove,
                "×"
            }
        }
    }
}

/// Slider grid for one open FX slot. One horizontal slider per
/// `PluginParamInfo`. Values are linear in `[min, max]` (the host trait's
/// "plain value" — backends convert to internal normalized form). Slider
/// drag → `set_mixer_slot_param` (queued by HostedPlugin and applied at the
/// top of the next audio block).
#[component]
fn ParamsEditor(
    target: FxTarget,
    slot_idx: usize,
    params: Vec<ParamInfo>,
    on_close: EventHandler<()>,
) -> Element {
    rsx! {
        div {
            style: "display:flex; flex-direction:column; gap:6px; padding:10px; \
                    background:#13131a; border:1px solid #9a7bff; border-radius:6px;",
            div {
                style: "display:flex; align-items:center; gap:8px;",
                div { style: "color:#9a7bff; font-weight:700; font-size:12px;",
                    "PARAMS — slot {slot_idx + 1}"
                }
                span { style: "color:#666; font-size:10px;", "{params.len()} param" }
                button {
                    style: "margin-left:auto; padding:3px 10px; font-size:11px; \
                            background:#2a2a2e; color:#ccc; border:1px solid #444; \
                            border-radius:4px; cursor:pointer;",
                    onclick: move |_| on_close.call(()),
                    "Close"
                }
            }
            div {
                style: "display:grid; grid-template-columns:1fr 1fr; gap:6px 14px; \
                        max-height:280px; overflow:auto; padding:6px 2px;",
                for p in params.iter().cloned() {
                    ParamSlider {
                        key: "{slot_idx}-{p.id}",
                        target,
                        slot_idx,
                        info: p,
                    }
                }
            }
        }
    }
}

#[component]
fn ParamSlider(target: FxTarget, slot_idx: usize, info: ParamInfo) -> Element {
    let sampler = use_context::<SamplerPlayer>();
    let mut value = use_signal(|| info.default);
    let min = info.min;
    let max = info.max;
    let span = (max - min).max(1e-9);
    let frac = (((value() - min) / span) as f32).clamp(0.0, 1.0);
    let fill_w = (frac * 100.0).round() as i32;
    let name = info.name.clone();
    let id = info.id;
    let display = format!("{:.3}", value());

    let apply = {
        let sampler = sampler.clone();
        move |new_val: f64| {
            let v = new_val.clamp(min, max);
            value.set(v);
            sampler.set_mixer_slot_param(super::INSTRUMENT_ID, target, slot_idx, id, v);
        }
    };
    let mut apply_for_move = apply.clone();
    let mut drag = use_signal(|| Option::<(f32, f64)>::None);

    rsx! {
        div {
            style: "display:flex; flex-direction:column; gap:2px;",
            div {
                style: "display:flex; align-items:center; gap:6px;",
                div {
                    style: "flex:1; min-width:0; color:#ccc; font-size:11px; \
                            white-space:nowrap; overflow:hidden; text-overflow:ellipsis;",
                    "{name}"
                }
                div { style: "color:#888; font-size:10px;", "{display}" }
            }
            div {
                style: "position:relative; height:14px; background:#0e0e10; \
                        border:1px solid #333; border-radius:3px; \
                        touch-action:none; cursor:ew-resize;",
                onpointerdown: move |e| {
                    drag.set(Some((e.client_coordinates().x as f32, value())));
                },
                onpointermove: move |e| {
                    if let Some((start_x, start_v)) = drag() {
                        // Rough: assume the slider track is ~150px wide;
                        // a full traversal moves through full param range.
                        let dx = e.client_coordinates().x as f32 - start_x;
                        let delta = (dx / 150.0) as f64 * span;
                        apply_for_move(start_v + delta);
                    }
                },
                onpointerup: move |_| drag.set(None),
                onpointerleave: move |_| drag.set(None),

                div {
                    style: "position:absolute; left:0; top:0; bottom:0; \
                            width:{fill_w}%; background:#9a7bff; border-radius:2px;",
                }
            }
        }
    }
}
