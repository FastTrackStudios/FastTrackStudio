//! Guitar-rig panel: a GigPerformer-style profile/patch switcher over a
//! [`signal_sampler::ProfileRig`].
//!
//! The rig is **lazily** created (a "Start Guitar Rig" button) rather than at
//! app launch, so we don't grab the audio *input* device — or open a second
//! output stream — until the user actually wants the rig. Once started, you can
//! load a `.styx` profile (or a single `.nam` amp), switch patches (instant,
//! click-free), toggle bypass / level-matching, and watch in/out meters.

use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use dioxus::prelude::*;
use signal_sampler::{GuitarRig, ProfileRig, RigAudioPrefs, RigManager, RigPatch, RigProfile};

/// The rig whose saved settings (interface, channel, profile) the panel opens.
const RIG_NAME: &str = "Guitar Rig";

/// Lazily-initialised guitar rig shared across the UI. `None` until started.
#[derive(Clone)]
pub struct RigHandle(pub Arc<Mutex<Option<ProfileRig>>>);

impl Default for RigHandle {
    fn default() -> Self {
        Self(Arc::new(Mutex::new(None)))
    }
}

/// Snapshot of rig state for one render (taken under a brief lock).
#[derive(Clone, Default)]
struct RigSnap {
    profile_name: Option<String>,
    /// `(name, available)` per patch.
    patches: Vec<(String, bool)>,
    active: Option<usize>,
    bypass: bool,
    level_match: bool,
    /// Audio summary, e.g. "Yamaha TF · ch4".
    audio: String,
}

const ACCENT: &str = "#e8813a";

const BTN_PRIMARY: &str = "padding:6px 14px; font-size:12px; background:#e8813a; color:#111; \
    border:none; border-radius:4px; cursor:pointer; font-weight:600;";
const BTN_GHOST: &str = "padding:6px 14px; font-size:12px; background:#2a2a2e; color:#ddd; \
    border:1px solid #3a3a3c; border-radius:4px; cursor:pointer;";
const INPUT_STYLE: &str = "flex:1; padding:6px 8px; font-size:12px; background:#0e0e10; \
    color:#ddd; border:1px solid #3a3a3c; border-radius:4px;";

#[component]
pub fn RigPanel() -> Element {
    let handle = use_context::<RigHandle>();

    let mut version = use_signal(|| 0u64);
    let mut profile_path = use_signal(String::new);
    let mut amp_path = use_signal(String::new);
    let mut status = use_signal(|| "Guitar rig not started".to_string());
    let mut in_db = use_signal(|| -120.0f32);
    let mut out_db = use_signal(|| -120.0f32);

    // ── Audio-I/O device selection (DAW-style picker) ────────────────────
    // Enumerate devices once; seed the selectors from the saved rig settings.
    let in_devs = use_hook(GuitarRig::input_devices);
    let out_devs = use_hook(GuitarRig::output_devices);
    let saved = use_hook(|| RigManager::load(RIG_NAME).audio);
    let mut sel_in = use_signal(|| saved.input_device.clone());
    let mut sel_in_ch = use_signal(|| saved.input_channel);
    let mut sel_out = use_signal(|| saved.output_device.clone());
    let mut sel_sr = use_signal(|| saved.sample_rate);
    let mut sel_buf = use_signal(|| saved.buffer_size);

    // Channel count of the currently-selected input device (for the channel
    // dropdown). 0 ⇒ unknown / default; fall back to a sensible 8.
    let in_ch_count = in_devs
        .iter()
        .find(|d| d.name == sel_in())
        .map(|d| d.channels.max(1))
        .unwrap_or(0);

    // Apply the selected devices: persist to the rig's settings + (re)open the
    // engine. Reused for both the initial Start and live device changes.
    let apply_devices = {
        let handle = handle.clone();
        move || {
            let mut mgr = RigManager::load(RIG_NAME);
            mgr.audio = RigAudioPrefs {
                input_device: sel_in(),
                input_channel: sel_in_ch(),
                output_device: sel_out(),
                sample_rate: sel_sr(),
                buffer_size: sel_buf(),
            };
            if let Err(e) = mgr.save() {
                status.set(format!("save failed: {e}"));
            }
            match mgr.open() {
                Ok(p) => {
                    // Reflect the actually-opened device names back into the UI.
                    let eff = p.rig().prefs().clone();
                    sel_in.set(eff.input_device.clone());
                    sel_out.set(eff.output_device.clone());
                    *handle.0.lock().unwrap() = Some(p);
                    status.set("Rig running".to_string());
                }
                Err(e) => status.set(format!("open failed: {e}")),
            }
            version.set(version() + 1);
        }
    };

    // Meter poll (also reflects under/overruns via status when running).
    {
        let handle = handle.clone();
        use_future(move || {
            let handle = handle.clone();
            async move {
                loop {
                    futures_timer::Delay::new(Duration::from_millis(100)).await;
                    if let Ok(g) = handle.0.lock() {
                        if let Some(prig) = g.as_ref() {
                            in_db.set(lin_to_db(prig.rig().input_peak()));
                            out_db.set(lin_to_db(prig.rig().output_peak()));
                        }
                    }
                }
            }
        });
    }

    // Re-render when an action bumps the version.
    let _ = version();

    // Snapshot rig state under a brief lock.
    let snap: Option<RigSnap> = {
        let guard = handle.0.lock().ok();
        guard.as_ref().and_then(|o| o.as_ref()).map(|prig| RigSnap {
            profile_name: prig.profile_name().map(|s| s.to_string()),
            patches: prig
                .patches()
                .iter()
                .enumerate()
                .map(|(i, p)| (p.name.clone(), prig.is_patch_available(i)))
                .collect(),
            active: prig.active_index(),
            bypass: prig.rig().is_bypassed(),
            level_match: prig.is_level_matching(),
            audio: {
                let p = prig.rig().prefs();
                let dev = if p.input_device.is_empty() {
                    "default"
                } else {
                    p.input_device.as_str()
                };
                format!("{dev} · ch{}", p.input_channel + 1)
            },
        })
    };

    let started = snap.is_some();
    let snap = snap.unwrap_or_default();

    rsx! {
        div {
            style: "display:flex; flex-direction:column; gap:12px; padding:14px; \
                    overflow:auto; height:100%; box-sizing:border-box;",

            // Header
            div {
                style: "display:flex; align-items:center; gap:10px;",
                div {
                    div { style: "color:{ACCENT}; font-weight:700; font-size:13px;", "GUITAR RIG" }
                    div { style: "color:#777; font-size:11px;",
                        if let Some(name) = &snap.profile_name { "{name}" } else { "{status}" }
                    }
                }
                div { style: "margin-left:auto; color:#888; font-size:12px; text-align:right;",
                    div { "in {in_db():.0} dB   ·   out {out_db():.0} dB" }
                    if started {
                        div { style: "color:#666; font-size:11px;", "{snap.audio}" }
                    }
                }
            }

            // ── Audio I/O — DAW-style device picker (input + output device,
            // input channel, sample rate, buffer). Changing + applying it
            // (re)opens the engine on the chosen devices and persists to the rig.
            div {
                style: "display:flex; flex-direction:column; gap:8px; padding:10px 12px; \
                        background:#161618; border:1px solid #2c2c2e; border-radius:6px;",
                div { style: "color:#999; font-weight:600; font-size:11px; letter-spacing:0.04em;", "AUDIO I/O" }
                div {
                    style: "display:grid; grid-template-columns:1fr 80px 1fr; gap:8px; align-items:end;",
                    // Input device
                    label {
                        style: "display:flex; flex-direction:column; gap:3px; color:#888; font-size:11px;",
                        "Input device"
                        select {
                            style: "{INPUT_STYLE}",
                            value: "{sel_in}",
                            onchange: move |e| sel_in.set(e.value()),
                            option { value: "", selected: sel_in().is_empty(), "System default" }
                            for d in in_devs.iter() {
                                option { value: "{d.name}", selected: d.name == sel_in(), "{d.name} ({d.channels}ch)" }
                            }
                        }
                    }
                    // Input channel (the guitar DI)
                    label {
                        style: "display:flex; flex-direction:column; gap:3px; color:#888; font-size:11px;",
                        "Channel"
                        select {
                            style: "{INPUT_STYLE}",
                            value: "{sel_in_ch}",
                            onchange: move |e| if let Ok(n) = e.value().parse::<usize>() { sel_in_ch.set(n); },
                            for ch in 0..(if in_ch_count == 0 { 8 } else { in_ch_count }) {
                                option { value: "{ch}", selected: ch as usize == sel_in_ch(), "Ch {ch + 1}" }
                            }
                        }
                    }
                    // Output device
                    label {
                        style: "display:flex; flex-direction:column; gap:3px; color:#888; font-size:11px;",
                        "Output device"
                        select {
                            style: "{INPUT_STYLE}",
                            value: "{sel_out}",
                            onchange: move |e| sel_out.set(e.value()),
                            option { value: "", selected: sel_out().is_empty(), "System default" }
                            for d in out_devs.iter() {
                                option { value: "{d.name}", selected: d.name == sel_out(), "{d.name} ({d.channels}ch)" }
                            }
                        }
                    }
                }
                div {
                    style: "display:flex; gap:8px; align-items:end;",
                    // Sample rate
                    label {
                        style: "display:flex; flex-direction:column; gap:3px; color:#888; font-size:11px; flex:1;",
                        "Sample rate"
                        select {
                            style: "{INPUT_STYLE}",
                            value: "{sel_sr}",
                            onchange: move |e| if let Ok(n) = e.value().parse::<u32>() { sel_sr.set(n); },
                            for sr in [0u32, 44100, 48000, 88200, 96000] {
                                option { value: "{sr}", selected: sr == sel_sr(),
                                    if sr == 0 { "Device native" } else { "{sr} Hz" }
                                }
                            }
                        }
                    }
                    // Buffer size
                    label {
                        style: "display:flex; flex-direction:column; gap:3px; color:#888; font-size:11px; flex:1;",
                        "Buffer"
                        select {
                            style: "{INPUT_STYLE}",
                            value: "{sel_buf}",
                            onchange: move |e| if let Ok(n) = e.value().parse::<u32>() { sel_buf.set(n); },
                            for buf in [0u32, 64, 128, 256, 512, 1024] {
                                option { value: "{buf}", selected: buf == sel_buf(),
                                    if buf == 0 { "Backend default" } else { "{buf} frames" }
                                }
                            }
                        }
                    }
                    button {
                        style: "{BTN_PRIMARY} padding:8px 16px;",
                        onclick: {
                            let mut apply = apply_devices.clone();
                            move |_| apply()
                        },
                        if started { "↻  Apply / Re-open" } else { "▶  Start Rig" }
                    }
                }
            }

            if !started {
                // Pre-start hint — pick devices above, then Start. The engine
                // opens on the chosen interface + input channel and loads the
                // saved "Guitar Rig" profile/patches.
                div {
                    style: "padding:12px; color:#777; font-size:12px; text-align:center;",
                    "Pick your interface + input channel above, then Start. Opens the saved \"{RIG_NAME}\" profile."
                }
            } else {
                // Profile loader.
                div {
                    style: "display:flex; gap:8px; align-items:center;",
                    input {
                        style: "{INPUT_STYLE}",
                        r#type: "text",
                        placeholder: "path/to/profile.styx",
                        value: "{profile_path}",
                        oninput: move |e| profile_path.set(e.value()),
                    }
                    button {
                        style: "{BTN_PRIMARY}",
                        onclick: {
                            let handle = handle.clone();
                            move |_| {
                                let path = PathBuf::from(profile_path().trim());
                                if let Ok(mut g) = handle.0.lock() {
                                    if let Some(prig) = g.as_mut() {
                                        match prig.load_profile_file(&path) {
                                            Ok(()) => status.set(format!("loaded {}", path.display())),
                                            Err(e) => status.set(format!("load failed: {e}")),
                                        }
                                    }
                                }
                                version.set(version() + 1);
                            }
                        },
                        "Load Profile"
                    }
                }

                // Quick single-amp loader.
                div {
                    style: "display:flex; gap:8px; align-items:center;",
                    input {
                        style: "{INPUT_STYLE}",
                        r#type: "text",
                        placeholder: "path/to/amp.nam  (quick single-amp patch)",
                        value: "{amp_path}",
                        oninput: move |e| amp_path.set(e.value()),
                    }
                    button {
                        style: "{BTN_GHOST}",
                        onclick: {
                            let handle = handle.clone();
                            move |_| {
                                let p = amp_path().trim().to_string();
                                let name = PathBuf::from(&p)
                                    .file_stem()
                                    .and_then(|s| s.to_str())
                                    .unwrap_or("Amp")
                                    .to_string();
                                let profile = RigProfile::new("Direct")
                                    .with_patch(RigPatch::amp(name, p));
                                if let Ok(mut g) = handle.0.lock() {
                                    if let Some(prig) = g.as_mut() {
                                        match prig.load_profile(profile, None) {
                                            Ok(()) => status.set("loaded amp".to_string()),
                                            Err(e) => status.set(format!("load failed: {e}")),
                                        }
                                    }
                                }
                                version.set(version() + 1);
                            }
                        },
                        "Load Amp"
                    }
                }

                // Patch buttons.
                div {
                    style: "display:flex; flex-wrap:wrap; gap:8px;",
                    for (i, (name, avail)) in snap.patches.iter().cloned().enumerate() {
                        button {
                            key: "{i}",
                            style: patch_style(Some(i) == snap.active, avail),
                            disabled: !avail,
                            onclick: {
                                let handle = handle.clone();
                                move |_| {
                                    if let Ok(mut g) = handle.0.lock() {
                                        if let Some(prig) = g.as_mut() {
                                            prig.activate(i);
                                        }
                                    }
                                    version.set(version() + 1);
                                }
                            },
                            "{name}"
                            if !avail {
                                span { style: "font-size:10px; color:#e5484d; margin-left:6px;", "missing" }
                            }
                        }
                    }
                    if snap.patches.is_empty() {
                        span { style: "color:#666; font-size:12px;", "No profile loaded yet." }
                    }
                }

                // Controls: bypass + level match.
                div {
                    style: "display:flex; gap:8px; align-items:center; margin-top:4px;",
                    button {
                        style: if snap.bypass { BTN_PRIMARY } else { BTN_GHOST },
                        onclick: {
                            let handle = handle.clone();
                            move |_| {
                                if let Ok(g) = handle.0.lock() {
                                    if let Some(prig) = g.as_ref() {
                                        prig.rig().set_bypass(!prig.rig().is_bypassed());
                                    }
                                }
                                version.set(version() + 1);
                            }
                        },
                        if snap.bypass { "Bypass: ON (clean DI)" } else { "Bypass: off" }
                    }
                    button {
                        style: if snap.level_match { BTN_PRIMARY } else { BTN_GHOST },
                        onclick: {
                            let handle = handle.clone();
                            move |_| {
                                if let Ok(mut g) = handle.0.lock() {
                                    if let Some(prig) = g.as_mut() {
                                        prig.set_level_match(!prig.is_level_matching());
                                    }
                                }
                                version.set(version() + 1);
                            }
                        },
                        if snap.level_match { "Level-match: ON" } else { "Level-match: off" }
                    }
                }
            }
        }
    }
}

/// Style for a patch button given active / available state.
fn patch_style(active: bool, avail: bool) -> String {
    let base = "min-width:96px; padding:10px 12px; border-radius:5px; font-size:13px; \
                font-weight:600;";
    if !avail {
        format!(
            "{base} background:#1a1a1c; color:#555; border:1px solid #2c2c2e; cursor:not-allowed;"
        )
    } else if active {
        format!("{base} background:{ACCENT}; color:#111; border:none; cursor:pointer;")
    } else {
        format!("{base} background:#2a2a2e; color:#ddd; border:1px solid #444; cursor:pointer;")
    }
}

fn lin_to_db(lin: f32) -> f32 {
    if lin <= 1e-9 {
        -120.0
    } else {
        20.0 * lin.log10()
    }
}
