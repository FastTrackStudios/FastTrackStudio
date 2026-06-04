//! FastTrackStudio — native (Blitz/GPU) DAW application.
//!
//! A real DAW shell built on the `daw-standalone` engine and rendered through
//! the reusable, vector-themeable `daw-ui` panels — both reached *only* through
//! the `daw` facade (domain types via `daw::service`, components via `daw::ui`).
//! The standalone engine is the source of truth: we seed a demo project, add
//! real tracks (folders, colours, volumes), then snapshot them into the panel
//! view-model and mount the [`DawWorkspace`].
//!
//! Run:
//! ```sh
//! just native            # dx serve -p daw-native (hot-reload)
//! # or a plain build:  cargo run -p daw-native
//! ```
//!
//! Milestones:
//!   1. (this) boot engine + render real engine tracks + transport play/stop.
//!   2. per-track metering tap (peak.rs) + cpal audio (test tones) so the
//!      meters move and playback is audible.

use std::rc::Rc;
use std::time::Duration;

use daw::service::transport::service::Transport;
use daw::service::{Peaks, ProjectContext, ProjectInfo, Track, TrackRef, Tracks};
use daw::ui::panels::{DawWorkspace, TrackView};
use daw::ui::theming::ThemeProvider;
use daw_standalone::audio_engine::{AudioEngine, test_tone};
use daw_standalone::sync::Standalone;
use dioxus::prelude::*;

/// Seed GUID for the demo project (shared between seeding, audio attach, and
/// metering).
const PROJECT_GUID: &str = "fts-native-demo";

/// Metering refresh rate (~30 fps): poll the engine peaks and push them into the
/// per-track meter signals.
const METER_INTERVAL: Duration = Duration::from_millis(33);

/// Default REAPER theme to import (the Anti-Theme — our fidelity target).
/// Override with `FTS_REAPER_THEME`; falls back to FTS dark when absent.
const DEFAULT_REAPER_THEME: &str =
    "/home/cody/Development/FastTrackStudio/reaper-theme/extracted/antitheme";

fn main() {
    prefer_wayland();
    init_logging();
    tracing::info!("FastTrackStudio native — booting Blitz window");
    dioxus_native::launch(App);
}

/// Force winit onto native Wayland whenever a compositor socket exists.
///
/// winit picks its backend from the environment: `WAYLAND_DISPLAY` → Wayland,
/// else `DISPLAY` → X11. Shells that only carry `DISPLAY` (tmux, SSH, some
/// launchers) push the app onto XWayland, where the compositor's initial
/// tiling resize races blitz's viewport init (`View::init` reads
/// `surface_size()` before the WM resizes, and X11 never re-sends) — the app
/// stays laid out at winit's default 800x600. Wayland re-configures after
/// init, so it's immune. No socket → leave the env alone (real X11 still
/// works).
fn prefer_wayland() {
    use std::path::Path;
    if std::env::var_os("WAYLAND_DISPLAY").is_some() {
        return;
    }
    let Some(dir) = std::env::var_os("XDG_RUNTIME_DIR") else {
        return;
    };
    for i in 0..10 {
        let name = format!("wayland-{i}");
        if Path::new(&dir).join(&name).exists() {
            // SAFETY: called at the top of main, before any other thread exists.
            unsafe { std::env::set_var("WAYLAND_DISPLAY", &name) };
            return;
        }
    }
}

/// Root component: boots the engine once, snapshots its tracks into the panel
/// view-model, and renders the workspace under a transport bar.
#[component]
fn App() -> Element {
    // Resolve the active theme once: `FTS_REAPER_THEME=<unpacked theme dir>`
    // imports a REAPER theme (palette + adjuster knobs). Defaults to the
    // Anti-Theme extraction (our import fidelity target) when present;
    // otherwise FTS dark.
    let theme_ctx = use_hook(|| {
        let mut ctx = daw::ui::theming::ThemeContext::new();
        let dir =
            std::env::var("FTS_REAPER_THEME").unwrap_or_else(|_| DEFAULT_REAPER_THEME.to_string());
        // 150% is REAPER's typical rendering on this rig (misc_dpi_translate
        // kicks in at >=134%); FTS_THEME_SCALE overrides.
        let scale = std::env::var("FTS_THEME_SCALE")
            .ok()
            .and_then(|s| s.parse::<f32>().ok())
            .unwrap_or(1.5);
        match daw::ui::theming::reaper_import::theme_from_dir_scaled(&dir, scale) {
            Ok(theme) => {
                tracing::info!("imported REAPER theme from {dir} at {scale}x");
                ctx = ctx.with_theme(theme);
            }
            Err(e) => tracing::warn!("REAPER theme import failed ({dir}): {e} — using FTS dark"),
        }
        ctx
    });

    // Boot the standalone engine + seed the demo project exactly once. The
    // engine is `Arc`-backed, so clones into event handlers are cheap.
    let engine = use_hook(|| {
        let engine = Standalone::new();
        seed_demo_project(&engine);
        engine
    });

    // Snapshot engine tracks → view-models once; the per-track `Signal`s then
    // live in this root scope and stay shared across all three panels.
    let tracks = use_signal(|| build_track_views(&engine));

    // Start the audio engine: one test tone per audible track, metering into the
    // engine's per-track peak bank. Kept alive for the component's lifetime (the
    // cpal stream stops when dropped); `None` if no audio device is available.
    let _audio: Option<Rc<AudioEngine>> = use_hook(|| start_audio(&engine).map(Rc::new));

    // Metering poll loop: ~30 fps, read engine peaks → push into the meter
    // signals so the mixer's meters track real per-track levels.
    let meter_engine = engine.clone();
    use_future(move || {
        let engine = meter_engine.clone();
        async move {
            // Meter signal handles are stable, Copy — snapshot once.
            let cells: Vec<(Signal<f32>, Signal<f32>, Signal<f32>)> = tracks
                .peek()
                .iter()
                .map(|t| (t.level, t.level_right, t.peak))
                .collect();
            loop {
                tokio::time::sleep(METER_INTERVAL).await;
                for (i, &(mut left, mut right, mut peak)) in cells.iter().enumerate() {
                    let idx = TrackRef::Index(i as u32);
                    let l = engine.track_peak(ProjectContext::Current, idx.clone(), 0);
                    let r = engine.track_peak(ProjectContext::Current, idx, 1);
                    // Only write on change — a `set` always re-renders the meter,
                    // so unconditional writes redraw every strip at 30 fps even
                    // when all tracks are silent.
                    set_if_changed(&mut left, db_to_norm(l.peak_db));
                    set_if_changed(&mut right, db_to_norm(r.peak_db));
                    set_if_changed(&mut peak, db_to_norm(l.peak_hold_db));
                }
            }
        }
    });

    let mut playing = use_signal(|| false);

    // Theme tokens drive the shell chrome (the panels theme themselves through
    // the provider below; App itself sits above it, so read the context value).
    let tk = theme_ctx.theme.tokens;
    let bg = tk.surface.css();
    let header_bg = tk.surface_raised.css();
    let border = tk.border.css();
    let text = tk.text.css();
    let text_strong = tk.text.lighten(0.2).css();
    let text_dim = tk.text_faint.css();
    let accent = tk.accent.css();

    let is_playing = playing();

    rsx! {
        // Blitz mounts the app inside `<div id="main">` under html/body; size the
        // whole ancestor chain to the viewport (the upstream blitz example
        // pattern) so percentage sizing reaches the app root. `vw/vh` on the app
        // root alone leaves the unstyled ancestors at auto height.
        style { {ROOT_CSS} }
        ThemeProvider {
            theme: theme_ctx.clone(),
        div {
            style: format!(
                "display:flex; flex-direction:column; width:100%; height:100%; \
                 background:{bg}; color:{text}; overflow:hidden; \
                 font-family:'Inter','SF Pro Display',system-ui,sans-serif;"
            ),

            // ── Transport / title bar ──
            div {
                style: format!(
                    "flex:0 0 auto; display:flex; align-items:center; gap:12px; \
                     padding:6px 12px; background:{header_bg}; border-bottom:1px solid {border}; \
                     font-size:12px; letter-spacing:0.04em;"
                ),
                span { style: format!("color:{text_strong}; font-weight:700;"), "FastTrackStudio" }
                span { style: format!("color:{text_dim}; font-weight:500;"), "Native" }

                div { style: "flex:1 1 0;" }

                // Play.
                button {
                    r#type: "button",
                    title: "Play",
                    style: format!(
                        "width:30px; height:24px; border-radius:5px; cursor:pointer; \
                         border:1px solid {border}; \
                         background:{bg}; color:{fg}; font-size:12px;",
                        bg = if is_playing { accent.clone() } else { "transparent".to_string() },
                        fg = if is_playing { tk.surface.css() } else { text.clone() },
                    ),
                    onclick: {
                        let engine = engine.clone();
                        move |_| {
                            if let Err(e) = engine.play(ProjectContext::Current) {
                                tracing::warn!("play failed: {e:?}");
                            }
                            playing.set(true);
                        }
                    },
                    "▶"
                }
                // Stop.
                button {
                    r#type: "button",
                    title: "Stop",
                    style: format!(
                        "width:30px; height:24px; border-radius:5px; cursor:pointer; \
                         border:1px solid {border}; background:transparent; color:{text}; font-size:12px;"
                    ),
                    onclick: {
                        let engine = engine.clone();
                        move |_| {
                            if let Err(e) = engine.stop(ProjectContext::Current) {
                                tracing::warn!("stop failed: {e:?}");
                            }
                            let _ = engine.goto_start(ProjectContext::Current);
                            playing.set(false);
                        }
                    },
                    "⏹"
                }
            }

            // ── Workspace (arrange-over-mixer) ──
            div {
                style: "flex:1 1 0; min-height:0;",
                DawWorkspace { tracks: tracks() }
            }
        }
        }
    }
}

/// Seed a small, folder-nested demo project into the engine: a DRUMS folder
/// (Kick / Snare / OHs), a BASS track, a GTRS folder (Rhythm L / R), and VOX.
/// `folder_delta` is the REAPER-style folder depth change (+1 opens a folder,
/// -1 on the last child closes it).
fn seed_demo_project(engine: &Standalone) {
    engine.seed_project(ProjectInfo {
        guid: PROJECT_GUID.to_string(),
        name: "FastTrackStudio — Demo".to_string(),
        path: String::new(),
    });

    // (name, 0xRRGGBB colour, initial volume 0–1, folder_delta)
    let specs: &[(&str, u32, f64, i32)] = &[
        ("DRUMS", 0xef4444, 0.80, 1),
        ("Kick", 0xf97316, 0.80, 0),
        ("Snare", 0xf59e0b, 0.70, 0),
        ("OHs", 0xeab308, 0.75, -1),
        ("BASS", 0x22c55e, 0.72, 0),
        ("GTRS", 0x38bdf8, 0.70, 1),
        ("Rhythm L", 0x0ea5e9, 0.68, 0),
        ("Rhythm R", 0x0284c7, 0.68, -1),
        ("VOX", 0xa855f7, 0.78, 0),
    ];

    for (name, color, volume, folder_delta) in specs {
        match engine.add(ProjectContext::Current, name, None) {
            Ok(guid) => {
                let tref = TrackRef::Guid(guid);
                let _ = engine.set_color(ProjectContext::Current, tref.clone(), *color);
                let _ = engine.set_volume(ProjectContext::Current, tref.clone(), *volume);
                if *folder_delta != 0 {
                    let _ = engine.set_folder_depth(ProjectContext::Current, tref, *folder_delta);
                }
            }
            Err(e) => tracing::warn!("seed: add track {name:?} failed: {e:?}"),
        }
    }
}

/// Snapshot the engine's current tracks into the panel [`TrackView`] model,
/// computing absolute folder depth from the running folder-depth delta.
fn build_track_views(engine: &Standalone) -> Vec<TrackView> {
    let mut depth: i32 = 0;
    let mut views = Vec::new();
    for (i, track) in engine.all(ProjectContext::Current).iter().enumerate() {
        views.push(track_to_view(i, track, depth.max(0) as u32));
        depth = (depth + track.folder_depth).max(0);
    }
    views
}

/// Adapt one engine [`Track`] into a [`TrackView`] at the given absolute depth.
fn track_to_view(id: usize, track: &Track, depth: u32) -> TrackView {
    let color = track.color.map(u32_to_hex);
    let mut view = TrackView::new(id, &track.name, color.as_deref())
        .fader(track.volume as f32)
        .depth(depth);
    if track.is_folder {
        view = view.folder();
    } else {
        view = view.stereo(); // dual-column meter for audible tracks
    }
    // Mirror the engine's per-track state into the shared UI signals.
    view.mute.set(track.muted);
    view.solo.set(track.soloed);
    view.record_arm.set(track.armed);
    view
}

/// `0xRRGGBB` → `#rrggbb`.
fn u32_to_hex(c: u32) -> String {
    format!("#{:06x}", c & 0x00ff_ffff)
}

/// Start the cpal audio engine for the demo project: attach to the project's
/// transport clock + a freshly-sized peak-meter bank, then load one rhythmic
/// test tone per audible (non-folder) track so playback is heard and the meters
/// move. Returns `None` if no audio device is available (the UI still runs).
fn start_audio(engine: &Standalone) -> Option<AudioEngine> {
    let tracks = engine.all(ProjectContext::Current);
    let audio = match AudioEngine::metered_for(engine, PROJECT_GUID, tracks.len()) {
        Ok(audio) => audio,
        Err(err) => {
            tracing::warn!("audio engine unavailable: {err} — UI runs without sound/meters");
            return None;
        }
    };
    let sample_rate = audio.sample_rate();
    for (index, track) in tracks.iter().enumerate() {
        if track.is_folder {
            continue; // folder parents carry no audio of their own
        }
        let (freq, pulse_hz) = tone_for(index);
        let tone = test_tone::pulse_tone(freq, pulse_hz, 0.45, 120.0, sample_rate);
        let handle = audio.add_track_metered(tone, index);
        audio.set_track_gain(handle, track.volume as f32);
    }
    audio.set_master_gain(0.5); // headroom for the simultaneous tones
    tracing::info!("audio engine started ({sample_rate} Hz)");
    Some(audio)
}

/// Per-track (carrier Hz, pulse Hz) for the demo tones — distinct pitches and
/// rhythms so the meters bounce polyrhythmically. Indexed by project track
/// order; folder rows are skipped by the caller.
fn tone_for(index: usize) -> (f32, f32) {
    const TONES: &[(f32, f32)] = &[
        (55.0, 2.0),   // DRUMS  (folder — skipped)
        (65.41, 2.0),  // Kick      C2
        (196.0, 4.0),  // Snare     G3
        (783.99, 8.0), // OHs       G5
        (82.41, 1.0),  // Bass      E2
        (110.0, 1.5),  // GTRS   (folder — skipped)
        (146.83, 3.0), // Rhythm L  D3
        (220.0, 3.0),  // Rhythm R  A3
        (329.63, 1.0), // VOX       E4
    ];
    TONES.get(index).copied().unwrap_or((220.0, 2.0))
}

/// Viewport sizing for the Blitz element chain above the app root
/// (`html > body > #main`): the upstream blitz examples size apps through this
/// percentage chain rather than `vw/vh` units on the root.
const ROOT_CSS: &str = "html, body, #main { margin: 0; padding: 0; width: 100%; height: 100%; }";

/// Map a dBFS reading to a normalized meter fill (`0..1`), with `-60 dB` the
/// floor and `0 dB` the top.
fn db_to_norm(db: f64) -> f32 {
    (((db + 60.0) / 60.0).clamp(0.0, 1.0)) as f32
}

/// Write a meter signal only when the value actually changed (any `set`
/// re-renders subscribers, silent or not).
fn set_if_changed(sig: &mut Signal<f32>, value: f32) {
    if *sig.peek() != value {
        sig.set(value);
    }
}

/// Log to a file so the GPU/winit/wgpu chatter doesn't drown the terminal.
fn init_logging() {
    use tracing_subscriber::EnvFilter;
    let path = "/tmp/daw-native.log";
    if let Ok(file) = std::fs::File::create(path) {
        let _ = tracing_subscriber::fmt()
            .with_env_filter(
                EnvFilter::try_from_default_env()
                    .unwrap_or_else(|_| EnvFilter::new("info,wgpu=warn,wgpu_core=warn,naga=warn")),
            )
            .with_writer(file)
            .with_ansi(false)
            .try_init();
    }
}
