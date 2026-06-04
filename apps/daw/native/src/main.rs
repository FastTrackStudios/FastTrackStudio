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

use daw::service::item::{Item, ItemRef, Items};
use daw::service::marker::Markers as MarkersService;
use daw::service::region::Regions as RegionsService;
use daw::service::take::Takes;
use daw::service::tempo_map::TempoMap;
use daw::service::transport::service::Transport;
use daw::service::{Peaks, ProjectContext, ProjectInfo, Track, TrackRef, Tracks};
use daw::ui::panels::{
    ClipView, DawWorkspace, EnvelopeView, MarkerView, RegionView, TempoMarkerView, TrackView,
    TransportBar,
};
use daw_standalone::reapeaks::ReaPeaks;
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

/// Default real project to open (a converted PT session: 123 tracks, 422
/// items, real markers/regions/tempo + media with REAPER peak caches).
/// Override with `FTS_RPP`; falls back to the seeded demo when absent.
const DEFAULT_RPP: &str = "/run/media/cody/15B0-07EB/Transfer May 26th Done/\u{1f535} PNG Project - PT Sessions/02 LORD OF THE FIGHT/02 LORD OF THE FIGHT.RPP";

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
        // FTS_THEME_SCALE picks the theme's DPI variant (1.5 → 150%_ layouts
        // + 150/ images); 1.0 matches REAPER at 100%.
        let scale = std::env::var("FTS_THEME_SCALE")
            .ok()
            .and_then(|s| s.parse::<f32>().ok())
            .unwrap_or(1.0);
        match daw::ui::theming::reaper_import::theme_from_dir_scaled(&dir, scale) {
            Ok(theme) => {
                tracing::info!("imported REAPER theme from {dir} at {scale}x");
                ctx = ctx.with_theme(theme);
            }
            Err(e) => tracing::warn!("REAPER theme import failed ({dir}): {e} — using FTS dark"),
        }
        ctx
    });

    // Boot the standalone engine once: open the real RPP project when its
    // file exists (structure only — audio materializes in the background),
    // otherwise seed the test-tone demo. `Arc`-backed, cheap to clone.
    let boot = use_hook(|| Rc::new(boot_project()));
    let engine = boot.engine.clone();
    let ctx = boot.ctx.clone();

    // Snapshot engine tracks → view-models once; the per-track `Signal`s then
    // live in this root scope and stay shared across all three panels.
    let tracks = use_signal(|| boot.tracks.clone());

    // Audio:
    // - demo: one test tone per audible track, immediately.
    // - real project: decode the project's media in the background
    //   (gigabytes — must not block first paint), then attach the cpal
    //   project renderer on the UI thread when it's done.
    let mut audio_slot: Signal<Option<Rc<AudioEngine>>> = use_signal(|| None);
    {
        let boot = boot.clone();
        let engine = engine.clone();
        use_future(move || {
            let boot = boot.clone();
            let engine = engine.clone();
            async move {
                match &boot.rpp_guid {
                    None => {
                        if let Some(audio) = start_audio(&engine) {
                            audio_slot.set(Some(Rc::new(audio)));
                        }
                    }
                    Some(guid) => {
                        let guid = guid.clone();
                        let mat_engine = engine.clone();
                        let mat_guid = guid.clone();
                        let report = tokio::task::spawn_blocking(move || {
                            daw_standalone::audio_engine::materialize::materialize_via_bay(
                                &mat_engine,
                                &mat_guid,
                            )
                        })
                        .await;
                        match report {
                            Ok(Ok(report)) => {
                                tracing::info!(
                                    "materialized {} audio sources ({} failed, {} no source)",
                                    report.loaded,
                                    report.failed.len(),
                                    report.skipped_no_source,
                                );
                                match engine.attach_audio_engine(&guid) {
                                    Ok(audio) => audio_slot.set(Some(Rc::new(audio))),
                                    Err(e) => tracing::warn!("audio attach failed: {e}"),
                                }
                            }
                            Ok(Err(e)) => tracing::warn!("materialize failed: {e}"),
                            Err(e) => tracing::warn!("materialize task failed: {e}"),
                        }
                    }
                }
            }
        });
    }

    // Metering poll loop: ~30 fps, read engine peaks → push into the meter
    // signals so the mixer's meters track real per-track levels.
    let meter_engine = engine.clone();
    let meter_ctx_outer = ctx.clone();
    use_future(move || {
        let engine = meter_engine.clone();
        let meter_ctx = meter_ctx_outer.clone();
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
                    let l = engine.track_peak(meter_ctx.clone(), idx.clone(), 0);
                    let r = engine.track_peak(meter_ctx.clone(), idx, 1);
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

    // UI → engine state sync: any mute/solo/arm/fader/pan signal change
    // pushes the whole snapshot into the engine (idempotent setters), so the
    // project renderer honours strip toggles during playback.
    {
        let engine = engine.clone();
        let ctx = ctx.clone();
        use_effect(move || {
            for (i, t) in tracks.peek().iter().enumerate() {
                let tref = TrackRef::Index(i as u32);
                let _ = Tracks::set_muted(&engine, ctx.clone(), tref.clone(), (t.mute)());
                let _ = Tracks::set_soloed(&engine, ctx.clone(), tref.clone(), (t.solo)());
                let _ = Tracks::set_armed(&engine, ctx.clone(), tref.clone(), (t.record_arm)());
                let _ =
                    Tracks::set_volume(&engine, ctx.clone(), tref.clone(), (t.fader)() as f64);
                let _ = Tracks::set_pan(
                    &engine,
                    ctx.clone(),
                    tref,
                    ((t.pan)() as f64) * 2.0 - 1.0,
                );
            }
        });
    }

    let mut playing = use_signal(|| false);
    let playhead = use_signal(|| 0.0f64);

    // Playhead poll: track the transport clock at the metering rate.
    let pos_engine = engine.clone();
    let pos_ctx_outer = ctx.clone();
    use_future(move || {
        let engine = pos_engine.clone();
        let pos_ctx = pos_ctx_outer.clone();
        let mut playhead = playhead;
        async move {
            loop {
                tokio::time::sleep(METER_INTERVAL).await;
                let pos = engine.get_position(pos_ctx.clone());
                if (playhead.peek().clone() - pos).abs() > 1e-3 {
                    playhead.set(pos);
                }
            }
        }
    });

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
            }

            // ── Transport (themed `trans.*` context) ──
            TransportBar {
                playing,
                bpm: boot.bpm,
                position: playhead(),
                on_play: {
                    let engine = engine.clone();
                    let ctx = ctx.clone();
                    move |_| {
                        if let Err(e) = engine.play(ctx.clone()) {
                            tracing::warn!("play failed: {e:?}");
                        }
                        playing.set(true);
                    }
                },
                on_stop: {
                    let engine = engine.clone();
                    let ctx = ctx.clone();
                    move |_| {
                        if let Err(e) = engine.stop(ctx.clone()) {
                            tracing::warn!("stop failed: {e:?}");
                        }
                        let _ = engine.goto_start(ctx.clone());
                        playing.set(false);
                    }
                },
            }

            // ── Workspace (arrange-over-mixer) ──
            div {
                style: "flex:1 1 0; min-height:0;",
                DawWorkspace {
                    tracks: tracks(),
                    markers: boot.markers.clone(),
                    regions: boot.regions.clone(),
                    tempo_markers: boot.tempo_markers.clone(),
                    bpm: boot.bpm,
                    beats_per_measure: boot.beats_per_measure,
                    seconds: boot.seconds,
                    cursor: 0.0,
                    playhead: is_playing.then(|| playhead()),
                }
            }
        }
        }
    }
}

/// Everything App needs from the booted project.
struct BootedProject {
    engine: Standalone,
    ctx: ProjectContext,
    /// `Some(guid)` when a real RPP was loaded (audio materializes async).
    rpp_guid: Option<String>,
    tracks: Vec<TrackView>,
    markers: Vec<MarkerView>,
    regions: Vec<RegionView>,
    tempo_markers: Vec<TempoMarkerView>,
    bpm: f64,
    beats_per_measure: u32,
    /// Timeline length (s) for the arrange view.
    seconds: f64,
}

/// Boot the engine: open the real RPP (`FTS_RPP`, default the converted PT
/// session) when present — real track structure, items, markers, tempo —
/// otherwise the seeded test-tone demo.
fn boot_project() -> BootedProject {
    let engine = Standalone::new();
    let rpp = std::env::var("FTS_RPP").unwrap_or_else(|_| DEFAULT_RPP.to_string());
    match load_rpp_project(&engine, &rpp) {
        Some(boot) => boot,
        None => {
            seed_demo_project(&engine);
            let tracks = build_track_views(&engine, &ProjectContext::Current, true);
            BootedProject {
                engine,
                ctx: ProjectContext::Current,
                rpp_guid: None,
                tracks,
                markers: demo_markers(),
                regions: demo_regions(),
                tempo_markers: demo_tempo_markers(),
                bpm: 120.0,
                beats_per_measure: 4,
                seconds: 120.0,
            }
        }
    }
}

/// Load a real `.RPP` (structure only — fast) and snapshot its tracks,
/// items (+ `.reapeaks` waveforms), markers, regions and tempo map into the
/// panel view-model.
fn load_rpp_project(engine: &Standalone, rpp_path: &str) -> Option<BootedProject> {
    use daw_standalone::media_bay::ProjectRelativeResolver;
    use daw_standalone::project_loader::load_rpp_text;
    use std::path::PathBuf;

    let path = PathBuf::from(rpp_path);
    let rpp_text = match std::fs::read_to_string(&path) {
        Ok(t) => t,
        Err(e) => {
            tracing::info!("no RPP at {rpp_path} ({e}) — using the demo project");
            return None;
        }
    };
    let project_dir = path.parent().map(PathBuf::from).unwrap_or_default();
    let name = path.file_stem().and_then(|s| s.to_str()).unwrap_or("project");

    engine
        .media_bay()
        .set_file_resolver(Box::new(ProjectRelativeResolver::new(project_dir.clone())));

    let proj = match load_rpp_text(engine, name, rpp_path, &rpp_text) {
        Ok(p) => p,
        Err(e) => {
            tracing::warn!("RPP load failed ({rpp_path}): {e} — using the demo project");
            return None;
        }
    };
    tracing::info!(
        "loaded {name}: {} tracks, {} items, {} markers, {} regions, {} tempo points",
        proj.track_count,
        proj.item_count,
        proj.marker_count,
        proj.region_count,
        proj.tempo_point_count,
    );
    let ctx = ProjectContext::Project(proj.project_guid.clone());

    // Tracks + items (+ real REAPER peak caches for the waveforms).
    let mut tracks = build_track_views(engine, &ctx, false);
    let mut peaks_cache: std::collections::HashMap<String, Option<Rc<ReaPeaks>>> =
        std::collections::HashMap::new();
    let mut end = 0.0f64;
    for (i, view) in tracks.iter_mut().enumerate() {
        let items = Items::get_items(engine, ctx.clone(), TrackRef::Index(i as u32));
        let clips: Vec<ClipView> = items
            .iter()
            .map(|item| {
                let clip = clip_from_item(engine, &ctx, item, &project_dir, &mut peaks_cache);
                end = end.max(clip.start + clip.length);
                clip
            })
            .collect();
        view.clips = clips;
    }

    // Markers / regions / tempo map.
    let markers = MarkersService::all(engine, ctx.clone())
        .into_iter()
        .enumerate()
        .map(|(i, m)| MarkerView {
            time: m.position_seconds(),
            color: m.color.map(u32_to_hex),
            idx: m.id.unwrap_or(i as u32 + 1),
            name: m.name,
        })
        .collect();
    let regions = RegionsService::all(engine, ctx.clone())
        .into_iter()
        .enumerate()
        .map(|(i, r)| RegionView {
            start: r
                .time_range
                .start
                .time
                .as_ref()
                .map(|t| t.as_seconds())
                .unwrap_or(0.0),
            end: r
                .time_range
                .end
                .time
                .as_ref()
                .map(|t| t.as_seconds())
                .unwrap_or(0.0),
            color: r.color.map(u32_to_hex),
            idx: r.id.unwrap_or(i as u32 + 1),
            name: r.name,
        })
        .collect();
    let tempo_markers: Vec<TempoMarkerView> = TempoMap::get_tempo_points(engine, ctx.clone())
        .into_iter()
        .map(|t| {
            let (num, den) = t
                .time_signature
                .map(|ts| (ts.numerator, ts.denominator))
                .unwrap_or((4, 4));
            TempoMarkerView {
                time: t.position_seconds(),
                bpm: t.bpm,
                num,
                den,
            }
        })
        .collect();
    let bpm = TempoMap::get_tempo_at(engine, ctx.clone(), 0.0);
    let (num, _den) = TempoMap::get_time_signature_at(engine, ctx.clone(), 0.0);

    Some(BootedProject {
        engine: engine.clone(),
        ctx,
        rpp_guid: Some(proj.project_guid),
        tracks,
        markers,
        regions,
        tempo_markers,
        bpm: if bpm > 1.0 { bpm } else { 120.0 },
        beats_per_measure: if num > 0 { num as u32 } else { 4 },
        seconds: (end + 16.0).max(60.0),
    })
}

/// One arrange clip from an engine item: active take's name, the item
/// colour/fades/states, and waveform columns from the source's `.reapeaks`
/// cache (REAPER keeps them next to the media or in a `peaks/` sibling).
fn clip_from_item(
    engine: &Standalone,
    ctx: &ProjectContext,
    item: &Item,
    project_dir: &std::path::Path,
    cache: &mut std::collections::HashMap<String, Option<Rc<ReaPeaks>>>,
) -> ClipView {
    let take = Takes::get_active_take(engine, ctx.clone(), ItemRef::Guid(item.guid.clone()));
    let name = take
        .as_ref()
        .map(|t| t.name.clone())
        .filter(|n| !n.is_empty())
        .unwrap_or_default();
    let start = item.position.as_seconds();
    let length = item.length.as_seconds();

    let mut clip = ClipView::new(start, length, name, None);
    clip.color = item
        .color
        .or(take.as_ref().and_then(|t| t.color))
        .map(u32_to_hex);
    clip.fade_in = item.fade_in_length.as_seconds();
    clip.fade_out = item.fade_out_length.as_seconds();
    clip.muted = item.muted;
    clip.selected = item.selected;

    // Waveform from the REAPER peak cache, windowed by the take's source
    // offset + play rate (REAPER's source mapping).
    if let Some(take) = &take
        && let Some(src) = &take.source_file_path
    {
        let peaks = cache
            .entry(src.clone())
            .or_insert_with(|| load_reapeaks_for(src, project_dir).map(Rc::new))
            .clone();
        if let Some(peaks) = peaks {
            let rate = if take.play_rate > 0.0 {
                take.play_rate
            } else {
                1.0
            };
            let s0 = take.start_offset.as_seconds();
            let s1 = s0 + length * rate;
            let cols = ((length * 12.0) as usize).clamp(8, 2000);
            clip.peaks = peaks.columns(0, s0, s1, cols);
        }
    }
    clip
}

/// Find the `.reapeaks` cache for a media file: `<file>.reapeaks` beside it
/// or in a `peaks/` sibling (REAPER's default layouts), trying the source's
/// own directory and the project's media folders — converted sessions often
/// carry source paths from another machine, so the filename is the anchor.
fn load_reapeaks_for(source: &str, project_dir: &std::path::Path) -> Option<ReaPeaks> {
    let src = std::path::Path::new(source);
    let file = src.file_name()?.to_str()?;
    let peak_name = format!("{file}.reapeaks");

    let mut candidates: Vec<std::path::PathBuf> = Vec::new();
    if let Some(dir) = src.parent() {
        candidates.push(dir.join(&peak_name));
        candidates.push(dir.join("peaks").join(&peak_name));
    }
    for sub in ["", "Audio Files", "Bounced Files", "Media"] {
        let base = if sub.is_empty() {
            project_dir.to_path_buf()
        } else {
            project_dir.join(sub)
        };
        candidates.push(base.join(&peak_name));
        candidates.push(base.join("peaks").join(&peak_name));
    }
    candidates
        .iter()
        .find(|p| p.exists())
        .and_then(|p| ReaPeaks::read(p).ok())
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
        match Tracks::add(engine, ProjectContext::Current, name, None) {
            Ok(guid) => {
                let tref = TrackRef::Guid(guid);
                let _ = Tracks::set_color(engine, ProjectContext::Current, tref.clone(), *color);
                let _ = Tracks::set_volume(engine, ProjectContext::Current, tref.clone(), *volume);
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
fn build_track_views(engine: &Standalone, ctx: &ProjectContext, demo: bool) -> Vec<TrackView> {
    let mut depth: i32 = 0;
    let mut views = Vec::new();
    for (i, track) in Tracks::all(engine, ctx.clone()).iter().enumerate() {
        views.push(track_to_view(i, track, depth.max(0) as u32, demo));
        depth = (depth + track.folder_depth).max(0);
    }
    views
}

/// Adapt one engine [`Track`] into a [`TrackView`] at the given absolute depth.
fn track_to_view(id: usize, track: &Track, depth: u32, demo: bool) -> TrackView {
    let color = track.color.map(u32_to_hex);
    let mut view = TrackView::new(id, &track.name, color.as_deref())
        .fader(track.volume as f32)
        .depth(depth);
    if track.is_folder {
        view = view.folder();
    } else {
        view = view.stereo(); // dual-column meter
        if demo {
            view = view.clips(demo_clips(id)); // demo items
        }
    }
    if demo && track.name == "BASS" {
        view = view.envelopes(vec![demo_envelope()]);
    }
    // Mirror the engine's per-track state into the shared UI signals.
    view.mute.set(track.muted);
    view.solo.set(track.soloed);
    view.record_arm.set(track.armed);
    view
}

/// Demo arrangement clips for an audible track: a couple of bars with fades,
/// staggered per track so the lanes read like a song (one muted clip on the
/// later tracks shows the mute overlay). Waveforms come from real REAPER
/// `.reapeaks` caches when a corpus is available (`FTS_REAPEAKS_DIR`),
/// synthetic peaks otherwise.
fn demo_clips(id: usize) -> Vec<ClipView> {
    let off = (id % 4) as f64 * 4.0;
    let mut a = ClipView::new(off, 16.0 - off, "Verse", None);
    a.fade_in = 0.8;
    a.fade_out = 1.5;
    a.peaks = demo_peaks(id, (a.length * 12.0) as usize);
    let mut b = ClipView::new(24.0, 16.0, "Chorus", None);
    b.fade_in = 0.4;
    b.fade_out = 2.0;
    b.selected = id == 2;
    b.muted = id == 7;
    b.peaks = demo_peaks(id + 3, (b.length * 12.0) as usize);
    vec![a, b]
}

/// Waveform peaks for the demo clips: REAPER `.reapeaks` files when present
/// (drawn exactly as REAPER would — mipmap pick + per-column min/max),
/// synthetic otherwise.
fn demo_peaks(seed: usize, columns: usize) -> Vec<(f32, f32)> {
    real_peaks(seed, columns).unwrap_or_else(|| synth_peaks(seed, columns))
}

/// Pull columns out of the n-th `.reapeaks` file in the corpus dir.
fn real_peaks(seed: usize, columns: usize) -> Option<Vec<(f32, f32)>> {
    use daw_standalone::reapeaks::ReaPeaks;
    let dir = match std::env::var("FTS_REAPEAKS_DIR") {
        Ok(d) => d,
        Err(_) => format!(
            "{}/Documents/REAPER Media/peaks",
            std::env::var("HOME").ok()?
        ),
    };
    let mut files: Vec<_> = std::fs::read_dir(dir)
        .ok()?
        .flatten()
        .map(|e| e.path())
        .filter(|p| p.extension().is_some_and(|e| e == "reapeaks"))
        .collect();
    if files.is_empty() {
        return None;
    }
    files.sort();
    // Prefer longer sources (they have interesting waveforms) by probing a
    // few candidates around the seed.
    let pick = files[seed % files.len()].clone();
    let peaks = ReaPeaks::read(&pick).ok()?;
    let len = peaks.length_seconds().min(20.0);
    if len < 0.5 {
        return None;
    }
    let cols = peaks.columns(0, 0.0, len, columns.max(2));
    // All-silent caches are boring — fall back to synth.
    cols.iter()
        .any(|(max, min)| (max - min).abs() > 0.01)
        .then_some(cols)
}

/// Deterministic fake waveform peaks (per-track phase so lanes differ).
fn synth_peaks(seed: usize, n: usize) -> Vec<(f32, f32)> {
    (0..n.max(2))
        .map(|i| {
            let t = i as f32 / n.max(2) as f32;
            let ph = seed as f32 * 1.7;
            let env = 0.55 + 0.35 * ((t * 6.3 + ph).sin() * (t * 23.0 + ph * 2.0).sin());
            let a = (env * (0.6 + 0.4 * (t * 91.0 + ph).sin().abs())).clamp(0.05, 1.0);
            (a, -a * 0.8)
        })
        .collect()
}

/// Demo tempo markers (ruler tempo lane).
fn demo_tempo_markers() -> Vec<TempoMarkerView> {
    vec![
        TempoMarkerView { time: 0.0, bpm: 120.0, num: 4, den: 4 },
        TempoMarkerView { time: 40.0, bpm: 140.0, num: 7, den: 8 },
    ]
}

/// A demo volume envelope (visible lane under the track).
fn demo_envelope() -> EnvelopeView {
    EnvelopeView {
        name: "Volume".to_string(),
        color: None,
        points: vec![
            (0.0, 0.75),
            (8.0, 0.75),
            (12.0, 0.4),
            (16.0, 0.4),
            (24.0, 0.9),
            (40.0, 0.6),
            (60.0, 0.6),
        ],
        height: 32,
        visible: true,
    }
}

/// Demo project markers (ruler marker lane).
fn demo_markers() -> Vec<MarkerView> {
    [(0.0, "Intro"), (16.0, "Verse"), (24.0, "Chorus"), (40.0, "Bridge")]
        .into_iter()
        .enumerate()
        .map(|(i, (t, n))| MarkerView {
            time: t,
            name: n.to_string(),
            color: None,
            idx: i as u32 + 1,
        })
        .collect()
}

/// Demo regions (ruler region lane).
fn demo_regions() -> Vec<RegionView> {
    [
        (0.0, 16.0, "Verse 1", "#7c5cff"),
        (24.0, 40.0, "Chorus", "#2dd4bf"),
    ]
    .into_iter()
    .enumerate()
    .map(|(i, (a, b, n, c))| RegionView {
        start: a,
        end: b,
        name: n.to_string(),
        color: Some(c.to_string()),
        idx: i as u32 + 1,
    })
    .collect()
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
    let tracks = Tracks::all(engine, ProjectContext::Current);
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
