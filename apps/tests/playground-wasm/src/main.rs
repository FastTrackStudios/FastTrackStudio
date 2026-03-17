//! WASM Playground — test daw-standalone components in the browser.
//!
//! Exercises transport, tracks, markers, and regions to verify
//! WASM compatibility of the daw-standalone crate.

use daw::service::{
    PlayState, ProjectContext, ProjectInfo, ProjectService, Transport, TransportService,
    marker::MarkerService,
    region::RegionService,
    track::{TrackRef, TrackService},
};
use daw::standalone::{
    StandaloneMarker, StandaloneProject, StandaloneRegion, StandaloneTrack, StandaloneTransport,
    audio_engine::{AudioEngine, TrackHandle, decode_audio_with_extension, rpp_loader, test_tone},
};
use dioxus::prelude::*;
use std::sync::Arc;

fn main() {
    #[cfg(target_arch = "wasm32")]
    tracing_wasm::set_as_global_default();

    launch(App);
}

/// Shared DAW state available to all components
#[derive(Clone)]
struct DawState {
    project: Arc<StandaloneProject>,
    transport: Arc<StandaloneTransport>,
    tracks: Arc<StandaloneTrack>,
    markers: Arc<StandaloneMarker>,
    regions: Arc<StandaloneRegion>,
}

impl PartialEq for DawState {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.project, &other.project)
    }
}

impl DawState {
    fn new() -> Self {
        let project = StandaloneProject::new();
        let shared = project.shared_state();
        let transport = StandaloneTransport::new(shared);

        Self {
            project: Arc::new(project),
            transport: Arc::new(transport),
            tracks: Arc::new(StandaloneTrack::new()),
            markers: Arc::new(StandaloneMarker::new()),
            regions: Arc::new(StandaloneRegion::new()),
        }
    }
}

#[component]
fn App() -> Element {
    let daw = use_signal(DawState::new);

    rsx! {
        div {
            style: "font-family: system-ui, sans-serif; max-width: 900px; margin: 0 auto; padding: 20px; color: #e0e0e0; background: #1a1a2e;",
            h1 { style: "color: #00d4ff; margin-bottom: 4px;", "FTS WASM Playground" }
            p { style: "color: #888; margin-top: 0;", "Testing daw-standalone components in the browser" }

            AudioPanel {}
            TransportPanel { daw: daw() }
            ProjectPanel { daw: daw() }
            TrackPanel { daw: daw() }
            MarkerPanel { daw: daw() }
            RegionPanel { daw: daw() }
        }
    }
}

// ─── Audio Engine ────────────────────────────────────────────────────────────

/// Track state for UI display
struct AudioTrackState {
    handle: TrackHandle,
    name: String,
    gain: f32,
    muted: bool,
    soloed: bool,
    duration: f64,
}

/// Ensure the audio engine is initialized, creating it on first call.
fn ensure_engine(
    engine: &mut Signal<Option<Arc<AudioEngine>>>,
    error_msg: &mut Signal<Option<String>>,
) -> Option<Arc<AudioEngine>> {
    if let Some(eng) = engine.read().as_ref() {
        return Some(eng.clone());
    }
    match AudioEngine::new() {
        Ok(eng) => {
            let eng = Arc::new(eng);
            engine.set(Some(eng.clone()));
            error_msg.set(None);
            Some(eng)
        }
        Err(e) => {
            error_msg.set(Some(format!("Failed to start audio: {e}")));
            None
        }
    }
}

/// Read files from a browser file input element by ID.
#[cfg(target_arch = "wasm32")]
async fn read_files_from_element(element_id: &str) -> Vec<(String, Vec<u8>)> {
    use wasm_bindgen::JsCast;
    use wasm_bindgen_futures::JsFuture;

    let window = web_sys::window().unwrap();
    let document = window.document().unwrap();
    let input: web_sys::HtmlInputElement = match document.get_element_by_id(element_id) {
        Some(el) => match el.dyn_into() {
            Ok(input) => input,
            Err(_) => return Vec::new(),
        },
        None => return Vec::new(),
    };

    let file_list = match input.files() {
        Some(fl) => fl,
        None => return Vec::new(),
    };

    let mut results = Vec::new();
    for i in 0..file_list.length() {
        let file = match file_list.get(i) {
            Some(f) => f,
            None => continue,
        };
        let name = file.name();
        let array_buffer = match JsFuture::from(file.array_buffer()).await {
            Ok(ab) => ab,
            Err(_) => continue,
        };
        let uint8 = js_sys::Uint8Array::new(&array_buffer);
        let bytes = uint8.to_vec();
        results.push((name, bytes));
    }
    results
}

#[component]
fn AudioPanel() -> Element {
    let mut engine: Signal<Option<Arc<AudioEngine>>> = use_signal(|| None);
    let mut audio_tracks: Signal<Vec<AudioTrackState>> = use_signal(Vec::new);
    let mut playing = use_signal(|| false);
    let mut position = use_signal(|| 0.0f64);
    let mut error_msg: Signal<Option<String>> = use_signal(|| None);
    let mut duration = use_signal(|| 0.0f64);
    let mut loading = use_signal(|| false);
    let mut status_msg: Signal<Option<String>> = use_signal(|| None);
    // Staged RPP text — set when user picks an RPP, audio files loaded separately
    let mut staged_rpp: Signal<Option<String>> = use_signal(|| None);
    let mut needed_files: Signal<Vec<String>> = use_signal(Vec::new);

    // Poll position
    let engine_poll = engine.read().clone();
    use_future(move || {
        let engine_poll = engine_poll.clone();
        async move {
            loop {
                if let Some(ref eng) = engine_poll {
                    position.set(eng.position_seconds());
                    playing.set(eng.is_playing());
                    duration.set(eng.duration_seconds());
                }
                #[cfg(target_arch = "wasm32")]
                gloo_timers::future::sleep(std::time::Duration::from_millis(50)).await;
                #[cfg(not(target_arch = "wasm32"))]
                tokio::time::sleep(std::time::Duration::from_millis(50)).await;
            }
        }
    });

    // Load demo tones
    let load_demo = move |_| {
        let Some(eng) = ensure_engine(&mut engine, &mut error_msg) else {
            return;
        };
        eng.clear_tracks();
        let sample_rate = eng.sample_rate();
        let demo = test_tone::demo_tracks(10.0, sample_rate);
        let mut states = Vec::new();
        for (name, audio) in demo {
            let dur = audio.duration_seconds();
            let handle = eng.add_track(audio);
            states.push(AudioTrackState {
                handle,
                name: name.to_string(),
                gain: 1.0,
                muted: false,
                soloed: false,
                duration: dur,
            });
        }
        audio_tracks.set(states);
        duration.set(eng.duration_seconds());
        eng.play();
    };

    // Open project folder via webkitdirectory file input
    let on_open_folder = move |evt: Event<FormData>| {
        #[cfg(target_arch = "wasm32")]
        {
            let file_data_list = evt.files();
            if file_data_list.is_empty() {
                return;
            }

            tracing::info!("Folder selected: {} files", file_data_list.len());
            loading.set(true);
            status_msg.set(Some(format!("Reading {} files...", file_data_list.len())));

            wasm_bindgen_futures::spawn_local(async move {
                let mut rpp_text: Option<String> = None;
                let mut audio_files: std::collections::HashMap<String, Vec<u8>> =
                    std::collections::HashMap::new();

                for file_data in &file_data_list {
                    let name = file_data.name();
                    let lower = name.to_lowercase();

                    // Skip non-relevant files (images, peaks, etc.)
                    if lower.ends_with(".rpp") {
                        match file_data.read_string().await {
                            Ok(text) => {
                                tracing::info!("RPP: {} ({} bytes)", name, text.len());
                                rpp_text = Some(text);
                            }
                            Err(e) => tracing::warn!("Failed to read RPP {}: {:?}", name, e),
                        }
                    } else if lower.ends_with(".wav")
                        || lower.ends_with(".mp3")
                        || lower.ends_with(".ogg")
                        || lower.ends_with(".flac")
                        || lower.ends_with(".aac")
                        || lower.ends_with(".m4a")
                    {
                        match file_data.read_bytes().await {
                            Ok(bytes) => {
                                tracing::info!("Audio: {} ({} bytes)", name, bytes.len());
                                audio_files.insert(name, bytes.to_vec());
                            }
                            Err(e) => tracing::warn!("Failed to read {}: {:?}", name, e),
                        }
                    }
                }

                let Some(rpp) = rpp_text else {
                    error_msg.set(Some("No .RPP file found in folder".to_string()));
                    loading.set(false);
                    status_msg.set(None);
                    return;
                };

                status_msg.set(Some(format!(
                    "Decoding {} audio files...",
                    audio_files.len()
                )));

                let Some(eng) = ensure_engine(&mut engine, &mut error_msg) else {
                    loading.set(false);
                    return;
                };

                eng.stop();
                eng.clear_tracks();
                audio_tracks.write().clear();

                match rpp_loader::load_rpp(&eng, &rpp, |file_path| {
                    // Try exact match
                    if let Some(bytes) = audio_files.get(file_path) {
                        return Some(bytes.clone());
                    }
                    // Try filename only (strip directory)
                    let filename = file_path.rsplit(['/', '\\']).next().unwrap_or(file_path);
                    let filename_lower = filename.to_lowercase();
                    for (key, val) in &audio_files {
                        let key_base = key.rsplit(['/', '\\']).next().unwrap_or(key);
                        if key_base.to_lowercase() == filename_lower {
                            return Some(val.clone());
                        }
                    }
                    None
                }) {
                    Ok(project) => {
                        let mut states = Vec::new();
                        for track in &project.tracks {
                            states.push(AudioTrackState {
                                handle: track.handle,
                                name: track.track_name.clone(),
                                gain: 1.0,
                                muted: false,
                                soloed: false,
                                duration: track.audio_duration,
                            });
                        }
                        audio_tracks.set(states);
                        duration.set(project.duration);

                        let msg = if project.failed.is_empty() {
                            format!(
                                "Loaded {} tracks ({:.0}s)",
                                project.tracks.len(),
                                project.duration
                            )
                        } else {
                            format!(
                                "Loaded {} tracks, {} failed: {}",
                                project.tracks.len(),
                                project.failed.len(),
                                project
                                    .failed
                                    .iter()
                                    .map(|(f, _)| f.as_str())
                                    .collect::<Vec<_>>()
                                    .join(", ")
                            )
                        };
                        status_msg.set(Some(msg));
                        error_msg.set(None);
                        eng.play();
                    }
                    Err(e) => {
                        error_msg.set(Some(format!("Failed to load: {e}")));
                    }
                }

                loading.set(false);
            });
        }
    };

    let pos = *position.read();
    let dur = *duration.read();
    let is_playing = *playing.read();
    let is_loading = *loading.read();
    let has_engine = engine.read().is_some();
    let track_count = audio_tracks.read().len();

    rsx! {
        div {
            style: "background: #16213e; border-radius: 8px; padding: 16px; margin: 12px 0; border: 2px solid #00d4ff;",
            h2 { style: "color: #00d4ff; margin-top: 0;", "Audio Engine (cpal + symphonia)" }

            if let Some(ref err) = *error_msg.read() {
                p { style: "color: #ff4444; background: #2a1010; padding: 8px; border-radius: 4px; margin: 8px 0;", "{err}" }
            }
            if let Some(ref msg) = *status_msg.read() {
                p { style: "color: #88ccff; background: #0a2040; padding: 8px; border-radius: 4px; margin: 8px 0; font-size: 0.9em;", "{msg}" }
            }

            // File loading controls
            div { style: "display: flex; gap: 8px; align-items: center; margin-bottom: 12px; flex-wrap: wrap;",
                // Open project folder — uses webkitdirectory for cross-browser support
                label {
                    style: "padding: 10px 20px; border-radius: 4px; border: 2px dashed #00d4ff; cursor: pointer; font-weight: bold; color: #00d4ff; background: #0a2040; display: inline-flex; align-items: center; gap: 6px;",
                    if is_loading { "Loading..." } else { "Open Project Folder" }
                    input {
                        r#type: "file",
                        // webkitdirectory makes the browser show a folder picker
                        // and returns ALL files in the folder recursively
                        "webkitdirectory": "true",
                        multiple: true,
                        style: "display: none;",
                        onchange: on_open_folder,
                    }
                }
                button {
                    style: "padding: 10px 16px; border-radius: 4px; border: 1px solid #444; cursor: pointer; background: #2a2a4a; color: #e0e0e0;",
                    onclick: load_demo,
                    "Demo Tones"
                }
                if has_engine {
                    button {
                        style: "padding: 10px 16px; border-radius: 4px; border: 1px solid #663333; cursor: pointer; background: #2a1a1a; color: #ff6666;",
                        onclick: move |_| {
                            if let Some(ref eng) = *engine.read() {
                                eng.stop();
                                eng.clear_tracks();
                            }
                            audio_tracks.write().clear();
                            status_msg.set(None);
                        },
                        "Clear All"
                    }
                }
            }

            // Show needed files
            if !needed_files.read().is_empty() && track_count == 0 {
                div { style: "margin-bottom: 12px; padding: 8px; background: #0d1117; border-radius: 4px; font-size: 0.85em;",
                    p { style: "color: #888; margin: 0 0 4px 0;", "Audio files needed:" }
                    for f in needed_files.read().iter() {
                        {
                            let basename = f.rsplit(['/', '\\']).next().unwrap_or(f);
                            rsx! { div { style: "color: #aaa; padding: 1px 0;", "{basename}" } }
                        }
                    }
                }
            }

            if track_count > 0 {
                // Transport controls
                div { style: "display: flex; gap: 8px; align-items: center; margin-bottom: 12px;",
                    button {
                        style: "padding: 10px 20px; border-radius: 4px; border: none; cursor: pointer; font-weight: bold; font-size: 1.1em; background: #00d4ff; color: #1a1a2e;",
                        onclick: move |_| {
                            if let Some(ref eng) = *engine.read() {
                                if eng.is_playing() { eng.pause(); } else { eng.play(); }
                            }
                        },
                        if is_playing { "Pause" } else { "Play" }
                    }
                    button {
                        style: "padding: 10px 16px; border-radius: 4px; border: 1px solid #444; cursor: pointer; background: #2a2a4a; color: #e0e0e0;",
                        onclick: move |_| {
                            if let Some(ref eng) = *engine.read() { eng.stop(); }
                        },
                        "Stop"
                    }
                    button {
                        style: "padding: 10px 16px; border-radius: 4px; border: 1px solid #444; cursor: pointer; background: #2a2a4a; color: #e0e0e0;",
                        onclick: move |_| {
                            if let Some(ref eng) = *engine.read() { eng.seek(0.0); }
                        },
                        "Rewind"
                    }
                }

                // Position bar
                div { style: "margin-bottom: 12px;",
                    div { style: "display: flex; justify-content: space-between; font-size: 0.85em; color: #888; margin-bottom: 4px;",
                        {
                            let mins = (pos / 60.0) as i32;
                            let secs = pos % 60.0;
                            let dur_mins = (dur / 60.0) as i32;
                            let dur_secs = dur % 60.0;
                            rsx! {
                                span { "{mins}:{secs:04.1}" }
                                span { "{dur_mins}:{dur_secs:04.1}" }
                            }
                        }
                    }
                    {
                        let pct = if dur > 0.0 { pos / dur * 100.0 } else { 0.0 };
                        rsx! {
                            div { style: "height: 6px; background: #0d1117; border-radius: 3px; overflow: hidden;",
                                div {
                                    style: "height: 100%; background: #00d4ff; transition: width 0.05s; width: {pct}%;",
                                }
                            }
                        }
                    }
                }

                // Per-track mixer
                h3 { style: "color: #aaa; margin-bottom: 8px; font-size: 0.9em; text-transform: uppercase; letter-spacing: 1px;",
                    "Mixer ({track_count} tracks)"
                }
                for (i, track) in audio_tracks.read().iter().enumerate() {
                    {
                        let handle = track.handle;
                        let muted = track.muted;
                        let soloed = track.soloed;
                        let gain = track.gain;
                        let track_dur = track.duration;
                        rsx! {
                            div { style: "display: flex; gap: 8px; align-items: center; padding: 6px 0; border-top: 1px solid #333;",
                                span { style: "width: 150px; font-size: 0.85em; color: #ccc; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;",
                                    title: "{track.name}",
                                    "{track.name}"
                                }
                                span { style: "width: 50px; font-size: 0.75em; color: #666;", "{track_dur:.1}s" }
                                input {
                                    r#type: "range",
                                    min: "0",
                                    max: "100",
                                    value: "{(gain * 100.0) as i32}",
                                    style: "flex: 1; accent-color: #00d4ff;",
                                    oninput: move |evt| {
                                        let val: f32 = evt.value().parse().unwrap_or(100.0) / 100.0;
                                        if let Some(ref eng) = *engine.read() {
                                            eng.set_track_gain(handle, val);
                                        }
                                        audio_tracks.write()[i].gain = val;
                                    },
                                }
                                span { style: "width: 35px; font-size: 0.8em; color: #888; text-align: right;", "{(gain * 100.0) as i32}%" }
                                button {
                                    style: if muted { "color: #ff4444; border: 1px solid #ff4444; background: #2a1010; cursor: pointer; border-radius: 3px; padding: 2px 8px; font-weight: bold;" }
                                           else { "color: #666; border: 1px solid #444; background: transparent; cursor: pointer; border-radius: 3px; padding: 2px 8px;" },
                                    onclick: move |_| {
                                        let new_muted = !muted;
                                        if let Some(ref eng) = *engine.read() { eng.set_track_muted(handle, new_muted); }
                                        audio_tracks.write()[i].muted = new_muted;
                                    },
                                    "M"
                                }
                                button {
                                    style: if soloed { "color: #ffdd00; border: 1px solid #ffdd00; background: #2a2a10; cursor: pointer; border-radius: 3px; padding: 2px 8px; font-weight: bold;" }
                                           else { "color: #666; border: 1px solid #444; background: transparent; cursor: pointer; border-radius: 3px; padding: 2px 8px;" },
                                    onclick: move |_| {
                                        let new_soloed = !soloed;
                                        if let Some(ref eng) = *engine.read() { eng.set_track_soloed(handle, new_soloed); }
                                        audio_tracks.write()[i].soloed = new_soloed;
                                    },
                                    "S"
                                }
                            }
                        }
                    }
                }
            } else if !has_engine {
                p { style: "color: #666; font-style: italic;",
                    "Select an .RPP file + its audio files (WAV/MP3/OGG/FLAC), or try demo tones."
                }
            }
        }
    }
}

// ─── Transport ───────────────────────────────────────────────────────────────

#[component]
fn TransportPanel(daw: DawState) -> Element {
    let mut transport_state = use_signal(|| Transport::new());
    let mut position = use_signal(|| 0.0f64);

    // Poll transport state
    let transport = daw.transport.clone();
    use_future(move || {
        let transport = transport.clone();
        async move {
            loop {
                let state = transport.get_state(ProjectContext::Current).await;
                let pos = transport.get_position(ProjectContext::Current).await;
                transport_state.set(state);
                position.set(pos);
                #[cfg(target_arch = "wasm32")]
                gloo_timers::future::sleep(std::time::Duration::from_millis(100)).await;
                #[cfg(not(target_arch = "wasm32"))]
                tokio::time::sleep(std::time::Duration::from_millis(100)).await;
            }
        }
    });

    let play_state = transport_state.read().play_state;
    let pos = *position.read();
    let tempo = transport_state.read().tempo.bpm();

    let transport_play = daw.transport.clone();
    let transport_stop = daw.transport.clone();
    let transport_seek = daw.transport.clone();

    rsx! {
        div {
            style: "background: #16213e; border-radius: 8px; padding: 16px; margin: 12px 0;",
            h2 { style: "color: #00d4ff; margin-top: 0;", "Transport" }

            div { style: "display: flex; gap: 8px; align-items: center; margin-bottom: 12px;",
                button {
                    style: "padding: 8px 16px; border-radius: 4px; border: none; cursor: pointer; font-weight: bold; background: #00d4ff; color: #1a1a2e;",
                    onclick: move |_| {
                        let t = transport_play.clone();
                        async move { t.play_stop(ProjectContext::Current).await; }
                    },
                    if play_state == PlayState::Playing { "Stop" } else { "Play" }
                }
                button {
                    style: "padding: 8px 16px; border-radius: 4px; border: 1px solid #444; cursor: pointer; background: #2a2a4a; color: #e0e0e0;",
                    onclick: move |_| {
                        let t = transport_stop.clone();
                        async move { t.goto_start(ProjectContext::Current).await; }
                    },
                    "Go to Start"
                }
                button {
                    style: "padding: 8px 16px; border-radius: 4px; border: 1px solid #444; cursor: pointer; background: #2a2a4a; color: #e0e0e0;",
                    onclick: move |_| {
                        let t = transport_seek.clone();
                        async move { t.set_position(ProjectContext::Current, 60.0).await; }
                    },
                    "Seek to 1:00"
                }
            }

            div { style: "display: grid; grid-template-columns: repeat(3, 1fr); gap: 12px;",
                StatusBadge { label: "State", value: format!("{play_state:?}") }
                StatusBadge { label: "Position", value: format!("{pos:.2}s") }
                StatusBadge { label: "Tempo", value: format!("{tempo:.0} BPM") }
            }
        }
    }
}

// ─── Projects ────────────────────────────────────────────────────────────────

#[component]
fn ProjectPanel(daw: DawState) -> Element {
    let mut projects = use_signal(Vec::<ProjectInfo>::new);
    let mut current = use_signal(|| None::<ProjectInfo>);

    let project = daw.project.clone();
    use_future(move || {
        let project = project.clone();
        async move {
            projects.set(project.list().await);
            current.set(project.get_current().await);
        }
    });

    let project_select = daw.project.clone();

    rsx! {
        div {
            style: "background: #16213e; border-radius: 8px; padding: 16px; margin: 12px 0;",
            h2 { style: "color: #00d4ff; margin-top: 0;", "Projects" }

            div { style: "display: flex; gap: 8px; flex-wrap: wrap;",
                for p in projects.read().iter() {
                    {
                        let is_current = current.read().as_ref().map(|c| &c.guid) == Some(&p.guid);
                        let guid = p.guid.clone();
                        let ps = project_select.clone();
                        rsx! {
                            button {
                                style: if is_current {
                                    "padding: 8px 16px; border-radius: 4px; border: 2px solid #00d4ff; cursor: pointer; font-weight: bold; background: #0a4870; color: #00d4ff;"
                                } else {
                                    "padding: 8px 16px; border-radius: 4px; border: 1px solid #444; cursor: pointer; background: #2a2a4a; color: #e0e0e0;"
                                },
                                onclick: move |_| {
                                    let ps = ps.clone();
                                    let guid = guid.clone();
                                    async move {
                                        ps.select(guid).await;
                                    }
                                },
                                "{p.name}"
                            }
                        }
                    }
                }
            }

            if let Some(c) = current.read().as_ref() {
                p { style: "color: #888; margin-bottom: 0; font-size: 0.85em;",
                    "Current: {c.name} ({c.guid})"
                }
            }
        }
    }
}

// ─── Tracks ──────────────────────────────────────────────────────────────────

#[component]
fn TrackPanel(daw: DawState) -> Element {
    let mut tracks = use_signal(Vec::new);

    let track_svc = daw.tracks.clone();
    use_future(move || {
        let track_svc = track_svc.clone();
        async move {
            tracks.set(track_svc.get_tracks(ProjectContext::Current).await);
        }
    });

    let tracks_mute = daw.tracks.clone();
    let tracks_solo = daw.tracks.clone();

    rsx! {
        div {
            style: "background: #16213e; border-radius: 8px; padding: 16px; margin: 12px 0;",
            h2 { style: "color: #00d4ff; margin-top: 0;", "Tracks ({tracks.read().len()})" }

            table { style: "width: 100%; border-collapse: collapse;",
                thead {
                    tr { style: "color: #888; text-align: left;",
                        th { style: "padding: 4px 8px;", "#" }
                        th { style: "padding: 4px 8px;", "Name" }
                        th { style: "padding: 4px 8px;", "Volume" }
                        th { style: "padding: 4px 8px;", "Mute" }
                        th { style: "padding: 4px 8px;", "Solo" }
                    }
                }
                tbody {
                    for track in tracks.read().iter() {
                        {
                            let guid = track.guid.clone();
                            let guid2 = track.guid.clone();
                            let tm = tracks_mute.clone();
                            let ts = tracks_solo.clone();
                            let muted = track.muted;
                            let soloed = track.soloed;
                            rsx! {
                                tr { style: "border-top: 1px solid #333;",
                                    td { style: "padding: 4px 8px; color: #888;", "{track.index}" }
                                    td { style: "padding: 4px 8px;", "{track.name}" }
                                    td { style: "padding: 4px 8px; color: #aaa;", "{track.volume:.2}" }
                                    td { style: "padding: 4px 8px;",
                                        button {
                                            style: if muted { "color: #ff4444; border: 1px solid #ff4444; background: transparent; cursor: pointer; border-radius: 3px; padding: 2px 8px;" }
                                                   else { "color: #666; border: 1px solid #444; background: transparent; cursor: pointer; border-radius: 3px; padding: 2px 8px;" },
                                            onclick: move |_| {
                                                let tm = tm.clone();
                                                let guid = guid.clone();
                                                async move {
                                                    tm.set_muted(ProjectContext::Current, TrackRef::Guid(guid), !muted).await;
                                                }
                                            },
                                            "M"
                                        }
                                    }
                                    td { style: "padding: 4px 8px;",
                                        button {
                                            style: if soloed { "color: #ffdd00; border: 1px solid #ffdd00; background: transparent; cursor: pointer; border-radius: 3px; padding: 2px 8px;" }
                                                   else { "color: #666; border: 1px solid #444; background: transparent; cursor: pointer; border-radius: 3px; padding: 2px 8px;" },
                                            onclick: move |_| {
                                                let ts = ts.clone();
                                                let guid2 = guid2.clone();
                                                async move {
                                                    ts.set_soloed(ProjectContext::Current, TrackRef::Guid(guid2), !soloed).await;
                                                }
                                            },
                                            "S"
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

// ─── Markers ─────────────────────────────────────────────────────────────────

#[component]
fn MarkerPanel(daw: DawState) -> Element {
    let mut markers = use_signal(Vec::new);

    let marker_svc = daw.markers.clone();
    use_future(move || {
        let marker_svc = marker_svc.clone();
        async move {
            markers.set(marker_svc.get_markers(ProjectContext::Current).await);
        }
    });

    rsx! {
        div {
            style: "background: #16213e; border-radius: 8px; padding: 16px; margin: 12px 0;",
            h2 { style: "color: #00d4ff; margin-top: 0;", "Markers ({markers.read().len()})" }

            div { style: "display: flex; gap: 6px; flex-wrap: wrap;",
                for marker in markers.read().iter() {
                    {
                        let color = marker.color.unwrap_or(0x888888);
                        let hex = format!("#{color:06x}");
                        rsx! {
                            span {
                                style: "padding: 4px 10px; border-radius: 4px; font-size: 0.85em; border: 1px solid {hex}; color: {hex};",
                                "{marker.name} @ {marker.position_seconds():.1}s"
                            }
                        }
                    }
                }
            }
        }
    }
}

// ─── Regions ─────────────────────────────────────────────────────────────────

#[component]
fn RegionPanel(daw: DawState) -> Element {
    let mut regions = use_signal(Vec::new);

    let region_svc = daw.regions.clone();
    use_future(move || {
        let region_svc = region_svc.clone();
        async move {
            regions.set(region_svc.get_regions(ProjectContext::Current).await);
        }
    });

    rsx! {
        div {
            style: "background: #16213e; border-radius: 8px; padding: 16px; margin: 12px 0;",
            h2 { style: "color: #00d4ff; margin-top: 0;", "Regions ({regions.read().len()})" }

            div { style: "display: flex; flex-direction: column; gap: 4px;",
                for region in regions.read().iter() {
                    {
                        let color = region.color.unwrap_or(0x888888);
                        let hex = format!("#{color:06x}");
                        let start = region.start_seconds();
                        let end = region.end_seconds();
                        let width_pct = ((end - start) / 300.0 * 100.0).min(100.0);
                        let left_pct = (start / 300.0 * 100.0).min(100.0);
                        rsx! {
                            div { style: "position: relative; height: 28px; background: #0d1117; border-radius: 4px; overflow: hidden;",
                                div {
                                    style: "position: absolute; left: {left_pct}%; width: {width_pct}%; height: 100%; background: {hex}22; border-left: 3px solid {hex}; display: flex; align-items: center; padding-left: 8px; font-size: 0.8em; color: {hex};",
                                    "{region.name} ({start:.0}s – {end:.0}s)"
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

// ─── Shared Components ───────────────────────────────────────────────────────

#[component]
fn StatusBadge(label: String, value: String) -> Element {
    rsx! {
        div { style: "background: #0d1117; border-radius: 6px; padding: 8px 12px;",
            div { style: "font-size: 0.75em; color: #888; text-transform: uppercase; letter-spacing: 1px;", "{label}" }
            div { style: "font-size: 1.2em; font-weight: bold; color: #e0e0e0;", "{value}" }
        }
    }
}
