//! `/session/{org}/{collection}/{song}` — the **session view**: a browser
//! multitrack player driven by the REAL fasttrackstudio session-ui components.
//!
//! Self-contained, client-side playback. The page is served by fts-server,
//! which also serves the song's media at `/media/songs/{slug}/…`:
//!
//! - `manifest.json` — title/key/bpm, the section map, and the stem list.
//! - `stems/*.ogg`   — one Opus file per stem.
//! - `chart.kf`      — the keyflow chart (optional).
//!
//! All fetched same-origin (no CORS). Each stem is **streamed** through an
//! `HTMLAudioElement` (progressive Opus/ogg — the browser range-requests and
//! decodes on the fly, so memory stays flat and playback starts fast). Every
//! element is routed into a Web Audio graph via
//! `AudioContext.createMediaElementSource` → its own `GainNode` →
//! destination, so the per-stem mixer (mute/solo/volume) drives the gains.
//! The elements all share one wall clock; element 0 is the master and a
//! ~10 Hz poll loop resyncs any stem that drifts past 50 ms.
//!
//! ## Presentation
//!
//! The engine is the same one the earlier streaming fix shipped. What changed
//! is the *presentation*: instead of bespoke divs we populate session-ui's
//! global signals from the player each tick and render the real session-ui
//! components (`SessionChartPane`, `SongTitle`, `SongProgressBar`,
//! `SectionProgressBar`, `TransportControlBar`, `MixerView`). This is the same
//! UI the fasttrackstudio wasm remote uses — here it is backed by the browser
//! Web-Audio mixer rather than a `SetlistService` RPC.
//!
//! Signals populated (see `session_ui::signals`):
//! - `SETLIST_STRUCTURE` — a one-song `session_proto::Setlist` built from the
//!   manifest (sections in seconds, tempo, time signature, chart text).
//! - `SONG_CHARTS[project_guid]` — `SongChartHydration { chart_text }`.
//! - `ACTIVE_INDICES` — song/section index + progress + is_playing, each tick.
//! - `SONG_TRANSPORT[0]` — `TransportState { position, bpm, ts, is_playing }`.
//! - `PLAYBACK_STATE` — Playing/Stopped (via `apply_active_indices`).
//!
//! The heavy lifting is `wasm32`-only (Web Audio + media elements). Off-wasm
//! the crate still has to compile (workspace member), so there's a tiny stub
//! below.

// ─────────────────────────────────────────────────────────────────────────────
// wasm32: the real player.
// ─────────────────────────────────────────────────────────────────────────────
#[cfg(target_arch = "wasm32")]
mod imp {
    use std::cell::RefCell;
    use std::rc::Rc;

    use dioxus::prelude::*;
    use serde::Deserialize;
    use wasm_bindgen::JsCast;
    use wasm_bindgen_futures::JsFuture;
    use web_sys::{AudioContext, GainNode, HtmlAudioElement, Response};

    use daw_proto::{MusicalPosition, Position, PositionInSeconds, TimeSignature, Track};
    use session_proto::{
        ActiveIndices, Section as SessionSection, SectionId, SectionType, Setlist,
        Song as SessionSong, SongChartHydration, SongId,
    };
    use session_ui::components::{
        MixerView, ProgressSection, SectionProgressBar, SongProgressBar, SongTitle,
        TransportControlBar,
    };
    use session_ui::{
        ACTIVE_INDICES, PLAYBACK_STATE, SETLIST_STRUCTURE, SONG_CHARTS, SONG_TRANSPORT,
        TransportState, apply_active_indices,
    };

    use crate::components::SessionChartPane;

    /// Drift tolerance (seconds) before a stem is snapped back to the master.
    const DRIFT_TOLERANCE: f64 = 0.05;
    /// `HTMLMediaElement.readyState` for HAVE_CURRENT_DATA.
    const HAVE_CURRENT_DATA: u16 = 2;
    /// Stable synthetic project guid for the single-song setlist.
    const PROJECT_GUID_PREFIX: &str = "web-session:";
    /// Transport poll interval (ms). ~10 Hz: smooth playhead + drift/buffering.
    const TICK_MS: u32 = 100;

    // ── manifest model ──────────────────────────────────────────────────────

    #[derive(Clone, Debug, PartialEq, Deserialize)]
    struct Manifest {
        #[allow(dead_code)]
        slug: Option<String>,
        title: Option<String>,
        artist: Option<String>,
        key: Option<String>,
        bpm: Option<f64>,
        time_signature: Option<String>,
        duration_sec: f64,
        #[serde(default)]
        sections: Vec<Section>,
        #[serde(default)]
        stems: Vec<StemSpec>,
    }

    #[derive(Clone, Debug, PartialEq, Deserialize)]
    struct Section {
        name: String,
        start_sec: f64,
        end_sec: f64,
    }

    #[derive(Clone, Debug, PartialEq, Deserialize)]
    struct StemSpec {
        name: String,
        #[serde(default)]
        group: Option<String>,
        file: String,
        #[serde(default)]
        default_muted: bool,
    }

    /// Per-stem UI/mix state, indexed parallel to `Manifest::stems`.
    #[derive(Clone, Copy)]
    struct StemUi {
        muted: bool,
        soloed: bool,
        volume: f32,
    }

    // ── the streaming Web Audio engine ──────────────────────────────────────

    /// One streamed stem: its media element (the actual audio source, streamed
    /// progressively) plus the gain node the mixer drives. The
    /// `MediaElementAudioSourceNode` is kept alive so the routing survives.
    struct StemNode {
        el: HtmlAudioElement,
        gain: GainNode,
        #[allow(dead_code)]
        node: web_sys::MediaElementAudioSourceNode,
    }

    /// The shared playback graph. Held in an `Rc<RefCell<…>>` so the resource
    /// future, the poll loop, and every event handler can drive it. Element 0
    /// is the master clock; there is no separate anchor arithmetic.
    struct EngineInner {
        #[allow(dead_code)]
        ctx: AudioContext,
        stems: Vec<StemNode>,
        duration: f64,
        playing: bool,
    }

    type Engine = Rc<RefCell<EngineInner>>;

    impl EngineInner {
        /// Current song position — the master element's playback time.
        fn position(&self) -> f64 {
            self.stems
                .first()
                .map(|s| s.el.current_time())
                .unwrap_or(0.0)
                .clamp(0.0, self.duration)
        }

        /// Resume the context, park every element at `offset`, and play them.
        fn play(&mut self, offset: f64) {
            let _ = self.ctx.resume();
            for s in &self.stems {
                s.el.set_current_time(offset);
                let _ = s.el.play();
            }
            self.playing = true;
        }

        fn pause(&mut self) {
            for s in &self.stems {
                let _ = s.el.pause();
            }
            self.playing = false;
        }

        /// Jump every element to `offset` (works whether or not playing —
        /// elements keep streaming from the new position).
        fn seek(&mut self, offset: f64) {
            for s in &self.stems {
                s.el.set_current_time(offset);
            }
        }

        fn set_stem_gain(&self, idx: usize, value: f32) {
            if let Some(stem) = self.stems.get(idx) {
                stem.gain.gain().set_value(value);
            }
        }

        /// Resync stems to the master (element 0). Never touches the master.
        fn correct_drift(&self) {
            let Some(master) = self.stems.first() else {
                return;
            };
            let m = master.el.current_time();
            for s in self.stems.iter().skip(1) {
                if (s.el.current_time() - m).abs() > DRIFT_TOLERANCE {
                    s.el.set_current_time(m);
                }
            }
        }

        /// How many stems have at least HAVE_CURRENT_DATA buffered.
        fn ready_count(&self) -> usize {
            self.stems
                .iter()
                .filter(|s| s.el.ready_state() >= HAVE_CURRENT_DATA)
                .count()
        }
    }

    /// Push the mixer state (mute/solo/volume) into the gain nodes. Solo wins:
    /// if anything is soloed, only soloed-and-unmuted stems are audible.
    fn apply_mix(eng: &Engine, ui: &[StemUi]) {
        let any_solo = ui.iter().any(|s| s.soloed);
        let e = eng.borrow();
        for (i, s) in ui.iter().enumerate() {
            let audible = if any_solo {
                s.soloed && !s.muted
            } else {
                !s.muted
            };
            e.set_stem_gain(i, if audible { s.volume } else { 0.0 });
        }
    }

    // ── fetch helpers (same-origin) ─────────────────────────────────────────

    async fn fetch_text(url: &str) -> Result<String, String> {
        let win = web_sys::window().ok_or_else(|| "no window".to_string())?;
        let resp_val = JsFuture::from(win.fetch_with_str(url))
            .await
            .map_err(|e| format!("fetch {url}: {e:?}"))?;
        let resp: Response = resp_val
            .dyn_into()
            .map_err(|_| "fetch did not return a Response".to_string())?;
        if !resp.ok() {
            return Err(format!("{url}: HTTP {}", resp.status()));
        }
        let promise = resp.text().map_err(|e| format!("{url}: text: {e:?}"))?;
        let val = JsFuture::from(promise)
            .await
            .map_err(|e| format!("{url}: text await: {e:?}"))?;
        val.as_string()
            .ok_or_else(|| format!("{url}: response was not text"))
    }

    async fn fetch_manifest(url: &str) -> Result<Manifest, String> {
        let txt = fetch_text(url).await?;
        serde_json::from_str(&txt).map_err(|e| format!("{url}: bad manifest json: {e}"))
    }

    /// Build the streaming graph: create the context, and for each stem create
    /// an `HTMLAudioElement` (progressive stream) routed through
    /// media-element-source → gain → destination. Synchronous and side-effect
    /// free apart from element creation — no fetch/decode, so no per-stem
    /// progress signal and nothing to re-fire on transport ticks.
    fn build_engine(slug: &str, manifest: &Manifest) -> Result<Engine, String> {
        let ctx = AudioContext::new().map_err(|e| format!("AudioContext: {e:?}"))?;
        let dest = ctx.destination();
        let mut stems = Vec::with_capacity(manifest.stems.len());
        for spec in &manifest.stems {
            let url = format!("/media/songs/{slug}/{}", spec.file);
            let el =
                HtmlAudioElement::new_with_src(&url).map_err(|e| format!("audio element: {e:?}"))?;
            el.set_preload("auto");
            el.set_loop(false);

            let node = ctx
                .create_media_element_source(&el)
                .map_err(|e| format!("media element source: {e:?}"))?;
            let gain = ctx.create_gain().map_err(|e| format!("create_gain: {e:?}"))?;
            // Deref coercion: &MediaElementAudioSourceNode / &GainNode → &AudioNode.
            let _ = node.connect_with_audio_node(&gain);
            let _ = gain.connect_with_audio_node(&dest);
            gain.gain()
                .set_value(if spec.default_muted { 0.0 } else { 1.0 });
            // Kick off buffering.
            el.load();

            stems.push(StemNode { el, gain, node });
        }

        Ok(Rc::new(RefCell::new(EngineInner {
            duration: manifest.duration_sec,
            ctx,
            stems,
            playing: false,
        })))
    }

    // ── manifest → session-proto mapping ────────────────────────────────────

    /// Parse a `"num/denom"` time-signature string, defaulting to 4/4.
    fn parse_time_sig(s: Option<&String>) -> (u32, u32) {
        s.and_then(|t| {
            let (n, d) = t.split_once('/')?;
            Some((n.trim().parse().ok()?, d.trim().parse().ok()?))
        })
        .filter(|(n, d)| *n > 0 && *d > 0)
        .unwrap_or((4, 4))
    }

    /// Map a section name to a keyflow `SectionType` (for colors / abbrev),
    /// falling back to a custom type so unknown names still render.
    fn section_type_of(name: &str) -> SectionType {
        SectionType::parse(name).unwrap_or_else(|_| SectionType::Custom(name.to_string()))
    }

    /// Build a `session_proto::Section` from a manifest section — reused for
    /// both the `SETLIST_STRUCTURE` song and the progress-bar segments so
    /// colors/short-names come from one place.
    fn to_session_section(sec: &Section) -> SessionSection {
        SessionSection {
            section_id: SectionId::new(),
            id: None,
            name: sec.name.clone(),
            comment: None,
            section_type: section_type_of(&sec.name),
            start_seconds: sec.start_sec,
            end_seconds: sec.end_sec,
            number: None,
            color: None,
        }
    }

    /// Build the one-song `Setlist` that drives session-ui from the manifest.
    fn build_setlist(slug: &str, manifest: &Manifest, chart_text: Option<String>) -> Setlist {
        let (ts_num, ts_denom) = parse_time_sig(manifest.time_signature.as_ref());
        let sections: Vec<SessionSection> = manifest.sections.iter().map(to_session_section).collect();
        let song = SessionSong {
            id: SongId::new(),
            name: manifest.title.clone().unwrap_or_default(),
            project_guid: format!("{PROJECT_GUID_PREFIX}{slug}"),
            start_seconds: 0.0,
            end_seconds: manifest.duration_sec,
            count_in_seconds: None,
            sections,
            comments: Vec::new(),
            tempo: manifest.bpm,
            time_signature: Some(TimeSignature::new(ts_num, ts_denom)),
            measure_positions: Vec::new(),
            chart_text,
            parsed_chart: None,
            detected_chords: Vec::new(),
            chart_fingerprint: None,
            advance_mode: None,
            color: None,
        };
        Setlist {
            id: Some(slug.to_string()),
            name: manifest.title.clone().unwrap_or_default(),
            advance_mode: session_proto::AdvanceMode::Wait,
            songs: vec![song],
        }
    }

    /// Progress-bar segments (percent of song duration) from the manifest.
    fn progress_sections(manifest: &Manifest) -> Vec<ProgressSection> {
        let dur = manifest.duration_sec.max(0.001);
        manifest
            .sections
            .iter()
            .map(|sec| {
                let s = to_session_section(sec);
                ProgressSection {
                    start_percent: (sec.start_sec / dur * 100.0).clamp(0.0, 100.0),
                    end_percent: (sec.end_sec / dur * 100.0).clamp(0.0, 100.0),
                    color: s.bright_color(),
                    name: s.display_name(),
                    short_name: s.short_display(),
                    comment: None,
                }
            })
            .collect()
    }

    /// Musical position (measure.beat.subdivision) from seconds + tempo/meter.
    fn musical_at(seconds: f64, bpm: f64, ts_num: u32) -> MusicalPosition {
        let bpm = if bpm > 0.0 { bpm } else { 120.0 };
        let num = ts_num.max(1) as f64;
        let beats_total = (seconds.max(0.0)) * bpm / 60.0;
        let measure = (beats_total / num).floor();
        let beat_in_measure = beats_total - measure * num;
        let beat = beat_in_measure.floor();
        let subdivision = ((beat_in_measure - beat) * 1000.0).round().clamp(0.0, 999.0);
        MusicalPosition::new(measure as i32, beat as i32, subdivision as i32)
    }

    /// Whether a stem is part of the click/guide bus (Click + Cue/Guide stems).
    fn is_guide_stem(spec: &StemSpec) -> bool {
        let hay = format!(
            "{} {}",
            spec.name.to_lowercase(),
            spec.group.as_deref().unwrap_or("").to_lowercase()
        );
        hay.contains("click") || hay.contains("guide") || hay.contains("cue")
    }

    /// Deterministic accent color (0xRRGGBB) for a stem group.
    fn group_color(group: &str) -> u32 {
        // Small fixed palette, hashed by group name for stable coloring.
        const PALETTE: [u32; 8] = [
            0x3B82F6, // blue
            0x22C55E, // green
            0xF59E0B, // amber
            0xEF4444, // red
            0xA855F7, // purple
            0x14B8A6, // teal
            0xEC4899, // pink
            0xF97316, // orange
        ];
        let sum: u32 = group.bytes().map(|b| b as u32).sum();
        PALETTE[(sum as usize) % PALETTE.len()]
    }

    /// Map the per-stem UI state to a flat `daw_proto::Track` list for MixerView.
    fn stems_to_tracks(manifest: &Manifest, ui: &[StemUi]) -> Vec<Track> {
        manifest
            .stems
            .iter()
            .enumerate()
            .map(|(i, spec)| {
                let st = ui.get(i).copied().unwrap_or(StemUi {
                    muted: spec.default_muted,
                    soloed: false,
                    volume: 1.0,
                });
                let mut t = Track::new(spec.file.clone(), i as u32, spec.name.clone());
                t.color = Some(group_color(spec.group.as_deref().unwrap_or("Other")));
                t.muted = st.muted;
                t.soloed = st.soloed;
                t.volume = st.volume as f64;
                t
            })
            .collect()
    }

    // ── small format helpers ────────────────────────────────────────────────

    fn fmt_time(s: f64) -> String {
        let s = s.max(0.0);
        let m = (s / 60.0) as u64;
        let sec = (s % 60.0) as u64;
        format!("{m}:{sec:02}")
    }

    // ── the component ───────────────────────────────────────────────────────

    #[component]
    pub fn SongSession(org: String, collection: String, song: String) -> Element {
        let mut playing = use_signal(|| false);
        let mut position = use_signal(|| 0.0_f64);
        let mut buffering = use_signal(|| true);
        // Per-stem mixer state; filled once the manifest lands (see effect).
        let mut stem_ui = use_signal(Vec::<StemUi>::new);

        // Fetch manifest → build the streaming graph. Keyed on the song slug
        // via `use_reactive!`: the future reads ONLY the `song` prop (no
        // signals), so it runs exactly once per song and never re-fires on
        // transport ticks / playhead updates.
        let song_r = song.clone();
        let loaded = use_resource(use_reactive!(|song_r| {
            let slug = song_r.clone();
            async move {
                let manifest = fetch_manifest(&format!("/media/songs/{slug}/manifest.json")).await?;
                let eng = build_engine(&slug, &manifest)?;
                Ok::<(Manifest, Engine), String>((manifest, eng))
            }
        }));

        // Chart source (optional). Fetched async; populates SONG_CHARTS +
        // SETLIST_STRUCTURE once present (see effect below).
        let song_c = song.clone();
        let mut chart_src = use_signal(String::new);
        use_future(move || {
            let slug = song_c.clone();
            async move {
                if let Ok(txt) = fetch_text(&format!("/media/songs/{slug}/chart.kf")).await {
                    chart_src.set(txt);
                }
            }
        });

        // Clone the engine Rc out of the resource (or None while loading).
        let engine_of = move || -> Option<Engine> {
            loaded
                .read()
                .as_ref()
                .and_then(|r| r.as_ref().ok())
                .map(|(_, e)| e.clone())
        };

        // Initialize the mixer state + populate SETLIST_STRUCTURE / SONG_CHARTS
        // once the engine is ready. Runs once per song (guarded by empty ui).
        let slug_for_setlist = song.clone();
        use_effect(move || {
            if let Some(Ok((m, eng))) = &*loaded.read() {
                if stem_ui.read().is_empty() && !m.stems.is_empty() {
                    let v: Vec<StemUi> = m
                        .stems
                        .iter()
                        .map(|s| StemUi {
                            muted: s.default_muted,
                            soloed: false,
                            volume: 1.0,
                        })
                        .collect();
                    apply_mix(eng, &v);
                    stem_ui.set(v);

                    // Populate the session-ui structural signals.
                    let chart = chart_src.peek().clone();
                    let chart_opt = (!chart.is_empty()).then(|| chart.clone());
                    let setlist = build_setlist(&slug_for_setlist, m, chart_opt);
                    let guid = setlist
                        .songs
                        .first()
                        .map(|s| s.project_guid.clone())
                        .unwrap_or_default();
                    *SETLIST_STRUCTURE.write() = setlist;
                    if !chart.is_empty() {
                        SONG_CHARTS.write().insert(
                            guid,
                            SongChartHydration {
                                project_guid: String::new(),
                                chart_text: chart,
                                detected_chords: Vec::new(),
                                chart_fingerprint: String::new(),
                            },
                        );
                    }
                }
            }
        });

        // Chart arrived after the structure was built: backfill SONG_CHARTS +
        // the song's chart_text so SessionChartPane picks it up.
        use_effect(move || {
            let chart = chart_src.read().clone();
            if chart.is_empty() {
                return;
            }
            let mut setlist = SETLIST_STRUCTURE.write();
            if let Some(song) = setlist.songs.first_mut() {
                let guid = song.project_guid.clone();
                if song.chart_text.as_deref() != Some(chart.as_str()) {
                    song.chart_text = Some(chart.clone());
                    drop(setlist);
                    SONG_CHARTS.write().insert(
                        guid,
                        SongChartHydration {
                            project_guid: String::new(),
                            chart_text: chart,
                            detected_chords: Vec::new(),
                            chart_fingerprint: String::new(),
                        },
                    );
                }
            }
        });

        // ~10 Hz loop: readiness (buffering), drift correction, playhead, end,
        // and session-ui signal population (ACTIVE_INDICES / SONG_TRANSPORT).
        // A short timeout (~4s) enables Play even if a stem is slow to buffer.
        use_future(move || async move {
            let mut ticks: u32 = 0;
            loop {
                gloo_timers::future::TimeoutFuture::new(TICK_MS).await;
                ticks += 1;
                let Some(eng) = engine_of() else {
                    continue;
                };
                let (rc, total, is_playing, pos, dur) = {
                    let e = eng.borrow();
                    (
                        e.ready_count(),
                        e.stems.len(),
                        e.playing,
                        e.position(),
                        e.duration,
                    )
                };
                let all_ready = total > 0 && rc >= total;
                let timed_out = ticks > (4000 / TICK_MS);
                buffering.set(!(all_ready || timed_out));

                let mut pos = pos;
                if is_playing {
                    eng.borrow().correct_drift();
                    position.set(pos);
                    if dur > 0.0 && pos >= dur - 0.25 {
                        eng.borrow_mut().pause();
                        playing.set(false);
                        position.set(dur);
                        pos = dur;
                    }
                }

                push_session_signals(pos, is_playing);
            }
        });

        // ── transport actions (as Callbacks for the session-ui components) ────
        let play_pause: Callback<()> = use_callback(move |()| {
            if let Some(eng) = engine_of() {
                if playing() {
                    eng.borrow_mut().pause();
                    playing.set(false);
                } else {
                    let off = position();
                    eng.borrow_mut().play(off);
                    playing.set(true);
                }
            }
        });
        let seek: Callback<f64> = use_callback(move |off: f64| {
            if let Some(eng) = engine_of() {
                eng.borrow_mut().seek(off);
            }
            position.set(off);
            push_session_signals(off, playing());
        });

        // ── mixer mutators (by stem index) ───────────────────────────────────
        let toggle_mute: Callback<usize> = use_callback(move |i: usize| {
            let mut ui = stem_ui();
            if let Some(s) = ui.get_mut(i) {
                s.muted = !s.muted;
            }
            if let Some(eng) = engine_of() {
                apply_mix(&eng, &ui);
            }
            stem_ui.set(ui);
        });
        let toggle_solo: Callback<usize> = use_callback(move |i: usize| {
            let mut ui = stem_ui();
            if let Some(s) = ui.get_mut(i) {
                s.soloed = !s.soloed;
            }
            if let Some(eng) = engine_of() {
                apply_mix(&eng, &ui);
            }
            stem_ui.set(ui);
        });
        let set_volume: Callback<(usize, f32)> = use_callback(move |(i, v): (usize, f32)| {
            let mut ui = stem_ui();
            if let Some(s) = ui.get_mut(i) {
                s.volume = v;
            }
            if let Some(eng) = engine_of() {
                apply_mix(&eng, &ui);
            }
            stem_ui.set(ui);
        });
        // Set an explicit mute value for a set of stems (used by the Guide
        // toggle so it deterministically un/mutes the click + cue bus).
        let set_mutes: Callback<(Vec<usize>, bool)> =
            use_callback(move |(idxs, muted): (Vec<usize>, bool)| {
                let mut ui = stem_ui();
                for i in idxs {
                    if let Some(s) = ui.get_mut(i) {
                        s.muted = muted;
                    }
                }
                if let Some(eng) = engine_of() {
                    apply_mix(&eng, &ui);
                }
                stem_ui.set(ui);
            });

        // ── render ──────────────────────────────────────────────────────────
        let body = match &*loaded.read_unchecked() {
            None => rsx! {
                div { class: "flex flex-col gap-2 py-10",
                    span { class: "text-sm text-muted-foreground", "Loading manifest…" }
                }
            },
            Some(Err(msg)) => rsx! {
                div { class: "flex flex-col gap-2 py-10",
                    span { class: "text-sm font-semibold text-destructive", "Could not load song" }
                    span { class: "text-sm text-muted-foreground", "{msg}" }
                }
            },
            Some(Ok((manifest, _))) => {
                let manifest = manifest.clone();
                rsx! {
                    Player {
                        org: org.clone(),
                        collection: collection.clone(),
                        manifest,
                        playing,
                        position,
                        buffering,
                        stem_ui,
                        play_pause,
                        seek,
                        toggle_mute,
                        toggle_solo,
                        set_volume,
                        set_mutes,
                    }
                }
            }
        };

        rsx! {
            document::Title { "{song} — FastTrackStudio" }
            div { class: "mx-auto max-w-4xl px-4 py-8 flex flex-col gap-5", {body} }
        }
    }

    /// Populate the session-ui global signals from the player's current state.
    /// Called each transport tick and after seeks. Uses `apply_active_indices`
    /// for the cursor + `PLAYBACK_STATE`, and writes `SONG_TRANSPORT[0]`.
    fn push_session_signals(pos: f64, is_playing: bool) {
        // Read structural facts from the setlist we populated on load.
        let (dur, count_in, bpm, ts_num, ts_denom, section_index, section_prog) = {
            let setlist = SETLIST_STRUCTURE.read();
            let Some(song) = setlist.songs.first() else {
                return;
            };
            let dur = song.duration().max(0.001);
            let bpm = song.tempo.unwrap_or(120.0);
            let ts = song.time_signature.unwrap_or(TimeSignature::COMMON_TIME);
            let (sec_idx, sec_prog) = song
                .section_at_position_with_index(pos)
                .map(|(i, s)| {
                    let d = s.duration().max(0.001);
                    (Some(i), ((pos - s.start_seconds) / d).clamp(0.0, 1.0))
                })
                .unwrap_or((None, 0.0));
            (
                dur,
                song.count_in_seconds.unwrap_or(0.0),
                bpm,
                ts.numerator(),
                ts.denominator(),
                sec_idx,
                sec_prog,
            )
        };

        let song_progress = (pos / dur).clamp(0.0, 1.0);
        let indices = ActiveIndices {
            song_index: Some(0),
            section_index,
            slide_index: None,
            song_progress: Some(song_progress),
            section_progress: Some(section_prog),
            is_playing,
            looping: false,
            loop_selection: None,
            queued_target: None,
        };
        apply_active_indices(&indices);

        let musical = musical_at(count_in + pos, bpm, ts_num);
        let transport = TransportState {
            position: Position::from_time_and_musical(PositionInSeconds::from_seconds(pos), musical),
            bpm,
            time_sig_num: ts_num as i32,
            time_sig_denom: ts_denom as i32,
            is_playing,
            is_looping: false,
            loop_region: None,
        };
        // Only write when the state actually changed to limit render fanout.
        let changed = SONG_TRANSPORT
            .peek()
            .get(&0)
            .map(|e| *e != transport)
            .unwrap_or(true);
        if changed {
            SONG_TRANSPORT.write().insert(0, transport);
        }
        // `PLAYBACK_STATE` is set by `apply_active_indices`; kept here as the
        // documented contract (Playing/Stopped).
        let _ = &*PLAYBACK_STATE;
    }

    /// The ready player: header, chart, progress, transport, mixer. Split out
    /// so the reactive reads (position/stem_ui) live in a child scope.
    #[component]
    fn Player(
        org: String,
        collection: String,
        manifest: Manifest,
        playing: Signal<bool>,
        position: Signal<f64>,
        buffering: Signal<bool>,
        stem_ui: Signal<Vec<StemUi>>,
        play_pause: Callback<()>,
        seek: Callback<f64>,
        toggle_mute: Callback<usize>,
        toggle_solo: Callback<usize>,
        set_volume: Callback<(usize, f32)>,
        set_mutes: Callback<(Vec<usize>, bool)>,
    ) -> Element {
        let duration = manifest.duration_sec.max(0.001);
        let pos = position();
        let is_playing = playing();
        let is_buffering = buffering();

        let title = manifest.title.clone().unwrap_or_default();
        let artist = manifest.artist.clone().unwrap_or_default();
        let sections = progress_sections(&manifest);

        // Song / section progress (0-100) for the session-ui progress bars.
        let song_progress = (pos / duration * 100.0).clamp(0.0, 100.0);
        let cur_section = manifest
            .sections
            .iter()
            .position(|s| pos >= s.start_sec && pos < s.end_sec);
        let section_progress = cur_section
            .and_then(|i| manifest.sections.get(i))
            .map(|s| {
                let d = (s.end_sec - s.start_sec).max(0.001);
                ((pos - s.start_sec) / d * 100.0).clamp(0.0, 100.0)
            })
            .unwrap_or(0.0);

        // Guide (click + cue) stem indices, and whether the bus is on.
        let guide_idxs: Vec<usize> = manifest
            .stems
            .iter()
            .enumerate()
            .filter(|(_, s)| is_guide_stem(s))
            .map(|(i, _)| i)
            .collect();
        let guide_on = {
            let ui = stem_ui.read();
            guide_idxs
                .iter()
                .any(|&i| ui.get(i).map(|s| !s.muted).unwrap_or(false))
        };

        // ── session-ui callback adapters (guid ↔ stem index) ─────────────────
        let stems_for_lookup = manifest.stems.clone();
        let index_of = move |guid: &str| stems_for_lookup.iter().position(|s| s.file == guid);

        let mixer_volume: Callback<(String, f64)> = use_callback({
            let index_of = index_of.clone();
            move |(guid, v): (String, f64)| {
                if let Some(i) = index_of(&guid) {
                    set_volume.call((i, v as f32));
                }
            }
        });
        let mixer_mute: Callback<String> = use_callback({
            let index_of = index_of.clone();
            move |guid: String| {
                if let Some(i) = index_of(&guid) {
                    toggle_mute.call(i);
                }
            }
        });
        let mixer_solo: Callback<String> = use_callback({
            let index_of = index_of.clone();
            move |guid: String| {
                if let Some(i) = index_of(&guid) {
                    toggle_solo.call(i);
                }
            }
        });

        // Transport bar adapters.
        let on_play_pause: Callback<()> = use_callback(move |()| play_pause.call(()));
        let noop: Callback<()> = use_callback(move |()| {});
        let sections_for_back = manifest.sections.clone();
        let on_back: Callback<()> = use_callback(move |()| {
            let p = position();
            // Jump to the previous section boundary (or the current section's
            // start if we're already past its head).
            let target = sections_for_back
                .iter()
                .map(|s| s.start_sec)
                .filter(|&s| s < p - 1.0)
                .fold(0.0_f64, f64::max);
            seek.call(target);
        });
        let sections_for_fwd = manifest.sections.clone();
        let on_forward: Callback<()> = use_callback(move |()| {
            let p = position();
            if let Some(next) = sections_for_fwd
                .iter()
                .map(|s| s.start_sec)
                .find(|&s| s > p + 0.5)
            {
                seek.call(next);
            }
        });

        // Guide toggle: un/mute the click + cue stems together.
        let guide_idxs_for_toggle = guide_idxs.clone();
        let on_guide: Callback<()> = use_callback(move |()| {
            // If currently on (unmuted), mute; else unmute.
            set_mutes.call((guide_idxs_for_toggle.clone(), guide_on));
        });

        // Section-click seeks to the section start.
        let sections_for_click = manifest.sections.clone();
        let on_section_click: Callback<usize> = use_callback(move |i: usize| {
            if let Some(s) = sections_for_click.get(i) {
                seek.call(s.start_sec);
            }
        });

        let tracks = stems_to_tracks(&manifest, &stem_ui.read());

        rsx! {
            // Breadcrumb back to the collection.
            div { class: "flex items-center gap-2 text-[11px] uppercase tracking-[0.15em] text-muted-foreground/70",
                Link {
                    to: crate::Route::SessionCollection { org: org.clone(), collection: collection.clone() },
                    class: "hover:text-foreground transition-colors",
                    "{collection}"
                }
                span { "/" }
                span { "{org}" }
            }

            // Title (real session-ui component) + metadata badges.
            SongTitle { song_name: title.clone() }
            div { class: "flex flex-wrap items-center justify-center gap-2 -mt-4",
                if !artist.is_empty() {
                    span { class: "text-sm text-muted-foreground mr-2", "{artist}" }
                }
                if let Some(k) = manifest.key.as_ref() {
                    Badge { label: "Key {k}" }
                }
                if let Some(b) = manifest.bpm {
                    Badge { label: "{b} BPM" }
                }
                if let Some(ts) = manifest.time_signature.as_ref() {
                    Badge { label: "{ts}" }
                }
                Badge { label: "{manifest.stems.len()} stems" }
            }

            // Chart pane (engraver SVG rendered INLINE + synced playhead).
            div { class: "border border-border rounded-lg overflow-hidden bg-white",
                SessionChartPane {}
            }

            // Song progress (segmented sections; click to seek).
            if !manifest.sections.is_empty() {
                div { class: "pt-6",
                    SongProgressBar {
                        progress: song_progress,
                        sections: sections.clone(),
                        song_key: manifest.key.clone(),
                        on_section_click,
                    }
                }
                // Progress within the current section.
                SectionProgressBar {
                    progress: section_progress,
                    sections: sections.clone(),
                    song_key: manifest.key.clone(),
                }
            }

            // Fine scrubber + time readout (kept for precise seeking).
            div { class: "flex items-center gap-3",
                span { class: "text-xs font-mono text-muted-foreground tabular-nums min-w-[84px]",
                    "{fmt_time(pos)} / {fmt_time(duration)}"
                }
                input {
                    r#type: "range",
                    class: "flex-1 accent-primary",
                    min: "0",
                    max: "{duration}",
                    step: "0.01",
                    value: "{pos}",
                    oninput: move |e| {
                        if let Ok(v) = e.value().parse::<f64>() {
                            seek.call(v);
                        }
                    },
                }
                if is_buffering {
                    span { class: "text-[11px] text-muted-foreground/70", "buffering…" }
                }
            }

            // Transport bar (real session-ui component). Record/arm/loop are
            // no-ops in the browser player; play/back/forward drive the engine.
            div { class: "h-16 rounded-lg overflow-hidden border border-border",
                TransportControlBar {
                    is_playing,
                    is_looping: false,
                    is_recording: false,
                    is_armed: false,
                    on_play_pause,
                    on_loop_toggle: noop,
                    on_record_toggle: noop,
                    on_arm_toggle: noop,
                    on_back,
                    on_forward,
                }
            }

            // Guide (click) toggle — un/mutes the Click + Cue stems together.
            if !guide_idxs.is_empty() {
                div { class: "flex items-center gap-3 p-3 border border-border rounded-lg bg-card",
                    span { class: "text-sm font-semibold text-foreground flex-1", "Guide / Click" }
                    button {
                        class: if guide_on {
                            "px-4 py-1.5 rounded-md text-sm font-semibold bg-primary text-primary-foreground hover:bg-primary/90 transition-colors"
                        } else {
                            "px-4 py-1.5 rounded-md text-sm font-semibold bg-muted text-muted-foreground hover:bg-accent transition-colors"
                        },
                        onclick: move |_| on_guide.call(()),
                        if guide_on { "On" } else { "Off" }
                    }
                }
            }

            // Per-stem mixer (real session-ui MixerView, backed by the
            // Web-Audio GainNodes / mute / solo state).
            div { class: "h-56 rounded-lg overflow-hidden border border-border bg-card",
                MixerView {
                    tracks,
                    on_volume: mixer_volume,
                    on_mute: mixer_mute,
                    on_solo: mixer_solo,
                }
            }
        }
    }

    #[component]
    fn Badge(label: String) -> Element {
        rsx! {
            span {
                class: "text-[11px] font-semibold uppercase tracking-[0.1em] text-muted-foreground border border-border rounded-full px-2 py-0.5",
                "{label}"
            }
        }
    }
}

#[cfg(target_arch = "wasm32")]
pub use imp::SongSession;

// ─────────────────────────────────────────────────────────────────────────────
// Non-wasm: a stub so the crate still compiles as a workspace member. The
// session view is a browser-only feature (Web Audio + media elements).
// ─────────────────────────────────────────────────────────────────────────────
#[cfg(not(target_arch = "wasm32"))]
mod stub {
    use dioxus::prelude::*;

    #[component]
    pub fn SongSession(org: String, collection: String, song: String) -> Element {
        let _ = (&org, &collection);
        rsx! {
            document::Title { "{song} — FastTrackStudio" }
            div { class: "mx-auto max-w-3xl px-4 py-10",
                span { class: "text-sm text-muted-foreground",
                    "The session player runs in the browser."
                }
            }
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub use stub::SongSession;
