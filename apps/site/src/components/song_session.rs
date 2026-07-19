//! `/session/{org}/{collection}/{song}` — the **session view**: a browser
//! multitrack player.
//!
//! Self-contained, client-side playback. The page is served by fts-server,
//! which also serves the song's media at `/media/songs/{slug}/…`:
//!
//! - `manifest.json` — title/key/bpm, the section map, and the stem list.
//! - `stems/*.ogg`   — one Opus file per stem.
//! - `chart.kf`      — the keyflow chart (optional).
//!
//! All fetched same-origin (no CORS). Each stem is decoded via
//! `AudioContext.decodeAudioData` into an `AudioBuffer`, wired through its own
//! `GainNode` → destination. Play spins up a fresh `AudioBufferSourceNode` per
//! stem, all `start()`ed at one common context time so they are
//! sample-synchronized. This is the web-native counterpart to
//! `daw-standalone`'s `WebRenderer`, but plain Web Audio (no worklet) is
//! enough for straight setlist playback.
//!
//! The heavy lifting is `wasm32`-only (Web Audio + fetch). Off-wasm the crate
//! still has to compile (workspace member), so there's a tiny stub below.

// ─────────────────────────────────────────────────────────────────────────────
// wasm32: the real player.
// ─────────────────────────────────────────────────────────────────────────────
#[cfg(target_arch = "wasm32")]
mod imp {
    use std::cell::RefCell;
    use std::rc::Rc;

    use dioxus::prelude::*;
    use js_sys::ArrayBuffer;
    use serde::Deserialize;
    use wasm_bindgen::JsCast;
    use wasm_bindgen_futures::JsFuture;
    use web_sys::{AudioBuffer, AudioContext, GainNode, Response};

    use crate::components::{PreviewMode, StaticChartRenderer};

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

    // ── the Web Audio engine ────────────────────────────────────────────────

    /// One decoded stem plus its permanent gain node (connected to the
    /// destination once) and the source that's currently playing it (if any).
    struct StemNode {
        buffer: AudioBuffer,
        gain: GainNode,
        source: Option<web_sys::AudioBufferSourceNode>,
    }

    /// The shared, mutable playback graph. Held in an `Rc<RefCell<…>>` so the
    /// resource future, the poll loop, and every event handler can drive it.
    struct EngineInner {
        ctx: AudioContext,
        stems: Vec<StemNode>,
        duration: f64,
        playing: bool,
        /// `ctx.currentTime` captured when the current run was scheduled.
        anchor_ctx_time: f64,
        /// Song offset (seconds) that `anchor_ctx_time` corresponds to.
        anchor_offset: f64,
    }

    type Engine = Rc<RefCell<EngineInner>>;

    impl EngineInner {
        /// Current song position in seconds, derived from the context clock.
        fn position(&self) -> f64 {
            let p = if self.playing {
                self.anchor_offset + (self.ctx.current_time() - self.anchor_ctx_time)
            } else {
                self.anchor_offset
            };
            p.clamp(0.0, self.duration)
        }

        /// Tear down every currently-playing source.
        fn stop_sources(&mut self) {
            for stem in self.stems.iter_mut() {
                if let Some(src) = stem.source.take() {
                    // `stop` is inherited from AudioScheduledSourceNode; call it
                    // through the parent to dodge the deprecated shim on the
                    // AudioBufferSourceNode alias.
                    let sched: &web_sys::AudioScheduledSourceNode = &src;
                    let _ = sched.stop();
                    let _ = src.disconnect();
                }
            }
        }

        /// Start every stem from `offset`, sample-synchronized: all sources are
        /// scheduled at one common context time slightly in the future.
        fn play(&mut self, offset: f64) {
            self.stop_sources();
            let when = self.ctx.current_time() + 0.03;
            for stem in self.stems.iter_mut() {
                if let Ok(src) = self.ctx.create_buffer_source() {
                    src.set_buffer(Some(&stem.buffer));
                    // Deref coercion: &GainNode → &AudioNode.
                    let _ = src.connect_with_audio_node(&stem.gain);
                    let _ = src.start_with_when_and_grain_offset(when, offset);
                    stem.source = Some(src);
                }
            }
            let _ = self.ctx.resume();
            self.anchor_ctx_time = when;
            self.anchor_offset = offset;
            self.playing = true;
        }

        fn pause(&mut self) {
            let pos = self.position();
            self.stop_sources();
            self.anchor_offset = pos;
            self.playing = false;
        }

        /// Jump to `offset`. Restarts the sources if currently playing so audio
        /// stays synced to the new position.
        fn seek(&mut self, offset: f64) {
            if self.playing {
                self.play(offset);
            } else {
                self.anchor_offset = offset;
            }
        }

        fn set_stem_gain(&self, idx: usize, value: f32) {
            if let Some(stem) = self.stems.get(idx) {
                stem.gain.gain().set_value(value);
            }
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

    async fn fetch_array_buffer(url: &str) -> Result<ArrayBuffer, String> {
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
        let promise = resp
            .array_buffer()
            .map_err(|e| format!("{url}: array_buffer: {e:?}"))?;
        let ab = JsFuture::from(promise)
            .await
            .map_err(|e| format!("{url}: array_buffer await: {e:?}"))?;
        ab.dyn_into::<ArrayBuffer>()
            .map_err(|_| format!("{url}: not an ArrayBuffer"))
    }

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

    /// Build the full playback graph: create the context, fetch + decode every
    /// stem (bumping `progress` as each lands), and wire the gain nodes.
    async fn build_engine(
        slug: &str,
        manifest: &Manifest,
        mut progress: Signal<usize>,
    ) -> Result<Engine, String> {
        let ctx = AudioContext::new().map_err(|e| format!("AudioContext: {e:?}"))?;
        let dest = ctx.destination();
        let mut stems = Vec::with_capacity(manifest.stems.len());
        for spec in &manifest.stems {
            let url = format!("/media/songs/{slug}/{}", spec.file);
            let buf = fetch_array_buffer(&url).await?;
            let promise = ctx
                .decode_audio_data(&buf)
                .map_err(|e| format!("{}: decode: {e:?}", spec.name))?;
            let decoded = JsFuture::from(promise)
                .await
                .map_err(|e| format!("{}: decode await: {e:?}", spec.name))?;
            let audio_buffer: AudioBuffer = decoded
                .dyn_into()
                .map_err(|_| format!("{}: decode did not return an AudioBuffer", spec.name))?;

            let gain = ctx.create_gain().map_err(|e| format!("create_gain: {e:?}"))?;
            let _ = gain.connect_with_audio_node(&dest);
            gain.gain()
                .set_value(if spec.default_muted { 0.0 } else { 1.0 });

            stems.push(StemNode {
                buffer: audio_buffer,
                gain,
                source: None,
            });
            progress.set(progress() + 1);
        }

        Ok(Rc::new(RefCell::new(EngineInner {
            duration: manifest.duration_sec,
            ctx,
            stems,
            playing: false,
            anchor_ctx_time: 0.0,
            anchor_offset: 0.0,
        })))
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
        // Transport + decode-progress state.
        let progress = use_signal(|| 0usize);
        let total = use_signal(|| 0usize);
        let mut playing = use_signal(|| false);
        let mut position = use_signal(|| 0.0_f64);
        // Per-stem mixer state; filled once the manifest lands (see effect).
        let mut stem_ui = use_signal(Vec::<StemUi>::new);

        // Fetch manifest → build (fetch+decode all stems) engine. Runs once per
        // song; the returned tuple lives in the resource's signal.
        let song_r = song.clone();
        let loaded = use_resource(move || {
            let slug = song_r.clone();
            let mut total = total;
            let progress = progress;
            async move {
                let manifest = fetch_manifest(&format!("/media/songs/{slug}/manifest.json")).await?;
                total.set(manifest.stems.len());
                let eng = build_engine(&slug, &manifest, progress).await?;
                Ok::<(Manifest, Engine), String>((manifest, eng))
            }
        });

        // Chart source (optional). Rendered once loaded (StaticChartRenderer
        // peeks its source on mount, so mount only after the text is present).
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
        let chart_mode = use_signal(|| PreviewMode::Page);

        // Clone the engine Rc out of the resource (or None while loading).
        let engine_of = move || -> Option<Engine> {
            loaded
                .read()
                .as_ref()
                .and_then(|r| r.as_ref().ok())
                .map(|(_, e)| e.clone())
        };

        // Initialize the mixer state once the engine is ready.
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
                }
            }
        });

        // Poll the context clock ~20×/s to drive the playhead + detect the end.
        use_future(move || async move {
            loop {
                gloo_timers::future::TimeoutFuture::new(50).await;
                let Some(eng) = engine_of() else { continue };
                let (is_playing, pos, dur) = {
                    let e = eng.borrow();
                    (e.playing, e.position(), e.duration)
                };
                if is_playing {
                    position.set(pos);
                    if pos >= dur - 0.02 {
                        eng.borrow_mut().pause();
                        playing.set(false);
                        position.set(dur);
                    }
                }
            }
        });

        // ── transport actions ───────────────────────────────────────────────
        let toggle_play = move |_| {
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
        };
        let mut do_seek = move |off: f64| {
            if let Some(eng) = engine_of() {
                eng.borrow_mut().seek(off);
            }
            position.set(off);
        };

        // ── mixer mutators ──────────────────────────────────────────────────
        let toggle_mute = move |i: usize| {
            let mut ui = stem_ui();
            if let Some(s) = ui.get_mut(i) {
                s.muted = !s.muted;
            }
            if let Some(eng) = engine_of() {
                apply_mix(&eng, &ui);
            }
            stem_ui.set(ui);
        };
        let toggle_solo = move |i: usize| {
            let mut ui = stem_ui();
            if let Some(s) = ui.get_mut(i) {
                s.soloed = !s.soloed;
            }
            if let Some(eng) = engine_of() {
                apply_mix(&eng, &ui);
            }
            stem_ui.set(ui);
        };
        let set_volume = move |(i, v): (usize, f32)| {
            let mut ui = stem_ui();
            if let Some(s) = ui.get_mut(i) {
                s.volume = v;
            }
            if let Some(eng) = engine_of() {
                apply_mix(&eng, &ui);
            }
            stem_ui.set(ui);
        };

        // ── render ──────────────────────────────────────────────────────────
        let body = match &*loaded.read_unchecked() {
            None => {
                let (p, t) = (progress(), total());
                rsx! {
                    div { class: "flex flex-col gap-2 py-10",
                        span { class: "text-sm text-muted-foreground",
                            if t == 0 { "Loading manifest…" } else { "Decoding stems… {p}/{t}" }
                        }
                    }
                }
            }
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
                        stem_ui,
                        chart_src,
                        chart_mode,
                        toggle_play,
                        do_seek,
                        toggle_mute,
                        toggle_solo,
                        set_volume,
                    }
                }
            }
        };

        rsx! {
            document::Title { "{song} — FastTrackStudio" }
            div { class: "mx-auto max-w-3xl px-4 py-8 flex flex-col gap-5", {body} }
        }
    }

    /// The ready player: header, transport, section bar, mixer, chart. Split
    /// out so the reactive reads (position/stem_ui) live in a child scope.
    #[component]
    fn Player(
        org: String,
        collection: String,
        manifest: Manifest,
        playing: Signal<bool>,
        position: Signal<f64>,
        stem_ui: Signal<Vec<StemUi>>,
        chart_src: Signal<String>,
        chart_mode: Signal<PreviewMode>,
        toggle_play: EventHandler<MouseEvent>,
        do_seek: EventHandler<f64>,
        toggle_mute: EventHandler<usize>,
        toggle_solo: EventHandler<usize>,
        set_volume: EventHandler<(usize, f32)>,
    ) -> Element {
        let duration = manifest.duration_sec.max(0.001);
        let pos = position();
        let is_playing = playing();

        // Which section contains the playhead?
        let cur_section = manifest
            .sections
            .iter()
            .position(|s| pos >= s.start_sec && pos < s.end_sec);

        // Group stems by `group`, first-seen order, keeping the global index.
        let mut groups: Vec<(String, Vec<usize>)> = Vec::new();
        for (i, s) in manifest.stems.iter().enumerate() {
            let g = s.group.clone().unwrap_or_else(|| "Other".to_string());
            match groups.iter_mut().find(|(n, _)| n == &g) {
                Some(entry) => entry.1.push(i),
                None => groups.push((g, vec![i])),
            }
        }

        let title = manifest.title.clone().unwrap_or_default();
        let artist = manifest.artist.clone().unwrap_or_default();

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

            // Header: title + metadata badges.
            div { class: "flex flex-col gap-1",
                span { class: "text-2xl font-bold text-foreground", "{title}" }
                if !artist.is_empty() {
                    span { class: "text-sm text-muted-foreground", "{artist}" }
                }
                div { class: "flex flex-wrap gap-2 mt-1",
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
            }

            // Transport.
            div { class: "flex items-center gap-3 p-3 border border-border rounded-lg bg-card",
                button {
                    class: "w-11 h-11 flex items-center justify-center rounded-full bg-primary text-primary-foreground text-lg hover:bg-primary/90 transition-colors",
                    onclick: move |e| toggle_play.call(e),
                    if is_playing { "⏸" } else { "▶" }
                }
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
                            do_seek.call(v);
                        }
                    },
                }
            }

            // Section progress bar — segments proportional to duration.
            if !manifest.sections.is_empty() {
                div { class: "flex flex-col gap-1",
                    div { class: "relative flex w-full h-8 rounded-md overflow-hidden border border-border",
                        for (i, sec) in manifest.sections.iter().enumerate() {
                            {
                                let w = ((sec.end_sec - sec.start_sec) / duration * 100.0).max(0.0);
                                let active = cur_section == Some(i);
                                let start = sec.start_sec;
                                let cls = if active {
                                    "h-full flex items-center justify-center overflow-hidden text-[10px] font-semibold text-primary-foreground bg-primary cursor-pointer transition-colors border-r border-background/40"
                                } else {
                                    "h-full flex items-center justify-center overflow-hidden text-[10px] text-muted-foreground bg-muted/40 hover:bg-muted cursor-pointer transition-colors border-r border-background/40"
                                };
                                rsx! {
                                    div {
                                        key: "{i}",
                                        class: "{cls}",
                                        style: "width: {w}%;",
                                        title: "{sec.name}",
                                        onclick: move |_| do_seek.call(start),
                                        span { class: "px-1 truncate", "{sec.name}" }
                                    }
                                }
                            }
                        }
                        // Playhead hairline across the section bar.
                        div {
                            class: "absolute top-0 bottom-0 w-px bg-foreground pointer-events-none",
                            style: "left: {(pos / duration * 100.0).clamp(0.0, 100.0)}%;",
                        }
                    }
                }
            }

            // Per-stem mixer, grouped.
            div { class: "flex flex-col gap-3",
                for (gi, (group, idxs)) in groups.iter().enumerate() {
                    div {
                        key: "g{gi}",
                        class: "flex flex-col gap-1.5 p-3 border border-border rounded-lg bg-card",
                        span { class: "text-[11px] font-semibold uppercase tracking-[0.12em] text-muted-foreground/70", "{group}" }
                        for &i in idxs.iter() {
                            {
                                let spec = &manifest.stems[i];
                                let st = stem_ui
                                    .read()
                                    .get(i)
                                    .copied()
                                    .unwrap_or(StemUi { muted: spec.default_muted, soloed: false, volume: 1.0 });
                                let mute_cls = if st.muted {
                                    "px-2 py-0.5 rounded text-[11px] font-semibold bg-destructive text-white"
                                } else {
                                    "px-2 py-0.5 rounded text-[11px] font-semibold bg-muted text-muted-foreground hover:bg-muted/70"
                                };
                                let solo_cls = if st.soloed {
                                    "px-2 py-0.5 rounded text-[11px] font-semibold bg-amber-500 text-black"
                                } else {
                                    "px-2 py-0.5 rounded text-[11px] font-semibold bg-muted text-muted-foreground hover:bg-muted/70"
                                };
                                let name = spec.name.clone();
                                rsx! {
                                    div {
                                        key: "s{i}",
                                        class: "flex items-center gap-2",
                                        span { class: "text-sm text-foreground flex-1 truncate", "{name}" }
                                        button {
                                            class: "{mute_cls}",
                                            onclick: move |_| toggle_mute.call(i),
                                            "M"
                                        }
                                        button {
                                            class: "{solo_cls}",
                                            onclick: move |_| toggle_solo.call(i),
                                            "S"
                                        }
                                        input {
                                            r#type: "range",
                                            class: "w-28 accent-primary",
                                            min: "0",
                                            max: "1",
                                            step: "0.01",
                                            value: "{st.volume}",
                                            oninput: move |e| {
                                                if let Ok(v) = e.value().parse::<f32>() {
                                                    set_volume.call((i, v));
                                                }
                                            },
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Chart (optional).
            if !chart_src.read().is_empty() {
                div { class: "border border-border rounded-lg overflow-hidden bg-white",
                    div { class: "w-full",
                        StaticChartRenderer { source: chart_src, mode: chart_mode }
                    }
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
// session view is a browser-only feature (Web Audio + fetch).
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
