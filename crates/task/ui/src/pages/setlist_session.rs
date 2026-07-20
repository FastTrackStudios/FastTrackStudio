//! `type: setlist` vault-note view — a browser multitrack **setlist session
//! player**: it loads a whole ordered set of songs, tracks a **current song**,
//! lets you navigate the whole set (prev / next / pick), and presents the
//! session view + chart as tabs that FOLLOW the current song.
//!
//! Built on the same streaming Web-Audio engine as the single-song
//! [`SongView`](crate::pages::song_session) — the reusable primitives (manifest
//! model, engine, mixer, session-proto mapping) are shared from
//! `song_session::imp`; this module only adds the multi-song orchestration.
//!
//! ## The model — the current song drives everything
//!
//! The session-ui views are designed around a setlist + an **active song
//! index** (`ACTIVE_INDICES.song_index`). On load we hydrate the WHOLE set:
//!
//! - `SETLIST_STRUCTURE` — a `session_proto::Setlist` with EVERY song
//!   (sections/tempo/key + `project_guid = web-session:{slug}`).
//! - `SONG_CHARTS[guid]` — each song's `chart.kf` text.
//!
//! Then the chart pane, section bar, navigator sidebar and transport ALL
//! follow the active song for free — switching songs is just a write to
//! `ACTIVE_INDICES.song_index` (via `current_song`) plus an audio swap.
//!
//! ## Audio — only the current song is loaded
//!
//! Loading every song's stems at once would mean 100+ media elements. Instead
//! the streaming graph holds ONLY the current song; when `current_song`
//! changes we tear the old graph down (`EngineInner::teardown` — pause,
//! detach, close the `AudioContext`) and build the new song's from its
//! manifest. Transport (play/pause/seek) drives the current song; a song
//! switch stops and resets to the head of the new song.

// ─────────────────────────────────────────────────────────────────────────────
// wasm32: the real player.
// ─────────────────────────────────────────────────────────────────────────────
#[cfg(target_arch = "wasm32")]
mod imp {
    use dioxus::prelude::*;

    use daw_proto::{Position, PositionInSeconds, TimeSignature};
    use session_proto::{ActiveIndices, Setlist, SongChartHydration};
    use session_ui::components::{
        MixerView, SongProgressBar, SongTitle, TransportControlBar,
    };
    use session_ui::{
        ACTIVE_INDICES, PerformanceSidebar, SETLIST_STRUCTURE, SONG_CHARTS, SONG_TRANSPORT,
        TransportState, apply_active_indices,
    };

    use crate::pages::session_chart_pane::SessionChartPane;
    // The streaming engine + manifest model + session-proto mapping are shared
    // with the single-song player (see `song_session::imp`).
    use crate::pages::song_session::imp as media;

    /// One row in the setlist navigator: the media slug + display title.
    #[derive(Clone, PartialEq)]
    struct SongMeta {
        slug: String,
        title: String,
    }

    /// A song's fetched artifacts: its slug, manifest, and optional chart text.
    type LoadedSong = (String, media::Manifest, Option<String>);

    /// Which tab is showing in the player.
    #[derive(Clone, Copy, PartialEq)]
    enum Tab {
        Session,
        Chart,
    }

    // ── the component ───────────────────────────────────────────────────────

    /// The `type: setlist` player. `songs` is the ordered list of media slugs
    /// (from the note's `songs:` frontmatter). Rendered above the note editor.
    #[component]
    pub fn SetlistPlayer(songs: Vec<String>) -> Element {
        // Current song in the set → drives `ACTIVE_INDICES.song_index`.
        let current_song = use_signal(|| 0usize);
        let playing = use_signal(|| false);
        let position = use_signal(|| 0.0_f64);
        let buffering = use_signal(|| true);
        // Per-stem mixer state for the CURRENT song (reset on song switch).
        let stem_ui = use_signal(Vec::<media::StemUi>::new);
        // The streaming graph for the CURRENT song only.
        let engine = use_signal(|| None::<media::Engine>);

        // Fetch EVERY song's manifest + chart.kf once. Keyed on the slug list
        // via `use_reactive!` so it runs exactly once per setlist (charts are
        // optional — a missing chart.kf just yields `None`).
        let songs_r = songs.clone();
        let loaded = use_resource(use_reactive!(|songs_r| {
            let songs = songs_r.clone();
            async move {
                let mut out: Vec<LoadedSong> = Vec::with_capacity(songs.len());
                for slug in &songs {
                    let manifest =
                        media::fetch_manifest(&format!("/media/songs/{slug}/manifest.json")).await?;
                    let chart = media::fetch_text(&format!("/media/songs/{slug}/chart.kf"))
                        .await
                        .ok()
                        .filter(|t| !t.is_empty());
                    out.push((slug.clone(), manifest, chart));
                }
                Ok::<Vec<LoadedSong>, String>(out)
            }
        }));

        // Hydrate the session-ui structural signals from the WHOLE set — runs
        // once when the fetch lands. After this the navigator, chart pane and
        // section bars follow `ACTIVE_INDICES.song_index` with no per-view
        // refetch.
        use_effect(move || {
            let guard = loaded.read();
            let Some(Ok(list)) = &*guard else {
                return;
            };
            let mut songs_out = Vec::with_capacity(list.len());
            let mut charts = Vec::new();
            for (slug, manifest, chart) in list.iter() {
                let song = media::build_song(slug, manifest, chart.clone());
                if let Some(c) = chart {
                    charts.push((song.project_guid.clone(), c.clone()));
                }
                songs_out.push(song);
            }
            drop(guard);

            *SETLIST_STRUCTURE.write() = Setlist {
                id: Some("web-setlist".to_owned()),
                name: "Setlist".to_owned(),
                advance_mode: session_proto::AdvanceMode::Wait,
                songs: songs_out,
            };
            let mut sc = SONG_CHARTS.write();
            for (guid, text) in charts {
                sc.insert(
                    guid,
                    SongChartHydration {
                        project_guid: String::new(),
                        chart_text: text,
                        detected_chords: Vec::new(),
                        chart_fingerprint: String::new(),
                    },
                );
            }
        });

        // Audio swap: (re)build the CURRENT song's streaming graph whenever the
        // song index changes (or the fetch first lands). Tears the previous
        // graph down first — pause, detach, close its `AudioContext` — so we
        // never hold N songs' worth of media elements, then resets transport to
        // the head of the new song.
        {
            let mut engine = engine;
            let mut stem_ui = stem_ui;
            let mut playing = playing;
            let mut position = position;
            let mut buffering = buffering;
            let current_song = current_song;
            use_effect(move || {
                let idx = current_song();
                let (slug, manifest) = {
                    let guard = loaded.read();
                    let Some(Ok(list)) = &*guard else {
                        return;
                    };
                    let Some((slug, manifest, _)) = list.get(idx) else {
                        return;
                    };
                    (slug.clone(), manifest.clone())
                };
                // Tear down the previous song's graph (peek: no reactive dep).
                if let Some(old) = engine.peek().clone() {
                    old.borrow_mut().teardown();
                }
                match media::build_engine(&slug, &manifest) {
                    Ok(eng) => {
                        let v: Vec<media::StemUi> = manifest
                            .stems
                            .iter()
                            .map(|s| media::StemUi {
                                muted: s.default_muted,
                                soloed: false,
                                volume: 1.0,
                            })
                            .collect();
                        media::apply_mix(&eng, &v);
                        stem_ui.set(v);
                        engine.set(Some(eng));
                        playing.set(false);
                        position.set(0.0);
                        buffering.set(true);
                        push_session_signals(idx, 0.0, false);
                    }
                    Err(e) => tracing::error!("setlist: engine build failed for {slug}: {e}"),
                }
            });
        }

        // ~10 Hz loop for the CURRENT song: readiness (buffering), drift
        // correction, playhead, end-of-song, and session-ui signal population
        // (ACTIVE_INDICES / SONG_TRANSPORT) for the active index.
        {
            let engine = engine;
            let current_song = current_song;
            let mut playing = playing;
            let mut position = position;
            let mut buffering = buffering;
            use_future(move || async move {
                let mut ticks: u32 = 0;
                let mut last_idx = usize::MAX;
                loop {
                    gloo_timers::future::TimeoutFuture::new(media::TICK_MS).await;
                    let Some(eng) = engine.peek().clone() else {
                        continue;
                    };
                    let idx = *current_song.peek();
                    if idx != last_idx {
                        ticks = 0;
                        last_idx = idx;
                    }
                    ticks += 1;
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
                    let timed_out = ticks > (4000 / media::TICK_MS);
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
                    push_session_signals(idx, pos, is_playing);
                }
            });
        }

        // ── transport actions ─────────────────────────────────────────────────
        let play_pause: Callback<()> = use_callback({
            let engine = engine;
            let mut playing = playing;
            let position = position;
            move |()| {
                if let Some(eng) = engine.peek().clone() {
                    if playing() {
                        eng.borrow_mut().pause();
                        playing.set(false);
                    } else {
                        let off = position();
                        eng.borrow_mut().play(off);
                        playing.set(true);
                    }
                }
            }
        });
        let seek: Callback<f64> = use_callback({
            let engine = engine;
            let mut position = position;
            let current_song = current_song;
            let playing = playing;
            move |off: f64| {
                if let Some(eng) = engine.peek().clone() {
                    eng.borrow_mut().seek(off);
                }
                position.set(off);
                push_session_signals(*current_song.peek(), off, playing());
            }
        });

        // ── mixer mutators (by stem index) ─────────────────────────────────────
        let toggle_mute: Callback<usize> = use_callback({
            let engine = engine;
            let mut stem_ui = stem_ui;
            move |i: usize| {
                let mut ui = stem_ui();
                if let Some(s) = ui.get_mut(i) {
                    s.muted = !s.muted;
                }
                if let Some(eng) = engine.peek().clone() {
                    media::apply_mix(&eng, &ui);
                }
                stem_ui.set(ui);
            }
        });
        let toggle_solo: Callback<usize> = use_callback({
            let engine = engine;
            let mut stem_ui = stem_ui;
            move |i: usize| {
                let mut ui = stem_ui();
                if let Some(s) = ui.get_mut(i) {
                    s.soloed = !s.soloed;
                }
                if let Some(eng) = engine.peek().clone() {
                    media::apply_mix(&eng, &ui);
                }
                stem_ui.set(ui);
            }
        });
        let set_volume: Callback<(usize, f32)> = use_callback({
            let engine = engine;
            let mut stem_ui = stem_ui;
            move |(i, v): (usize, f32)| {
                let mut ui = stem_ui();
                if let Some(s) = ui.get_mut(i) {
                    s.volume = v;
                }
                if let Some(eng) = engine.peek().clone() {
                    media::apply_mix(&eng, &ui);
                }
                stem_ui.set(ui);
            }
        });
        let set_mutes: Callback<(Vec<usize>, bool)> = use_callback({
            let engine = engine;
            let mut stem_ui = stem_ui;
            move |(idxs, muted): (Vec<usize>, bool)| {
                let mut ui = stem_ui();
                for i in idxs {
                    if let Some(s) = ui.get_mut(i) {
                        s.muted = muted;
                    }
                }
                if let Some(eng) = engine.peek().clone() {
                    media::apply_mix(&eng, &ui);
                }
                stem_ui.set(ui);
            }
        });

        // ── set navigation: pick / prev / next a whole song ────────────────────
        let goto_song: Callback<usize> = use_callback({
            let mut current_song = current_song;
            let loaded = loaded;
            move |i: usize| {
                let count = match &*loaded.read_unchecked() {
                    Some(Ok(list)) => list.len(),
                    _ => 0,
                };
                if count == 0 {
                    return;
                }
                let i = i.min(count - 1);
                if i != *current_song.peek() {
                    current_song.set(i);
                }
            }
        });

        // ── render ─────────────────────────────────────────────────────────────
        let idx = current_song();
        let body = match &*loaded.read_unchecked() {
            None => rsx! {
                div { class: "flex flex-col gap-2 py-10",
                    span { class: "text-sm text-muted-foreground", "Loading setlist…" }
                }
            },
            Some(Err(msg)) => rsx! {
                div { class: "flex flex-col gap-2 py-10",
                    span { class: "text-sm font-semibold text-destructive", "Could not load setlist" }
                    span { class: "text-sm text-muted-foreground", "{msg}" }
                }
            },
            Some(Ok(list)) if list.is_empty() => rsx! {
                div { class: "flex flex-col gap-2 py-10",
                    span { class: "text-sm text-muted-foreground",
                        "This setlist has no songs. Add a `songs:` list to the note frontmatter."
                    }
                }
            },
            Some(Ok(list)) => {
                let songs_meta: Vec<SongMeta> = list
                    .iter()
                    .map(|(slug, m, _)| SongMeta {
                        slug: slug.clone(),
                        title: m.title.clone().unwrap_or_else(|| slug.clone()),
                    })
                    .collect();
                let manifest = list
                    .get(idx.min(list.len() - 1))
                    .map(|(_, m, _)| m.clone())
                    .unwrap_or_else(|| list[0].1.clone());
                rsx! {
                    SetlistBody {
                        songs_meta,
                        manifest,
                        current_song,
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
                        goto_song,
                    }
                }
            }
        };

        rsx! {
            div { class: "mx-auto w-full max-w-6xl px-4 py-6", {body} }
        }
    }

    /// Populate the session-ui global signals for the ACTIVE song. Called each
    /// transport tick and after seeks/song-switches. Writes `ACTIVE_INDICES`
    /// (with `song_index`), `SONG_TRANSPORT[song_index]`, and `PLAYBACK_STATE`
    /// (via `apply_active_indices`).
    fn push_session_signals(song_index: usize, pos: f64, is_playing: bool) {
        let (dur, count_in, bpm, ts_num, ts_denom, section_index, section_prog) = {
            let setlist = SETLIST_STRUCTURE.read();
            let Some(song) = setlist.songs.get(song_index) else {
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
            song_index: Some(song_index),
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

        let musical = media::musical_at(count_in + pos, bpm, ts_num);
        let transport = TransportState {
            position: Position::from_time_and_musical(PositionInSeconds::from_seconds(pos), musical),
            bpm,
            time_sig_num: ts_num as i32,
            time_sig_denom: ts_denom as i32,
            is_playing,
            is_looping: false,
            loop_region: None,
        };
        let changed = SONG_TRANSPORT
            .peek()
            .get(&song_index)
            .map(|e| *e != transport)
            .unwrap_or(true);
        if changed {
            SONG_TRANSPORT.write().insert(song_index, transport);
        }
    }

    /// The ready setlist player: a **navigator** of the whole set (left) beside
    /// the current song's transport + **Session / Chart** tabs (right). All the
    /// right-hand views follow the current song via the shared session-ui
    /// signals. Split out so the per-frame reactive reads (position / stem_ui)
    /// live in a child scope, away from the parent's resource/effect setup.
    #[allow(clippy::too_many_arguments)]
    #[component]
    fn SetlistBody(
        songs_meta: Vec<SongMeta>,
        manifest: media::Manifest,
        current_song: Signal<usize>,
        playing: Signal<bool>,
        position: Signal<f64>,
        buffering: Signal<bool>,
        stem_ui: Signal<Vec<media::StemUi>>,
        play_pause: Callback<()>,
        seek: Callback<f64>,
        toggle_mute: Callback<usize>,
        toggle_solo: Callback<usize>,
        set_volume: Callback<(usize, f32)>,
        set_mutes: Callback<(Vec<usize>, bool)>,
        goto_song: Callback<usize>,
    ) -> Element {
        let mut tab = use_signal(|| Tab::Chart);

        let count = songs_meta.len();
        let idx = current_song();
        let duration = manifest.duration_sec.max(0.001);
        let pos = position();
        let is_playing = playing();
        let is_buffering = buffering();

        let title = manifest.title.clone().unwrap_or_default();
        let artist = manifest.artist.clone().unwrap_or_default();
        let sections = media::progress_sections(&manifest);

        let song_progress = (pos / duration * 100.0).clamp(0.0, 100.0);

        // Guide (click + cue) stem indices, and whether the bus is on.
        let guide_idxs: Vec<usize> = manifest
            .stems
            .iter()
            .enumerate()
            .filter(|(_, s)| media::is_guide_stem(s))
            .map(|(i, _)| i)
            .collect();
        let guide_on = {
            let ui = stem_ui.read();
            guide_idxs
                .iter()
                .any(|&i| ui.get(i).map(|s| !s.muted).unwrap_or(false))
        };

        // ── session-ui mixer adapters (guid = stem file ↔ index) ──────────────
        let stems_for_lookup = manifest.stems.clone();
        let index_of =
            move |guid: &str| stems_for_lookup.iter().position(|s| s.file == guid);
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

        // ── transport bar adapters (section nav; crosses into the set at
        //     song boundaries) ──────────────────────────────────────────────
        let on_play_pause: Callback<()> = use_callback(move |()| play_pause.call(()));
        let noop: Callback<()> = use_callback(move |()| {});
        let sections_for_back = manifest.sections.clone();
        let on_back: Callback<()> = use_callback(move |()| {
            let p = position();
            // At the head of the song, Back steps to the previous SONG.
            if p <= 1.0 && *current_song.peek() > 0 {
                goto_song.call(*current_song.peek() - 1);
                return;
            }
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
            } else if *current_song.peek() + 1 < count {
                // Past the last section, Advance steps to the next SONG.
                goto_song.call(*current_song.peek() + 1);
            }
        });

        // Guide toggle: un/mute the click + cue stems together.
        let guide_idxs_for_toggle = guide_idxs.clone();
        let on_guide: Callback<()> = use_callback(move |()| {
            set_mutes.call((guide_idxs_for_toggle.clone(), guide_on));
        });

        // Section-click seeks to the section start.
        let sections_for_click = manifest.sections.clone();
        let on_section_click: Callback<usize> = use_callback(move |i: usize| {
            if let Some(s) = sections_for_click.get(i) {
                seek.call(s.start_sec);
            }
        });

        let tracks = media::stems_to_tracks(&manifest, &stem_ui.read());
        let active = tab();
        let at_first = idx == 0;
        let at_last = idx + 1 >= count;

        rsx! {
            div { class: "flex flex-col gap-4 md:flex-row",
                // ── Setlist navigator (the session Navigator, client-driven) ──
                aside { class: "w-full shrink-0 md:w-72",
                    div { class: "rounded-lg border border-border overflow-hidden h-[70vh]",
                        PerformanceSidebar {
                            on_song_select: goto_song,
                            on_section_select: Callback::new(move |(_song, sec): (usize, usize)| {
                                on_section_click.call(sec)
                            }),
                        }
                    }
                }

                // ── Current song: transport + tabs ───────────────────────────
                div { class: "flex min-w-0 flex-1 flex-col gap-5",
                    // Title + metadata badges + set position.
                    SongTitle { song_name: title.clone() }
                    div { class: "flex flex-wrap items-center justify-center gap-2 -mt-4",
                        if !artist.is_empty() {
                            span { class: "text-sm text-muted-foreground mr-2", "{artist}" }
                        }
                        Badge { label: "Song {idx + 1}/{count}" }
                        if let Some(k) = manifest.key.as_ref() {
                            Badge { label: "Key {k}" }
                        }
                        if let Some(b) = manifest.bpm {
                            Badge { label: "{b} BPM" }
                        }
                        if let Some(ts) = manifest.time_signature.as_ref() {
                            Badge { label: "{ts}" }
                        }
                        if is_buffering {
                            span { class: "text-[11px] text-muted-foreground/70", "buffering…" }
                        }
                    }

                    // Song progress (segmented; sections + click-to-seek). The
                    // old standalone SectionProgressBar + fine scrubber/timestamp
                    // row were redundant with this bar and have been removed;
                    // seeking still works via `on_section_click` below.
                    if !manifest.sections.is_empty() {
                        div { class: "pt-2",
                            SongProgressBar {
                                progress: song_progress,
                                sections: sections.clone(),
                                song_key: manifest.key.clone(),
                                on_section_click,
                            }
                        }
                    }

                    // Transport (full width so the 6 controls never squish) +
                    // whole-song prev/next below it.
                    div { class: "flex flex-col gap-2",
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
                        div { class: "flex items-center justify-between gap-2",
                            button {
                                class: "px-3 py-2 rounded-md text-sm font-semibold border border-border text-foreground hover:bg-accent disabled:opacity-40 transition-colors",
                                disabled: at_first,
                                onclick: {
                                    let goto_song = goto_song;
                                    move |_| if idx > 0 { goto_song.call(idx - 1) }
                                },
                                "‹ Prev song"
                            }
                            button {
                                class: "px-3 py-2 rounded-md text-sm font-semibold border border-border text-foreground hover:bg-accent disabled:opacity-40 transition-colors",
                                disabled: at_last,
                                onclick: {
                                    let goto_song = goto_song;
                                    move |_| if idx + 1 < count { goto_song.call(idx + 1) }
                                },
                                "Next song ›"
                            }
                        }
                    }

                    // ── Session / Chart tab switcher ─────────────────────────
                    div { class: "flex items-center gap-1 border-b border-border",
                        button {
                            class: if active == Tab::Session {
                                "px-4 py-2 text-sm font-semibold border-b-2 border-primary text-foreground"
                            } else {
                                "px-4 py-2 text-sm font-semibold border-b-2 border-transparent text-muted-foreground hover:text-foreground transition-colors"
                            },
                            onclick: move |_| tab.set(Tab::Session),
                            "Session"
                        }
                        button {
                            class: if active == Tab::Chart {
                                "px-4 py-2 text-sm font-semibold border-b-2 border-primary text-foreground"
                            } else {
                                "px-4 py-2 text-sm font-semibold border-b-2 border-transparent text-muted-foreground hover:text-foreground transition-colors"
                            },
                            onclick: move |_| tab.set(Tab::Chart),
                            "Chart"
                        }
                    }

                    // Both tabs stay mounted (block/hidden) so audio + mixer
                    // state persist across tab switches.

                    // Session tab — per-stem mixer + the Guide/click toggle.
                    div { class: if active == Tab::Session { "flex flex-col gap-3" } else { "hidden" },
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
                        div { class: "h-56 rounded-lg overflow-hidden border border-border bg-card",
                            MixerView {
                                tracks,
                                on_volume: mixer_volume,
                                on_mute: mixer_mute,
                                on_solo: mixer_solo,
                            }
                        }
                    }

                    // Chart tab — follows ACTIVE_INDICES.song_index for free.
                    div { class: if active == Tab::Chart { "block" } else { "hidden" },
                        div { class: "border border-border rounded-lg overflow-hidden bg-white",
                            SessionChartPane {}
                        }
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
pub use imp::SetlistPlayer;

// ─────────────────────────────────────────────────────────────────────────────
// Non-wasm: a stub so the crate still compiles on native.
// ─────────────────────────────────────────────────────────────────────────────
#[cfg(not(target_arch = "wasm32"))]
mod stub {
    use dioxus::prelude::*;

    #[component]
    pub fn SetlistPlayer(songs: Vec<String>) -> Element {
        let _ = &songs;
        rsx! {
            div { class: "mx-auto max-w-3xl px-4 py-10",
                span { class: "text-sm text-muted-foreground",
                    "The setlist session player runs in the browser."
                }
            }
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub use stub::SetlistPlayer;
