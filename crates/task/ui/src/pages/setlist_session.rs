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
    use session_ui::components::{MixerView, SongProgressBar, TransportControlBar};
    use session_ui::{
        SETLIST_STRUCTURE, SONG_CHARTS, SONG_TRANSPORT, TransportState, apply_active_indices,
    };

    use crate::pages::session_chart_pane::SessionChartPane;
    // The streaming engine + manifest model + session-proto mapping are shared
    // with the single-song player (see `song_session::imp`).
    use crate::pages::song_session::imp as media;

    /// One row in the setlist navigator: display title plus the at-a-glance
    /// facts (key / tempo) and the song's accent color (its first section's
    /// bright color — the same per-section palette the timeline uses).
    #[derive(Clone, PartialEq)]
    struct SongMeta {
        title: String,
        key: Option<String>,
        bpm: Option<f64>,
        accent: String,
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
                let urls: Vec<String> = manifest
                    .stems
                    .iter()
                    .map(|s| format!("/media/songs/{slug}/{}", s.file))
                    .collect();
                match media::build_engine(&manifest, &urls) {
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
                        title: m.title.clone().unwrap_or_else(|| slug.clone()),
                        key: m.key.clone(),
                        bpm: m.bpm,
                        accent: media::progress_sections(m)
                            .first()
                            .map(|s| s.color.clone())
                            .unwrap_or_else(|| "#3b82f6".to_owned()),
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
                // Before the first section's start, clamp to section 0
                // (same fix as the single-song player).
                .unwrap_or_else(|| {
                    if song.sections.first().is_some_and(|s| pos < s.start_seconds) {
                        (Some(0), 0.0)
                    } else {
                        (None, 0.0)
                    }
                });
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

        // The active song's accent (its first section color) is the panel's
        // accent — reused for the title rule, the navigator selection, and the
        // playing indicators so the whole panel reads as one color system.
        let accent = songs_meta
            .get(idx)
            .map(|s| s.accent.clone())
            .unwrap_or_else(|| "#3b82f6".to_owned());
        let prev_title = idx
            .checked_sub(1)
            .and_then(|i| songs_meta.get(i))
            .map(|s| s.title.clone());
        let next_title = songs_meta.get(idx + 1).map(|s| s.title.clone());
        // The section the playhead is in — the "where am I" caption.
        let cur_section_name = sections
            .iter()
            .find(|s| song_progress >= s.start_percent && song_progress < s.end_percent)
            .or_else(|| sections.last())
            .map(|s| s.name.clone());
        let time_str = format!("{} / {}", fmt_time(pos), fmt_time(duration));
        let progress_clamped = song_progress.clamp(0.0, 100.0);

        rsx! {
            div { class: "flex flex-col gap-4 md:flex-row md:gap-5",

                // ── Setlist navigator: every song visible + scannable ─────────
                aside { class: "shrink-0 md:w-64",
                    div { class: "rounded-xl border border-border bg-card overflow-hidden",
                        div { class: "flex items-baseline justify-between px-3 pt-3 pb-2",
                            span {
                                class: "text-[11px] font-semibold uppercase tracking-[0.14em] text-muted-foreground",
                                "Setlist"
                            }
                            span { class: "text-[11px] tabular-nums text-muted-foreground", "{count} songs" }
                        }
                        // Horizontal chips on a narrow pane; a vertical list on md+.
                        div {
                            class: "flex gap-1.5 overflow-x-auto px-2 pb-2 md:flex-col md:gap-0.5 md:overflow-x-visible md:overflow-y-auto md:max-h-[60vh]",
                            for (i , s) in songs_meta.iter().enumerate() {
                                {
                                    let is_cur = i == idx;
                                    let goto = goto_song;
                                    let facts = {
                                        let mut parts: Vec<String> = Vec::new();
                                        if let Some(k) = &s.key {
                                            parts.push(k.clone());
                                        }
                                        if let Some(b) = s.bpm {
                                            parts.push(format!("{b:.0} bpm"));
                                        }
                                        parts.join(" · ")
                                    };
                                    rsx! {
                                        div { key: "{i}", class: "shrink-0 md:w-full",
                                            button {
                                                r#type: "button",
                                                class: if is_cur {
                                                    "flex w-full items-center gap-2.5 rounded-lg border-l-2 px-2.5 py-2 text-left min-w-[9.5rem] md:min-w-0 transition-colors"
                                                } else {
                                                    "flex w-full items-center gap-2.5 rounded-lg border-l-2 border-transparent px-2.5 py-2 text-left min-w-[9.5rem] md:min-w-0 hover:bg-accent transition-colors"
                                                },
                                                style: if is_cur {
                                                    format!("border-color:{a}; background:{a}14;", a = s.accent)
                                                } else {
                                                    String::new()
                                                },
                                                onclick: move |_| goto.call(i),
                                                // Index / color badge
                                                span {
                                                    class: if is_cur {
                                                        "flex h-6 w-6 shrink-0 items-center justify-center rounded-md text-[11px] font-bold tabular-nums text-white"
                                                    } else {
                                                        "flex h-6 w-6 shrink-0 items-center justify-center rounded-md text-[11px] font-bold tabular-nums bg-muted text-muted-foreground"
                                                    },
                                                    style: if is_cur { format!("background:{};", s.accent) } else { String::new() },
                                                    "{i + 1}"
                                                }
                                                // Title + key/bpm caption
                                                span { class: "flex min-w-0 flex-1 flex-col",
                                                    span {
                                                        class: if is_cur {
                                                            "truncate text-sm font-semibold text-foreground"
                                                        } else {
                                                            "truncate text-sm font-medium text-foreground/80"
                                                        },
                                                        "{s.title}"
                                                    }
                                                    if !facts.is_empty() {
                                                        span {
                                                            class: "truncate text-[10px] tabular-nums text-muted-foreground",
                                                            "{facts}"
                                                        }
                                                    }
                                                }
                                                // Playing pulse (only the loaded/current song plays)
                                                if is_cur && is_playing {
                                                    span {
                                                        class: "ml-auto h-2 w-2 shrink-0 rounded-full animate-pulse",
                                                        style: format!("background:{};", s.accent),
                                                    }
                                                }
                                            }
                                            // Slim section strip for the active song — a glanceable
                                            // structure preview + playhead, NOT the tall boxes.
                                            if is_cur && !sections.is_empty() {
                                                div { class: "mt-1 mb-1 px-2.5",
                                                    div { class: "relative h-1.5 w-full overflow-hidden rounded-full bg-muted",
                                                        for (si , seg) in sections.iter().enumerate() {
                                                            div {
                                                                key: "{si}",
                                                                class: "absolute inset-y-0",
                                                                style: format!(
                                                                    "left:{}%; width:{}%; background:{};",
                                                                    seg.start_percent,
                                                                    seg.end_percent - seg.start_percent,
                                                                    seg.color,
                                                                ),
                                                            }
                                                        }
                                                        // Dim the not-yet-played remainder (theme-adaptive).
                                                        div {
                                                            class: "absolute inset-y-0 rounded-r-full",
                                                            style: format!(
                                                                "left:{p}%; width:{}%; background:color-mix(in oklch, var(--card) 55%, transparent);",
                                                                100.0 - progress_clamped,
                                                                p = progress_clamped,
                                                            ),
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

                // ── Current song: the hero — header, timeline, transport, tabs ─
                div { class: "flex min-w-0 flex-1 flex-col gap-4",

                    // Header: accent rule + title + quiet meta caption.
                    div { class: "flex items-start gap-3",
                        div {
                            class: "mt-1 h-9 w-1 shrink-0 rounded-full",
                            style: format!("background:{accent};"),
                        }
                        div { class: "min-w-0 flex-1",
                            div { class: "flex flex-wrap items-baseline gap-x-2",
                                h1 { class: "text-2xl font-bold leading-tight tracking-tight text-foreground truncate",
                                    "{title}"
                                }
                                if !artist.is_empty() {
                                    span { class: "text-sm text-muted-foreground truncate", "{artist}" }
                                }
                            }
                            div {
                                class: "mt-1.5 flex flex-wrap items-center gap-x-2.5 gap-y-1 text-[11px] font-semibold uppercase tracking-[0.1em] text-muted-foreground",
                                span { "Song {idx + 1} / {count}" }
                                if let Some(k) = manifest.key.as_ref() {
                                    span { class: "text-border", "·" }
                                    span { "Key {k}" }
                                }
                                if let Some(b) = manifest.bpm {
                                    span { class: "text-border", "·" }
                                    span { "{b:.0} BPM" }
                                }
                                if let Some(ts) = manifest.time_signature.as_ref() {
                                    span { class: "text-border", "·" }
                                    span { "{ts}" }
                                }
                                if is_buffering {
                                    span { class: "text-border", "·" }
                                    span { class: "normal-case tracking-normal text-muted-foreground/70", "buffering…" }
                                }
                            }
                        }
                    }

                    // Section timeline — the "where am I" hero. Caption above
                    // names the current section and the elapsed / total time.
                    if !manifest.sections.is_empty() {
                        div { class: "flex flex-col gap-2",
                            div { class: "flex items-center justify-between gap-2",
                                span { class: "truncate text-xs font-semibold uppercase tracking-[0.08em] text-foreground",
                                    {cur_section_name.clone().unwrap_or_default()}
                                }
                                span { class: "shrink-0 text-[11px] font-medium tabular-nums text-muted-foreground",
                                    "{time_str}"
                                }
                            }
                            SongProgressBar {
                                progress: song_progress,
                                sections: sections.clone(),
                                song_key: manifest.key.clone(),
                                on_section_click,
                            }
                        }
                    }

                    // Transport (full-width, compact so the six controls fit) +
                    // whole-song prev/next below it.
                    div { class: "flex flex-col gap-2",
                        div { class: "h-14 rounded-lg overflow-hidden border border-border",
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
                                compact: true,
                            }
                        }
                        div { class: "grid grid-cols-2 gap-2",
                            button {
                                class: "flex items-center gap-2 rounded-lg border border-border px-3 py-1.5 text-left hover:bg-accent disabled:opacity-40 transition-colors",
                                disabled: at_first,
                                onclick: {
                                    let goto_song = goto_song;
                                    move |_| if idx > 0 { goto_song.call(idx - 1) }
                                },
                                span { class: "text-lg leading-none text-muted-foreground", "‹" }
                                span { class: "flex min-w-0 flex-col",
                                    span { class: "text-[10px] font-semibold uppercase tracking-wide text-muted-foreground", "Prev" }
                                    span { class: "truncate text-sm font-medium text-foreground",
                                        {prev_title.clone().unwrap_or_else(|| "—".to_owned())}
                                    }
                                }
                            }
                            button {
                                class: "flex items-center justify-end gap-2 rounded-lg border border-border px-3 py-1.5 text-right hover:bg-accent disabled:opacity-40 transition-colors",
                                disabled: at_last,
                                onclick: {
                                    let goto_song = goto_song;
                                    move |_| if idx + 1 < count { goto_song.call(idx + 1) }
                                },
                                span { class: "flex min-w-0 flex-col items-end",
                                    span { class: "text-[10px] font-semibold uppercase tracking-wide text-muted-foreground", "Next" }
                                    span { class: "truncate text-sm font-medium text-foreground",
                                        {next_title.clone().unwrap_or_else(|| "—".to_owned())}
                                    }
                                }
                                span { class: "text-lg leading-none text-muted-foreground", "›" }
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

                    // Chart tab — follows the active song index for free.
                    div { class: if active == Tab::Chart { "block" } else { "hidden" },
                        div { class: "border border-border rounded-lg overflow-hidden bg-card",
                            SessionChartPane {}
                        }
                    }
                }
            }
        }
    }

    /// `mm:ss` for the timeline caption's elapsed / total readout.
    fn fmt_time(secs: f64) -> String {
        let s = secs.max(0.0);
        let m = (s / 60.0).floor() as i64;
        let rem = (s % 60.0).floor() as i64;
        format!("{m}:{rem:02}")
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
