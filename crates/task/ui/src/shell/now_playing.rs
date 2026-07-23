//! The global **Now Playing** mini-player.
//!
//! Split in two so the music survives navigation while the UI lives in the
//! status bar:
//!
//! - [`GlobalNowPlayer`] is the headless engine — mounted once in
//!   [`crate::shell::app_shell::AppShell`], OUTSIDE the route `Outlet`, so
//!   its single `<audio>` element keeps playing across route changes. It
//!   owns the queue (captured at play time, see
//!   [`crate::chrome::NowPlayingRequest`]), mirrors its state into
//!   [`NowPlayingCtl`], and executes transport commands the UI posts back.
//!   It renders nothing.
//! - [`NowPlayingTab`] is the UI — a small rounded tab docked in the
//!   bottom-right of the IDE status bar (poking up a few px above the status
//!   line), expanding on hover to reveal prev/next + a scrubber. It reads
//!   [`NowPlayingCtl`] and posts transport commands.
//!
//! It plays each song's REFERENCE stem (one `<audio>` at a time, streamed
//! off disk from `/org/{org}/media/songs/{slug}/{file}`), reusing the
//! setlist stream player's `Track`/`load_tracks`/`element_for`. The
//! multitrack rehearsal rig and the per-song page stay separate.
//!
//! Future: a `video:` queue swaps the `<audio>` for a `<video>`.

use dioxus::prelude::*;

/// A transport command the [`NowPlayingTab`] UI posts to the headless
/// [`GlobalNowPlayer`] engine. `Seek` carries a 0..1 fraction of duration.
#[derive(Clone, Copy, PartialEq)]
pub enum NpCmd {
    Toggle,
    Next,
    Prev,
    Seek(f64),
}

/// Shared control surface between the engine and the status-bar UI. The
/// engine WRITES the view signals and READS `cmd`; the tab READS the view
/// and WRITES `cmd`. Provided once at the app shell so both (siblings) see
/// the same instance.
#[derive(Clone, Copy)]
pub struct NowPlayingCtl {
    /// Current track title. `None` ⇒ nothing playing ⇒ the tab hides.
    pub track_title: Signal<Option<String>>,
    /// Queue label, e.g. `"Sunday Worship · 1/6"`.
    pub queue_label: Signal<String>,
    pub playing: Signal<bool>,
    /// Progress 0..1.
    pub frac: Signal<f64>,
    pub pos: Signal<f64>,
    pub dur: Signal<f64>,
    pub can_prev: Signal<bool>,
    pub can_next: Signal<bool>,
    /// Command bus: `(generation, cmd)` — a bumped generation makes repeats
    /// observable.
    pub cmd: Signal<(u64, NpCmd)>,
}

/// Install [`NowPlayingCtl`]. Call once in the app shell, above both the
/// engine and the status bar.
pub fn provide_now_playing_ctl() {
    use_context_provider(|| NowPlayingCtl {
        track_title: Signal::new(None),
        queue_label: Signal::new(String::new()),
        playing: Signal::new(false),
        frac: Signal::new(0.0),
        pos: Signal::new(0.0),
        dur: Signal::new(0.0),
        can_prev: Signal::new(false),
        can_next: Signal::new(false),
        cmd: Signal::new((0, NpCmd::Toggle)),
    });
}

fn fmt_mmss(s: f64) -> String {
    let s = s.max(0.0) as u64;
    format!("{}:{:02}", s / 60, s % 60)
}

/// The bottom-right status-bar tab. Renders nothing until something plays.
/// Collapsed it's just play/pause + title + a hairline progress bar; on
/// hover it expands to prev/next + a scrubber + times.
#[component]
pub fn NowPlayingTab() -> Element {
    let ctl = use_context::<NowPlayingCtl>();
    let title = ctl.track_title.read().clone();
    let Some(title) = title else {
        return rsx! {};
    };
    let label = ctl.queue_label.read().clone();
    let playing = (ctl.playing)();
    let frac = (ctl.frac)();
    let pos = (ctl.pos)();
    let dur = (ctl.dur)();
    let can_prev = (ctl.can_prev)();
    let can_next = (ctl.can_next)();
    let cmd = ctl.cmd;
    let send = move |c: NpCmd| {
        let mut cmd = cmd; // local Copy of the signal handle → `send` stays `Fn`
        let g = cmd.peek().0 + 1;
        cmd.set((g, c));
    };

    rsx! {
        // A proper mini-player tab: ~twice the status-bar height (h-12 vs
        // h-6) so it pokes well up out of the status line, wide, with a
        // full-width seekable progress bar and transport always visible.
        // Rounded top, open bottom → reads as a tab embedded in the bar.
        div {
            class: "flex h-12 w-80 flex-col justify-center gap-1 rounded-t-lg border border-b-0 border-border bg-card px-3 py-1.5 shadow-md",
            title: "{label}",
            // row 1: transport · title/queue · time
            div { class: "flex items-center gap-2",
                button {
                    r#type: "button",
                    class: "shrink-0 text-xs text-muted-foreground hover:text-foreground disabled:opacity-30",
                    disabled: !can_prev,
                    onclick: move |_| send(NpCmd::Prev),
                    "⏮"
                }
                button {
                    r#type: "button",
                    class: "flex h-6 w-6 shrink-0 items-center justify-center rounded-full bg-primary text-[11px] text-primary-foreground hover:opacity-90",
                    onclick: move |_| send(NpCmd::Toggle),
                    if playing { "⏸" } else { "▶" }
                }
                button {
                    r#type: "button",
                    class: "shrink-0 text-xs text-muted-foreground hover:text-foreground disabled:opacity-30",
                    disabled: !can_next,
                    onclick: move |_| send(NpCmd::Next),
                    "⏭"
                }
                div { class: "min-w-0 flex-1 leading-tight",
                    div { class: "truncate text-xs font-semibold text-foreground", "{title}" }
                    div { class: "truncate text-[10px] text-muted-foreground", "{label}" }
                }
                span { class: "shrink-0 text-[10px] tabular-nums text-muted-foreground",
                    "{fmt_mmss(pos)} / {fmt_mmss(dur)}"
                }
            }
            // row 2: full-width seekable progress bar
            input {
                r#type: "range",
                min: "0",
                max: "1000",
                value: "{(frac * 1000.0) as i64}",
                class: "h-1 w-full cursor-pointer accent-primary",
                oninput: move |e| {
                    if let Ok(v) = e.value().parse::<f64>() {
                        send(NpCmd::Seek(v / 1000.0));
                    }
                },
            }
        }
    }
}

#[cfg(target_arch = "wasm32")]
mod imp {
    use std::cell::RefCell;
    use std::rc::Rc;

    use dioxus::prelude::*;
    use web_sys::HtmlAudioElement;

    use super::{NowPlayingCtl, NpCmd};
    use crate::chrome::NowPlaying;
    use crate::pages::setlist_stream::imp::{Track, element_for, load_tracks};

    /// Headless engine: owns the audio + queue, mirrors state to
    /// [`NowPlayingCtl`], and runs transport commands. Renders nothing (the
    /// UI is [`super::NowPlayingTab`], in the status bar).
    #[component]
    pub fn GlobalNowPlayer() -> Element {
        let element: Rc<RefCell<Option<HtmlAudioElement>>> =
            use_hook(|| Rc::new(RefCell::new(None)));
        let mut queue_key = use_signal(|| (String::new(), Vec::<String>::new()));
        let mut title = use_signal(String::new);
        let mut current = use_signal(|| None::<usize>);
        let mut playing = use_signal(|| false);
        let mut position = use_signal(|| 0.0f64);
        let mut duration = use_signal(|| 0.0f64);
        let mut pending_start = use_signal(|| None::<usize>);

        let tracks = use_resource(move || {
            let (org, songs) = queue_key();
            async move {
                if songs.is_empty() {
                    Vec::<Track>::new()
                } else {
                    load_tracks(&org, &songs).await.unwrap_or_default()
                }
            }
        });

        let select = use_callback({
            let element = element.clone();
            move |i: usize| {
                let track = {
                    let list = tracks.peek();
                    match list.as_ref().and_then(|l| l.get(i).cloned()) {
                        Some(t) => t,
                        None => return,
                    }
                };
                let Some(file) = track.reference.clone() else {
                    tracing::warn!("now-playing: `{}` has no reference stem", track.slug);
                    return;
                };
                let org = queue_key.peek().0.clone();
                if let Some(old) = element.borrow_mut().take() {
                    let _ = old.pause();
                }
                current.set(Some(i));
                playing.set(true);
                position.set(0.0);
                duration.set(track.duration_sec);
                match element_for(&org, &track.slug, &file) {
                    Ok(el) => {
                        let _ = el.play();
                        *element.borrow_mut() = Some(el);
                    }
                    Err(e) => tracing::warn!("now-playing: `{}`: {e}", track.slug),
                }
            }
        });

        let toggle = use_callback({
            let element = element.clone();
            move |()| {
                if current.peek().is_none() {
                    select.call(0);
                    return;
                }
                if let Some(el) = element.borrow().as_ref() {
                    if el.paused() {
                        let _ = el.play();
                        playing.set(true);
                    } else {
                        let _ = el.pause();
                        playing.set(false);
                    }
                }
            }
        });

        let seek = use_callback({
            let element = element.clone();
            move |frac: f64| {
                let dur = duration.peek().max(0.0);
                if dur <= 0.0 {
                    return;
                }
                let t = frac.clamp(0.0, 1.0) * dur;
                if let Some(el) = element.borrow().as_ref() {
                    el.set_current_time(t);
                    position.set(t);
                }
            }
        });

        // Play a pending start once the queue's tracks load.
        use_effect(move || {
            let ready = tracks.read().as_ref().map(|l| !l.is_empty()).unwrap_or(false);
            let pending = *pending_start.peek();
            if ready {
                if let Some(s) = pending {
                    pending_start.set(None);
                    select.call(s);
                }
            }
        });

        // Answer global play requests.
        {
            let req = use_context::<NowPlaying>().0;
            let mut last_gen = use_signal(|| 0u64);
            use_effect(move || {
                let r = req();
                if r.generation == 0 || r.generation == *last_gen.peek() {
                    return;
                }
                last_gen.set(r.generation);
                let same = {
                    let k = queue_key.peek();
                    k.0 == r.org && k.1 == r.songs
                };
                if same {
                    let ready = tracks.peek().as_ref().map(|l| !l.is_empty()).unwrap_or(false);
                    if ready {
                        if r.toggle {
                            toggle.call(());
                        } else {
                            select.call(r.start);
                        }
                    } else {
                        pending_start.set(Some(r.start));
                    }
                } else {
                    title.set(r.title.clone());
                    current.set(None);
                    pending_start.set(Some(r.start));
                    queue_key.set((r.org.clone(), r.songs.clone()));
                }
            });
        }

        // Run transport commands posted by the status-bar tab.
        {
            let ctl = use_context::<NowPlayingCtl>();
            let cmd = ctl.cmd;
            let mut last = use_signal(|| 0u64);
            use_effect(move || {
                let (g, c) = cmd();
                if g == 0 || g == *last.peek() {
                    return;
                }
                last.set(g);
                match c {
                    NpCmd::Toggle => toggle.call(()),
                    NpCmd::Next => {
                        let i = (*current.peek()).map(|i| i + 1).unwrap_or(0);
                        select.call(i);
                    }
                    NpCmd::Prev => {
                        let i = (*current.peek()).map(|i| i.saturating_sub(1)).unwrap_or(0);
                        select.call(i);
                    }
                    NpCmd::Seek(f) => seek.call(f),
                }
            });
        }

        // Mirror engine state → the shared control surface for the UI.
        {
            let ctl = use_context::<NowPlayingCtl>();
            use_effect(move || {
                let cur = current();
                let list = tracks.read();
                let len = queue_key.read().1.len();
                let qtitle = title();
                match cur.and_then(|i| list.as_ref().and_then(|l| l.get(i)).map(|t| (i, t.clone()))) {
                    Some((i, t)) => {
                        let label = if len > 1 {
                            format!("{qtitle} · {}/{}", i + 1, len)
                        } else {
                            qtitle
                        };
                        ctl.track_title.clone().set(Some(t.title.clone()));
                        ctl.queue_label.clone().set(label);
                        ctl.can_prev.clone().set(i > 0);
                        ctl.can_next.clone().set(i + 1 < len);
                    }
                    None => {
                        ctl.track_title.clone().set(None);
                    }
                }
            });
            use_effect(move || {
                ctl.playing.clone().set(playing());
            });
            use_effect(move || {
                let d = duration();
                let p = position();
                ctl.pos.clone().set(p);
                ctl.dur.clone().set(d);
                ctl.frac.clone().set(if d > 0.0 { (p / d).clamp(0.0, 1.0) } else { 0.0 });
            });
        }

        // 300 ms poll: mirror position/duration, auto-advance on ended.
        {
            let element = element.clone();
            use_future(move || {
                let element = element.clone();
                async move {
                    loop {
                        architect::platform::sleep(std::time::Duration::from_millis(300)).await;
                        let (pos, dur, ended) = match element.borrow().as_ref() {
                            Some(el) => {
                                let d = el.duration();
                                (el.current_time(), if d.is_finite() { d } else { 0.0 }, el.ended())
                            }
                            None => continue,
                        };
                        position.set(pos);
                        if dur > 0.0 {
                            duration.set(dur);
                        }
                        if ended {
                            let len = tracks.peek().as_ref().map(|l| l.len()).unwrap_or(0);
                            let next = (*current.peek()).map(|i| i + 1).unwrap_or(0);
                            if next < len {
                                select.call(next);
                            } else {
                                playing.set(false);
                            }
                        }
                    }
                }
            });
        }

        // Headless — the UI is the status-bar tab.
        rsx! {}
    }
}

#[cfg(target_arch = "wasm32")]
pub use imp::GlobalNowPlayer;

#[cfg(not(target_arch = "wasm32"))]
mod stub {
    use dioxus::prelude::*;

    /// Server/native build: the engine runs in the browser only.
    #[component]
    pub fn GlobalNowPlayer() -> Element {
        rsx! {}
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub use stub::GlobalNowPlayer;
