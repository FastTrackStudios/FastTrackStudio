//! The global **Now Playing** mini-player.
//!
//! Mounted once in [`crate::shell::app_shell::AppShell`], OUTSIDE the
//! route `Outlet`, so its single `<audio>` element — and therefore the
//! music — survives navigation. You can open the setlist, hit play, then
//! wander off into other notes and keep hearing it; skip-next stays
//! setlist-aware because the player owns a copy of the queue captured at
//! play time (see [`crate::chrome::NowPlayingRequest`]), not a live
//! reference to whichever note is on screen.
//!
//! It plays each song's REFERENCE stem (one `<audio>` at a time, streamed
//! off disk from `/org/{org}/media/songs/{slug}/{file}`) — the same
//! filesystem-first source as the inline setlist stream player, whose
//! `Track`/`load_tracks`/`element_for` helpers it reuses. The multitrack
//! rehearsal rig (fullscreen `SetlistPlayer`) and the per-song page
//! (`SongView`) remain separate, richer experiences.
//!
//! The queue loads through `use_resource` (which owns its async task's
//! lifecycle) rather than a `spawn` inside an effect — an effect re-run
//! would cancel an in-flight `spawn`, so rapid play clicks used to drop
//! the load and never render the bar. A pending start index waits for the
//! resource to resolve and then plays.
//!
//! Future: a `video:` queue would swap the `<audio>` for a `<video>` and
//! surface the frame here — the transport + queue plumbing is unchanged.

#[cfg(target_arch = "wasm32")]
mod imp {
    use std::cell::RefCell;
    use std::rc::Rc;

    use dioxus::prelude::*;
    use web_sys::HtmlAudioElement;

    use crate::chrome::NowPlaying;
    use crate::pages::setlist_stream::imp::{Track, element_for, fmt_time, load_tracks};

    #[component]
    pub fn GlobalNowPlayer() -> Element {
        // The live element (one song at a time). Rc'd so callbacks share it;
        // held in a hook so it lives for the app's lifetime → playback
        // persists across route changes.
        let element: Rc<RefCell<Option<HtmlAudioElement>>> =
            use_hook(|| Rc::new(RefCell::new(None)));
        // (org, songs) of the queue to load; drives the tracks resource.
        let mut queue_key = use_signal(|| (String::new(), Vec::<String>::new()));
        let mut title = use_signal(String::new);
        let mut current = use_signal(|| None::<usize>);
        let mut playing = use_signal(|| false);
        let mut position = use_signal(|| 0.0f64);
        let mut duration = use_signal(|| 0.0f64);
        // A start index waiting for the queue's tracks to finish loading.
        let mut pending_start = use_signal(|| None::<usize>);

        // Load the queue's tracks whenever the queue changes. use_resource
        // owns the task, so an effect re-run can't cancel it mid-flight.
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

        // Select + play track `i` from the loaded queue. Replaces the live
        // element (the old one pauses as it drops out of the slot).
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

        // When the queue's tracks finish loading and a start is pending, play it.
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

        // Answer global play requests. The player captures the queue here,
        // so it's independent of whichever note fired the request.
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
                        // Loaded: header ▶ toggles, a strip click jumps.
                        if r.toggle {
                            toggle.call(());
                        } else {
                            select.call(r.start);
                        }
                    } else {
                        // Still loading the same queue: play once it lands.
                        pending_start.set(Some(r.start));
                    }
                } else {
                    // New queue: swap it in and play `start` once loaded.
                    title.set(r.title.clone());
                    current.set(None);
                    pending_start.set(Some(r.start));
                    queue_key.set((r.org.clone(), r.songs.clone()));
                }
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

        // Nothing queued yet → no bar.
        let Some(idx) = current() else {
            return rsx! {};
        };
        let track = {
            let list = tracks.read();
            match list.as_ref().and_then(|l| l.get(idx).cloned()) {
                Some(t) => t,
                None => return rsx! {},
            }
        };
        let queue_len = queue_key.peek().1.len();
        let dur = duration();
        let pos = position();
        let frac = if dur > 0.0 { (pos / dur).clamp(0.0, 1.0) } else { 0.0 };
        let is_playing = playing();
        let qtitle = title();

        rsx! {
            div {
                class: "fixed inset-x-0 bottom-14 z-40 border-t border-border bg-card/95 backdrop-blur md:bottom-6",
                div { class: "mx-auto flex w-full max-w-3xl items-center gap-3 px-3 py-2",
                    // artwork
                    div { class: "flex h-10 w-10 shrink-0 items-center justify-center rounded-md bg-gradient-to-br from-primary/70 to-primary/20 text-lg",
                        "🎵"
                    }
                    // titles
                    div { class: "min-w-0 flex-1",
                        div { class: "truncate text-sm font-semibold text-foreground", "{track.title}" }
                        div { class: "flex items-center gap-1 truncate text-[11px] uppercase tracking-wider text-muted-foreground",
                            span { class: "truncate", "{qtitle}" }
                            if queue_len > 1 {
                                span { class: "shrink-0", "· {idx + 1}/{queue_len}" }
                            }
                        }
                    }
                    // transport: prev · play/pause · next
                    button {
                        r#type: "button",
                        class: "shrink-0 rounded px-1 text-base text-muted-foreground hover:text-foreground disabled:opacity-30",
                        disabled: idx == 0,
                        onclick: move |_| select.call(idx.saturating_sub(1)),
                        "⏮"
                    }
                    button {
                        r#type: "button",
                        class: "flex h-9 w-9 shrink-0 items-center justify-center rounded-full bg-primary text-primary-foreground hover:opacity-90",
                        onclick: move |_| toggle.call(()),
                        if is_playing { "⏸" } else { "▶" }
                    }
                    button {
                        r#type: "button",
                        class: "shrink-0 rounded px-1 text-base text-muted-foreground hover:text-foreground disabled:opacity-30",
                        disabled: idx + 1 >= queue_len,
                        onclick: move |_| select.call(idx + 1),
                        "⏭"
                    }
                    // time + scrub
                    span { class: "hidden w-10 shrink-0 text-right text-[11px] tabular-nums text-muted-foreground sm:inline",
                        "{fmt_time(pos)}"
                    }
                    input {
                        r#type: "range",
                        min: "0",
                        max: "1000",
                        value: "{(frac * 1000.0) as i64}",
                        class: "hidden h-1.5 w-40 shrink-0 cursor-pointer accent-primary sm:block",
                        oninput: move |e| {
                            if let Ok(v) = e.value().parse::<f64>() {
                                seek.call(v / 1000.0);
                            }
                        },
                    }
                    span { class: "hidden w-10 shrink-0 text-[11px] tabular-nums text-muted-foreground sm:inline",
                        "{fmt_time(dur)}"
                    }
                }
            }
        }
    }
}

#[cfg(target_arch = "wasm32")]
pub use imp::GlobalNowPlayer;

#[cfg(not(target_arch = "wasm32"))]
mod stub {
    use dioxus::prelude::*;

    /// Server/native build: the player runs in the browser only.
    #[component]
    pub fn GlobalNowPlayer() -> Element {
        rsx! {}
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub use stub::GlobalNowPlayer;
