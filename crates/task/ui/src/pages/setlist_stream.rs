//! The embedded (non-fullscreen) setlist view: a Spotify-style streaming
//! player. One row per song, ONE audio stream at a time — each song's
//! REFERENCE track (the `original-track` stem, falling back to the first
//! stem), streamed **over vox** via `MediaService`:
//! MSE `audio/webm; codecs="opus"` fed from `MediaChunk`s
//! (`vox_media_source`), with the signed-URL element as the fallback for
//! non-webm ingests — exactly the `song_session::resolve_front` policy.
//! No worklet, no engine: this is the listen-through player; the
//! fullscreen Experience remains the rehearsal multitrack rig.
//!
//! Track metadata (title, duration, stem hashes) comes from the songs'
//! vault notes (`Songs/<Title>.md` frontmatter), fetched over VaultSync —
//! the same org lane everything else rides.

#[cfg(target_arch = "wasm32")]
mod imp {
    use std::cell::RefCell;
    use std::rc::Rc;

    use dioxus::prelude::*;
    use wasm_bindgen::JsCast;
    use web_sys::HtmlAudioElement;

    use crate::pages::vault::song_front_from;

    /// One playable row.
    #[derive(Clone, PartialEq)]
    pub(crate) struct Track {
        pub slug: String,
        pub title: String,
        pub duration_sec: f64,
        /// Reference stem's content hash — `None` when the song has no
        /// note/stems yet (row renders, can't play).
        pub reference: Option<String>,
        pub stem_count: usize,
    }

    /// Load the setlist's tracks: enumerate `Songs/*.md` over VaultSync,
    /// parse each note's frontmatter, and match notes to setlist slugs by
    /// slugified basename.
    async fn load_tracks(org: &str, slugs: &[String]) -> Result<Vec<Track>, String> {
        let vault = crate::vox_clients::vault_client(org).await?;
        let manifest = vault
            .manifest("default".to_owned())
            .await
            .map_err(|e| format!("vault manifest: {e:?}"))?;
        let mut by_slug: std::collections::HashMap<String, Track> = Default::default();
        for entry in &manifest.files {
            let path = &entry.path;
            let Some(base) = path
                .strip_prefix("Songs/")
                .and_then(|p: &str| p.strip_suffix(".md"))
            else {
                continue;
            };
            let slug = crate::feeds::slugify(base);
            if !slugs.contains(&slug) {
                continue;
            }
            let Ok(bytes) = vault.get_file("default".to_owned(), path.clone()).await else {
                continue;
            };
            let text = String::from_utf8_lossy(&bytes.0).to_string();
            let front = song_front_from(&text);
            let reference = front
                .stems
                .iter()
                .find(|s| {
                    let n = s.name.to_lowercase();
                    n.contains("original") || n.contains("reference")
                })
                .or_else(|| front.stems.first())
                .map(|s| s.content_hash.clone());
            by_slug.insert(
                slug.clone(),
                Track {
                    slug,
                    title: base.to_owned(),
                    duration_sec: front
                        .duration_sec
                        .or_else(|| front.sections.last().map(|s| s.end_sec))
                        .unwrap_or(0.0),
                    reference,
                    stem_count: front.stems.len(),
                },
            );
        }
        // Setlist order; unmatched slugs still get a (silent) row.
        Ok(slugs
            .iter()
            .map(|slug| {
                by_slug.remove(slug).unwrap_or_else(|| Track {
                    slug: slug.clone(),
                    title: slug.replace('-', " "),
                    duration_sec: 0.0,
                    reference: None,
                    stem_count: 0,
                })
            })
            .collect())
    }

    /// Build the `<audio>` element for a reference hash — vox-MSE when the
    /// blob is webm/opus and the browser speaks it, else the signed URL.
    async fn element_for(org: &str, hash: &str) -> Result<HtmlAudioElement, String> {
        use attachments_proto::ContentHashArg;
        if crate::pages::vox_media_source::mse_supported() {
            if let Ok(media) = crate::vox_clients::media_client(org).await {
                if let Ok(info) = media.stat(hash.to_owned()).await {
                    if info.mime_type.starts_with("audio/webm") {
                        return crate::pages::vox_media_source::audio_element_over_vox(
                            org.to_owned(),
                            hash.to_owned(),
                        );
                    }
                }
            }
        }
        let client = crate::vox_clients::attachments_client(org).await?;
        let signed = client
            .get_download_url(ContentHashArg {
                content_hash: hash.to_owned(),
            })
            .await
            .map_err(|e| format!("signed url: {e:?}"))?;
        let el = HtmlAudioElement::new().map_err(|e| format!("audio element: {e:?}"))?;
        el.set_preload("auto");
        el.set_src(&signed.url);
        Ok(el)
    }

    fn fmt_time(sec: f64) -> String {
        let s = sec.max(0.0) as u64;
        format!("{}:{:02}", s / 60, s % 60)
    }

    #[component]
    pub fn SetlistStreamPlayer(org: String, title: String, songs: Vec<String>) -> Element {
        // The live element (one at a time). Rc'd so callbacks share it.
        let element: Rc<RefCell<Option<HtmlAudioElement>>> = use_hook(|| Rc::new(RefCell::new(None)));
        let current = use_signal(|| None::<usize>);
        let playing = use_signal(|| false);
        let position = use_signal(|| 0.0f64);

        let songs_key = songs.clone();
        let org_r = org.clone();
        let tracks = use_resource(use_reactive!(|(songs_key, org_r)| {
            async move { load_tracks(&org_r, &songs_key).await }
        }));

        // Select + play song `i`. Replaces the element (old one pauses on
        // drop-out of the slot); resolves the source over vox.
        let select = use_callback({
            let element = element.clone();
            let org = org.clone();
            let tracks = tracks;
            let mut current = current;
            let mut playing = playing;
            let mut position = position;
            move |i: usize| {
                let Some(Ok(list)) = &*tracks.read_unchecked() else {
                    return;
                };
                let Some(track) = list.get(i).cloned() else { return };
                let Some(hash) = track.reference.clone() else {
                    tracing::warn!("stream: `{}` has no reference stem", track.slug);
                    return;
                };
                if let Some(old) = element.borrow_mut().take() {
                    let _ = old.pause();
                }
                current.set(Some(i));
                playing.set(true);
                position.set(0.0);
                let element = element.clone();
                let org = org.clone();
                spawn(async move {
                    match element_for(&org, &hash).await {
                        Ok(el) => {
                            let _ = el.play();
                            *element.borrow_mut() = Some(el);
                        }
                        Err(e) => tracing::warn!("stream: `{}`: {e}", track.slug),
                    }
                });
            }
        });

        let toggle = use_callback({
            let element = element.clone();
            let mut playing = playing;
            let current = current;
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

        // 300 ms poll: mirror position, auto-advance on ended.
        {
            let element = element.clone();
            let mut position = position;
            let current = current;
            use_future(move || {
                let element = element.clone();
                async move {
                    loop {
                        architect::platform::sleep(std::time::Duration::from_millis(300)).await;
                        let (pos, ended) = match element.borrow().as_ref() {
                            Some(el) => (el.current_time(), el.ended()),
                            None => continue,
                        };
                        position.set(pos);
                        if ended {
                            let next = (*current.peek()).map(|i| i + 1).unwrap_or(0);
                            select.call(next); // no-op past the end
                        }
                    }
                }
            });
        }

        let cur = current();
        let pos = position();

        rsx! {
            div { class: "mx-auto w-full max-w-3xl px-2 py-4",
                // ── header: artwork tile, titles, transport ──
                div { class: "mb-4 flex items-center gap-4",
                    div { class: "flex h-16 w-16 shrink-0 items-center justify-center rounded-lg bg-gradient-to-br from-primary/70 to-primary/20 text-2xl",
                        "🎵"
                    }
                    div { class: "min-w-0 flex-1",
                        div { class: "truncate text-lg font-semibold text-foreground", "{title}" }
                        div { class: "flex items-center gap-1 text-xs uppercase tracking-wider text-muted-foreground",
                            span { "Setlist" }
                            if let Some(i) = cur {
                                span { "· {i + 1} of {songs.len()}" }
                            }
                        }
                    }
                    button {
                        class: "flex h-11 w-11 items-center justify-center rounded-full bg-primary text-primary-foreground hover:opacity-90",
                        onclick: move |_| toggle.call(()),
                        if playing() { "⏸" } else { "▶" }
                    }
                }

                // ── rows ──
                match &*tracks.read_unchecked() {
                    None => rsx! {
                        div { class: "py-8 text-center text-sm text-muted-foreground", "Loading setlist…" }
                    },
                    Some(Err(e)) => rsx! {
                        div { class: "py-8 text-center text-sm text-destructive", "Could not load songs: {e}" }
                    },
                    Some(Ok(list)) => rsx! {
                        div { class: "flex flex-col",
                            for (i, track) in list.clone().into_iter().enumerate() {
                                {
                                    let is_current = cur == Some(i);
                                    let playable = track.reference.is_some();
                                    rsx! {
                                        div {
                                            key: "{track.slug}",
                                            class: if is_current {
                                                "group flex cursor-pointer items-center gap-3 rounded-md bg-accent px-3 py-2.5"
                                            } else if i % 2 == 1 {
                                                "group flex cursor-pointer items-center gap-3 rounded-md bg-muted/30 px-3 py-2.5 hover:bg-accent/60"
                                            } else {
                                                "group flex cursor-pointer items-center gap-3 rounded-md px-3 py-2.5 hover:bg-accent/60"
                                            },
                                            onclick: move |_| select.call(i),
                                            span {
                                                class: if is_current {
                                                    "w-6 shrink-0 text-right text-sm tabular-nums text-primary"
                                                } else {
                                                    "w-6 shrink-0 text-right text-sm tabular-nums text-muted-foreground"
                                                },
                                                if is_current && playing() { "▶" } else { "{i + 1}" }
                                            }
                                            span {
                                                class: if playable {
                                                    "min-w-0 flex-1 truncate text-sm text-foreground"
                                                } else {
                                                    "min-w-0 flex-1 truncate text-sm text-muted-foreground"
                                                },
                                                "{track.title}"
                                            }
                                            if track.stem_count > 0 {
                                                span { class: "shrink-0 rounded-full border border-border px-1.5 py-0.5 text-[10px] text-muted-foreground",
                                                    "{track.stem_count} stems"
                                                }
                                            }
                                            span { class: "w-12 shrink-0 text-right text-xs tabular-nums text-muted-foreground",
                                                if is_current {
                                                    "{fmt_time(pos)}"
                                                } else {
                                                    "{fmt_time(track.duration_sec)}"
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    },
                }
            }
        }
    }
}

#[cfg(target_arch = "wasm32")]
pub use imp::SetlistStreamPlayer;

#[cfg(not(target_arch = "wasm32"))]
mod stub {
    use dioxus::prelude::*;

    #[component]
    pub fn SetlistStreamPlayer(org: String, title: String, songs: Vec<String>) -> Element {
        let _ = (&org, &title, &songs);
        rsx! {
            div { class: "px-4 py-8 text-sm text-muted-foreground",
                "The setlist streaming player runs in the browser."
            }
        }
    }
}

#[cfg(not(target_arch = "wasm32"))]
pub use stub::SetlistStreamPlayer;
