//! [`MiniPlayer`] — the embeddable player for file pages: stage +
//! timeline (markers included, they're the point) + slim transport,
//! and an expand button that opens the full [`ReviewScreen`] as a
//! fixed overlay. The conversation lives in the full screen; the mini
//! shows where it lands.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::Maximize2;
use uuid::Uuid;

use super::progress::TimelineBand;
use super::stage::VideoStage;
use super::transport::TransportBar;
use super::{DrawCtx, PlayerCtx, ReviewScreen, resolve_sources, use_review_data};

/// Props are `ReadSignal`s so the resource re-resolves when the mount
/// is reused with a different file (a root swap can hand this
/// instance a new `(root_id, path)` in place — plain props would keep
/// streaming the old file's proxy).
#[component]
pub fn MiniPlayer(
    org: ReadSignal<String>,
    root_id: ReadSignal<Uuid>,
    path: ReadSignal<String>,
) -> Element {
    let video_id = use_hook(|| format!("review-mini-{}", Uuid::new_v4().simple()));
    let stage_id = use_hook(|| format!("review-ministage-{}", Uuid::new_v4().simple()));
    let container_id = use_hook(|| format!("review-minibox-{}", Uuid::new_v4().simple()));

    let player = use_hook(|| PlayerCtx::install(&video_id, &stage_id));
    use_context_provider(|| player);
    // The mini never draws, but the stage + timeline read the draw
    // context (focus, viewing) — install an inert one.
    let draw = use_hook(DrawCtx::install);
    use_context_provider(|| draw);
    let data = use_review_data(org, root_id, path);
    use_context_provider(|| data);

    let sources = use_resource(move || {
        let (org, root_id, path) = (org(), root_id(), path());
        async move { resolve_sources(&org, root_id, &path, None).await }
    });

    let mut expanded = use_signal(|| false);

    rsx! {
        div {
            id: container_id.clone(),
            class: "flex flex-col overflow-hidden rounded-md border border-border/40 bg-card",
            {match &*sources.read_unchecked() {
                None => rsx! {
                    div { class: "flex aspect-video max-h-96 items-center justify-center bg-black",
                        div { class: "h-8 w-8 animate-spin rounded-full border-4 border-white/20 border-t-white" }
                    }
                },
                Some(Err(e)) => rsx! {
                    div { class: "px-3 py-2 text-xs text-muted-foreground", "No proxy rendition: {e}" }
                },
                Some(Ok(src)) => rsx! {
                    div { class: "relative",
                        VideoStage {
                            stage_id: stage_id.clone(),
                            video_id: video_id.clone(),
                            src: src.proxy.clone(),
                            mini: true,
                        }
                        // The door to the full experience.
                        button {
                            class: "absolute right-2 top-2 flex h-7 items-center gap-1.5 rounded-md bg-black/60 px-2 text-xs text-white opacity-80 hover:opacity-100",
                            title: "Open the full review",
                            onclick: {
                                let video_id = video_id.clone();
                                move |_| {
                                    // Two mounted players must not both
                                    // sound — hold the mini while the
                                    // full screen is up.
                                    super::pause(&video_id);
                                    expanded.set(true);
                                }
                            },
                            Maximize2 { size: 12 }
                            "Review"
                        }
                    }
                    TimelineBand {
                        video_id: video_id.clone(),
                        comments: ReadSignal::from(data.sorted),
                        filmstrip: src.filmstrip.clone(),
                        mini: true,
                    }
                    TransportBar {
                        video_id: video_id.clone(),
                        container_id: container_id.clone(),
                        mini: true,
                    }
                },
            }}
        }
        if expanded() {
            div { class: "fixed inset-0 z-50",
                ReviewScreen {
                    org,
                    root_id,
                    path,
                    on_close: move |()| expanded.set(false),
                }
            }
        }
    }
}
