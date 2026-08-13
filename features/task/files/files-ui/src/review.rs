//! The Review player (issue #270, Phase A / AC 1): a media file's proxy
//! rendition in a `<video>`, with timecode seeking and filmstrip
//! scrubbing. Originals never stream — the player resolves *renditions*
//! (issue #269) over the `rendition` RPC and streams them from the
//! ranged rendition route, so seeking never downloads the whole file.
//!
//! ## Why the URL carries a token
//!
//! `<video src>` can't set an `Authorization` header and can't await an
//! RPC, so the player mints one signed media grant over vox — prefix
//! `files/renditions/{root_id}`, covering the file's whole rendition
//! ladder — and appends it as `?token=`. Minting goes through the
//! shared [`task_ui_core::media_grant`] cache (one grant per (org,
//! prefix), refresh margin included); a failed mint yields an empty
//! suffix, which still plays while `TASK_ENFORCE_MEDIA_TOKEN` is off
//! (the same rollout contract as the stem player's grants).
//!
//! ## Why seeks go through `document::eval`
//!
//! Dioxus media events don't expose `currentTime`, and a seek is a
//! property write on the live element — both are two-line JS against the
//! element's id, same as the shell's watch page.

use dioxus::prelude::*;
use files_proto::{
    AnnotationPoint, AnnotationStroke, FilesEvent, FilesServiceStreamClient, NewReviewComment,
    RenditionKind, Review, ReviewComment,
};
use fts_ui::prelude::*;
use uuid::Uuid;

/// Extensions the review player mounts for. Matching is by name only —
/// the `rendition` RPC is the authority (a misnamed file simply resolves
/// no proxy and the player says so).
pub fn is_video_path(path: &str) -> bool {
    let ext = path.rsplit('.').next().unwrap_or_default().to_lowercase();
    matches!(
        ext.as_str(),
        "mov" | "mp4" | "m4v" | "mkv" | "webm" | "avi" | "mxf" | "mts"
    )
}

/// The streaming route's URL for a resolved rendition (`tok` is the
/// `?token=…` suffix, or empty).
fn rendition_url(
    org: &str,
    root_id: Uuid,
    kind: RenditionKind,
    file_id: &str,
    tok: &str,
) -> String {
    format!(
        "/org/{org}/files/renditions/{root_id}/{}/{file_id}{tok}",
        kind.tag()
    )
}

/// Parse a timecode — `ss(.f)`, `mm:ss`, or `h:mm:ss` — to seconds.
/// (The `links_proto` timecode parser is whole-second; a review seek
/// wants fractional precision, hence the f64 twin.)
pub fn parse_timecode(s: &str) -> Option<f64> {
    let s = s.trim();
    if s.is_empty() {
        return None;
    }
    let parts: Vec<&str> = s.split(':').collect();
    if parts.len() > 3 {
        return None;
    }
    let mut total = 0.0;
    for part in parts {
        let v: f64 = part.trim().parse().ok()?;
        if v < 0.0 || !v.is_finite() {
            return None;
        }
        total = total * 60.0 + v;
    }
    Some(total)
}

/// The player's clock display — the shared playback-clock spelling
/// (`m:ss` / `h:mm:ss`), saturated from the element's float seconds.
fn display_timecode(secs: f64) -> String {
    // f64→u32 `as` saturates and maps NaN to 0.
    task_ui_core::format::duration_hms(secs as u32)
}

/// What the player streams: the proxy URL, and the filmstrip URL when
/// the source yields one.
#[derive(Clone, Debug, PartialEq)]
struct Sources {
    proxy: String,
    filmstrip: Option<String>,
}

/// Resolve the opened file to its streamable sources: proxy + filmstrip
/// renditions over the RPC (generated on demand, cached server-side),
/// plus one grant covering both URLs. `at` pins a past version (the
/// switcher, issue #270 AC 4); `None` follows the checkpoint head.
async fn resolve_sources(
    org: &str,
    root_id: Uuid,
    path: &str,
    at: Option<String>,
) -> Result<Sources, String> {
    let c = crate::client(org).await?;
    let rendition = |kind: RenditionKind| {
        let c = c.clone();
        let at = at.clone();
        async move {
            match at {
                Some(commit) => c.rendition_at(root_id, path.to_owned(), commit, kind).await,
                None => c.rendition(root_id, path.to_owned(), kind).await,
            }
        }
    };
    let proxy = rendition(RenditionKind::Proxy720)
        .await
        .map_err(|e| e.to_string())?;
    // No filmstrip is not an error — the proxy still plays; the scrub
    // strip simply doesn't render.
    let filmstrip = rendition(RenditionKind::Filmstrip).await.ok();
    let tok =
        task_ui_core::media_grant::suffix_for_prefix(org, &format!("files/renditions/{root_id}"))
            .await;
    Ok(Sources {
        proxy: rendition_url(org, root_id, RenditionKind::Proxy720, &proxy.file_id, &tok),
        filmstrip: filmstrip
            .map(|f| rendition_url(org, root_id, RenditionKind::Filmstrip, &f.file_id, &tok)),
    })
}

/// The file's review if it exists — a pure read (follows renames
/// server-side). Browsing must never mint entities.
async fn find_review(org: &str, root_id: Uuid, path: &str) -> Result<Option<Review>, String> {
    crate::client(org)
        .await?
        .find_review(root_id, path.to_owned())
        .await
        .map_err(|e| e.to_string())
}

/// Get-or-create the file's review — called when feedback actually
/// starts (the first comment), never on mount.
async fn ensure_review(org: &str, root_id: Uuid, path: &str) -> Result<Review, String> {
    crate::client(org)
        .await?
        .review_for_file(root_id, path.to_owned())
        .await
        .map_err(|e| e.to_string())
}

async fn fetch_comments(org: &str, review_id: Uuid) -> Result<Vec<ReviewComment>, String> {
    crate::client(org)
        .await?
        .review_comments(review_id)
        .await
        .map_err(|e| e.to_string())
}

async fn post_comment(
    org: &str,
    review_id: Uuid,
    comment: NewReviewComment,
) -> Result<ReviewComment, String> {
    crate::client(org)
        .await?
        .add_review_comment(review_id, comment)
        .await
        .map_err(|e| e.to_string())
}

async fn remove_comment(org: &str, id: Uuid) -> Result<(), String> {
    crate::client(org)
        .await?
        .delete_review_comment(id)
        .await
        .map_err(|e| e.to_string())
}

/// Seek the player to an absolute time.
fn seek_to(video_id: &str, secs: f64) {
    let _ = dioxus::document::eval(&format!(
        "var v=document.getElementById('{video_id}');if(v){{v.currentTime={secs};}}"
    ));
}

/// Seek the player to a horizontal fraction of the filmstrip —
/// `x` is the click's element-relative x in CSS pixels.
fn scrub_to(video_id: &str, strip_id: &str, x: f64) {
    let _ = dioxus::document::eval(&format!(
        "var v=document.getElementById('{video_id}');\
         var s=document.getElementById('{strip_id}');\
         if(v&&s&&s.clientWidth>0&&isFinite(v.duration)){{\
         v.currentTime=Math.max(0,Math.min(1,{x}/s.clientWidth))*v.duration;}}"
    ));
}

/// Read the player's clock back into Rust — the timecode display.
async fn read_time(video_id: &str) -> f64 {
    let mut e = dioxus::document::eval(&format!(
        "var v=document.getElementById('{video_id}');dioxus.send(v?v.currentTime:0);"
    ));
    e.recv::<f64>().await.unwrap_or(0.0)
}

/// An element's CSS-pixel size — what normalizes pointer coordinates
/// into the frame's `0..=1` space when a drawing starts. Measured via
/// `getBoundingClientRect` (an SVG element's `clientWidth`/`Height`
/// are 0 on Firefox).
async fn element_dims(id: &str) -> (f64, f64) {
    let mut e = dioxus::document::eval(&format!(
        "var el=document.getElementById('{id}');\
         if(el){{var r=el.getBoundingClientRect();dioxus.send([r.width,r.height]);}}\
         else{{dioxus.send([0,0]);}}"
    ));
    e.recv::<(f64, f64)>().await.unwrap_or((0.0, 0.0))
}

/// The one stroke style Phase C draws with (frame.io-red, ~4px on a
/// 1000px-wide frame). A palette is later polish; the data model
/// already carries per-stroke color/width.
const STROKE_COLOR: &str = "#ff3355";
const STROKE_WIDTH: f32 = 0.004;

/// Normalize a pointer position against the overlay's size, clamped
/// into the frame.
fn normalized_point(x: f64, y: f64, dims: (f64, f64)) -> Option<AnnotationPoint> {
    if dims.0 <= 0.0 || dims.1 <= 0.0 {
        return None;
    }
    #[allow(clippy::cast_possible_truncation)]
    Some(AnnotationPoint {
        x: (x / dims.0).clamp(0.0, 1.0) as f32,
        y: (y / dims.1).clamp(0.0, 1.0) as f32,
    })
}

/// A stroke's `<polyline points>` attribute in the normalized viewBox.
fn points_attr(stroke: &AnnotationStroke) -> String {
    stroke
        .points
        .iter()
        .map(|p| format!("{},{}", p.x, p.y))
        .collect::<Vec<_>>()
        .join(" ")
}

/// The review player for one opened media file: proxy playback,
/// timecode display + seek, filmstrip scrub.
///
/// Props are `ReadSignal`s so the resource re-resolves when the
/// mount is reused with a different file (a root swap can hand this
/// instance a new `(root_id, path)` in place — plain props would keep
/// streaming the old file's proxy).
#[component]
pub fn ReviewPlayer(
    org: ReadSignal<String>,
    root_id: ReadSignal<Uuid>,
    path: ReadSignal<String>,
) -> Element {
    // Stable per-mount element ids — the eval seams address the live
    // elements by id, and two open files must not cross wires.
    let video_id = use_hook(|| format!("review-video-{}", Uuid::new_v4().simple()));
    let strip_id = use_hook(|| format!("review-strip-{}", Uuid::new_v4().simple()));

    // The version switcher (issue #270 AC 4): the file's chain, the
    // pinned version (`None` follows the head), and an optional second
    // version compared side by side. A pin carries `(commit, path)` —
    // the chain follows renames, so a pre-rename version must be
    // resolved under the path it lived at, not the current one.
    let versions = use_resource(move || {
        let (org, root_id, path) = (org(), root_id(), path());
        async move { crate::fetch_chain(&org, root_id, path).await }
    });
    let mut selected = use_signal(|| Option::<(String, String)>::None);
    let mut compare = use_signal(|| Option::<(String, String)>::None);
    let head_commit = use_memo(move || match &*versions.read() {
        Some(Ok(chain)) => chain.first().map(|e| e.commit_id.clone()),
        _ => None,
    });
    // What the main player is actually showing — what a fresh comment
    // records as its file version.
    let watching = use_memo(move || selected().map(|(commit, _)| commit).or(head_commit()));

    let sources = use_resource(move || {
        let (org, root_id, path, pin) = (org(), root_id(), path(), selected());
        async move {
            match pin {
                Some((commit, vpath)) => resolve_sources(&org, root_id, &vpath, Some(commit)).await,
                None => resolve_sources(&org, root_id, &path, None).await,
            }
        }
    });
    let compare_sources = use_resource(move || {
        let (org, root_id, pin) = (org(), root_id(), compare());
        async move {
            match pin {
                Some((commit, vpath)) => {
                    Some(resolve_sources(&org, root_id, &vpath, Some(commit)).await)
                }
                None => None,
            }
        }
    });

    // A checkpoint anywhere on this root moves the head: re-read the
    // chain (so the switcher shows the new version and fresh comments
    // record the right commit) and, when following the head, re-resolve
    // the stream. Without this an open review page would silently
    // attribute new feedback to the previous version.
    architect::use_stream(
        move |tx| {
            let org = org();
            async move {
                let Ok(stream) =
                    task_ui_core::vox_clients::establish_for::<FilesServiceStreamClient>(&org)
                        .await
                else {
                    return false;
                };
                stream.events(tx).await.is_ok()
            }
        },
        move |event: FilesEvent| {
            let (mut versions, mut sources) = (versions, sources);
            if let FilesEvent::Checkpointed(info) = &event
                && info.root_id == *root_id.peek()
            {
                versions.restart();
                if selected.peek().is_none() {
                    sources.restart();
                }
            }
        },
    );

    let mut now = use_signal(|| 0.0f64);
    let timecode_input = use_signal(String::new);

    // Frame-annotation state (issue #270 Phase C). `pending` is the
    // drawing being authored (attached to the next comment); `viewing`
    // is a saved comment's drawing shown over the frame; `active` is
    // the stroke under the pointer right now. All in normalized frame
    // coordinates — `dims` (the overlay's CSS size, read when draw
    // mode opens) is only the capture-time divisor.
    let mut draw_mode = use_signal(|| false);
    let mut pending = use_signal(Vec::<AnnotationStroke>::new);
    let viewing = use_signal(Vec::<AnnotationStroke>::new);
    let mut active = use_signal(|| Option::<AnnotationStroke>::None);
    let mut dims = use_signal(|| (0.0f64, 0.0f64));
    let overlay_id = use_hook(|| format!("review-overlay-{}", Uuid::new_v4().simple()));

    let on_seek = {
        let video_id = video_id.clone();
        move |_| {
            if let Some(secs) = parse_timecode(&timecode_input.peek()) {
                seek_to(&video_id, secs);
            }
        }
    };

    let toggle_draw = {
        let (video_id, overlay_id) = (video_id.clone(), overlay_id.clone());
        move |_| {
            let entering = !*draw_mode.peek();
            draw_mode.set(entering);
            active.set(None);
            if entering {
                // Drawing pins a frame: pause, then measure the overlay
                // so pointer coordinates normalize against it.
                let _ = dioxus::document::eval(&format!(
                    "var v=document.getElementById('{video_id}');if(v){{v.pause();}}"
                ));
                let overlay_id = overlay_id.clone();
                spawn(async move { dims.set(element_dims(&overlay_id).await) });
            } else {
                pending.set(Vec::new());
            }
        }
    };

    rsx! {
        div { class: "flex flex-col gap-2 rounded-md border border-border/30 p-2",
            {match &*sources.read_unchecked() {
                None => rsx! {
                    Text { variant: TextVariant::Muted, class: "text-xs", "Resolving proxy rendition…" }
                },
                Some(Err(e)) => rsx! {
                    // No transcoder, or the file yields no proxy — the
                    // review player degrades to a note, never a broken
                    // <video>.
                    div { class: "text-xs text-muted-foreground", "No proxy rendition: {e}" }
                },
                Some(Ok(src)) => rsx! {
                    // Version switcher: pin the player to any version in
                    // the chain; ⇆ holds a second one beside it (AC 4).
                    if let Some(Ok(chain)) = &*versions.read_unchecked() {
                        if chain.len() > 1 {
                            div { class: "flex items-center gap-1 overflow-x-auto text-xs",
                                span { class: "text-muted-foreground shrink-0", "Version:" }
                                for (i , entry) in chain.iter().cloned().enumerate() {
                                    VersionChip {
                                        key: "{entry.commit_id}",
                                        number: chain.len() - i,
                                        entry: entry.clone(),
                                        watched: *watching.read() == Some(entry.commit_id.clone()),
                                        compared: compare().map(|(commit, _)| commit) == Some(entry.commit_id.clone()),
                                        on_watch: {
                                            // The pin carries the version's
                                            // OWN path — a pre-rename entry
                                            // resolves under it.
                                            let pin = (entry.commit_id.clone(), entry.path.clone());
                                            move |()| {
                                                // Head stays "follow the
                                                // head", not a pin.
                                                if i == 0 {
                                                    selected.set(None);
                                                } else {
                                                    selected.set(Some(pin.clone()));
                                                }
                                            }
                                        },
                                        on_compare: {
                                            let pin = (entry.commit_id.clone(), entry.path.clone());
                                            move |()| {
                                                let same = compare.peek().as_ref().map(|(commit, _)| commit)
                                                    == Some(&pin.0);
                                                if same {
                                                    compare.set(None);
                                                } else {
                                                    compare.set(Some(pin.clone()));
                                                }
                                            }
                                        },
                                    }
                                }
                            }
                        }
                    }
                    if selected().is_some() {
                        div { class: "flex items-center gap-2 text-xs",
                            Badge { variant: BadgeVariant::Outline, "viewing an older version" }
                            button {
                                class: "rounded px-1.5 py-0.5 text-muted-foreground hover:bg-muted/50",
                                onclick: move |_| selected.set(None),
                                "back to latest"
                            }
                        }
                    }
                    div { class: if compare().is_some() { "grid gap-2 md:grid-cols-2" } else { "contents" },
                    // The wrapper shrink-wraps the video's USED box
                    // (inline-block), so the inset-0 overlay is exactly
                    // the picture — with a full-width wrapper the video
                    // narrows under max-h and normalized coordinates
                    // would mis-anchor (AC 3's whole point).
                    div { class: "relative inline-block max-w-full",
                        video {
                            id: video_id.clone(),
                            src: src.proxy.clone(),
                            controls: !draw_mode(),
                            preload: "metadata",
                            class: "block max-w-full max-h-96 rounded bg-black/80",
                            ontimeupdate: {
                                let video_id = video_id.clone();
                                move |_| {
                                    let video_id = video_id.clone();
                                    spawn(async move { now.set(read_time(&video_id).await) });
                                }
                            },
                        }
                        // Annotation layer: normalized frame space
                        // (`0..=1` both axes, stretched to the box), so a
                        // drawing re-anchors across renditions and window
                        // sizes (AC 3). Pointer-transparent unless drawing.
                        svg {
                            id: overlay_id.clone(),
                            class: if draw_mode() { "absolute inset-0 h-full w-full cursor-crosshair touch-none" } else { "absolute inset-0 h-full w-full pointer-events-none" },
                            view_box: "0 0 1 1",
                            preserve_aspect_ratio: "none",
                            onmousedown: {
                                let overlay_for_measure = overlay_id.clone();
                                move |evt: Event<MouseData>| {
                                    if !*draw_mode.peek() {
                                        return;
                                    }
                                    // Every stroke start re-measures for the
                                    // NEXT events (resize between strokes);
                                    // an unmeasured overlay (the async
                                    // measure from ✏ hasn't landed) captures
                                    // nothing rather than capturing garbage.
                                    let overlay_for_measure = overlay_for_measure.clone();
                                    spawn(async move {
                                        dims.set(element_dims(&overlay_for_measure).await);
                                    });
                                    let c = evt.data().element_coordinates();
                                    if let Some(p) = normalized_point(c.x, c.y, *dims.peek()) {
                                        active.set(Some(AnnotationStroke {
                                            points: vec![p],
                                            color: STROKE_COLOR.into(),
                                            width: STROKE_WIDTH,
                                        }));
                                    }
                                }
                            },
                            onmousemove: move |evt: Event<MouseData>| {
                                if active.peek().is_none() {
                                    return;
                                }
                                let c = evt.data().element_coordinates();
                                if let Some(p) = normalized_point(c.x, c.y, *dims.peek()) {
                                    if let Some(stroke) = active.write().as_mut() {
                                        stroke.points.push(p);
                                    }
                                }
                            },
                            onmouseup: move |_| {
                                if let Some(stroke) = active.take() {
                                    if stroke.points.len() > 1 {
                                        pending.write().push(stroke);
                                    }
                                }
                            },
                            onmouseleave: move |_| {
                                if let Some(stroke) = active.take() {
                                    if stroke.points.len() > 1 {
                                        pending.write().push(stroke);
                                    }
                                }
                            },
                            for stroke in viewing().iter().chain(pending().iter()).chain(active().iter()) {
                                polyline {
                                    points: points_attr(stroke),
                                    fill: "none",
                                    stroke: stroke.color.clone(),
                                    stroke_linecap: "round",
                                    stroke_linejoin: "round",
                                    // Non-scaling stroke: the viewBox is
                                    // stretched non-uniformly, so width
                                    // must be screen-space.
                                    stroke_width: format!("{}", (f64::from(stroke.width) * dims().0).max(2.5)),
                                    "vector-effect": "non-scaling-stroke",
                                    // Never a pointer target: a mousedown
                                    // over an existing stroke must report
                                    // overlay-relative coordinates, not
                                    // polyline-relative ones.
                                    pointer_events: "none",
                                }
                            }
                        }
                    }
                    // The compared version, side by side (its own
                    // independent transport — a compare is two eyes on
                    // two cuts, not one synced clock).
                    if compare().is_some() {
                        div { class: "flex flex-col gap-1",
                            {match &*compare_sources.read_unchecked() {
                                Some(Some(Ok(cmp))) => rsx! {
                                    video {
                                        src: cmp.proxy.clone(),
                                        controls: true,
                                        preload: "metadata",
                                        class: "block w-full max-h-96 rounded bg-black/80",
                                    }
                                },
                                Some(Some(Err(e))) => rsx! {
                                    div { class: "text-xs text-muted-foreground", "No proxy for that version: {e}" }
                                },
                                _ => rsx! {
                                    Text { variant: TextVariant::Muted, class: "text-xs", "Resolving version…" }
                                },
                            }}
                            div { class: "flex items-center gap-2 text-xs",
                                Badge { variant: BadgeVariant::Secondary, "comparing" }
                                button {
                                    class: "rounded px-1.5 py-0.5 text-muted-foreground hover:bg-muted/50",
                                    onclick: move |_| compare.set(None),
                                    "close compare"
                                }
                            }
                        }
                    }
                    }
                    div { class: "flex items-center gap-2 flex-wrap",
                        Badge { variant: BadgeVariant::Outline, "{display_timecode(now())}" }
                        Input {
                            value: timecode_input,
                            size: InputSize::Small,
                            placeholder: "Seek to… (m:ss)".to_string(),
                        }
                        Button {
                            variant: ButtonVariant::Secondary,
                            size: ButtonSize::Small,
                            on_click: on_seek,
                            "Go"
                        }
                        Button {
                            variant: if draw_mode() { ButtonVariant::Destructive } else { ButtonVariant::Outline },
                            size: ButtonSize::Small,
                            on_click: toggle_draw,
                            if draw_mode() { "Cancel drawing" } else { "✏ Draw" }
                        }
                        if !pending().is_empty() {
                            Button {
                                variant: ButtonVariant::Ghost,
                                size: ButtonSize::Small,
                                on_click: move |_| pending.set(Vec::new()),
                                "Clear strokes"
                            }
                        }
                        if !viewing().is_empty() && !draw_mode() {
                            Button {
                                variant: ButtonVariant::Ghost,
                                size: ButtonSize::Small,
                                on_click: {
                                    let mut viewing = viewing;
                                    move |_| viewing.set(Vec::new())
                                },
                                "Hide drawing"
                            }
                        }
                    }
                    if let Some(strip) = src.filmstrip.clone() {
                        // The whole strip is one image, left-to-right over
                        // the file's duration — a click maps x to time.
                        img {
                            id: strip_id.clone(),
                            src: strip,
                            alt: "Filmstrip — click to scrub",
                            class: "w-full cursor-pointer rounded border border-border/30",
                            onclick: {
                                let (video_id, strip_id) = (video_id.clone(), strip_id.clone());
                                move |evt: Event<MouseData>| {
                                    scrub_to(&video_id, &strip_id, evt.data().element_coordinates().x);
                                }
                            },
                        }
                    }
                },
            }}
            // Outside the sources match on purpose: a proxy that fails
            // to resolve must not hide the conversation (the comments
            // are vault data, not renditions).
            CommentsPanel {
                org,
                root_id,
                path,
                video_id: video_id.clone(),
                now,
                pending,
                viewing,
                draw_mode,
                watching,
                head: head_commit,
            }
        }
    }
}

/// One version in the switcher row: `v<n>`, its Named Version stars,
/// watch-on-click, and the ⇆ compare toggle (issue #270 AC 4).
#[component]
fn VersionChip(
    number: usize,
    entry: files_proto::ChainEntry,
    watched: bool,
    compared: bool,
    on_watch: EventHandler<()>,
    on_compare: EventHandler<()>,
) -> Element {
    rsx! {
        div { class: if watched { "flex shrink-0 items-center gap-1 rounded-md border border-ring bg-muted/40 px-1.5 py-0.5" } else { "flex shrink-0 items-center gap-1 rounded-md border border-border/40 px-1.5 py-0.5 hover:bg-muted/20" },
            button {
                class: "flex items-center gap-1",
                title: "{entry.commit_id}",
                onclick: move |_| on_watch.call(()),
                span { class: "font-medium", "v{number}" }
                // A curated Named Version is the thing a client compares
                // — its stars ride the chip.
                for nm in entry.names.iter() {
                    span { class: "text-muted-foreground", "★ {nm}" }
                }
            }
            button {
                class: if compared { "rounded bg-primary/20 px-1" } else { "rounded px-1 text-muted-foreground hover:bg-muted/50" },
                title: "Compare side by side",
                onclick: move |_| on_compare.call(()),
                "⇆"
            }
        }
    }
}

/// The timecoded comment thread (issue #270 Phase B): every comment
/// pins a timecode (click → seek) and records the file version it was
/// made on; a comment from an older version wears a badge instead of
/// silently blending in (AC 2's visible half).
#[component]
fn CommentsPanel(
    org: ReadSignal<String>,
    root_id: ReadSignal<Uuid>,
    path: ReadSignal<String>,
    /// The player element the timecode chips seek.
    video_id: String,
    /// The player's clock — what "Comment @ now" stamps.
    now: ReadSignal<f64>,
    /// Strokes authored in the player's draw mode — attached to the
    /// next posted comment, then cleared (issue #270 Phase C).
    pending: Signal<Vec<AnnotationStroke>>,
    /// The player's view overlay — a row's saved drawing lands here.
    viewing: Signal<Vec<AnnotationStroke>>,
    /// Cleared when a drawing-carrying comment posts.
    draw_mode: Signal<bool>,
    /// The version the player is showing — what a fresh comment
    /// records (a reviewer pinned to v2 is commenting on v2, AC 2).
    watching: ReadSignal<Option<String>>,
    /// The chain head — what an older comment's badge contrasts with.
    head: ReadSignal<Option<String>>,
) -> Element {
    let toast = use_toast();
    // A pure read on mount: browsing a video must never write a review
    // page — the entity is minted by the first posted comment.
    let mut scope = use_resource(move || {
        let (org, root_id, path) = (org(), root_id(), path());
        async move { find_review(&org, root_id, &path).await }
    });
    let review_id = use_memo(move || match &*scope.read() {
        Some(Ok(Some(review))) => Some(review.id),
        _ => None,
    });
    let mut comments = use_resource(move || {
        let org = org();
        let review_id = review_id();
        async move {
            match review_id {
                Some(id) => Some(fetch_comments(&org, id).await),
                None => None,
            }
        }
    });

    // Live: a comment landing anywhere (another reviewer, another
    // device) re-reads this thread.
    architect::use_stream(
        move |tx| {
            let org = org();
            async move {
                let Ok(stream) =
                    task_ui_core::vox_clients::establish_for::<FilesServiceStreamClient>(&org)
                        .await
                else {
                    return false;
                };
                stream.events(tx).await.is_ok()
            }
        },
        move |event: FilesEvent| {
            let mut comments = comments;
            let touched = match &event {
                FilesEvent::ReviewCommentAdded(c) | FilesEvent::ReviewCommentDeleted(c) => {
                    Some(c.review_id)
                }
                _ => None,
            };
            if touched.is_some() && touched == *review_id.peek() {
                comments.restart();
            }
        },
    );

    let author = use_signal(String::new);
    let body = use_signal(String::new);
    let mut busy = use_signal(|| false);

    let post = move |_| {
        // The recorded version is the one on screen; without a chain
        // yet there is nothing to attribute to.
        let Some(commit_id) = watching.peek().clone() else {
            return;
        };
        let text = body.peek().trim().to_string();
        let strokes = pending.peek().clone();
        // A drawing alone is a valid comment (the server allows text OR
        // strokes); an empty everything is nothing to post.
        if (text.is_empty() && strokes.is_empty()) || *busy.peek() {
            return;
        }
        busy.set(true);
        let org = org.peek().clone();
        let (root_id_now, path_now) = (*root_id.peek(), path.peek().clone());
        let existing = *review_id.peek();
        let comment = NewReviewComment {
            timecode_secs: *now.peek(),
            author: author.peek().trim().to_string(),
            body: text,
            commit_id,
            annotation: strokes,
        };
        let (mut body, mut pending, mut draw_mode) = (body, pending, draw_mode);
        spawn(async move {
            // First comment on this file: mint the review now (the one
            // write-on-demand), then post into it.
            let target = match existing {
                Some(id) => Ok(id),
                None => ensure_review(&org, root_id_now, &path_now)
                    .await
                    .map(|r| r.id),
            };
            let posted = match target {
                Ok(id) => post_comment(&org, id, comment).await.map(|_| ()),
                Err(e) => Err(e),
            };
            match posted {
                Ok(()) => {
                    body.set(String::new());
                    pending.set(Vec::new());
                    draw_mode.set(false);
                    if existing.is_none() {
                        scope.restart();
                    }
                    comments.restart();
                }
                Err(e) => {
                    toast.error(
                        "Couldn't comment".into(),
                        ToastOptions::new().description(e),
                    );
                }
            }
            busy.set(false);
        });
    };

    rsx! {
        div { class: "flex flex-col gap-2 pt-1",
            {match &*scope.read_unchecked() {
                None => rsx! {
                    Text { variant: TextVariant::Muted, class: "text-xs", "Checking for a review…" }
                },
                Some(Err(e)) => rsx! {
                    div { class: "text-xs text-muted-foreground", "No review: {e}" }
                },
                Some(Ok(found)) => {
                    let head = head().unwrap_or_default();
                    rsx! {
                        if let Some(review) = found {
                            div { class: "flex items-center gap-2",
                                Badge { variant: BadgeVariant::Secondary, "Review" }
                                Text { variant: TextVariant::Muted, class: "text-xs", "{review.title}" }
                            }
                            {match &*comments.read_unchecked() {
                                Some(Some(Ok(list))) if list.is_empty() => rsx! {
                                    Text { variant: TextVariant::Muted, class: "text-xs",
                                        "No comments yet — be the first."
                                    }
                                },
                                Some(Some(Ok(list))) => rsx! {
                                    div { class: "flex flex-col gap-1",
                                        for comment in list.iter().cloned() {
                                            CommentRow {
                                                key: "{comment.id}",
                                                org,
                                                comment,
                                                head_commit: head.clone(),
                                                video_id: video_id.clone(),
                                                on_removed: move |()| comments.restart(),
                                                on_view: {
                                                    let mut viewing = viewing;
                                                    move |strokes| viewing.set(strokes)
                                                },
                                            }
                                        }
                                    }
                                },
                                Some(Some(Err(e))) => rsx! {
                                    div { class: "text-xs text-destructive", "Couldn't read comments: {e}" }
                                },
                                _ => rsx! {
                                    Text { variant: TextVariant::Muted, class: "text-xs", "Reading comments…" }
                                },
                            }}
                        } else {
                            // No entity yet, on purpose: browsing is a
                            // read; the first comment mints the review.
                            Text { variant: TextVariant::Muted, class: "text-xs",
                                "No review yet — the first comment starts one."
                            }
                        }
                        div { class: "flex items-center gap-2",
                            Input {
                                value: author,
                                size: InputSize::Small,
                                placeholder: "Name".to_string(),
                                class: "max-w-28",
                            }
                            Input {
                                value: body,
                                size: InputSize::Small,
                                placeholder: "Add a comment…".to_string(),
                            }
                            Button {
                                variant: ButtonVariant::Secondary,
                                size: ButtonSize::Small,
                                // No chain head yet = nothing to attribute
                                // a comment to; disabled beats a silent
                                // no-op click.
                                disabled: busy() || watching().is_none(),
                                on_click: post,
                                if pending().is_empty() {
                                    "Comment @ {display_timecode(now())}"
                                } else {
                                    "Comment @ {display_timecode(now())} ✏"
                                }
                            }
                        }
                    }
                },
            }}
        }
    }
}

/// One comment: its timecode chip (click → seek), author, text, and —
/// when it was made on a version older than the current head — the
/// version badge that keeps old feedback honestly attributed.
#[component]
fn CommentRow(
    org: ReadSignal<String>,
    comment: ReviewComment,
    head_commit: String,
    video_id: String,
    on_removed: EventHandler<()>,
    /// Show this comment's drawing over the frame (empty = clear the
    /// overlay — clicking an undrawn comment hides a stale drawing).
    on_view: EventHandler<Vec<AnnotationStroke>>,
) -> Element {
    let toast = use_toast();
    let mut busy = use_signal(|| false);
    let tc = comment.timecode_secs;
    // Only badge against a KNOWN head — an unloaded chain must not
    // flag every comment as old.
    let stale = !head_commit.is_empty() && comment.commit_id != head_commit;
    let author = if comment.author.is_empty() {
        "Guest".to_string()
    } else {
        comment.author.clone()
    };
    let delete = {
        let id = comment.id;
        move |_| {
            if *busy.peek() {
                return;
            }
            busy.set(true);
            let org = org.peek().clone();
            spawn(async move {
                match remove_comment(&org, id).await {
                    Ok(()) => on_removed.call(()),
                    Err(e) => {
                        toast.error("Couldn't delete".into(), ToastOptions::new().description(e));
                    }
                }
                busy.set(false);
            });
        }
    };
    rsx! {
        div { class: "flex items-start gap-2 rounded-md border border-border/30 px-2 py-1.5 text-xs",
            button {
                class: "shrink-0 rounded bg-muted/40 px-1.5 py-0.5 font-mono tabular-nums hover:bg-muted/70",
                onclick: {
                    let video_id = video_id.clone();
                    let strokes = comment.annotation.clone();
                    // Jump to the moment AND show (or clear) its drawing
                    // — the overlay always reflects the comment in focus.
                    move |_| {
                        seek_to(&video_id, tc);
                        on_view.call(strokes.clone());
                    }
                },
                "{display_timecode(tc)}"
            }
            div { class: "flex flex-col gap-0.5 min-w-0",
                div { class: "flex items-center gap-2 flex-wrap",
                    span { class: "font-medium", "{author}" }
                    if !comment.via_link.is_empty() {
                        // Guest-lane feedback carries the link it came
                        // through (issue #272 AC 1).
                        Badge { variant: BadgeVariant::Outline, "via {comment.via_link}" }
                    }
                    if !comment.annotation.is_empty() {
                        Badge { variant: BadgeVariant::Secondary, "✏ drawing" }
                    }
                    if stale {
                        // Feedback about an older cut stays attributed to
                        // it (AC 2) — never silently re-anchored.
                        Badge { variant: BadgeVariant::Outline,
                            "on version {crate::short_id(&comment.commit_id)}"
                        }
                    }
                }
                span { class: "whitespace-pre-wrap break-words", "{comment.body}" }
            }
            div { class: "ml-auto",
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    disabled: busy(),
                    on_click: delete,
                    "✕"
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn video_paths_are_recognized_by_extension() {
        assert!(is_video_path("cut.mov"));
        assert!(is_video_path("takes/Cut Final.MP4"));
        assert!(!is_video_path("mix.wav"));
        assert!(!is_video_path("notes.txt"));
        assert!(!is_video_path("no-extension"));
    }

    #[test]
    fn timecodes_parse_in_all_three_shapes() {
        assert_eq!(parse_timecode("90"), Some(90.0));
        assert_eq!(parse_timecode("1:30"), Some(90.0));
        assert_eq!(parse_timecode("1:00:05"), Some(3605.0));
        assert_eq!(parse_timecode("2.5"), Some(2.5));
        assert_eq!(parse_timecode(""), None);
        assert_eq!(parse_timecode("1:2:3:4"), None);
        assert_eq!(parse_timecode("abc"), None);
        assert_eq!(parse_timecode("-5"), None);
    }

    #[test]
    fn timecodes_render_readably() {
        assert_eq!(display_timecode(0.0), "0:00");
        assert_eq!(display_timecode(90.4), "1:30");
        assert_eq!(display_timecode(3605.0), "1:00:05");
        assert_eq!(display_timecode(f64::NAN), "0:00");
        assert_eq!(display_timecode(-3.0), "0:00");
    }

    #[test]
    fn pointer_positions_normalize_and_clamp_into_the_frame() {
        let dims = (800.0, 450.0);
        let p = normalized_point(400.0, 225.0, dims).expect("center");
        assert!((p.x - 0.5).abs() < 1e-6 && (p.y - 0.5).abs() < 1e-6);
        // Outside the box clamps to the edge rather than escaping 0..=1.
        let p = normalized_point(-10.0, 9000.0, dims).expect("clamped");
        assert_eq!((p.x, p.y), (0.0, 1.0));
        // An unmeasured overlay captures nothing (no divide-by-zero).
        assert!(normalized_point(10.0, 10.0, (0.0, 0.0)).is_none());
    }

    #[test]
    fn strokes_render_as_normalized_polylines() {
        let stroke = AnnotationStroke {
            points: vec![
                AnnotationPoint { x: 0.1, y: 0.2 },
                AnnotationPoint { x: 0.5, y: 0.75 },
            ],
            color: "#ff3355".into(),
            width: 0.004,
        };
        assert_eq!(points_attr(&stroke), "0.1,0.2 0.5,0.75");
    }

    #[test]
    fn rendition_urls_target_the_streaming_route() {
        let root = Uuid::nil();
        assert_eq!(
            rendition_url("acme", root, RenditionKind::Proxy720, "abc123", "?token=t"),
            format!("/org/acme/files/renditions/{root}/proxy-720/abc123?token=t")
        );
        assert_eq!(
            rendition_url("acme", root, RenditionKind::Filmstrip, "abc123", ""),
            format!("/org/acme/files/renditions/{root}/filmstrip/abc123")
        );
    }
}
