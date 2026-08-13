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
//! ladder — and appends it as `?token=`. A failed mint yields an empty
//! suffix, which still plays while `TASK_ENFORCE_MEDIA_TOKEN` is off
//! (the same rollout contract as the stem player's grants).
//!
//! ## Why seeks go through `document::eval`
//!
//! Dioxus media events don't expose `currentTime`, and a seek is a
//! property write on the live element — both are two-line JS against the
//! element's id, same as the shell's watch page.

use dioxus::prelude::*;
use files_proto::RenditionKind;
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

/// Render seconds as `m:ss` (or `h:mm:ss` past the hour) — the
/// player's timecode display.
pub fn format_timecode(secs: f64) -> String {
    let secs = if secs.is_finite() && secs > 0.0 {
        secs
    } else {
        0.0
    };
    let whole = secs as u64;
    let (h, m, s) = (whole / 3600, (whole / 60) % 60, whole % 60);
    if h > 0 {
        format!("{h}:{m:02}:{s:02}")
    } else {
        format!("{m}:{s:02}")
    }
}

/// Mint the `?token=` suffix for this root's rendition URLs. Empty on
/// failure, on purpose — see the module docs.
async fn grant_suffix(org: &str, root_id: Uuid) -> String {
    use media_proto::MediaServiceClient;
    let Ok(client) = task_ui_core::vox_clients::establish_for::<MediaServiceClient>(org).await
    else {
        return String::new();
    };
    match client
        .media_grant(format!("files/renditions/{root_id}"))
        .await
    {
        Ok(grant) => format!("?token={}", grant.token),
        // Not fatal: without a grant the URL still serves while
        // TASK_ENFORCE_MEDIA_TOKEN is off.
        Err(_) => String::new(),
    }
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
/// plus one grant covering both URLs.
async fn resolve_sources(org: &str, root_id: Uuid, path: &str) -> Result<Sources, String> {
    let c = crate::client(org).await?;
    let proxy = c
        .rendition(root_id, path.to_owned(), RenditionKind::Proxy720)
        .await
        .map_err(|e| e.to_string())?;
    // No filmstrip is not an error — the proxy still plays; the scrub
    // strip simply doesn't render.
    let filmstrip = c
        .rendition(root_id, path.to_owned(), RenditionKind::Filmstrip)
        .await
        .ok();
    let tok = grant_suffix(org, root_id).await;
    Ok(Sources {
        proxy: rendition_url(org, root_id, RenditionKind::Proxy720, &proxy.file_id, &tok),
        filmstrip: filmstrip
            .map(|f| rendition_url(org, root_id, RenditionKind::Filmstrip, &f.file_id, &tok)),
    })
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

/// The review player for one opened media file: proxy playback,
/// timecode display + seek, filmstrip scrub.
#[component]
pub fn ReviewPlayer(org: String, root_id: Uuid, path: String) -> Element {
    // Stable per-mount element ids — the eval seams address the live
    // elements by id, and two open files must not cross wires.
    let video_id = use_hook(|| format!("review-video-{}", Uuid::new_v4().simple()));
    let strip_id = use_hook(|| format!("review-strip-{}", Uuid::new_v4().simple()));

    let sources = {
        let (org, path) = (org.clone(), path.clone());
        use_resource(move || {
            let (org, path) = (org.clone(), path.clone());
            async move { resolve_sources(&org, root_id, &path).await }
        })
    };

    let mut now = use_signal(|| 0.0f64);
    let timecode_input = use_signal(String::new);

    let on_seek = {
        let video_id = video_id.clone();
        move |_| {
            if let Some(secs) = parse_timecode(&timecode_input.peek()) {
                seek_to(&video_id, secs);
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
                    video {
                        id: video_id.clone(),
                        src: src.proxy.clone(),
                        controls: true,
                        preload: "metadata",
                        class: "w-full max-h-96 rounded bg-black/80",
                        ontimeupdate: {
                            let video_id = video_id.clone();
                            move |_| {
                                let video_id = video_id.clone();
                                spawn(async move { now.set(read_time(&video_id).await) });
                            }
                        },
                    }
                    div { class: "flex items-center gap-2",
                        Badge { variant: BadgeVariant::Outline, "{format_timecode(now())}" }
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
        assert_eq!(format_timecode(0.0), "0:00");
        assert_eq!(format_timecode(90.4), "1:30");
        assert_eq!(format_timecode(3605.0), "1:00:05");
        assert_eq!(format_timecode(f64::NAN), "0:00");
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
