//! `/session/{org}/{collection}` — a read-only collection library browser.
//!
//! Collections live on the **task-server**, which exposes one
//! `CollectionService` per org on its per-org router at `/org/{slug}/vox`.
//! This view dials `<task-server base>/org/{org}/vox`, `list`s that org's
//! collections, and picks the one whose title slug matches the URL's
//! `{collection}` segment — then renders its title, kind badge, and the
//! ordered list of song items.
//!
//! Ported from `apps/fasttrackstudio/src/collection_browser.rs`; adapted to
//! the site's dioxus Router (route props instead of reading `location`) and
//! its Tailwind chrome (rendered inside the site `Layout`, so the navbar
//! frames it).
//!
//! Configure the task-server base with the `task-server-ws-url` pref
//! (browser localStorage key `fts.task-server-ws-url`); default
//! `ws://127.0.0.1:18080`.

use collection_proto::{Collection, CollectionItem, CollectionServiceClient};
use dioxus::prelude::*;

use crate::Route;

// ── task-server URL + establish plumbing ─────────────────────────────────────

/// Read the `fts.<key>` pref from browser localStorage. `None` off-wasm or
/// when unset/empty.
#[cfg(target_arch = "wasm32")]
fn pref(key: &str) -> Option<String> {
    let store = web_sys::window()?.local_storage().ok()??;
    let val = store.get_item(&format!("fts.{key}")).ok()??;
    let val = val.trim().to_string();
    (!val.is_empty()).then_some(val)
}

#[cfg(not(target_arch = "wasm32"))]
fn pref(_key: &str) -> Option<String> {
    None
}

/// The task-server base WebSocket URL (scheme + host, no path). Read from the
/// `task-server-ws-url` pref, default `ws://127.0.0.1:18080`. Any trailing
/// `/vox` or `/` is stripped so the per-org path composes cleanly.
fn task_server_base() -> String {
    // Explicit override wins (localStorage `fts.task-server-ws-url`).
    if let Some(raw) = pref("task-server-ws-url") {
        let t = raw.trim();
        let t = t.strip_suffix("/vox").unwrap_or(t);
        return t.trim_end_matches('/').to_string();
    }
    // Architect same-origin: the host that served this page also serves vox
    // (prod: the fasttrackstudio.app ingress; dev: dx proxies the path to
    // task-server). No second port for the browser to reach.
    #[cfg(target_arch = "wasm32")]
    if let Some(w) = web_sys::window() {
        let loc = w.location();
        if let (Ok(proto), Ok(host)) = (loc.protocol(), loc.host()) {
            let scheme = if proto == "https:" { "wss" } else { "ws" };
            if !host.is_empty() {
                return format!("{scheme}://{host}");
            }
        }
    }
    "ws://127.0.0.1:18080".to_string()
}

/// The per-org vox URL for `org` on the configured task-server.
fn org_vox_url(org: &str) -> String {
    format!("{}/org/{}/vox", task_server_base(), org)
}

/// Establish one typed vox client over a fresh WebSocket link. `None` if the
/// server is unreachable or the handshake fails. A vox caller is
/// service-bound once constructed, so each service gets its own link.
async fn establish<C: vox_core::FromVoxLane>(url: &str) -> Option<C> {
    let link = vox_websocket::WsLink::connect(url)
        .await
        .map_err(|e| tracing::debug!("ws connect {url}: {e:?}"))
        .ok()?;
    vox_core::initiator_on(link)
        .establish::<C>()
        .await
        .map_err(|e| tracing::warn!("vox handshake: {e:?}"))
        .ok()
}

// ── slug + label helpers ────────────────────────────────────────────────────

/// A URL-safe slug: lowercase, non-alphanumerics collapsed to single dashes,
/// edges trimmed. Matches a collection's human title against the
/// `{collection}` path segment case-insensitively.
fn slugify(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut prev_dash = true; // trims leading dashes
    for ch in s.chars() {
        if ch.is_ascii_alphanumeric() {
            out.extend(ch.to_lowercase());
            prev_dash = false;
        } else if !prev_dash {
            out.push('-');
            prev_dash = true;
        }
    }
    while out.ends_with('-') {
        out.pop();
    }
    out
}

/// A prettier label for a song-ish node id: `keep-on-finding-more` →
/// `Keep On Finding More`. Falls back to the node kind for an empty id.
fn pretty_label(item: &CollectionItem) -> String {
    let id = &item.node.id;
    if id.is_empty() {
        return item.node.kind.as_str().to_string();
    }
    id.split(['-', '_', ' '])
        .filter(|w| !w.is_empty())
        .map(|w| {
            let mut c = w.chars();
            match c.next() {
                Some(first) => first.to_uppercase().collect::<String>() + c.as_str(),
                None => String::new(),
            }
        })
        .collect::<Vec<_>>()
        .join(" ")
}

/// Loaded/resolved state for the requested collection.
enum Loaded {
    /// Resolved the collection for this slug.
    Found(Collection),
    /// Connected + listed, but no collection matched the slug.
    NotFound,
}

// ── component ────────────────────────────────────────────────────────────────

/// `/session/{org}/{collection}` — dial the task-server, list the org's
/// collections, resolve the one matching `collection`, and render its
/// ordered songs. Rendered inside the site `Layout` (navbar above).
#[component]
pub fn SessionCollection(org: String, collection: String) -> Element {
    // async: connect → list → resolve by slug. `Result<Loaded, String>`;
    // the `Err` string is a human message for the error panel.
    let org_r = org.clone();
    let slug_r = collection.clone();
    let data = use_resource(move || {
        let org = org_r.clone();
        let slug = slug_r.clone();
        async move {
            let url = org_vox_url(&org);
            let client = establish::<CollectionServiceClient>(&url)
                .await
                .ok_or_else(|| format!("could not reach the task-server at {url}"))?;
            let mut collections = client
                .list(org.clone(), None)
                .await
                .map_err(|e| format!("task-server error: {e}"))?;
            let want = slugify(&slug);
            match collections
                .iter()
                .position(|c| slugify(&c.title) == want || c.title.eq_ignore_ascii_case(&slug))
            {
                Some(i) => {
                    let mut col = collections.swap_remove(i);
                    col.sort_items(); // canonical ascending-by-rank order
                    Ok::<Loaded, String>(Loaded::Found(col))
                }
                None => Ok(Loaded::NotFound),
            }
        }
    });

    let body = match &*data.read_unchecked() {
        None => rsx! {
            div { class: "text-sm text-muted-foreground", "Connecting to the task-server…" }
        },
        Some(Err(msg)) => rsx! {
            div { class: "flex flex-col gap-2",
                span { class: "text-sm font-semibold text-destructive", "Could not load collection" }
                span { class: "text-sm text-muted-foreground", "{msg}" }
                span { class: "text-xs text-muted-foreground/70",
                    "Set the task-server address in localStorage (key fts.task-server-ws-url) to point elsewhere."
                }
            }
        },
        Some(Ok(Loaded::NotFound)) => rsx! {
            div { class: "flex flex-col gap-1.5",
                span { class: "text-base font-semibold text-foreground", "Collection not found" }
                span { class: "text-sm text-muted-foreground",
                    "No collection titled \"{collection}\" in org \"{org}\"."
                }
            }
        },
        Some(Ok(Loaded::Found(col))) => rsx! {
            CollectionCard { org: org.clone(), collection_slug: collection.clone(), collection: col.clone() }
        },
    };

    rsx! {
        document::Title { "{collection} — FastTrackStudio" }

        div {
            class: "mx-auto max-w-2xl px-4 py-10 flex flex-col gap-4",
            div {
                class: "text-[11px] uppercase tracking-[0.15em] text-muted-foreground/70",
                "{org}"
            }
            {body}
        }
    }
}

/// The song slug for an item's node id — `song:praise` → `praise`. This is the
/// `{song}` segment of the session-view route and the media base slug.
fn song_slug(item: &CollectionItem) -> String {
    let id = &item.node.id;
    id.strip_prefix("song:").unwrap_or(id).to_string()
}

/// The resolved collection: title, kind badge, and an ordered song list. Each
/// song links to the session view (`/session/{org}/{collection}/{song}`).
#[component]
fn CollectionCard(org: String, collection_slug: String, collection: Collection) -> Element {
    let items = collection.items.clone();
    rsx! {
        div {
            class: "flex items-baseline gap-3",
            span { class: "text-2xl font-bold text-foreground", "{collection.title}" }
            span {
                class: "text-[11px] font-semibold uppercase tracking-[0.1em] text-muted-foreground border border-border rounded-full px-2 py-0.5",
                "{collection.kind.as_str()}"
            }
        }
        if items.is_empty() {
            div { class: "text-sm text-muted-foreground mt-1.5", "No songs yet." }
        } else {
            ol {
                class: "list-none m-0 p-0 flex flex-col gap-1.5 mt-1.5",
                for (i, item) in items.iter().enumerate() {
                    Link {
                        key: "{item.node.to_token()}",
                        to: Route::SongSession {
                            org: org.clone(),
                            collection: collection_slug.clone(),
                            song: song_slug(item),
                        },
                        class: "flex items-baseline gap-3 px-3 py-2.5 border border-border rounded-lg bg-card hover:border-primary/60 hover:bg-accent/40 transition-colors",
                        span {
                            class: "text-xs text-muted-foreground/70 min-w-[22px] text-right",
                            "{i + 1}"
                        }
                        div {
                            class: "flex flex-col gap-0.5",
                            span { class: "text-sm font-medium text-foreground", "{pretty_label(item)}" }
                            span { class: "text-[11px] text-muted-foreground/60", "{item.node.to_token()}" }
                        }
                    }
                }
            }
        }
    }
}
