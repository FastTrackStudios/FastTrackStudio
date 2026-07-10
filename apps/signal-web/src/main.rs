//! Signal Web — the guitar rig UI in the browser, as a pure remote.
//!
//! Connects to a running `signal-engine` over a vox WebSocket and mounts the
//! same [`GuitarRigRemote`] components the desktop shell uses. The audio
//! engine never leaves the engine process — only the UI is wasm.
//!
//! The engine serves this bundle itself (http://<host>:4040/), so by default
//! the remote talks to the same origin it was loaded from. A connect screen
//! lets the user point it elsewhere (ws url) or at an iroh endpoint id
//! ("rig key"); choices persist in localStorage.
//!
//! Run:
//!   cargo run -p signal-engine                     # the headless core (native)
//!   cd apps/signal-web && dx serve --platform web  # this UI (dev loop)

#[cfg(target_arch = "wasm32")]
mod session_client;

use architect::iroh_link::iroh;
use dioxus::prelude::*;

use signal_guitar_proto::audio::AudioSettingsClient;
use signal_guitar_proto::rig::{RigClient, RigStreamClient};
use signal_guitar_ui::GuitarRigRemote;

/// Compiled Tailwind (same sheet the desktop embeds), inlined so no asset
/// pipeline is needed.
const TAILWIND: &str = include_str!("../assets/tailwind.css");

/// Viewport reset — the app fills the window like the desktop shell.
const BASE_CSS: &str = r#"
html, body, #main {
    height: 100%;
    margin: 0;
    padding: 0;
    overflow: hidden;
    background: oklch(14.5% 0 0);
}
/* Native form controls follow the dark theme. */
:root { color-scheme: dark; }
select, input {
    accent-color: #8fa8c8;
    outline: none;
}
select:focus, input:focus {
    border-color: #52525b !important;
}
/* Slim, theme-colored scrollbars everywhere. */
* {
    scrollbar-width: thin;
    scrollbar-color: #3f3f46 transparent;
}
*::-webkit-scrollbar {
    width: 6px;
    height: 6px;
}
*::-webkit-scrollbar-track {
    background: transparent;
}
*::-webkit-scrollbar-thumb {
    background: #3f3f46;
    border-radius: 3px;
}
*::-webkit-scrollbar-thumb:hover {
    background: #52525b;
}
"#;

// ── Connection target ────────────────────────────────────────────────────

/// localStorage key: user-saved ws url override.
const LS_WS_URL: &str = "fts.engine.ws-url";
/// localStorage key: user-saved iroh endpoint id ("rig key").
const LS_IROH_ID: &str = "fts.engine.iroh-id";

fn local_storage() -> Option<web_sys::Storage> {
    web_sys::window()?.local_storage().ok().flatten()
}

/// Read a localStorage key, treating blank/whitespace as unset.
fn ls_get(key: &str) -> Option<String> {
    local_storage()?
        .get_item(key)
        .ok()
        .flatten()
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
}

/// Write a localStorage key; a blank value removes it.
fn ls_set(key: &str, value: &str) {
    let Some(storage) = local_storage() else {
        return;
    };
    let v = value.trim();
    let _ = if v.is_empty() {
        storage.remove_item(key)
    } else {
        storage.set_item(key, v)
    };
}

/// Where the rig core lives when nothing is saved: same origin as the page —
/// the engine serves this bundle itself, so `ws(s)://<location.host>/vox` is
/// the engine. Exceptions:
/// - a compile-time `SIGNAL_ENGINE_URL` (or legacy `RIGD_URL`) still wins;
/// - a localhost dx dev server (`dx serve`, default port 8080) is not the
///   engine — fall back to the engine's default port.
fn same_origin_url() -> String {
    if let Some(url) = option_env!("SIGNAL_ENGINE_URL").or(option_env!("RIGD_URL")) {
        return url.to_string();
    }
    let loc = web_sys::window().map(|w| w.location());
    let hostname = loc
        .as_ref()
        .and_then(|l| l.hostname().ok())
        .unwrap_or_default();
    if hostname.is_empty() {
        return "ws://127.0.0.1:4040/vox".to_string();
    }
    let port = loc.as_ref().and_then(|l| l.port().ok()).unwrap_or_default();
    let localhost = hostname == "localhost" || hostname == "127.0.0.1" || hostname == "[::1]";
    if localhost && port == "8080" {
        // `dx serve` on its own port — the engine isn't this origin.
        return "ws://127.0.0.1:4040/vox".to_string();
    }
    let https = loc
        .as_ref()
        .and_then(|l| l.protocol().ok())
        .as_deref()
        == Some("https:");
    let host = loc.as_ref().and_then(|l| l.host().ok()).unwrap_or(hostname);
    let scheme = if https { "wss" } else { "ws" };
    format!("{scheme}://{host}/vox")
}

/// How to reach the engine. Precedence: saved iroh id > saved ws url >
/// same-origin default.
#[derive(Clone, PartialEq, Debug)]
enum Target {
    Ws(String),
    Iroh(String),
}

impl Target {
    /// Resolve the current target from localStorage + location.
    fn effective() -> Target {
        if let Some(id) = ls_get(LS_IROH_ID) {
            return Target::Iroh(id);
        }
        if let Some(url) = ls_get(LS_WS_URL) {
            return Target::Ws(url);
        }
        Target::Ws(same_origin_url())
    }

    fn label(&self) -> String {
        match self {
            Target::Ws(url) => url.clone(),
            Target::Iroh(id) => format!("iroh:{id}"),
        }
    }
}

// ── Connect flow ─────────────────────────────────────────────────────────

type Clients = (RigClient, RigStreamClient, AudioSettingsClient);

/// Establish one typed client over its own WebSocket. Per-transport
/// dial+establish (rather than a generic link parameter) so the link
/// type stays inferred — WsLink is generic on native and the vox
/// builder's Send bounds differ per platform.
async fn establish_ws<C: vox_core::FromVoxLane>(url: &str) -> Option<C> {
    let link = vox_websocket::WsLink::connect(url)
        .await
        .map_err(|e| tracing::error!("ws connect {url}: {e:?}"))
        .ok()?;
    vox_core::initiator_on(link)
        .establish::<C>()
        .await
        .map_err(|e| tracing::error!("vox handshake: {e:?}"))
        .ok()
}

/// One WebSocket connect attempt for all three clients — one link per typed
/// client (a vox caller is service-bound once constructed, so sibling
/// services don't share one).
async fn connect_ws(url: &str) -> Option<Clients> {
    let rig: RigClient = establish_ws(url).await?;
    let stream: RigStreamClient = establish_ws(url).await?;
    let settings: AudioSettingsClient = establish_ws(url).await?;
    Some((rig, stream, settings))
}

/// localStorage key: this browser's iroh device key (hex) — the same key
/// the fasttrackstudio web build uses, so one origin = one device
/// identity.
const LS_DEVICE_KEY: &str = "fts.iroh-key";

/// This browser's iroh endpoint — one per page, device key persisted in
/// localStorage. Browsers dial relay-only (no UDP in the sandbox).
async fn app_endpoint() -> Option<iroh::Endpoint> {
    static CELL: std::sync::OnceLock<iroh::Endpoint> = std::sync::OnceLock::new();
    if let Some(ep) = CELL.get() {
        return Some(ep.clone());
    }
    let key = match ls_get(LS_DEVICE_KEY)
        .and_then(|hex| architect::iroh_link::secret_key_from_hex(&hex).ok())
    {
        Some(key) => key,
        None => {
            let key = iroh::SecretKey::generate();
            ls_set(LS_DEVICE_KEY, &architect::iroh_link::secret_key_to_hex(&key));
            key
        }
    };
    let ep = architect::iroh_link::bind_endpoint(key)
        .await
        .map_err(|e| tracing::error!("iroh bind: {e}"))
        .ok()?;
    Some(CELL.get_or_init(|| ep).clone())
}

/// Establish one typed client over its own iroh bi-stream.
async fn establish_iroh<C: vox_core::FromVoxLane>(
    ep: &iroh::Endpoint,
    id: iroh::EndpointId,
) -> Option<C> {
    let link = architect::iroh_link::connect(ep, id)
        .await
        .map_err(|e| tracing::error!("iroh connect {id}: {e:?}"))
        .ok()?;
    vox_core::initiator_on(link)
        .establish::<C>()
        .await
        .map_err(|e| tracing::error!("vox handshake (iroh): {e:?}"))
        .ok()
}

/// One connect attempt over iroh, by bare endpoint id ("rig key") — one
/// bi-stream link per typed client, same shape as [`connect_ws`].
async fn connect_iroh(id: &str) -> Option<Clients> {
    let id: iroh::EndpointId = id
        .trim()
        .parse()
        .map_err(|e| tracing::error!("bad rig key: {e}"))
        .ok()?;
    let ep = app_endpoint().await?;
    let rig: RigClient = establish_iroh(&ep, id).await?;
    let stream: RigStreamClient = establish_iroh(&ep, id).await?;
    let settings: AudioSettingsClient = establish_iroh(&ep, id).await?;
    Some((rig, stream, settings))
}

/// One connect attempt against the current target.
async fn connect_once(target: &Target) -> Option<Clients> {
    match target {
        Target::Ws(url) => connect_ws(url).await,
        Target::Iroh(id) => connect_iroh(id).await,
    }
}

fn main() {
    dioxus::launch(App);
}

#[component]
fn App() -> Element {
    // Connection lifecycle: retry until the core answers, watchdog-ping
    // while connected, and on engine death tear the UI down, reconnect,
    // and remount fresh (new subscriptions, reseeded state) — no manual
    // reload, ever.
    let mut attempts = use_signal(|| 0u32);
    let mut generation = use_signal(|| 0u32);
    // The engine was up and went away (vs never seen) — changes the copy.
    let mut lost = use_signal(|| false);
    // Where to connect (saved iroh id > saved ws url > same-origin default);
    // the settings panel rewrites this after persisting to localStorage.
    let target = use_signal(Target::effective);
    // Settings overlay while connected (gear button, top-right).
    let mut show_settings = use_signal(|| false);

    let clients = use_resource(move || {
        let generation = generation();
        let target = target();
        async move {
            loop {
                if let Some(c) = connect_once(&target).await {
                    attempts.set(0);
                    return (generation, c);
                }
                attempts += 1;
                // Relay dials are slower than a LAN ws — back off harder.
                let wait = match &target {
                    Target::Ws(_) => 1200,
                    Target::Iroh(_) => 5000,
                };
                architect::platform::sleep(std::time::Duration::from_millis(wait)).await;
            }
        }
    });

    // Watchdog: ping the rig every 1.5 s. Two consecutive failures =
    // engine down → bump the generation (reconnect loop + full remount).
    use_future(move || async move {
        let mut fails = 0u32;
        loop {
            architect::platform::sleep(std::time::Duration::from_millis(1500)).await;
            let current = clients.peek().as_ref().cloned();
            let Some((gen, (rig, _, _))) = current else {
                fails = 0;
                continue;
            };
            if gen != *generation.peek() {
                fails = 0;
                continue;
            }
            if rig.status().await.is_ok() {
                fails = 0;
                lost.set(false);
            } else {
                fails += 1;
                if fails >= 2 {
                    tracing::warn!("rig core lost — reconnecting");
                    fails = 0;
                    lost.set(true);
                    generation += 1; // restarts the connect resource
                }
            }
        }
    });

    // Session-engine link (Session view: songs/sections/charts) — its own
    // reconnect loop against the standalone session gateway (:3030).
    #[cfg(target_arch = "wasm32")]
    {
        let session_state = use_signal(|| session_ui::ConnectionState::Disconnected);
        use_hook(move || session_client::start(session_state));
    }

    let state = clients
        .read()
        .as_ref()
        .filter(|(gen, _)| *gen == generation())
        .map(|(_, c)| c.clone());
    rsx! {
        document::Title { "Signal · Guitar Rig" }
        document::Style { {BASE_CSS} }
        document::Style { {TAILWIND} }
        match state {
            Some((rig, stream, settings)) => {
                let _ = provide_context(rig);
                let _ = provide_context(stream);
                let _ = provide_context(settings);
                rsx! {
                    GuitarRigRemote { key: "{generation}" }
                    // Small header affordance: reopen the connect screen
                    // while connected.
                    button {
                        style: "position: fixed; top: 6px; right: 6px; z-index: 50; \
                                width: 26px; height: 26px; display: flex; align-items: center; \
                                justify-content: center; background: rgba(24, 24, 27, 0.75); \
                                border: 1px solid #3f3f46; border-radius: 6px; color: #a1a1aa; \
                                font-size: 13px; cursor: pointer; padding: 0;",
                        title: "Engine connection…",
                        onclick: move |_| show_settings.set(true),
                        "⚙"
                    }
                    if show_settings() {
                        div {
                            style: "position: fixed; inset: 0; z-index: 60; display: flex; \
                                    align-items: center; justify-content: center; \
                                    background: rgba(0, 0, 0, 0.6);",
                            onclick: move |_| show_settings.set(false),
                            div {
                                onclick: move |e| e.stop_propagation(),
                                ConnectSettings {
                                    target,
                                    generation,
                                    on_applied: move |_| show_settings.set(false),
                                }
                            }
                        }
                    }
                }
            }
            None => rsx! {
                div {
                    style: "display: flex; flex-direction: column; align-items: center; \
                            justify-content: center; gap: 14px; height: 100%; \
                            background: #0a0a0a; overflow-y: auto;",
                    div {
                        style: "display: flex; flex-direction: column; align-items: center; gap: 8px;",
                        span { class: "animate-pulse",
                            style: if lost() {
                                "width: 12px; height: 12px; border-radius: 9999px; background-color: #ef4444;"
                            } else {
                                "width: 12px; height: 12px; border-radius: 9999px; background-color: #22c55e;"
                            } }
                        span { style: "font-size: 14px; font-weight: 600; color: #e4e4e7;",
                            if lost() { "Engine down — reconnecting…" } else { "Looking for the rig core…" }
                        }
                        span { style: "font-size: 12px; font-family: monospace; color: #71717a;",
                            "{target().label()}"
                        }
                        if attempts() > 0 {
                            span { style: "font-size: 12px; color: #71717a;",
                                "Retrying — start signal-engine and this page will connect on its own."
                            }
                        }
                    }
                    ConnectSettings { target, generation }
                }
            },
        }
    }
}

/// The connect/settings card: enter a ws URL or an iroh endpoint id ("rig
/// key"); both persist to localStorage (`fts.engine.ws-url`,
/// `fts.engine.iroh-id`). A saved rig key takes precedence over a saved ws
/// url, which takes precedence over the same-origin default.
#[component]
fn ConnectSettings(
    target: Signal<Target>,
    generation: Signal<u32>,
    on_applied: Option<EventHandler<()>>,
) -> Element {
    let mut ws_url = use_signal(|| ls_get(LS_WS_URL).unwrap_or_default());
    let mut iroh_id = use_signal(|| ls_get(LS_IROH_ID).unwrap_or_default());

    let mut apply = {
        let mut target = target;
        let mut generation = generation;
        move |ws: String, iroh: String| {
            ls_set(LS_WS_URL, &ws);
            ls_set(LS_IROH_ID, &iroh);
            target.set(Target::effective());
            generation += 1; // tear down + reconnect against the new target
            if let Some(cb) = &on_applied {
                cb.call(());
            }
        }
    };

    let label_style = "font-size: 11px; font-weight: 600; letter-spacing: 0.05em; \
                       text-transform: uppercase; color: #a1a1aa;";
    let input_style = "width: 100%; box-sizing: border-box; padding: 7px 10px; \
                       font-size: 13px; font-family: monospace; color: #e4e4e7; \
                       background: #101012; border: 1px solid #3f3f46; border-radius: 6px;";

    rsx! {
        div {
            style: "display: flex; flex-direction: column; gap: 12px; \
                    width: min(420px, calc(100vw - 32px)); padding: 16px; \
                    background: #18181b; border: 1px solid #27272a; border-radius: 10px;",
            span { style: "font-size: 13px; font-weight: 600; color: #e4e4e7;",
                "Engine connection"
            }
            div { style: "display: flex; flex-direction: column; gap: 5px;",
                label { style: "{label_style}", "WebSocket URL" }
                input {
                    style: "{input_style}",
                    r#type: "text",
                    placeholder: "{same_origin_url()}",
                    value: "{ws_url}",
                    oninput: move |e| ws_url.set(e.value()),
                }
            }
            div { style: "display: flex; flex-direction: column; gap: 5px;",
                label { style: "{label_style}", "Rig key (iroh endpoint id)" }
                input {
                    style: "{input_style}",
                    r#type: "text",
                    placeholder: "paste the engine's endpoint id",
                    value: "{iroh_id}",
                    oninput: move |e| iroh_id.set(e.value()),
                }
                span { style: "font-size: 11px; color: #71717a;",
                    "Takes precedence over the URL when set. The browser dials \
                     the rig p2p over iroh relays — any network, no port \
                     forwarding."
                }
            }
            div { style: "display: flex; gap: 8px;",
                button {
                    style: "flex: 1; padding: 7px 10px; font-size: 13px; font-weight: 600; \
                            color: #0a0a0a; background: #8fa8c8; border: none; \
                            border-radius: 6px; cursor: pointer;",
                    onclick: move |_| apply(ws_url(), iroh_id()),
                    "Save & Connect"
                }
                button {
                    style: "padding: 7px 10px; font-size: 13px; color: #a1a1aa; \
                            background: transparent; border: 1px solid #3f3f46; \
                            border-radius: 6px; cursor: pointer;",
                    title: "Clear saved overrides and use the same-origin default",
                    onclick: move |_| {
                        ws_url.set(String::new());
                        iroh_id.set(String::new());
                        apply(String::new(), String::new());
                    },
                    "Use default"
                }
            }
        }
    }
}
