//! Signal Web — the guitar rig UI in the browser, as a pure remote.
//!
//! Connects to a running `signal-rigd` over a vox WebSocket and mounts the
//! same [`GuitarRigRemote`] components the desktop shell uses. The audio
//! engine never leaves the rigd process — only the UI is wasm.
//!
//! Run:
//!   cargo run -p signal-rigd                # the headless core (native)
//!   cd apps/web && dx serve --platform web  # this UI

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

/// Where the rig core lives. Same host as the page by default; override at
/// build time with `RIGD_URL` if the core runs elsewhere.
fn server_url() -> String {
    if let Some(url) = option_env!("RIGD_URL") {
        return url.to_string();
    }
    let host = web_sys::window()
        .and_then(|w| w.location().hostname().ok())
        .filter(|h| !h.is_empty())
        .unwrap_or_else(|| "127.0.0.1".to_string());
    format!("ws://{host}:4040/vox")
}

/// Establish one typed client over its own WebSocket (a vox caller is
/// service-bound once constructed, so sibling services don't share one).
async fn establish<C: vox_core::FromVoxLane>(url: &str) -> Option<C> {
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

fn main() {
    dioxus::launch(App);
}

/// One connect attempt for all three clients.
async fn connect_once(url: &str) -> Option<(RigClient, RigStreamClient, AudioSettingsClient)> {
    let rig: RigClient = establish(url).await?;
    let stream: RigStreamClient = establish(url).await?;
    let settings: AudioSettingsClient = establish(url).await?;
    Some((rig, stream, settings))
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

    let clients = use_resource(move || {
        let generation = generation();
        async move {
            let url = server_url();
            loop {
                if let Some(c) = connect_once(&url).await {
                    attempts.set(0);
                    return (generation, c);
                }
                attempts += 1;
                architect::platform::sleep(std::time::Duration::from_millis(1200)).await;
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
                rsx! { GuitarRigRemote { key: "{generation}" } }
            }
            None => rsx! {
                div { class: "flex flex-col items-center justify-center gap-3 h-full",
                    span { class: "w-3 h-3 rounded-full animate-pulse",
                        style: if lost() { "background-color: #ef4444;" } else { "background-color: #22c55e;" } }
                    span { class: "text-sm font-semibold", style: "color: #e4e4e7;",
                        if lost() { "Engine down — reconnecting…" } else { "Looking for the rig core…" }
                    }
                    span { class: "text-xs font-mono", style: "color: #71717a;",
                        "{server_url()}"
                    }
                    if attempts() > 0 {
                        span { class: "text-xs", style: "color: #71717a;",
                            "Retrying — start signal-rigd and this page will connect on its own."
                        }
                    }
                }
            },
        }
    }
}
