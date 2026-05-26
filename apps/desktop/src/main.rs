//! FastTrackStudio Desktop
//!
//! Currently mirrors the Session desktop app: connects to REAPER, runs a
//! WebSocket gateway for web clients, and renders the session UI shell.

use dioxus::desktop::{tao::window::WindowBuilder, Config};
use dioxus::prelude::*;

use session_ui::{ConnectionState, SessionShell};

use fasttrackstudio_desktop::{daw_status, gateway, services, tools};

#[derive(Clone, Copy, PartialEq, Eq)]
enum Tab {
    Session,
    Daw,
    Tools,
}

/// Path of the desktop log file. Override with `FTS_DESKTOP_LOG`.
fn log_path() -> std::path::PathBuf {
    std::env::var_os("FTS_DESKTOP_LOG")
        .map(std::path::PathBuf::from)
        // Fixed /tmp path (not std::env::temp_dir(), which honors the
        // nix-shell $TMPDIR and lands somewhere unpredictable).
        .unwrap_or_else(|| std::path::PathBuf::from("/tmp/fasttrackstudio-desktop.log"))
}

/// Send tracing to a grep-able logfile (truncated each launch) instead of
/// stdout, and silence the cranelift JIT IR firehose. The file is written
/// unbuffered (a fresh `File` handle per line) so the tail survives an abort.
fn init_logging() {
    let default = "info,vox_core=warn,schema_deser=off,\
                   cranelift=off,cranelift_jit=off,cranelift_codegen=off,\
                   regalloc2=off,vox_jit=warn,cranelift_frontend=off";
    let filter = tracing_subscriber::EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new(default));

    let path = log_path();
    match std::fs::OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(&path)
    {
        Ok(file) => {
            tracing_subscriber::fmt()
                .with_env_filter(filter)
                .with_ansi(false)
                .with_writer(move || file.try_clone().expect("clone desktop log file"))
                .init();
            eprintln!("FastTrackStudio: logging to {}", path.display());
        }
        Err(e) => {
            eprintln!("FastTrackStudio: could not open {} ({e}); logging to stderr", path.display());
            tracing_subscriber::fmt().with_env_filter(filter).init();
        }
    }

    // Capture panic messages into the logfile too (the default hook only
    // writes to stderr). Critical for the abort-on-panic JIT path, where the
    // reason is otherwise lost from the grep-able log.
    let default_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        tracing::error!(target: "panic", "{info}");
        default_hook(info);
    }));
}

fn main() {
    init_logging();

    tracing::info!("Starting FastTrackStudio");

    let cfg = Config::new().with_window(
        WindowBuilder::new()
            .with_title("FastTrackStudio")
            .with_inner_size(dioxus::desktop::tao::dpi::LogicalSize::new(1400.0, 900.0)),
    );

    LaunchBuilder::desktop().with_cfg(cfg).launch(App);
}

#[component]
fn App() -> Element {
    rsx! { DesktopShell {} }
}

#[component]
fn DesktopShell() -> Element {
    let mut connection_state = use_signal(|| ConnectionState::Disconnected);
    let mut gateway_info: Signal<Option<gateway::GatewayInfo>> = use_signal(|| None);

    // Start the gateway immediately (serves web app even without REAPER)
    let _gateway = use_future(move || async move {
        match services::start_gateway().await {
            Ok(info) => {
                tracing::info!("Gateway started on port {}", info.port);
                gateway_info.set(Some(info));
            }
            Err(e) => {
                tracing::error!("Failed to start gateway: {e}");
            }
        }
    });

    // Connection manager: connect to REAPER and keep the connection alive,
    // reconnecting whenever the transport drops (e.g. REAPER restarts with a
    // new /tmp/fts-daw-*.sock). This is the single source of truth for
    // `connection_state`; the subscription loops below gate on it.
    let _reaper = use_future(move || async move {
        let mut backoff = std::time::Duration::from_millis(500);
        let max_backoff = std::time::Duration::from_secs(5);
        loop {
            connection_state.set(ConnectionState::Connecting);
            match services::connect_to_reaper().await {
                Ok(caller) => {
                    connection_state.set(ConnectionState::Connected);
                    tracing::info!("Connected to REAPER");
                    backoff = std::time::Duration::from_millis(500);
                    // Park until the transport closes, then reconnect. The next
                    // connect() rediscovers the newest socket, so a restarted
                    // REAPER is picked up automatically.
                    caller.closed().await;
                    tracing::warn!("REAPER connection closed — reconnecting");
                    connection_state.set(ConnectionState::Disconnected);
                }
                Err(e) => {
                    tracing::warn!("Waiting for REAPER: {e}");
                    connection_state.set(ConnectionState::Disconnected);
                    tokio::time::sleep(backoff).await;
                    backoff = (backoff * 2).min(max_backoff);
                }
            }
        }
    });

    // Subscribe to setlist events whenever connected; resubscribe on reconnect.
    // Uses a DIRECT client built from the live caller — streaming RPC can't be
    // proxied through the gateway forwarder, so this path can't go through the
    // session proxy.
    let _subscription = use_future(move || async move {
        loop {
            // Gate until the connection manager reports Connected.
            while connection_state() != ConnectionState::Connected {
                tokio::time::sleep(std::time::Duration::from_millis(100)).await;
            }
            let Some(caller) = gateway::remote_conn()
                .lock()
                .expect("remote conn poisoned")
                .clone()
            else {
                tokio::time::sleep(std::time::Duration::from_millis(100)).await;
                continue;
            };
            let setlist = session::SetlistServiceClient::new(caller);

            if let Err(e) = setlist.build_from_open_projects().await {
                tracing::warn!("build_from_open_projects failed: {e:?}");
            }

            // The subscribe handler runs its loop in-flight (never returns until
            // the stream ends), so poll the receiver CONCURRENTLY with the
            // subscribe future rather than awaiting subscribe() first.
            let (tx, mut rx) = vox::channel::<session::SetlistEvent>();
            let mut sub = std::pin::pin!(setlist.subscribe(tx));
            tracing::info!("Subscribing to setlist events");

            // Periodic rescan while subscribed — rebuilds the client each tick
            // so it always uses the live caller.
            let poll_handle = tokio::spawn(async move {
                loop {
                    tokio::time::sleep(std::time::Duration::from_secs(5)).await;
                    let caller = gateway::remote_conn()
                        .lock()
                        .expect("remote conn poisoned")
                        .clone();
                    if let Some(caller) = caller {
                        let setlist = session::SetlistServiceClient::new(caller);
                        if let Err(e) = setlist.build_from_open_projects().await {
                            tracing::debug!("Periodic project scan failed: {e:?}");
                        }
                    }
                }
            });

            let web_registry = gateway::web_client_registry();
            loop {
                tokio::select! {
                    res = &mut sub => {
                        if let Err(e) = res {
                            tracing::error!("Setlist subscribe error: {e:?}");
                        }
                        break;
                    }
                    ev = rx.recv() => {
                        match ev {
                            Ok(Some(event_ref)) => {
                                let event = event_ref.get();
                                web_registry.broadcast(event).await;
                                session_ui::apply_setlist_event(event);
                            }
                            _ => break,
                        }
                    }
                }
            }

            poll_handle.abort();
            tracing::info!("Setlist subscription ended — will resubscribe on reconnect");
            tokio::time::sleep(std::time::Duration::from_millis(200)).await;
        }
    });

    // Subscribe to the DAW EventBus whenever connected — drives the live DAW
    // status panel. Reuses the live caller published by the connection manager
    // and resubscribes on reconnect.
    let _daw_subscription = use_future(move || async move {
        loop {
            while connection_state() != ConnectionState::Connected {
                tokio::time::sleep(std::time::Duration::from_millis(100)).await;
            }
            let caller = gateway::remote_conn()
                .lock()
                .expect("remote conn poisoned")
                .clone();
            if let Some(caller) = caller {
                daw_status::run_daw_event_bus(caller).await;
            }
            // Stream ended — re-gate; the manager owns reconnect + state.
            tokio::time::sleep(std::time::Duration::from_millis(500)).await;
        }
    });

    let mut tab = use_signal(|| Tab::Session);
    let tab_class = |active: bool| {
        if active {
            "px-3 py-1.5 text-sm rounded-md bg-neutral-200 font-medium"
        } else {
            "px-3 py-1.5 text-sm rounded-md hover:bg-neutral-100 opacity-70"
        }
    };

    rsx! {
        document::Stylesheet { href: asset!("/assets/tailwind.css") }
        div { class: "flex flex-col h-screen",
            nav { class: "flex gap-1 px-3 py-2 border-b border-neutral-200 shrink-0",
                button {
                    class: tab_class(tab() == Tab::Session),
                    onclick: move |_| tab.set(Tab::Session),
                    "Session"
                }
                button {
                    class: tab_class(tab() == Tab::Daw),
                    onclick: move |_| tab.set(Tab::Daw),
                    "DAW"
                }
                button {
                    class: tab_class(tab() == Tab::Tools),
                    onclick: move |_| tab.set(Tab::Tools),
                    "Tools"
                }
            }
            div { class: "flex-1 min-h-0 overflow-auto",
                // SessionShell stays mounted across tab switches so its
                // connection/event state isn't torn down; other tabs render on top.
                div { class: if tab() == Tab::Session { "h-full" } else { "hidden" },
                    SessionShell { connection_state }
                }
                if tab() == Tab::Daw {
                    daw_status::DawStatusPanel {}
                }
                if tab() == Tab::Tools {
                    tools::ToolsPage {}
                }
            }
        }
        // Connection/gateway info popover, overlaid on top of everything.
        DesktopConnectionOverlay { connection_state, gateway_info }
    }
}

/// Desktop-only connection overlay — a click-to-open popover (top-right) that
/// reports REAPER connection state and the web gateway URLs. Ported from
/// session's own desktop app for parity.
#[component]
fn DesktopConnectionOverlay(
    connection_state: Signal<ConnectionState>,
    gateway_info: Signal<Option<gateway::GatewayInfo>>,
) -> Element {
    let mut show_popover = use_signal(|| false);

    if gateway_info.read().is_none() {
        return rsx! {};
    }

    rsx! {
        div { class: "fixed top-0 right-0 h-10 z-30 flex items-center pr-4",
            button {
                class: "w-32 h-8 cursor-pointer opacity-0",
                onclick: move |_| show_popover.toggle(),
            }
        }

        if show_popover() {
            div {
                class: "fixed inset-0 z-40",
                onclick: move |_| show_popover.set(false),
            }
            div {
                class: "fixed right-4 top-12 z-50 w-72 rounded-lg border border-neutral-300 bg-white shadow-xl p-3 text-sm",
                div { class: "font-medium mb-3", "Connection Info" }
                div { class: "flex items-center justify-between py-1.5",
                    span { class: "opacity-60", "REAPER" }
                    match connection_state() {
                        ConnectionState::Connected => rsx! { span { class: "text-green-600 font-medium", "Connected" } },
                        ConnectionState::Connecting => rsx! { span { class: "text-yellow-600 font-medium", "Connecting…" } },
                        ConnectionState::Disconnected => rsx! { span { class: "text-red-600 font-medium", "Disconnected" } },
                    }
                }
                div { class: "border-t border-neutral-200 my-2" }
                if let Some(ref info) = *gateway_info.read() {
                    div { class: "space-y-1.5",
                        div { class: "text-xs font-medium opacity-60 uppercase tracking-wider mb-1", "Web Gateway" }
                        div { class: "flex items-center justify-between py-0.5",
                            span { class: "opacity-60", "Port" }
                            span { class: "font-mono", "{info.port}" }
                        }
                        div { class: "flex items-center justify-between py-0.5",
                            span { class: "opacity-60", "Local" }
                            span { class: "font-mono text-blue-600 text-xs", "{info.local_url()}" }
                        }
                        if let Some(url) = info.network_url() {
                            div { class: "flex items-center justify-between py-0.5",
                                span { class: "opacity-60", "Network" }
                                span { class: "font-mono text-blue-600 text-xs", "{url}" }
                            }
                        }
                        div { class: "flex items-center justify-between py-0.5",
                            span { class: "opacity-60", "Web App" }
                            if info.serving_web_app {
                                span { class: "text-green-600", "Serving" }
                            } else {
                                span { class: "text-yellow-600", "Not built" }
                            }
                        }
                    }
                }
            }
        }
    }
}
