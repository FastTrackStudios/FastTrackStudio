//! FastTrackStudio Desktop
//!
//! Currently mirrors the Session desktop app: connects to REAPER, runs a
//! WebSocket gateway for web clients, and renders the session UI shell.

use dioxus::desktop::{tao::window::WindowBuilder, Config};
use dioxus::prelude::*;

use session_ui::{ConnectionState, Session, SessionShell};

mod daw_status;
mod gateway;
mod services;
mod tools;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Tab {
    Session,
    Daw,
    Tools,
}

fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env().unwrap_or_else(|_| {
                tracing_subscriber::EnvFilter::new("info,vox_core=warn,schema_deser=off")
            }),
        )
        .init();

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

    // Connect to REAPER in the background — retries until found
    let _reaper = use_future(move || async move {
        loop {
            connection_state.set(ConnectionState::Connecting);
            match services::connect_to_reaper().await {
                Ok(()) => {
                    connection_state.set(ConnectionState::Connected);
                    tracing::info!("Connected to REAPER");
                    return;
                }
                Err(e) => {
                    tracing::warn!("Waiting for REAPER: {e}");
                    connection_state.set(ConnectionState::Disconnected);
                    tokio::time::sleep(std::time::Duration::from_secs(3)).await;
                }
            }
        }
    });

    // Subscribe to setlist events once connected
    let _subscription = use_future(move || async move {
        loop {
            if connection_state() == ConnectionState::Connected {
                break;
            }
            tokio::time::sleep(std::time::Duration::from_millis(50)).await;
        }

        let session = Session::get();

        loop {
            if let Err(e) = session.setlist().build_from_open_projects().await {
                tracing::warn!("build_from_open_projects failed: {e:?}");
            }

            let (tx, mut rx) = vox::channel::<session::SetlistEvent>();

            if let Err(e) = session.setlist().subscribe(tx).await {
                tracing::error!("Failed to subscribe to setlist events: {e:?}");
                tokio::time::sleep(std::time::Duration::from_secs(2)).await;
                continue;
            }

            tracing::info!("Subscribed to setlist events");

            let poll_session = session.clone();
            let poll_handle = tokio::spawn(async move {
                loop {
                    tokio::time::sleep(std::time::Duration::from_secs(5)).await;
                    if let Err(e) = poll_session.setlist().build_from_open_projects().await {
                        tracing::debug!("Periodic project scan failed: {e:?}");
                    }
                }
            });

            let web_registry = gateway::web_client_registry();

            while let Ok(Some(event_ref)) = rx.recv().await {
                let event = event_ref.get();
                web_registry.broadcast(event).await;
                session_ui::apply_setlist_event(event);
            }

            poll_handle.abort();
            tracing::info!("Setlist event subscription ended, will retry...");
            tokio::time::sleep(std::time::Duration::from_secs(2)).await;
        }
    });

    // Subscribe to the DAW EventBus once connected — drives the live DAW
    // status panel (transport / counts / events) from the same fts-extensions
    // connection. Reuses the caller published by connect_to_reaper.
    let _daw_subscription = use_future(move || async move {
        loop {
            if connection_state() == ConnectionState::Connected {
                break;
            }
            tokio::time::sleep(std::time::Duration::from_millis(50)).await;
        }
        loop {
            let caller = gateway::remote_conn()
                .lock()
                .expect("remote conn poisoned")
                .clone();
            if let Some(caller) = caller {
                daw_status::run_daw_event_bus(caller).await;
            }
            // Stream ended or not connected yet — retry.
            tokio::time::sleep(std::time::Duration::from_secs(2)).await;
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
