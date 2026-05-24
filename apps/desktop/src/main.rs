//! FastTrackStudio Desktop
//!
//! Currently mirrors the Session desktop app: connects to REAPER, runs a
//! WebSocket gateway for web clients, and renders the session UI shell.

use dioxus::desktop::{tao::window::WindowBuilder, Config};
use dioxus::prelude::*;

use session_ui::{ConnectionState, Session, SessionShell};

mod gateway;
mod services;
mod tools;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Tab {
    Session,
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
                    class: tab_class(tab() == Tab::Tools),
                    onclick: move |_| tab.set(Tab::Tools),
                    "Tools"
                }
            }
            div { class: "flex-1 min-h-0 overflow-auto",
                // SessionShell stays mounted across tab switches so its
                // connection/event state isn't torn down; Tools is shown on top.
                div { class: if tab() == Tab::Session { "h-full" } else { "hidden" },
                    SessionShell { connection_state }
                }
                if tab() == Tab::Tools {
                    tools::ToolsPage {}
                }
            }
        }
    }
}
