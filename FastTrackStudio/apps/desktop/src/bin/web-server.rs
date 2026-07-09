//! Headless FastTrackStudio web server.
//!
//! Runs the same WebSocket gateway + fts-extensions connection the desktop app
//! uses, but with no GUI — so the WASM webapp (`apps/web`) can run end-to-end
//! without the desktop. It:
//!
//! 1. serves the built webapp + its `/ws` backend on `:3030`,
//! 2. connects to REAPER's `fts-extensions` (reconnecting on drop),
//! 3. pumps the setlist subscription and broadcasts events to web clients.
//!
//! Browser RPC (seek/toggle/etc.) is forwarded to REAPER by the gateway. Build
//! the webapp first (`dx build -p fasttrackstudio-web --release`) so the
//! gateway can serve its static files.
//!
//! Run: `cargo run -p fasttrackstudio-desktop --bin fasttrackstudio-web-server`

use std::time::Duration;

use fasttrackstudio_desktop::{gateway, services};

#[tokio::main]
async fn main() -> eyre::Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("info")),
        )
        .init();

    tracing::info!("Starting FastTrackStudio web server (headless)");

    // 1. Serve the webapp + WS gateway. Browser RPC is forwarded to whatever
    //    caller is published to `remote_conn` (set by the connection manager).
    let info = services::start_gateway().await?;
    tracing::info!("Web server listening on port {}", info.port);
    tracing::info!("  Local:   {}", info.local_url());
    if let Some(url) = info.network_url() {
        tracing::info!("  Network: {url}");
    }

    // 2. Connection manager: connect to fts-extensions and reconnect on drop
    //    (rediscovers the newest socket — survives REAPER restarts).
    tokio::spawn(async move {
        let mut backoff = Duration::from_millis(500);
        let max_backoff = Duration::from_secs(5);
        loop {
            match services::connect_to_reaper().await {
                Ok(caller) => {
                    tracing::info!("Connected to fts-extensions");
                    backoff = Duration::from_millis(500);
                    caller.closed().await;
                    tracing::warn!("fts-extensions connection closed — reconnecting");
                }
                Err(e) => {
                    tracing::warn!("Waiting for fts-extensions: {e}");
                    tokio::time::sleep(backoff).await;
                    backoff = (backoff * 2).min(max_backoff);
                }
            }
        }
    });

    // 3. Setlist subscription pump → broadcast to web clients. Unlike the
    //    desktop, there is no local Dioxus UI, so we only rebroadcast (no
    //    `apply_setlist_event`).
    tokio::spawn(async move {
        loop {
            let Some(caller) = gateway::remote_conn()
                .lock()
                .expect("remote conn poisoned")
                .clone()
            else {
                tokio::time::sleep(Duration::from_millis(150)).await;
                continue;
            };
            let setlist = session::SetlistServiceClient::new(caller);

            if let Err(e) = setlist.build_from_open_projects().await {
                tracing::warn!("build_from_open_projects failed: {e:?}");
            }

            // The subscribe handler runs its event loop in-flight (it doesn't
            // return until the stream ends), so poll the receiver CONCURRENTLY
            // with the subscribe future rather than awaiting it first.
            let (tx, mut rx) = vox::channel::<session::SetlistEvent>();
            let mut sub = std::pin::pin!(setlist.subscribe(tx));
            tracing::info!("Subscribing to setlist events; rebroadcasting to web clients");

            // Periodic rescan while subscribed (rebuild against the live caller).
            let poll = tokio::spawn(async move {
                loop {
                    tokio::time::sleep(Duration::from_secs(5)).await;
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
                        match res {
                            Ok(()) => tracing::info!("Setlist subscription closed"),
                            Err(e) => tracing::error!("Setlist subscribe error: {e:?}"),
                        }
                        break;
                    }
                    ev = rx.recv() => {
                        match ev {
                            Ok(Some(event_ref)) => {
                                let event = event_ref.get();
                                if let session::SetlistEvent::SetlistChanged(sl) = event {
                                    tracing::info!(
                                        "received SetlistChanged: {} songs ({})",
                                        sl.songs.len(),
                                        sl.songs.iter().map(|s| s.name.as_str())
                                            .collect::<Vec<_>>().join(", ")
                                    );
                                    tracing::info!("FULL SETLIST STRUCT:\n{sl:#?}");
                                }
                                web_registry.broadcast(event).await;
                            }
                            _ => break,
                        }
                    }
                }
            }

            poll.abort();
            tracing::info!("Setlist subscription ended — will resubscribe");
            tokio::time::sleep(Duration::from_millis(200)).await;
        }
    });

    // Run forever.
    std::future::pending::<()>().await;
    Ok(())
}
