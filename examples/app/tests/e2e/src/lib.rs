//! App-level end-to-end test scaffold.
//!
//! Wires a real `example-server` (in-memory backend) to a real vox
//! client over a TCP-bound axum WebSocket. Lives alongside the runtime
//! binaries so adding a second feature/app doesn't fight for ownership
//! of the test harness.
//!
//! Currently a scaffold — the helpers are here, an actual test will
//! plug them together in the next pass. Kept compiling so CI catches
//! breakage in the surface this test depends on.

#![cfg(test)]

use std::sync::Arc;

use axum::{Router, routing::get};
use example::axum_ws;
use example::{ExampleRepoDispatcher, backend_memory::ExampleRepoMemory};
use tokio::sync::oneshot;

/// Build the same Router that `apps/example/server` mounts. Exposed as
/// a helper so each test can bind it to an ephemeral port and tear it
/// down cleanly.
pub fn router(repo: ExampleRepoMemory) -> Router {
    let state = Arc::new(repo);
    Router::new()
        .route("/api/health", get(|| async { "ok" }))
        .route(
            "/vox",
            get({
                |ws: axum::extract::WebSocketUpgrade,
                 axum::extract::State(state): axum::extract::State<Arc<ExampleRepoMemory>>| async move {
                    ws.on_upgrade(move |socket| async move {
                        let repo = (*state).clone();
                        let factory = axum_ws::acceptor_fn(move |req, conn| {
                            match req.service() {
                                "ExampleRepo" => {
                                    conn.handle_with(ExampleRepoDispatcher::new(repo.clone()));
                                    Ok(())
                                }
                                _ => Err(vec![]),
                            }
                        });
                        axum_ws::serve(socket, factory).await;
                    })
                }
            }),
        )
        .with_state(state)
}

/// Spawn the router on an OS-assigned port. Returns the bound URL and
/// a shutdown sender — drop the sender or send `()` to stop the server.
pub async fn spawn() -> (String, oneshot::Sender<()>) {
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap();
    let (shutdown_tx, shutdown_rx) = oneshot::channel::<()>();
    let app = router(ExampleRepoMemory::new());
    tokio::spawn(async move {
        let _ = axum::serve(listener, app)
            .with_graceful_shutdown(async {
                let _ = shutdown_rx.await;
            })
            .await;
    });
    (format!("ws://{addr}/vox"), shutdown_tx)
}

#[tokio::test]
async fn router_builds_and_serves_health() {
    let (ws_url, shutdown) = spawn().await;
    // Strip /vox + ws:// to hit /api/health over http.
    let http = ws_url
        .replace("ws://", "http://")
        .replace("/vox", "/api/health");
    let body = reqwest_health(&http).await;
    assert_eq!(body, "ok");
    let _ = shutdown.send(());
}

// Tiny stdlib HTTP client to avoid pulling reqwest into the test crate
// for one GET.
async fn reqwest_health(url: &str) -> String {
    let (host_port, path) = url.trim_start_matches("http://").split_once('/').unwrap();
    let mut stream = tokio::net::TcpStream::connect(host_port).await.unwrap();
    use tokio::io::{AsyncReadExt, AsyncWriteExt};
    let req = format!("GET /{path} HTTP/1.1\r\nHost: {host_port}\r\nConnection: close\r\n\r\n");
    stream.write_all(req.as_bytes()).await.unwrap();
    let mut buf = Vec::new();
    stream.read_to_end(&mut buf).await.unwrap();
    let s = String::from_utf8_lossy(&buf);
    s.rsplit("\r\n\r\n").next().unwrap_or("").trim().to_string()
}
