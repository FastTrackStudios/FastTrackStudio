//! Task server — Loro sync-relay library.
//!
//! Exposed so tests can spin up the server in-process on a random
//! port, with any `Persistence` impl (e.g. `InMemoryPersistence` for
//! fast realtime-sync integration tests). The binary at `src/main.rs`
//! is a thin shell around this.
//!
//! WebSocket protocol per `/sync/:doc_id`:
//!
//! 1. Server → client (on connect): one binary frame containing
//!    `doc.export(Snapshot)` — bring the new peer up to date with
//!    everything the doc has accumulated.
//! 2. Client → server: binary frames of Loro update bytes (typically
//!    one per `commit()` on the client doc, from
//!    `subscribe_local_update`).
//! 3. Server → other clients: forward each imported update to every
//!    other socket in the room. We skip echoing back to the
//!    originator — keeps things tidy even though Loro would handle
//!    a self-echo gracefully.
//!
//! Bytes are bytes. No text frames, no JSON, no control protocol.

use std::collections::HashMap;
use std::sync::Arc;

use axum::Router;
use axum::extract::ws::{Message, WebSocket, WebSocketUpgrade};
use axum::extract::{Path, State};
use axum::response::IntoResponse;
use axum::routing::get;
use crdt::Persistence;
use eyre::WrapErr;
use futures::{SinkExt, StreamExt};
use loro::{ExportMode, LoroDoc};
use tokio::sync::{Mutex, broadcast};
use tracing::{error, info, warn};
use uuid::Uuid;

// ── Public API ────────────────────────────────────────────────────────

/// Application state — clone-friendly handle to the room registry +
/// persistence backend. Pass to [`router`] to mount the WS endpoint.
#[derive(Clone)]
pub struct AppState {
    rooms: Arc<Mutex<HashMap<Uuid, Arc<RoomState>>>>,
    persistence: Arc<dyn Persistence>,
}

impl AppState {
    pub fn new<P: Persistence>(persistence: P) -> Self {
        Self {
            rooms: Arc::new(Mutex::new(HashMap::new())),
            persistence: Arc::new(persistence),
        }
    }

    /// Build with an already-erased persistence handle. Useful for
    /// tests that share one `Arc<InMemoryPersistence>` across the
    /// server and assertion helpers.
    pub fn with_persistence(persistence: Arc<dyn Persistence>) -> Self {
        Self {
            rooms: Arc::new(Mutex::new(HashMap::new())),
            persistence,
        }
    }
}

/// Build the axum router. `/health` + `/sync/{doc_id}` + permissive
/// CORS. Mount under any TCP listener with `axum::serve`.
pub fn router(state: AppState) -> Router {
    Router::new()
        .route("/health", get(|| async { "ok" }))
        .route("/sync/{doc_id}", get(ws_handler))
        .layer(tower_http::cors::CorsLayer::permissive())
        .with_state(state)
}

// ── Internal: rooms ───────────────────────────────────────────────────

/// One per active doc_id. The LoroDoc is the server's authoritative
/// copy; the broadcast channel fans out every imported update to
/// every connected peer's outbound write loop. Loro's docs call out
/// this server-side-import pattern as the canonical relay shape.
///
/// Wrapped in `Arc` so room handles are cheap to clone per
/// connection. Inside, the LoroDoc is guarded by an async mutex —
/// only one task touches it at a time.
struct RoomState {
    doc: Mutex<LoroDoc>,
    tx: broadcast::Sender<RelayMsg>,
}

/// Outbound broadcast payload. `origin` is the conn id that produced
/// the bytes — we use it to skip echoing back to the sender.
#[derive(Clone, Debug)]
struct RelayMsg {
    origin: ConnId,
    bytes: Arc<Vec<u8>>,
}

type ConnId = u64;

fn next_conn_id() -> ConnId {
    use std::sync::atomic::{AtomicU64, Ordering};
    static N: AtomicU64 = AtomicU64::new(1);
    N.fetch_add(1, Ordering::Relaxed)
}

async fn get_or_create_room(state: &AppState, doc_id: Uuid) -> eyre::Result<Arc<RoomState>> {
    {
        let rooms = state.rooms.lock().await;
        if let Some(r) = rooms.get(&doc_id) {
            return Ok(r.clone());
        }
    }

    let doc = LoroDoc::new();
    // Hydrate from persistence: snapshot first, then any updates
    // captured after the most recent compaction.
    if let Some(snap) = state.persistence.load_snapshot(doc_id).await? {
        doc.import(&snap)
            .map_err(|e| eyre::eyre!("import snapshot: {e}"))?;
        info!(%doc_id, bytes = snap.len(), "rehydrated from snapshot");
    }
    let updates = state.persistence.load_updates(doc_id).await?;
    let update_count = updates.len();
    for u in updates {
        doc.import(&u)
            .map_err(|e| eyre::eyre!("import update: {e}"))?;
    }
    if update_count > 0 {
        info!(%doc_id, n = update_count, "replayed updates");
    }

    let (tx, _rx) = broadcast::channel(256);
    let room = Arc::new(RoomState {
        doc: Mutex::new(doc),
        tx,
    });

    let mut rooms = state.rooms.lock().await;
    if let Some(existing) = rooms.get(&doc_id) {
        return Ok(existing.clone());
    }
    rooms.insert(doc_id, room.clone());
    Ok(room)
}

// ── WS handler ────────────────────────────────────────────────────────

async fn ws_handler(
    Path(doc_id): Path<Uuid>,
    State(state): State<AppState>,
    ws: WebSocketUpgrade,
) -> impl IntoResponse {
    ws.on_upgrade(move |socket| async move {
        if let Err(e) = run_socket(state, doc_id, socket).await {
            warn!(%doc_id, "socket closed with error: {e:?}");
        }
    })
}

async fn run_socket(state: AppState, doc_id: Uuid, socket: WebSocket) -> eyre::Result<()> {
    let conn_id = next_conn_id();
    let room = get_or_create_room(&state, doc_id).await?;
    let mut rx = room.tx.subscribe();
    info!(%doc_id, conn_id, "peer joined");

    let (mut ws_out, mut ws_in) = socket.split();

    // Snapshot bring-up. Send the current state before anything else.
    let snap = {
        let doc = room.doc.lock().await;
        doc.export(ExportMode::Snapshot)
            .map_err(|e| eyre::eyre!("export snapshot: {e}"))?
    };
    if let Err(e) = ws_out.send(Message::Binary(snap.into())).await {
        warn!(%doc_id, conn_id, "send snapshot failed: {e}");
        return Ok(());
    }

    // Outbound: forward broadcast messages from other peers.
    let conn_id_for_out = conn_id;
    let outbound = tokio::spawn(async move {
        while let Ok(msg) = rx.recv().await {
            if msg.origin == conn_id_for_out {
                continue; // don't echo our own update back to us
            }
            let bytes: Vec<u8> = (*msg.bytes).clone();
            if ws_out.send(Message::Binary(bytes.into())).await.is_err() {
                break;
            }
        }
    });

    // Inbound: read frames, import + persist + broadcast.
    let inbound = async {
        while let Some(msg) = ws_in.next().await {
            let msg = msg.wrap_err("read ws")?;
            match msg {
                Message::Binary(bytes) => {
                    let bytes_vec = bytes.to_vec();
                    // Import into the server's authoritative doc.
                    {
                        let doc = room.doc.lock().await;
                        if let Err(e) = doc.import(&bytes_vec) {
                            warn!(%doc_id, conn_id, "import failed: {e}");
                            continue;
                        }
                    }
                    // Persist + broadcast.
                    if let Err(e) = state.persistence.append_update(doc_id, &bytes_vec).await {
                        error!(%doc_id, conn_id, "persist failed: {e}");
                    }
                    let _ = room.tx.send(RelayMsg {
                        origin: conn_id,
                        bytes: Arc::new(bytes_vec),
                    });
                }
                Message::Close(_) => break,
                Message::Ping(_) | Message::Pong(_) | Message::Text(_) => {
                    // Ignore — protocol is binary-only.
                }
            }
        }
        Ok::<_, eyre::Error>(())
    };

    let result = inbound.await;
    outbound.abort();
    info!(%doc_id, conn_id, "peer left");
    result
}
