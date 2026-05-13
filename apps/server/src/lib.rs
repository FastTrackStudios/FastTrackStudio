//! Task server — Loro sync-relay library + webhook receivers.
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
//!
//! ## Webhook surface
//!
//! Mounted alongside the WS relay:
//!
//! - `POST /webhooks/github/{repo_path}` — GitHub events, signature
//!   verified via `X-Hub-Signature-256` against the unsealed
//!   `GitRepoConnection.webhook_secret_hash`.
//! - `POST /webhooks/hermes/event` — Hermes dashboard events,
//!   signature verified via `X-Webhook-Signature` against the
//!   configured `hermes_webhook_secret`.
//!
//! Both routes are *additive*: AppState constructors that pre-date
//! webhooks (used by the realtime-sync test) leave the optional
//! fields `None`, and the routes return 401 / 404 cleanly.

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

use agent_crdt::{
    AgentConversationRepoLoro, AgentLogLineRepoLoro, AgentRunRepoLoro, GitRepoConnectionRepoLoro,
};
use agent_proto::ChatModelRegistry;
use agent_proto::integration::{EventSink, EventSinkImpl, IntegrationRegistry, ShutdownSignal};
use chat_crdt::MessageRepoLoro;
use crdt::CrdtDoc;
use project_crdt::TaskRepoLoro;

pub mod chat;
pub mod integration_sink;
pub mod sealing;
pub mod webhook_inbox;
pub mod webhooks;

pub use integration_sink::ServerEventSink;
pub use sealing::{Sealing, SealingError};
pub use webhook_inbox::{WebhookInbox, WebhookInboxRow};

// ── Public API ────────────────────────────────────────────────────────

/// Application state — clone-friendly handle to the room registry +
/// persistence backend + webhook plumbing. Pass to [`router`] to
/// mount everything.
#[derive(Clone)]
pub struct AppState {
    rooms: Arc<Mutex<HashMap<Uuid, Arc<RoomState>>>>,
    persistence: Arc<dyn Persistence>,

    // ── Webhook + integration plumbing ───────────────────────────────
    //
    // All `Option` / cheap defaults so legacy constructors (used by
    // the realtime-sync integration test) still work unchanged. The
    // production main.rs fills these in.
    pub sealing: Option<Sealing>,
    pub webhook_inbox: WebhookInbox,
    pub registry: Arc<IntegrationRegistry>,
    pub event_sink: Option<EventSink>,
    pub hermes_webhook_secret: Option<String>,

    pub task_repo: Arc<TaskRepoLoro>,
    pub agent_run_repo: Arc<AgentRunRepoLoro>,
    pub agent_log_repo: Arc<AgentLogLineRepoLoro>,
    pub git_repo_repo: Arc<GitRepoConnectionRepoLoro>,

    // ── AI chat ──────────────────────────────────────────────────────
    pub message_repo: Arc<MessageRepoLoro>,
    pub agent_conversation_repo: Arc<AgentConversationRepoLoro>,
    pub chat_model_registry: Arc<ChatModelRegistry>,
    pub chat_sessions: Arc<chat::ChatStreamSessions>,

    pub workspace_doc: Arc<CrdtDoc>,
    pub shutdown: ShutdownSignal,
}

impl AppState {
    /// Default construction — opens the workspace `CrdtDoc` against
    /// the provided persistence and builds the per-feature repos that
    /// webhook handlers write into.
    ///
    /// **v1 wiring limitation**: webhook writes hit a *separate*
    /// `LoroDoc` from the per-room sync doc; they share persistence,
    /// so updates appear to WS clients only after the room
    /// rehydrates from storage (i.e. after the last peer drops and a
    /// new one joins). Promoting this to live broadcast is a
    /// follow-up.
    pub async fn new<P: Persistence>(persistence: P) -> eyre::Result<Self> {
        let persistence: Arc<dyn Persistence> = Arc::new(persistence);
        Self::with_persistence_async(persistence).await
    }

    pub async fn with_persistence_async(persistence: Arc<dyn Persistence>) -> eyre::Result<Self> {
        let workspace_doc_id = task_db::WORKSPACE_DOC_ID;
        // Open the workspace doc against the shared persistence.
        // CrdtDoc::open is server-only (uses tokio::spawn for the
        // local-update flush), which matches our deployment.
        let crdt_doc = CrdtDoc::open(workspace_doc_id, ErasedPersistence(persistence.clone()))
            .await
            .map_err(|e| eyre::eyre!("open workspace CrdtDoc: {e}"))?;
        let crdt_doc = Arc::new(crdt_doc);

        let task_repo = Arc::new(TaskRepoLoro::new(&crdt_doc));
        let agent_run_repo = Arc::new(AgentRunRepoLoro::new(&crdt_doc));
        let agent_log_repo = Arc::new(AgentLogLineRepoLoro::new(&crdt_doc));
        let git_repo_repo = Arc::new(GitRepoConnectionRepoLoro::new(&crdt_doc));
        let message_repo = Arc::new(MessageRepoLoro::new(&crdt_doc));
        let agent_conversation_repo = Arc::new(AgentConversationRepoLoro::new(&crdt_doc));

        Ok(Self {
            rooms: Arc::new(Mutex::new(HashMap::new())),
            persistence,
            sealing: None,
            webhook_inbox: WebhookInbox::new(),
            registry: Arc::new(IntegrationRegistry::new()),
            event_sink: None,
            hermes_webhook_secret: None,
            task_repo,
            agent_run_repo,
            agent_log_repo,
            git_repo_repo,
            message_repo,
            agent_conversation_repo,
            chat_model_registry: Arc::new(ChatModelRegistry::new()),
            chat_sessions: Arc::new(chat::ChatStreamSessions::new()),
            workspace_doc: crdt_doc,
            shutdown: ShutdownSignal::new(),
        })
    }

    /// Legacy synchronous constructor for tests that don't need the
    /// webhook surface. Builds a parallel in-memory CrdtDoc for the
    /// repo handles so the struct is still inhabited; webhooks
    /// against this state will see an empty repo (which is what
    /// realtime-sync tests want).
    pub fn new_sync<P: Persistence>(persistence: P) -> Self {
        let persistence: Arc<dyn Persistence> = Arc::new(persistence);
        Self::with_persistence_sync(persistence)
    }

    /// Build with an already-erased persistence handle. Used by the
    /// realtime-sync integration test to share one
    /// `Arc<InMemoryPersistence>` across server + assertion helpers.
    pub fn with_persistence(persistence: Arc<dyn Persistence>) -> Self {
        Self::with_persistence_sync(persistence)
    }

    fn with_persistence_sync(persistence: Arc<dyn Persistence>) -> Self {
        // Test path: use an ephemeral CrdtDoc — no async needed.
        let crdt_doc = Arc::new(CrdtDoc::ephemeral());
        let task_repo = Arc::new(TaskRepoLoro::new(&crdt_doc));
        let agent_run_repo = Arc::new(AgentRunRepoLoro::new(&crdt_doc));
        let agent_log_repo = Arc::new(AgentLogLineRepoLoro::new(&crdt_doc));
        let git_repo_repo = Arc::new(GitRepoConnectionRepoLoro::new(&crdt_doc));
        let message_repo = Arc::new(MessageRepoLoro::new(&crdt_doc));
        let agent_conversation_repo = Arc::new(AgentConversationRepoLoro::new(&crdt_doc));
        Self {
            rooms: Arc::new(Mutex::new(HashMap::new())),
            persistence,
            sealing: None,
            webhook_inbox: WebhookInbox::new(),
            registry: Arc::new(IntegrationRegistry::new()),
            event_sink: None,
            hermes_webhook_secret: None,
            task_repo,
            agent_run_repo,
            agent_log_repo,
            git_repo_repo,
            message_repo,
            agent_conversation_repo,
            chat_model_registry: Arc::new(ChatModelRegistry::new()),
            chat_sessions: Arc::new(chat::ChatStreamSessions::new()),
            workspace_doc: crdt_doc,
            shutdown: ShutdownSignal::new(),
        }
    }

    /// Construct + wire the event sink from the in-state repos. Call
    /// after registering integration plugins so they share the same
    /// sink instance.
    pub fn build_event_sink(&self) -> EventSink {
        let sink: Arc<dyn EventSinkImpl> = Arc::new(ServerEventSink::new(
            self.agent_run_repo.clone(),
            self.agent_log_repo.clone(),
            self.task_repo.clone(),
        ));
        EventSink { inner: sink }
    }
}

/// Tiny `Persistence` newtype to coerce `Arc<dyn Persistence>` into a
/// type that satisfies `CrdtDoc::open`'s `P: Persistence` bound. The
/// generic bound prevents passing the trait object directly.
struct ErasedPersistence(Arc<dyn Persistence>);

#[async_trait::async_trait]
impl Persistence for ErasedPersistence {
    async fn load_snapshot(&self, doc_id: Uuid) -> Result<Option<Vec<u8>>, crdt::PersistError> {
        self.0.load_snapshot(doc_id).await
    }
    async fn load_updates(&self, doc_id: Uuid) -> Result<Vec<Vec<u8>>, crdt::PersistError> {
        self.0.load_updates(doc_id).await
    }
    async fn append_update(&self, doc_id: Uuid, bytes: &[u8]) -> Result<(), crdt::PersistError> {
        self.0.append_update(doc_id, bytes).await
    }
    async fn write_snapshot(&self, doc_id: Uuid, bytes: &[u8]) -> Result<(), crdt::PersistError> {
        self.0.write_snapshot(doc_id, bytes).await
    }
}

/// Build the axum router. `/health` + `/sync/{doc_id}` +
/// `/webhooks/github/{repo_path}` + `/webhooks/hermes/event` +
/// permissive CORS. Mount under any TCP listener with `axum::serve`.
pub fn router(state: AppState) -> Router {
    Router::new()
        .route("/health", get(|| async { "ok" }))
        .route("/sync/{doc_id}", get(ws_handler))
        .nest("/webhooks/github", webhooks::github::router())
        .nest("/webhooks/hermes", webhooks::hermes::router())
        .nest("/api/agent-chat", chat::router())
        .layer(tower_http::cors::CorsLayer::permissive())
        .with_state(state)
}

// ── Internal: rooms ───────────────────────────────────────────────────

/// One per active doc_id. The LoroDoc is the server's authoritative
/// copy; the broadcast channel fans out every imported update to
/// every connected peer's outbound write loop. Loro's docs call out
/// this server-side-import pattern as the canonical relay shape.
struct RoomState {
    doc: Mutex<LoroDoc>,
    tx: broadcast::Sender<RelayMsg>,
}

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

    let snap = {
        let doc = room.doc.lock().await;
        doc.export(ExportMode::Snapshot)
            .map_err(|e| eyre::eyre!("export snapshot: {e}"))?
    };
    if let Err(e) = ws_out.send(Message::Binary(snap.into())).await {
        warn!(%doc_id, conn_id, "send snapshot failed: {e}");
        return Ok(());
    }

    let conn_id_for_out = conn_id;
    let outbound = tokio::spawn(async move {
        while let Ok(msg) = rx.recv().await {
            if msg.origin == conn_id_for_out {
                continue;
            }
            let bytes: Vec<u8> = (*msg.bytes).clone();
            if ws_out.send(Message::Binary(bytes.into())).await.is_err() {
                break;
            }
        }
    });

    let inbound = async {
        while let Some(msg) = ws_in.next().await {
            let msg = msg.wrap_err("read ws")?;
            match msg {
                Message::Binary(bytes) => {
                    let bytes_vec = bytes.to_vec();
                    {
                        let doc = room.doc.lock().await;
                        if let Err(e) = doc.import(&bytes_vec) {
                            warn!(%doc_id, conn_id, "import failed: {e}");
                            continue;
                        }
                    }
                    if let Err(e) = state.persistence.append_update(doc_id, &bytes_vec).await {
                        error!(%doc_id, conn_id, "persist failed: {e}");
                    }
                    let _ = room.tx.send(RelayMsg {
                        origin: conn_id,
                        bytes: Arc::new(bytes_vec),
                    });
                }
                Message::Close(_) => break,
                Message::Ping(_) | Message::Pong(_) | Message::Text(_) => {}
            }
        }
        Ok::<_, eyre::Error>(())
    };

    let result = inbound.await;
    outbound.abort();
    info!(%doc_id, conn_id, "peer left");
    result
}
