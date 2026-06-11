//! Reusable server wiring for the example app.
//!
//! The HTTP + vox surface is generic over the `ExampleRepo` backend, so
//! the same router serves the sqlite, in-memory, or any future backend —
//! and the e2e tests mount it with the in-memory repo instead of
//! re-deriving the wiring. The bin (`main.rs`) only owns backend
//! selection + migrations; everything else lives here.
//!
//! The vox surface is composed through architect's `Layer` system. The
//! backend is wrapped **once** in the derive-emitted
//! [`example::ExampleEvented`] — whose `Services` bundle mounts the CRUD
//! repo *and* the live `ExampleEvents` feed together, broadcasting every
//! successful write to every subscriber — then merged with the
//! hand-written `ExampleService`. Swapping the repo backend changes
//! nothing here.

pub mod service_impl;

use std::sync::Arc;

use axum::{
    Router,
    extract::{State, WebSocketUpgrade},
    response::{IntoResponse, Response},
    routing::get,
};
use crdt::CrdtDoc;
use crdt::sync::{
    DocPresenceDispatcher, DocSyncDispatcher, DocSyncHost, PresenceHost,
    doc_presence_service_descriptor, doc_sync_service_descriptor,
};
use example::architect::{Filter, Layer, LayerRouter, Page, RepoError, Services, Sort, layers};
use example::axum_ws;
use example::{
    COLLAB_DOC_ID, Example, ExampleCreate, ExampleEvented, ExampleList, ExampleRepo,
    ExampleServiceDispatcher, ExampleUpdate,
};
use service_impl::ExampleServiceImpl;
use tower_http::cors::CorsLayer;
use uuid::Uuid;

/// Compose the app's whole vox surface into one `LayerRouter`: the
/// evented repo's `Services` bundle (CRUD + the live event feed) plus
/// `ExampleService` (search/duplicate). The router dispatches every
/// service's methods by id.
///
/// Wraps the backend in [`ExampleEvented`] internally — call **once** per
/// process (the broadcast hub lives inside the wrapper; building several
/// routers from one wrapped repo is fine, wrapping twice splits the hub).
/// The axum server ([`vox_router`]) wraps once and builds a router per
/// WebSocket connection over the shared instance; an in-process host
/// (`architect::LocalServer::serve(service_router(repo), …)`, see the
/// desktop app) calls this directly — live events included, no socket.
pub fn service_router<R>(repo: R, collab: &Collab) -> LayerRouter
where
    R: ExampleRepo + Services + Clone + Send + Sync + 'static,
{
    evented_router(ExampleEvented::new(repo), collab)
}

/// Build the service router over an already-wrapped (shared) repo.
pub fn evented_router<R>(repo: ExampleEvented<R>, collab: &Collab) -> LayerRouter
where
    R: ExampleRepo + Clone + Send + Sync + 'static,
{
    let router = repo.clone().into_router().with(
        example::example_service_service_descriptor(),
        ExampleServiceDispatcher::new(ExampleServiceImpl::new(repo)),
    );
    collab.mount(router)
}

// ── Collaborative state (the shared notes doc) ────────────────────────

/// The app's collaborative surface: one canonical notes doc
/// ([`DocSyncHost`] — every client holds a replica, edits merge) and its
/// presence channel ([`PresenceHost`] — who's online, ephemeral).
///
/// Like [`ExampleEvented`], build it **once per process** and share it:
/// the fan-out hubs live inside, and the hosts are cheap clones over the
/// same state, so per-connection routers all serve the same doc.
#[derive(Clone)]
pub struct Collab {
    pub sync: DocSyncHost,
    pub presence: PresenceHost,
}

impl Collab {
    /// In-memory doc — for tests and the in-process desktop transport.
    pub fn ephemeral() -> Self {
        Self::from_doc(CrdtDoc::ephemeral())
    }

    /// File-persisted doc under `data_dir` — the server's notes survive
    /// restarts, and the host compacts the update log as it grows.
    pub async fn open(data_dir: impl Into<std::path::PathBuf>) -> Result<Self, crdt::PersistError> {
        let doc = CrdtDoc::open(COLLAB_DOC_ID, crdt::FilePersistence::new(data_dir)).await?;
        Ok(Self::from_doc(doc))
    }

    fn from_doc(doc: CrdtDoc) -> Self {
        Self {
            // Compact every 64 updates so storage stays bounded no
            // matter how chatty the doc gets.
            sync: DocSyncHost::new(COLLAB_DOC_ID, doc).with_compaction(64),
            // Presence states expire 30s after their peer goes quiet.
            presence: PresenceHost::new(COLLAB_DOC_ID, 30_000),
        }
    }

    /// The canonical doc (e.g. for server-side reads via `NoteRepoLoro`).
    pub fn doc(&self) -> &CrdtDoc {
        self.sync.doc()
    }

    /// Mount both services onto a router.
    pub fn mount(&self, router: LayerRouter) -> LayerRouter {
        router
            .with(
                doc_sync_service_descriptor(),
                DocSyncDispatcher::new(self.sync.clone()),
            )
            .with(
                doc_presence_service_descriptor(),
                DocPresenceDispatcher::new(self.presence.clone()),
            )
    }
}

/// Build the app's axum router for any `ExampleRepo` backend that also
/// declares its `Services` bundle: a health endpoint plus the `/vox`
/// WebSocket serving [`evented_router`].
///
/// The backend is wrapped here, **once at startup** — every WebSocket
/// connection's router shares the same [`ExampleEvented`] instance, so
/// broadcasts cross connections (a write on one socket reaches
/// subscribers on every other).
pub fn vox_router<R>(repo: R, collab: Collab) -> Router
where
    R: ExampleRepo + Services + Clone + Send + Sync + 'static,
{
    let evented = ExampleEvented::new(LatencyRepo::new(repo));
    Router::new()
        .route("/api/health", get(health))
        .route("/vox", get(vox_ws_handler::<LatencyRepo<R>>))
        .layer(CorsLayer::permissive())
        .with_state(Arc::new((evented, collab)))
}

async fn health() -> &'static str {
    "ok"
}

async fn vox_ws_handler<R>(
    ws: WebSocketUpgrade,
    State(state): State<Arc<(ExampleEvented<R>, Collab)>>,
) -> Response
where
    R: ExampleRepo + Clone + Send + Sync + 'static,
{
    ws.on_upgrade(move |socket| async move {
        let (repo, collab) = (*state).clone();
        let router = evented_router(repo, &collab);
        let factory = axum_ws::acceptor_fn(move |_req, connection| {
            connection.handle_with(router.clone());
            Ok(())
        });
        axum_ws::serve(socket, factory).await;
    })
    .into_response()
}

// ── Test-only latency injector ─────────────────────────────────────────
//
// Wraps any `ExampleRepo` and sleeps `EXAMPLE_LATENCY_MS` milliseconds
// before each *mutation* (create/update/delete) so a manual tester can
// watch the client's optimistic rows sit in their pending state before the
// server reconciles them. Reads (get/list) pass straight through.
//
// With the env var unset (the default) it's a zero-cost passthrough, so
// the e2e suite sees no added latency.
//
//   EXAMPLE_LATENCY_MS=2000 cargo run -p app-server   # or: just server-slow

/// Read the configured artificial latency; `0` (the default) means none.
async fn fake_latency() {
    let ms = std::env::var("EXAMPLE_LATENCY_MS")
        .ok()
        .and_then(|s| s.parse::<u64>().ok())
        .unwrap_or(0);
    if ms > 0 {
        tokio::time::sleep(std::time::Duration::from_millis(ms)).await;
    }
}

/// Latency decorator over an `ExampleRepo`. It also declares the same
/// `Services` bundle as any backend (`ExampleRepoLayer`), so
/// `service_router` accepts it transparently.
#[derive(Clone)]
pub struct LatencyRepo<R> {
    inner: R,
}

impl<R> LatencyRepo<R> {
    pub fn new(inner: R) -> Self {
        Self { inner }
    }
}

impl<R> ExampleRepo for LatencyRepo<R>
where
    R: ExampleRepo + Send + Sync + 'static,
{
    async fn get(&self, id: Uuid) -> Result<Example, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: Page,
        sort: Option<Sort>,
        filter: Option<Filter>,
    ) -> Result<ExampleList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: ExampleCreate) -> Result<Example, RepoError> {
        fake_latency().await;
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: ExampleUpdate) -> Result<Example, RepoError> {
        fake_latency().await;
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        fake_latency().await;
        self.inner.delete(id).await
    }
}

impl<R> Services for LatencyRepo<R>
where
    R: ExampleRepo + Send + Sync + 'static,
{
    fn layers() -> impl Layer<Self> {
        layers![example::ExampleRepoLayer]
    }
}
