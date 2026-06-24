//! In-process transport — serve a [`LayerRouter`] over a vox in-memory
//! link so a typed `<T>Client` consumes a *local* backend with no server
//! and no socket.
//!
//! This is the "inject the transport, not the backend" half of
//! architect's DI story: screens/callers program against the generated
//! `<T>Client`; whether that client talks to a remote server (a vox
//! WebSocket) or a backend running in the same process is chosen at the
//! edge. The remote side is `axum_ws::serve`; this is its in-process
//! twin — same acceptor dance ([`vox_core::acceptor_on`] +
//! [`vox_core::lane_acceptor_fn`] + `establish_connection()`), but over
//! [`vox_core::memory_link_pair`] instead of a network socket.
//!
//! Native only: vox's `MemoryLink` isn't compiled for wasm, so browser
//! clients stay remote. Desktop, CLI, tests, and servers can run fully
//! in-process.
//!
//! ```ignore
//! let scope = Scope::new();
//! let local = architect::serve_local(ExampleRepoMemory::new(), &scope);
//! let repo: ExampleRepoClient = local.establish().await?;   // no server!
//! let row = repo.create(/* … */).await?;
//! scope.close().await;
//! ```

use std::sync::Arc;

use crate::layer::{LayerRouter, Services};
use crate::resource::Scope;

/// A `LayerRouter` served in-process. Each [`establish`](LocalServer::establish)
/// opens a fresh in-memory link + acceptor task (one client per session,
/// mirroring the remote per-service connection shape); the task is
/// aborted when the [`Scope`] closes.
#[derive(Clone)]
pub struct LocalServer {
    router: LayerRouter,
    scope: Arc<Scope>,
}

impl LocalServer {
    /// Serve an already-built router in-process.
    pub fn serve(router: LayerRouter, scope: Arc<Scope>) -> Self {
        Self { router, scope }
    }

    /// Establish a typed client against the local router — the same
    /// `<T>Client` you'd get over a WebSocket, no network involved.
    pub async fn establish<C>(&self) -> eyre::Result<C>
    where
        C: vox_core::FromVoxLane,
    {
        let (client_link, server_link) = vox_core::memory_link_pair(16);
        let router = self.router.clone();

        // Server side: accept on one end and serve the router for any
        // requested lane (it dispatches by method id). Hold the
        // connection alive until the task is aborted on scope close.
        let task = tokio::task::spawn(async move {
            let acceptor = vox_core::lane_acceptor_fn(move |_req, connection| {
                connection.handle_with(router.clone());
                Ok(())
            });
            match vox_core::acceptor_on(server_link)
                .on_lane(acceptor)
                .establish_connection()
                .await
            {
                Ok(connection) => {
                    let _connection = connection;
                    std::future::pending::<()>().await;
                }
                Err(e) => tracing::warn!(error = %e, "local vox acceptor failed"),
            }
        });
        self.scope.defer(move || async move {
            task.abort();
        });

        // Client side: establish over the other end.
        vox_core::initiator_on(client_link)
            .establish::<C>()
            .await
            .map_err(|e| eyre::eyre!("local establish: {e:?}"))
    }
}

/// Build a [`Services`] backend's router and serve it in-process. The
/// shortcut for the common case — `serve_local(backend, &scope)` then
/// `.establish::<SomeClient>()`.
pub fn serve_local<B>(backend: B, scope: &Arc<Scope>) -> LocalServer
where
    B: Services + Clone + Send + Sync + 'static,
{
    LocalServer::serve(backend.into_router(), Arc::clone(scope))
}
