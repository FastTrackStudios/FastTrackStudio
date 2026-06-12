//! Cached vox service clients — cross-target (wasm + native).
//!
//! Establishing a vox client opens a WebSocket and runs the handshake.
//! On **wasm** the client's wasm-bindgen callback closures live exactly
//! as long as the established client value — a naive connect → use → drop
//! tears those closures down at the end of the request while the socket
//! can still fire a queued event, surfacing as
//! `closure invoked recursively or after being dropped` and a dead page.
//! So on wasm each `(service, org)` client is established **once** and
//! cached for the page's lifetime, then reused for every request.
//!
//! On **native** there's no dropped-closure hazard, so the native build
//! skips the cache and establishes per call (a native client cache is a
//! possible follow-up if the desktop/SSR path turns hot).
//!
//! Both targets share one transport — `vox_websocket::WsLink::connect`
//! (web-sys `WebSocket` on wasm, `tokio-tungstenite` on native) plus
//! vox-core's `initiator_on(..).establish()`. There is **no** per-target
//! API duplication: architect's generated clients carry no transport
//! assumptions, and the vox stack is already cross-target, so the same
//! `establish_for` works on both.
//!
//! Reconnection after a server restart is a follow-up — a stale client
//! surfaces as a request error, and a reload re-establishes.

use crate::vox_session::vox_url;

/// Establish a client of type `C` against `url` — no caching. Shared by
/// every public helper; cross-target.
async fn establish_at<C>(url: &str) -> Result<C, String>
where
    C: vox_core::FromVoxSession + 'static,
{
    use vox_core::{TransportMode, initiator_on};
    if url.is_empty() {
        return Err("no vox URL configured (set TASK_VOX_URL[_WEB])".to_owned());
    }
    let link = vox_websocket::WsLink::connect(url)
        .await
        .map_err(|e| format!("ws connect `{url}`: {e:?}"))?;
    initiator_on(link, TransportMode::Bare)
        .establish::<C>()
        .await
        .map_err(|e| format!("establish `{url}`: {e:?}"))
}

/// Per-org vox endpoint: the configured base retargeted at
/// `/org/<slug>/vox`. Empty when no base is configured.
fn org_ws_url(slug: &str) -> String {
    let base = vox_url();
    if base.is_empty() {
        return String::new();
    }
    let trimmed = base.trim_end_matches("/vox").trim_end_matches('/');
    format!("{trimmed}/org/{slug}/vox")
}

/// One shared [`vox_core::Caller`] per org — the handle every typed
/// client is built from. On wasm the connection root (a liveness-only
/// `NoopClient`) is cached per slug: one socket per org, no matter how
/// many services a page touches. The server's per-org `LayerRouter`
/// dispatches every service on that one connection.
///
/// This also backs the app root's `Connection<Caller>`
/// (`architect::use_app_reactive` over the active org) — pages that
/// migrate to atom hooks build clients from the shared caller; legacy
/// `feeds::*` fns ride the same socket through [`establish_for`].
pub async fn caller_for(slug: &str) -> Result<vox_core::Caller, String> {
    #[cfg(target_arch = "wasm32")]
    {
        use std::cell::RefCell;
        use std::collections::HashMap;

        // The cached NoopClient is the connection's liveness anchor —
        // dropping the last caller closes the socket, so the cache
        // holds the root for the page's lifetime. Typed clients built
        // from the caller are virtual views (no session handle).
        thread_local! {
            static ROOTS: RefCell<HashMap<String, vox_core::NoopClient>> =
                RefCell::new(HashMap::new());
        }
        if let Some(caller) =
            ROOTS.with(|m| m.borrow().get(slug).map(|root| root.caller.clone()))
        {
            return Ok(caller);
        }
        let root = establish_at::<vox_core::NoopClient>(&org_ws_url(slug)).await?;
        let caller = root.caller.clone();
        ROOTS.with(|m| m.borrow_mut().insert(slug.to_owned(), root));
        Ok(caller)
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        // Native has no cache (mirrors the old per-call behavior); the
        // root is returned through the caller it carries — callers that
        // need liveness across awaits should hold the typed client from
        // `establish_for` instead.
        let root = establish_at::<vox_core::NoopClient>(&org_ws_url(slug)).await?;
        Ok(root.caller.clone())
    }
}

/// Establish *any* service client against a specific org's vox endpoint.
/// Wasm: a cheap typed view over the org's one cached connection
/// ([`caller_for`]) — previously this cached a socket per
/// `(service, org)`. Native: per-call establish, as before.
pub async fn establish_for<C>(slug: &str) -> Result<C, String>
where
    C: vox_core::FromVoxSession + Clone + 'static,
{
    #[cfg(target_arch = "wasm32")]
    {
        let caller = caller_for(slug).await?;
        Ok(C::from_vox_session(caller, None))
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        establish_at::<C>(&org_ws_url(slug)).await
    }
}

/// An org's `TaskServiceClient` — a view over the org's shared caller.
pub async fn task_client(slug: &str) -> Result<task::TaskServiceClient, String> {
    establish_for::<task::TaskServiceClient>(slug).await
}

/// An org's `ProjectServiceClient`.
pub async fn project_client(slug: &str) -> Result<project::ProjectServiceClient, String> {
    establish_for::<project::ProjectServiceClient>(slug).await
}

/// An org's `VaultSyncClient` — backs the `/vault` route (manifest,
/// read, conditional write over the same long-lived socket).
pub async fn vault_client(slug: &str) -> Result<vault_proto::VaultSyncClient, String> {
    establish_for::<vault_proto::VaultSyncClient>(slug).await
}

/// An org's `VaultGraphClient` — link-graph reads (backlinks /
/// links / orphans / unresolved / deadends / tags) for the vault
/// page's backlinks panel and the editor's tag candidates.
pub async fn vault_graph_client(slug: &str) -> Result<vault_proto::VaultGraphClient, String> {
    establish_for::<vault_proto::VaultGraphClient>(slug).await
}
