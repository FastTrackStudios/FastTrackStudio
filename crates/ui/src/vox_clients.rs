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

/// Establish *any* service client against a specific org's vox endpoint.
/// On wasm, cached per `(service type, slug)` so the multi-org fetchers
/// reuse one socket per org instead of re-dialing on every render. This
/// is what backs the org switcher's "All" mode (fan out across slugs)
/// and single-org mode alike.
pub async fn establish_for<C>(slug: &str) -> Result<C, String>
where
    C: vox_core::FromVoxSession + Clone + 'static,
{
    #[cfg(target_arch = "wasm32")]
    {
        use std::any::{Any, TypeId};
        use std::cell::RefCell;
        use std::collections::HashMap;

        thread_local! {
            static CACHE: RefCell<HashMap<(TypeId, String), Box<dyn Any>>> =
                RefCell::new(HashMap::new());
        }
        let key = (TypeId::of::<C>(), slug.to_owned());
        if let Some(c) = CACHE.with(|m| {
            m.borrow()
                .get(&key)
                .and_then(|b| b.downcast_ref::<C>().cloned())
        }) {
            return Ok(c);
        }
        let client = establish_at::<C>(&org_ws_url(slug)).await?;
        CACHE.with(|m| m.borrow_mut().insert(key, Box::new(client.clone())));
        Ok(client)
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        establish_at::<C>(&org_ws_url(slug)).await
    }
}

/// The home org's `TaskServiceClient`, established (and on wasm cached)
/// via [`establish_for`]. Org is the `codywright` home org until the
/// org-context signal threads through here.
pub async fn task_client() -> Result<task::TaskServiceClient, String> {
    establish_for::<task::TaskServiceClient>("codywright").await
}

/// The home org's `ProjectServiceClient`.
pub async fn project_client() -> Result<project::ProjectServiceClient, String> {
    establish_for::<project::ProjectServiceClient>("codywright").await
}

/// The home org's `VaultSyncClient` — backs the `/vault` route (manifest,
/// read, conditional write over the same long-lived socket).
pub async fn vault_client() -> Result<vault_proto::VaultSyncClient, String> {
    establish_for::<vault_proto::VaultSyncClient>("codywright").await
}
