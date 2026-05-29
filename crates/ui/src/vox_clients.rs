//! Cached vox service clients.
//!
//! Establishing a vox client opens a WebSocket whose wasm-bindgen
//! callback closures live exactly as long as the established client
//! value. The naive "connect → use → drop" pattern tears those
//! closures down at the end of the request while the socket can still
//! fire a queued event — surfacing as
//! `closure invoked recursively or after being dropped` in the browser
//! console and a dead page.
//!
//! So each service client is established **once** and cached for the
//! page's lifetime (the same reason `vox_session` `mem::forget`s its
//! bootstrap session), then reused for every request. One long-lived
//! WebSocket per service, no per-call churn, no dropped closures.
//!
//! Reconnection after a server restart is a follow-up — a stale client
//! surfaces as a request error, and a reload re-establishes.

#[cfg(target_arch = "wasm32")]
use crate::vox_session::org_vox_url;

/// The org's `TaskServiceClient`, established once and cached.
pub async fn task_client() -> Result<task::TaskServiceClient, String> {
    #[cfg(target_arch = "wasm32")]
    {
        use std::cell::RefCell;
        thread_local! {
            static CACHE: RefCell<Option<task::TaskServiceClient>> = const { RefCell::new(None) };
        }
        if let Some(c) = CACHE.with(|c| c.borrow().clone()) {
            return Ok(c);
        }
        use vox_core::{TransportMode, initiator_on};
        let link = link().await?;
        let client = initiator_on(link, TransportMode::Bare)
            .establish::<task::TaskServiceClient>()
            .await
            .map_err(|e| format!("establish: {e:?}"))?;
        CACHE.with(|cell| *cell.borrow_mut() = Some(client.clone()));
        Ok(client)
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        Err("native client not wired yet".to_owned())
    }
}

/// The org's `ProjectServiceClient`, established once and cached.
pub async fn project_client() -> Result<project::ProjectServiceClient, String> {
    #[cfg(target_arch = "wasm32")]
    {
        use std::cell::RefCell;
        thread_local! {
            static CACHE: RefCell<Option<project::ProjectServiceClient>> =
                const { RefCell::new(None) };
        }
        if let Some(c) = CACHE.with(|c| c.borrow().clone()) {
            return Ok(c);
        }
        use vox_core::{TransportMode, initiator_on};
        let link = link().await?;
        let client = initiator_on(link, TransportMode::Bare)
            .establish::<project::ProjectServiceClient>()
            .await
            .map_err(|e| format!("establish: {e:?}"))?;
        CACHE.with(|cell| *cell.borrow_mut() = Some(client.clone()));
        Ok(client)
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        Err("native client not wired yet".to_owned())
    }
}

/// The org's `VaultSyncClient`, established once and cached.
/// Backs the `/vault` route — file manifest, read, and
/// conditional write over the same long-lived socket.
pub async fn vault_client() -> Result<vault_proto::VaultSyncClient, String> {
    #[cfg(target_arch = "wasm32")]
    {
        use std::cell::RefCell;
        thread_local! {
            static CACHE: RefCell<Option<vault_proto::VaultSyncClient>> =
                const { RefCell::new(None) };
        }
        if let Some(c) = CACHE.with(|c| c.borrow().clone()) {
            return Ok(c);
        }
        use vox_core::{TransportMode, initiator_on};
        let link = link().await?;
        let client = initiator_on(link, TransportMode::Bare)
            .establish::<vault_proto::VaultSyncClient>()
            .await
            .map_err(|e| format!("establish: {e:?}"))?;
        CACHE.with(|cell| *cell.borrow_mut() = Some(client.clone()));
        Ok(client)
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        Err("native client not wired yet".to_owned())
    }
}

/// Open a WS link to the (default home) org endpoint.
#[cfg(target_arch = "wasm32")]
async fn link() -> Result<vox_websocket::WsLink, String> {
    let url = org_vox_url();
    if url.is_empty() {
        return Err("no vox URL configured (set TASK_VOX_URL_WEB)".to_owned());
    }
    vox_websocket::WsLink::connect(&url)
        .await
        .map_err(|e| format!("ws connect: {e:?}"))
}

/// Per-org vox endpoint: the configured base retargeted at
/// `/org/<slug>/vox`. Empty when no base is configured.
#[cfg(target_arch = "wasm32")]
fn org_ws_url(slug: &str) -> String {
    let base = crate::vox_session::vox_url();
    if base.is_empty() {
        return String::new();
    }
    let trimmed = base.trim_end_matches("/vox").trim_end_matches('/');
    format!("{trimmed}/org/{slug}/vox")
}

/// Establish *any* service client against a specific org's vox
/// endpoint, cached per `(service type, slug)` so the multi-org
/// fetchers reuse one socket per org instead of re-dialing on every
/// render. This is what backs the org switcher's "All" mode (fan out
/// across slugs) and single-org mode alike.
#[cfg(target_arch = "wasm32")]
pub async fn establish_for<C>(slug: &str) -> Result<C, String>
where
    C: vox_core::FromVoxSession + Clone + 'static,
{
    use std::any::{Any, TypeId};
    use std::cell::RefCell;
    use std::collections::HashMap;
    use vox_core::{TransportMode, initiator_on};

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
    let url = org_ws_url(slug);
    if url.is_empty() {
        return Err("no vox URL configured (set TASK_VOX_URL_WEB)".to_owned());
    }
    let link = vox_websocket::WsLink::connect(&url)
        .await
        .map_err(|e| format!("ws connect `{url}`: {e:?}"))?;
    let client = initiator_on(link, TransportMode::Bare)
        .establish::<C>()
        .await
        .map_err(|e| format!("establish `{slug}`: {e:?}"))?;
    CACHE.with(|m| m.borrow_mut().insert(key, Box::new(client.clone())));
    Ok(client)
}

#[cfg(not(target_arch = "wasm32"))]
pub async fn establish_for<C>(_slug: &str) -> Result<C, String> {
    Err("native client not wired yet".to_owned())
}
