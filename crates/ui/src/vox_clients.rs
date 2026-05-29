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

/// Open a WS link to the org endpoint.
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
