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
//! ## Liveness
//!
//! A cached root can die (server restart, socket drop). Every cache
//! access **validates** the root via `Caller::is_connected()` — vox's
//! session-liveness primitive, false the moment the session observes
//! transport EOF/error — and a dead entry is evicted + transparently
//! re-established. We validate per-access rather than keying entries by
//! the app `Connection`'s generation because this cache is *below* that
//! layer: multi-org fan-out (`feeds::*`) reaches it for orgs the app
//! connection isn't even pointed at, so the root's own liveness is the
//! only invariant that always applies. The generation still drives
//! hook-level invalidation upstream (`architect::Connection::generation`).

use crate::vox_session::vox_url;

/// Establish a client of type `C` against `url` — no caching. Shared by
/// every public helper; cross-target.
async fn establish_at<C>(url: &str) -> Result<C, String>
where
    C: vox_core::FromVoxLane + 'static,
{
    use vox_core::initiator_on;
    if url.is_empty() {
        return Err("no vox URL configured (set TASK_VOX_URL[_WEB])".to_owned());
    }
    #[cfg(target_arch = "wasm32")]
    let link = dial_ws(url).await?;
    #[cfg(not(target_arch = "wasm32"))]
    let link = vox_websocket::WsLink::connect(url)
        .await
        .map_err(|e| format!("ws connect `{url}`: {e:?}"))?;
    initiator_on(link)
        .establish::<C>()
        .await
        .map_err(|e| format!("establish `{url}`: {e:?}"))
}

/// Cancel-safe browser WebSocket dial (wasm replacement for
/// `vox_websocket::WsLink::connect`).
///
/// `WsLink::connect`'s dial phase is not cancel-safe: it attaches
/// `onopen`/`onerror` wasm-bindgen closures to the connecting socket and
/// only detaches them on the *success* path. On the error path — and,
/// worse, when the connect **future is dropped mid-dial** (the app-root
/// supervisor restarts the moment org discovery lands and its signal
/// dependency fires) — the closures drop while still attached to a
/// socket that hasn't finished failing. The browser then delivers the
/// socket's `error`/`close` event into the freed closure, surfacing as
/// an uncaught `closure invoked recursively or after being dropped`.
///
/// This dial keeps the connect-phase closures in a guard whose `Drop`
/// **detaches them from the socket first** (and closes a socket we're
/// abandoning), so no event can ever reach a dropped closure — drop
/// order inside one synchronous Rust drop can't be interleaved with
/// browser event dispatch. On success the guard detaches and hands the
/// open socket to `WsLink::new`, which installs the steady-state
/// handlers it owns.
#[cfg(target_arch = "wasm32")]
async fn dial_ws(url: &str) -> Result<vox_websocket::WsLink, String> {
    use std::cell::RefCell;
    use std::rc::Rc;

    use wasm_bindgen::JsCast;
    use wasm_bindgen::closure::Closure;

    /// Connect-phase state: the socket plus its temporary handlers.
    /// Detaches the handlers before the closure fields drop; closes the
    /// socket unless the dial completed and ownership moved to `WsLink`.
    struct Dial {
        ws: web_sys::WebSocket,
        _onopen: Closure<dyn FnMut()>,
        _onerror: Closure<dyn FnMut(web_sys::Event)>,
        _onclose: Closure<dyn FnMut(web_sys::CloseEvent)>,
        keep_open: bool,
    }
    impl Drop for Dial {
        fn drop(&mut self) {
            // Detach FIRST — after these lines the browser holds no
            // reference into the closures, so dropping them (field drop,
            // right after this body) is always safe.
            self.ws.set_onopen(None);
            self.ws.set_onerror(None);
            self.ws.set_onclose(None);
            if !self.keep_open {
                // Abandoned dial (error or caller cancellation): tear the
                // socket down so it doesn't keep connecting in the void.
                let _ = self.ws.close();
            }
        }
    }

    let ws = web_sys::WebSocket::new(url)
        .map_err(|e| format!("WebSocket::new `{url}`: {e:?}"))?;
    ws.set_binary_type(web_sys::BinaryType::Arraybuffer);

    let (tx, rx) = futures_channel::oneshot::channel::<Result<(), String>>();
    let tx = Rc::new(RefCell::new(Some(tx)));

    // FnMut (not `Closure::once`) so a stray double-fire can't trip
    // wasm-bindgen's invoked-after-consumed check; the oneshot's
    // take() makes later fires no-ops.
    let tx_open = Rc::clone(&tx);
    let onopen = Closure::wrap(Box::new(move || {
        if let Some(tx) = tx_open.borrow_mut().take() {
            let _ = tx.send(Ok(()));
        }
    }) as Box<dyn FnMut()>);
    let tx_error = Rc::clone(&tx);
    let err_url = url.to_owned();
    let onerror = Closure::wrap(Box::new(move |_: web_sys::Event| {
        if let Some(tx) = tx_error.borrow_mut().take() {
            let _ = tx.send(Err(format!("WebSocket open failed: `{err_url}`")));
        }
    }) as Box<dyn FnMut(web_sys::Event)>);
    // `close` can arrive without a preceding `error` (clean rejection);
    // without this handler such a dial would hang forever.
    let tx_close = Rc::clone(&tx);
    let close_url = url.to_owned();
    let onclose = Closure::wrap(Box::new(move |e: web_sys::CloseEvent| {
        if let Some(tx) = tx_close.borrow_mut().take() {
            let _ = tx.send(Err(format!(
                "WebSocket closed during open: `{close_url}` (code {})",
                e.code()
            )));
        }
    }) as Box<dyn FnMut(web_sys::CloseEvent)>);

    ws.set_onopen(Some(onopen.as_ref().unchecked_ref()));
    ws.set_onerror(Some(onerror.as_ref().unchecked_ref()));
    ws.set_onclose(Some(onclose.as_ref().unchecked_ref()));

    let mut dial = Dial {
        ws,
        _onopen: onopen,
        _onerror: onerror,
        _onclose: onclose,
        keep_open: false,
    };

    // Cancellation-safe await: dropping this future drops `dial`, whose
    // Drop detaches the handlers and closes the half-open socket.
    rx.await.map_err(|_| "dial cancelled".to_owned())??;

    // Success: keep the socket, detach the connect-phase handlers (the
    // guard's Drop), and let WsLink install its own.
    dial.keep_open = true;
    let ws = dial.ws.clone();
    drop(dial);
    Ok(vox_websocket::WsLink::new(ws))
}

/// Untyped root lane — retains the raw [`vox_core::Caller`] plus the
/// [`vox_core::ConnectionHandle`] (dropping the handle tears the
/// connection down). The rc-fleet replacement for the old fork's
/// `NoopClient`: typed clients are cheap views built from the caller.
#[derive(Clone)]
pub struct RootLane {
    /// The established lane's caller.
    pub caller: vox_core::Caller,
    /// Underlying connection — kept alive with the cache entry.
    _connection: Option<vox_core::ConnectionHandle>,
}

impl vox_core::FromVoxLane for RootLane {
    const SERVICE_NAME: &'static str = "Noop";

    fn from_vox_lane(
        caller: vox_core::Caller,
        connection: Option<vox_core::ConnectionHandle>,
    ) -> Self {
        Self { caller, _connection: connection }
    }
}

/// Per-org vox endpoint: the configured base retargeted at
/// `/org/<slug>/vox`.
///
/// Errors on an **empty slug** rather than producing `/org//vox` — this
/// is the choke point every org client funnels through, so no caller
/// can dial before org discovery has resolved a real slug (the app-root
/// supervisor and several hooks run with `home_slug` == "" until the
/// well-known fetch lands; previously that raced into a doomed
/// WebSocket to `/org//vox` plus a console error). Callers just retry /
/// re-run when the org-list signal fires.
fn org_ws_url(slug: &str) -> Result<String, String> {
    if slug.is_empty() {
        return Err("awaiting org discovery (no org slug yet)".to_owned());
    }
    let base = vox_url();
    if base.is_empty() {
        return Err("no vox URL configured (set TASK_VOX_URL[_WEB])".to_owned());
    }
    let trimmed = base.trim_end_matches("/vox").trim_end_matches('/');
    Ok(format!("{trimmed}/org/{slug}/vox"))
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
        // holds the root while it lives. Typed clients built from the
        // caller are virtual views (no session handle).
        thread_local! {
            static ROOTS: RefCell<HashMap<String, RootLane>> =
                RefCell::new(HashMap::new());
        }
        // Validate on access: a dead root (server restart, socket drop)
        // is evicted so the next lines re-establish instead of handing
        // out a corpse forever. See the module docs ("Liveness") for why
        // this is a per-access check rather than generation keying.
        if let Some(caller) = ROOTS.with(|m| {
            let mut m = m.borrow_mut();
            match m.get(slug) {
                Some(root) if root.caller.is_connected() => Some(root.caller.clone()),
                Some(_) => {
                    tracing::warn!(slug, "vox: cached org root is dead; re-establishing");
                    m.remove(slug);
                    None
                }
                None => None,
            }
        }) {
            return Ok(caller);
        }
        let root = establish_at::<RootLane>(&org_ws_url(slug)?).await?;
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
        let root = establish_at::<RootLane>(&org_ws_url(slug)?).await?;
        Ok(root.caller.clone())
    }
}

/// Establish *any* service client against a specific org's vox endpoint.
/// Wasm: a cheap typed view over the org's one cached connection
/// ([`caller_for`]) — previously this cached a socket per
/// `(service, org)`. Native: per-call establish, as before.
pub async fn establish_for<C>(slug: &str) -> Result<C, String>
where
    C: vox_core::FromVoxLane + Clone + 'static,
{
    #[cfg(target_arch = "wasm32")]
    {
        let caller = caller_for(slug).await?;
        Ok(C::from_vox_lane(caller, None))
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        establish_at::<C>(&org_ws_url(slug)?).await
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
