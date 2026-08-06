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
//! **Native shares the same cache** (2026-08-06). It used to establish
//! per call — no dropped-closure hazard, so it looked harmless — but
//! "per call" means one WebSocket per typed client, and a single desktop
//! page load touches dozens of services. Production traces caught it:
//! one load opened **72 sockets** to `/org/{slug}/vox` where the browser
//! opens one, which is both a slow load (concurrent-connection limits)
//! and 72 handshakes of server work per page.
//!
//! Both targets now resolve an endpoint to ONE cached root connection and
//! build typed clients as cheap views over its caller. The server's
//! per-org `LayerRouter` was always able to dispatch every service on one
//! connection; only the client was fanning out.
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

    let ws = web_sys::WebSocket::new(url).map_err(|e| format!("WebSocket::new `{url}`: {e:?}"))?;
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
        Self {
            caller,
            _connection: connection,
        }
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

/// Server-level vox endpoint (`/server/vox`) — the process-wide surface
/// (identity locker, etc.), NOT a per-org one. Normalizes any active
/// base — `wss://host/vox`, bare `wss://host`, or a per-org
/// `wss://host/org/<slug>/vox` — down to `wss://host/server/vox` by
/// keeping only the scheme + authority.
fn server_ws_url(base_override: Option<&str>) -> Result<String, String> {
    let base = base_override.map(str::to_owned).unwrap_or_else(vox_url);
    let base = base.trim();
    if base.is_empty() {
        return Err("no vox URL configured (set TASK_VOX_URL[_WEB])".to_owned());
    }
    let (scheme, rest) = base.split_once("://").unwrap_or(("wss", base));
    let host = rest.split('/').next().unwrap_or(rest);
    Ok(format!("{scheme}://{host}/server/vox"))
}

/// Establish *any* service client against the server-level `/server/vox`
/// endpoint (see [`server_ws_url`]). Cross-target — `establish_at`
/// handles the wasm vs native transport. Used for the identity locker,
/// which is mounted per server-process, not per org.
pub async fn establish_server<C>(base_override: Option<&str>) -> Result<C, String>
where
    C: vox_core::FromVoxLane + Clone + 'static,
{
    let caller = shared_caller_at(&server_ws_url(base_override)?).await?;
    Ok(C::from_vox_lane(caller, None))
}

/// The shared root connection for one endpoint URL — establish once,
/// reuse for every service.
///
/// This is the single choke point that makes "one socket per endpoint"
/// true. Keyed by the **full URL**, not a slug: the same slug on two
/// servers (multi-server registry) must be two independent sockets, and
/// switching the active server must not hand back the previous server's
/// root.
///
/// The cache owns the [`RootLane`], which owns the
/// [`vox_core::ConnectionHandle`] — that ownership is what holds the
/// socket open, since typed clients built from the caller are views with
/// no session handle of their own.
///
/// Entries are validated on access via `Caller::is_connected()` rather
/// than keyed by the app `Connection`'s generation: this cache sits
/// *below* that layer (multi-org fan-out reaches it for orgs the app
/// connection isn't pointed at), so the root's own liveness is the only
/// invariant that always holds. A dead root is evicted and re-established
/// transparently.
async fn shared_caller_at(url: &str) -> Result<vox_core::Caller, String> {
    if let Some(caller) = cached_live_caller(url) {
        return Ok(caller);
    }
    let root = establish_at::<RootLane>(url).await?;
    Ok(insert_root(url, root))
}

/// Look up a cached root, evicting it if the connection has died.
fn cached_live_caller(url: &str) -> Option<vox_core::Caller> {
    with_roots(|roots| match roots.get(url) {
        Some(root) if root.caller.is_connected() => Some(root.caller.clone()),
        Some(_) => {
            tracing::warn!(url, "vox: cached root is dead; re-establishing");
            roots.remove(url);
            None
        }
        None => None,
    })
}

/// Publish a freshly established root, resolving the dial race.
///
/// Two tasks can miss the cache and dial the same URL concurrently (the
/// lock is deliberately not held across the dial — a network round trip
/// under a global lock would serialize every org's first connect). The
/// loser drops its socket rather than evicting the winner, so callers
/// that already hold the winner's caller keep a live connection.
fn insert_root(url: &str, root: RootLane) -> vox_core::Caller {
    with_roots(|roots| match roots.get(url) {
        Some(existing) if existing.caller.is_connected() => existing.caller.clone(),
        _ => {
            let caller = root.caller.clone();
            roots.insert(url.to_owned(), root);
            caller
        }
    })
}

/// Run `f` against the per-target root cache.
///
/// wasm is single-threaded, so a `thread_local` is the whole story.
/// Native needs a process-global map because the desktop app establishes
/// from whatever runtime thread happens to poll the future. The lock is
/// never held across an await (see [`insert_root`]), so a plain `std`
/// mutex is enough and this needs no async-lock dependency.
#[cfg(target_arch = "wasm32")]
fn with_roots<R>(f: impl FnOnce(&mut std::collections::HashMap<String, RootLane>) -> R) -> R {
    use std::cell::RefCell;
    use std::collections::HashMap;
    thread_local! {
        static ROOTS: RefCell<HashMap<String, RootLane>> = RefCell::new(HashMap::new());
    }
    ROOTS.with(|roots| f(&mut roots.borrow_mut()))
}

#[cfg(not(target_arch = "wasm32"))]
fn with_roots<R>(f: impl FnOnce(&mut std::collections::HashMap<String, RootLane>) -> R) -> R {
    use std::collections::HashMap;
    use std::sync::{Mutex, OnceLock};
    static ROOTS: OnceLock<Mutex<HashMap<String, RootLane>>> = OnceLock::new();
    let roots = ROOTS.get_or_init(|| Mutex::new(HashMap::new()));
    // Poisoning only means some other caller panicked mid-map-edit; the
    // map itself is still a valid cache, so recover rather than cascade.
    let mut guard = roots.lock().unwrap_or_else(|e| e.into_inner());
    f(&mut guard)
}

/// One shared [`vox_core::Caller`] per org — the handle every typed
/// client is built from. One socket per org on both targets, no matter
/// how many services a page touches: the server's per-org `LayerRouter`
/// dispatches every service on that one connection.
///
/// This also backs the app root's `Connection<Caller>`
/// (`architect::use_app_reactive` over the active org) — pages that
/// migrate to atom hooks build clients from the shared caller; legacy
/// `feeds::*` fns ride the same socket through [`establish_for`].
pub async fn caller_for(slug: &str) -> Result<vox_core::Caller, String> {
    shared_caller_at(&org_ws_url(slug)?).await
}

/// Establish *any* service client against a specific org's vox endpoint:
/// a cheap typed view over the org's ONE cached connection
/// ([`caller_for`]). Identical on both targets.
pub async fn establish_for<C>(slug: &str) -> Result<C, String>
where
    C: vox_core::FromVoxLane + Clone + 'static,
{
    let caller = caller_for(slug).await?;
    Ok(C::from_vox_lane(caller, None))
}
