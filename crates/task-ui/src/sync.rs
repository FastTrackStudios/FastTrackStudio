//! Loro ↔ WebSocket sync glue. Shared by every per-feature route.
//!
//! Owns one `LoroDoc` and one `WebSocket`. Wires the four pieces the
//! Loro authors call out as the canonical pattern:
//!
//! - **Server → client (on connect)**: first frame is a Snapshot. We
//!   `doc.import(&bytes)` it to catch up.
//! - **Client → server (after every commit)**: `subscribe_local_update`
//!   hands us bytes; we send them as a binary frame.
//! - **Server → client (live)**: subsequent binary frames are updates
//!   from other peers; `doc.import(&bytes)` and Loro's CRDT merge
//!   handles the conflict resolution.
//! - **Imports do NOT trigger `subscribe_local_update`** — no echo
//!   loop, no special filtering needed.
//!
//! Separately we register `doc.subscribe_root` to know *when* state
//! changed so the UI re-renders. That callback fires on both local
//! and remote changes.

/// Emit a single line into the browser devtools console. The
/// `tracing` crate is no-op on wasm without a subscriber wired up,
/// and the feature routes are small enough that a one-liner per
/// log site is fine. Set RUST_LOG-style filtering aside until we
/// have a real reason to.
#[macro_export]
macro_rules! ui_log {
    ($($arg:tt)*) => {
        ::web_sys::console::log_1(&::wasm_bindgen::JsValue::from_str(&format!($($arg)*)))
    };
}

use std::sync::Arc;

use crdt::CrdtDoc;
use js_sys::Uint8Array;
use loro::Subscription;
use wasm_bindgen::closure::Closure;
use wasm_bindgen::{JsCast, JsValue};
use web_sys::{BinaryType, MessageEvent, WebSocket};

/// All the JS handles we have to keep alive for the WebSocket
/// session — drops here would cause the closures to detach and
/// messages to silently disappear.
pub struct SyncSession {
    /// The browser WebSocket. We send local updates through it.
    ws: WebSocket,
    /// Loro subscription that forwards outbound updates over `ws`.
    /// Dropping it unsubscribes (so old sessions stop sending).
    _local_update_sub: Subscription,
    /// Loro subscription that fires for every doc change (local +
    /// remote) so the caller can re-render the UI.
    _root_sub: Subscription,
    /// Closures held alive for the lifetime of the WebSocket. JS
    /// would garbage-collect them otherwise.
    _on_message: Closure<dyn FnMut(MessageEvent)>,
    _on_open: Closure<dyn FnMut(JsValue)>,
    _on_error: Closure<dyn FnMut(JsValue)>,
}

impl Drop for SyncSession {
    fn drop(&mut self) {
        // Best-effort close; ignore errors (e.g. socket already
        // disconnected). The Subscription drops above this point and
        // the closures get freed after we return.
        let _ = self.ws.close();
    }
}

/// Open a WebSocket against `ws_url`, wire it to `doc`, and start
/// pumping bytes in both directions. The returned handle keeps
/// everything alive — drop it to disconnect.
///
/// `on_change` fires every time the doc's state mutates, from either
/// a local Repo write or an imported remote update. Use it to set a
/// Dioxus signal so the UI re-reads from the Repo.
pub fn connect(
    ws_url: &str,
    doc: &CrdtDoc,
    on_change: impl Fn() + Send + Sync + 'static,
) -> Result<SyncSession, JsValue> {
    crate::ui_log!("sync::connect: opening {ws_url}");
    let ws = match WebSocket::new(ws_url) {
        Ok(w) => {
            crate::ui_log!("sync::connect: WebSocket::new ok");
            w
        }
        Err(e) => {
            crate::ui_log!("sync::connect: WebSocket::new FAILED: {e:?}");
            return Err(e);
        }
    };
    ws.set_binary_type(BinaryType::Arraybuffer);
    crate::ui_log!("sync::connect: binary type set, wiring subscriptions");

    // ── Outbound: every local commit produces bytes; send them.
    //
    // Wrap the WS in an Rc<RefCell> so the closure can clone the
    // handle. The Loro callback is sync and `Send + Sync`-free, so
    // single-threaded interior mutability is enough.
    let ws_for_local = ws.clone();
    let loro = doc.loro();
    crate::ui_log!("sync::connect: about to call subscribe_local_update");
    let local_update_sub = loro.subscribe_local_update(Box::new(move |bytes| {
        // Loro hands us `&[u8]`; `WebSocket::send_with_u8_array`
        // takes the same slice. Errors here just mean the socket is
        // closed — let Drop clean up.
        let _ = ws_for_local.send_with_u8_array(bytes);
        true
    }));

    // ── UI re-render trigger: any doc state change.
    //
    // Loro's `subscribe_root` requires `Arc<dyn Fn(_) + Send + Sync>`
    // — so the caller's closure must be `Send + Sync` too. Dioxus
    // signals satisfy that, so the typical pattern works.
    crate::ui_log!("sync::connect: subscribe_local_update done, about to subscribe_root");
    let on_change_rc: Arc<dyn Fn() + Send + Sync> = Arc::new(on_change);
    let on_change_for_root = on_change_rc.clone();
    let root_sub = loro.subscribe_root(Arc::new(move |_diff| {
        on_change_for_root();
    }));
    crate::ui_log!("sync::connect: subscribe_root done, wiring onmessage/onopen/onerror");

    // ── Inbound: import the bytes the server sends.
    let doc_for_msg = doc.clone();
    let on_change_for_msg = on_change_rc.clone();
    let on_message = Closure::wrap(Box::new(move |evt: MessageEvent| {
        let buf = match evt.data().dyn_into::<js_sys::ArrayBuffer>() {
            Ok(b) => b,
            Err(_) => return, // ignore non-binary frames
        };
        let array = Uint8Array::new(&buf);
        let mut bytes = vec![0u8; array.length() as usize];
        array.copy_to(&mut bytes);
        if let Err(e) = doc_for_msg.apply_remote(&bytes) {
            web_sys::console::error_2(
                &"sync: apply_remote failed".into(),
                &format!("{e}").into(),
            );
            return;
        }
        // Importing a remote update doesn't trigger
        // subscribe_local_update OR subscribe_root reliably across
        // versions — nudge the UI re-render explicitly so we don't
        // depend on Loro's event semantics in detail.
        on_change_for_msg();
    }) as Box<dyn FnMut(MessageEvent)>);
    ws.set_onmessage(Some(on_message.as_ref().unchecked_ref()));

    let on_open = Closure::wrap(Box::new(move |_: JsValue| {
        web_sys::console::log_1(&"sync: connected".into());
    }) as Box<dyn FnMut(JsValue)>);
    ws.set_onopen(Some(on_open.as_ref().unchecked_ref()));

    let on_error = Closure::wrap(Box::new(move |_: JsValue| {
        web_sys::console::error_1(&"sync: ws error".into());
    }) as Box<dyn FnMut(JsValue)>);
    ws.set_onerror(Some(on_error.as_ref().unchecked_ref()));

    crate::ui_log!("sync::connect: all wiring done, returning SyncSession");
    Ok(SyncSession {
        ws,
        _local_update_sub: local_update_sub,
        _root_sub: root_sub,
        _on_message: on_message,
        _on_open: on_open,
        _on_error: on_error,
    })
}

/// Compute the WebSocket URL for the demo sync server. Hardcoded to
/// localhost:9090 — production would derive from window.location.
pub fn sync_url(path: &str) -> String {
    format!("ws://127.0.0.1:9090{path}")
}

/// Shared workspace doc id — every feature route hits the same room
/// on the sync server, so all peers' edits across all features land
/// on the same authoritative server doc. Change to scope a route to
/// a different room.
pub const WORKSPACE_DOC_ID: uuid::Uuid = uuid::uuid!("00000000-0000-0000-0000-000000000001");
