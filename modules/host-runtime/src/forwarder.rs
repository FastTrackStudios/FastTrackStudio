//! Multi-target forwarder that can forward RPC calls to multiple cells.
//!
//! This module provides dispatchers for routing RPC calls to different cells
//! based on method IDs. The key pattern is:
//!
//! - `MethodRoutedForwarder`: Routes calls to specific cells based on method ID.
//!   Each target cell registers its method IDs, enabling deterministic routing.
//!
//! - `RebindableHandle`: A handle that can be rebound after cell hot-reload.
//!   Unlike `LateBoundHandle`, this can be updated multiple times.
//!
//! - `MultiForwarder`: Legacy catch-all forwarder (deprecated - use MethodRoutedForwarder).

use roam::session::{
    dispatch_unknown_method, ChannelRegistry, ConnectionHandle, Context, ForwardingDispatcher,
    ServiceDispatcher,
};
use std::collections::HashMap;
use std::future::Future;
use std::pin::Pin;
use std::sync::{Arc, RwLock};
use tracing::debug;

// ============================================================================
// RebindableHandle - Hot-Reload Compatible Handle
// ============================================================================

/// A handle that can be rebound multiple times (for hot-reload support).
///
/// Unlike `LateBoundHandle` which uses `OnceLock` and can only be set once,
/// `RebindableHandle` uses `RwLock` and can be updated when a cell is reloaded.
///
/// # Example
///
/// ```ignore
/// let handle = RebindableHandle::new();
///
/// // Initially unbound - calls will return Cancelled
/// handle.set(connection1);  // Bind to first connection
/// handle.set(connection2);  // Rebind after hot-reload (no panic!)
/// ```
#[derive(Clone)]
pub struct RebindableHandle {
    inner: Arc<RwLock<Option<ConnectionHandle>>>,
}

impl RebindableHandle {
    /// Create a new unbound handle.
    pub fn new() -> Self {
        Self {
            inner: Arc::new(RwLock::new(None)),
        }
    }

    /// Bind or rebind the handle to a connection.
    ///
    /// Can be called multiple times (unlike `LateBoundHandle`).
    pub fn set(&self, handle: ConnectionHandle) {
        if let Ok(mut guard) = self.inner.write() {
            *guard = Some(handle);
        }
    }

    /// Clear the handle (unbind).
    ///
    /// After calling this, `get()` will return `None` until `set()` is called again.
    pub fn clear(&self) {
        if let Ok(mut guard) = self.inner.write() {
            *guard = None;
        }
    }

    /// Try to get the bound handle, if set.
    pub fn get(&self) -> Option<ConnectionHandle> {
        self.inner.read().ok().and_then(|guard| guard.clone())
    }

    /// Check if the handle is currently bound.
    pub fn is_bound(&self) -> bool {
        self.inner
            .read()
            .ok()
            .map(|guard| guard.is_some())
            .unwrap_or(false)
    }
}

impl Default for RebindableHandle {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// RebindableForwarder - Forwarder using RebindableHandle
// ============================================================================

/// A forwarder that uses `RebindableHandle` for hot-reload support.
///
/// This is similar to `LateBoundForwarder` but supports rebinding after a cell
/// is hot-reloaded.
#[derive(Clone)]
pub struct RebindableForwarder {
    handle: RebindableHandle,
}

impl RebindableForwarder {
    /// Create a new forwarder with the given rebindable handle.
    pub fn new(handle: RebindableHandle) -> Self {
        Self { handle }
    }
}

impl ServiceDispatcher for RebindableForwarder {
    fn method_ids(&self) -> Vec<u64> {
        // Forwarder handles all methods transparently
        vec![]
    }

    fn dispatch(
        &self,
        cx: Context,
        payload: Vec<u8>,
        registry: &mut ChannelRegistry,
    ) -> Pin<Box<dyn Future<Output = ()> + Send + 'static>> {
        if let Some(handle) = self.handle.get() {
            // Delegate to ForwardingDispatcher now that we have the handle
            ForwardingDispatcher::new(handle).dispatch(cx, payload, registry)
        } else {
            // Handle not bound yet - return cancelled
            debug!(
                method_id = cx.method_id().raw(),
                "RebindableForwarder: handle not bound, returning cancelled"
            );
            dispatch_unknown_method(&cx, registry)
        }
    }
}

// ============================================================================
// MethodRoutedForwarder - Routes by Method ID
// ============================================================================

/// A dispatcher that routes calls to specific cells based on method ID.
///
/// This is the proper roam pattern for multi-cell forwarding. Each target cell
/// registers its method IDs, and incoming calls are routed to the correct cell.
///
/// Unlike `MultiForwarder` which blindly forwards to the first target,
/// `MethodRoutedForwarder` uses explicit routing based on the method being called.
///
/// Uses `RebindableHandle` internally to support cell hot-reload.
///
/// # Example
///
/// ```ignore
/// let mut forwarder = MethodRoutedForwarder::new();
///
/// // Register DAW cell methods
/// forwarder.add_target(
///     daw_handle,
///     TransportServiceDispatcher::<()>::method_ids()
///         .into_iter()
///         .chain(ProjectServiceDispatcher::<()>::method_ids())
///         .collect(),
/// );
///
/// // Register Session cell methods
/// forwarder.add_target(
///     session_handle,
///     SetlistServiceDispatcher::<()>::method_ids()
///         .into_iter()
///         .chain(SessionServiceDispatcher::<()>::method_ids())
///         .collect(),
/// );
/// ```
pub struct MethodRoutedForwarder {
    /// Map from method ID to the forwarder that handles it
    routes: HashMap<u64, RebindableForwarder>,
    /// All method IDs we handle (for ServiceDispatcher::method_ids)
    all_method_ids: Vec<u64>,
}

impl MethodRoutedForwarder {
    /// Create a new empty method-routed forwarder.
    pub fn new() -> Self {
        Self {
            routes: HashMap::new(),
            all_method_ids: Vec::new(),
        }
    }

    /// Add a target cell with the specified method IDs.
    ///
    /// All calls to any of the specified method IDs will be forwarded to this target.
    pub fn add_target(&mut self, handle: RebindableHandle, method_ids: Vec<u64>) {
        let forwarder = RebindableForwarder::new(handle);
        for method_id in method_ids {
            self.routes.insert(method_id, forwarder.clone());
            self.all_method_ids.push(method_id);
        }
    }

    /// Check if this forwarder has any targets.
    pub fn is_empty(&self) -> bool {
        self.routes.is_empty()
    }
}

impl Default for MethodRoutedForwarder {
    fn default() -> Self {
        Self::new()
    }
}

impl ServiceDispatcher for MethodRoutedForwarder {
    fn method_ids(&self) -> Vec<u64> {
        self.all_method_ids.clone()
    }

    fn dispatch(
        &self,
        cx: Context,
        payload: Vec<u8>,
        registry: &mut ChannelRegistry,
    ) -> Pin<Box<dyn Future<Output = ()> + Send + 'static>> {
        let method_id = cx.method_id().raw();

        // Route to the forwarder registered for this method ID
        if let Some(forwarder) = self.routes.get(&method_id) {
            tracing::trace!(
                method_id,
                "MethodRoutedForwarder: routing call to registered target"
            );
            return forwarder.dispatch(cx, payload, registry);
        }

        // Unknown method - no forwarder registered for this method ID
        tracing::warn!(
            method_id,
            known_methods = ?self.all_method_ids,
            "MethodRoutedForwarder: unknown method, no route found"
        );
        dispatch_unknown_method(&cx, registry)
    }
}

// ============================================================================
// MultiForwarder - Legacy Catch-All (Deprecated)
// ============================================================================

/// A dispatcher that forwards to multiple rebindable targets.
///
/// **DEPRECATED**: Use `MethodRoutedForwarder` instead for explicit method routing.
///
/// This dispatcher forwards all calls to the first available target, which doesn't
/// work correctly when different targets handle different services. It's kept for
/// backwards compatibility but should not be used for new code.
pub struct MultiForwarder {
    forwarders: Vec<RebindableForwarder>,
}

impl MultiForwarder {
    /// Create a new multi-forwarder from a list of rebindable handles.
    pub fn new(handles: Vec<RebindableHandle>) -> Self {
        Self {
            forwarders: handles.into_iter().map(RebindableForwarder::new).collect(),
        }
    }

    /// Create an empty multi-forwarder (no targets).
    pub fn empty() -> Self {
        Self {
            forwarders: Vec::new(),
        }
    }

    /// Check if this forwarder has any targets.
    pub fn is_empty(&self) -> bool {
        self.forwarders.is_empty()
    }
}

impl ServiceDispatcher for MultiForwarder {
    fn method_ids(&self) -> Vec<u64> {
        // MultiForwarder acts as a catch-all fallback - it doesn't advertise specific methods
        // because RebindableForwarder forwards ALL methods transparently.
        // Returning empty here means RoutedDispatcher will use us as a fallback.
        vec![]
    }

    fn dispatch(
        &self,
        cx: Context,
        payload: Vec<u8>,
        registry: &mut ChannelRegistry,
    ) -> Pin<Box<dyn Future<Output = ()> + Send + 'static>> {
        // Forward to the first available forwarder.
        // RebindableForwarder handles both the "bound" and "not bound" cases gracefully.
        // We try each forwarder in order - the first bound one will handle the call.
        // If none are bound, the first one returns Cancelled.
        if let Some(forwarder) = self.forwarders.first() {
            return forwarder.dispatch(cx, payload, registry);
        }

        // No forwarders at all - return unknown method
        dispatch_unknown_method(&cx, registry)
    }
}

/// A wrapper that implements ServiceDispatcher for Arc<dyn ServiceDispatcher>.
///
/// This allows storing a dispatcher in an Arc while still being able to use it
/// as a ServiceDispatcher in routed dispatcher chains.
#[derive(Clone)]
pub struct ArcDispatcher {
    inner: Arc<dyn ServiceDispatcher>,
}

impl ArcDispatcher {
    /// Create a new ArcDispatcher wrapping the given dispatcher.
    pub fn new(dispatcher: Arc<dyn ServiceDispatcher>) -> Self {
        Self { inner: dispatcher }
    }
}

impl ServiceDispatcher for ArcDispatcher {
    fn method_ids(&self) -> Vec<u64> {
        self.inner.method_ids()
    }

    fn dispatch(
        &self,
        cx: Context,
        payload: Vec<u8>,
        registry: &mut ChannelRegistry,
    ) -> Pin<Box<dyn Future<Output = ()> + Send + 'static>> {
        self.inner.dispatch(cx, payload, registry)
    }
}
