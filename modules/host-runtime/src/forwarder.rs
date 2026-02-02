//! Multi-target forwarder that can forward RPC calls to multiple cells.
//!
//! This allows a cell to call methods on any of its configured target cells.

use roam::session::{
    ChannelRegistry, Context, LateBoundForwarder, LateBoundHandle, ServiceDispatcher,
};
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

/// A dispatcher that forwards to multiple late-bound targets.
///
/// When dispatch is called, it tries each forwarder in order until one handles the call.
/// This allows a cell to call methods on any of its configured target cells.
pub struct MultiForwarder {
    forwarders: Vec<LateBoundForwarder>,
}

impl MultiForwarder {
    /// Create a new multi-forwarder from a list of late-bound handles.
    pub fn new(handles: Vec<LateBoundHandle>) -> Self {
        Self {
            forwarders: handles.into_iter().map(LateBoundForwarder::new).collect(),
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
        // Collect method IDs from all forwarders
        self.forwarders
            .iter()
            .flat_map(|f| f.method_ids())
            .collect()
    }

    fn dispatch(
        &self,
        cx: Context,
        payload: Vec<u8>,
        registry: &mut ChannelRegistry,
    ) -> Pin<Box<dyn Future<Output = ()> + Send + 'static>> {
        // Try each forwarder - the first one that's bound will handle it
        // Note: LateBoundForwarder handles unbound case gracefully
        for forwarder in &self.forwarders {
            // Check if this forwarder might handle this call
            // LateBoundForwarder forwards everything when bound
            if !forwarder.method_ids().is_empty() {
                return forwarder.dispatch(cx, payload, registry);
            }
        }

        // If no forwarders are ready, try the first one anyway
        // (it will handle the "not bound yet" case)
        if let Some(forwarder) = self.forwarders.first() {
            return forwarder.dispatch(cx, payload, registry);
        }

        // No forwarders at all - return empty future
        Box::pin(async {})
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
