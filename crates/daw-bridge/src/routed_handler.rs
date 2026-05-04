//! Handler composition and connection acceptance for vox.
//!
//! - `RoutedHandler`: routes incoming calls to the correct service dispatcher
//!   by matching `method_id` against each service's known methods.
//!
//! - `DawConnectionAcceptor`: a `ConnectionAcceptor` that runs a `Driver`
//!   with a `RoutedHandler` for each virtual connection.

use std::collections::HashMap;
use std::sync::Arc;
use tracing::{debug, info, warn};
use vox::facet;
use vox::{
    ConnectionAcceptor, ConnectionRequest, DriverReplySink, Handler, MetadataValue, MethodId,
    PendingConnection, ReplySink, RetryPolicy, SchemaRecvTracker, SelfRef, ServiceDescriptor,
    VoxError,
};

// ============================================================================
// RoutedHandler — method-ID-based dispatch
// ============================================================================

/// A handler entry wrapping a concrete dispatcher behind a trait object.
trait DynHandler: Send + Sync + 'static {
    fn retry_policy(&self, method_id: MethodId) -> RetryPolicy;

    fn args_have_channels(&self, method_id: MethodId) -> bool;

    fn response_wire_shape(&self, method_id: MethodId) -> Option<&'static facet::Shape>;

    fn handle(
        &self,
        call: SelfRef<vox::RequestCall<'static>>,
        reply: DriverReplySink,
        schemas: Arc<SchemaRecvTracker>,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = ()> + Send + '_>>;
}

/// Blanket impl: any `Handler<DriverReplySink>` can be wrapped.
impl<H: Handler<DriverReplySink>> DynHandler for H {
    fn retry_policy(&self, method_id: MethodId) -> RetryPolicy {
        Handler::retry_policy(self, method_id)
    }

    fn args_have_channels(&self, method_id: MethodId) -> bool {
        Handler::args_have_channels(self, method_id)
    }

    fn response_wire_shape(&self, method_id: MethodId) -> Option<&'static facet::Shape> {
        Handler::response_wire_shape(self, method_id)
    }

    fn handle(
        &self,
        call: SelfRef<vox::RequestCall<'static>>,
        reply: DriverReplySink,
        schemas: Arc<SchemaRecvTracker>,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = ()> + Send + '_>> {
        Box::pin(Handler::handle(self, call, reply, schemas))
    }
}

/// Routes incoming calls to the correct service dispatcher by method_id.
#[derive(Clone)]
pub struct RoutedHandler {
    /// method_id → index into `handlers`
    method_map: HashMap<MethodId, usize>,
    /// Concrete dispatchers, type-erased.
    handlers: Vec<Arc<dyn DynHandler>>,
}

impl RoutedHandler {
    pub fn new() -> Self {
        Self {
            method_map: HashMap::new(),
            handlers: Vec::new(),
        }
    }

    /// Register a service dispatcher with its known methods.
    pub fn with<H: Handler<DriverReplySink>>(
        mut self,
        descriptor: &ServiceDescriptor,
        handler: H,
    ) -> Self {
        let idx = self.handlers.len();
        self.handlers.push(Arc::new(handler));
        for method in descriptor.methods {
            self.method_map.insert(method.id, idx);
        }
        self
    }
}

impl Handler<DriverReplySink> for RoutedHandler {
    fn retry_policy(&self, method_id: MethodId) -> RetryPolicy {
        self.method_map
            .get(&method_id)
            .map(|&idx| self.handlers[idx].retry_policy(method_id))
            .unwrap_or(RetryPolicy::VOLATILE)
    }

    fn args_have_channels(&self, method_id: MethodId) -> bool {
        self.method_map
            .get(&method_id)
            .map(|&idx| self.handlers[idx].args_have_channels(method_id))
            .unwrap_or(false)
    }

    fn response_wire_shape(&self, method_id: MethodId) -> Option<&'static facet::Shape> {
        self.method_map
            .get(&method_id)
            .and_then(|&idx| self.handlers[idx].response_wire_shape(method_id))
    }

    async fn handle(
        &self,
        call: SelfRef<vox::RequestCall<'static>>,
        reply: DriverReplySink,
        schemas: Arc<SchemaRecvTracker>,
    ) {
        let method_id = call.get().method_id;
        if let Some(&idx) = self.method_map.get(&method_id) {
            debug!(?method_id, handler_index = idx, "routing vox call");
            self.handlers[idx].handle(call, reply, schemas).await;
        } else {
            warn!(?method_id, "unknown vox method");
            reply
                .send_error(VoxError::<core::convert::Infallible>::UnknownMethod)
                .await;
        }
    }
}

// ============================================================================
// DawConnectionAcceptor — virtual-connection-based service routing
// ============================================================================

/// Accepts inbound virtual connections and spawns a `Driver` with the
/// `RoutedHandler` for each one.
///
/// Clients open virtual connections with metadata to identify themselves.
/// Currently all connections get the full set of 16 DAW services.
/// Future: use metadata to restrict service sets per role.
#[derive(Clone)]
pub struct DawConnectionAcceptor {
    handler: Arc<RoutedHandler>,
}

impl DawConnectionAcceptor {
    pub fn new(handler: RoutedHandler) -> Self {
        Self {
            handler: Arc::new(handler),
        }
    }
}

impl ConnectionAcceptor for DawConnectionAcceptor {
    fn accept(
        &self,
        request: &ConnectionRequest,
        connection: PendingConnection,
    ) -> Result<(), vox::Metadata<'static>> {
        let role = request
            .metadata()
            .iter()
            .find(|e| e.key == "role")
            .and_then(|e| match &e.value {
                MetadataValue::String(s) => Some(s.as_ref()),
                _ => None,
            })
            .unwrap_or("unknown");

        info!("Accepting virtual connection: role={}", role);
        connection.handle_with(self.handler.as_ref().clone());
        Ok(())
    }
}
