//! Handler composition for roam v7.
//!
//! In roam v7, the old `RoutedDispatcher` / `ServiceDispatcher` chaining
//! was removed. Each generated `*Dispatcher<H>` implements `Handler<R>`.
//! This module provides a `RoutedHandler` that routes incoming calls to
//! the correct dispatcher by matching `method_id` against each service's
//! known methods (from its `ServiceDescriptor`).
//!
//! ## Usage
//!
//! ```ignore
//! use routed_handler::RoutedHandler;
//!
//! let handler = RoutedHandler::new()
//!     .with(TransportService::service_descriptor(), TransportServiceDispatcher::new(transport))
//!     .with(ProjectService::service_descriptor(), ProjectServiceDispatcher::new(project));
//! ```

use roam::{DriverReplySink, Handler, MethodId, ReplySink, RoamError, SchemaRecvTracker, SelfRef, ServiceDescriptor};
use std::collections::HashMap;
use std::sync::Arc;

/// A handler entry wrapping a concrete dispatcher behind a trait object.
trait DynHandler: Send + Sync + 'static {
    fn handle(
        &self,
        call: SelfRef<roam::RequestCall<'static>>,
        reply: DriverReplySink,
        schemas: Arc<SchemaRecvTracker>,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = ()> + Send + '_>>;
}

/// Blanket impl: any `Handler<DriverReplySink>` can be wrapped.
impl<H: Handler<DriverReplySink>> DynHandler for H {
    fn handle(
        &self,
        call: SelfRef<roam::RequestCall<'static>>,
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
    async fn handle(&self, call: SelfRef<roam::RequestCall<'static>>, reply: DriverReplySink, schemas: Arc<SchemaRecvTracker>) {
        let method_id = call.method_id;
        if let Some(&idx) = self.method_map.get(&method_id) {
            self.handlers[idx].handle(call, reply, schemas).await;
        } else {
            reply
                .send_error(RoamError::<core::convert::Infallible>::UnknownMethod)
                .await;
        }
    }
}
