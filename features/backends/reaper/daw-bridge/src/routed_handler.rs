//! Bridge-side glue: just the [`DawConnectionAcceptor`] now.
//!
//! Method-ID dispatch was promoted into [`architect::LayerRouter`] —
//! `daw-bridge`'s old in-tree `RoutedHandler` had identical shape and was
//! retired alongside the architect-rpc port. Use
//! `daw_proto::service_set::daw_layers(backend).serve()` to build a
//! fully populated `LayerRouter` and pass it to [`DawConnectionAcceptor::new`].

use std::sync::Arc;
use tracing::info;
use vox::{ConnectionAcceptor, ConnectionRequest, MetadataValue, PendingConnection};

use architect::LayerRouter;

// ============================================================================
// DawConnectionAcceptor — virtual-connection-based service routing
// ============================================================================

/// Accepts inbound virtual connections and spawns a `Driver` with the
/// [`architect::LayerRouter`] for each one.
///
/// Clients open virtual connections with metadata identifying themselves.
/// All connections currently get the full DAW service set. Future:
/// metadata-driven sub-bundles for role-restricted views.
#[derive(Clone)]
pub struct DawConnectionAcceptor {
    handler: Arc<LayerRouter>,
}

impl DawConnectionAcceptor {
    pub fn new(handler: LayerRouter) -> Self {
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
