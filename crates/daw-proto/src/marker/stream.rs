//! Markers streaming service trait.
//!
//! Sibling of [`crate::marker::service::Markers`]. Pure-async,
//! `#[vox::service]`-decorated — bypasses the architect-rpc
//! sync-bridge.
//!
//! Occasional channel: every marker change (add / remove / modify /
//! bulk reload) is delivered. Backpressure handling: hub buffers 128
//! events; lagged subscribers see a `RecvError::Lagged` and a
//! warning, but keep going.

use crate::ProjectContext;
use crate::marker::event::MarkerStreamEvent;
use vox::{Tx, service};

#[service]
pub trait MarkersStream {
    /// Subscribe to marker changes across all open projects.
    /// Subscribers filter by `project_guid` on the envelope; this
    /// keeps the hub channel single, which simplifies the broadcaster
    /// and matches how helgobox's ProtoHub fans out occasional
    /// updates.
    ///
    /// The `project` argument is informational at the moment —
    /// subscribers receive events for every project. Per-project
    /// filtering happens client-side on the envelope's
    /// `project_guid`. We'll tighten this if a real use case wants
    /// server-side filtering.
    async fn subscribe(&self, project: ProjectContext, tx: Tx<MarkerStreamEvent>);
}

// ─ Service token + Bind<B> + composition impls ──────────────────────

#[cfg(feature = "vox")]
#[derive(Debug, Default, Clone, Copy)]
pub struct Service;

#[cfg(feature = "vox")]
impl architect::BindAny for Service {
    fn descriptor(&self) -> &'static architect::vox::ServiceDescriptor {
        markers_stream_service_descriptor()
    }
}

#[cfg(feature = "vox")]
impl<S> architect::Bind<S> for Service
where
    S: MarkersStream + Clone + Send + Sync + 'static,
{
    fn bind_into(self, backend: &S, router: &mut architect::LayerRouter) {
        use architect::LayerSink as _;
        router.add_mounted(architect::Mounted::new(
            markers_stream_service_descriptor(),
            MarkersStreamDispatcher::new(backend.clone()),
        ));
    }
}

#[cfg(feature = "vox")]
impl<R> architect::Append<R> for Service {
    type Output = architect::Cons<Service, R>;
    fn append(self, rhs: R) -> Self::Output {
        architect::Cons::new(self, rhs)
    }
}

#[cfg(feature = "vox")]
impl architect::Descriptors for Service {
    fn collect(&self, out: &mut ::std::vec::Vec<&'static architect::vox::ServiceDescriptor>) {
        out.push(markers_stream_service_descriptor());
    }
}

#[cfg(feature = "vox")]
pub fn layer<S>(backend: S) -> architect::Mounted
where
    S: MarkersStream + Clone + Send + Sync + 'static,
{
    architect::Mounted::new(
        markers_stream_service_descriptor(),
        MarkersStreamDispatcher::new(backend),
    )
}
