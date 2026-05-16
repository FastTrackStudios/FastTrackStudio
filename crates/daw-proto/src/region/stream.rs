//! Regions streaming service trait. Mirrors `markers::stream`.

use crate::ProjectContext;
use crate::region::event::RegionStreamEvent;
use vox::{Tx, service};

#[service]
pub trait RegionsStream {
    /// Subscribe to region changes across all open projects.
    /// Subscribers filter by `project_guid` on the envelope.
    async fn subscribe(&self, project: ProjectContext, tx: Tx<RegionStreamEvent>);
}

// ─ Service token + composition impls ────────────────────────────────

#[cfg(feature = "vox")]
#[derive(Debug, Default, Clone, Copy)]
pub struct Service;

#[cfg(feature = "vox")]
impl architect::BindAny for Service {
    fn descriptor(&self) -> &'static architect::vox::ServiceDescriptor {
        regions_stream_service_descriptor()
    }
}

#[cfg(feature = "vox")]
impl<S> architect::Bind<S> for Service
where
    S: RegionsStream + Clone + Send + Sync + 'static,
{
    fn bind_into(self, backend: &S, router: &mut architect::LayerRouter) {
        use architect::LayerSink as _;
        router.add_mounted(architect::Mounted::new(
            regions_stream_service_descriptor(),
            RegionsStreamDispatcher::new(backend.clone()),
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
        out.push(regions_stream_service_descriptor());
    }
}

#[cfg(feature = "vox")]
pub fn layer<S>(backend: S) -> architect::Mounted
where
    S: RegionsStream + Clone + Send + Sync + 'static,
{
    architect::Mounted::new(
        regions_stream_service_descriptor(),
        RegionsStreamDispatcher::new(backend),
    )
}
