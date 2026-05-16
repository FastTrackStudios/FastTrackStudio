//! Tempo map streaming service trait.

use crate::ProjectContext;
use crate::tempo_map::event::TempoMapStreamEvent;
use vox::{Tx, service};

#[service]
pub trait TempoMapStream {
    async fn subscribe(&self, project: ProjectContext, tx: Tx<TempoMapStreamEvent>);
}

#[cfg(feature = "vox")]
#[derive(Debug, Default, Clone, Copy)]
pub struct Service;

#[cfg(feature = "vox")]
impl architect::BindAny for Service {
    fn descriptor(&self) -> &'static architect::vox::ServiceDescriptor {
        tempo_map_stream_service_descriptor()
    }
}

#[cfg(feature = "vox")]
impl<S> architect::Bind<S> for Service
where
    S: TempoMapStream + Clone + Send + Sync + 'static,
{
    fn bind_into(self, backend: &S, router: &mut architect::LayerRouter) {
        use architect::LayerSink as _;
        router.add_mounted(architect::Mounted::new(
            tempo_map_stream_service_descriptor(),
            TempoMapStreamDispatcher::new(backend.clone()),
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
        out.push(tempo_map_stream_service_descriptor());
    }
}

#[cfg(feature = "vox")]
pub fn layer<S>(backend: S) -> architect::Mounted
where
    S: TempoMapStream + Clone + Send + Sync + 'static,
{
    architect::Mounted::new(
        tempo_map_stream_service_descriptor(),
        TempoMapStreamDispatcher::new(backend),
    )
}
