//! Tracks streaming service trait. Mirrors `markers::stream`.

use crate::ProjectContext;
use crate::track::event::TrackStreamEvent;
use vox::{Tx, service};

#[service]
pub trait TracksStream {
    /// Subscribe to track add/remove/modify events across all open
    /// projects. Subscribers filter by `project_guid` on the envelope.
    async fn subscribe(&self, project: ProjectContext, tx: Tx<TrackStreamEvent>);
}

#[cfg(feature = "vox")]
#[derive(Debug, Default, Clone, Copy)]
pub struct Service;

#[cfg(feature = "vox")]
impl architect::BindAny for Service {
    fn descriptor(&self) -> &'static architect::vox::ServiceDescriptor {
        tracks_stream_service_descriptor()
    }
}

#[cfg(feature = "vox")]
impl<S> architect::Bind<S> for Service
where
    S: TracksStream + Clone + Send + Sync + 'static,
{
    fn bind_into(self, backend: &S, router: &mut architect::LayerRouter) {
        use architect::LayerSink as _;
        router.add_mounted(architect::Mounted::new(
            tracks_stream_service_descriptor(),
            TracksStreamDispatcher::new(backend.clone()),
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
        out.push(tracks_stream_service_descriptor());
    }
}

#[cfg(feature = "vox")]
pub fn layer<S>(backend: S) -> architect::Mounted
where
    S: TracksStream + Clone + Send + Sync + 'static,
{
    architect::Mounted::new(
        tracks_stream_service_descriptor(),
        TracksStreamDispatcher::new(backend),
    )
}
