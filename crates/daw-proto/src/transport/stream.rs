//! Transport streaming service trait.
//!
//! Sibling of [`crate::transport::service::Transport`] (the
//! sync-method RPC trait). Pure-async — methods take `Tx<T>` and
//! return `()`, so they bypass the `#[architect::rpc]` sync-bridge
//! and use vox's native streaming surface directly.
//!
//! Two streams per project:
//!
//! - [`TransportStream::subscribe_state`] — occasional channel, every
//!   transition delivered.
//! - [`TransportStream::subscribe_position`] — continuous channel,
//!   ~30Hz, drop-old policy on backpressure.
//!
//! See `docs/streaming-design.md` for the broader architecture.

use crate::ProjectContext;
use crate::transport::event::{PositionTick, TransportEvent};
use vox::{Tx, service};

#[service]
pub trait TransportStream {
    /// Subscribe to discrete transport state changes (play, stop,
    /// record, tempo, time signature, loop region edits) for a given
    /// project. The implementation pushes an initial
    /// [`TransportEvent::Snapshot`] before any incremental events so
    /// new subscribers see current state without a separate query.
    async fn subscribe_state(&self, project: ProjectContext, tx: Tx<TransportEvent>);

    /// Subscribe to position ticks at ~30Hz. Continuous channel —
    /// subscribers may miss intermediate samples under load; the next
    /// tick is "close enough" for a playhead UI.
    async fn subscribe_position(&self, project: ProjectContext, tx: Tx<PositionTick>);
}

// ─ Service token + Bind<B> for layer composition ────────────────────
//
// Hand-rolled equivalent of what `#[architect::rpc]` emits for
// sync-trait services. `#[vox::service]` is pure-async streaming so
// it doesn't go through the architect-rpc sync-bridge — but we still
// want the same composition surface: drop `transport::stream::Service`
// into a `Reaper::layers()` bundle.

/// Deferred-bind token for [`TransportStream`]. Slot it into
/// `architect::layers![...]` like any sync-service token; the
/// `Bind<S>` impl below resolves it against any backend `S` that
/// implements `TransportStream`.
#[cfg(feature = "vox")]
#[derive(Debug, Default, Clone, Copy)]
pub struct Service;

#[cfg(feature = "vox")]
impl architect::BindAny for Service {
    fn descriptor(&self) -> &'static architect::vox::ServiceDescriptor {
        transport_stream_service_descriptor()
    }
}

#[cfg(feature = "vox")]
impl<S> architect::Bind<S> for Service
where
    S: TransportStream + Clone + Send + Sync + 'static,
{
    fn bind_into(self, backend: &S, router: &mut architect::LayerRouter) {
        use architect::LayerSink as _;
        router.add_mounted(architect::Mounted::new(
            transport_stream_service_descriptor(),
            TransportStreamDispatcher::new(backend.clone()),
        ));
    }
}

// ── Layer-composition impls ────────────────────────────────────────
//
// Same shape `#[architect::rpc]` emits for sync-trait service tokens
// — lets `transport::stream::Service` slot into `layers![...]`
// alongside the existing sync-trait service tokens. The blanket
// `impl<B, T: Bind<B> + Descriptors + Sized> Layer<B> for T` in
// architect picks up `Layer<B>` for free once `Bind` and
// `Descriptors` are in place.

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
        out.push(transport_stream_service_descriptor());
    }
}

/// Immediate-bind helper. Returns a [`Mounted`](architect::Mounted)
/// ready to merge into someone else's `Layer`. Use when you want to
/// pre-bind the stream service to a different backend than the rest
/// of the bundle (e.g. a mock state source).
#[cfg(feature = "vox")]
pub fn layer<S>(backend: S) -> architect::Mounted
where
    S: TransportStream + Clone + Send + Sync + 'static,
{
    architect::Mounted::new(
        transport_stream_service_descriptor(),
        TransportStreamDispatcher::new(backend),
    )
}
