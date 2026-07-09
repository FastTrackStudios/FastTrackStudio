//! Cross-domain event bus handle.
//!
//! Single-subscribe surface that multiplexes every per-domain stream
//! (`Tracks::subscribe`, `Markers::subscribe`, etc.) onto one channel.
//! Use this when a consumer wants several domains at once (OSC/MIDI
//! bridges, inspectors, web UIs). Use the per-domain `Project::tracks()`
//! / `markers()` / etc. when only one domain is interesting.

use std::sync::Arc;

use crate::{DawClients, Result};
use daw_proto::event_bus::{BusFilter, DawEvent};

/// Handle for subscribing to the cross-domain event bus. Cheap to
/// clone — wraps a shared `Arc<DawClients>`.
#[derive(Clone)]
pub struct Events {
    clients: Arc<DawClients>,
}

impl Events {
    pub(crate) fn new(clients: Arc<DawClients>) -> Self {
        Self { clients }
    }

    /// Subscribe with the given filter. Returns an `Rx` that yields a
    /// stream of `DawEvent`s for every enabled domain. The returned
    /// stream lives until the server drops the subscription.
    pub async fn subscribe(&self, filter: BusFilter) -> Result<vox::Rx<DawEvent>> {
        let (tx, rx) = vox::channel();
        self.clients.event_bus.subscribe(filter, tx).await?;
        Ok(rx)
    }

    /// Convenience: subscribe with `BusFilter::all()`. Use this when
    /// the consumer wants every event the bridge can emit — typical
    /// for OSC/MIDI translation layers.
    pub async fn subscribe_all(&self) -> Result<vox::Rx<DawEvent>> {
        self.subscribe(BusFilter::all()).await
    }
}

impl std::fmt::Debug for Events {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Events").finish_non_exhaustive()
    }
}
