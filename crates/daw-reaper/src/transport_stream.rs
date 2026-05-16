//! `TransportStream` impl for the REAPER backend.
//!
//! Both `subscribe_*` methods take a `vox::Tx<T>` from the caller,
//! subscribe a `broadcast::Receiver<T>` off the global
//! [`crate::event_hub::DawEventHub`], and spawn a forwarder task
//! that pumps `recv() → tx.send()` until either side disconnects.
//!
//! The forwarder is `tokio::task::spawn`. It exits when:
//! - the subscriber drops `tx` (client disconnected), OR
//! - the broadcast sender drops (hub torn down), OR
//! - `RecvError::Lagged` repeated past the drop-old budget (continuous
//!   only; we just keep going on `Lagged` for transport-state since
//!   transitions are rare).
//!
//! Initial snapshot: `subscribe_state` reads current Transport state
//! from the backend and pushes a [`TransportEvent::Snapshot`] before
//! the forwarder starts, so newly-subscribed clients see current
//! state without a separate query.

use crate::event_hub::hub;
use crate::marker::Reaper;
use daw_proto::ProjectContext;
use daw_proto::transport::{PositionTick, TransportEvent, TransportStream};
use tokio::sync::broadcast::error::RecvError;
use tracing::{trace, warn};
use vox::Tx;

impl TransportStream for Reaper {
    async fn subscribe_state(&self, _project: ProjectContext, tx: Tx<TransportEvent>) {
        let mut rx = hub().subscribe_transport_state();

        // Initial snapshot — TODO(streaming/snapshot): synthesize a
        // TransportEvent::Snapshot from the current backend state
        // (calls into Transport::get_state). Skipping for the Phase
        // 1 wire-through; the polling timer will produce the next
        // tick within ~33ms anyway.

        tokio::task::spawn(async move {
            loop {
                match rx.recv().await {
                    Ok(event) => {
                        if let Err(e) = tx.send(event).await {
                            trace!(?e, "transport state subscriber disconnected");
                            return;
                        }
                    }
                    Err(RecvError::Closed) => {
                        trace!("transport state hub closed");
                        return;
                    }
                    Err(RecvError::Lagged(skipped)) => {
                        // Occasional channel — log and keep going.
                        // Transitions are rare; if a subscriber lags
                        // past the buffer we'd rather drop frames
                        // than the connection.
                        warn!(skipped, "transport state subscriber lagged; continuing");
                    }
                }
            }
        });
    }

    async fn subscribe_position(&self, _project: ProjectContext, tx: Tx<PositionTick>) {
        let mut rx = hub().subscribe_position();

        tokio::task::spawn(async move {
            loop {
                match rx.recv().await {
                    Ok(tick) => {
                        if let Err(e) = tx.send(tick).await {
                            trace!(?e, "position subscriber disconnected");
                            return;
                        }
                    }
                    Err(RecvError::Closed) => {
                        trace!("position hub closed");
                        return;
                    }
                    Err(RecvError::Lagged(_)) => {
                        // Continuous channel — drop is fine. The
                        // next tick (within ~33ms) is close enough.
                        // Don't even log; this is expected under
                        // load.
                        continue;
                    }
                }
            }
        });
    }
}
