//! `EventBus` service — single-subscribe surface for all stream domains.
//!
//! Architect-bridged: the only method is async + streaming, so the
//! bridge classifies the trait as `AllAsync` and passes calls through
//! unchanged. The Reaper backend wires a `select!` over whichever hub
//! receivers the `BusFilter` enables and wraps each event in
//! `DawEvent`.

use super::event::{BusFilter, DawEvent};
use vox::Tx;

#[architect::rpc]
pub trait EventBus {
    /// Subscribe to a multiplexed stream of `DawEvent`s. `filter`
    /// selects which domains forward to `tx`; disabled domains are
    /// never observed (no decode, no broadcast subscription, no
    /// wakeups for the forwarder task).
    async fn subscribe(&self, filter: BusFilter, tx: Tx<DawEvent>);
}
