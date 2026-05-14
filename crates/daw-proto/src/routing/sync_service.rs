//! Routing service (architect::rpc port) — sync surface for the
//! per-track sends / receives / hardware-outputs core.
//!
//! The fuller async `RoutingService` (set_mono, set_phase,
//! source/dest channel mapping, parent send, subscribe_routing,
//! …) stays parallel for now — it's tracked as separate work in the
//! punch list. This trait covers the canonical sync surface.

use crate::{DawResult, TrackRoute};

#[architect_rpc_derive::rpc]
pub trait Routing {
    fn sends(&self, source_track_guid: &str) -> Vec<TrackRoute>;
    fn receives(&self, dest_track_guid: &str) -> Vec<TrackRoute>;
    fn hardware_outputs(&self, track_guid: &str) -> Vec<TrackRoute>;

    fn send_count(&self, track_guid: &str) -> u32;
    fn receive_count(&self, track_guid: &str) -> u32;

    fn add_send(&self, source_track_guid: &str, dest_track_guid: &str) -> DawResult<u32>;
    fn remove_send(&self, source_track_guid: &str, send_idx: u32) -> DawResult<()>;

    fn set_send_volume(&self, track_guid: &str, send_idx: u32, volume: f64) -> DawResult<()>;
    fn set_send_pan(&self, track_guid: &str, send_idx: u32, pan: f64) -> DawResult<()>;
    fn set_send_muted(&self, track_guid: &str, send_idx: u32, muted: bool) -> DawResult<()>;
    fn is_send_muted(&self, track_guid: &str, send_idx: u32) -> bool;
}

#[cfg(feature = "vox")]
pub use RoutingRpcDispatcher as Dispatcher;
#[cfg(feature = "vox")]
pub use routing_rpc_service_descriptor as descriptor;
