//! Service trait for the patchbay domain.
//!
//! One service covers the whole surface: graph snapshot + live events,
//! link create/destroy, presets, aliases, clock control, and the Dante
//! stack. Served by the `patchbay` engine crate; consumed in-process by
//! the desktop app and over ws/iroh by remotes.

use facet::Facet;
use serde::{Deserialize, Serialize};
use vox::Tx;

use crate::types::{
    AliasEntry, ApplyReport, ClockInfo, DanteDevice, DanteStatus, GraphEvent, GraphSnapshot,
    RoutingPreset, ServiceAction, ServiceStatus,
};

/// Typed error for patchbay service boundaries.
#[repr(C)]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet, thiserror::Error)]
pub enum PatchbayError {
    /// Entity not found (port, link, preset, …).
    #[error("{entity} not found: {id}")]
    NotFound { entity: String, id: String },

    /// The PipeWire engine isn't running (no daemon, engine thread died).
    #[error("pipewire engine unavailable: {0}")]
    EngineUnavailable(String),

    /// Catch-all for unexpected failures.
    #[error("internal error: {0}")]
    Internal(String),
}

impl PatchbayError {
    pub fn not_found(entity: impl Into<String>, id: impl ToString) -> Self {
        Self::NotFound {
            entity: entity.into(),
            id: id.to_string(),
        }
    }
}

impl From<String> for PatchbayError {
    fn from(s: String) -> Self {
        Self::Internal(s)
    }
}

pub mod patchbay_service {
    use super::*;

    #[architect::rpc]
    pub trait PatchbayService {
        // ── Graph ────────────────────────────────────────────────────

        /// Complete current graph. Render from this, then apply
        /// `graph_events` incrementally.
        async fn graph(&self) -> Result<GraphSnapshot, PatchbayError>;

        /// Create a link between an output port and an input port
        /// (global ids). Created with `object.linger` so it survives
        /// the app exiting.
        async fn create_link(&self, output_port: u32, input_port: u32)
        -> Result<(), PatchbayError>;

        /// Destroy a link by global id.
        async fn destroy_link(&self, link_id: u32) -> Result<(), PatchbayError>;

        /// Every graph change, as it happens.
        #[subscribe]
        fn graph_events(&self) -> GraphEvent;

        // ── Presets (connection memory) ──────────────────────────────

        async fn list_presets(&self) -> Result<Vec<RoutingPreset>, PatchbayError>;

        /// Snapshot the current connections into a named preset
        /// (overwrites an existing preset of the same name).
        async fn save_preset(
            &self,
            name: String,
            description: String,
        ) -> Result<RoutingPreset, PatchbayError>;

        /// Re-apply a preset: create every remembered link whose
        /// endpoints exist. `exclusive` also destroys current links
        /// that are NOT in the preset (full-state restore).
        async fn apply_preset(
            &self,
            name: String,
            exclusive: bool,
        ) -> Result<ApplyReport, PatchbayError>;

        async fn delete_preset(&self, name: String) -> Result<(), PatchbayError>;

        // ── Aliases (pretty names) ───────────────────────────────────

        async fn aliases(&self) -> Result<Vec<AliasEntry>, PatchbayError>;

        /// Set a display alias for `"node.name"` or `"node.name:port.name"`.
        /// An empty alias clears the entry.
        async fn set_alias(&self, target: String, alias: String) -> Result<(), PatchbayError>;

        /// Pull channel names from a REAPER ChanMap (`nameN=Label`,
        /// 0-based → channel N+1) into port aliases on `node`: every
        /// port whose numeric suffix is N+1 (playback/capture/monitor)
        /// gets the label. Empty `path` = the host's default chanmap
        /// (`~/.fasttrackstudio/Reaper/ChanMaps/<hostname>.ReaperChanMap`).
        /// Returns the number of aliases written.
        async fn import_chanmap(&self, node: String, path: String) -> Result<u32, PatchbayError>;

        /// Push `node`'s port aliases back into a REAPER ChanMap's
        /// `nameN=` lines (other lines preserved; file created if
        /// missing). Empty `path` = the host default. Returns the
        /// number of channel names written.
        async fn export_chanmap(&self, node: String, path: String) -> Result<u32, PatchbayError>;

        // ── Clock ────────────────────────────────────────────────────

        async fn clock(&self) -> Result<ClockInfo, PatchbayError>;

        /// Force the graph quantum (frames); `0` returns to automatic.
        async fn force_quantum(&self, frames: u32) -> Result<(), PatchbayError>;

        // ── Dante / Inferno stack ────────────────────────────────────

        async fn dante_status(&self) -> Result<DanteStatus, PatchbayError>;

        /// Bring `dante.target` up or down (systemd user unit).
        async fn set_dante(&self, on: bool) -> Result<(), PatchbayError>;

        // ── Managed services (rig health) ────────────────────────────

        /// Status of every managed audio-stack unit (PipeWire,
        /// WirePlumber, statime, Inferno nodes, routing links, …).
        async fn services(&self) -> Result<Vec<ServiceStatus>, PatchbayError>;

        /// Start/stop/restart one managed unit. Only whitelisted units
        /// are accepted — this is not a general systemctl proxy.
        async fn service_action(
            &self,
            unit: String,
            action: ServiceAction,
        ) -> Result<(), PatchbayError>;

        // ── Dante network (ARC routing grid) ─────────────────────────

        /// Discover Dante devices (mDNS) and fetch each one's TX/RX
        /// channel lists + live subscriptions over ARC. Slowish
        /// (seconds — network round-trips); call on demand.
        async fn dante_network(&self) -> Result<Vec<DanteDevice>, PatchbayError>;

        /// Subscribe `rx_device`'s channel `rx_channel` to
        /// `tx_device`'s channel named `tx_channel`.
        async fn dante_subscribe(
            &self,
            rx_device: String,
            rx_channel: u32,
            tx_device: String,
            tx_channel: String,
        ) -> Result<(), PatchbayError>;

        /// Clear the subscription on `rx_device`'s channel `rx_channel`.
        async fn dante_unsubscribe(
            &self,
            rx_device: String,
            rx_channel: u32,
        ) -> Result<(), PatchbayError>;
    }
}

pub use patchbay_service::{
    PatchbayService, PatchbayServiceClient, Service as PatchbayServiceLayer,
    layer as patchbay_service_layer, patchbay_service_rpc_service_descriptor,
    patchbay_service_rpc_service_descriptor as patchbay_service_service_descriptor,
    serve as serve_patchbay_service,
};
pub use patchbay_service::{
    PatchbayServiceRpcDispatcher as PatchbayServiceDispatcher, PatchbayServiceStreamClient,
    patchbay_service_stream_service_descriptor, stream_serve as patchbay_service_stream_serve,
};
