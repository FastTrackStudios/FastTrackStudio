//! In-process diagnostic probes (latency, throughput).

mod service;

pub use service::{AudioSyncSnapshot, Diagnostics, DiagnosticsRpc, PeerSummary};

#[cfg(feature = "vox")]
pub use service::{
    DiagnosticsClient, DiagnosticsRpcDispatcher as Dispatcher, Service,
    diagnostics_rpc_service_descriptor as descriptor, layer, serve,
};
