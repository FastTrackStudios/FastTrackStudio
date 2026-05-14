//! Tempo map module — canonical home for everything tempo-map-related.

mod error;
mod event;
mod service;
mod tempo_point;

pub use error::*;
pub use event::*;
pub use service::{TempoMap, TempoMapRpc};
#[cfg(feature = "vox")]
pub use service::{
    TempoMapClient, TempoMapRpcDispatcher as Dispatcher, serve,
    tempo_map_rpc_service_descriptor as descriptor,
};
pub use tempo_point::*;
