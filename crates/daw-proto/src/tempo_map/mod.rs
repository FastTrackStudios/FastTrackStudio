//! Tempo map module — canonical home for everything tempo-map-related.

mod engine;
mod error;
mod event;
mod service;
mod tempo_point;

pub use engine::*;
pub use error::*;
pub use event::*;
#[cfg(feature = "vox")]
pub use service::{
    Service, TempoMapClient, TempoMapRpcDispatcher as Dispatcher, layer, serve,
    tempo_map_rpc_service_descriptor as descriptor,
};
pub use service::{TempoMap, TempoMapRpc};
pub use tempo_point::*;
