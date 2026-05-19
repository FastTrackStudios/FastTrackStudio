//! Take service — canonical home for the `Takes` trait + architect
//! re-exports. Take *data* types (`Take`, `TakeRef`, `TakeMarker`, …)
//! live under `crate::item::take` for now; this module owns the
//! service surface so `take::serve(Reaper)` mounts cleanly alongside
//! the rest of the architect::rpc family.

pub mod service;

pub use service::{Takes, TakesRpc};

#[cfg(feature = "vox")]
pub use service::{
    Service, TakesClient, TakesRpcDispatcher as Dispatcher, layer, serve,
    takes_rpc_service_descriptor as descriptor,
};
