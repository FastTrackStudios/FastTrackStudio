//! Routing module for track sends, receives, and hardware outputs
//!
//! This module provides types and services for managing audio routing between tracks
//! and to hardware outputs. Routes can be sends (from a track), receives (to a track),
//! or hardware outputs.

mod error;
mod event;
mod route;
mod service;
mod sync_service;

pub use error::*;
pub use event::*;
pub use route::*;
pub use service::*;
pub use sync_service::{Routing, RoutingRpc};

// architect::rpc aliases — `routing::descriptor()` + `routing::serve(Reaper)`
// mount the sync trait alongside the rest of the architect::rpc family.
#[cfg(feature = "vox")]
pub use sync_service::{Dispatcher, RoutingClient, descriptor, serve};
