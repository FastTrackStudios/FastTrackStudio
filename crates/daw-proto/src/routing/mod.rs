//! Routing — sends, receives, hardware outputs.

mod error;
mod event;
mod route;
mod service;

pub use error::*;
pub use event::*;
pub use route::*;
pub use service::{RouteLocation, Routing, RoutingRpc};

#[cfg(feature = "vox")]
pub use service::{Dispatcher, RoutingClient, descriptor, serve};
