//! Window geometry — types + service trait.

mod service;
mod types;

pub use service::{WindowGeometry, WindowGeometryRpc};
pub use types::{WindowGeometryResult, WindowTarget};

#[cfg(feature = "vox")]
pub use service::{Dispatcher, Service, WindowGeometryClient, descriptor, layer, serve};
