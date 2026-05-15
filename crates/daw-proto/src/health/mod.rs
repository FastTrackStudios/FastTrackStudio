//! Health — connection liveness probe service.

mod service;

pub use service::{Health, HealthRpc};

#[cfg(feature = "vox")]
pub use service::{Dispatcher, HealthClient, descriptor, serve};
