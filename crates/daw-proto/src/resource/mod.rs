//! Resource paths — service trait.

mod service;

pub use service::{ResourcePaths, ResourcePathsRpc};

#[cfg(feature = "vox")]
pub use service::{Dispatcher, ResourcePathsClient, Service, descriptor, layer, serve};
