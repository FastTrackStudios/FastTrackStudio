//! Persistent key-value storage (REAPER ext-state API) — service trait.

mod service;

pub use service::{ExtState, ExtStateRpc};

#[cfg(feature = "vox")]
pub use service::{Dispatcher, ExtStateClient, Service, descriptor, layer, serve};
