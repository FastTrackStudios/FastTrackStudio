//! FX parameter operations — service trait.

mod service;

pub use service::{FxParams, FxParamsRpc};

#[cfg(feature = "vox")]
pub use service::{Dispatcher, FxParamsClient, descriptor, serve};
