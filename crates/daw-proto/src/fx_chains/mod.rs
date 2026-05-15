//! Whole-FX-chain operations — service trait.

mod service;

pub use service::{FxChains, FxChainsRpc};

#[cfg(feature = "vox")]
pub use service::{Dispatcher, FxChainsClient, descriptor, layer, serve};
