//! Position conversion — service trait.

mod service;

pub use service::{PositionConversion, PositionConversionRpc};

#[cfg(feature = "vox")]
pub use service::{Dispatcher, PositionConversionClient, descriptor, serve};
