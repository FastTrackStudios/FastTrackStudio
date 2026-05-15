//! Audio engine — types + service trait.

mod service;
mod types;

pub use service::{AudioEngine, AudioEngineRpc};
pub use types::{AudioEngineState, AudioInputChannel, AudioInputInfo, AudioLatency};

#[cfg(feature = "vox")]
pub use service::{AudioEngineClient, Dispatcher, descriptor, layer, serve};
