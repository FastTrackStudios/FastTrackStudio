//! Peaks — types + service trait.

mod service;
mod types;

pub use service::{Peaks, PeaksRpc};
pub use types::*;

#[cfg(feature = "vox")]
pub use service::{Dispatcher, PeaksClient, descriptor, serve};
