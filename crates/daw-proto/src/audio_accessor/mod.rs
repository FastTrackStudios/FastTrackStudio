//! Audio accessor — types + service trait.

mod service;
mod types;

pub use service::{AudioAccessors, AudioAccessorsRpc};
pub use types::*;

#[cfg(feature = "vox")]
pub use service::{AudioAccessorsClient, Dispatcher, descriptor, layer, serve};
