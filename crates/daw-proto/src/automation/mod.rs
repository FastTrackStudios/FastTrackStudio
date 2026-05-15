//! Automation — envelope + point types + service trait.

mod envelope;
mod error;
mod event;
mod service;

pub use envelope::*;
pub use error::*;
pub use event::*;
pub use service::{AddPointParams, Automation, AutomationRpc, SetPointParams, TimeRangeParams};

#[cfg(feature = "vox")]
pub use service::{AutomationClient, Dispatcher, Service, descriptor, layer, serve};
