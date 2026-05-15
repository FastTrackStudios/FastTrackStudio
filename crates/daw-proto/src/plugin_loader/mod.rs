//! Plugin loader — types + service trait.

mod service;
mod types;

pub use service::{PluginLoading, PluginLoadingRpc};
pub use types::{LoadedPluginInfo, PluginLoadResult};

#[cfg(feature = "vox")]
pub use service::{Dispatcher, PluginLoadingClient, descriptor, layer, serve};
