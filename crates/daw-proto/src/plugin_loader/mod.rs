//! Plugin loader — types + service traits.

mod service;
mod types;

pub use service::{
    PluginLoaderService, PluginLoaderServiceClient, PluginLoaderServiceDispatcher, PluginLoading,
    plugin_loader_service_service_descriptor,
};
pub use types::{LoadedPluginInfo, PluginLoadResult};
