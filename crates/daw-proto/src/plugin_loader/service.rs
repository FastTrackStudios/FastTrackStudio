//! Plugin loader service traits — async `PluginLoaderService` for
//! over-RPC loading + sync `PluginLoader` handle for in-process use.

use super::{LoadedPluginInfo, PluginLoadResult};
use crate::DawResult;
use vox::service;

/// Service for eagerly loading REAPER plugins into the host process.
///
/// Plugins are loaded via `dlopen` + `ReaperPluginEntry`, giving them
/// their own REAPER context. Loaded libraries are kept alive for the
/// process lifetime.
#[service]
pub trait PluginLoaderService {
    /// Load a plugin from the given filesystem path.
    async fn load_plugin(&self, plugin_path: String) -> PluginLoadResult;

    /// List all currently loaded plugins.
    async fn list_loaded(&self) -> Vec<LoadedPluginInfo>;

    /// Check if a plugin at the given path is already loaded.
    async fn is_loaded(&self, plugin_path: String) -> bool;
}

/// Sync handle counterpart — used by in-process callers that already
/// hold a backend reference.
pub trait PluginLoading {
    fn load(&self, path: &str) -> DawResult<LoadedPluginInfo>;
    fn list_loaded(&self) -> Vec<LoadedPluginInfo>;
    fn is_loaded(&self, path: &str) -> bool;
}
