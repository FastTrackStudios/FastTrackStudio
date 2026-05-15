//! `impl PluginLoading for Standalone` — stub.

use daw_proto::plugin_loader::{LoadedPluginInfo, PluginLoadResult, PluginLoading};

use crate::sync::Standalone;

impl PluginLoading for Standalone {
    fn load(&self, _path: &str) -> PluginLoadResult {
        PluginLoadResult::Error("standalone has no plugin host".to_string())
    }
    fn list_loaded(&self) -> Vec<LoadedPluginInfo> {
        Vec::new()
    }
    fn is_loaded(&self, _path: &str) -> bool {
        false
    }
}
