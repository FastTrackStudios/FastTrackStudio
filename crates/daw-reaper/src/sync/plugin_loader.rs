//! Sync `PluginLoader` impl.
//!
//! `load` is delegated to the existing infrastructure in `crate::plugin_loader`
//! which manages the global `loaded_plugins()` table and dlopen state. The
//! sync surface returns `not_supported` for `load` because the loading path
//! is genuinely async / requires the plugin context state. `list_loaded` and
//! `is_loaded` are pure reads against the global mutex and work fine sync.

use daw_proto::{DawError, DawResult, LoadedPluginInfo, PluginLoading};

use super::ReaperMainThread;

pub struct ReaperPluginLoader<'a> {
    _mt: &'a ReaperMainThread,
}

impl<'a> ReaperPluginLoader<'a> {
    pub(crate) fn new(mt: &'a ReaperMainThread) -> Self {
        Self { _mt: mt }
    }
}

impl<'a> PluginLoading for ReaperPluginLoader<'a> {
    fn load(&self, path: &str) -> DawResult<LoadedPluginInfo> {
        crate::plugin_loader::load_plugin_native(path).map_err(DawError::operation_failed)
    }

    fn list_loaded(&self) -> Vec<LoadedPluginInfo> {
        crate::plugin_loader::snapshot_loaded()
    }

    fn is_loaded(&self, path: &str) -> bool {
        crate::plugin_loader::path_is_loaded(path)
    }
}
