//! `StandalonePluginLoader` — sync plugin-loader sub-handle.
//!
//! Records "loaded" plugins in an in-memory map keyed by path. Loading is
//! idempotent: re-loading the same path returns the cached entry.

use daw_proto::{DawResult, LoadedPluginInfo, PluginLoading};

use super::daw::Standalone;

pub struct StandalonePluginLoader<'a> {
    daw: &'a Standalone,
}

impl<'a> StandalonePluginLoader<'a> {
    pub(crate) fn new(daw: &'a Standalone) -> Self {
        Self { daw }
    }
}

fn name_from_path(path: &str) -> String {
    path.rsplit(['/', '\\']).next().unwrap_or(path).to_string()
}

impl<'a> PluginLoading for StandalonePluginLoader<'a> {
    fn load(&self, path: &str) -> DawResult<LoadedPluginInfo> {
        let mut s = self.daw.state.lock().expect("standalone state poisoned");
        let entry = s
            .plugins
            .entry(path.to_string())
            .or_insert_with(|| LoadedPluginInfo {
                path: path.to_string(),
                name: name_from_path(path),
            })
            .clone();
        Ok(entry)
    }

    fn list_loaded(&self) -> Vec<LoadedPluginInfo> {
        let s = self.daw.state.lock().expect("standalone state poisoned");
        s.plugins.values().cloned().collect()
    }

    fn is_loaded(&self, path: &str) -> bool {
        let s = self.daw.state.lock().expect("standalone state poisoned");
        s.plugins.contains_key(path)
    }
}
