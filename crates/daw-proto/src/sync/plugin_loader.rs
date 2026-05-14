//! Load CLAP / VST plugins into the host's process.

use crate::{DawResult, LoadedPluginInfo};

pub trait PluginLoader {
    fn load(&self, path: &str) -> DawResult<LoadedPluginInfo>;
    fn list_loaded(&self) -> Vec<LoadedPluginInfo>;
    fn is_loaded(&self, path: &str) -> bool;
}
