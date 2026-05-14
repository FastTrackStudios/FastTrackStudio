//! Remote plugin loader handle: [`RemotePluginLoader`].

use daw_proto::{Daw as _, PluginLoader as PluginLoaderTrait};
use daw_proto::{DawResult, LoadedPluginInfo};

use super::{ReaperRemote, dispatch, dispatch_read, main_thread};

pub struct RemotePluginLoader<'a> {
    remote: &'a ReaperRemote,
}

impl<'a> RemotePluginLoader<'a> {
    pub(crate) fn new(remote: &'a ReaperRemote) -> Self {
        Self { remote }
    }
}

impl<'a> PluginLoaderTrait for RemotePluginLoader<'a> {
    fn load(&self, path: &str) -> DawResult<LoadedPluginInfo> {
        let path = path.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.plugin_loader().load(&path)
        })
    }

    fn list_loaded(&self) -> Vec<LoadedPluginInfo> {
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return Vec::new();
            };
            mt.plugin_loader().list_loaded()
        })
    }

    fn is_loaded(&self, path: &str) -> bool {
        let path = path.to_string();
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return false;
            };
            mt.plugin_loader().is_loaded(&path)
        })
    }
}
