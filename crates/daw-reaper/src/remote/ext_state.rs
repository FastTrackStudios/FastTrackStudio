//! Remote global ExtState handle: [`RemoteExtState`].

use daw_proto::DawResult;
use daw_proto::sync::{Daw as _, ExtState as ExtStateTrait, Project as _};

use super::{ReaperRemote, dispatch, main_thread};

pub struct RemoteExtState<'a> {
    remote: &'a ReaperRemote,
}

impl<'a> RemoteExtState<'a> {
    pub(crate) fn new(remote: &'a ReaperRemote) -> Self {
        Self { remote }
    }
}

impl<'a> ExtStateTrait for RemoteExtState<'a> {
    fn get(&self, section: &str, key: &str) -> Option<String> {
        let section = section.to_string();
        let key = key.to_string();
        self.remote.runtime.block_on(async move {
            crate::main_thread::query(move || {
                let mt = crate::sync::ReaperMainThread::try_new()?;
                // ExtState is global; pick any project (current) to get a handle.
                let project = mt.current_project().ok()?;
                project.ext_state().get(&section, &key)
            })
            .await
            .flatten()
        })
    }

    fn set(&self, section: &str, key: &str, value: &str, persist: bool) -> DawResult<()> {
        let section = section.to_string();
        let key = key.to_string();
        let value = value.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.current_project()?
                .ext_state()
                .set(&section, &key, &value, persist)
        })
    }

    fn delete(&self, section: &str, key: &str, persist: bool) -> DawResult<()> {
        let section = section.to_string();
        let key = key.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.current_project()?
                .ext_state()
                .delete(&section, &key, persist)
        })
    }

    fn has(&self, section: &str, key: &str) -> bool {
        let section = section.to_string();
        let key = key.to_string();
        self.remote.runtime.block_on(async move {
            crate::main_thread::query(move || {
                let mt = crate::sync::ReaperMainThread::try_new()?;
                let project = mt.current_project().ok()?;
                Some(project.ext_state().has(&section, &key))
            })
            .await
            .flatten()
            .unwrap_or(false)
        })
    }
}
