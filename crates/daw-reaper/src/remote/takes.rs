//! Remote takes handle: [`RemoteTakes`].

use daw_proto::sync::{Daw as _, Project as _, Takes as TakesTrait};
use daw_proto::{DawResult, Take};

use super::{ReaperRemote, dispatch, dispatch_read, main_thread};

pub struct RemoteTakes<'a> {
    remote: &'a ReaperRemote,
    guid: String,
}

impl<'a> RemoteTakes<'a> {
    pub(crate) fn new(remote: &'a ReaperRemote, guid: &str) -> Self {
        Self {
            remote,
            guid: guid.to_string(),
        }
    }
}

impl<'a> TakesTrait for RemoteTakes<'a> {
    fn list(&self, item_guid: &str) -> Vec<Take> {
        let guid = self.guid.clone();
        let item_guid = item_guid.to_string();
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return Vec::new();
            };
            let Ok(project) = mt.project(&guid) else {
                return Vec::new();
            };
            project.takes().list(&item_guid)
        })
    }

    fn count(&self, item_guid: &str) -> u32 {
        let guid = self.guid.clone();
        let item_guid = item_guid.to_string();
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return 0;
            };
            let Ok(project) = mt.project(&guid) else {
                return 0;
            };
            project.takes().count(&item_guid)
        })
    }

    fn active(&self, item_guid: &str) -> Option<Take> {
        let guid = self.guid.clone();
        let item_guid = item_guid.to_string();
        self.remote.runtime.block_on(async move {
            crate::main_thread::query(move || {
                let mt = crate::sync::ReaperMainThread::try_new()?;
                let project = mt.project(&guid).ok()?;
                project.takes().active(&item_guid)
            })
            .await
            .flatten()
        })
    }

    fn get(&self, take_guid: &str) -> Option<Take> {
        let guid = self.guid.clone();
        let take_guid = take_guid.to_string();
        self.remote.runtime.block_on(async move {
            crate::main_thread::query(move || {
                let mt = crate::sync::ReaperMainThread::try_new()?;
                let project = mt.project(&guid).ok()?;
                project.takes().get(&take_guid)
            })
            .await
            .flatten()
        })
    }

    fn set_active(&self, item_guid: &str, take_idx: u32) -> DawResult<()> {
        let guid = self.guid.clone();
        let item_guid = item_guid.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.takes().set_active(&item_guid, take_idx)
        })
    }

    fn rename(&self, take_guid: &str, name: &str) -> DawResult<()> {
        let guid = self.guid.clone();
        let take_guid = take_guid.to_string();
        let name = name.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.takes().rename(&take_guid, &name)
        })
    }
}
