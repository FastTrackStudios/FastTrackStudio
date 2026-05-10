//! Remote items handle: [`RemoteItems`].

use daw_proto::sync::{Daw as _, Items as ItemsTrait, Project as _};
use daw_proto::{DawResult, Item};

use super::{ReaperRemote, dispatch, dispatch_read, main_thread};

pub struct RemoteItems<'a> {
    remote: &'a ReaperRemote,
    guid: String,
}

impl<'a> RemoteItems<'a> {
    pub(crate) fn new(remote: &'a ReaperRemote, guid: &str) -> Self {
        Self {
            remote,
            guid: guid.to_string(),
        }
    }
}

impl<'a> ItemsTrait for RemoteItems<'a> {
    fn list(&self, track_guid: &str) -> Vec<Item> {
        let guid = self.guid.clone();
        let track_guid = track_guid.to_string();
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return Vec::new();
            };
            let Ok(project) = mt.project(&guid) else {
                return Vec::new();
            };
            project.items().list(&track_guid)
        })
    }

    fn count(&self, track_guid: &str) -> u32 {
        let guid = self.guid.clone();
        let track_guid = track_guid.to_string();
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return 0;
            };
            let Ok(project) = mt.project(&guid) else {
                return 0;
            };
            project.items().count(&track_guid)
        })
    }

    fn get(&self, item_guid: &str) -> Option<Item> {
        let guid = self.guid.clone();
        let item_guid = item_guid.to_string();
        self.remote.runtime.block_on(async move {
            crate::main_thread::query(move || {
                let mt = crate::sync::ReaperMainThread::try_new()?;
                let project = mt.project(&guid).ok()?;
                project.items().get(&item_guid)
            })
            .await
            .flatten()
        })
    }

    fn add(&self, track_guid: &str, position: f64, length: f64) -> DawResult<String> {
        let guid = self.guid.clone();
        let track_guid = track_guid.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?
                .items()
                .add(&track_guid, position, length)
        })
    }

    fn remove(&self, item_guid: &str) -> DawResult<()> {
        let guid = self.guid.clone();
        let item_guid = item_guid.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.items().remove(&item_guid)
        })
    }

    fn duplicate(&self, item_guid: &str) -> DawResult<String> {
        let guid = self.guid.clone();
        let item_guid = item_guid.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.items().duplicate(&item_guid)
        })
    }

    fn set_position(&self, item_guid: &str, position: f64) -> DawResult<()> {
        let guid = self.guid.clone();
        let item_guid = item_guid.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?
                .items()
                .set_position(&item_guid, position)
        })
    }

    fn set_length(&self, item_guid: &str, length: f64) -> DawResult<()> {
        let guid = self.guid.clone();
        let item_guid = item_guid.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.items().set_length(&item_guid, length)
        })
    }

    fn set_muted(&self, item_guid: &str, muted: bool) -> DawResult<()> {
        let guid = self.guid.clone();
        let item_guid = item_guid.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.items().set_muted(&item_guid, muted)
        })
    }

    fn set_color(&self, item_guid: &str, color: u32) -> DawResult<()> {
        let guid = self.guid.clone();
        let item_guid = item_guid.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.items().set_color(&item_guid, color)
        })
    }
}
