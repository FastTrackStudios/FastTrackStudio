//! Remote regions handle: [`RemoteRegions`].

use daw_proto::sync::{Daw as _, Project as _, Regions as RegionsTrait};
use daw_proto::{DawResult, Region};

use super::{ReaperRemote, dispatch, dispatch_read, main_thread};

pub struct RemoteRegions<'a> {
    remote: &'a ReaperRemote,
    guid: String,
}

impl<'a> RemoteRegions<'a> {
    pub(crate) fn new(remote: &'a ReaperRemote, guid: &str) -> Self {
        Self {
            remote,
            guid: guid.to_string(),
        }
    }
}

impl<'a> RegionsTrait for RemoteRegions<'a> {
    fn all(&self) -> Vec<Region> {
        let guid = self.guid.clone();
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return Vec::new();
            };
            let Ok(project) = mt.project(&guid) else {
                return Vec::new();
            };
            project.regions().all()
        })
    }

    fn get(&self, id: u32) -> Option<Region> {
        let guid = self.guid.clone();
        self.remote.runtime.block_on(async move {
            crate::main_thread::query(move || {
                let mt = crate::sync::ReaperMainThread::try_new()?;
                let project = mt.project(&guid).ok()?;
                project.regions().get(id)
            })
            .await
            .flatten()
        })
    }

    fn count(&self) -> u32 {
        let guid = self.guid.clone();
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return 0;
            };
            let Ok(project) = mt.project(&guid) else {
                return 0;
            };
            project.regions().count()
        })
    }

    fn add(&self, start: f64, end: f64, name: &str) -> DawResult<u32> {
        let guid = self.guid.clone();
        let name = name.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.regions().add(start, end, &name)
        })
    }

    fn remove(&self, id: u32) -> DawResult<()> {
        let guid = self.guid.clone();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.regions().remove(id)
        })
    }

    fn set_bounds(&self, id: u32, start: f64, end: f64) -> DawResult<()> {
        let guid = self.guid.clone();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.regions().set_bounds(id, start, end)
        })
    }

    fn rename(&self, id: u32, name: &str) -> DawResult<()> {
        let guid = self.guid.clone();
        let name = name.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.regions().rename(id, &name)
        })
    }

    fn set_color(&self, id: u32, color: u32) -> DawResult<()> {
        let guid = self.guid.clone();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.regions().set_color(id, color)
        })
    }
}
