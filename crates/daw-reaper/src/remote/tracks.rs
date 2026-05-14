//! Remote tracks handle: [`RemoteTracks`].

use daw_proto::sync::{Daw as _, Project as _, Tracks as TracksTrait};
use daw_proto::{DawError, DawResult, Track};

use super::{ReaperRemote, dispatch, dispatch_read, main_thread};

pub struct RemoteTracks<'a> {
    remote: &'a ReaperRemote,
    guid: String,
}

impl<'a> RemoteTracks<'a> {
    pub(crate) fn new(remote: &'a ReaperRemote, guid: &str) -> Self {
        Self {
            remote,
            guid: guid.to_string(),
        }
    }
}

impl<'a> TracksTrait for RemoteTracks<'a> {
    fn all(&self) -> Vec<Track> {
        let guid = self.guid.clone();
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return Vec::new();
            };
            let Ok(project) = mt.project(&guid) else {
                return Vec::new();
            };
            project.tracks().all()
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
            project.tracks().count()
        })
    }

    fn by_guid(&self, guid: &str) -> Option<Track> {
        let proj_guid = self.guid.clone();
        let track_guid = guid.to_string();
        self.remote.runtime.block_on(async move {
            crate::main_thread::query(move || {
                let mt = crate::sync::ReaperMainThread::try_new()?;
                let project = mt.project(&proj_guid).ok()?;
                project.tracks().by_guid(&track_guid)
            })
            .await
            .flatten()
        })
    }

    fn selected(&self) -> Vec<Track> {
        let guid = self.guid.clone();
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return Vec::new();
            };
            let Ok(project) = mt.project(&guid) else {
                return Vec::new();
            };
            project.tracks().selected()
        })
    }

    fn add(&self, name: &str, at_index: Option<u32>) -> DawResult<String> {
        let guid = self.guid.clone();
        let name = name.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.tracks().add(&name, at_index)
        })
    }

    fn remove(&self, guid: &str) -> DawResult<()> {
        let proj_guid = self.guid.clone();
        let track_guid = guid.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&proj_guid)?.tracks().remove(&track_guid)
        })
    }

    fn remove_all(&self) -> DawResult<()> {
        let guid = self.guid.clone();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.tracks().remove_all()
        })
    }

    fn master(&self) -> DawResult<Track> {
        let guid = self.guid.clone();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.tracks().master()
        })
    }

    fn set_muted(&self, guid: &str, muted: bool) -> DawResult<()> {
        let proj_guid = self.guid.clone();
        let track_guid = guid.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&proj_guid)?
                .tracks()
                .set_muted(&track_guid, muted)
        })
    }

    fn set_soloed(&self, guid: &str, soloed: bool) -> DawResult<()> {
        let proj_guid = self.guid.clone();
        let track_guid = guid.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&proj_guid)?
                .tracks()
                .set_soloed(&track_guid, soloed)
        })
    }

    fn set_volume(&self, guid: &str, volume: f64) -> DawResult<()> {
        let proj_guid = self.guid.clone();
        let track_guid = guid.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&proj_guid)?
                .tracks()
                .set_volume(&track_guid, volume)
        })
    }

    fn set_pan(&self, guid: &str, pan: f64) -> DawResult<()> {
        let proj_guid = self.guid.clone();
        let track_guid = guid.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&proj_guid)?.tracks().set_pan(&track_guid, pan)
        })
    }

    fn rename(&self, guid: &str, name: &str) -> DawResult<()> {
        let proj_guid = self.guid.clone();
        let track_guid = guid.to_string();
        let name = name.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&proj_guid)?.tracks().rename(&track_guid, &name)
        })
    }

    fn set_color(&self, guid: &str, color: u32) -> DawResult<()> {
        let proj_guid = self.guid.clone();
        let track_guid = guid.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&proj_guid)?
                .tracks()
                .set_color(&track_guid, color)
        })
    }

    fn get_ext_state(&self, guid: &str, section: &str, key: &str) -> Option<String> {
        let proj_guid = self.guid.clone();
        let track_guid = guid.to_string();
        let section = section.to_string();
        let key = key.to_string();
        self.remote.runtime.block_on(async move {
            crate::main_thread::query(move || {
                let mt = crate::sync::ReaperMainThread::try_new()?;
                let project = mt.project(&proj_guid).ok()?;
                project.tracks().get_ext_state(&track_guid, &section, &key)
            })
            .await
            .flatten()
        })
    }

    fn set_ext_state(&self, guid: &str, section: &str, key: &str, value: &str) -> DawResult<()> {
        let proj_guid = self.guid.clone();
        let track_guid = guid.to_string();
        let section = section.to_string();
        let key = key.to_string();
        let value = value.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&proj_guid)?
                .tracks()
                .set_ext_state(&track_guid, &section, &key, &value)
        })
    }
}

// Silence unused-import warning for DawError when not referenced explicitly.
#[allow(dead_code)]
fn _assert_error_in_scope(e: DawError) -> DawError {
    e
}
