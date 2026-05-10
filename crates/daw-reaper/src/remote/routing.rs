//! Remote routing handle: [`RemoteRouting`].

use daw_proto::sync::{Daw as _, Project as _, Routing as RoutingTrait};
use daw_proto::{DawResult, TrackRoute};

use super::{ReaperRemote, dispatch, dispatch_read, main_thread};

pub struct RemoteRouting<'a> {
    remote: &'a ReaperRemote,
    guid: String,
}

impl<'a> RemoteRouting<'a> {
    pub(crate) fn new(remote: &'a ReaperRemote, guid: &str) -> Self {
        Self {
            remote,
            guid: guid.to_string(),
        }
    }
}

impl<'a> RoutingTrait for RemoteRouting<'a> {
    fn sends(&self, source_track_guid: &str) -> Vec<TrackRoute> {
        let guid = self.guid.clone();
        let track_guid = source_track_guid.to_string();
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return Vec::new();
            };
            let Ok(project) = mt.project(&guid) else {
                return Vec::new();
            };
            project.routing().sends(&track_guid)
        })
    }

    fn receives(&self, dest_track_guid: &str) -> Vec<TrackRoute> {
        let guid = self.guid.clone();
        let track_guid = dest_track_guid.to_string();
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return Vec::new();
            };
            let Ok(project) = mt.project(&guid) else {
                return Vec::new();
            };
            project.routing().receives(&track_guid)
        })
    }

    fn hardware_outputs(&self, track_guid: &str) -> Vec<TrackRoute> {
        let guid = self.guid.clone();
        let track_guid = track_guid.to_string();
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return Vec::new();
            };
            let Ok(project) = mt.project(&guid) else {
                return Vec::new();
            };
            project.routing().hardware_outputs(&track_guid)
        })
    }

    fn send_count(&self, track_guid: &str) -> u32 {
        let guid = self.guid.clone();
        let track_guid = track_guid.to_string();
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return 0;
            };
            let Ok(project) = mt.project(&guid) else {
                return 0;
            };
            project.routing().send_count(&track_guid)
        })
    }

    fn receive_count(&self, track_guid: &str) -> u32 {
        let guid = self.guid.clone();
        let track_guid = track_guid.to_string();
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return 0;
            };
            let Ok(project) = mt.project(&guid) else {
                return 0;
            };
            project.routing().receive_count(&track_guid)
        })
    }

    fn add_send(&self, source_track_guid: &str, dest_track_guid: &str) -> DawResult<u32> {
        let guid = self.guid.clone();
        let src = source_track_guid.to_string();
        let dst = dest_track_guid.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.routing().add_send(&src, &dst)
        })
    }

    fn remove_send(&self, source_track_guid: &str, send_idx: u32) -> DawResult<()> {
        let guid = self.guid.clone();
        let src = source_track_guid.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.routing().remove_send(&src, send_idx)
        })
    }

    fn set_send_volume(&self, track_guid: &str, send_idx: u32, volume: f64) -> DawResult<()> {
        let guid = self.guid.clone();
        let track_guid = track_guid.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?
                .routing()
                .set_send_volume(&track_guid, send_idx, volume)
        })
    }

    fn set_send_pan(&self, track_guid: &str, send_idx: u32, pan: f64) -> DawResult<()> {
        let guid = self.guid.clone();
        let track_guid = track_guid.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?
                .routing()
                .set_send_pan(&track_guid, send_idx, pan)
        })
    }

    fn set_send_muted(&self, track_guid: &str, send_idx: u32, muted: bool) -> DawResult<()> {
        let guid = self.guid.clone();
        let track_guid = track_guid.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?
                .routing()
                .set_send_muted(&track_guid, send_idx, muted)
        })
    }

    fn is_send_muted(&self, track_guid: &str, send_idx: u32) -> bool {
        let guid = self.guid.clone();
        let track_guid = track_guid.to_string();
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return false;
            };
            let Ok(project) = mt.project(&guid) else {
                return false;
            };
            project.routing().is_send_muted(&track_guid, send_idx)
        })
    }
}
