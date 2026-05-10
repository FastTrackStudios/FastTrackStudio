//! Remote tempo-map handle: [`RemoteTempoMap`].

use daw_proto::sync::{Daw as _, Project as _, TempoMap as TempoMapTrait};
use daw_proto::{DawResult, TempoPoint};

use super::{ReaperRemote, dispatch, dispatch_read, main_thread};

pub struct RemoteTempoMap<'a> {
    remote: &'a ReaperRemote,
    guid: String,
}

impl<'a> RemoteTempoMap<'a> {
    pub(crate) fn new(remote: &'a ReaperRemote, guid: &str) -> Self {
        Self {
            remote,
            guid: guid.to_string(),
        }
    }
}

impl<'a> TempoMapTrait for RemoteTempoMap<'a> {
    fn points(&self) -> Vec<TempoPoint> {
        let guid = self.guid.clone();
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return Vec::new();
            };
            let Ok(project) = mt.project(&guid) else {
                return Vec::new();
            };
            project.tempo_map().points()
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
            project.tempo_map().count()
        })
    }

    fn tempo_at(&self, seconds: f64) -> f64 {
        let guid = self.guid.clone();
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return 120.0;
            };
            let Ok(project) = mt.project(&guid) else {
                return 120.0;
            };
            project.tempo_map().tempo_at(seconds)
        })
    }

    fn time_to_musical(&self, seconds: f64) -> (i32, i32, f64) {
        let guid = self.guid.clone();
        self.remote.runtime.block_on(async move {
            crate::main_thread::query(move || {
                let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                    return (1, 1, 0.0);
                };
                let Ok(project) = mt.project(&guid) else {
                    return (1, 1, 0.0);
                };
                project.tempo_map().time_to_musical(seconds)
            })
            .await
            .unwrap_or((1, 1, 0.0))
        })
    }

    fn musical_to_time(&self, measure: i32, beat: i32, frac: f64) -> f64 {
        let guid = self.guid.clone();
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return 0.0;
            };
            let Ok(project) = mt.project(&guid) else {
                return 0.0;
            };
            project.tempo_map().musical_to_time(measure, beat, frac)
        })
    }

    fn add_point(&self, seconds: f64, bpm: f64) -> DawResult<()> {
        let guid = self.guid.clone();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.tempo_map().add_point(seconds, bpm)
        })
    }

    fn remove_point(&self, idx: u32) -> DawResult<()> {
        let guid = self.guid.clone();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.tempo_map().remove_point(idx)
        })
    }
}
