//! Remote transport handle: [`RemoteTransport`].

use daw_proto::sync::{Daw as _, Project as _, Transport as TransportTrait};
use daw_proto::{DawResult, PlayState, Transport as TransportState};

use super::{ReaperRemote, dispatch, dispatch_read, main_thread};

pub struct RemoteTransport<'a> {
    remote: &'a ReaperRemote,
    guid: String,
}

impl<'a> RemoteTransport<'a> {
    pub(crate) fn new(remote: &'a ReaperRemote, guid: &str) -> Self {
        Self {
            remote,
            guid: guid.to_string(),
        }
    }
}

impl<'a> TransportTrait for RemoteTransport<'a> {
    fn state(&self) -> DawResult<TransportState> {
        let guid = self.guid.clone();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.transport().state()
        })
    }

    fn position(&self) -> f64 {
        let guid = self.guid.clone();
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return 0.0;
            };
            let Ok(project) = mt.project(&guid) else {
                return 0.0;
            };
            project.transport().position()
        })
    }

    fn set_position(&self, seconds: f64) -> DawResult<()> {
        let guid = self.guid.clone();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.transport().set_position(seconds)
        })
    }

    fn time_selection(&self) -> Option<(f64, f64)> {
        let guid = self.guid.clone();
        self.remote.runtime.block_on(async move {
            crate::main_thread::query(move || {
                let mt = crate::sync::ReaperMainThread::try_new()?;
                let project = mt.project(&guid).ok()?;
                project.transport().time_selection()
            })
            .await
            .flatten()
        })
    }

    fn set_time_selection(&self, start: f64, end: f64) -> DawResult<()> {
        let guid = self.guid.clone();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?
                .transport()
                .set_time_selection(start, end)
        })
    }

    fn clear_time_selection(&self) -> DawResult<()> {
        let guid = self.guid.clone();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.transport().clear_time_selection()
        })
    }

    fn play_state(&self) -> PlayState {
        let guid = self.guid.clone();
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return PlayState::Stopped;
            };
            let Ok(project) = mt.project(&guid) else {
                return PlayState::Stopped;
            };
            project.transport().play_state()
        })
    }

    fn play(&self) -> DawResult<()> {
        let guid = self.guid.clone();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.transport().play()
        })
    }

    fn pause(&self) -> DawResult<()> {
        let guid = self.guid.clone();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.transport().pause()
        })
    }

    fn stop(&self) -> DawResult<()> {
        let guid = self.guid.clone();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.transport().stop()
        })
    }

    fn record(&self) -> DawResult<()> {
        let guid = self.guid.clone();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.transport().record()
        })
    }

    fn tempo_bpm(&self) -> f64 {
        let guid = self.guid.clone();
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return 120.0;
            };
            let Ok(project) = mt.project(&guid) else {
                return 120.0;
            };
            project.transport().tempo_bpm()
        })
    }

    fn set_tempo_bpm(&self, bpm: f64) -> DawResult<()> {
        let guid = self.guid.clone();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.transport().set_tempo_bpm(bpm)
        })
    }

    fn is_looping(&self) -> bool {
        let guid = self.guid.clone();
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return false;
            };
            let Ok(project) = mt.project(&guid) else {
                return false;
            };
            project.transport().is_looping()
        })
    }

    fn set_looping(&self, looping: bool) -> DawResult<()> {
        let guid = self.guid.clone();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.transport().set_looping(looping)
        })
    }
}
