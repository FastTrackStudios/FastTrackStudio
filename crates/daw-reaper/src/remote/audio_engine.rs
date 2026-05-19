//! Remote audio engine handle: [`RemoteAudioEngine`].

use daw_proto::{AudioEngine as AudioEngineTrait, Daw as _};
use daw_proto::{AudioEngineState, AudioInputInfo, AudioLatency, DawResult};

use super::{ReaperRemote, dispatch, dispatch_read, main_thread};

pub struct RemoteAudioEngine<'a> {
    remote: &'a ReaperRemote,
}

impl<'a> RemoteAudioEngine<'a> {
    pub(crate) fn new(remote: &'a ReaperRemote) -> Self {
        Self { remote }
    }
}

impl<'a> AudioEngineTrait for RemoteAudioEngine<'a> {
    fn state(&self) -> DawResult<AudioEngineState> {
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.audio_engine().state()
        })
    }

    fn latency(&self) -> AudioLatency {
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return AudioLatency::default();
            };
            mt.audio_engine().latency()
        })
    }

    fn is_running(&self) -> bool {
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return false;
            };
            mt.audio_engine().is_running()
        })
    }

    fn inputs(&self) -> Vec<AudioInputInfo> {
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return Vec::new();
            };
            mt.audio_engine().inputs()
        })
    }

    fn init(&self) -> DawResult<()> {
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.audio_engine().init()
        })
    }

    fn quit(&self) -> DawResult<()> {
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.audio_engine().quit()
        })
    }
}
