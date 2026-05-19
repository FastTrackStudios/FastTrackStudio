//! Sync `AudioEngine` impl — direct reaper-medium calls on the main thread.

use daw_proto::{
    AudioEngineState, AudioInputChannel, AudioInputInfo, AudioLatency, DawError, DawResult,
    sync::AudioEngine,
};
use reaper_high::Reaper;

use crate::audio_engine::get_audio_latency_internal;
use crate::safe_wrappers::audio as sw;

use super::ReaperMainThread;

pub struct ReaperAudioEngine<'a> {
    _mt: &'a ReaperMainThread,
}

impl<'a> ReaperAudioEngine<'a> {
    pub(crate) fn new(mt: &'a ReaperMainThread) -> Self {
        Self { _mt: mt }
    }
}

impl<'a> AudioEngine for ReaperAudioEngine<'a> {
    fn state(&self) -> DawResult<AudioEngineState> {
        let reaper = Reaper::get();
        let medium = reaper.medium_reaper();
        let is_running = medium.audio_is_running();
        let is_prebuffer = medium.low().Audio_IsPreBuffer() != 0;
        let latency = get_audio_latency_internal(medium);
        Ok(AudioEngineState {
            is_running,
            is_prebuffer,
            latency,
        })
    }

    fn latency(&self) -> AudioLatency {
        let reaper = Reaper::get();
        get_audio_latency_internal(reaper.medium_reaper())
    }

    fn is_running(&self) -> bool {
        Reaper::get().medium_reaper().audio_is_running()
    }

    fn inputs(&self) -> Vec<AudioInputInfo> {
        let reaper = Reaper::get();
        let medium = reaper.medium_reaper();
        let low = medium.low();
        let device_name = sw::get_audio_device_info(low, c"IDENT_IN", 256).unwrap_or_default();
        let num_inputs = low.GetNumAudioInputs() as u32;
        let channels: Vec<AudioInputChannel> = (0..num_inputs)
            .map(|i| {
                let name = medium.get_input_channel_name(i, |cstr| {
                    cstr.map(|s| s.to_string_lossy().into_owned())
                        .unwrap_or_else(|| format!("Input {}", i + 1))
                });
                AudioInputChannel { index: i, name }
            })
            .collect();
        vec![AudioInputInfo {
            device_name,
            channels,
        }]
    }

    fn init(&self) -> DawResult<()> {
        Reaper::get().medium_reaper().low().Audio_Init();
        Ok(())
    }

    fn quit(&self) -> DawResult<()> {
        Reaper::get().medium_reaper().low().Audio_Quit();
        Ok(())
    }
}

// Silence unused-import lint when DawError isn't otherwise referenced.
#[allow(dead_code)]
fn _types(_: DawError) {}
