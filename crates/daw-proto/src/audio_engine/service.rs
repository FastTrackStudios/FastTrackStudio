//! Audio engine service traits.
//!
//! Read-only access to engine state, latency, and audio devices, plus
//! init/quit lifecycle. State is global to the DAW instance — no
//! `ProjectContext` needed.

use super::{AudioEngineState, AudioInputInfo, AudioLatency};
use crate::DawResult;
use vox::service;

#[service]
pub trait AudioEngineService {
    async fn get_state(&self) -> AudioEngineState;
    async fn get_latency(&self) -> AudioLatency;
    /// Convenience: just the output latency in seconds (0.0 when
    /// engine isn't running). Useful for visual sync compensation.
    async fn get_output_latency_seconds(&self) -> f64;
    async fn is_running(&self) -> bool;
    async fn get_audio_inputs(&self) -> AudioInputInfo;

    /// Open all audio + MIDI devices.
    async fn init(&self);
    /// Close all audio + MIDI devices.
    async fn quit(&self);
}

/// Sync handle counterpart.
pub trait AudioEngine {
    fn state(&self) -> DawResult<AudioEngineState>;
    fn latency(&self) -> AudioLatency;
    fn is_running(&self) -> bool;
    fn inputs(&self) -> Vec<AudioInputInfo>;

    fn init(&self) -> DawResult<()>;
    fn quit(&self) -> DawResult<()>;
}
