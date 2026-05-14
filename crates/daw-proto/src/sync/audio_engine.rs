//! Audio engine state queries.

use crate::{AudioEngineState, AudioInputInfo, AudioLatency, DawResult};

pub trait AudioEngine {
    fn state(&self) -> DawResult<AudioEngineState>;
    fn latency(&self) -> AudioLatency;
    fn is_running(&self) -> bool;
    fn inputs(&self) -> Vec<AudioInputInfo>;

    fn init(&self) -> DawResult<()>;
    fn quit(&self) -> DawResult<()>;
}
