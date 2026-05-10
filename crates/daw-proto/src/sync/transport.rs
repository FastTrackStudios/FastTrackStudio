use crate::{DawResult, PlayState, Transport as TransportState};

pub trait Transport {
    /// Full transport snapshot (one call, all fields).
    fn state(&self) -> DawResult<TransportState>;

    fn position(&self) -> f64;
    fn set_position(&self, seconds: f64) -> DawResult<()>;

    fn time_selection(&self) -> Option<(f64, f64)>;
    fn set_time_selection(&self, start: f64, end: f64) -> DawResult<()>;
    fn clear_time_selection(&self) -> DawResult<()>;

    fn play_state(&self) -> PlayState;
    fn play(&self) -> DawResult<()>;
    fn pause(&self) -> DawResult<()>;
    fn stop(&self) -> DawResult<()>;
    fn record(&self) -> DawResult<()>;

    fn tempo_bpm(&self) -> f64;
    fn set_tempo_bpm(&self, bpm: f64) -> DawResult<()>;

    fn is_looping(&self) -> bool;
    fn set_looping(&self, looping: bool) -> DawResult<()>;
}
