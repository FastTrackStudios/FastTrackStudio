use crate::{DawResult, TempoPoint};

pub trait TempoMap {
    fn points(&self) -> Vec<TempoPoint>;
    fn count(&self) -> u32;

    fn tempo_at(&self, seconds: f64) -> f64;
    fn time_to_musical(&self, seconds: f64) -> (i32, i32, f64);
    fn musical_to_time(&self, measure: i32, beat: i32, frac: f64) -> f64;

    fn add_point(&self, seconds: f64, bpm: f64) -> DawResult<()>;
    fn remove_point(&self, idx: u32) -> DawResult<()>;
}
