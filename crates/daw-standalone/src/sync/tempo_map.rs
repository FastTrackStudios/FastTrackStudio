//! `StandaloneTempoMap` — sync tempo map sub-handle.
//!
//! Tempo math here is intentionally simple: we treat the *first* tempo point's
//! BPM and time signature as the global tempo for `time_to_musical` /
//! `musical_to_time`. No tempo curves, no time-signature changes mid-project.
//! This is fine for tests; production tempo math lives in the REAPER backend.

use daw_proto::{DawError, DawResult, TempoPoint, sync::TempoMap};

use super::daw::Standalone;

pub struct StandaloneTempoMap<'a> {
    daw: &'a Standalone,
    guid: String,
}

impl<'a> StandaloneTempoMap<'a> {
    pub(crate) fn new(daw: &'a Standalone, guid: String) -> Self {
        Self { daw, guid }
    }

    /// Returns `(bpm, beats_per_measure)` from the first tempo point, falling
    /// back to the project's transport tempo / time signature, then to defaults.
    fn tempo_and_meter(&self) -> (f64, f64) {
        self.daw
            .with_project(&self.guid, |p| {
                let bpm = p
                    .tempo_points
                    .first()
                    .map(|t| t.bpm)
                    .unwrap_or_else(|| p.transport.tempo.bpm());
                let beats_per_measure = p
                    .tempo_points
                    .first()
                    .and_then(|t| t.time_signature)
                    .unwrap_or(p.transport.time_signature)
                    .numerator() as f64;
                (bpm, beats_per_measure)
            })
            .unwrap_or((120.0, 4.0))
    }
}

impl<'a> TempoMap for StandaloneTempoMap<'a> {
    fn points(&self) -> Vec<TempoPoint> {
        self.daw
            .with_project(&self.guid, |p| p.tempo_points.clone())
            .unwrap_or_default()
    }

    fn count(&self) -> u32 {
        self.daw
            .with_project(&self.guid, |p| p.tempo_points.len() as u32)
            .unwrap_or(0)
    }

    fn tempo_at(&self, seconds: f64) -> f64 {
        // Walk points, return the BPM of the latest point at or before `seconds`.
        self.daw
            .with_project(&self.guid, |p| {
                let mut bpm = p.transport.tempo.bpm();
                if let Some(first) = p.tempo_points.first() {
                    bpm = first.bpm;
                }
                for tp in &p.tempo_points {
                    if tp.position_seconds() <= seconds {
                        bpm = tp.bpm;
                    } else {
                        break;
                    }
                }
                bpm
            })
            .unwrap_or(120.0)
    }

    fn time_to_musical(&self, seconds: f64) -> (i32, i32, f64) {
        let (bpm, beats_per_measure) = self.tempo_and_meter();
        if bpm <= 0.0 || beats_per_measure <= 0.0 {
            return (0, 0, 0.0);
        }
        let total_beats = seconds * bpm / 60.0;
        let measure = (total_beats / beats_per_measure).floor() as i32;
        let beat_in_measure = total_beats - (measure as f64 * beats_per_measure);
        let beat = beat_in_measure.floor() as i32;
        let frac = beat_in_measure - beat as f64;
        (measure, beat, frac)
    }

    fn musical_to_time(&self, measure: i32, beat: i32, frac: f64) -> f64 {
        let (bpm, beats_per_measure) = self.tempo_and_meter();
        if bpm <= 0.0 {
            return 0.0;
        }
        let total_beats = measure as f64 * beats_per_measure + beat as f64 + frac;
        total_beats * 60.0 / bpm
    }

    fn add_point(&self, seconds: f64, bpm: f64) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            p.tempo_points.push(TempoPoint::from_seconds(seconds, bpm));
            // Keep ordered by position so iteration in `tempo_at` is correct.
            p.tempo_points.sort_by(|a, b| {
                a.position_seconds()
                    .partial_cmp(&b.position_seconds())
                    .unwrap_or(std::cmp::Ordering::Equal)
            });
        })
    }

    fn remove_point(&self, idx: u32) -> DawResult<()> {
        self.daw.with_project_mut(&self.guid, |p| {
            let i = idx as usize;
            if i >= p.tempo_points.len() {
                return Err(DawError::out_of_range(
                    idx,
                    p.tempo_points.len() as u32,
                    "tempo_points",
                ));
            }
            p.tempo_points.remove(i);
            Ok(())
        })?
    }
}
