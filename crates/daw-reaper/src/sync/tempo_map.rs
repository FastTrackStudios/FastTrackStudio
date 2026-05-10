//! Sync tempo-map handle: [`ReaperTempoMap`].

use daw_proto::sync::TempoMap as TempoMapTrait;
use daw_proto::{DawError, DawResult, Position, TempoPoint, TimePosition, TimeSignature};
use reaper_high::Reaper;
use reaper_medium::{MeasureMode, ProjectContext as ReaperProjectContext};

use crate::safe_wrappers::tempo as sw;
use crate::safe_wrappers::time_map as tw;

use super::ReaperMainThread;

pub struct ReaperTempoMap<'a> {
    _mt: &'a ReaperMainThread,
    guid: &'a str,
}

impl<'a> ReaperTempoMap<'a> {
    pub(crate) fn new(mt: &'a ReaperMainThread, guid: &'a str) -> Self {
        Self { _mt: mt, guid }
    }

    fn ctx(&self) -> DawResult<ReaperProjectContext> {
        super::resolve_reaper_ctx(self.guid)
    }

    fn ctx_or_current(&self) -> ReaperProjectContext {
        self.ctx().unwrap_or(ReaperProjectContext::CurrentProject)
    }
}

fn marker_to_point(m: &sw::TempoMarkerRaw) -> TempoPoint {
    let time_sig = if m.timesig_num > 0 && m.timesig_denom > 0 {
        Some(TimeSignature::new(
            m.timesig_num as u32,
            m.timesig_denom as u32,
        ))
    } else {
        None
    };
    TempoPoint {
        position: Position::from_time(TimePosition::from_seconds(m.timepos)),
        bpm: m.bpm,
        time_signature: time_sig,
        shape: None,
        bezier_tension: None,
        selected: None,
        linear: Some(m.lineartempo),
    }
}

impl<'a> TempoMapTrait for ReaperTempoMap<'a> {
    fn points(&self) -> Vec<TempoPoint> {
        let medium = Reaper::get().medium_reaper();
        let low = medium.low();
        let ctx = self.ctx_or_current();
        let count = medium.count_tempo_time_sig_markers(ctx);
        let mut points = Vec::with_capacity(count as usize);
        for i in 0..count {
            if let Some(m) = sw::get_tempo_marker(low, ctx, i as i32) {
                points.push(marker_to_point(&m));
            }
        }
        points
    }

    fn count(&self) -> u32 {
        let medium = Reaper::get().medium_reaper();
        let ctx = self.ctx_or_current();
        medium.count_tempo_time_sig_markers(ctx)
    }

    fn tempo_at(&self, seconds: f64) -> f64 {
        let reaper = Reaper::get();
        let medium = reaper.medium_reaper();
        let ctx = self.ctx_or_current();
        if let Ok(pos) = reaper_medium::PositionInSeconds::new(seconds) {
            medium.time_map_2_get_divided_bpm_at_time(ctx, pos).get()
        } else if let Ok(project) = super::resolve_project(self.guid) {
            project.tempo().bpm().get()
        } else {
            120.0
        }
    }

    fn time_to_musical(&self, seconds: f64) -> (i32, i32, f64) {
        let medium = Reaper::get().medium_reaper();
        let ctx = self.ctx_or_current();
        if let Ok(pos) = reaper_medium::PositionInSeconds::new(seconds) {
            let result = medium.time_map_2_time_to_beats(ctx, pos);
            let measure = result.measure_index + 1;
            let beats_since = result.beats_since_measure.get();
            let beat_in_measure = beats_since.floor() as i32 + 1;
            let fraction = beats_since.fract();
            (measure, beat_in_measure, fraction)
        } else {
            (1, 1, 0.0)
        }
    }

    fn musical_to_time(&self, measure: i32, beat: i32, frac: f64) -> f64 {
        let medium = Reaper::get().medium_reaper();
        let ctx = self.ctx_or_current();

        let measure_0based = (measure - 1).max(0);
        let beat_0based = (beat - 1).max(0) as f64 + frac;

        let measure_start = tw::get_measure_info(medium.low(), ctx, measure_0based);
        let Ok(measure_start_pos) = reaper_medium::PositionInSeconds::new(measure_start) else {
            return 0.0;
        };
        let measure_start_beats = medium
            .time_map_2_time_to_beats(ctx, measure_start_pos)
            .full_beats;
        let absolute_beats = measure_start_beats.get() + beat_0based;
        let Ok(beats) = reaper_medium::PositionInBeats::new(absolute_beats) else {
            return 0.0;
        };
        medium
            .time_map_2_beats_to_time(ctx, MeasureMode::IgnoreMeasure, beats)
            .get()
    }

    fn add_point(&self, seconds: f64, bpm: f64) -> DawResult<()> {
        let ctx = self.ctx()?;
        let low = Reaper::get().medium_reaper().low();
        let ok = sw::set_tempo_marker(
            low, ctx, -1, // add new
            seconds, -1,   // measurepos (auto)
            -1.0, // beatpos (auto)
            bpm, 0, // timesig_num (don't change)
            0, // timesig_denom (don't change)
            false,
        );
        if !ok {
            return Err(DawError::operation_failed("set_tempo_marker failed"));
        }
        Ok(())
    }

    fn remove_point(&self, idx: u32) -> DawResult<()> {
        let ctx = self.ctx()?;
        let low = Reaper::get().medium_reaper().low();
        sw::delete_tempo_marker(low, ctx, idx as i32);
        Ok(())
    }
}
