//! Sync transport handle: [`ReaperTransport`].

use daw_proto::sync::Transport as TransportTrait;
use daw_proto::{DawError, DawResult, PlayState, Transport as TransportState};
use reaper_high::Reaper;
use reaper_medium::{
    AutoSeekBehavior, CommandId, PositionInSeconds, ProjectContext as ReaperProjectContext,
    SetEditCurPosOptions, TimeRangeType, UndoBehavior,
};

use crate::transport::read_transport_state_for_project;

use super::ReaperMainThread;

/// Per-project sync transport handle.
pub struct ReaperTransport<'a> {
    _mt: &'a ReaperMainThread,
    guid: &'a str,
}

impl<'a> ReaperTransport<'a> {
    pub(crate) fn new(mt: &'a ReaperMainThread, guid: &'a str) -> Self {
        Self { _mt: mt, guid }
    }

    fn ctx(&self) -> DawResult<ReaperProjectContext> {
        super::resolve_reaper_ctx(self.guid)
    }
}

impl<'a> TransportTrait for ReaperTransport<'a> {
    fn state(&self) -> DawResult<TransportState> {
        let project = super::resolve_project(self.guid)?;
        let reaper = Reaper::get();
        let medium = reaper.medium_reaper();
        let reaper_ctx = ReaperProjectContext::Proj(project.raw());
        Ok(read_transport_state_for_project(
            &project, reaper_ctx, medium,
        ))
    }

    fn position(&self) -> f64 {
        let Ok(project) = super::resolve_project(self.guid) else {
            return 0.0;
        };
        project
            .play_or_edit_cursor_position()
            .map(|p| p.get())
            .unwrap_or(0.0)
    }

    fn set_position(&self, seconds: f64) -> DawResult<()> {
        let project = super::resolve_project(self.guid)?;
        let pos = PositionInSeconds::new(seconds)
            .map_err(|e| DawError::operation_failed(format!("invalid position: {e:?}")))?;
        project.set_edit_cursor_position(
            pos,
            SetEditCurPosOptions {
                move_view: false,
                seek_play: true,
            },
        );
        Ok(())
    }

    fn time_selection(&self) -> Option<(f64, f64)> {
        let ctx = self.ctx().ok()?;
        let medium = Reaper::get().medium_reaper();
        let range = medium.get_set_loop_time_range_2_get(ctx, TimeRangeType::TimeSelection)?;
        Some((range.start.get(), range.end.get()))
    }

    fn set_time_selection(&self, start: f64, end: f64) -> DawResult<()> {
        let ctx = self.ctx()?;
        let s = start.min(end).max(0.0);
        let e = start.max(end).max(0.0);
        let s = PositionInSeconds::new(s)
            .map_err(|e| DawError::operation_failed(format!("invalid start: {e:?}")))?;
        let e = PositionInSeconds::new(e)
            .map_err(|e| DawError::operation_failed(format!("invalid end: {e:?}")))?;
        Reaper::get().medium_reaper().get_set_loop_time_range_2_set(
            ctx,
            TimeRangeType::TimeSelection,
            s,
            e,
            AutoSeekBehavior::DenyAutoSeek,
        );
        Ok(())
    }

    fn clear_time_selection(&self) -> DawResult<()> {
        let ctx = self.ctx()?;
        Reaper::get().medium_reaper().get_set_loop_time_range_2_set(
            ctx,
            TimeRangeType::TimeSelection,
            PositionInSeconds::new_panic(0.0),
            PositionInSeconds::new_panic(0.0),
            AutoSeekBehavior::DenyAutoSeek,
        );
        Ok(())
    }

    fn play_state(&self) -> PlayState {
        let Ok(ctx) = self.ctx() else {
            return PlayState::Stopped;
        };
        let state = Reaper::get().medium_reaper().get_play_state_ex(ctx);
        if state.is_recording {
            PlayState::Recording
        } else if state.is_playing {
            PlayState::Playing
        } else if state.is_paused {
            PlayState::Paused
        } else {
            PlayState::Stopped
        }
    }

    fn play(&self) -> DawResult<()> {
        run_command(1007)
    }

    fn pause(&self) -> DawResult<()> {
        run_command(1008)
    }

    fn stop(&self) -> DawResult<()> {
        run_command(1016)
    }

    fn record(&self) -> DawResult<()> {
        run_command(1013)
    }

    fn tempo_bpm(&self) -> f64 {
        super::resolve_project(self.guid)
            .map(|p| p.tempo().bpm().get())
            .unwrap_or(120.0)
    }

    fn set_tempo_bpm(&self, bpm: f64) -> DawResult<()> {
        let project = super::resolve_project(self.guid)?;
        let bpm_value = reaper_medium::Bpm::new(bpm)
            .map_err(|e| DawError::operation_failed(format!("invalid bpm: {e:?}")))?;
        let tempo = reaper_high::Tempo::from_bpm(bpm_value);
        project
            .set_tempo(tempo, UndoBehavior::OmitUndoPoint)
            .map_err(|e| DawError::operation_failed(format!("set_tempo failed: {e:?}")))?;
        Ok(())
    }

    fn is_looping(&self) -> bool {
        let Ok(ctx) = self.ctx() else { return false };
        Reaper::get().medium_reaper().get_set_repeat_ex_get(ctx)
    }

    fn set_looping(&self, looping: bool) -> DawResult<()> {
        let ctx = self.ctx()?;
        Reaper::get()
            .medium_reaper()
            .get_set_repeat_ex_set(ctx, looping);
        Ok(())
    }
}

fn run_command(id: u32) -> DawResult<()> {
    Reaper::get().medium_reaper().main_on_command_ex(
        CommandId::new(id),
        0,
        ReaperProjectContext::CurrentProject,
    );
    Ok(())
}
