//! Sync FX chains handle: [`ReaperFxChains`].

use daw_proto::sync::FxChains as FxChainsTrait;
use daw_proto::{DawError, DawResult, Fx, FxChainContext};
use reaper_high::{FxChain, Reaper, Track};
use reaper_medium::{TrackFxLocation, TransferBehavior};

use crate::fx::build_fx_info;
use crate::safe_wrappers::fx as fx_sw;

use super::ReaperMainThread;

pub struct ReaperFxChains<'a> {
    _mt: &'a ReaperMainThread,
    guid: &'a str,
}

impl<'a> ReaperFxChains<'a> {
    pub(crate) fn new(mt: &'a ReaperMainThread, guid: &'a str) -> Self {
        Self { _mt: mt, guid }
    }

    /// Resolve `(track, chain)` for a given context.
    ///
    /// `Monitoring` is approximated as the master track's input FX chain to
    /// match the existing async impl in `crate::fx::resolve_fx_chain`.
    fn resolve_chain(&self, ctx: &FxChainContext) -> DawResult<(Track, FxChain)> {
        let project = super::resolve_project(self.guid)?;
        match ctx {
            FxChainContext::Track(track_guid) => {
                let track = super::find_track_by_guid(&project, track_guid)
                    .ok_or_else(|| DawError::not_found("Track", track_guid))?;
                let chain = track.normal_fx_chain();
                Ok((track, chain))
            }
            FxChainContext::Input(track_guid) => {
                let track = super::find_track_by_guid(&project, track_guid)
                    .ok_or_else(|| DawError::not_found("Track", track_guid))?;
                let chain = track.input_fx_chain();
                Ok((track, chain))
            }
            FxChainContext::Monitoring => {
                let track = project
                    .master_track()
                    .map_err(|e| DawError::operation_failed(format!("master_track: {e:?}")))?;
                let chain = track.input_fx_chain();
                Ok((track, chain))
            }
        }
    }

    fn resolve_chain_opt(&self, ctx: &FxChainContext) -> Option<(Track, FxChain)> {
        self.resolve_chain(ctx).ok()
    }

    fn fx_location(index: u32, chain: &FxChain) -> TrackFxLocation {
        if chain.is_input_fx() {
            TrackFxLocation::InputFxChain(index)
        } else {
            TrackFxLocation::NormalFxChain(index)
        }
    }
}

impl<'a> FxChainsTrait for ReaperFxChains<'a> {
    fn list(&self, ctx: FxChainContext) -> Vec<Fx> {
        let Some((_track, chain)) = self.resolve_chain_opt(&ctx) else {
            return Vec::new();
        };
        chain
            .fxs()
            .map(|fx| build_fx_info(&fx, Some(&chain)))
            .collect()
    }

    fn count(&self, ctx: FxChainContext) -> u32 {
        self.resolve_chain_opt(&ctx)
            .map(|(_, chain)| chain.fx_count())
            .unwrap_or(0)
    }

    fn get(&self, ctx: FxChainContext, fx_idx: u32) -> Option<Fx> {
        let (_track, chain) = self.resolve_chain_opt(&ctx)?;
        if fx_idx >= chain.fx_count() {
            return None;
        }
        let fx = chain.fx_by_index_untracked(fx_idx);
        Some(build_fx_info(&fx, Some(&chain)))
    }

    fn name(&self, ctx: FxChainContext, fx_idx: u32) -> Option<String> {
        let (_track, chain) = self.resolve_chain_opt(&ctx)?;
        if fx_idx >= chain.fx_count() {
            return None;
        }
        let fx = chain.fx_by_index_untracked(fx_idx);
        Some(fx.name().to_str().to_string())
    }

    fn add(&self, ctx: FxChainContext, name: &str) -> DawResult<u32> {
        let (_track, chain) = self.resolve_chain(&ctx)?;
        let fx = chain.add_fx_by_original_name(name).ok_or_else(|| {
            DawError::operation_failed(format!("add_fx_by_original_name({name}) failed"))
        })?;
        Ok(fx.index())
    }

    fn remove(&self, ctx: FxChainContext, fx_idx: u32) -> DawResult<()> {
        let (track, chain) = self.resolve_chain(&ctx)?;
        if fx_idx >= chain.fx_count() {
            return Err(DawError::out_of_range(
                fx_idx,
                chain.fx_count(),
                "fx_chain.fx_idx",
            ));
        }
        let raw_track = track
            .raw()
            .map_err(|e| DawError::invalid_object("Track", &format!("{e:?}")))?;
        let location = Self::fx_location(fx_idx, &chain);
        fx_sw::track_fx_delete(Reaper::get().medium_reaper(), raw_track, location)
            .map_err(|e| DawError::operation_failed(format!("track_fx_delete: {e:?}")))?;
        Ok(())
    }

    fn move_to(&self, ctx: FxChainContext, from_idx: u32, to_idx: u32) -> DawResult<()> {
        let (track, chain) = self.resolve_chain(&ctx)?;
        let count = chain.fx_count();
        if from_idx >= count {
            return Err(DawError::out_of_range(from_idx, count, "fx_chain.from_idx"));
        }
        if to_idx >= count {
            return Err(DawError::out_of_range(to_idx, count, "fx_chain.to_idx"));
        }
        let raw_track = track
            .raw()
            .map_err(|e| DawError::invalid_object("Track", &format!("{e:?}")))?;
        fx_sw::track_fx_copy_to_track(
            Reaper::get().medium_reaper(),
            (raw_track, Self::fx_location(from_idx, &chain)),
            (raw_track, Self::fx_location(to_idx, &chain)),
            TransferBehavior::Move,
        );
        Ok(())
    }

    fn rename(&self, ctx: FxChainContext, fx_idx: u32, name: &str) -> DawResult<()> {
        let (_track, chain) = self.resolve_chain(&ctx)?;
        if fx_idx >= chain.fx_count() {
            return Err(DawError::out_of_range(
                fx_idx,
                chain.fx_count(),
                "fx_chain.fx_idx",
            ));
        }
        let fx = chain.fx_by_index_untracked(fx_idx);
        fx_sw::fx_set_named_config_param(&fx, "renamed_name", name)
            .map_err(|e| DawError::operation_failed(format!("rename failed: {e}")))
    }

    fn set_enabled(&self, ctx: FxChainContext, fx_idx: u32, enabled: bool) -> DawResult<()> {
        let (_track, chain) = self.resolve_chain(&ctx)?;
        if fx_idx >= chain.fx_count() {
            return Err(DawError::out_of_range(
                fx_idx,
                chain.fx_count(),
                "fx_chain.fx_idx",
            ));
        }
        let fx = chain.fx_by_index_untracked(fx_idx);
        if enabled {
            fx.enable()
                .map_err(|e| DawError::operation_failed(format!("enable: {e}")))
        } else {
            fx.disable()
                .map_err(|e| DawError::operation_failed(format!("disable: {e}")))
        }
    }

    fn set_online(&self, ctx: FxChainContext, fx_idx: u32, online: bool) -> DawResult<()> {
        let (_track, chain) = self.resolve_chain(&ctx)?;
        if fx_idx >= chain.fx_count() {
            return Err(DawError::out_of_range(
                fx_idx,
                chain.fx_count(),
                "fx_chain.fx_idx",
            ));
        }
        let fx = chain.fx_by_index_untracked(fx_idx);
        fx.set_online(online)
            .map_err(|e| DawError::operation_failed(format!("set_online: {e}")))
    }

    fn set_show_ui(&self, ctx: FxChainContext, fx_idx: u32, show: bool) -> DawResult<()> {
        let (_track, chain) = self.resolve_chain(&ctx)?;
        if fx_idx >= chain.fx_count() {
            return Err(DawError::out_of_range(
                fx_idx,
                chain.fx_count(),
                "fx_chain.fx_idx",
            ));
        }
        let fx = chain.fx_by_index_untracked(fx_idx);
        if show {
            let _ = fx.show_in_floating_window();
        } else {
            let _ = fx.hide_floating_window();
        }
        Ok(())
    }

    fn state_chunk(&self, ctx: FxChainContext, fx_idx: u32) -> Option<String> {
        let (_track, chain) = self.resolve_chain_opt(&ctx)?;
        if fx_idx >= chain.fx_count() {
            return None;
        }
        let fx = chain.fx_by_index_untracked(fx_idx);
        // tag_chunk works for VST/VST3; CLAP fallback path is only available
        // through the async service. For sync we return None on tag_chunk failure.
        match fx.tag_chunk() {
            Ok(tag) => Some(tag.content().to_string()),
            Err(_) => None,
        }
    }

    fn set_state_chunk(&self, ctx: FxChainContext, fx_idx: u32, chunk: &str) -> DawResult<()> {
        let (_track, chain) = self.resolve_chain(&ctx)?;
        if fx_idx >= chain.fx_count() {
            return Err(DawError::out_of_range(
                fx_idx,
                chain.fx_count(),
                "fx_chain.fx_idx",
            ));
        }
        let fx = chain.fx_by_index_untracked(fx_idx);
        fx.set_tag_chunk(chunk)
            .map_err(|e| DawError::operation_failed(format!("set_tag_chunk: {e}")))
    }
}
