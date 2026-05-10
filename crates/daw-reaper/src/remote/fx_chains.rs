//! Remote FX chains handle: [`RemoteFxChains`].

use daw_proto::sync::{Daw as _, FxChains as FxChainsTrait, Project as _};
use daw_proto::{DawResult, Fx, FxChainContext};

use super::{ReaperRemote, dispatch, dispatch_read, main_thread};

pub struct RemoteFxChains<'a> {
    remote: &'a ReaperRemote,
    guid: String,
}

impl<'a> RemoteFxChains<'a> {
    pub(crate) fn new(remote: &'a ReaperRemote, guid: &str) -> Self {
        Self {
            remote,
            guid: guid.to_string(),
        }
    }
}

impl<'a> FxChainsTrait for RemoteFxChains<'a> {
    fn list(&self, ctx: FxChainContext) -> Vec<Fx> {
        let guid = self.guid.clone();
        let ctx = ctx.clone();
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return Vec::new();
            };
            let Ok(project) = mt.project(&guid) else {
                return Vec::new();
            };
            project.fx_chains().list(ctx)
        })
    }

    fn count(&self, ctx: FxChainContext) -> u32 {
        let guid = self.guid.clone();
        let ctx = ctx.clone();
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return 0;
            };
            let Ok(project) = mt.project(&guid) else {
                return 0;
            };
            project.fx_chains().count(ctx)
        })
    }

    fn get(&self, ctx: FxChainContext, fx_idx: u32) -> Option<Fx> {
        let guid = self.guid.clone();
        let ctx = ctx.clone();
        self.remote.runtime.block_on(async move {
            crate::main_thread::query(move || {
                let mt = crate::sync::ReaperMainThread::try_new()?;
                let project = mt.project(&guid).ok()?;
                project.fx_chains().get(ctx, fx_idx)
            })
            .await
            .flatten()
        })
    }

    fn name(&self, ctx: FxChainContext, fx_idx: u32) -> Option<String> {
        let guid = self.guid.clone();
        let ctx = ctx.clone();
        self.remote.runtime.block_on(async move {
            crate::main_thread::query(move || {
                let mt = crate::sync::ReaperMainThread::try_new()?;
                let project = mt.project(&guid).ok()?;
                project.fx_chains().name(ctx, fx_idx)
            })
            .await
            .flatten()
        })
    }

    fn add(&self, ctx: FxChainContext, name: &str) -> DawResult<u32> {
        let guid = self.guid.clone();
        let ctx = ctx.clone();
        let name = name.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.fx_chains().add(ctx, &name)
        })
    }

    fn remove(&self, ctx: FxChainContext, fx_idx: u32) -> DawResult<()> {
        let guid = self.guid.clone();
        let ctx = ctx.clone();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.fx_chains().remove(ctx, fx_idx)
        })
    }

    fn move_to(&self, ctx: FxChainContext, from_idx: u32, to_idx: u32) -> DawResult<()> {
        let guid = self.guid.clone();
        let ctx = ctx.clone();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?
                .fx_chains()
                .move_to(ctx, from_idx, to_idx)
        })
    }

    fn rename(&self, ctx: FxChainContext, fx_idx: u32, name: &str) -> DawResult<()> {
        let guid = self.guid.clone();
        let ctx = ctx.clone();
        let name = name.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?.fx_chains().rename(ctx, fx_idx, &name)
        })
    }

    fn set_enabled(&self, ctx: FxChainContext, fx_idx: u32, enabled: bool) -> DawResult<()> {
        let guid = self.guid.clone();
        let ctx = ctx.clone();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?
                .fx_chains()
                .set_enabled(ctx, fx_idx, enabled)
        })
    }

    fn set_online(&self, ctx: FxChainContext, fx_idx: u32, online: bool) -> DawResult<()> {
        let guid = self.guid.clone();
        let ctx = ctx.clone();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?
                .fx_chains()
                .set_online(ctx, fx_idx, online)
        })
    }

    fn set_show_ui(&self, ctx: FxChainContext, fx_idx: u32, show: bool) -> DawResult<()> {
        let guid = self.guid.clone();
        let ctx = ctx.clone();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?
                .fx_chains()
                .set_show_ui(ctx, fx_idx, show)
        })
    }

    fn state_chunk(&self, ctx: FxChainContext, fx_idx: u32) -> Option<String> {
        let guid = self.guid.clone();
        let ctx = ctx.clone();
        self.remote.runtime.block_on(async move {
            crate::main_thread::query(move || {
                let mt = crate::sync::ReaperMainThread::try_new()?;
                let project = mt.project(&guid).ok()?;
                project.fx_chains().state_chunk(ctx, fx_idx)
            })
            .await
            .flatten()
        })
    }

    fn set_state_chunk(&self, ctx: FxChainContext, fx_idx: u32, chunk: &str) -> DawResult<()> {
        let guid = self.guid.clone();
        let ctx = ctx.clone();
        let chunk = chunk.to_string();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?
                .fx_chains()
                .set_state_chunk(ctx, fx_idx, &chunk)
        })
    }
}
