//! Remote FX params handle: [`RemoteFxParams`].

use daw_proto::sync::{Daw as _, FxParams as FxParamsTrait, Project as _};
use daw_proto::{DawResult, FxChainContext, FxParameter};

use super::{ReaperRemote, dispatch, dispatch_read, main_thread};

pub struct RemoteFxParams<'a> {
    remote: &'a ReaperRemote,
    guid: String,
}

impl<'a> RemoteFxParams<'a> {
    pub(crate) fn new(remote: &'a ReaperRemote, guid: &str) -> Self {
        Self {
            remote,
            guid: guid.to_string(),
        }
    }
}

impl<'a> FxParamsTrait for RemoteFxParams<'a> {
    fn count(&self, ctx: FxChainContext, fx_idx: u32) -> u32 {
        let guid = self.guid.clone();
        let ctx = ctx.clone();
        dispatch_read(self.remote, move || {
            let Some(mt) = crate::sync::ReaperMainThread::try_new() else {
                return 0;
            };
            let Ok(project) = mt.project(&guid) else {
                return 0;
            };
            project.fx_params().count(ctx, fx_idx)
        })
    }

    fn get(&self, ctx: FxChainContext, fx_idx: u32, param_idx: u32) -> Option<f64> {
        let guid = self.guid.clone();
        let ctx = ctx.clone();
        self.remote.runtime.block_on(async move {
            crate::main_thread::query(move || {
                let mt = crate::sync::ReaperMainThread::try_new()?;
                let project = mt.project(&guid).ok()?;
                project.fx_params().get(ctx, fx_idx, param_idx)
            })
            .await
            .flatten()
        })
    }

    fn set(&self, ctx: FxChainContext, fx_idx: u32, param_idx: u32, value: f64) -> DawResult<()> {
        let guid = self.guid.clone();
        let ctx = ctx.clone();
        dispatch(self.remote, move || {
            let mt = main_thread()?;
            mt.project(&guid)?
                .fx_params()
                .set(ctx, fx_idx, param_idx, value)
        })
    }

    fn name(&self, ctx: FxChainContext, fx_idx: u32, param_idx: u32) -> Option<String> {
        let guid = self.guid.clone();
        let ctx = ctx.clone();
        self.remote.runtime.block_on(async move {
            crate::main_thread::query(move || {
                let mt = crate::sync::ReaperMainThread::try_new()?;
                let project = mt.project(&guid).ok()?;
                project.fx_params().name(ctx, fx_idx, param_idx)
            })
            .await
            .flatten()
        })
    }

    fn info(&self, ctx: FxChainContext, fx_idx: u32, param_idx: u32) -> Option<FxParameter> {
        let guid = self.guid.clone();
        let ctx = ctx.clone();
        self.remote.runtime.block_on(async move {
            crate::main_thread::query(move || {
                let mt = crate::sync::ReaperMainThread::try_new()?;
                let project = mt.project(&guid).ok()?;
                project.fx_params().info(ctx, fx_idx, param_idx)
            })
            .await
            .flatten()
        })
    }
}
