//! `StandaloneFxParams` — sync FX parameter sub-handle.

use daw_proto::{DawError, DawResult, FxChainContext, FxParameter, sync::FxParams};

use super::daw::{FxChainKey, FxEntry, ProjectState, Standalone};

pub struct StandaloneFxParams<'a> {
    daw: &'a Standalone,
    project_guid: String,
}

impl<'a> StandaloneFxParams<'a> {
    pub(crate) fn new(daw: &'a Standalone, project_guid: String) -> Self {
        Self { daw, project_guid }
    }
}

fn ctx_label(ctx: &FxChainContext) -> String {
    match ctx {
        FxChainContext::Track(g) => format!("Track({})", g),
        FxChainContext::Input(g) => format!("Input({})", g),
        FxChainContext::Monitoring => "Monitoring".to_string(),
    }
}

fn entry<'p>(p: &'p ProjectState, ctx: &FxChainContext, fx_idx: u32) -> Option<&'p FxEntry> {
    p.fx_chains
        .get(&FxChainKey::from(ctx))
        .and_then(|chain| chain.get(fx_idx as usize))
}

impl<'a> FxParams for StandaloneFxParams<'a> {
    fn count(&self, ctx: FxChainContext, fx_idx: u32) -> u32 {
        self.daw
            .with_project(&self.project_guid, |p| {
                entry(p, &ctx, fx_idx)
                    .map(|e| e.fx.parameter_count)
                    .unwrap_or(0)
            })
            .unwrap_or(0)
    }

    fn get(&self, ctx: FxChainContext, fx_idx: u32, param_idx: u32) -> Option<f64> {
        self.daw
            .with_project(&self.project_guid, |p| {
                entry(p, &ctx, fx_idx).and_then(|e| e.params.get(&param_idx).copied())
            })
            .ok()
            .flatten()
    }

    fn set(&self, ctx: FxChainContext, fx_idx: u32, param_idx: u32, value: f64) -> DawResult<()> {
        self.daw.with_project_mut(&self.project_guid, |p| {
            let chain = p
                .fx_chains
                .get_mut(&FxChainKey::from(&ctx))
                .ok_or_else(|| DawError::not_found("FxChain", &ctx_label(&ctx)))?;
            let e = chain
                .get_mut(fx_idx as usize)
                .ok_or_else(|| DawError::not_found("Fx", &fx_idx.to_string()))?;
            e.params.insert(param_idx, value);
            // Track the highest seen param idx so `count` reflects activity.
            if param_idx + 1 > e.fx.parameter_count {
                e.fx.parameter_count = param_idx + 1;
            }
            Ok::<(), DawError>(())
        })?
    }

    fn name(&self, ctx: FxChainContext, fx_idx: u32, param_idx: u32) -> Option<String> {
        // No real plugin metadata in standalone — synthesize a stable name.
        self.daw
            .with_project(&self.project_guid, |p| {
                entry(p, &ctx, fx_idx).map(|_| format!("Param {}", param_idx))
            })
            .ok()
            .flatten()
    }

    fn info(&self, ctx: FxChainContext, fx_idx: u32, param_idx: u32) -> Option<FxParameter> {
        self.daw
            .with_project(&self.project_guid, |p| {
                entry(p, &ctx, fx_idx).map(|e| {
                    let value = e.params.get(&param_idx).copied().unwrap_or(0.0);
                    FxParameter::new(param_idx, format!("Param {}", param_idx), value)
                })
            })
            .ok()
            .flatten()
    }
}
