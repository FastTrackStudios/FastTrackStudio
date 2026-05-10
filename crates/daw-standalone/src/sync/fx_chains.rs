//! `StandaloneFxChains` — sync FX chain sub-handle.

use daw_proto::{DawError, DawResult, Fx, FxChainContext, sync::FxChains};

use super::daw::{FxChainKey, FxEntry, ProjectState, Standalone};

pub struct StandaloneFxChains<'a> {
    daw: &'a Standalone,
    project_guid: String,
}

impl<'a> StandaloneFxChains<'a> {
    pub(crate) fn new(daw: &'a Standalone, project_guid: String) -> Self {
        Self { daw, project_guid }
    }
}

fn reindex(chain: &mut [FxEntry]) {
    for (i, entry) in chain.iter_mut().enumerate() {
        entry.fx.index = i as u32;
    }
}

fn ctx_label(ctx: &FxChainContext) -> String {
    match ctx {
        FxChainContext::Track(g) => format!("Track({})", g),
        FxChainContext::Input(g) => format!("Input({})", g),
        FxChainContext::Monitoring => "Monitoring".to_string(),
    }
}

fn with_chain<R>(
    p: &ProjectState,
    ctx: &FxChainContext,
    f: impl FnOnce(&Vec<FxEntry>) -> R,
    default: R,
) -> R {
    match p.fx_chains.get(&FxChainKey::from(ctx)) {
        Some(chain) => f(chain),
        None => default,
    }
}

impl<'a> FxChains for StandaloneFxChains<'a> {
    fn list(&self, ctx: FxChainContext) -> Vec<Fx> {
        self.daw
            .with_project(&self.project_guid, |p| {
                with_chain(
                    p,
                    &ctx,
                    |chain| chain.iter().map(|e| e.fx.clone()).collect(),
                    Vec::new(),
                )
            })
            .unwrap_or_default()
    }

    fn count(&self, ctx: FxChainContext) -> u32 {
        self.daw
            .with_project(&self.project_guid, |p| {
                with_chain(p, &ctx, |chain| chain.len() as u32, 0)
            })
            .unwrap_or(0)
    }

    fn get(&self, ctx: FxChainContext, fx_idx: u32) -> Option<Fx> {
        self.daw
            .with_project(&self.project_guid, |p| {
                with_chain(
                    p,
                    &ctx,
                    |chain| chain.get(fx_idx as usize).map(|e| e.fx.clone()),
                    None,
                )
            })
            .ok()
            .flatten()
    }

    fn name(&self, ctx: FxChainContext, fx_idx: u32) -> Option<String> {
        self.get(ctx, fx_idx).map(|fx| fx.name)
    }

    fn add(&self, ctx: FxChainContext, name: &str) -> DawResult<u32> {
        self.daw.with_project_mut(&self.project_guid, |p| {
            let counter = p.next_fx_counter;
            p.next_fx_counter += 1;
            let chain = p.fx_chains.entry(FxChainKey::from(&ctx)).or_default();
            let idx = chain.len() as u32;
            let guid = format!("standalone-fx-{:016x}", counter);
            let fx = Fx::new(guid, idx, name.to_string());
            chain.push(FxEntry {
                fx,
                state_chunk: String::new(),
                params: Default::default(),
            });
            idx
        })
    }

    fn remove(&self, ctx: FxChainContext, fx_idx: u32) -> DawResult<()> {
        self.daw.with_project_mut(&self.project_guid, |p| {
            let chain = p
                .fx_chains
                .get_mut(&FxChainKey::from(&ctx))
                .ok_or_else(|| DawError::not_found("FxChain", &ctx_label(&ctx)))?;
            if (fx_idx as usize) >= chain.len() {
                return Err(DawError::not_found("Fx", &fx_idx.to_string()));
            }
            chain.remove(fx_idx as usize);
            reindex(chain);
            Ok::<(), DawError>(())
        })?
    }

    fn move_to(&self, ctx: FxChainContext, from_idx: u32, to_idx: u32) -> DawResult<()> {
        self.daw.with_project_mut(&self.project_guid, |p| {
            let chain = p
                .fx_chains
                .get_mut(&FxChainKey::from(&ctx))
                .ok_or_else(|| DawError::not_found("FxChain", &ctx_label(&ctx)))?;
            let len = chain.len();
            if (from_idx as usize) >= len {
                return Err(DawError::not_found("Fx", &from_idx.to_string()));
            }
            let to = (to_idx as usize).min(len.saturating_sub(1));
            let entry = chain.remove(from_idx as usize);
            chain.insert(to, entry);
            reindex(chain);
            Ok::<(), DawError>(())
        })?
    }

    fn rename(&self, ctx: FxChainContext, fx_idx: u32, name: &str) -> DawResult<()> {
        self.daw.with_project_mut(&self.project_guid, |p| {
            let chain = p
                .fx_chains
                .get_mut(&FxChainKey::from(&ctx))
                .ok_or_else(|| DawError::not_found("FxChain", &ctx_label(&ctx)))?;
            let entry = chain
                .get_mut(fx_idx as usize)
                .ok_or_else(|| DawError::not_found("Fx", &fx_idx.to_string()))?;
            entry.fx.name = name.to_string();
            Ok::<(), DawError>(())
        })?
    }

    fn set_enabled(&self, ctx: FxChainContext, fx_idx: u32, enabled: bool) -> DawResult<()> {
        self.daw.with_project_mut(&self.project_guid, |p| {
            let chain = p
                .fx_chains
                .get_mut(&FxChainKey::from(&ctx))
                .ok_or_else(|| DawError::not_found("FxChain", &ctx_label(&ctx)))?;
            let entry = chain
                .get_mut(fx_idx as usize)
                .ok_or_else(|| DawError::not_found("Fx", &fx_idx.to_string()))?;
            entry.fx.enabled = enabled;
            Ok::<(), DawError>(())
        })?
    }

    fn set_online(&self, ctx: FxChainContext, fx_idx: u32, online: bool) -> DawResult<()> {
        self.daw.with_project_mut(&self.project_guid, |p| {
            let chain = p
                .fx_chains
                .get_mut(&FxChainKey::from(&ctx))
                .ok_or_else(|| DawError::not_found("FxChain", &ctx_label(&ctx)))?;
            let entry = chain
                .get_mut(fx_idx as usize)
                .ok_or_else(|| DawError::not_found("Fx", &fx_idx.to_string()))?;
            entry.fx.offline = !online;
            Ok::<(), DawError>(())
        })?
    }

    fn set_show_ui(&self, ctx: FxChainContext, fx_idx: u32, show: bool) -> DawResult<()> {
        self.daw.with_project_mut(&self.project_guid, |p| {
            let chain = p
                .fx_chains
                .get_mut(&FxChainKey::from(&ctx))
                .ok_or_else(|| DawError::not_found("FxChain", &ctx_label(&ctx)))?;
            let entry = chain
                .get_mut(fx_idx as usize)
                .ok_or_else(|| DawError::not_found("Fx", &fx_idx.to_string()))?;
            entry.fx.window_open = show;
            Ok::<(), DawError>(())
        })?
    }

    fn state_chunk(&self, ctx: FxChainContext, fx_idx: u32) -> Option<String> {
        self.daw
            .with_project(&self.project_guid, |p| {
                with_chain(
                    p,
                    &ctx,
                    |chain| chain.get(fx_idx as usize).map(|e| e.state_chunk.clone()),
                    None,
                )
            })
            .ok()
            .flatten()
    }

    fn set_state_chunk(&self, ctx: FxChainContext, fx_idx: u32, chunk: &str) -> DawResult<()> {
        self.daw.with_project_mut(&self.project_guid, |p| {
            let chain = p
                .fx_chains
                .get_mut(&FxChainKey::from(&ctx))
                .ok_or_else(|| DawError::not_found("FxChain", &ctx_label(&ctx)))?;
            let entry = chain
                .get_mut(fx_idx as usize)
                .ok_or_else(|| DawError::not_found("Fx", &fx_idx.to_string()))?;
            entry.state_chunk = chunk.to_string();
            Ok::<(), DawError>(())
        })?
    }
}
