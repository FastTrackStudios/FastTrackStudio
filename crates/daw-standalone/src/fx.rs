//! `impl Effects for Standalone` — backed by `ProjectState.fx_chains`.
//!
//! Standalone has no real plugin host, so every "plugin" added is a
//! synthetic FX with 8 generic continuous parameters (0..=1.0). That's
//! enough to exercise routing / chain manipulation / state-chunk
//! round-tripping without a CLAP/VST3 backend.
//!
//! Implemented surface:
//! - `list_installed` — returns a small canned list of "built-in" FX
//!   names; future audio-graph FX integration replaces this.
//! - Chain queries: `list` / `get` / `count`
//! - State: `set_enabled` / `set_offline`
//! - Management: `add` / `add_at` / `remove` / `move_to`
//! - Parameters: `parameters` / `parameter` / `parameter_by_name` /
//!   `set_parameter` / `set_parameter_by_name`
//! - UI: `open_ui` / `close_ui` / `toggle_ui` (mutates `window_open`)
//! - State chunks: `state_chunk` / `set_state_chunk` /
//!   `state_chunk_encoded` / `set_state_chunk_encoded` /
//!   `chain_state` / `set_chain_state`
//!
//! Still `todo!()`: containers / `tree` / latency / param modulation /
//! channel config / preset cycle / named-config / chain chunk text.
//! These need either real plugin metadata or a deeper modeling pass.

use daw_proto::fx::{
    AddFxAtRequest, CreateContainerRequest, EncloseInContainerRequest, Fx, FxChainContext,
    FxChannelConfig, FxContainerChannelConfig, FxLatency, FxNodeId, FxParamModulation, FxParameter,
    FxPinMappings, FxPresetIndex, FxRef, FxRoutingMode, FxStateChunk, FxTarget, FxTree, FxType,
    InstalledFx, LastTouchedFx, MoveFromContainerRequest, MoveToContainerRequest,
    SetContainerChannelConfigRequest, SetNamedConfigRequest, SetParameterByNameRequest,
    SetParameterRequest,
};
use daw_proto::project::ProjectContext;
use daw_proto::{DawError, DawResult, fx::Effects};
use uuid::Uuid;

use crate::sync::{FxChainKey, FxEntry, Standalone};

fn resolve_project(daw: &Standalone, ctx: &ProjectContext) -> Option<String> {
    match ctx {
        ProjectContext::Project(guid) => Some(guid.clone()),
        ProjectContext::Current => {
            let state = daw.state.lock().ok()?;
            state.current_project_guid.clone()
        }
    }
}

fn not_found_proj() -> DawError {
    DawError::not_found("Project", "context")
}

fn not_found_fx() -> DawError {
    DawError::not_found("Fx", "")
}

fn find_fx_index(chain: &[FxEntry], fx_ref: &FxRef) -> Option<usize> {
    match fx_ref {
        FxRef::Guid(g) => chain.iter().position(|e| e.fx.guid == *g),
        FxRef::Index(i) => {
            let u = *i as usize;
            (u < chain.len()).then_some(u)
        }
        FxRef::Name(name) => chain.iter().position(|e| e.fx.plugin_name == *name),
    }
}

/// Synthesize 8 generic parameters for a new FX. Provides enough surface
/// for tests + UI prototypes without needing real plugin metadata.
fn default_params() -> std::collections::HashMap<u32, f64> {
    (0..8u32).map(|i| (i, 0.5)).collect()
}

fn param_name(idx: u32) -> String {
    format!("Param {}", idx + 1)
}

/// Built-in FX list — returned by `list_installed`. These are aspirational
/// — the audio-graph integration replaces this with real installed FX
/// (FastTrackStudio DSP, CLAP plugins) once available.
fn built_in_installed() -> Vec<InstalledFx> {
    [
        ("ReaComp", "JS: ReaComp"),
        ("ReaEQ", "JS: ReaEQ"),
        ("ReaGate", "JS: ReaGate"),
        ("ReaDelay", "JS: ReaDelay"),
        ("ReaVerbate", "JS: ReaVerbate"),
    ]
    .into_iter()
    .map(|(name, ident)| InstalledFx {
        name: name.to_string(),
        ident: ident.to_string(),
    })
    .collect()
}

impl Effects for Standalone {
    fn list_installed(&self) -> Vec<InstalledFx> {
        built_in_installed()
    }

    fn last_touched(&self) -> Option<LastTouchedFx> {
        let s = self.state.lock().ok()?;
        s.last_touched_fx.clone()
    }

    fn list(&self, project: ProjectContext, chain: FxChainContext) -> Vec<Fx> {
        let Some(guid) = resolve_project(self, &project) else {
            return Vec::new();
        };
        let key: FxChainKey = (&chain).into();
        self.with_project(&guid, |p| {
            p.fx_chains
                .get(&key)
                .map(|c| c.iter().map(|e| e.fx.clone()).collect())
                .unwrap_or_default()
        })
        .unwrap_or_default()
    }

    fn get(&self, project: ProjectContext, target: FxTarget) -> Option<Fx> {
        let guid = resolve_project(self, &project)?;
        let key: FxChainKey = (&target.context).into();
        self.with_project(&guid, |p| {
            let chain = p.fx_chains.get(&key)?;
            let i = find_fx_index(chain, &target.fx)?;
            Some(chain[i].fx.clone())
        })
        .ok()
        .flatten()
    }

    fn count(&self, project: ProjectContext, chain: FxChainContext) -> u32 {
        let Some(guid) = resolve_project(self, &project) else {
            return 0;
        };
        let key: FxChainKey = (&chain).into();
        self.with_project(&guid, |p| {
            p.fx_chains.get(&key).map(|c| c.len() as u32).unwrap_or(0)
        })
        .unwrap_or(0)
    }

    fn set_enabled(
        &self,
        project: ProjectContext,
        target: FxTarget,
        enabled: bool,
    ) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        let key: FxChainKey = (&target.context).into();
        self.with_project_mut(&guid, |p| {
            let chain = p.fx_chains.get_mut(&key).ok_or_else(not_found_fx)?;
            let i = find_fx_index(chain, &target.fx).ok_or_else(not_found_fx)?;
            chain[i].fx.enabled = enabled;
            Ok::<(), DawError>(())
        })?
    }

    fn set_offline(
        &self,
        project: ProjectContext,
        target: FxTarget,
        offline: bool,
    ) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        let key: FxChainKey = (&target.context).into();
        self.with_project_mut(&guid, |p| {
            let chain = p.fx_chains.get_mut(&key).ok_or_else(not_found_fx)?;
            let i = find_fx_index(chain, &target.fx).ok_or_else(not_found_fx)?;
            chain[i].fx.offline = offline;
            Ok::<(), DawError>(())
        })?
    }

    fn add(&self, project: ProjectContext, chain: FxChainContext, name: &str) -> Option<String> {
        let guid = resolve_project(self, &project)?;
        let key: FxChainKey = (&chain).into();
        // Try to load a real plugin first (CLAP today; VST3/LV2 land
        // when their backends ship). On failure or for synthetic names
        // fall through to the existing synthetic 8-param entry.
        let mut real_plugin = crate::plugin::load_plugin(name).ok().flatten();
        let real_param_count = real_plugin
            .as_mut()
            .map(|p| p.params().len() as u32)
            .unwrap_or(8);
        let real_descriptor = real_plugin.as_ref().map(|p| p.descriptor());
        let plugin_format = real_descriptor
            .as_ref()
            .map(|d| d.format)
            .unwrap_or(crate::plugin::PluginFormat::Synthetic);

        let new_guid = self.with_project_mut(&guid, |p| {
            let chain_vec = p.fx_chains.entry(key).or_default();
            let new_index = chain_vec.len() as u32;
            let new_guid = Uuid::new_v4().to_string();
            p.next_fx_counter += 1;

            let mut fx = Fx::new(new_guid.clone(), new_index, name.to_string());
            fx.plugin_name = real_descriptor
                .as_ref()
                .map(|d| d.name.clone())
                .unwrap_or_else(|| name.to_string());
            fx.plugin_type = match plugin_format {
                crate::plugin::PluginFormat::Clap => daw_proto::fx::FxType::Clap,
                crate::plugin::PluginFormat::Vst3 => daw_proto::fx::FxType::Vst3,
                _ => guess_fx_type(name),
            };
            fx.parameter_count = real_param_count;

            chain_vec.push(FxEntry {
                fx,
                state_chunk: String::new(),
                params: default_params(),
            });
            new_guid
        })
        .ok()?;
        // Stash the loaded plugin instance for the renderer to find.
        if let Some(plugin) = real_plugin {
            self.plugin_instances
                .lock()
                .expect("plugin_instances poisoned")
                .insert(new_guid.clone(), plugin);
        }
        Some(new_guid)
    }

    fn add_at(&self, project: ProjectContext, request: AddFxAtRequest) -> Option<String> {
        let guid = resolve_project(self, &project)?;
        let key: FxChainKey = (&request.context).into();
        self.with_project_mut(&guid, |p| {
            let chain_vec = p.fx_chains.entry(key).or_default();
            let pos = (request.index as usize).min(chain_vec.len());
            let new_guid = Uuid::new_v4().to_string();
            p.next_fx_counter += 1;

            let mut fx = Fx::new(new_guid.clone(), pos as u32, request.name.clone());
            fx.plugin_name = request.name.clone();
            fx.plugin_type = guess_fx_type(&request.name);
            fx.parameter_count = 8;

            chain_vec.insert(
                pos,
                FxEntry {
                    fx,
                    state_chunk: String::new(),
                    params: default_params(),
                },
            );
            // Renumber.
            for (i, e) in chain_vec.iter_mut().enumerate() {
                e.fx.index = i as u32;
            }
            Some(new_guid)
        })
        .ok()
        .flatten()
    }

    fn remove(&self, project: ProjectContext, target: FxTarget) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        let key: FxChainKey = (&target.context).into();
        let removed_guid = self.with_project_mut(&guid, |p| {
            let chain = p.fx_chains.get_mut(&key).ok_or_else(not_found_fx)?;
            let i = find_fx_index(chain, &target.fx).ok_or_else(not_found_fx)?;
            let removed = chain.remove(i);
            for (i, e) in chain.iter_mut().enumerate() {
                e.fx.index = i as u32;
            }
            Ok::<String, DawError>(removed.fx.guid)
        })??;
        // Drop any loaded plugin instance backing this FX entry.
        self.plugin_instances
            .lock()
            .expect("plugin_instances poisoned")
            .remove(&removed_guid);
        Ok(())
    }

    fn move_to(&self, project: ProjectContext, target: FxTarget, new_index: u32) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        let key: FxChainKey = (&target.context).into();
        self.with_project_mut(&guid, |p| {
            let chain = p.fx_chains.get_mut(&key).ok_or_else(not_found_fx)?;
            let i = find_fx_index(chain, &target.fx).ok_or_else(not_found_fx)?;
            let entry = chain.remove(i);
            let dest = (new_index as usize).min(chain.len());
            chain.insert(dest, entry);
            for (i, e) in chain.iter_mut().enumerate() {
                e.fx.index = i as u32;
            }
            Ok::<(), DawError>(())
        })?
    }

    fn parameters(&self, project: ProjectContext, target: FxTarget) -> Vec<FxParameter> {
        let Some(guid) = resolve_project(self, &project) else {
            return Vec::new();
        };
        let key: FxChainKey = (&target.context).into();
        self.with_project(&guid, |p| {
            let chain = match p.fx_chains.get(&key) {
                Some(c) => c,
                None => return Vec::new(),
            };
            let Some(i) = find_fx_index(chain, &target.fx) else {
                return Vec::new();
            };
            let entry = &chain[i];
            (0..entry.fx.parameter_count)
                .map(|pidx| FxParameter {
                    index: pidx,
                    name: param_name(pidx),
                    value: entry.params.get(&pidx).copied().unwrap_or(0.0),
                    formatted: format!("{:.2}", entry.params.get(&pidx).copied().unwrap_or(0.0)),
                    is_toggle: false,
                    step_count: None,
                    step_labels: Vec::new(),
                })
                .collect()
        })
        .unwrap_or_default()
    }

    fn parameter(
        &self,
        project: ProjectContext,
        target: FxTarget,
        index: u32,
    ) -> Option<FxParameter> {
        Effects::parameters(self, project, target)
            .into_iter()
            .find(|p| p.index == index)
    }

    fn set_parameter(
        &self,
        project: ProjectContext,
        request: SetParameterRequest,
    ) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        let key: FxChainKey = (&request.target.context).into();
        self.with_project_mut(&guid, |p| {
            let chain = p.fx_chains.get_mut(&key).ok_or_else(not_found_fx)?;
            let i = find_fx_index(chain, &request.target.fx).ok_or_else(not_found_fx)?;
            chain[i]
                .params
                .insert(request.index, request.value.clamp(0.0, 1.0));
            // Update last_touched.
            Ok::<(), DawError>(())
        })?;
        // Update last-touched FX.
        let (track_guid, is_input_fx) = match &request.target.context {
            FxChainContext::Track(g) => (g.clone(), false),
            FxChainContext::Input(g) => (g.clone(), true),
            FxChainContext::Monitoring => (String::new(), false),
        };
        // Look up the FX's chain index for the touched record.
        let fx_index = Effects::get(self, project, request.target.clone())
            .map(|fx| fx.index)
            .unwrap_or(0);
        let mut s = self
            .state
            .lock()
            .map_err(|_| DawError::operation_failed("standalone state poisoned"))?;
        s.last_touched_fx = Some(LastTouchedFx {
            track_guid,
            fx_index,
            param_index: request.index,
            is_input_fx,
        });
        let _ = guid;
        Ok(())
    }

    fn parameter_by_name(
        &self,
        project: ProjectContext,
        target: FxTarget,
        name: &str,
    ) -> Option<FxParameter> {
        Effects::parameters(self, project, target)
            .into_iter()
            .find(|p| p.name == name)
    }

    fn set_parameter_by_name(
        &self,
        project: ProjectContext,
        request: SetParameterByNameRequest,
    ) -> DawResult<()> {
        let pname = request.name.clone();
        let param =
            Effects::parameter_by_name(self, project.clone(), request.target.clone(), &pname)
                .ok_or_else(|| DawError::not_found("FxParameter", &pname))?;
        Effects::set_parameter(
            self,
            project,
            SetParameterRequest {
                target: request.target,
                index: param.index,
                value: request.value,
            },
        )
    }

    fn open_ui(&self, project: ProjectContext, target: FxTarget) -> DawResult<()> {
        set_window_open(self, project, target, true)
    }
    fn close_ui(&self, project: ProjectContext, target: FxTarget) -> DawResult<()> {
        set_window_open(self, project, target, false)
    }
    fn toggle_ui(&self, project: ProjectContext, target: FxTarget) -> DawResult<()> {
        let cur = Effects::get(self, project.clone(), target.clone())
            .map(|fx| fx.window_open)
            .unwrap_or(false);
        set_window_open(self, project, target, !cur)
    }

    fn state_chunk(&self, project: ProjectContext, target: FxTarget) -> Option<Vec<u8>> {
        let guid = resolve_project(self, &project)?;
        let key: FxChainKey = (&target.context).into();
        self.with_project(&guid, |p| {
            let chain = p.fx_chains.get(&key)?;
            let i = find_fx_index(chain, &target.fx)?;
            Some(chain[i].state_chunk.clone().into_bytes())
        })
        .ok()
        .flatten()
    }

    fn set_state_chunk(
        &self,
        project: ProjectContext,
        target: FxTarget,
        chunk: Vec<u8>,
    ) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        let key: FxChainKey = (&target.context).into();
        let text = String::from_utf8(chunk)
            .map_err(|_| DawError::operation_failed("non-UTF8 state chunk"))?;
        self.with_project_mut(&guid, |p| {
            let chain = p.fx_chains.get_mut(&key).ok_or_else(not_found_fx)?;
            let i = find_fx_index(chain, &target.fx).ok_or_else(not_found_fx)?;
            chain[i].state_chunk = text;
            Ok::<(), DawError>(())
        })?
    }

    fn state_chunk_encoded(&self, _project: ProjectContext, _target: FxTarget) -> Option<String> {
        // Base64 wrapping isn't pulled in as a dep yet. Round-trip via
        // the raw `state_chunk` accessor instead.
        todo!(
            "standalone: Effects::state_chunk_encoded — needs base64 dep; use state_chunk for now"
        )
    }

    fn set_state_chunk_encoded(
        &self,
        _project: ProjectContext,
        _target: FxTarget,
        _encoded: &str,
    ) -> DawResult<()> {
        todo!("standalone: Effects::set_state_chunk_encoded — needs base64 dep")
    }

    fn chain_state(&self, project: ProjectContext, chain: FxChainContext) -> Vec<FxStateChunk> {
        let Some(guid) = resolve_project(self, &project) else {
            return Vec::new();
        };
        let key: FxChainKey = (&chain).into();
        self.with_project(&guid, |p| {
            p.fx_chains
                .get(&key)
                .map(|c| {
                    c.iter()
                        .map(|e| FxStateChunk {
                            fx_guid: e.fx.guid.clone(),
                            fx_index: e.fx.index,
                            plugin_name: e.fx.plugin_name.clone(),
                            encoded_chunk: e.state_chunk.clone(),
                        })
                        .collect()
                })
                .unwrap_or_default()
        })
        .unwrap_or_default()
    }

    fn set_chain_state(
        &self,
        project: ProjectContext,
        chain: FxChainContext,
        chunks: Vec<FxStateChunk>,
    ) -> DawResult<()> {
        let guid = resolve_project(self, &project).ok_or_else(not_found_proj)?;
        let key: FxChainKey = (&chain).into();
        self.with_project_mut(&guid, |p| {
            let chain_vec = p.fx_chains.get_mut(&key).ok_or_else(not_found_fx)?;
            for chunk in chunks {
                if let Some(e) = chain_vec.iter_mut().find(|e| e.fx.guid == chunk.fx_guid) {
                    e.state_chunk = chunk.encoded_chunk;
                }
            }
            Ok::<(), DawError>(())
        })?
    }

    // ── Deferred — todo!() ────────────────────────────────────────────

    fn preset_index(&self, _project: ProjectContext, _target: FxTarget) -> Option<FxPresetIndex> {
        todo!("standalone: Effects::preset_index — preset enumeration needs real plugin metadata")
    }
    fn next_preset(&self, _project: ProjectContext, _target: FxTarget) -> DawResult<()> {
        todo!("standalone: Effects::next_preset")
    }
    fn prev_preset(&self, _project: ProjectContext, _target: FxTarget) -> DawResult<()> {
        todo!("standalone: Effects::prev_preset")
    }
    fn set_preset(
        &self,
        _project: ProjectContext,
        _target: FxTarget,
        _index: u32,
    ) -> DawResult<()> {
        todo!("standalone: Effects::set_preset")
    }
    fn named_config(
        &self,
        _project: ProjectContext,
        _target: FxTarget,
        _key: &str,
    ) -> Option<String> {
        todo!("standalone: Effects::named_config — needs plugin-specific key/value backing")
    }
    fn set_named_config(
        &self,
        _project: ProjectContext,
        _request: SetNamedConfigRequest,
    ) -> DawResult<()> {
        todo!("standalone: Effects::set_named_config")
    }
    fn latency(&self, _project: ProjectContext, _target: FxTarget) -> Option<FxLatency> {
        Some(FxLatency::default())
    }
    fn param_modulation(
        &self,
        _project: ProjectContext,
        _target: FxTarget,
        _param_index: u32,
    ) -> Option<FxParamModulation> {
        Some(FxParamModulation::default())
    }
    fn channel_config(
        &self,
        _project: ProjectContext,
        _target: FxTarget,
    ) -> Option<FxChannelConfig> {
        todo!("standalone: Effects::channel_config — pin routing not modeled yet")
    }
    fn set_channel_config(
        &self,
        _project: ProjectContext,
        _target: FxTarget,
        _config: FxChannelConfig,
    ) -> DawResult<()> {
        todo!("standalone: Effects::set_channel_config")
    }
    fn silence_output(
        &self,
        _project: ProjectContext,
        _target: FxTarget,
    ) -> DawResult<FxPinMappings> {
        todo!("standalone: Effects::silence_output")
    }
    fn restore_output(
        &self,
        _project: ProjectContext,
        _target: FxTarget,
        _saved: FxPinMappings,
    ) -> DawResult<()> {
        todo!("standalone: Effects::restore_output")
    }
    fn tree(&self, _project: ProjectContext, _chain: FxChainContext) -> FxTree {
        todo!("standalone: Effects::tree — FX containers not modeled yet")
    }
    fn create_container(
        &self,
        _project: ProjectContext,
        _request: CreateContainerRequest,
    ) -> Option<FxNodeId> {
        todo!("standalone: Effects::create_container")
    }
    fn move_to_container(
        &self,
        _project: ProjectContext,
        _request: MoveToContainerRequest,
    ) -> DawResult<()> {
        todo!("standalone: Effects::move_to_container")
    }
    fn move_from_container(
        &self,
        _project: ProjectContext,
        _request: MoveFromContainerRequest,
    ) -> DawResult<()> {
        todo!("standalone: Effects::move_from_container")
    }
    fn set_routing_mode(
        &self,
        _project: ProjectContext,
        _chain: FxChainContext,
        _node_id: FxNodeId,
        _mode: FxRoutingMode,
    ) -> DawResult<()> {
        todo!("standalone: Effects::set_routing_mode")
    }
    fn container_channel_config(
        &self,
        _project: ProjectContext,
        _chain: FxChainContext,
        _container_id: FxNodeId,
    ) -> Option<FxContainerChannelConfig> {
        todo!("standalone: Effects::container_channel_config")
    }
    fn set_container_channel_config(
        &self,
        _project: ProjectContext,
        _request: SetContainerChannelConfigRequest,
    ) -> DawResult<()> {
        todo!("standalone: Effects::set_container_channel_config")
    }
    fn enclose_in_container(
        &self,
        _project: ProjectContext,
        _request: EncloseInContainerRequest,
    ) -> Option<FxNodeId> {
        todo!("standalone: Effects::enclose_in_container")
    }
    fn explode_container(
        &self,
        _project: ProjectContext,
        _chain: FxChainContext,
        _container_id: FxNodeId,
    ) -> DawResult<()> {
        todo!("standalone: Effects::explode_container")
    }
    fn rename_container(
        &self,
        _project: ProjectContext,
        _chain: FxChainContext,
        _container_id: FxNodeId,
        _name: &str,
    ) -> DawResult<()> {
        todo!("standalone: Effects::rename_container")
    }
    fn chain_chunk_text(&self, _project: ProjectContext, _chain: FxChainContext) -> Option<String> {
        todo!("standalone: Effects::chain_chunk_text — raw RPP block parsing not implemented")
    }
    fn insert_chain_chunk(
        &self,
        _project: ProjectContext,
        _chain: FxChainContext,
        _chunk_text: &str,
    ) -> DawResult<()> {
        todo!("standalone: Effects::insert_chain_chunk")
    }
}

fn set_window_open(
    daw: &Standalone,
    project: ProjectContext,
    target: FxTarget,
    open: bool,
) -> DawResult<()> {
    let guid = resolve_project(daw, &project).ok_or_else(not_found_proj)?;
    let key: FxChainKey = (&target.context).into();
    daw.with_project_mut(&guid, |p| {
        let chain = p.fx_chains.get_mut(&key).ok_or_else(not_found_fx)?;
        let i = find_fx_index(chain, &target.fx).ok_or_else(not_found_fx)?;
        chain[i].fx.window_open = open;
        Ok::<(), DawError>(())
    })?
}

fn guess_fx_type(name: &str) -> FxType {
    let lower = name.to_lowercase();
    if lower.starts_with("vst3:") {
        FxType::Vst3
    } else if lower.starts_with("vst:") || lower.starts_with("vst2:") {
        FxType::Vst2
    } else if lower.starts_with("clap:") {
        FxType::Clap
    } else if lower.starts_with("au:") {
        FxType::Au
    } else if lower.starts_with("js:") {
        FxType::Js
    } else {
        FxType::Unknown
    }
}
