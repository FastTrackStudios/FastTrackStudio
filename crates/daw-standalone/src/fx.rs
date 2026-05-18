//! `impl Effects for Standalone` — every method panics with
//! `todo!("standalone: …")`. Standalone has no plugin host yet
//! (CLAP/VST3/AU/JS), so FX state/parameters/preset/UI/container
//! plumbing all need real backends behind them.

use daw_proto::fx::{
    AddFxAtRequest, CreateContainerRequest, EncloseInContainerRequest, Fx, FxChainContext,
    FxChannelConfig, FxContainerChannelConfig, FxLatency, FxNodeId, FxParamModulation, FxParameter,
    FxPinMappings, FxPresetIndex, FxRoutingMode, FxStateChunk, FxTarget, FxTree, InstalledFx,
    LastTouchedFx, MoveFromContainerRequest, MoveToContainerRequest,
    SetContainerChannelConfigRequest, SetNamedConfigRequest, SetParameterByNameRequest,
    SetParameterRequest,
};
use daw_proto::project::ProjectContext;
use daw_proto::{DawResult, fx::Effects};

use crate::sync::Standalone;

impl Effects for Standalone {
    fn list_installed(&self) -> Vec<InstalledFx> {
        todo!("standalone: Effects::list_installed — no plugin host yet")
    }
    fn last_touched(&self) -> Option<LastTouchedFx> {
        todo!("standalone: Effects::last_touched")
    }
    fn list(&self, _project: ProjectContext, _chain: FxChainContext) -> Vec<Fx> {
        todo!("standalone: Effects::list")
    }
    fn get(&self, _project: ProjectContext, _target: FxTarget) -> Option<Fx> {
        todo!("standalone: Effects::get")
    }
    fn count(&self, _project: ProjectContext, _chain: FxChainContext) -> u32 {
        todo!("standalone: Effects::count")
    }
    fn set_enabled(
        &self,
        _project: ProjectContext,
        _target: FxTarget,
        _enabled: bool,
    ) -> DawResult<()> {
        todo!("standalone: Effects::set_enabled")
    }
    fn set_offline(
        &self,
        _project: ProjectContext,
        _target: FxTarget,
        _offline: bool,
    ) -> DawResult<()> {
        todo!("standalone: Effects::set_offline")
    }
    fn add(&self, _project: ProjectContext, _chain: FxChainContext, _name: &str) -> Option<String> {
        todo!("standalone: Effects::add")
    }
    fn add_at(&self, _project: ProjectContext, _request: AddFxAtRequest) -> Option<String> {
        todo!("standalone: Effects::add_at")
    }
    fn remove(&self, _project: ProjectContext, _target: FxTarget) -> DawResult<()> {
        todo!("standalone: Effects::remove")
    }
    fn move_to(
        &self,
        _project: ProjectContext,
        _target: FxTarget,
        _new_index: u32,
    ) -> DawResult<()> {
        todo!("standalone: Effects::move_to")
    }
    fn parameters(&self, _project: ProjectContext, _target: FxTarget) -> Vec<FxParameter> {
        todo!("standalone: Effects::parameters")
    }
    fn parameter(
        &self,
        _project: ProjectContext,
        _target: FxTarget,
        _index: u32,
    ) -> Option<FxParameter> {
        todo!("standalone: Effects::parameter")
    }
    fn set_parameter(
        &self,
        _project: ProjectContext,
        _request: SetParameterRequest,
    ) -> DawResult<()> {
        todo!("standalone: Effects::set_parameter")
    }
    fn parameter_by_name(
        &self,
        _project: ProjectContext,
        _target: FxTarget,
        _name: &str,
    ) -> Option<FxParameter> {
        todo!("standalone: Effects::parameter_by_name")
    }
    fn set_parameter_by_name(
        &self,
        _project: ProjectContext,
        _request: SetParameterByNameRequest,
    ) -> DawResult<()> {
        todo!("standalone: Effects::set_parameter_by_name")
    }
    fn preset_index(&self, _project: ProjectContext, _target: FxTarget) -> Option<FxPresetIndex> {
        todo!("standalone: Effects::preset_index")
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
    fn open_ui(&self, _project: ProjectContext, _target: FxTarget) -> DawResult<()> {
        todo!("standalone: Effects::open_ui")
    }
    fn close_ui(&self, _project: ProjectContext, _target: FxTarget) -> DawResult<()> {
        todo!("standalone: Effects::close_ui")
    }
    fn toggle_ui(&self, _project: ProjectContext, _target: FxTarget) -> DawResult<()> {
        todo!("standalone: Effects::toggle_ui")
    }
    fn named_config(
        &self,
        _project: ProjectContext,
        _target: FxTarget,
        _key: &str,
    ) -> Option<String> {
        todo!("standalone: Effects::named_config")
    }
    fn set_named_config(
        &self,
        _project: ProjectContext,
        _request: SetNamedConfigRequest,
    ) -> DawResult<()> {
        todo!("standalone: Effects::set_named_config")
    }
    fn latency(&self, _project: ProjectContext, _target: FxTarget) -> Option<FxLatency> {
        todo!("standalone: Effects::latency")
    }
    fn param_modulation(
        &self,
        _project: ProjectContext,
        _target: FxTarget,
        _param_index: u32,
    ) -> Option<FxParamModulation> {
        todo!("standalone: Effects::param_modulation")
    }
    fn channel_config(
        &self,
        _project: ProjectContext,
        _target: FxTarget,
    ) -> Option<FxChannelConfig> {
        todo!("standalone: Effects::channel_config")
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
    fn state_chunk(&self, _project: ProjectContext, _target: FxTarget) -> Option<Vec<u8>> {
        todo!("standalone: Effects::state_chunk")
    }
    fn set_state_chunk(
        &self,
        _project: ProjectContext,
        _target: FxTarget,
        _chunk: Vec<u8>,
    ) -> DawResult<()> {
        todo!("standalone: Effects::set_state_chunk")
    }
    fn state_chunk_encoded(&self, _project: ProjectContext, _target: FxTarget) -> Option<String> {
        todo!("standalone: Effects::state_chunk_encoded")
    }
    fn set_state_chunk_encoded(
        &self,
        _project: ProjectContext,
        _target: FxTarget,
        _encoded: &str,
    ) -> DawResult<()> {
        todo!("standalone: Effects::set_state_chunk_encoded")
    }
    fn chain_state(&self, _project: ProjectContext, _chain: FxChainContext) -> Vec<FxStateChunk> {
        todo!("standalone: Effects::chain_state")
    }
    fn set_chain_state(
        &self,
        _project: ProjectContext,
        _chain: FxChainContext,
        _chunks: Vec<FxStateChunk>,
    ) -> DawResult<()> {
        todo!("standalone: Effects::set_chain_state")
    }
    fn tree(&self, _project: ProjectContext, _chain: FxChainContext) -> FxTree {
        todo!("standalone: Effects::tree")
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
        todo!("standalone: Effects::chain_chunk_text")
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
