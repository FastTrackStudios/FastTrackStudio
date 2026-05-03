//! MCP server for agent control of a running DAW session.

use std::path::PathBuf;

use rmcp::{
    Json, ServerHandler, ServiceExt,
    handler::server::{router::tool::ToolRouter, wrapper::Parameters},
    model::{ServerCapabilities, ServerInfo},
    tool, tool_handler, tool_router,
};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

#[derive(Debug, Clone)]
pub struct DawMcpServer {
    socket: Option<PathBuf>,
    tool_router: ToolRouter<Self>,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct TrackParams {
    /// Track name or zero-based index.
    pub track: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct TrackSetParams {
    /// Track name or zero-based index.
    pub track: String,
    /// Field to set: muted, soloed, armed, selected, volume, pan, name, color, folder_depth, num_channels, visible_in_tcp, visible_in_mixer, parent_send.
    pub field: String,
    /// New value for the selected field.
    pub value: Value,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct TrackMoveParams {
    /// Track name or zero-based index.
    pub track: String,
    /// New zero-based track index.
    pub index: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct TrackExtStateParams {
    pub track: String,
    pub section: String,
    pub key: String,
    pub value: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct FxParams {
    /// Track name or zero-based index.
    pub track: String,
    /// FX name or zero-based index.
    pub fx: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct FxAddParams {
    pub track: String,
    pub name: String,
    pub at: Option<u32>,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct FxSetEnabledParams {
    pub track: String,
    pub fx: String,
    pub enabled: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct FxMoveParams {
    pub track: String,
    pub fx: String,
    pub index: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct FxSetParamParams {
    pub track: String,
    pub fx: String,
    pub param: u32,
    pub value: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct FxSetParamByNameParams {
    pub track: String,
    pub fx: String,
    pub param: String,
    pub value: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct FxActionParams {
    pub track: String,
    pub fx: String,
    /// Action: open, close, toggle.
    pub action: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct FxPresetParams {
    pub track: String,
    pub fx: String,
    /// Action: get, next, previous, set.
    pub action: String,
    pub index: Option<u32>,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct TransportActionParams {
    /// Action: play, pause, stop, play_pause, play_stop, record, stop_recording, toggle_recording, goto_start, goto_end, toggle_loop.
    pub action: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct SecondsParams {
    pub seconds: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct BpmParams {
    pub bpm: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct BoolParams {
    pub enabled: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct RateParams {
    pub rate: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct MeasureParams {
    pub measure: i32,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct OpenProjectParams {
    /// Path to the .rpp project file.
    pub path: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct ProjectGuidParams {
    pub guid: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct ProjectCommandParams {
    pub command: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct ProjectInfoStringParams {
    pub key: String,
    pub value: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct ProjectInfoNumberParams {
    pub key: String,
    pub value: Option<f64>,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct AddTrackParams {
    /// Track name. Defaults to "New Track".
    pub name: Option<String>,
    /// Zero-based insert index. Defaults to append.
    pub at: Option<u32>,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct FilePathParams {
    /// Path to a DAW project or setlist file.
    pub path: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct MarkerAddParams {
    pub position: f64,
    pub name: String,
    pub lane: Option<u32>,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct MarkerIdParams {
    pub id: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct MarkerMoveParams {
    pub id: u32,
    pub position: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct NamedIdParams {
    pub id: u32,
    pub name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct RegionAddParams {
    pub start: f64,
    pub end: f64,
    pub name: String,
    pub lane: Option<u32>,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct RegionBoundsParams {
    pub id: u32,
    pub start: f64,
    pub end: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct ExtStateParams {
    pub section: String,
    pub key: String,
    pub value: Option<String>,
    pub persist: Option<bool>,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct PathParam {
    pub path: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct ActionIdParams {
    pub action_id: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct CommandNameParams {
    pub command_name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct ActionToggleParams {
    pub command_name: String,
    pub is_on: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct VoxBatchParams {
    /// Facet JSON representation of daw::service::BatchRequest.
    pub request: Value,
}

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct CombineParams {
    /// Path to a .RPL setlist or .RPP input.
    pub input: String,
    /// Output .RPP path. Defaults to the input stem with .RPP.
    pub output: Option<String>,
    /// Gap between songs in measures.
    pub gap: Option<u32>,
}

impl DawMcpServer {
    pub fn new(socket: Option<PathBuf>) -> Self {
        Self {
            socket,
            tool_router: Self::tool_router(),
        }
    }

    async fn connect(&self) -> Result<crate::DawConnection, String> {
        crate::connect(self.socket.clone())
            .await
            .map_err(|err| err.to_string())
    }
}

#[tool_handler(router = self.tool_router)]
impl ServerHandler for DawMcpServer {
    fn get_info(&self) -> ServerInfo {
        ServerInfo::new(ServerCapabilities::builder().enable_tools().build())
    }
}

#[tool_router(router = tool_router)]
impl DawMcpServer {
    #[tool(
        name = "daw_ping",
        description = "Check whether the DAW session is reachable."
    )]
    pub async fn daw_ping(&self) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        Ok(Json(json!({ "ok": conn.daw.healthcheck().await })))
    }

    #[tool(
        name = "daw_vox_service_catalog",
        description = "Return the generated vox DAW service and method catalog."
    )]
    pub async fn daw_vox_service_catalog(&self) -> Result<Json<Value>, String> {
        Ok(Json(crate::ops::vox_service_catalog()))
    }

    #[tool(
        name = "daw_vox_execute_batch",
        description = "Execute a vox-native DAW BatchRequest encoded as Facet JSON."
    )]
    pub async fn daw_vox_execute_batch(
        &self,
        Parameters(params): Parameters<VoxBatchParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::vox_execute_batch(&conn.daw, params.request)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_project_info",
        description = "Return information about the current project."
    )]
    pub async fn daw_project_info(&self) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::project_info(&conn.daw)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_list_tracks",
        description = "List tracks in the current project."
    )]
    pub async fn daw_list_tracks(&self) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::tracks(&conn.daw)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_get_track",
        description = "Return detailed information for one track."
    )]
    pub async fn daw_get_track(
        &self,
        Parameters(params): Parameters<TrackParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::track(&conn.daw, &params.track)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(name = "daw_list_fx", description = "List FX on a track.")]
    pub async fn daw_list_fx(
        &self,
        Parameters(params): Parameters<TrackParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::fx(&conn.daw, &params.track)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(name = "daw_list_plugins", description = "List installed FX plugins.")]
    pub async fn daw_list_plugins(&self) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::plugins(&conn.daw)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_last_touched_fx",
        description = "Return the last touched FX parameter."
    )]
    pub async fn daw_last_touched_fx(&self) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::last_touched_fx(&conn.daw)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_list_fx_params",
        description = "List parameters for an FX."
    )]
    pub async fn daw_list_fx_params(
        &self,
        Parameters(params): Parameters<FxParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::fx_params(&conn.daw, &params.track, &params.fx)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(name = "daw_add_fx", description = "Add an FX to a track chain.")]
    pub async fn daw_add_fx(
        &self,
        Parameters(params): Parameters<FxAddParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::fx_add(&conn.daw, &params.track, &params.name, params.at)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_remove_fx",
        description = "Remove an FX from a track chain."
    )]
    pub async fn daw_remove_fx(
        &self,
        Parameters(params): Parameters<FxParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::fx_remove(&conn.daw, &params.track, &params.fx)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(name = "daw_set_fx_enabled", description = "Enable or bypass an FX.")]
    pub async fn daw_set_fx_enabled(
        &self,
        Parameters(params): Parameters<FxSetEnabledParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::fx_set_enabled(&conn.daw, &params.track, &params.fx, params.enabled)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(name = "daw_move_fx", description = "Move an FX to a new chain index.")]
    pub async fn daw_move_fx(
        &self,
        Parameters(params): Parameters<FxMoveParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::fx_move(&conn.daw, &params.track, &params.fx, params.index)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_set_fx_param",
        description = "Set an FX parameter by index."
    )]
    pub async fn daw_set_fx_param(
        &self,
        Parameters(params): Parameters<FxSetParamParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::fx_set_param(
            &conn.daw,
            &params.track,
            &params.fx,
            params.param,
            params.value,
        )
        .await
        .map(Json)
        .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_set_fx_param_by_name",
        description = "Set an FX parameter by name."
    )]
    pub async fn daw_set_fx_param_by_name(
        &self,
        Parameters(params): Parameters<FxSetParamByNameParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::fx_set_param_by_name(
            &conn.daw,
            &params.track,
            &params.fx,
            &params.param,
            params.value,
        )
        .await
        .map(Json)
        .map_err(|err| err.to_string())
    }

    #[tool(name = "daw_fx_ui", description = "Open, close, or toggle an FX UI.")]
    pub async fn daw_fx_ui(
        &self,
        Parameters(params): Parameters<FxActionParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::fx_ui(&conn.daw, &params.track, &params.fx, &params.action)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(name = "daw_fx_preset", description = "Get or change an FX preset.")]
    pub async fn daw_fx_preset(
        &self,
        Parameters(params): Parameters<FxPresetParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::fx_preset(
            &conn.daw,
            &params.track,
            &params.fx,
            &params.action,
            params.index,
        )
        .await
        .map(Json)
        .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_transport_state",
        description = "Return the current transport state."
    )]
    pub async fn daw_transport_state(&self) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::transport(&conn.daw)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(name = "daw_transport", description = "Run a transport action.")]
    pub async fn daw_transport(
        &self,
        Parameters(params): Parameters<TransportActionParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::transport_control(&conn.daw, &params.action)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_set_position",
        description = "Set playhead position in seconds."
    )]
    pub async fn daw_set_position(
        &self,
        Parameters(params): Parameters<SecondsParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::transport_set_position(&conn.daw, params.seconds)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_set_tempo",
        description = "Set current project tempo in BPM."
    )]
    pub async fn daw_set_tempo(
        &self,
        Parameters(params): Parameters<BpmParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::transport_set_tempo(&conn.daw, params.bpm)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(name = "daw_set_loop", description = "Set transport loop state.")]
    pub async fn daw_set_loop(
        &self,
        Parameters(params): Parameters<BoolParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::transport_set_loop(&conn.daw, params.enabled)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(name = "daw_set_playrate", description = "Set transport playrate.")]
    pub async fn daw_set_playrate(
        &self,
        Parameters(params): Parameters<RateParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::transport_set_playrate(&conn.daw, params.rate)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_goto_measure",
        description = "Seek to a zero-based measure."
    )]
    pub async fn daw_goto_measure(
        &self,
        Parameters(params): Parameters<MeasureParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::transport_goto_measure(&conn.daw, params.measure)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_list_markers",
        description = "List markers in the current project."
    )]
    pub async fn daw_list_markers(&self) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::markers(&conn.daw)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_list_regions",
        description = "List regions in the current project."
    )]
    pub async fn daw_list_regions(&self) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::regions(&conn.daw)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(name = "daw_add_marker", description = "Add a project marker.")]
    pub async fn daw_add_marker(
        &self,
        Parameters(params): Parameters<MarkerAddParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::marker_add(&conn.daw, params.position, &params.name, params.lane)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(name = "daw_remove_marker", description = "Remove a project marker.")]
    pub async fn daw_remove_marker(
        &self,
        Parameters(params): Parameters<MarkerIdParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::marker_remove(&conn.daw, params.id)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(name = "daw_move_marker", description = "Move a marker to a new time.")]
    pub async fn daw_move_marker(
        &self,
        Parameters(params): Parameters<MarkerMoveParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::marker_move(&conn.daw, params.id, params.position)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(name = "daw_rename_marker", description = "Rename a project marker.")]
    pub async fn daw_rename_marker(
        &self,
        Parameters(params): Parameters<NamedIdParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::marker_rename(&conn.daw, params.id, &params.name)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(name = "daw_add_region", description = "Add a project region.")]
    pub async fn daw_add_region(
        &self,
        Parameters(params): Parameters<RegionAddParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::region_add(
            &conn.daw,
            params.start,
            params.end,
            &params.name,
            params.lane,
        )
        .await
        .map(Json)
        .map_err(|err| err.to_string())
    }

    #[tool(name = "daw_remove_region", description = "Remove a project region.")]
    pub async fn daw_remove_region(
        &self,
        Parameters(params): Parameters<MarkerIdParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::region_remove(&conn.daw, params.id)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_set_region_bounds",
        description = "Set project region bounds."
    )]
    pub async fn daw_set_region_bounds(
        &self,
        Parameters(params): Parameters<RegionBoundsParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::region_set_bounds(&conn.daw, params.id, params.start, params.end)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(name = "daw_rename_region", description = "Rename a project region.")]
    pub async fn daw_rename_region(
        &self,
        Parameters(params): Parameters<NamedIdParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::region_rename(&conn.daw, params.id, &params.name)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(name = "daw_list_projects", description = "List open project tabs.")]
    pub async fn daw_list_projects(&self) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::projects(&conn.daw)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(name = "daw_create_project", description = "Create a new project tab.")]
    pub async fn daw_create_project(&self) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::create_project(&conn.daw)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_select_project",
        description = "Select an open project by GUID."
    )]
    pub async fn daw_select_project(
        &self,
        Parameters(params): Parameters<ProjectGuidParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        let guid = params.guid.ok_or_else(|| "guid is required".to_string())?;
        crate::ops::select_project(&conn.daw, &guid)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_open_project",
        description = "Open a REAPER .rpp project file."
    )]
    pub async fn daw_open_project(
        &self,
        Parameters(params): Parameters<OpenProjectParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::open_project(&conn.daw, &params.path)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_close_project",
        description = "Close a project tab by GUID or current project if omitted."
    )]
    pub async fn daw_close_project(
        &self,
        Parameters(params): Parameters<ProjectGuidParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::close_project(&conn.daw, params.guid.as_deref())
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(name = "daw_save_project", description = "Save the current project.")]
    pub async fn daw_save_project(&self) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::save_project(&conn.daw)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_save_all_projects",
        description = "Save all open projects."
    )]
    pub async fn daw_save_all_projects(&self) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::save_all_projects(&conn.daw)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(name = "daw_undo", description = "Undo in the current project.")]
    pub async fn daw_undo(&self) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::project_undo(&conn.daw)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(name = "daw_redo", description = "Redo in the current project.")]
    pub async fn daw_redo(&self) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::project_redo(&conn.daw)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_run_command",
        description = "Run a REAPER command/action in the current project."
    )]
    pub async fn daw_run_command(
        &self,
        Parameters(params): Parameters<ProjectCommandParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::project_run_command(&conn.daw, &params.command)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_project_info_string",
        description = "Get or set a project info string key."
    )]
    pub async fn daw_project_info_string(
        &self,
        Parameters(params): Parameters<ProjectInfoStringParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::project_info_string(&conn.daw, &params.key, params.value.as_deref())
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_project_info_number",
        description = "Get or set a project info numeric key."
    )]
    pub async fn daw_project_info_number(
        &self,
        Parameters(params): Parameters<ProjectInfoNumberParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::project_info_number(&conn.daw, &params.key, params.value)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_add_track",
        description = "Add a track to the current project."
    )]
    pub async fn daw_add_track(
        &self,
        Parameters(params): Parameters<AddTrackParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::add_track(&conn.daw, params.name.as_deref(), params.at)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(name = "daw_set_track", description = "Set a track field.")]
    pub async fn daw_set_track(
        &self,
        Parameters(params): Parameters<TrackSetParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::track_set(&conn.daw, &params.track, &params.field, params.value)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_move_track",
        description = "Move a track to a new zero-based index."
    )]
    pub async fn daw_move_track(
        &self,
        Parameters(params): Parameters<TrackMoveParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::track_move(&conn.daw, &params.track, params.index)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_remove_track",
        description = "Remove a track from the current project."
    )]
    pub async fn daw_remove_track(
        &self,
        Parameters(params): Parameters<TrackParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::remove_track(&conn.daw, &params.track)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_track_ext_state",
        description = "Get or set track-scoped P_EXT state."
    )]
    pub async fn daw_track_ext_state(
        &self,
        Parameters(params): Parameters<TrackExtStateParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::track_ext_state(
            &conn.daw,
            &params.track,
            &params.section,
            &params.key,
            params.value.as_deref(),
        )
        .await
        .map(Json)
        .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_delete_track_ext_state",
        description = "Delete track-scoped P_EXT state."
    )]
    pub async fn daw_delete_track_ext_state(
        &self,
        Parameters(params): Parameters<TrackExtStateParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::track_delete_ext_state(&conn.daw, &params.track, &params.section, &params.key)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_ext_state",
        description = "Get or set global REAPER ExtState."
    )]
    pub async fn daw_ext_state(
        &self,
        Parameters(params): Parameters<ExtStateParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        match params.value {
            Some(value) => {
                crate::ops::ext_state_set(
                    &conn.daw,
                    &params.section,
                    &params.key,
                    &value,
                    params.persist.unwrap_or(false),
                )
                .await
            }
            None => crate::ops::ext_state_get(&conn.daw, &params.section, &params.key).await,
        }
        .map(Json)
        .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_delete_ext_state",
        description = "Delete global REAPER ExtState."
    )]
    pub async fn daw_delete_ext_state(
        &self,
        Parameters(params): Parameters<ExtStateParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::ext_state_delete(
            &conn.daw,
            &params.section,
            &params.key,
            params.persist.unwrap_or(false),
        )
        .await
        .map(Json)
        .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_audio_engine",
        description = "Return audio engine state and latency."
    )]
    pub async fn daw_audio_engine(&self) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::audio_engine(&conn.daw)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_audio_engine_control",
        description = "Run an audio engine action: init or quit."
    )]
    pub async fn daw_audio_engine_control(
        &self,
        Parameters(params): Parameters<TransportActionParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::audio_engine_control(&conn.daw, &params.action)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_load_plugin",
        description = "Load a plugin binary into REAPER."
    )]
    pub async fn daw_load_plugin(
        &self,
        Parameters(params): Parameters<PathParam>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::plugin_loader_load(&conn.daw, &params.path)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_loaded_plugins",
        description = "List loaded plugin binaries."
    )]
    pub async fn daw_loaded_plugins(&self) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::plugin_loader_list(&conn.daw)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_execute_action",
        description = "Execute a REAPER action by numeric ID or command name."
    )]
    pub async fn daw_execute_action(
        &self,
        Parameters(params): Parameters<ActionIdParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::action_execute(&conn.daw, &params.action_id)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_action_lookup",
        description = "Look up an action registration and command ID."
    )]
    pub async fn daw_action_lookup(
        &self,
        Parameters(params): Parameters<CommandNameParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::action_lookup(&conn.daw, &params.command_name)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_set_action_toggle",
        description = "Set a registered action toggle state."
    )]
    pub async fn daw_set_action_toggle(
        &self,
        Parameters(params): Parameters<ActionToggleParams>,
    ) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::action_set_toggle(&conn.daw, &params.command_name, params.is_on)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "daw_toolbar_status",
        description = "Return dynamic toolbar availability and tracked buttons."
    )]
    pub async fn daw_toolbar_status(&self) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::toolbar_status(&conn.daw)
            .await
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "rpp_summary",
        description = "Parse a .rpp file and return a project summary."
    )]
    pub async fn rpp_summary(
        &self,
        Parameters(params): Parameters<FilePathParams>,
    ) -> Result<Json<Value>, String> {
        crate::ops::rpp_summary(&params.path)
            .map(Json)
            .map_err(|err| err.to_string())
    }

    #[tool(
        name = "rpp_combine",
        description = "Combine a .RPL setlist into one .RPP project."
    )]
    pub async fn rpp_combine(
        &self,
        Parameters(params): Parameters<CombineParams>,
    ) -> Result<Json<Value>, String> {
        crate::ops::combine_rpl(
            &params.input,
            params.output.as_deref(),
            params.gap.unwrap_or(0),
        )
        .map(Json)
        .map_err(|err| err.to_string())
    }
}

pub async fn serve_stdio(socket: Option<PathBuf>) -> eyre::Result<()> {
    let server = DawMcpServer::new(socket);
    server
        .serve((tokio::io::stdin(), tokio::io::stdout()))
        .await?
        .waiting()
        .await?;
    Ok(())
}
