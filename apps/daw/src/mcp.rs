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
pub struct OpenProjectParams {
    /// Path to the .rpp project file.
    pub path: String,
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

    #[tool(name = "daw_list_projects", description = "List open project tabs.")]
    pub async fn daw_list_projects(&self) -> Result<Json<Value>, String> {
        let conn = self.connect().await?;
        crate::ops::projects(&conn.daw)
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
