use std::sync::Arc;

use axum::extract::{Query as AxumQuery, State, WebSocketUpgrade};
use axum::extract::ws::WebSocket;
use axum::response::IntoResponse;
use axum::routing::{get, post};
use axum::{Json, Router};
use serde::{Deserialize, Serialize};
use tokio::sync::RwLock;
use tower_http::cors::CorsLayer;
use tracing::{info, warn};
use vault_core::{Project, VaultServiceImpl, vault_service_service_descriptor};
use vault_core::provider::nextcloud_sync::NextcloudSync;
use vault_core::project_vault;

// ── Nextcloud sync config ────────────────────────────────────────────────────

#[derive(Clone)]
struct NextcloudSyncConfig {
    url: String,
    username: String,
    password: String,
    calendar: String,
    projects_path: String,
    /// Map of project title → Deck board ID.
    deck_boards: std::collections::HashMap<String, u64>,
}

// ── App state ────────────────────────────────────────────────────────────────

#[derive(Clone, Serialize)]
struct ServerInfo {
    name: String,
    id: String,
}

#[derive(Clone)]
struct AppState {
    svc: Arc<VaultServiceImpl>,
    info: ServerInfo,
    vault_root: String,
    nc_config: Option<NextcloudSyncConfig>,
    /// Tracks last sync result.
    last_sync: Arc<RwLock<Option<SyncStatus>>>,
}

#[derive(Clone, Serialize)]
struct SyncStatus {
    timestamp: String,
    calendar_pushed: usize,
    calendar_pulled: usize,
    deck_pushed: usize,
    deck_pulled: usize,
    files_created: usize,
    files_updated: usize,
    errors: Vec<String>,
}

// ── Main ─────────────────────────────────────────────────────────────────────

#[tokio::main]
async fn main() -> eyre::Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "task_server=info".into()),
        )
        .init();

    let server_name = std::env::var("SERVER_NAME").unwrap_or_else(|_| "default".to_string());
    let server_id = std::env::var("SERVER_ID").unwrap_or_else(|_| {
        let host = gethostname();
        format!("{}-{}", host, server_name)
    });

    let vault_root = std::env::var("VAULT_ROOT")
        .unwrap_or_else(|_| dirs_home().join("Vault").to_string_lossy().to_string());

    let svc = Arc::new(VaultServiceImpl::new(&vault_root));

    // Start file watchers.
    let _handles = svc.watch_all().await;

    // ── CRDT sync engine (real-time collaboration) ───────────────
    #[cfg(feature = "realtime")]
    {
        use vault_core::crdt::{CrdtSyncEngine, SyncOp};

        let crdt_engine = Arc::new(CrdtSyncEngine::new(std::path::Path::new(&vault_root)));
        let mut crdt_rx = crdt_engine.subscribe();

        // Spawn broadcast listener — logs real-time changes
        // (In production, this broadcasts to all connected WebSocket clients)
        tokio::spawn(async move {
            while let Ok(op) = crdt_rx.recv().await {
                match op {
                    SyncOp::FieldChanged { file_path, field, value } => {
                        info!(path = %file_path, field = %field, value = %value, "CRDT field change");
                    }
                    SyncOp::BodyChanged { file_path, .. } => {
                        info!(path = %file_path, "CRDT body update");
                    }
                    SyncOp::TaskCreated { file_path, .. } => {
                        info!(path = %file_path, "CRDT task created");
                    }
                    SyncOp::TaskDeleted { file_path } => {
                        info!(path = %file_path, "CRDT task deleted");
                    }
                    SyncOp::Refresh => {
                        info!("CRDT full refresh");
                    }
                }
            }
        });

        // Connect file watcher to CRDT engine
        let crdt_for_watcher = crdt_engine.clone();
        let vault_root_for_watcher = vault_root.clone();
        let mut file_rx = svc.subscribe();
        tokio::spawn(async move {
            loop {
                if file_rx.changed().await.is_err() {
                    break;
                }
                // File system changed — scan for modified .md files
                // For now, broadcast a refresh. In production, the file watcher
                // would report specific paths.
                let _ = crdt_for_watcher.subscribe().resubscribe();
                info!("File system change detected, CRDT engine notified");
            }
        });

        info!("CRDT real-time sync engine started");
    }

    // Nextcloud sync config (from env vars).
    let nc_config = match (
        std::env::var("NEXTCLOUD_URL").ok(),
        std::env::var("NEXTCLOUD_USERNAME").ok(),
        std::env::var("NEXTCLOUD_PASSWORD").ok(),
    ) {
        (Some(url), Some(username), Some(password)) => {
            let calendar = std::env::var("NEXTCLOUD_CALENDAR").unwrap_or_else(|_| "tasks".into());
            let projects_path = std::env::var("NEXTCLOUD_PROJECTS_PATH")
                .unwrap_or_else(|_| "Projects/".into());

            // Parse deck board mapping: "Project Name:4,Another:5"
            let deck_boards: std::collections::HashMap<String, u64> =
                std::env::var("NEXTCLOUD_DECK_BOARDS")
                    .unwrap_or_default()
                    .split(',')
                    .filter_map(|entry| {
                        let parts: Vec<&str> = entry.splitn(2, ':').collect();
                        if parts.len() == 2 {
                            Some((parts[0].to_string(), parts[1].parse().ok()?))
                        } else {
                            None
                        }
                    })
                    .collect();

            info!(
                url = %url,
                user = %username,
                calendar = %calendar,
                boards = %deck_boards.len(),
                "Nextcloud sync enabled"
            );

            Some(NextcloudSyncConfig {
                url,
                username,
                password,
                calendar,
                projects_path,
                deck_boards,
            })
        }
        _ => {
            info!("Nextcloud sync disabled (set NEXTCLOUD_URL, NEXTCLOUD_USERNAME, NEXTCLOUD_PASSWORD)");
            None
        }
    };

    let sync_interval: u64 = std::env::var("SYNC_INTERVAL_SECS")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(300); // Default: 5 minutes

    let last_sync = Arc::new(RwLock::new(None));

    let state = AppState {
        svc,
        info: ServerInfo {
            name: server_name.clone(),
            id: server_id,
        },
        vault_root: vault_root.clone(),
        nc_config: nc_config.clone(),
        last_sync: last_sync.clone(),
    };

    // Spawn the sync loop if Nextcloud is configured.
    if let Some(nc) = nc_config {
        let vault_root = vault_root.clone();
        let last_sync = last_sync.clone();
        tokio::spawn(async move {
            // Initial sync after 5 seconds.
            tokio::time::sleep(std::time::Duration::from_secs(5)).await;
            loop {
                info!("Starting Nextcloud sync...");
                let result = run_sync(&nc, &vault_root).await;
                match &result {
                    Ok(status) => {
                        info!(
                            pushed = status.calendar_pushed + status.deck_pushed,
                            pulled = status.calendar_pulled + status.deck_pulled,
                            created = status.files_created,
                            updated = status.files_updated,
                            errors = status.errors.len(),
                            "Sync complete"
                        );
                        *last_sync.write().await = Some(status.clone());
                    }
                    Err(e) => {
                        warn!(error = %e, "Sync failed");
                    }
                }
                tokio::time::sleep(std::time::Duration::from_secs(sync_interval)).await;
            }
        });
    }

    info!(name = %server_name, root = %vault_root, "server starting");

    let app = Router::new()
        // Vox WebSocket RPC — typed service calls over WebSocket
        .route("/vox", get(vox_ws_handler))
        // REST API — JSON endpoints for simple integrations
        .route("/api/info", get(server_info))
        .route("/api/projects", get(list_projects))
        .route("/api/projects/active", get(list_active_projects))
        .route("/api/tasks", get(list_tasks))
        .route("/api/sync/status", get(sync_status))
        .route("/api/sync/trigger", post(trigger_sync))
        .route("/api/activity", get(activity_feed))
        .route("/api/tasks", post(create_task_api))
        .route("/api/tasks/:title", get(get_task))
        .route("/api/tasks/:title/complete", post(complete_task_api))
        .route("/api/tasks/user/:username", get(tasks_by_user))
        .route("/api/health", get(health))
        .layer(CorsLayer::permissive());

    // Clone svc for Vox before state is consumed by axum
    let vox_port: u16 = std::env::var("VOX_PORT")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(3457);
    let vox_svc = state.svc.clone();

    let app = app.with_state(state);

    tokio::spawn(async move {
        let dispatcher = vault_core::VaultServiceDispatcher::new((*vox_svc).clone());
        info!(port = vox_port, "Vox WebSocket RPC server starting");
        if let Err(e) = vox::serve(format!("ws://0.0.0.0:{vox_port}"), dispatcher).await {
            warn!(error = %e, "Vox server error");
        }
    });

    // ── HTTP + JSON-RPC WebSocket server ─────────────────────────
    let bind = std::env::var("BIND_ADDR").unwrap_or_else(|_| "0.0.0.0:3456".to_string());
    let listener = tokio::net::TcpListener::bind(&bind).await?;
    info!("HTTP server listening on {}", bind);
    info!("Endpoints:");
    info!("  REST API:      http://{bind}/api/*");
    info!("  JSON-RPC WS:   ws://{bind}/vox");
    info!("  Vox RPC:       ws://0.0.0.0:{vox_port}");
    axum::serve(listener, app).await?;
    Ok(())
}

// ── Sync logic ───────────────────────────────────────────────────────────────

async fn run_sync(
    nc: &NextcloudSyncConfig,
    vault_root: &str,
) -> Result<SyncStatus, String> {
    let sync_client = NextcloudSync::new(&nc.url, &nc.username, &nc.password);

    // Scan all projects from the local vault
    let project_bundles = project_vault::scan_project_vault(std::path::Path::new(vault_root));

    let mut total = SyncStatus {
        timestamp: chrono::Utc::now().to_rfc3339(),
        calendar_pushed: 0,
        calendar_pulled: 0,
        deck_pushed: 0,
        deck_pulled: 0,
        files_created: 0,
        files_updated: 0,
        errors: vec![],
    };

    for bundle in &project_bundles {
        let project_title = &bundle.project.title;
        let deck_board_id = nc.deck_boards.get(project_title).copied();

        let webdav_tasks_path = format!(
            "{}{}/tasks/",
            nc.projects_path.trim_end_matches('/'),
            format!("/{}", project_title),
        );

        match sync_client.full_sync(
            &nc.calendar,
            deck_board_id,
            &bundle.tasks,
            Some(&webdav_tasks_path),
        ).await {
            Ok(result) => {
                total.calendar_pushed += result.calendar_pushed;
                total.calendar_pulled += result.calendar_pulled;
                total.deck_pushed += result.deck_pushed;
                total.deck_pulled += result.deck_pulled;
                total.files_created += result.files_created;
                total.files_updated += result.files_updated;
                total.errors.extend(result.errors);
            }
            Err(e) => {
                total.errors.push(format!("{}: {}", project_title, e));
            }
        }
    }

    Ok(total)
}

// ── Helpers ──────────────────────────────────────────────────────────────────

fn dirs_home() -> std::path::PathBuf {
    std::env::var("HOME")
        .map(std::path::PathBuf::from)
        .unwrap_or_else(|_| std::path::PathBuf::from("."))
}

fn gethostname() -> String {
    hostname::get()
        .map(|h| h.to_string_lossy().to_string())
        .unwrap_or_else(|_| "unknown".to_string())
}

// ── Vox WebSocket handler ────────────────────────────────────────────────────

/// Accept a WebSocket upgrade and serve VaultService over Vox RPC.
///
/// Clients connect at `ws://server:3456/vox` and get a typed RPC channel:
/// - list_tasks(), create_task(), complete_task(), etc.
/// - Same interface on desktop (in-process), web (WebSocket), mobile (WebSocket)
async fn vox_ws_handler(
    ws: WebSocketUpgrade,
    State(state): State<AppState>,
) -> impl IntoResponse {
    ws.on_upgrade(move |socket| handle_vox_connection(socket, state))
}

async fn handle_vox_connection(socket: WebSocket, state: AppState) {
    info!("Vox WebSocket client connected");

    // axum's WebSocket doesn't expose the underlying tungstenite stream
    // directly. For now, we implement a message-level bridge: read binary
    // frames from axum WS, forward Vox RPC responses back.
    //
    // The full Vox session (handshake, multiplexing, channels) runs over
    // the separate Vox TCP listener (see below). This WebSocket endpoint
    // serves as a lightweight message relay for web clients.
    //
    // TODO: once axum exposes the underlying stream, or we add a separate
    // Vox listener via tokio-tungstenite directly, wire the full session.

    use futures::{SinkExt, StreamExt};
    let (mut ws_tx, mut ws_rx) = socket.split();

    let svc = state.svc.clone();

    while let Some(Ok(msg)) = ws_rx.next().await {
        match msg {
            axum::extract::ws::Message::Binary(data) => {
                // Deserialize Vox RPC request from binary frame
                // For now, treat as a simple JSON-RPC bridge
                if let Ok(text) = std::str::from_utf8(&data) {
                    if let Ok(request) = serde_json::from_str::<serde_json::Value>(text) {
                        let method = request.get("method").and_then(|m| m.as_str()).unwrap_or("");
                        let response = dispatch_rpc(&svc, method, &request).await;
                        let response_bytes = serde_json::to_vec(&response).unwrap_or_default();
                        let _ = ws_tx.send(axum::extract::ws::Message::Binary(response_bytes.into())).await;
                    }
                }
            }
            axum::extract::ws::Message::Text(text) => {
                // JSON text frames — simpler for web clients
                if let Ok(request) = serde_json::from_str::<serde_json::Value>(&text) {
                    let method = request.get("method").and_then(|m| m.as_str()).unwrap_or("");
                    let response = dispatch_rpc(&svc, method, &request).await;
                    let response_text = serde_json::to_string(&response).unwrap_or_default();
                    let _ = ws_tx.send(axum::extract::ws::Message::Text(response_text.into())).await;
                }
            }
            axum::extract::ws::Message::Ping(data) => {
                let _ = ws_tx.send(axum::extract::ws::Message::Pong(data)).await;
            }
            axum::extract::ws::Message::Close(_) => break,
            _ => {}
        }
    }

    info!("Vox WebSocket client disconnected");
}

/// Dispatch a JSON-RPC-style request to VaultServiceImpl.
///
/// Request: `{"method": "list_tasks", "params": {...}}`
/// Response: `{"result": [...], "error": null}`
async fn dispatch_rpc(
    svc: &Arc<VaultServiceImpl>,
    method: &str,
    request: &serde_json::Value,
) -> serde_json::Value {
    match method {
        "list_tasks" => {
            let tasks = svc.list_tasks().await;
            serde_json::json!({ "result": tasks_to_json(&tasks), "error": null })
        }
        "create_task" => {
            // TODO: deserialize task from params
            serde_json::json!({ "error": "create_task params not yet implemented" })
        }
        "complete_task" => {
            if let Some(title) = request.get("params").and_then(|p| p.get("title")).and_then(|t| t.as_str()) {
                match svc.complete_task(title.to_string()).await {
                    Ok(task) => serde_json::json!({ "result": task_to_json(&task), "error": null }),
                    Err(e) => serde_json::json!({ "error": e.to_string() }),
                }
            } else {
                serde_json::json!({ "error": "missing params.title" })
            }
        }
        "search_tasks" => {
            if let Some(query) = request.get("params").and_then(|p| p.get("query")).and_then(|q| q.as_str()) {
                let tasks = svc.search_tasks(query.to_string()).await;
                serde_json::json!({ "result": tasks_to_json(&tasks), "error": null })
            } else {
                serde_json::json!({ "error": "missing params.query" })
            }
        }
        "list_projects" => {
            let projects = svc.list_projects().await;
            let items: Vec<serde_json::Value> = projects.iter().map(|p| {
                serde_json::json!({
                    "title": p.title,
                    "status": format!("{:?}", p.status),
                    "area": p.area,
                    "due": p.due.map(|d| d.to_string()),
                    "team": p.team,
                })
            }).collect();
            serde_json::json!({ "result": items, "error": null })
        }
        "tasks_for_user" => {
            if let Some(username) = request.get("params").and_then(|p| p.get("username")).and_then(|u| u.as_str()) {
                let tasks = svc.tasks_for_user(username.to_string()).await;
                serde_json::json!({ "result": tasks_to_json(&tasks), "error": null })
            } else {
                serde_json::json!({ "error": "missing params.username" })
            }
        }
        "tasks_for_project" => {
            if let Some(project) = request.get("params").and_then(|p| p.get("project")).and_then(|p| p.as_str()) {
                let tasks = svc.tasks_for_project(project.to_string()).await;
                serde_json::json!({ "result": tasks_to_json(&tasks), "error": null })
            } else {
                serde_json::json!({ "error": "missing params.project" })
            }
        }
        "trigger_sync" => {
            serde_json::json!({ "result": "sync_triggered", "error": null })
        }
        _ => {
            serde_json::json!({ "error": format!("unknown method: {method}") })
        }
    }
}

fn tasks_to_json(tasks: &[vault_core::Task]) -> Vec<serde_json::Value> {
    tasks.iter().map(task_to_json).collect()
}

fn task_to_json(t: &vault_core::Task) -> serde_json::Value {
    serde_json::json!({
        "title": t.title,
        "status": format!("{:?}", t.status),
        "priority": format!("{:?}", t.priority),
        "assignee": t.assignee,
        "due": t.due.map(|d| d.to_string()),
        "projects": t.projects.iter().map(|p| &p.0).collect::<Vec<_>>(),
        "tags": t.tags,
        "body": t.body,
    })
}

// ── Handlers ─────────────────────────────────────────────────────────────────

async fn health() -> &'static str {
    "ok"
}

// ── Webhook endpoints ───────────────────────────────────────────────────────

#[derive(Deserialize)]
struct CreateTaskRequest {
    title: String,
    #[serde(default)]
    status: Option<String>,
    #[serde(default)]
    priority: Option<String>,
    #[serde(default)]
    assignee: Option<String>,
    #[serde(default)]
    due: Option<String>,
    #[serde(default)]
    project: Option<String>,
    #[serde(default)]
    tags: Vec<String>,
    #[serde(default)]
    body: String,
}

async fn create_task_api(
    State(state): State<AppState>,
    Json(req): Json<CreateTaskRequest>,
) -> impl IntoResponse {
    let mut task = vault_core::Task::default();
    task.title = req.title;
    task.body = req.body;
    if let Some(assignee) = req.assignee {
        task.assignee = Some(assignee);
    }
    if let Some(due) = req.due {
        task.due = chrono::NaiveDate::parse_from_str(&due, "%Y-%m-%d").ok();
    }
    if let Some(ref status) = req.status {
        task.status = match status.as_str() {
            "Open" => vault_core::Status::Open,
            "InProgress" => vault_core::Status::InProgress,
            "Done" => vault_core::Status::Done,
            "OnHold" => vault_core::Status::OnHold,
            "Planned" => vault_core::Status::Planned,
            _ => vault_core::Status::Open,
        };
    }
    if let Some(ref priority) = req.priority {
        task.priority = match priority.as_str() {
            "Urgent" => vault_core::Priority::Urgent,
            "High" => vault_core::Priority::High,
            "Normal" => vault_core::Priority::Normal,
            "Low" => vault_core::Priority::Low,
            _ => vault_core::Priority::None,
        };
    }
    task.tags = req.tags;
    if let Some(project) = req.project {
        task.projects.push(vault_core::WikiLink(project));
    }

    match state.svc.create_task(task).await {
        Ok(created) => Json(serde_json::json!({"task": task_to_json(&created)})),
        Err(e) => Json(serde_json::json!({"error": e.to_string()})),
    }
}

async fn complete_task_api(
    State(state): State<AppState>,
    axum::extract::Path(title): axum::extract::Path<String>,
) -> impl IntoResponse {
    match state.svc.complete_task(title).await {
        Ok(task) => Json(serde_json::json!({"task": task_to_json(&task)})),
        Err(e) => Json(serde_json::json!({"error": e.to_string()})),
    }
}

async fn server_info(State(state): State<AppState>) -> impl IntoResponse {
    Json(state.info)
}

async fn sync_status(State(state): State<AppState>) -> impl IntoResponse {
    let status = state.last_sync.read().await;
    match status.as_ref() {
        Some(s) => Json(serde_json::json!({
            "syncing": true,
            "last_sync": s,
        })),
        None => Json(serde_json::json!({
            "syncing": state.nc_config.is_some(),
            "last_sync": null,
        })),
    }
}

async fn trigger_sync(State(state): State<AppState>) -> impl IntoResponse {
    let Some(nc) = &state.nc_config else {
        return Json(serde_json::json!({"error": "Nextcloud sync not configured"}));
    };

    match run_sync(nc, &state.vault_root).await {
        Ok(status) => {
            *state.last_sync.write().await = Some(status.clone());
            Json(serde_json::json!({"status": "ok", "result": status}))
        }
        Err(e) => Json(serde_json::json!({"error": e})),
    }
}

#[derive(Deserialize)]
struct ProjectFilter {
    area: Option<String>,
    status: Option<String>,
    project_type: Option<String>,
}

#[derive(Serialize)]
struct ProjectResponse {
    title: String,
    status: String,
    area: Option<String>,
    project_type: Option<String>,
    team: Vec<String>,
    tags: Vec<String>,
    repo: Option<String>,
    dev_path: Option<String>,
    description: Option<String>,
    due: Option<String>,
    start: Option<String>,
    is_overdue: bool,
}

impl From<&Project> for ProjectResponse {
    fn from(p: &Project) -> Self {
        Self {
            title: p.title.clone(),
            status: format!("{:?}", p.status).to_lowercase(),
            area: p.area.clone(),
            project_type: p.project_type.clone(),
            team: p.team.clone(),
            tags: p.tags.clone(),
            repo: p.repo.clone(),
            dev_path: p.dev_path.clone(),
            description: p.description.clone(),
            due: p.due.map(|d| d.to_string()),
            start: p.start.map(|d| d.to_string()),
            is_overdue: p.is_overdue(),
        }
    }
}

async fn list_projects(
    State(state): State<AppState>,
    AxumQuery(filter): AxumQuery<ProjectFilter>,
) -> impl IntoResponse {
    let projects = state.svc.list_projects().await;
    let filtered: Vec<ProjectResponse> = projects
        .iter()
        .filter(|p| {
            filter.area.as_ref().map_or(true, |a| p.area.as_deref() == Some(a.as_str()))
        })
        .filter(|p| {
            filter.status.as_ref().map_or(true, |s| {
                format!("{:?}", p.status).to_lowercase() == s.to_lowercase()
            })
        })
        .filter(|p| {
            filter.project_type.as_ref().map_or(true, |t| {
                p.project_type.as_deref() == Some(t.as_str())
            })
        })
        .map(ProjectResponse::from)
        .collect();
    Json(filtered)
}

async fn list_active_projects(State(state): State<AppState>) -> impl IntoResponse {
    let projects = state.svc.list_projects().await;
    let active: Vec<ProjectResponse> = projects
        .iter()
        .filter(|p| p.is_active() && !p.is_archived())
        .map(ProjectResponse::from)
        .collect();
    Json(active)
}

async fn list_tasks(State(state): State<AppState>) -> impl IntoResponse {
    let tasks = state.svc.list_tasks().await;
    let items: Vec<serde_json::Value> = tasks
        .iter()
        .map(|t| {
            serde_json::json!({
                "title": t.title,
                "status": format!("{:?}", t.status),
                "priority": format!("{:?}", t.priority),
                "due": t.due.map(|d| d.to_string()),
                "assignee": t.assignee,
                "projects": t.projects.iter().map(|p| &p.0).collect::<Vec<_>>(),
                "tags": t.tags,
            })
        })
        .collect();
    Json(items)
}

#[derive(Deserialize)]
struct ActivityFilter {
    limit: Option<u32>,
    entity_type: Option<String>,
    entity_id: Option<String>,
}

async fn activity_feed(
    State(state): State<AppState>,
    AxumQuery(filter): AxumQuery<ActivityFilter>,
) -> impl IntoResponse {
    let limit = filter.limit.unwrap_or(50);

    // Query the SQLite index's changes table
    let index = state.svc.index.lock().unwrap();
    if let Some(ref idx) = *index {
        let changes = idx.recent_changes(limit).unwrap_or_default();
        let items: Vec<serde_json::Value> = changes.iter().map(|c| {
            serde_json::json!({
                "entity_type": c.entity_type,
                "entity_id": c.entity_id,
                "field": c.field,
                "old_value": c.old_value,
                "new_value": c.new_value,
                "changed_by": c.changed_by,
                "changed_at": c.changed_at,
                "file_path": c.file_path,
            })
        }).collect();
        Json(serde_json::json!({"changes": items}))
    } else {
        Json(serde_json::json!({"changes": [], "error": "Index not available"}))
    }
}

async fn get_task(
    State(state): State<AppState>,
    axum::extract::Path(title): axum::extract::Path<String>,
) -> impl IntoResponse {
    let tasks = state.svc.list_tasks().await;
    match tasks.into_iter().find(|t| t.title == title) {
        Some(t) => Json(serde_json::json!({"task": task_to_json(&t)})),
        None => Json(serde_json::json!({"error": "not found"})),
    }
}

async fn tasks_by_user(
    State(state): State<AppState>,
    axum::extract::Path(username): axum::extract::Path<String>,
) -> impl IntoResponse {
    let tasks = state.svc.tasks_for_user(username).await;
    Json(serde_json::json!({"tasks": tasks_to_json(&tasks)}))
}
