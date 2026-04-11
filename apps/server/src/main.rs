use std::sync::Arc;

use axum::extract::{Query as AxumQuery, State};
use axum::response::IntoResponse;
use axum::routing::{get, post};
use axum::{Json, Router};
use serde::{Deserialize, Serialize};
use tokio::sync::RwLock;
use tower_http::cors::CorsLayer;
use tracing::{info, warn};
use vault_core::{Project, VaultServiceImpl};
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
        .route("/api/info", get(server_info))
        .route("/api/projects", get(list_projects))
        .route("/api/projects/active", get(list_active_projects))
        .route("/api/tasks", get(list_tasks))
        .route("/api/sync/status", get(sync_status))
        .route("/api/sync/trigger", post(trigger_sync))
        .route("/api/health", get(health))
        .layer(CorsLayer::permissive())
        .with_state(state);

    let bind = std::env::var("BIND_ADDR").unwrap_or_else(|_| "0.0.0.0:3456".to_string());
    let listener = tokio::net::TcpListener::bind(&bind).await?;
    info!("listening on {}", bind);
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

// ── Handlers ─────────────────────────────────────────────────────────────────

async fn health() -> &'static str {
    "ok"
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
