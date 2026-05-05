use std::sync::Arc;

use axum::extract::ws::{Message as AxumWsMessage, WebSocket};
use axum::extract::{Path as AxumPath, Query, State, WebSocketUpgrade};
use axum::http::{HeaderMap, StatusCode, header};
use axum::response::{Html, IntoResponse, Response};
use axum::routing::get;
use axum::{Json, Router};
use base64::Engine as _;
use better_auth::AxumIntegration;
use better_auth_core::adapters::{MemberOps, OrganizationOps, UserOps};
use better_auth_core::config::AuthConfig;
use better_auth_core::{CreateMember, CreateOrganization, CreateUser};
use chrono::Utc;
use serde::Serialize;
use serde_json::json;
use task_core::VaultServiceImpl;
use task_core::crdt::{CrdtSyncEngine, SyncOp};
use task_core::workflows::{
    DownloadBundle, DownloadPortal, PortalVisibility, parse_download_portal,
};
use task_db::SeaOrmAuthAdapter;
use task_db::entities::auth::{auth_member, auth_organization, auth_session};
use task_db::sea_orm::{
    self, ActiveModelTrait, ColumnTrait, EntityTrait, QueryFilter, QueryOrder, Set,
};
use tokio::sync::{mpsc, oneshot};
use tower_http::cors::CorsLayer;
use tracing::{info, warn};

// ── App state ────────────────────────────────────────────────────────────────

#[derive(Clone, Serialize)]
struct ServerInfo {
    name: String,
    id: String,
    public_base_url: String,
    db: String,
    vault_enabled: bool,
    demo_seeded: bool,
}

#[derive(Clone)]
struct AppState {
    info: ServerInfo,
    db: sea_orm::DatabaseConnection,
    crdt: Option<Arc<CrdtSyncEngine>>,
    vault_service: Option<Arc<VaultServiceImpl>>,
    vault_root: Option<String>,
}

#[derive(Clone, Serialize)]
struct ServerRoute {
    server_id: String,
    server_name: String,
    base_url: String,
    vox_url: String,
    crdt_url: String,
    local: bool,
}

#[derive(Clone, Serialize)]
struct OrganizationRoute {
    organization_id: String,
    slug: String,
    name: String,
    server_id: String,
    server_name: String,
    server_url: String,
    vox_url: String,
    crdt_url: String,
    local: bool,
}

#[derive(Clone, Debug)]
struct VoxAuthContext {
    user_id: String,
    session_id: String,
    organization_id: String,
    member_id: String,
    role: String,
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
        let host = hostname::get()
            .unwrap_or_default()
            .to_string_lossy()
            .to_string();
        format!("{}-{}", host, server_name)
    });

    // ── Database initialization ─────────────────────────────────
    let (db, db_label) = init_server_db().await?;
    info!(db = %db_label, "Database initialized");

    // ── Auth initialization (better-auth) ───────────────────────
    let bind_addr = std::env::var("BIND_ADDR").unwrap_or_else(|_| "0.0.0.0:3456".to_string());
    let auth_secret = std::env::var("AUTH_SECRET")
        .unwrap_or_else(|_| "task-server-secret-key-must-be-at-least-32-chars".to_string());
    let auth_base_url =
        std::env::var("PUBLIC_BASE_URL").unwrap_or_else(|_| format!("http://{bind_addr}"));
    let auth_config = AuthConfig::new(&auth_secret).base_url(&auth_base_url);

    let auth_db = SeaOrmAuthAdapter::new(db.clone());
    let auth = Arc::new(
        better_auth::AuthBuilder::new(auth_config)
            .database(auth_db)
            .plugin(better_auth_api::plugins::EmailPasswordPlugin::new().enable_signup(true))
            .plugin(better_auth_api::plugins::SessionManagementPlugin::new())
            .plugin(better_auth_api::plugins::OrganizationPlugin::new())
            .plugin(better_auth_api::plugins::VoxRpcPlugin::new())
            .plugin(better_auth_api::plugins::GhostUserPlugin::new())
            .build()
            .await
            .expect("Failed to initialize auth"),
    );
    info!("Auth system initialized (better-auth)");

    let vault_path = std::env::var("TASK_VAULT")
        .ok()
        .or_else(|| std::env::var("VAULT_ROOT").ok());
    let (crdt, vault_service) = if let Some(path) = vault_path.as_deref() {
        let peer = std::env::var("TASK_PEER_ID")
            .ok()
            .and_then(|s| s.parse::<u64>().ok())
            .unwrap_or_else(|| {
                let mut hash = 1469598103934665603u64;
                for b in server_id.as_bytes() {
                    hash ^= *b as u64;
                    hash = hash.wrapping_mul(1099511628211);
                }
                hash
            });
        let engine = Arc::new(CrdtSyncEngine::new(std::path::Path::new(path), peer));
        let service = Arc::new(VaultServiceImpl::new(path));
        spawn_crdt_conflict_persister(engine.clone(), service.clone());
        spawn_crdt_file_watch_bridge(engine.clone(), service.clone());
        info!(vault = %path, peer, "Loro CRDT engine enabled");
        (Some(engine), Some(service))
    } else {
        info!("TASK_VAULT is not set; Loro CRDT endpoint will report disabled");
        (None, None)
    };

    let vault_enabled = vault_service.is_some();
    let demo_seeded = env_truthy_default("TASK_SEED_DEMO", true);
    let info_payload = ServerInfo {
        name: server_name.clone(),
        id: server_id,
        public_base_url: auth_base_url.clone(),
        db: db_label,
        vault_enabled,
        demo_seeded,
    };

    if demo_seeded {
        seed_auth_data(&auth, &info_payload).await;
        if let Ok(token) = std::env::var("TASK_TEST_SESSION_TOKEN") {
            seed_test_session(&db, &token).await;
        }
        info!("Auth mock data seeded");
    } else {
        info!("Demo seed disabled by TASK_SEED_DEMO=0");
    }

    let state = AppState {
        crdt,
        vault_service,
        vault_root: vault_path.clone(),
        db,
        info: info_payload,
    };

    info!(name = %server_name, "server starting");

    let app = Router::new()
        // Vox WebSocket RPC — typed service calls over WebSocket
        .route("/vox", get(vox_ws_handler))
        // Loro CRDT sync — realtime task document replication
        .route("/crdt", get(crdt_ws_handler))
        // Minimal HTTP metadata endpoints; domain operations go through Vox.
        .route("/api/info", get(server_info))
        .route("/api/servers", get(server_routes))
        .route("/api/organizations/routes", get(organization_routes))
        .route("/api/crdt/status", get(crdt_status))
        .route("/api/health", get(health))
        .route("/portal/{slug}", get(portal_page))
        .route("/portal/{slug}/{bundle_id}", get(portal_bundle_page))
        .route(
            "/portal/{slug}/{bundle_id}/file/{file_index}",
            get(portal_file),
        )
        .layer(CorsLayer::permissive());

    // Mount better-auth routes under /api/auth
    let auth_router = auth.clone().axum_router();
    let app = app.nest("/api/auth", auth_router.with_state(auth.clone()));

    let app = app.with_state(state);

    // ── HTTP + Vox WebSocket server ──────────────────────────────
    let listener = tokio::net::TcpListener::bind(&bind_addr).await
        .map_err(|e| eyre::eyre!("Failed to bind {bind_addr}: {e}. Is another task-server still running? Kill it with: pkill -f task-server"))?;
    info!("HTTP server listening on {}", bind_addr);
    info!("Endpoints:");
    info!("  Info:          http://{bind_addr}/api/info");
    info!("  Health:        http://{bind_addr}/api/health");
    info!("  Auth:          http://{bind_addr}/api/auth/*");
    info!("  Vox WS:        ws://{bind_addr}/vox");
    axum::serve(listener, app).await?;
    Ok(())
}

async fn init_server_db() -> eyre::Result<(sea_orm::DatabaseConnection, String)> {
    if let Ok(url) = std::env::var("TASK_DATABASE_URL").or_else(|_| std::env::var("DATABASE_URL")) {
        let db = task_db::init(&url).await?;
        return Ok((db, redact_db_url(&url)));
    }

    if let Ok(path) = std::env::var("TASK_DB_PATH") {
        if let Some(parent) = std::path::Path::new(&path).parent() {
            if !parent.as_os_str().is_empty() {
                std::fs::create_dir_all(parent)?;
            }
        }
        let db = task_db::init_file(&path).await?;
        return Ok((db, format!("sqlite://{path}")));
    }

    let db = task_db::init_memory().await?;
    Ok((db, "sqlite::memory:".to_string()))
}

fn redact_db_url(url: &str) -> String {
    if url.contains('@') {
        "<database-url>".to_string()
    } else {
        url.to_string()
    }
}

fn env_truthy_default(name: &str, default: bool) -> bool {
    std::env::var(name)
        .map(|v| match v.to_ascii_lowercase().as_str() {
            "1" | "true" | "yes" | "on" => true,
            "0" | "false" | "no" | "off" => false,
            _ => default,
        })
        .unwrap_or(default)
}

/// Seed mock users and organizations into better-auth.
async fn seed_auth_data(
    auth: &Arc<better_auth::BetterAuth<SeaOrmAuthAdapter>>,
    server: &ServerInfo,
) {
    let db = auth.database();

    // ── Users ───────────────────────────────────────────────────────
    struct MockUser {
        id: &'static str,
        username: &'static str,
        name: &'static str,
        email: &'static str,
        role_title: &'static str,
        department: &'static str,
        account_status: &'static str,
    }

    let users = [
        MockUser {
            id: "user_cody",
            username: "cody",
            name: "Cody Wright",
            email: "cody@fasttrackstudio.com",
            role_title: "Founder & Producer",
            department: "leadership",
            account_status: "claimed",
        },
        MockUser {
            id: "user_amy",
            username: "amy",
            name: "Amy Chen",
            email: "amy@fasttrackstudio.com",
            role_title: "Creative Director",
            department: "leadership",
            account_status: "claimed",
        },
        MockUser {
            id: "user_carter",
            username: "carter",
            name: "Carter Whitlock",
            email: "carter@fasttrackstudio.com",
            role_title: "Drummer & Live Sound",
            department: "music",
            account_status: "claimed",
        },
        MockUser {
            id: "user_tom",
            username: "tom",
            name: "Tom Brooks",
            email: "tom@fasttrackstudio.com",
            role_title: "Guitarist",
            department: "music",
            account_status: "claimed",
        },
        MockUser {
            id: "user_bri",
            username: "bri",
            name: "Bri Zacharias",
            email: "bri@fasttrackstudio.com",
            role_title: "Bass & Tour Manager",
            department: "music",
            account_status: "claimed",
        },
        MockUser {
            id: "user_kai",
            username: "kai",
            name: "Kai Nakamura",
            email: "kai@fasttrackstudio.com",
            role_title: "Backend Developer",
            department: "engineering",
            account_status: "claimed",
        },
        MockUser {
            id: "user_luna",
            username: "luna",
            name: "Luna Zhang",
            email: "luna@fasttrackstudio.com",
            role_title: "Frontend Developer",
            department: "engineering",
            account_status: "claimed",
        },
        MockUser {
            id: "user_elena",
            username: "elena",
            name: "Elena Vasquez",
            email: "elena.vasquez@gmail.com",
            role_title: "Keys & Arrangements",
            department: "music",
            account_status: "invited",
        },
        MockUser {
            id: "user_marcus",
            username: "marcus",
            name: "Marcus Cole",
            email: "marcus@mixengineer.com",
            role_title: "Mix Engineer",
            department: "music",
            account_status: "invited",
        },
        MockUser {
            id: "user_jade",
            username: "jade",
            name: "Jade Kim",
            email: "jade@sterling-sound.com",
            role_title: "Mastering Engineer",
            department: "music",
            account_status: "invited",
        },
        MockUser {
            id: "user_devon",
            username: "devon",
            name: "Devon Miles",
            email: "devon.miles@outlook.com",
            role_title: "Event Coordinator",
            department: "events",
            account_status: "invited",
        },
        MockUser {
            id: "user_alex",
            username: "alex",
            name: "Alex Petrov",
            email: "alex.petrov@proton.me",
            role_title: "DevOps Engineer",
            department: "engineering",
            account_status: "invited",
        },
        MockUser {
            id: "user_noah",
            username: "noah",
            name: "Noah Park",
            email: "noah@fasttrackstudio.com",
            role_title: "Recording Engineer",
            department: "music",
            account_status: "placeholder",
        },
        MockUser {
            id: "user_priya",
            username: "priya",
            name: "Priya Sharma",
            email: "priya@fasttrackstudio.com",
            role_title: "Vocal Coach",
            department: "music",
            account_status: "placeholder",
        },
        MockUser {
            id: "user_riley",
            username: "riley",
            name: "Riley Foster",
            email: "riley@fasttrackstudio.com",
            role_title: "Marketing Lead",
            department: "events",
            account_status: "placeholder",
        },
        MockUser {
            id: "user_jordan",
            username: "jordan",
            name: "Jordan Lee",
            email: "jordan@fasttrackstudio.com",
            role_title: "Lighting & Stage",
            department: "events",
            account_status: "placeholder",
        },
        MockUser {
            id: "user_sam",
            username: "sam",
            name: "Sam Rivera",
            email: "sam@fasttrackstudio.com",
            role_title: "Video & Content",
            department: "events",
            account_status: "placeholder",
        },
        MockUser {
            id: "user_mira",
            username: "mira",
            name: "Mira Okafor",
            email: "mira@fasttrackstudio.com",
            role_title: "QA Engineer",
            department: "engineering",
            account_status: "placeholder",
        },
        MockUser {
            id: "user_omar",
            username: "omar",
            name: "Omar Hassan",
            email: "omar@fasttrackstudio.com",
            role_title: "Business Manager",
            department: "business",
            account_status: "placeholder",
        },
        MockUser {
            id: "user_tess",
            username: "tess",
            name: "Tess Moreno",
            email: "tess@fasttrackstudio.com",
            role_title: "A&R",
            department: "business",
            account_status: "placeholder",
        },
    ];

    let password_hash = better_auth_core::hash_password(None, "mock-password-123!")
        .await
        .expect("Failed to hash mock password");

    for u in &users {
        let mut create = CreateUser::new()
            .with_email(u.email)
            .with_name(u.name)
            .with_username(u.username)
            .with_email_verified(u.account_status == "claimed")
            .with_metadata(serde_json::json!({
                "password_hash": password_hash,
                "role_title": u.role_title,
                "department": u.department,
                "account_status": u.account_status,
            }));
        create.id = Some(u.id.to_string());
        if let Err(e) = db.create_user(create).await {
            warn!(user = u.username, error = %e, "Failed to seed user");
        }
    }
    info!("Seeded {} users into better-auth", users.len());

    // ── Organizations ───────────────────────────────────────────────
    struct MockOrg {
        id: &'static str,
        name: &'static str,
        slug: &'static str,
        emoji: &'static str,
        hue: u64,
        description: &'static str,
        owner: &'static str,
        members: &'static [&'static str],
    }

    let orgs = [
        MockOrg {
            id: "org_personal",
            name: "Personal",
            slug: "personal",
            emoji: "user",
            hue: 0,
            description: "Private tasks and projects",
            owner: "cody",
            members: &["cody"],
        },
        MockOrg {
            id: "org_fta",
            name: "FastTrackAudio",
            slug: "fasttrackaudio",
            emoji: "music",
            hue: 210,
            description: "Music production company — albums, EPs, mixing clients",
            owner: "cody",
            members: &[
                "cody", "amy", "carter", "tom", "bri", "elena", "marcus", "jade", "noah", "priya",
                "tess", "omar",
            ],
        },
        MockOrg {
            id: "org_fts",
            name: "FastTrackStudio",
            slug: "fasttrackstudio",
            emoji: "code",
            hue: 270,
            description: "Software development — audio tools, plugins, infrastructure",
            owner: "cody",
            members: &["cody", "tom", "kai", "luna", "alex", "mira"],
        },
        MockOrg {
            id: "org_jf",
            name: "Just Friends",
            slug: "just-friends",
            emoji: "guitar",
            hue: 145,
            description: "Band project — recurring gigs, rehearsals, recordings",
            owner: "cody",
            members: &["cody", "amy", "carter", "tom", "bri", "elena"],
        },
        MockOrg {
            id: "org_tbm",
            name: "TomBrooksMusic",
            slug: "tombrooksmusic",
            emoji: "music2",
            hue: 35,
            description: "Tom Brooks' solo artist projects and collaborations",
            owner: "tom",
            members: &["tom", "cody", "marcus"],
        },
    ];

    for org in &orgs {
        let mut create =
            CreateOrganization::new(org.name, org.slug).with_metadata(serde_json::json!({
                "emoji": org.emoji,
                "hue": org.hue,
                "description": org.description,
                "owner": org.owner,
                "server_id": server.id,
                "server_name": server.name,
                "server_url": server.public_base_url,
            }));
        create.id = Some(org.id.to_string());
        if let Err(e) = db.create_organization(create).await {
            warn!(org = org.slug, error = %e, "Failed to seed org");
            continue;
        }

        for username in org.members {
            let user_id = format!("user_{username}");
            let role = if *username == org.owner {
                "owner"
            } else {
                "member"
            };
            let create_member = CreateMember::new(org.id, &user_id, role);
            if let Err(e) = db.create_member(create_member).await {
                warn!(org = org.slug, user = username, error = %e, "Failed to add member");
            }
        }
    }
    info!("Seeded {} organizations into better-auth", orgs.len());
}

async fn seed_test_session(db: &sea_orm::DatabaseConnection, token: &str) {
    if token.trim().is_empty() {
        return;
    }

    let now = Utc::now();
    let session = auth_session::ActiveModel {
        id: Set("session_test_agent".to_string()),
        expires_at: Set(now + chrono::Duration::days(7)),
        token: Set(token.to_string()),
        created_at: Set(now),
        updated_at: Set(now),
        ip_address: Set(Some("127.0.0.1".to_string())),
        user_agent: Set(Some("task-server e2e".to_string())),
        user_id: Set("user_cody".to_string()),
        impersonated_by: Set(None),
        active_organization_id: Set(Some("org_fts".to_string())),
        active: Set(true),
    };

    if let Err(e) = session.insert(db).await {
        warn!(error = %e, "Failed to seed TASK_TEST_SESSION_TOKEN session");
    }
}

// ── Loro CRDT sync ───────────────────────────────────────────────────────────

async fn crdt_status(State(state): State<AppState>) -> Json<serde_json::Value> {
    match state.crdt.as_ref() {
        Some(engine) => Json(json!({
            "enabled": true,
            "peer": engine.local_peer().to_string(),
            "loaded_documents": engine.loaded_count().await,
        })),
        None => Json(json!({
            "enabled": false,
            "reason": "TASK_VAULT is not set",
        })),
    }
}

async fn crdt_ws_handler(ws: WebSocketUpgrade, State(state): State<AppState>) -> impl IntoResponse {
    ws.on_upgrade(move |socket| handle_crdt_connection(socket, state))
}

async fn handle_crdt_connection(socket: WebSocket, state: AppState) {
    let Some(engine) = state.crdt.clone() else {
        return;
    };

    info!("Loro CRDT WebSocket client connected");

    use futures::{SinkExt, StreamExt};
    let (mut ws_tx, mut ws_rx) = socket.split();
    let mut sync_rx = engine.subscribe();
    let mut conflict_rx = engine.subscribe_conflicts();

    let ready = json!({
        "type": "ready",
        "peer": engine.local_peer().to_string(),
        "protocol": "task.loro.v1",
    });
    let _ = ws_tx
        .send(axum::extract::ws::Message::Text(ready.to_string().into()))
        .await;

    loop {
        tokio::select! {
            msg = ws_rx.next() => {
                match msg {
                    Some(Ok(msg)) => match msg {
                    axum::extract::ws::Message::Text(text) => {
                        let response = handle_crdt_request(&engine, &text).await;
                        if let Some(response) = response {
                            let _ = ws_tx.send(axum::extract::ws::Message::Text(response.to_string().into())).await;
                        }
                    }
                    axum::extract::ws::Message::Binary(data) => {
                        if let Ok(text) = std::str::from_utf8(&data) {
                            let response = handle_crdt_request(&engine, text).await;
                            if let Some(response) = response {
                                let _ = ws_tx.send(axum::extract::ws::Message::Text(response.to_string().into())).await;
                            }
                        }
                    }
                    axum::extract::ws::Message::Ping(data) => {
                        let _ = ws_tx.send(axum::extract::ws::Message::Pong(data)).await;
                    }
                    axum::extract::ws::Message::Close(_) => break,
                    _ => {}
                    },
                    Some(Err(e)) => {
                        warn!(error = %e, "Loro CRDT WebSocket receive error");
                        break;
                    }
                    None => break,
                };
            }
            recv = sync_rx.recv() => {
                match recv {
                    Ok(op) => {
                        let event = sync_op_to_json(op);
                        let _ = ws_tx.send(axum::extract::ws::Message::Text(event.to_string().into())).await;
                    }
                    Err(tokio::sync::broadcast::error::RecvError::Lagged(skipped)) => {
                        let event = json!({"type": "lagged", "skipped": skipped});
                        let _ = ws_tx.send(axum::extract::ws::Message::Text(event.to_string().into())).await;
                    }
                    Err(tokio::sync::broadcast::error::RecvError::Closed) => break,
                }
            }
            recv = conflict_rx.recv() => {
                match recv {
                    Ok(conflict) => {
                        let event = json!({
                            "type": "conflict",
                            "path": conflict.file_path,
                            "field": conflict.field,
                            "losing_value": conflict.losing_value,
                            "winning_value": conflict.winning_value,
                            "losing_peer": conflict.losing_peer.map(|p| p.to_string()),
                            "winning_peer": conflict.winning_peer.map(|p| p.to_string()),
                        });
                        let _ = ws_tx.send(axum::extract::ws::Message::Text(event.to_string().into())).await;
                    }
                    Err(tokio::sync::broadcast::error::RecvError::Lagged(skipped)) => {
                        let event = json!({"type": "conflict_lagged", "skipped": skipped});
                        let _ = ws_tx.send(axum::extract::ws::Message::Text(event.to_string().into())).await;
                    }
                    Err(tokio::sync::broadcast::error::RecvError::Closed) => break,
                }
            }
            else => break,
        }
    }

    info!("Loro CRDT WebSocket client disconnected");
}

async fn handle_crdt_request(
    engine: &Arc<CrdtSyncEngine>,
    text: &str,
) -> Option<serde_json::Value> {
    let request: serde_json::Value = match serde_json::from_str(text) {
        Ok(v) => v,
        Err(e) => return Some(json!({"type": "error", "error": format!("invalid JSON: {e}")})),
    };

    let id = request
        .get("id")
        .cloned()
        .unwrap_or(serde_json::Value::Null);
    let kind = request.get("type").and_then(|t| t.as_str()).unwrap_or("");
    let path = request
        .get("path")
        .and_then(|p| p.as_str())
        .unwrap_or("")
        .trim_start_matches('/');

    match kind {
        "hello" => Some(json!({
            "type": "ready",
            "id": id,
            "peer": engine.local_peer().to_string(),
            "protocol": "task.loro.v1",
        })),
        "snapshot" => {
            if path.is_empty() {
                return Some(json!({"type": "error", "id": id, "error": "missing path"}));
            }
            match engine.export_snapshot(path).await {
                Ok(Some(bytes)) => {
                    let task = engine.task(path).await.map(task_to_crdt_json);
                    Some(json!({
                        "type": "snapshot",
                        "id": id,
                        "path": path,
                        "data": encode_b64(&bytes),
                        "task": task,
                    }))
                }
                Ok(None) => Some(
                    json!({"type": "error", "id": id, "path": path, "error": "document not found"}),
                ),
                Err(e) => {
                    Some(json!({"type": "error", "id": id, "path": path, "error": e.to_string()}))
                }
            }
        }
        "task" => {
            if path.is_empty() {
                return Some(json!({"type": "error", "id": id, "error": "missing path"}));
            }
            let task = engine.task(path).await.map(task_to_crdt_json);
            Some(json!({"type": "task", "id": id, "path": path, "task": task}))
        }
        "field" => {
            let field = request.get("field").and_then(|f| f.as_str()).unwrap_or("");
            let value = request.get("value").and_then(|v| v.as_str()).unwrap_or("");
            if path.is_empty() || field.is_empty() {
                return Some(json!({"type": "error", "id": id, "error": "missing path or field"}));
            }
            match engine.apply_field_change(path, field, value).await {
                Ok(()) => Some(json!({"type": "ok", "id": id, "path": path})),
                Err(e) => {
                    Some(json!({"type": "error", "id": id, "path": path, "error": e.to_string()}))
                }
            }
        }
        "body" => {
            let body = request.get("body").and_then(|b| b.as_str()).unwrap_or("");
            if path.is_empty() {
                return Some(json!({"type": "error", "id": id, "error": "missing path"}));
            }
            match engine.apply_body_change(path, body).await {
                Ok(()) => Some(json!({"type": "ok", "id": id, "path": path})),
                Err(e) => {
                    Some(json!({"type": "error", "id": id, "path": path, "error": e.to_string()}))
                }
            }
        }
        "update" => {
            let Some(data) = request.get("data").and_then(|d| d.as_str()) else {
                return Some(json!({"type": "error", "id": id, "error": "missing data"}));
            };
            if path.is_empty() {
                return Some(json!({"type": "error", "id": id, "error": "missing path"}));
            }
            let bytes = match decode_b64(data) {
                Ok(bytes) => bytes,
                Err(e) => {
                    return Some(json!({"type": "error", "id": id, "path": path, "error": e}));
                }
            };
            match engine.apply_remote_update(path, &bytes).await {
                Ok(()) => Some(json!({"type": "ok", "id": id, "path": path})),
                Err(e) => {
                    Some(json!({"type": "error", "id": id, "path": path, "error": e.to_string()}))
                }
            }
        }
        "subscribe" => Some(json!({"type": "ok", "id": id, "subscribed": true})),
        _ => Some(
            json!({"type": "error", "id": id, "error": format!("unknown CRDT message type: {kind}")}),
        ),
    }
}

fn sync_op_to_json(op: SyncOp) -> serde_json::Value {
    match op {
        SyncOp::FieldChanged {
            file_path,
            field,
            value,
            peer,
        } => json!({
            "type": "field_changed",
            "path": file_path,
            "field": field,
            "value": value,
            "peer": peer.map(|p| p.to_string()),
        }),
        SyncOp::DocUpdate { file_path, update } => json!({
            "type": "doc_update",
            "path": file_path,
            "data": encode_b64(&update),
        }),
        SyncOp::TaskCreated { file_path, task } => json!({
            "type": "task_created",
            "path": file_path,
            "task": task_to_crdt_json(task),
        }),
        SyncOp::TaskDeleted { file_path } => json!({
            "type": "task_deleted",
            "path": file_path,
        }),
        SyncOp::Refresh => json!({"type": "refresh"}),
    }
}

fn task_to_crdt_json(task: task_core::Task) -> serde_json::Value {
    json!({
        "id": task.id,
        "title": task.title,
        "status": format!("{:?}", task.status),
        "priority": format!("{:?}", task.priority),
        "projects": task.projects.into_iter().map(|p| p.0).collect::<Vec<_>>(),
        "contexts": task.contexts,
        "tags": task.tags,
        "due": task.due.map(|d| d.to_string()),
        "scheduled": task.scheduled.map(|d| d.to_string()),
        "assignee": task.assignee,
        "body": task.body,
    })
}

fn encode_b64(bytes: &[u8]) -> String {
    base64::engine::general_purpose::STANDARD.encode(bytes)
}

fn decode_b64(data: &str) -> Result<Vec<u8>, String> {
    base64::engine::general_purpose::STANDARD
        .decode(data)
        .map_err(|e| format!("invalid base64: {e}"))
}

fn spawn_crdt_conflict_persister(
    engine: Arc<CrdtSyncEngine>,
    vault_service: Arc<VaultServiceImpl>,
) {
    tokio::spawn(async move {
        let mut conflicts = engine.subscribe_conflicts();
        while let Ok(conflict) = conflicts.recv().await {
            let winning_peer = conflict.winning_peer.map(|p| p.to_string());
            let losing_peer = conflict.losing_peer.map(|p| p.to_string());
            let result = vault_service
                .record_conflict(
                    "task",
                    &conflict.file_path,
                    &conflict.field,
                    conflict.winning_value.as_deref(),
                    conflict.losing_value.as_deref(),
                    winning_peer.as_deref(),
                    losing_peer.as_deref(),
                    Some(&conflict.file_path),
                    "concurrent",
                )
                .await;
            if let Err(e) = result {
                warn!(path = %conflict.file_path, field = %conflict.field, error = %e, "failed to persist CRDT conflict");
            }
        }
    });
}

fn spawn_crdt_file_watch_bridge(engine: Arc<CrdtSyncEngine>, vault_service: Arc<VaultServiceImpl>) {
    tokio::spawn(async move {
        let handles = vault_service.watch_all().await;
        for handle in &handles {
            if let Err(e) = handle {
                warn!(error = %e, "failed to start CRDT file watcher");
            }
        }

        if let Err(e) = engine.rescan_vault().await {
            warn!(error = %e, "initial CRDT vault scan failed");
        }

        let mut changes = vault_service.subscribe();
        loop {
            if changes.changed().await.is_err() {
                break;
            }
            if let Err(e) = engine.rescan_vault().await {
                warn!(error = %e, "CRDT vault rescan failed");
            }
        }

        drop(handles);
    });
}

// ── Vox WebSocket handler ────────────────────────────────────────────────────

async fn vox_ws_handler(
    ws: WebSocketUpgrade,
    headers: HeaderMap,
    Query(query): Query<std::collections::HashMap<String, String>>,
    State(state): State<AppState>,
) -> Response {
    match authenticate_vox_request(&state, &headers, &query).await {
        Ok(auth) => ws
            .on_upgrade(move |socket| handle_vox_connection(socket, state, auth))
            .into_response(),
        Err(status) => status.into_response(),
    }
}

async fn handle_vox_connection(socket: WebSocket, state: AppState, auth: VoxAuthContext) {
    info!(
        user_id = %auth.user_id,
        session_id = %auth.session_id,
        organization_id = %auth.organization_id,
        member_id = %auth.member_id,
        role = %auth.role,
        "Vox WebSocket client connected"
    );

    let service = state.vault_service.clone();
    let db = state.db.clone();

    let request_auth = auth.clone();
    let factory = vox::acceptor_fn(
        move |request: &vox::ConnectionRequest,
              connection: vox::PendingConnection|
              -> Result<(), vox::Metadata<'static>> {
            info!(
                user_id = %request_auth.user_id,
                organization_id = %request_auth.organization_id,
                role = %request_auth.role,
                service = request.service(),
                "Vox service accepted"
            );
            match request.service() {
                "TaskRepo" => {
                    connection.handle_with(task_core::task::TaskRepoDispatcher::new(
                        task_core::task::TaskRepoStorage::new(db.clone()),
                    ));
                    Ok(())
                }
                "ProjectRepo" => {
                    connection.handle_with(task_core::project::ProjectRepoDispatcher::new(
                        task_core::project::ProjectRepoStorage::new(db.clone()),
                    ));
                    Ok(())
                }
                "ClientRepo" => {
                    connection.handle_with(task_core::client::ClientRepoDispatcher::new(
                        task_core::client::ClientRepoStorage::new(db.clone()),
                    ));
                    Ok(())
                }
                "ExpenseRepo" => {
                    connection.handle_with(task_core::expense::ExpenseRepoDispatcher::new(
                        task_core::expense::ExpenseRepoStorage::new(db.clone()),
                    ));
                    Ok(())
                }
                "RevenueRepo" => {
                    connection.handle_with(task_core::revenue::RevenueRepoDispatcher::new(
                        task_core::revenue::RevenueRepoStorage::new(db.clone()),
                    ));
                    Ok(())
                }
                "CalendarEventRepo" => {
                    connection.handle_with(
                        task_core::calendar_event::CalendarEventRepoDispatcher::new(
                            task_core::calendar_event::CalendarEventRepoStorage::new(db.clone()),
                        ),
                    );
                    Ok(())
                }
                "TeamMemberRepo" => {
                    connection.handle_with(task_core::team::TeamMemberRepoDispatcher::new(
                        task_core::team::TeamMemberRepoStorage::new(db.clone()),
                    ));
                    Ok(())
                }
                "SavedViewRepo" => {
                    connection.handle_with(task_core::views::SavedViewRepoDispatcher::new(
                        task_core::views::SavedViewRepoStorage::new(db.clone()),
                    ));
                    Ok(())
                }
                "AssetRepo" => {
                    connection.handle_with(task_core::asset::AssetRepoDispatcher::new(
                        task_core::asset::AssetRepoStorage::new(db.clone()),
                    ));
                    Ok(())
                }
                "InvoiceRepo" => {
                    connection.handle_with(task_core::invoice::InvoiceRepoDispatcher::new(
                        task_core::invoice::InvoiceRepoStorage::new(db.clone()),
                    ));
                    Ok(())
                }
                "CycleRepo" => {
                    connection.handle_with(task_core::cycle::CycleRepoDispatcher::new(
                        task_core::cycle::CycleRepoStorage::new(db.clone()),
                    ));
                    Ok(())
                }
                "LocationRepo" => {
                    connection.handle_with(task_core::location::LocationRepoDispatcher::new(
                        task_core::location::LocationRepoStorage::new(db.clone()),
                    ));
                    Ok(())
                }
                "ModuleRepo" => {
                    connection.handle_with(task_core::module::ModuleRepoDispatcher::new(
                        task_core::module::ModuleRepoStorage::new(db.clone()),
                    ));
                    Ok(())
                }
                "EmailRefRepo" => {
                    connection.handle_with(task_core::email::EmailRefRepoDispatcher::new(
                        task_core::email::EmailRefRepoStorage::new(db.clone()),
                    ));
                    Ok(())
                }
                "PersonRepo" => {
                    connection.handle_with(task_core::people::PersonRepoDispatcher::new(
                        task_core::people::PersonRepoStorage::new(db.clone()),
                    ));
                    Ok(())
                }
                "IntegrationRepo" => {
                    connection.handle_with(task_core::integration::IntegrationRepoDispatcher::new(
                        task_core::integration::IntegrationRepoStorage::new(db.clone()),
                    ));
                    Ok(())
                }
                "ActivityRepo" => {
                    connection.handle_with(task_core::activity::ActivityRepoDispatcher::new(
                        task_core::activity::ActivityRepoStorage::new(db.clone()),
                    ));
                    Ok(())
                }
                "CommentRepo" => {
                    connection.handle_with(task_core::comment::CommentRepoDispatcher::new(
                        task_core::comment::CommentRepoStorage::new(db.clone()),
                    ));
                    Ok(())
                }
                "ReactionRepo" => {
                    connection.handle_with(task_core::reaction::ReactionRepoDispatcher::new(
                        task_core::reaction::ReactionRepoStorage::new(db.clone()),
                    ));
                    Ok(())
                }
                "NotificationRepo" => {
                    connection.handle_with(
                        task_core::notification::NotificationRepoDispatcher::new(
                            task_core::notification::NotificationRepoStorage::new(db.clone()),
                        ),
                    );
                    Ok(())
                }
                "TaskRelationRepo" => {
                    connection.handle_with(
                        task_core::task_relation::TaskRelationRepoDispatcher::new(
                            task_core::task_relation::TaskRelationRepoStorage::new(db.clone()),
                        ),
                    );
                    Ok(())
                }
                "Noop" => {
                    connection.handle_with(());
                    Ok(())
                }
                "TaskService" => {
                    connection.handle_with(task_core::TaskServiceDispatcher::new(
                        task_core::TaskBusinessService::new(task_core::task::TaskRepoStorage::new(
                            db.clone(),
                        )),
                    ));
                    Ok(())
                }
                "ProjectService" => {
                    connection.handle_with(task_core::ProjectServiceDispatcher::new(
                        task_core::ProjectBusinessService::new(
                            task_core::project::ProjectRepoStorage::new(db.clone()),
                            task_core::task::TaskRepoStorage::new(db.clone()),
                        ),
                    ));
                    Ok(())
                }
                "ExpenseService" => {
                    connection.handle_with(task_core::ExpenseServiceDispatcher::new(
                        task_core::ExpenseBusinessService::new(
                            task_core::expense::ExpenseRepoStorage::new(db.clone()),
                        ),
                    ));
                    Ok(())
                }
                "CalendarService" => {
                    connection.handle_with(task_core::CalendarServiceDispatcher::new(
                        task_core::CalendarBusinessService::new(
                            task_core::task::TaskRepoStorage::new(db.clone()),
                            task_core::calendar_event::CalendarEventRepoStorage::new(db.clone()),
                        ),
                    ));
                    Ok(())
                }
                _ if service.is_none() => {
                    warn!(
                        service = request.service(),
                        "Vox service requires TASK_VAULT compatibility service"
                    );
                    Err(vec![])
                }
                "InboxService" => {
                    let service = service.as_ref().expect("service checked above");
                    connection.handle_with(task_core::InboxServiceDispatcher::new(
                        service.as_ref().clone(),
                    ));
                    Ok(())
                }
                "TimeService" => {
                    let service = service.as_ref().expect("service checked above");
                    connection.handle_with(task_core::TimeServiceDispatcher::new(
                        service.as_ref().clone(),
                    ));
                    Ok(())
                }
                "PeopleService" => {
                    let service = service.as_ref().expect("service checked above");
                    connection.handle_with(task_core::PeopleServiceDispatcher::new(
                        service.as_ref().clone(),
                    ));
                    Ok(())
                }
                "ConversationService" => {
                    let service = service.as_ref().expect("service checked above");
                    connection.handle_with(task_core::ConversationServiceDispatcher::new(
                        service.as_ref().clone(),
                    ));
                    Ok(())
                }
                "OperatingService" => {
                    let service = service.as_ref().expect("service checked above");
                    connection.handle_with(task_core::OperatingServiceDispatcher::new(
                        service.as_ref().clone(),
                    ));
                    Ok(())
                }
                "InvoiceService" => {
                    let service = service.as_ref().expect("service checked above");
                    connection.handle_with(task_core::InvoiceServiceDispatcher::new(
                        service.as_ref().clone(),
                    ));
                    Ok(())
                }
                "ActivityService" => {
                    let service = service.as_ref().expect("service checked above");
                    connection.handle_with(task_core::ActivityServiceDispatcher::new(
                        service.as_ref().clone(),
                    ));
                    Ok(())
                }
                "MailService" => {
                    let service = service.as_ref().expect("service checked above");
                    connection.handle_with(task_core::MailServiceDispatcher::new(
                        service.as_ref().clone(),
                    ));
                    Ok(())
                }
                "FileService" => {
                    let service = service.as_ref().expect("service checked above");
                    connection.handle_with(task_core::FileServiceDispatcher::new(
                        service.as_ref().clone(),
                    ));
                    Ok(())
                }
                "SystemService" => {
                    let service = service.as_ref().expect("service checked above");
                    connection.handle_with(task_core::SystemServiceDispatcher::new(
                        service.as_ref().clone(),
                    ));
                    Ok(())
                }
                other => {
                    warn!(service = other, "Unknown Vox service requested");
                    Err(vec![])
                }
            }
        },
    );

    let (closed_tx, closed_rx) = oneshot::channel();
    let root = match vox::acceptor_on(AxumWsLink::new(socket, closed_tx))
        .on_connection(factory)
        .establish::<vox::NoopClient>()
        .await
    {
        Ok(root) => root,
        Err(e) => {
            warn!(error = %e, "Vox WebSocket session failed");
            return;
        }
    };

    let _root = root;
    let _ = closed_rx.await;

    info!(
        user_id = %auth.user_id,
        organization_id = %auth.organization_id,
        "Vox WebSocket client disconnected"
    );
}

async fn authenticate_vox_request(
    state: &AppState,
    headers: &HeaderMap,
    query: &std::collections::HashMap<String, String>,
) -> Result<VoxAuthContext, StatusCode> {
    let Some(token) = extract_session_token(headers, query) else {
        warn!("Vox WebSocket rejected: missing auth session token");
        return Err(StatusCode::UNAUTHORIZED);
    };

    let session = auth_session::Entity::find()
        .filter(auth_session::Column::Token.eq(&token))
        .one(&state.db)
        .await
        .map_err(|e| {
            warn!(error = %e, "Vox WebSocket rejected: session lookup failed");
            StatusCode::INTERNAL_SERVER_ERROR
        })?
        .filter(|session| session.active && session.expires_at > Utc::now())
        .ok_or_else(|| {
            warn!("Vox WebSocket rejected: invalid or expired session");
            StatusCode::UNAUTHORIZED
        })?;

    let organization_id =
        requested_organization(headers, query).or_else(|| session.active_organization_id.clone());

    let member = match organization_id {
        Some(org_id) => auth_member::Entity::find()
            .filter(auth_member::Column::OrganizationId.eq(org_id))
            .filter(auth_member::Column::UserId.eq(&session.user_id))
            .one(&state.db)
            .await
            .map_err(|e| {
                warn!(error = %e, "Vox WebSocket rejected: member lookup failed");
                StatusCode::INTERNAL_SERVER_ERROR
            })?,
        None => {
            let memberships = auth_member::Entity::find()
                .filter(auth_member::Column::UserId.eq(&session.user_id))
                .all(&state.db)
                .await
                .map_err(|e| {
                    warn!(error = %e, "Vox WebSocket rejected: membership lookup failed");
                    StatusCode::INTERNAL_SERVER_ERROR
                })?;
            if memberships.len() == 1 {
                memberships.into_iter().next()
            } else {
                warn!(
                    user_id = %session.user_id,
                    membership_count = memberships.len(),
                    "Vox WebSocket rejected: no unambiguous active organization"
                );
                return Err(StatusCode::FORBIDDEN);
            }
        }
    }
    .ok_or_else(|| {
        warn!(
            user_id = %session.user_id,
            "Vox WebSocket rejected: user is not a member of the requested organization"
        );
        StatusCode::FORBIDDEN
    })?;

    Ok(VoxAuthContext {
        user_id: session.user_id,
        session_id: session.id,
        organization_id: member.organization_id,
        member_id: member.id,
        role: member.role,
    })
}

fn extract_session_token(
    headers: &HeaderMap,
    query: &std::collections::HashMap<String, String>,
) -> Option<String> {
    header_value(headers, "authorization")
        .and_then(|value| value.strip_prefix("Bearer ").map(str::to_string))
        .or_else(|| query.get("token").cloned())
        .or_else(|| query.get("session_token").cloned())
        .or_else(|| query.get("sessionToken").cloned())
        .or_else(|| cookie_value(headers, "better-auth.session-token"))
}

fn requested_organization(
    headers: &HeaderMap,
    query: &std::collections::HashMap<String, String>,
) -> Option<String> {
    header_value(headers, "x-task-organization-id")
        .or_else(|| header_value(headers, "x-organization-id"))
        .or_else(|| query.get("organization_id").cloned())
        .or_else(|| query.get("organizationId").cloned())
        .or_else(|| query.get("org_id").cloned())
        .filter(|value| !value.is_empty())
}

fn header_value(headers: &HeaderMap, name: &str) -> Option<String> {
    headers
        .get(name)
        .and_then(|value| value.to_str().ok())
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .map(str::to_string)
}

fn cookie_value(headers: &HeaderMap, name: &str) -> Option<String> {
    let cookie = header_value(headers, "cookie")?;
    cookie.split(';').find_map(|part| {
        let (key, value) = part.trim().split_once('=')?;
        (key == name && !value.is_empty()).then(|| value.to_string())
    })
}

struct AxumWsLink {
    socket: WebSocket,
    closed: oneshot::Sender<()>,
}

impl AxumWsLink {
    fn new(socket: WebSocket, closed: oneshot::Sender<()>) -> Self {
        Self { socket, closed }
    }
}

impl vox::Link for AxumWsLink {
    type Tx = AxumWsTx;
    type Rx = AxumWsRx;

    fn split(self) -> (Self::Tx, Self::Rx) {
        let (tx_out, rx_out) = mpsc::channel::<Vec<u8>>(1);
        let (tx_in, rx_in) = mpsc::channel::<Result<AxumWsMessage, AxumWsError>>(1);
        let io_task = tokio::spawn(axum_ws_io_loop(self.socket, rx_out, tx_in, self.closed));

        (
            AxumWsTx {
                tx: tx_out,
                io_task,
            },
            AxumWsRx { rx: rx_in },
        )
    }
}

async fn axum_ws_io_loop(
    socket: WebSocket,
    mut rx_out: mpsc::Receiver<Vec<u8>>,
    tx_in: mpsc::Sender<Result<AxumWsMessage, AxumWsError>>,
    closed: oneshot::Sender<()>,
) {
    use futures::{SinkExt, StreamExt};
    let (mut ws_tx, mut ws_rx) = socket.split();
    let _closed = NotifyOnDrop(Some(closed));

    loop {
        tokio::select! {
            outbound = rx_out.recv() => {
                match outbound {
                    Some(bytes) => {
                        if let Err(e) = ws_tx.feed(AxumWsMessage::Binary(bytes.into())).await {
                            let _ = tx_in.send(Err(AxumWsError(e.to_string()))).await;
                            return;
                        }
                        while let Ok(bytes) = rx_out.try_recv() {
                            if let Err(e) = ws_tx.feed(AxumWsMessage::Binary(bytes.into())).await {
                                let _ = tx_in.send(Err(AxumWsError(e.to_string()))).await;
                                return;
                            }
                        }
                        if let Err(e) = ws_tx.flush().await {
                            let _ = tx_in.send(Err(AxumWsError(e.to_string()))).await;
                            return;
                        }
                    }
                    None => return,
                }
            }
            inbound = ws_rx.next() => {
                match inbound {
                    Some(Ok(msg)) => {
                        if tx_in.send(Ok(msg)).await.is_err() {
                            return;
                        }
                    }
                    Some(Err(e)) => {
                        let _ = tx_in.send(Err(AxumWsError(e.to_string()))).await;
                        return;
                    }
                    None => return,
                }
            }
        }
    }
}

struct NotifyOnDrop(Option<oneshot::Sender<()>>);

impl Drop for NotifyOnDrop {
    fn drop(&mut self) {
        if let Some(tx) = self.0.take() {
            let _ = tx.send(());
        }
    }
}

struct AxumWsTx {
    tx: mpsc::Sender<Vec<u8>>,
    io_task: tokio::task::JoinHandle<()>,
}

impl vox::LinkTx for AxumWsTx {
    async fn send(&self, bytes: Vec<u8>) -> std::io::Result<()> {
        let permit = self.tx.clone().reserve_owned().await.map_err(|_| {
            std::io::Error::new(
                std::io::ErrorKind::ConnectionReset,
                "axum websocket writer task stopped",
            )
        })?;
        drop(permit.send(bytes));
        Ok(())
    }

    async fn close(self) -> std::io::Result<()> {
        drop(self.tx);
        self.io_task.await.map_err(std::io::Error::other)
    }
}

struct AxumWsRx {
    rx: mpsc::Receiver<Result<AxumWsMessage, AxumWsError>>,
}

#[derive(Debug)]
struct AxumWsError(String);

impl std::fmt::Display for AxumWsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "axum websocket: {}", self.0)
    }
}

impl std::error::Error for AxumWsError {}

impl vox::LinkRx for AxumWsRx {
    type Error = AxumWsError;

    async fn recv(&mut self) -> Result<Option<vox::Backing>, Self::Error> {
        loop {
            match self.rx.recv().await {
                Some(Ok(AxumWsMessage::Binary(data))) => {
                    return Ok(Some(vox::Backing::Boxed(
                        Vec::from(data).into_boxed_slice(),
                    )));
                }
                Some(Ok(AxumWsMessage::Close(_))) | None => return Ok(None),
                Some(Ok(AxumWsMessage::Ping(_) | AxumWsMessage::Pong(_))) => continue,
                Some(Ok(AxumWsMessage::Text(_))) => {
                    return Err(AxumWsError(
                        "text frames are not valid Vox websocket payloads".to_string(),
                    ));
                }
                Some(Err(e)) => return Err(e),
            }
        }
    }
}

// ── HTTP metadata handlers ──────────────────────────────────────────────────

async fn health() -> &'static str {
    "ok"
}

async fn server_info(State(state): State<AppState>) -> impl IntoResponse {
    Json(state.info)
}

async fn server_routes(State(state): State<AppState>) -> Json<Vec<ServerRoute>> {
    Json(vec![route_for_server(&state.info)])
}

async fn portal_page(
    State(state): State<AppState>,
    AxumPath(slug): AxumPath<String>,
    Query(query): Query<std::collections::HashMap<String, String>>,
) -> Response {
    match load_portal(&state, &slug) {
        Ok(portal) => render_portal_response(&portal, None, &query),
        Err(response) => response,
    }
}

async fn portal_bundle_page(
    State(state): State<AppState>,
    AxumPath((slug, bundle_id)): AxumPath<(String, String)>,
    Query(query): Query<std::collections::HashMap<String, String>>,
) -> Response {
    match load_portal(&state, &slug) {
        Ok(portal) => render_portal_response(&portal, Some(&bundle_id), &query),
        Err(response) => response,
    }
}

async fn portal_file(
    State(state): State<AppState>,
    AxumPath((slug, bundle_id, file_index)): AxumPath<(String, String, usize)>,
    Query(query): Query<std::collections::HashMap<String, String>>,
) -> Response {
    let Some(root) = state.vault_root.as_deref() else {
        return (
            StatusCode::SERVICE_UNAVAILABLE,
            "TASK_VAULT is not configured",
        )
            .into_response();
    };
    if !safe_slug(&slug) || !safe_slug(&bundle_id) {
        return (StatusCode::BAD_REQUEST, "invalid portal path").into_response();
    }

    let Some(portal_path) = find_portal_file(std::path::Path::new(root), &slug) else {
        return (StatusCode::NOT_FOUND, "portal not found").into_response();
    };
    let content = match std::fs::read_to_string(&portal_path) {
        Ok(content) => content,
        Err(e) => {
            return (
                StatusCode::INTERNAL_SERVER_ERROR,
                format!("failed to read portal: {e}"),
            )
                .into_response();
        }
    };
    let Some(portal) = parse_download_portal(&content).filter(|portal| portal.slug == slug) else {
        return (
            StatusCode::INTERNAL_SERVER_ERROR,
            "portal frontmatter is invalid",
        )
            .into_response();
    };
    if let Err(response) = authorize_portal(&portal, &query) {
        return response;
    }

    let Some(bundle) = portal.bundles.iter().find(|bundle| bundle.id == bundle_id) else {
        return (StatusCode::NOT_FOUND, "bundle not found").into_response();
    };
    let Some(file) = bundle.files.get(file_index) else {
        return (StatusCode::NOT_FOUND, "file not found").into_response();
    };
    let Some(project_root) = project_root_for_portal(&portal_path) else {
        return (StatusCode::INTERNAL_SERVER_ERROR, "invalid portal location").into_response();
    };
    let Some(relative_path) = safe_project_relative_path(&file.source) else {
        return (StatusCode::BAD_REQUEST, "invalid file source").into_response();
    };
    let path = project_root.join(relative_path);
    let bytes = match std::fs::read(&path) {
        Ok(bytes) => bytes,
        Err(_) => return (StatusCode::NOT_FOUND, "file not found").into_response(),
    };
    let display_name = file.dest.as_deref().unwrap_or_else(|| {
        path.file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("download")
    });
    let mime = file
        .mime_type
        .as_deref()
        .filter(|mime| !mime.is_empty())
        .unwrap_or_else(|| guess_mime_type(display_name));

    (
        [
            (header::CONTENT_TYPE, mime.to_string()),
            (
                header::CONTENT_DISPOSITION,
                format!("inline; filename=\"{}\"", header_escape(display_name)),
            ),
        ],
        bytes,
    )
        .into_response()
}

fn load_portal(state: &AppState, slug: &str) -> Result<DownloadPortal, Response> {
    if !safe_slug(slug) {
        return Err((StatusCode::BAD_REQUEST, "invalid portal slug").into_response());
    }

    let Some(root) = state.vault_root.as_deref() else {
        return Err((
            StatusCode::SERVICE_UNAVAILABLE,
            "TASK_VAULT is not configured",
        )
            .into_response());
    };

    let Some(path) = find_portal_file(std::path::Path::new(root), slug) else {
        return Err((StatusCode::NOT_FOUND, "portal not found").into_response());
    };

    let content = match std::fs::read_to_string(&path) {
        Ok(content) => content,
        Err(e) => {
            return Err((
                StatusCode::INTERNAL_SERVER_ERROR,
                format!("failed to read portal: {e}"),
            )
                .into_response());
        }
    };

    parse_download_portal(&content)
        .filter(|portal| portal.slug == slug)
        .ok_or_else(|| {
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                "portal frontmatter is invalid",
            )
                .into_response()
        })
}

fn find_portal_file(root: &std::path::Path, slug: &str) -> Option<std::path::PathBuf> {
    let mut stack = vec![root.to_path_buf()];
    while let Some(dir) = stack.pop() {
        let entries = std::fs::read_dir(&dir).ok()?;
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                if path.file_name().and_then(|name| name.to_str()) == Some("target") {
                    continue;
                }
                stack.push(path);
                continue;
            }

            if path.file_name().and_then(|name| name.to_str()) != Some("portal.md") {
                continue;
            }

            let content = std::fs::read_to_string(&path).ok()?;
            if parse_download_portal(&content).is_some_and(|portal| portal.slug == slug) {
                return Some(path);
            }
        }
    }
    None
}

fn render_portal_response(
    portal: &DownloadPortal,
    selected_bundle_id: Option<&str>,
    query: &std::collections::HashMap<String, String>,
) -> Response {
    if !portal.published {
        return (StatusCode::NOT_FOUND, "portal is not published").into_response();
    }

    if portal
        .expires
        .is_some_and(|expires| expires < Utc::now().date_naive())
    {
        return (StatusCode::GONE, "portal has expired").into_response();
    }

    if let Some(password) = portal.password.as_deref() {
        let provided = query
            .get("password")
            .or_else(|| query.get("p"))
            .map(String::as_str);
        if provided != Some(password) {
            return (StatusCode::UNAUTHORIZED, Html(render_password_page(portal))).into_response();
        }
    }

    let selected =
        selected_bundle_id.and_then(|id| portal.bundles.iter().find(|bundle| bundle.id == id));
    if selected_bundle_id.is_some() && selected.is_none() {
        return (StatusCode::NOT_FOUND, "bundle not found").into_response();
    }

    Html(render_portal_html(portal, selected)).into_response()
}

fn render_password_page(portal: &DownloadPortal) -> String {
    format!(
        "<!doctype html><html><head><meta charset=\"utf-8\"><title>{}</title>{}</head><body><main class=\"card\"><h1>{}</h1><p>This portal is password protected.</p><form><input name=\"password\" type=\"password\" autofocus><button type=\"submit\">Open portal</button></form></main></body></html>",
        html_escape(&portal.title),
        portal_css(),
        html_escape(&portal.title),
    )
}

fn render_portal_html(portal: &DownloadPortal, selected: Option<&DownloadBundle>) -> String {
    let mut html = format!(
        "<!doctype html><html><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width,initial-scale=1\"><title>{}</title>{}</head><body><main><header><p class=\"eyebrow\">{}</p><h1>{}</h1><p>{}</p></header>",
        html_escape(&portal.title),
        portal_css(),
        html_escape(&portal.event),
        html_escape(&portal.title),
        html_escape(&portal.message),
    );

    html.push_str("<section class=\"grid\">");
    for bundle in &portal.bundles {
        html.push_str(&render_bundle_card(
            portal,
            bundle,
            selected.is_some_and(|chosen| chosen.id == bundle.id),
        ));
    }
    html.push_str("</section>");

    if let Some(bundle) = selected {
        html.push_str(&render_bundle_detail(portal, bundle, &portal.visibility));
    }

    html.push_str("</main></body></html>");
    html
}

fn render_bundle_card(portal: &DownloadPortal, bundle: &DownloadBundle, selected: bool) -> String {
    let selected_class = if selected { " selected" } else { "" };
    let icon = bundle.icon.as_deref().unwrap_or("📦");
    let group = bundle.group.as_deref().unwrap_or("General");
    format!(
        "<a class=\"bundle{}\" href=\"/portal/{}/{}\"><span class=\"icon\">{}</span><span><strong>{}</strong><small>{}</small></span></a>",
        selected_class,
        url_path_segment(&portal.slug),
        url_path_segment(&bundle.id),
        html_escape(icon),
        html_escape(&bundle.name),
        html_escape(group),
    )
}

fn render_bundle_detail(
    portal: &DownloadPortal,
    bundle: &DownloadBundle,
    visibility: &PortalVisibility,
) -> String {
    let mut html = format!(
        "<section class=\"card\"><h2>{}</h2>",
        html_escape(&bundle.name)
    );
    if !bundle.notes.is_empty() {
        html.push_str(&format!("<p>{}</p>", html_escape(&bundle.notes)));
    }

    if let Some(url) = bundle.direct_url.as_deref() {
        html.push_str(&format!(
            "<p><a class=\"button\" href=\"{}\">Open download share</a></p>",
            html_escape(url)
        ));
    }

    html.push_str("<h3>Files</h3><ul>");
    for (index, file) in bundle.files.iter().enumerate() {
        let category = file.category.as_deref().unwrap_or("Files");
        let dest = file.dest.as_deref().unwrap_or(&file.source);
        let file_url = format!(
            "/portal/{}/{}/file/{}",
            url_path_segment(&portal.slug),
            url_path_segment(&bundle.id),
            index
        );
        html.push_str(&format!(
            "<li><a href=\"{}\" download>{}</a><small>{}</small>{}</li>",
            html_escape(&file_url),
            html_escape(dest),
            html_escape(category),
            render_audio_preview(file, &file_url),
        ));
    }
    if bundle.files.is_empty() {
        html.push_str("<li>No explicit files listed yet.</li>");
    }
    html.push_str("</ul>");

    if matches!(visibility, PortalVisibility::BrowseAll) {
        html.push_str("<p class=\"hint\">Other roles are browseable, but download access is scoped to each role share.</p>");
    }
    html.push_str("</section>");
    html
}

fn render_audio_preview(file: &task_core::workflows::BundleFile, file_url: &str) -> String {
    if is_audio_file(file) {
        format!(
            "<audio controls preload=\"metadata\" src=\"{}\"></audio>",
            html_escape(file_url)
        )
    } else {
        String::new()
    }
}

fn is_audio_file(file: &task_core::workflows::BundleFile) -> bool {
    file.mime_type
        .as_deref()
        .is_some_and(|mime| mime.starts_with("audio/"))
        || file
            .dest
            .as_deref()
            .unwrap_or(&file.source)
            .rsplit('.')
            .next()
            .is_some_and(|ext| {
                matches!(
                    ext.to_ascii_lowercase().as_str(),
                    "aac" | "aif" | "aiff" | "flac" | "m4a" | "mp3" | "ogg" | "opus" | "wav"
                )
            })
}

fn authorize_portal(
    portal: &DownloadPortal,
    query: &std::collections::HashMap<String, String>,
) -> Result<(), Response> {
    if !portal.published {
        return Err((StatusCode::NOT_FOUND, "portal is not published").into_response());
    }
    if portal
        .expires
        .is_some_and(|expires| expires < Utc::now().date_naive())
    {
        return Err((StatusCode::GONE, "portal has expired").into_response());
    }
    if let Some(password) = portal.password.as_deref() {
        let provided = query
            .get("password")
            .or_else(|| query.get("p"))
            .map(String::as_str);
        if provided != Some(password) {
            return Err(
                (StatusCode::UNAUTHORIZED, Html(render_password_page(portal))).into_response(),
            );
        }
    }
    Ok(())
}

fn project_root_for_portal(portal_path: &std::path::Path) -> Option<std::path::PathBuf> {
    let parent = portal_path.parent()?;
    if parent.file_name().and_then(|name| name.to_str()) == Some("downloads") {
        parent.parent().map(std::path::Path::to_path_buf)
    } else {
        Some(parent.to_path_buf())
    }
}

fn safe_project_relative_path(source: &str) -> Option<std::path::PathBuf> {
    let path = std::path::Path::new(source);
    if path.is_absolute() {
        return None;
    }
    let mut safe = std::path::PathBuf::new();
    for component in path.components() {
        match component {
            std::path::Component::Normal(part) => safe.push(part),
            std::path::Component::CurDir => {}
            _ => return None,
        }
    }
    (!safe.as_os_str().is_empty()).then_some(safe)
}

fn guess_mime_type(name: &str) -> &'static str {
    match name
        .rsplit('.')
        .next()
        .unwrap_or_default()
        .to_ascii_lowercase()
        .as_str()
    {
        "mp3" => "audio/mpeg",
        "wav" => "audio/wav",
        "flac" => "audio/flac",
        "m4a" => "audio/mp4",
        "ogg" => "audio/ogg",
        "pdf" => "application/pdf",
        "txt" | "md" => "text/plain; charset=utf-8",
        _ => "application/octet-stream",
    }
}

fn header_escape(value: &str) -> String {
    value.replace(['\\', '"', '\r', '\n'], "_")
}

fn portal_css() -> &'static str {
    "<style>body{margin:0;font-family:system-ui,-apple-system,Segoe UI,sans-serif;background:#0b1020;color:#edf2ff}main{max-width:1100px;margin:0 auto;padding:48px 20px}.eyebrow{color:#8fb4ff;text-transform:uppercase;letter-spacing:.12em}h1{font-size:clamp(2rem,6vw,4rem);margin:.2em 0}.grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(220px,1fr));gap:16px;margin:32px 0}.bundle,.card{background:#151d35;border:1px solid #2f3b61;border-radius:18px;padding:20px;color:inherit;text-decoration:none}.bundle{display:flex;gap:14px;align-items:center}.bundle:hover,.selected{border-color:#8fb4ff;background:#1b2748}.icon{font-size:2rem}.bundle small{display:block;color:#aab6d3}.button{display:inline-block;background:#8fb4ff;color:#071022;padding:12px 16px;border-radius:12px;text-decoration:none;font-weight:700}input,button{font:inherit;padding:12px;border-radius:10px;border:0}button{background:#8fb4ff;color:#071022;font-weight:700}li{margin:.6em 0}.hint{color:#aab6d3}</style>"
}

fn safe_slug(value: &str) -> bool {
    !value.is_empty()
        && value
            .bytes()
            .all(|b| b.is_ascii_alphanumeric() || matches!(b, b'-' | b'_'))
}

fn url_path_segment(value: &str) -> String {
    value
        .replace('%', "%25")
        .replace('/', "%2F")
        .replace(' ', "%20")
}

fn html_escape(value: &str) -> String {
    value
        .replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
        .replace('\'', "&#39;")
}

async fn organization_routes(State(state): State<AppState>) -> Json<Vec<OrganizationRoute>> {
    let orgs = match auth_organization::Entity::find()
        .order_by_asc(auth_organization::Column::Slug)
        .all(&state.db)
        .await
    {
        Ok(orgs) => orgs,
        Err(e) => {
            warn!(error = %e, "failed to list organization routes");
            return Json(vec![]);
        }
    };

    let routes = orgs
        .into_iter()
        .map(|org| {
            let metadata = org.metadata.unwrap_or_default();
            let server_id = metadata
                .get("server_id")
                .and_then(|v| v.as_str())
                .unwrap_or(&state.info.id)
                .to_string();
            let server_name = metadata
                .get("server_name")
                .and_then(|v| v.as_str())
                .unwrap_or(&state.info.name)
                .to_string();
            let server_url = metadata
                .get("server_url")
                .and_then(|v| v.as_str())
                .unwrap_or(&state.info.public_base_url)
                .trim_end_matches('/')
                .to_string();
            OrganizationRoute {
                organization_id: org.id,
                slug: org.slug,
                name: org.name,
                vox_url: ws_url(&server_url, "/vox"),
                crdt_url: ws_url(&server_url, "/crdt"),
                local: server_id == state.info.id,
                server_id,
                server_name,
                server_url,
            }
        })
        .collect();

    Json(routes)
}

fn route_for_server(info: &ServerInfo) -> ServerRoute {
    let base_url = info.public_base_url.trim_end_matches('/').to_string();
    ServerRoute {
        server_id: info.id.clone(),
        server_name: info.name.clone(),
        vox_url: ws_url(&base_url, "/vox"),
        crdt_url: ws_url(&base_url, "/crdt"),
        base_url,
        local: true,
    }
}

fn ws_url(base_url: &str, path: &str) -> String {
    let mut url = base_url.trim_end_matches('/').to_string();
    if let Some(rest) = url.strip_prefix("https://") {
        url = format!("wss://{rest}");
    } else if let Some(rest) = url.strip_prefix("http://") {
        url = format!("ws://{rest}");
    }
    format!("{url}{path}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extracts_vox_session_token_from_bearer_query_or_cookie() {
        let mut headers = HeaderMap::new();
        headers.insert("authorization", "Bearer auth-token".parse().unwrap());
        assert_eq!(
            extract_session_token(&headers, &std::collections::HashMap::new()).as_deref(),
            Some("auth-token")
        );

        let mut query = std::collections::HashMap::new();
        query.insert("sessionToken".to_string(), "query-token".to_string());
        assert_eq!(
            extract_session_token(&HeaderMap::new(), &query).as_deref(),
            Some("query-token")
        );

        let mut headers = HeaderMap::new();
        headers.insert(
            "cookie",
            "theme=dark; better-auth.session-token=cookie-token"
                .parse()
                .unwrap(),
        );
        assert_eq!(
            extract_session_token(&headers, &std::collections::HashMap::new()).as_deref(),
            Some("cookie-token")
        );
    }

    #[test]
    fn escapes_portal_html_and_url_segments() {
        assert_eq!(
            html_escape("Tom & <Jerry> \"quote\""),
            "Tom &amp; &lt;Jerry&gt; &quot;quote&quot;"
        );
        assert_eq!(url_path_segment("a b/c%"), "a%20b%2Fc%25");
        assert!(safe_slug("campus-jax_2026"));
        assert!(!safe_slug("../secret"));
    }

    #[test]
    fn renders_audio_files_with_preview_and_download_links() {
        let portal = DownloadPortal {
            slug: "campus-jax".to_string(),
            ..Default::default()
        };
        let bundle = DownloadBundle {
            id: "vocals".to_string(),
            name: "Vocalists".to_string(),
            files: vec![task_core::workflows::BundleFile {
                source: "audio/reference mix.mp3".to_string(),
                dest: Some("Reference Mix.mp3".to_string()),
                category: Some("Audio".to_string()),
                mime_type: Some("audio/mpeg".to_string()),
                ..Default::default()
            }],
            ..Default::default()
        };

        let html = render_bundle_detail(&portal, &bundle, &PortalVisibility::BrowseAll);

        assert!(html.contains("<audio controls"));
        assert!(html.contains("/portal/campus-jax/vocals/file/0"));
        assert!(html.contains("download"));
    }

    #[test]
    fn extracts_requested_organization_from_header_or_query() {
        let mut headers = HeaderMap::new();
        headers.insert("x-task-organization-id", "org_header".parse().unwrap());
        assert_eq!(
            requested_organization(&headers, &std::collections::HashMap::new()).as_deref(),
            Some("org_header")
        );

        let mut query = std::collections::HashMap::new();
        query.insert("organizationId".to_string(), "org_query".to_string());
        assert_eq!(
            requested_organization(&HeaderMap::new(), &query).as_deref(),
            Some("org_query")
        );
    }
}
