use std::sync::Arc;

use axum::extract::ws::{Message as AxumWsMessage, WebSocket};
use axum::extract::{State, WebSocketUpgrade};
use axum::response::IntoResponse;
use axum::routing::get;
use axum::{Json, Router};
use base64::Engine as _;
use better_auth::AxumIntegration;
use better_auth_core::adapters::{MemberOps, OrganizationOps, UserOps};
use better_auth_core::config::AuthConfig;
use better_auth_core::{CreateMember, CreateOrganization, CreateUser};
use serde::Serialize;
use serde_json::json;
use task_core::crdt::{CrdtSyncEngine, SyncOp};
use task_core::VaultServiceImpl;
use task_db::entities::auth::auth_organization;
use task_db::sea_orm::{self, EntityTrait, QueryOrder};
use task_db::SeaOrmAuthAdapter;
use tokio::sync::mpsc;
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
        info!("Auth mock data seeded");
    } else {
        info!("Demo seed disabled by TASK_SEED_DEMO=0");
    }

    let state = AppState {
        crdt,
        vault_service,
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
                    return Some(json!({"type": "error", "id": id, "path": path, "error": e}))
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

async fn vox_ws_handler(ws: WebSocketUpgrade, State(state): State<AppState>) -> impl IntoResponse {
    ws.on_upgrade(move |socket| handle_vox_connection(socket, state))
}

async fn handle_vox_connection(socket: WebSocket, state: AppState) {
    info!("Vox WebSocket client connected");

    let Some(service) = state.vault_service.clone() else {
        warn!("Vox connection rejected because TASK_VAULT is not configured");
        return;
    };

    let factory = vox::acceptor_fn(
        move |request: &vox::ConnectionRequest,
              connection: vox::PendingConnection|
              -> Result<(), vox::Metadata<'static>> {
            match request.service() {
                "TaskService" => {
                    connection.handle_with(task_core::TaskServiceDispatcher::new(
                        service.as_ref().clone(),
                    ));
                    Ok(())
                }
                "ProjectService" => {
                    connection.handle_with(task_core::ProjectServiceDispatcher::new(
                        service.as_ref().clone(),
                    ));
                    Ok(())
                }
                "TimeService" => {
                    connection.handle_with(task_core::TimeServiceDispatcher::new(
                        service.as_ref().clone(),
                    ));
                    Ok(())
                }
                "ClientService" => {
                    connection.handle_with(task_core::ClientServiceDispatcher::new(
                        service.as_ref().clone(),
                    ));
                    Ok(())
                }
                "InvoiceService" => {
                    connection.handle_with(task_core::InvoiceServiceDispatcher::new(
                        service.as_ref().clone(),
                    ));
                    Ok(())
                }
                "ActivityService" => {
                    connection.handle_with(task_core::ActivityServiceDispatcher::new(
                        service.as_ref().clone(),
                    ));
                    Ok(())
                }
                "CalendarService" => {
                    connection.handle_with(task_core::CalendarServiceDispatcher::new(
                        service.as_ref().clone(),
                    ));
                    Ok(())
                }
                "Noop" => {
                    connection.handle_with(());
                    Ok(())
                }
                other => {
                    warn!(service = other, "Unknown Vox service requested");
                    Err(vec![])
                }
            }
        },
    );

    if let Err(e) = vox::acceptor_on(AxumWsLink::new(socket))
        .on_connection(factory)
        .establish::<vox::NoopClient>()
        .await
    {
        warn!(error = %e, "Vox WebSocket session failed");
    }

    info!("Vox WebSocket client disconnected");
}

struct AxumWsLink {
    socket: WebSocket,
}

impl AxumWsLink {
    fn new(socket: WebSocket) -> Self {
        Self { socket }
    }
}

impl vox::Link for AxumWsLink {
    type Tx = AxumWsTx;
    type Rx = AxumWsRx;

    fn split(self) -> (Self::Tx, Self::Rx) {
        let (tx_out, rx_out) = mpsc::channel::<Vec<u8>>(1);
        let (tx_in, rx_in) = mpsc::channel::<Result<AxumWsMessage, AxumWsError>>(1);
        let io_task = tokio::spawn(axum_ws_io_loop(self.socket, rx_out, tx_in));

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
) {
    use futures::{SinkExt, StreamExt};
    let (mut ws_tx, mut ws_rx) = socket.split();

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
