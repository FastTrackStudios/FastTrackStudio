use std::sync::Arc;

use axum::extract::ws::{Message as AxumWsMessage, WebSocket};
use axum::extract::{Query, State, WebSocketUpgrade};
use axum::http::{HeaderMap, StatusCode};
use axum::response::{IntoResponse, Response};
use axum::routing::get;
use axum::{Json, Router};
use better_auth::AxumIntegration;
use better_auth_core::adapters::{MemberOps, OrganizationOps, UserOps};
use better_auth_core::config::AuthConfig;
use better_auth_core::{CreateMember, CreateOrganization, CreateUser};
use chrono::Utc;
use serde::Serialize;
use task_core::service::{
    HealthCheck, NextcloudCapability, SystemCapabilities, SystemHealth, VaultCapability,
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
    demo_seeded: bool,
}

#[derive(Clone)]
struct AppState {
    info: ServerInfo,
    db: sea_orm::DatabaseConnection,
}

#[derive(Clone, Serialize)]
struct ServerRoute {
    server_id: String,
    server_name: String,
    base_url: String,
    vox_url: String,
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

    let demo_seeded = env_truthy_default("TASK_SEED_DEMO", true);
    let info_payload = ServerInfo {
        name: server_name.clone(),
        id: server_id,
        public_base_url: auth_base_url.clone(),
        db: db_label,
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
        db,
        info: info_payload,
    };

    info!(name = %server_name, "server starting");

    let app = Router::new()
        // Vox WebSocket RPC — typed service calls over WebSocket
        .route("/vox", get(vox_ws_handler))
        // Minimal HTTP metadata endpoints; domain operations go through Vox.
        .route("/api/info", get(server_info))
        .route("/api/servers", get(server_routes))
        .route("/api/organizations/routes", get(organization_routes))
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

    let db = state.db.clone();
    let info = state.info.clone();

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
                        task_core::TaskServiceImpl::new(task_core::task::TaskRepoStorage::new(
                            db.clone(),
                        )),
                    ));
                    Ok(())
                }
                "ProjectService" => {
                    connection.handle_with(task_core::ProjectServiceDispatcher::new(
                        task_core::ProjectServiceImpl::new(
                            task_core::project::ProjectRepoStorage::new(db.clone()),
                            task_core::task::TaskRepoStorage::new(db.clone()),
                        ),
                    ));
                    Ok(())
                }
                "ExpenseService" => {
                    connection.handle_with(task_core::ExpenseServiceDispatcher::new(
                        task_core::ExpenseServiceImpl::new(
                            task_core::expense::ExpenseRepoStorage::new(db.clone()),
                        ),
                    ));
                    Ok(())
                }
                "CalendarService" => {
                    connection.handle_with(task_core::CalendarServiceDispatcher::new(
                        task_core::CalendarServiceImpl::new(
                            task_core::task::TaskRepoStorage::new(db.clone()),
                            task_core::calendar_event::CalendarEventRepoStorage::new(db.clone()),
                        ),
                    ));
                    Ok(())
                }
                "InvoiceService" => {
                    connection.handle_with(task_core::InvoiceServiceDispatcher::new(
                        task_core::InvoiceServiceImpl::new(
                            task_core::invoice::InvoiceRepoStorage::new(db.clone()),
                        ),
                    ));
                    Ok(())
                }
                "SystemService" => {
                    connection.handle_with(task_core::SystemServiceDispatcher::new(
                        ServerSystemService::new(info.clone()),
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

#[derive(Clone)]
struct ServerSystemService {
    info: ServerInfo,
}

impl ServerSystemService {
    fn new(info: ServerInfo) -> Self {
        Self { info }
    }
}

impl task_core::service::SystemService for ServerSystemService {
    async fn capabilities(&self) -> SystemCapabilities {
        SystemCapabilities {
            package: "task-server".into(),
            version: env!("CARGO_PKG_VERSION").into(),
            protocol_version: 1,
            min_cli_version: "0.1.0".into(),
            min_server_version: "0.1.0".into(),
            services: vec![
                "TaskRepo".into(),
                "ProjectRepo".into(),
                "ClientRepo".into(),
                "ExpenseRepo".into(),
                "RevenueRepo".into(),
                "CalendarEventRepo".into(),
                "TeamMemberRepo".into(),
                "SavedViewRepo".into(),
                "AssetRepo".into(),
                "InvoiceRepo".into(),
                "CycleRepo".into(),
                "LocationRepo".into(),
                "ModuleRepo".into(),
                "EmailRefRepo".into(),
                "PersonRepo".into(),
                "IntegrationRepo".into(),
                "ActivityRepo".into(),
                "CommentRepo".into(),
                "ReactionRepo".into(),
                "NotificationRepo".into(),
                "TaskRelationRepo".into(),
                "TaskService".into(),
                "ProjectService".into(),
                "ExpenseService".into(),
                "CalendarService".into(),
                "SystemService".into(),
            ],
            features: vec![
                "sqlite".into(),
                "sea-orm".into(),
                "generated-repos".into(),
                "vox".into(),
                "auth".into(),
            ],
            nextcloud: NextcloudCapability::default(),
            vault: VaultCapability::default(),
        }
    }

    async fn health(&self, deep: bool) -> SystemHealth {
        SystemHealth {
            ok: true,
            degraded: false,
            deep,
            checks: vec![HealthCheck {
                name: "sqlite".into(),
                code: "SQLITE_OK".into(),
                severity: "ok".into(),
                ok: true,
                configured: true,
                detail: format!("database configured as {}", self.info.db),
                hint: None,
            }],
        }
    }
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
