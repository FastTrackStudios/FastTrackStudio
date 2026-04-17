use std::sync::Arc;

use axum::extract::{Query as AxumQuery, State, WebSocketUpgrade};
use axum::extract::ws::WebSocket;
use axum::response::IntoResponse;
use axum::routing::{get, post};
use axum::{Json, Router};
use chrono::Utc;
use serde::{Deserialize, Serialize};
use tower_http::cors::CorsLayer;
use tower_http::services::{ServeDir, ServeFile};
use tracing::{info, warn};
use better_auth_core::config::AuthConfig;
use better_auth::AxumIntegration;
use better_auth_core::adapters::{UserOps, OrganizationOps, MemberOps};
use better_auth_core::{CreateUser, CreateOrganization, CreateMember};
use task_db::SeaOrmAuthAdapter;
use task_db::sea_orm::{self, EntityTrait, QueryFilter, ColumnTrait, ActiveModelTrait, Set, QueryOrder, PaginatorTrait};
use task_db::entities::{task, project, activity};
use task_db::sea_orm::prelude::Uuid;

// ── App state ────────────────────────────────────────────────────────────────

#[derive(Clone, Serialize)]
struct ServerInfo {
    name: String,
    id: String,
}

#[derive(Clone)]
struct AppState {
    db: sea_orm::DatabaseConnection,
    auth: Arc<better_auth::BetterAuth<SeaOrmAuthAdapter>>,
    info: ServerInfo,
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
        let host = hostname::get().unwrap_or_default().to_string_lossy().to_string();
        format!("{}-{}", host, server_name)
    });


    // ── Database initialization ─────────────────────────────────
    let db = task_db::init_memory().await
        .expect("Failed to initialize in-memory database");
    info!("In-memory database initialized");

    // ── Auth initialization (better-auth) ───────────────────────
    let bind_addr = std::env::var("BIND_ADDR").unwrap_or_else(|_| "0.0.0.0:3456".to_string());
    let auth_config = AuthConfig::new("task-server-secret-key-must-be-at-least-32-chars")
        .base_url(&format!("http://{bind_addr}"));

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
            .expect("Failed to initialize auth")
    );
    info!("Auth system initialized (better-auth)");

    // Seed mock data
    {
        seed_auth_data(&auth).await;
        info!("Auth mock data seeded");
        seed_mock_db(&db).await;
        info!("Mock DB data seeded");
    }

    let state = AppState {
        db,
        auth: auth.clone(),
        info: ServerInfo {
            name: server_name.clone(),
            id: server_id,
        },
    };

    info!(name = %server_name, "server starting");

    let app = Router::new()
        // Vox WebSocket RPC — typed service calls over WebSocket
        .route("/vox", get(vox_ws_handler))
        // REST API — JSON endpoints for simple integrations
        .route("/api/info", get(server_info))
        .route("/api/projects", get(list_projects_handler))
        .route("/api/projects/active", get(list_active_projects_handler))
        .route("/api/tasks", get(list_tasks_handler))
        .route("/api/activity", get(activity_feed))
        .route("/api/tasks", post(create_task_api))
        .route("/api/tasks/{title}", get(get_task))
        .route("/api/tasks/{title}/complete", post(complete_task_api))
        .route("/api/tasks/user/{username}", get(tasks_by_user))
        .route("/api/health", get(health))
        .layer(CorsLayer::permissive());

    // Mount better-auth routes under /api/auth
    let auth_router = auth.clone().axum_router();
    let app = app.nest("/api/auth", auth_router.with_state(auth.clone()));

    // Serve the Dioxus WASM web app — auto-discover dx build output
    let web_dist = std::env::var("WEB_DIST").ok().or_else(|| {
        let candidates = [
            "target/dx/task-web/debug/web/public",
            "apps/web/dist",
            "../web/dist",
        ];
        candidates.iter()
            .map(std::path::Path::new)
            .find(|p| p.join("index.html").exists())
            .map(|p| p.to_string_lossy().to_string())
    });
    let app = if let Some(ref dist_path) = web_dist {
        let index_path = format!("{}/index.html", dist_path);
        info!(path = %dist_path, "Serving web UI from dist directory");
        app.fallback_service(
            ServeDir::new(dist_path)
                .not_found_service(ServeFile::new(&index_path)),
        )
    } else {
        info!("No web UI found — run `cd apps/web && dx build` first, or set WEB_DIST");
        app
    };

    let app = app.with_state(state);

    // ── HTTP + JSON-RPC WebSocket server ─────────────────────────
    let listener = tokio::net::TcpListener::bind(&bind_addr).await
        .map_err(|e| eyre::eyre!("Failed to bind {bind_addr}: {e}. Is another task-server still running? Kill it with: pkill -f task-server"))?;
    info!("HTTP server listening on {}", bind_addr);
    info!("Endpoints:");
    info!("  REST API:      http://{bind_addr}/api/*");
    info!("  Auth:          http://{bind_addr}/api/auth/*");
    info!("  JSON-RPC WS:   ws://{bind_addr}/vox");
    axum::serve(listener, app).await?;
    Ok(())
}

// ── Mock data seeder (SQLite) ──────────────────────────────────────────────

async fn seed_mock_db(db: &sea_orm::DatabaseConnection) {
    // ── Projects ────────────────────────────────────────────────
    let now = Utc::now();

    #[allow(dead_code)]
    struct ProjectSeed {
        title: &'static str,
        slug: &'static str,
        status: &'static str,
        project_type: &'static str,
        description: &'static str,
        organization: &'static str,
        parent_slug: Option<&'static str>,
        team: Vec<&'static str>,
        repo: Option<&'static str>,
    }

    let projects = vec![
        ProjectSeed {
            title: "Demo Album", slug: "demo-album", status: "Active", project_type: "album",
            description: "Full-length demo album. 5 tracks, targeting streaming release late summer.",
            organization: "fasttrackaudio", parent_slug: None,
            team: vec!["cody", "amy", "carter", "tom", "elena", "marcus", "jade", "noah"],
            repo: None,
        },
        ProjectSeed {
            title: "Sunrise", slug: "sunrise", status: "Active", project_type: "song",
            description: "Key: G | Tempo: 120 BPM",
            organization: "fasttrackaudio", parent_slug: Some("demo-album"),
            team: vec!["cody", "amy"], repo: None,
        },
        ProjectSeed {
            title: "City Lights", slug: "city-lights", status: "Active", project_type: "song",
            description: "Key: Bb | Tempo: 95 BPM",
            organization: "fasttrackaudio", parent_slug: Some("demo-album"),
            team: vec!["cody", "amy"], repo: None,
        },
        ProjectSeed {
            title: "Overflow", slug: "overflow", status: "Active", project_type: "song",
            description: "Key: Em | Tempo: 140 BPM",
            organization: "fasttrackaudio", parent_slug: Some("demo-album"),
            team: vec!["cody", "amy"], repo: None,
        },
        ProjectSeed {
            title: "Midnight EP", slug: "midnight-ep", status: "Active", project_type: "ep",
            description: "3-track EP. Dark, moody vibes. Fast turnaround for summer release.",
            organization: "fasttrackaudio", parent_slug: None,
            team: vec!["cody", "amy", "carter", "marcus"], repo: None,
        },
        ProjectSeed {
            title: "Campus Jax Live", slug: "campus-jax-live", status: "Active", project_type: "concert",
            description: "4-band showcase at Campus Jax. Our band headlines, 3 guest acts.",
            organization: "just-friends", parent_slug: None,
            team: vec!["cody", "amy", "carter", "tom", "bri", "devon", "jordan", "sam"],
            repo: None,
        },
        ProjectSeed {
            title: "FTS Sync", slug: "fts-sync", status: "Active", project_type: "code",
            description: "Multi-device sync and clock engine. Ableton Link compatible.",
            organization: "fasttrackstudio", parent_slug: None,
            team: vec!["cody", "kai", "alex"],
            repo: Some("FastTrackStudios/fts-sync"),
        },
        ProjectSeed {
            title: "FTS Session", slug: "fts-session", status: "Active", project_type: "code",
            description: "DAW session management and recall system.",
            organization: "fasttrackstudio", parent_slug: None,
            team: vec!["cody", "kai", "luna"],
            repo: Some("FastTrackStudios/fts-session"),
        },
        ProjectSeed {
            title: "Task App", slug: "task-app", status: "Active", project_type: "code",
            description: "Task management application with real-time sync.",
            organization: "fasttrackstudio", parent_slug: None,
            team: vec!["cody", "kai", "luna", "alex", "mira"],
            repo: Some("FastTrackStudios/task"),
        },
        ProjectSeed {
            title: "Wildflower Album Mix", slug: "wildflower-album-mix", status: "Active", project_type: "mixing-client",
            description: "Full album mix for indie band Wildflower. 10 tracks, stems delivered.",
            organization: "fasttrackaudio", parent_slug: None,
            team: vec!["cody", "marcus", "jade"], repo: None,
        },
        ProjectSeed {
            title: "Tom Solo EP", slug: "tom-solo-ep", status: "Active", project_type: "ep",
            description: "4-track guitar instrumental EP. Fingerstyle + looper.",
            organization: "tombrooksmusic", parent_slug: None,
            team: vec!["tom", "cody", "marcus"], repo: None,
        },
        ProjectSeed {
            title: "Website Redesign", slug: "website-redesign", status: "Planning", project_type: "web",
            description: "Redesign the FastTrackStudio website.",
            organization: "fasttrackstudio", parent_slug: None,
            team: vec!["luna", "cody"], repo: None,
        },
    ];

    for p in &projects {
        let model = project::ActiveModel {
            id: Set(Uuid::new_v4()),
            title: Set(p.title.to_string()),
            slug: Set(p.slug.to_string()),
            status: Set(p.status.to_string()),
            project_type: Set(Some(p.project_type.to_string())),
            description: Set(Some(p.description.to_string())),
            area: Set(Some("Music".to_string())),
            identifier: Set(None),
            next_sequence: Set(None),
            parent_slug: Set(p.parent_slug.map(|s| s.to_string())),
            lead: Set(None),
            default_assignee: Set(None),
            emoji: Set(None),
            organization: Set(Some(p.organization.to_string())),
            team: Set(serde_json::json!(p.team).into()),
            references: Set(serde_json::json!([]).into()),
            due: Set(None),
            start: Set(None),
            file_path: Set(None),
            deleted_at: Set(None),
            archived_at: Set(None),
            created_at: Set(now),
            updated_at: Set(now),
        };
        model.insert(db).await.unwrap();
    }
    info!(count = projects.len(), "Seeded projects");

    // ── Tasks ───────────────────────────────────────────────────
    struct TaskSeed {
        title: &'static str,
        status: &'static str,
        priority: &'static str,
        assignee: &'static str,
        project: &'static str,
        tags: Vec<&'static str>,
        body: &'static str,
        due: Option<chrono::NaiveDate>,
    }

    let tasks = vec![
        TaskSeed {
            title: "Edit vocal comps for Sunrise", status: "InProgress", priority: "High",
            assignee: "cody", project: "sunrise", tags: vec!["editing", "vocals"],
            body: "Comp the best takes from session 3 and crossfade edits.", due: None,
        },
        TaskSeed {
            title: "Mix v3 drums for Sunrise", status: "Open", priority: "High",
            assignee: "marcus", project: "sunrise", tags: vec!["mixing"],
            body: "Kick needs more 60Hz in chorus, snare verb automation.", due: None,
        },
        TaskSeed {
            title: "Master Sunrise", status: "Open", priority: "Normal",
            assignee: "jade", project: "sunrise", tags: vec!["mastering"],
            body: "Target -14 LUFS integrated. 24/96 + 16/44.1.", due: None,
        },
        TaskSeed {
            title: "Record guitar overdubs for City Lights", status: "InProgress", priority: "Normal",
            assignee: "tom", project: "city-lights", tags: vec!["recording"],
            body: "Bridge section needs sustained pad guitar. Consider e-bow.", due: None,
        },
        TaskSeed {
            title: "Write bridge section for Overflow", status: "Open", priority: "High",
            assignee: "amy", project: "overflow", tags: vec!["writing"],
            body: "Current draft feels too literal. Try more abstract imagery.", due: None,
        },
        TaskSeed {
            title: "Mix Midnight EP rough", status: "InProgress", priority: "Normal",
            assignee: "marcus", project: "midnight-ep", tags: vec!["mixing"],
            body: "Dark, spacious mix. Reference: James Blake.", due: None,
        },
        TaskSeed {
            title: "Book sound system for Campus Jax", status: "Done", priority: "Urgent",
            assignee: "devon", project: "campus-jax-live", tags: vec!["logistics"],
            body: "FOH + monitor engineer booked. Backline confirmed.", due: None,
        },
        TaskSeed {
            title: "Design poster for Campus Jax", status: "InProgress", priority: "Normal",
            assignee: "riley", project: "campus-jax-live", tags: vec!["marketing"],
            body: "Neon/retro aesthetic. 4 band names, date, QR code for tickets.", due: None,
        },
        TaskSeed {
            title: "Finalize setlist for Campus Jax", status: "Open", priority: "High",
            assignee: "cody", project: "campus-jax-live", tags: vec!["planning"],
            body: "45-min headline slot. 6 songs + encore.", due: None,
        },
        TaskSeed {
            title: "Fix clock drift bug in sync engine", status: "InProgress", priority: "Urgent",
            assignee: "kai", project: "task-app", tags: vec!["bug"],
            body: "Drift of ~2ms per minute when syncing to external MIDI clock.", due: None,
        },
        TaskSeed {
            title: "Implement WebSocket reconnect", status: "Open", priority: "High",
            assignee: "kai", project: "task-app", tags: vec!["feature"],
            body: "Auto-reconnect with exponential backoff on connection drop.", due: None,
        },
        TaskSeed {
            title: "Design saved views UI", status: "Open", priority: "Normal",
            assignee: "luna", project: "task-app", tags: vec!["design", "ui"],
            body: "Saved filter presets with custom naming and sharing.", due: None,
        },
        TaskSeed {
            title: "Add org switcher filtering", status: "InProgress", priority: "Normal",
            assignee: "luna", project: "task-app", tags: vec!["feature", "ui"],
            body: "Filter projects and tasks by selected organization.", due: None,
        },
        TaskSeed {
            title: "Set up CI/CD pipeline", status: "Open", priority: "High",
            assignee: "alex", project: "task-app", tags: vec!["devops"],
            body: "GitHub Actions for build, test, deploy.", due: None,
        },
        TaskSeed {
            title: "Write integration tests", status: "Open", priority: "Normal",
            assignee: "mira", project: "task-app", tags: vec!["testing"],
            body: "End-to-end tests for task CRUD and sync operations.", due: None,
        },
        TaskSeed {
            title: "DAW session auto-save", status: "Open", priority: "Normal",
            assignee: "kai", project: "fts-session", tags: vec!["feature"],
            body: "Auto-save every 5 minutes with dirty-flag optimization.", due: None,
        },
        TaskSeed {
            title: "Network discovery for sync", status: "Open", priority: "High",
            assignee: "alex", project: "fts-sync", tags: vec!["feature"],
            body: "Discover other Sync instances on the LAN via mDNS/Bonjour.", due: None,
        },
        TaskSeed {
            title: "Mix track 3 for Wildflower", status: "InProgress", priority: "Normal",
            assignee: "marcus", project: "wildflower-album-mix", tags: vec!["mixing"],
            body: "Reference: Phoebe Bridgers. Keep it dreamy.", due: None,
        },
        TaskSeed {
            title: "Master Wildflower final", status: "Open", priority: "Normal",
            assignee: "jade", project: "wildflower-album-mix", tags: vec!["mastering"],
            body: "All 10 tracks mastered. -14 LUFS target.", due: None,
        },
        TaskSeed {
            title: "Record acoustic guitar for Tom EP", status: "InProgress", priority: "Normal",
            assignee: "tom", project: "tom-solo-ep", tags: vec!["recording"],
            body: "Solo acoustic piece. Martin D-28, Neumann KM184 stereo pair.", due: None,
        },
        TaskSeed {
            title: "Mix Tom EP rough", status: "Open", priority: "Normal",
            assignee: "marcus", project: "tom-solo-ep", tags: vec!["mixing"],
            body: "Minimal mixing — just balance, EQ, light compression.", due: None,
        },
        TaskSeed {
            title: "Design new landing page", status: "Open", priority: "Normal",
            assignee: "luna", project: "website-redesign", tags: vec!["design"],
            body: "Modern, clean design showcasing studio services.", due: None,
        },
        TaskSeed {
            title: "Update album cover art", status: "Open", priority: "Low",
            assignee: "riley", project: "demo-album", tags: vec!["design", "art"],
            body: "Warm analog tones, Montreal skyline at dusk, handwritten title.", due: None,
        },
        TaskSeed {
            title: "Submit Demo Album to distributors", status: "Open", priority: "Normal",
            assignee: "omar", project: "demo-album", tags: vec!["distribution"],
            body: "Upload to DistroKid. Schedule Friday release.",
            due: Some(chrono::NaiveDate::from_ymd_opt(2026, 8, 1).unwrap()),
        },
        TaskSeed {
            title: "Review Campus Jax vendor contracts", status: "Open", priority: "High",
            assignee: "omar", project: "campus-jax-live", tags: vec!["business", "contracts"],
            body: "Review all vendor agreements before event.", due: None,
        },
    ];

    for t in &tasks {
        let model = task::ActiveModel {
            id: Set(Uuid::new_v4()),
            sequence_id: Set(None),
            title: Set(t.title.to_string()),
            status: Set(t.status.to_string()),
            priority: Set(t.priority.to_string()),
            issue_type: Set(None),
            project: Set(Some(t.project.to_string())),
            assignee: Set(Some(t.assignee.to_string())),
            assignees: Set(serde_json::json!([]).into()),
            created_by: Set(None),
            due: Set(t.due),
            scheduled: Set(None),
            start: Set(None),
            completed_date: Set(None),
            tags: Set(serde_json::json!(t.tags).into()),
            time_estimate: Set(None),
            sort_order: Set(None),
            body: Set(Some(t.body.to_string())),
            file_path: Set(None),
            is_draft: Set(false),
            deleted_at: Set(None),
            created_at: Set(now),
            updated_at: Set(now),
        };
        model.insert(db).await.unwrap();
    }
    info!(count = tasks.len(), "Seeded tasks");
}

/// Seed mock users and organizations into better-auth.
async fn seed_auth_data(auth: &Arc<better_auth::BetterAuth<SeaOrmAuthAdapter>>) {
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
        MockUser { id: "user_cody",   username: "cody",   name: "Cody Wright",      email: "cody@fasttrackstudio.com",   role_title: "Founder & Producer",    department: "leadership",  account_status: "claimed" },
        MockUser { id: "user_amy",    username: "amy",    name: "Amy Chen",          email: "amy@fasttrackstudio.com",    role_title: "Creative Director",     department: "leadership",  account_status: "claimed" },
        MockUser { id: "user_carter", username: "carter", name: "Carter Whitlock",   email: "carter@fasttrackstudio.com", role_title: "Drummer & Live Sound",  department: "music",       account_status: "claimed" },
        MockUser { id: "user_tom",    username: "tom",    name: "Tom Brooks",        email: "tom@fasttrackstudio.com",    role_title: "Guitarist",             department: "music",       account_status: "claimed" },
        MockUser { id: "user_bri",    username: "bri",    name: "Bri Zacharias",     email: "bri@fasttrackstudio.com",    role_title: "Bass & Tour Manager",   department: "music",       account_status: "claimed" },
        MockUser { id: "user_kai",    username: "kai",    name: "Kai Nakamura",      email: "kai@fasttrackstudio.com",    role_title: "Backend Developer",     department: "engineering", account_status: "claimed" },
        MockUser { id: "user_luna",   username: "luna",   name: "Luna Zhang",        email: "luna@fasttrackstudio.com",   role_title: "Frontend Developer",    department: "engineering", account_status: "claimed" },
        MockUser { id: "user_elena",  username: "elena",  name: "Elena Vasquez",     email: "elena.vasquez@gmail.com",    role_title: "Keys & Arrangements",   department: "music",       account_status: "invited" },
        MockUser { id: "user_marcus", username: "marcus", name: "Marcus Cole",       email: "marcus@mixengineer.com",     role_title: "Mix Engineer",          department: "music",       account_status: "invited" },
        MockUser { id: "user_jade",   username: "jade",   name: "Jade Kim",          email: "jade@sterling-sound.com",    role_title: "Mastering Engineer",    department: "music",       account_status: "invited" },
        MockUser { id: "user_devon",  username: "devon",  name: "Devon Miles",       email: "devon.miles@outlook.com",    role_title: "Event Coordinator",     department: "events",      account_status: "invited" },
        MockUser { id: "user_alex",   username: "alex",   name: "Alex Petrov",       email: "alex.petrov@proton.me",      role_title: "DevOps Engineer",       department: "engineering", account_status: "invited" },
        MockUser { id: "user_noah",   username: "noah",   name: "Noah Park",         email: "noah@fasttrackstudio.com",   role_title: "Recording Engineer",    department: "music",       account_status: "placeholder" },
        MockUser { id: "user_priya",  username: "priya",  name: "Priya Sharma",      email: "priya@fasttrackstudio.com",  role_title: "Vocal Coach",           department: "music",       account_status: "placeholder" },
        MockUser { id: "user_riley",  username: "riley",  name: "Riley Foster",      email: "riley@fasttrackstudio.com",  role_title: "Marketing Lead",        department: "events",      account_status: "placeholder" },
        MockUser { id: "user_jordan", username: "jordan", name: "Jordan Lee",        email: "jordan@fasttrackstudio.com", role_title: "Lighting & Stage",      department: "events",      account_status: "placeholder" },
        MockUser { id: "user_sam",    username: "sam",    name: "Sam Rivera",        email: "sam@fasttrackstudio.com",    role_title: "Video & Content",       department: "events",      account_status: "placeholder" },
        MockUser { id: "user_mira",   username: "mira",   name: "Mira Okafor",       email: "mira@fasttrackstudio.com",   role_title: "QA Engineer",           department: "engineering", account_status: "placeholder" },
        MockUser { id: "user_omar",   username: "omar",   name: "Omar Hassan",       email: "omar@fasttrackstudio.com",   role_title: "Business Manager",      department: "business",    account_status: "placeholder" },
        MockUser { id: "user_tess",   username: "tess",   name: "Tess Moreno",       email: "tess@fasttrackstudio.com",   role_title: "A&R",                   department: "business",    account_status: "placeholder" },
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
            id: "org_personal", name: "Personal", slug: "personal",
            emoji: "user", hue: 0,
            description: "Private tasks and projects",
            owner: "cody", members: &["cody"],
        },
        MockOrg {
            id: "org_fta", name: "FastTrackAudio", slug: "fasttrackaudio",
            emoji: "music", hue: 210,
            description: "Music production company — albums, EPs, mixing clients",
            owner: "cody",
            members: &["cody", "amy", "carter", "tom", "bri", "elena", "marcus", "jade", "noah", "priya", "tess", "omar"],
        },
        MockOrg {
            id: "org_fts", name: "FastTrackStudio", slug: "fasttrackstudio",
            emoji: "code", hue: 270,
            description: "Software development — audio tools, plugins, infrastructure",
            owner: "cody",
            members: &["cody", "tom", "kai", "luna", "alex", "mira"],
        },
        MockOrg {
            id: "org_jf", name: "Just Friends", slug: "just-friends",
            emoji: "guitar", hue: 145,
            description: "Band project — recurring gigs, rehearsals, recordings",
            owner: "cody",
            members: &["cody", "amy", "carter", "tom", "bri", "elena"],
        },
        MockOrg {
            id: "org_tbm", name: "TomBrooksMusic", slug: "tombrooksmusic",
            emoji: "music2", hue: 35,
            description: "Tom Brooks' solo artist projects and collaborations",
            owner: "tom",
            members: &["tom", "cody", "marcus"],
        },
    ];

    for org in &orgs {
        let mut create = CreateOrganization::new(org.name, org.slug)
            .with_metadata(serde_json::json!({
                "emoji": org.emoji,
                "hue": org.hue,
                "description": org.description,
                "owner": org.owner,
            }));
        create.id = Some(org.id.to_string());
        if let Err(e) = db.create_organization(create).await {
            warn!(org = org.slug, error = %e, "Failed to seed org");
            continue;
        }

        for username in org.members {
            let user_id = format!("user_{username}");
            let role = if *username == org.owner { "owner" } else { "member" };
            let create_member = CreateMember::new(org.id, &user_id, role);
            if let Err(e) = db.create_member(create_member).await {
                warn!(org = org.slug, user = username, error = %e, "Failed to add member");
            }
        }
    }
    info!("Seeded {} organizations into better-auth", orgs.len());
}

// ── Helpers ──────────────────────────────────────────────────────────────────

/// Derive a stable oklch hue from a string (0-360).
fn hue_from_string(s: &str) -> u32 {
    let hash: u32 = s.bytes().fold(5381u32, |h, b| h.wrapping_mul(33).wrapping_add(b as u32));
    hash % 360
}

// ── Vox WebSocket handler ────────────────────────────────────────────────────

async fn vox_ws_handler(
    ws: WebSocketUpgrade,
    State(state): State<AppState>,
) -> impl IntoResponse {
    ws.on_upgrade(move |socket| handle_vox_connection(socket, state))
}

async fn handle_vox_connection(socket: WebSocket, state: AppState) {
    info!("Vox WebSocket client connected");

    use futures::{SinkExt, StreamExt};
    let (mut ws_tx, mut ws_rx) = socket.split();

    while let Some(Ok(msg)) = ws_rx.next().await {
        match msg {
            axum::extract::ws::Message::Binary(data) => {
                if let Ok(text) = std::str::from_utf8(&data) {
                    if let Ok(request) = serde_json::from_str::<serde_json::Value>(text) {
                        let method = request.get("method").and_then(|m| m.as_str()).unwrap_or("");
                        let response = dispatch_rpc(&state.db, &state.auth, method, &request).await;
                        let response_bytes = serde_json::to_vec(&response).unwrap_or_default();
                        let _ = ws_tx.send(axum::extract::ws::Message::Binary(response_bytes.into())).await;
                    }
                }
            }
            axum::extract::ws::Message::Text(text) => {
                if let Ok(request) = serde_json::from_str::<serde_json::Value>(&text) {
                    let method = request.get("method").and_then(|m| m.as_str()).unwrap_or("");
                    let response = dispatch_rpc(&state.db, &state.auth, method, &request).await;
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

/// Convert a DB project model to the API shape the web client expects.
fn project_to_api(p: &project::Model) -> serde_json::Value {
    let hue = hue_from_string(&p.title);
    let today = chrono::Local::now().date_naive();
    let is_overdue = p.due.map_or(false, |d| d < today) && p.status != "Completed" && p.status != "Archived";
    serde_json::json!({
        "title": p.title,
        "slug": p.slug,
        "status": p.status.to_lowercase(),
        "area": p.area,
        "due": p.due.map(|d| d.to_string()),
        "team": p.team,
        "is_overdue": is_overdue,
        "hue": hue,
        "project_type": p.project_type,
        "organization": p.organization,
        "description": p.description,
        "repo": null,
        "workflow": null,
        "workflow_stage": null,
        "parent": p.parent_slug,
        "references": p.references,
    })
}

/// Convert a DB task model to the API shape the web client expects.
fn task_to_api(t: &task::Model) -> serde_json::Value {
    serde_json::json!({
        "title": t.title,
        "status": t.status,
        "priority": t.priority,
        "assignee": t.assignee,
        "due": t.due.map(|d| d.to_string()),
        "projects": t.project.as_ref().map(|p| vec![p.clone()]).unwrap_or_default(),
        "tags": t.tags,
        "body": t.body,
    })
}

/// Dispatch a JSON-RPC-style request to the database.
async fn dispatch_rpc(
    db: &sea_orm::DatabaseConnection,
    auth: &Arc<better_auth::BetterAuth<SeaOrmAuthAdapter>>,
    method: &str,
    request: &serde_json::Value,
) -> serde_json::Value {
    info!(method = method, "RPC dispatch");
    match method {
        "list_tasks" => {
            match task::Entity::find().all(db).await {
                Ok(tasks) => serde_json::json!({ "result": tasks.iter().map(task_to_api).collect::<Vec<_>>(), "error": null }),
                Err(e) => serde_json::json!({ "error": e.to_string() }),
            }
        }
        "create_task" => {
            let params = request.get("params").cloned().unwrap_or_default();
            let title = params.get("title").and_then(|t| t.as_str()).unwrap_or("Untitled");
            let now = Utc::now();
            let model = task::ActiveModel {
                id: Set(Uuid::new_v4()),
                sequence_id: Set(None),
                title: Set(title.to_string()),
                status: Set(params.get("status").and_then(|s| s.as_str()).unwrap_or("Open").to_string()),
                priority: Set(params.get("priority").and_then(|p| p.as_str()).unwrap_or("Normal").to_string()),
                issue_type: Set(None),
                project: Set(params.get("project").and_then(|p| p.as_str()).map(|s| s.to_string())),
                assignee: Set(params.get("assignee").and_then(|a| a.as_str()).map(|s| s.to_string())),
                assignees: Set(serde_json::json!([]).into()),
                created_by: Set(None),
                due: Set(params.get("due").and_then(|d| d.as_str()).and_then(|d| chrono::NaiveDate::parse_from_str(d, "%Y-%m-%d").ok())),
                scheduled: Set(None),
                start: Set(None),
                completed_date: Set(None),
                tags: Set(params.get("tags").cloned().unwrap_or(serde_json::json!([])).into()),
                time_estimate: Set(None),
                sort_order: Set(None),
                body: Set(params.get("body").and_then(|b| b.as_str()).map(|s| s.to_string())),
                file_path: Set(None),
                is_draft: Set(false),
                deleted_at: Set(None),
                created_at: Set(now),
                updated_at: Set(now),
            };
            match model.insert(db).await {
                Ok(created) => serde_json::json!({ "result": task_to_api(&created), "error": null }),
                Err(e) => serde_json::json!({ "error": e.to_string() }),
            }
        }
        "task_detail" => {
            if let Some(title) = request.get("params").and_then(|p| p.get("title")).and_then(|t| t.as_str()) {
                match task::Entity::find().filter(task::Column::Title.eq(title)).one(db).await {
                    Ok(Some(t)) => serde_json::json!({
                        "result": {
                            "task": task_to_api(&t),
                            "subtasks": [],
                            "subtask_progress": null,
                            "comments": [],
                        },
                        "error": null
                    }),
                    Ok(None) => serde_json::json!({ "error": format!("task not found: {title}") }),
                    Err(e) => serde_json::json!({ "error": e.to_string() }),
                }
            } else {
                serde_json::json!({ "error": "missing params.title" })
            }
        }
        "update_task" => {
            let params = request.get("params").cloned().unwrap_or_default();
            if let Some(title) = params.get("title").and_then(|t| t.as_str()) {
                match task::Entity::find().filter(task::Column::Title.eq(title)).one(db).await {
                    Ok(Some(existing)) => {
                        let mut active: task::ActiveModel = existing.into();
                        if let Some(status) = params.get("status").and_then(|s| s.as_str()) {
                            active.status = Set(status.to_string());
                        }
                        if let Some(priority) = params.get("priority").and_then(|p| p.as_str()) {
                            active.priority = Set(priority.to_string());
                        }
                        if let Some(assignee) = params.get("assignee").and_then(|a| a.as_str()) {
                            active.assignee = Set(if assignee.is_empty() { None } else { Some(assignee.to_string()) });
                        }
                        if let Some(due) = params.get("due").and_then(|d| d.as_str()) {
                            active.due = Set(chrono::NaiveDate::parse_from_str(due, "%Y-%m-%d").ok());
                        }
                        if let Some(body) = params.get("body").and_then(|b| b.as_str()) {
                            active.body = Set(Some(body.to_string()));
                        }
                        active.updated_at = Set(Utc::now());
                        match active.update(db).await {
                            Ok(updated) => serde_json::json!({ "result": task_to_api(&updated), "error": null }),
                            Err(e) => serde_json::json!({ "error": e.to_string() }),
                        }
                    }
                    Ok(None) => serde_json::json!({ "error": format!("task not found: {title}") }),
                    Err(e) => serde_json::json!({ "error": e.to_string() }),
                }
            } else {
                serde_json::json!({ "error": "missing params.title" })
            }
        }
        "complete_task" => {
            if let Some(title) = request.get("params").and_then(|p| p.get("title")).and_then(|t| t.as_str()) {
                match task::Entity::find().filter(task::Column::Title.eq(title)).one(db).await {
                    Ok(Some(existing)) => {
                        let mut active: task::ActiveModel = existing.into();
                        active.status = Set("Done".to_string());
                        active.completed_date = Set(Some(chrono::Local::now().date_naive()));
                        active.updated_at = Set(Utc::now());
                        match active.update(db).await {
                            Ok(updated) => serde_json::json!({ "result": task_to_api(&updated), "error": null }),
                            Err(e) => serde_json::json!({ "error": e.to_string() }),
                        }
                    }
                    Ok(None) => serde_json::json!({ "error": format!("task not found: {title}") }),
                    Err(e) => serde_json::json!({ "error": e.to_string() }),
                }
            } else {
                serde_json::json!({ "error": "missing params.title" })
            }
        }
        "search_tasks" => {
            if let Some(query) = request.get("params").and_then(|p| p.get("query")).and_then(|q| q.as_str()) {
                match task::Entity::find().filter(task::Column::Title.contains(query)).all(db).await {
                    Ok(tasks) => serde_json::json!({ "result": tasks.iter().map(task_to_api).collect::<Vec<_>>(), "error": null }),
                    Err(e) => serde_json::json!({ "error": e.to_string() }),
                }
            } else {
                serde_json::json!({ "error": "missing params.query" })
            }
        }
        "list_projects" => {
            match project::Entity::find().all(db).await {
                Ok(projects) => serde_json::json!({ "result": projects.iter().map(project_to_api).collect::<Vec<_>>(), "error": null }),
                Err(e) => serde_json::json!({ "error": e.to_string() }),
            }
        }
        "list_active_projects" => {
            match project::Entity::find()
                .filter(project::Column::Status.eq("Active"))
                .filter(project::Column::ParentSlug.is_null())
                .all(db).await
            {
                Ok(projects) => serde_json::json!({ "result": projects.iter().map(project_to_api).collect::<Vec<_>>(), "error": null }),
                Err(e) => serde_json::json!({ "error": e.to_string() }),
            }
        }
        "project_detail" => {
            if let Some(title) = request.get("params").and_then(|p| p.get("title")).and_then(|t| t.as_str()) {
                let slug_val = slug::slugify(title);
                let project_result = project::Entity::find()
                    .filter(
                        sea_orm::Condition::any()
                            .add(project::Column::Title.eq(title))
                            .add(project::Column::Slug.eq(&slug_val))
                    )
                    .one(db).await;

                match project_result {
                    Ok(Some(p)) => {
                        // Get tasks for this project
                        let project_tasks = task::Entity::find()
                            .filter(task::Column::Project.eq(&p.slug))
                            .all(db).await.unwrap_or_default();

                        let total = project_tasks.len();
                        let done = project_tasks.iter().filter(|t| t.status == "Done").count();

                        let by_status = |s: &str| -> Vec<serde_json::Value> {
                            project_tasks.iter()
                                .filter(|t| t.status == s)
                                .map(task_to_api)
                                .collect()
                        };

                        // Find child projects
                        let children = project::Entity::find()
                            .filter(project::Column::ParentSlug.eq(&p.slug))
                            .all(db).await.unwrap_or_default();

                        let children_json: Vec<serde_json::Value> = children.iter().map(|child| {
                            serde_json::json!({
                                "project": project_to_api(child),
                                "total_tasks": 0,
                                "done_tasks": 0,
                            })
                        }).collect();

                        serde_json::json!({
                            "result": {
                                "project": project_to_api(&p),
                                "total_tasks": total,
                                "done_tasks": done,
                                "tasks_by_status": {
                                    "open": by_status("Open"),
                                    "in_progress": by_status("InProgress"),
                                    "planned": by_status("Planned"),
                                    "on_hold": by_status("OnHold"),
                                    "done": by_status("Done"),
                                },
                                "all_tasks": project_tasks.iter().map(task_to_api).collect::<Vec<_>>(),
                                "workflow": {},
                                "children": children_json,
                                "referenced_projects": [],
                            },
                            "error": null
                        })
                    }
                    Ok(None) => serde_json::json!({ "error": format!("project not found: {title}") }),
                    Err(e) => serde_json::json!({ "error": e.to_string() }),
                }
            } else {
                serde_json::json!({ "error": "missing params.title" })
            }
        }
        "project_ancestry" => {
            if let Some(title) = request.get("params").and_then(|p| p.get("title")).and_then(|t| t.as_str()) {
                let slug_val = slug::slugify(title);
                let target = project::Entity::find()
                    .filter(
                        sea_orm::Condition::any()
                            .add(project::Column::Title.eq(title))
                            .add(project::Column::Slug.eq(&slug_val))
                    )
                    .one(db).await.ok().flatten();

                let Some(target) = target else {
                    return serde_json::json!({ "result": [], "error": null });
                };

                // Walk up the parent chain
                let mut chain = vec![target.clone()];
                let mut current = target;
                while let Some(ref parent_slug) = current.parent_slug {
                    if let Ok(Some(parent)) = project::Entity::find()
                        .filter(project::Column::Slug.eq(parent_slug))
                        .one(db).await
                    {
                        chain.push(parent.clone());
                        current = parent;
                    } else {
                        break;
                    }
                }
                chain.reverse(); // root first

                // Build columns: each ancestor's children
                let mut columns: Vec<serde_json::Value> = Vec::new();
                for proj in &chain {
                    let children = project::Entity::find()
                        .filter(project::Column::ParentSlug.eq(&proj.slug))
                        .all(db).await.unwrap_or_default();

                    let children_json: Vec<serde_json::Value> = children.iter().map(|child| {
                        serde_json::json!({
                            "title": child.title,
                            "slug": child.slug,
                            "display_name": child.title,
                            "hue": hue_from_string(&child.title),
                        })
                    }).collect();

                    columns.push(serde_json::json!({
                        "title": proj.title,
                        "slug": proj.slug,
                        "hue": hue_from_string(&proj.title),
                        "children": children_json,
                    }));
                }

                serde_json::json!({ "result": columns, "error": null })
            } else {
                serde_json::json!({ "error": "missing params.title" })
            }
        }
        "command_center" => {
            let projects = project::Entity::find()
                .filter(project::Column::Status.eq("Active"))
                .filter(project::Column::ParentSlug.is_null())
                .all(db).await.unwrap_or_default();

            let all_tasks = task::Entity::find().all(db).await.unwrap_or_default();
            let today = chrono::Local::now().date_naive();

            let items: Vec<serde_json::Value> = projects.iter().map(|p| {
                let project_tasks: Vec<_> = all_tasks.iter()
                    .filter(|t| t.project.as_deref() == Some(&p.slug))
                    .collect();
                let open_tasks: Vec<_> = project_tasks.iter()
                    .filter(|t| t.status != "Done" && t.status != "Cancelled")
                    .collect();
                let total = project_tasks.len();
                let done = project_tasks.iter().filter(|t| t.status == "Done").count();
                let urgent_count = open_tasks.iter().filter(|t| t.priority == "Urgent").count();
                let overdue_count = open_tasks.iter().filter(|t| {
                    t.due.map_or(false, |d| d < today)
                }).count();
                let in_progress = open_tasks.iter().filter(|t| t.status == "InProgress").count();

                let next = open_tasks.iter()
                    .min_by_key(|t| {
                        let prio = match t.priority.as_str() {
                            "Urgent" => 0,
                            "High" => 1,
                            "Normal" => 2,
                            "Low" => 3,
                            _ => 4,
                        };
                        let due_sort = t.due.map_or(9999_99_99i32, |d| {
                            use chrono::Datelike;
                            d.year() * 10000 + d.month() as i32 * 100 + d.day() as i32
                        });
                        (prio, due_sort)
                    })
                    .map(|t| task_to_api(*t));

                let notifications = urgent_count + overdue_count;
                serde_json::json!({
                    "project": project_to_api(p),
                    "total_tasks": total,
                    "done_tasks": done,
                    "open_tasks": open_tasks.len(),
                    "in_progress": in_progress,
                    "urgent_count": urgent_count,
                    "overdue_count": overdue_count,
                    "notifications": notifications,
                    "next_task": next,
                })
            }).collect();

            serde_json::json!({ "result": items, "error": null })
        }
        "tasks_for_user" => {
            if let Some(username) = request.get("params").and_then(|p| p.get("username")).and_then(|u| u.as_str()) {
                match task::Entity::find().filter(task::Column::Assignee.eq(username)).all(db).await {
                    Ok(tasks) => serde_json::json!({ "result": tasks.iter().map(task_to_api).collect::<Vec<_>>(), "error": null }),
                    Err(e) => serde_json::json!({ "error": e.to_string() }),
                }
            } else {
                serde_json::json!({ "error": "missing params.username" })
            }
        }
        "tasks_for_project" => {
            if let Some(proj) = request.get("params").and_then(|p| p.get("project")).and_then(|p| p.as_str()) {
                match task::Entity::find().filter(task::Column::Project.eq(proj)).all(db).await {
                    Ok(tasks) => serde_json::json!({ "result": tasks.iter().map(task_to_api).collect::<Vec<_>>(), "error": null }),
                    Err(e) => serde_json::json!({ "error": e.to_string() }),
                }
            } else {
                serde_json::json!({ "error": "missing params.project" })
            }
        }
        "list_notifications" => {
            let all_tasks = task::Entity::find().all(db).await.unwrap_or_default();
            let today = chrono::Local::now().date_naive();
            let mut notifs: Vec<serde_json::Value> = Vec::new();

            for t in all_tasks.iter() {
                if t.status != "Done" {
                    if let Some(due) = t.due {
                        if due < today {
                            notifs.push(serde_json::json!({
                                "id": format!("overdue-{}", t.title),
                                "message": format!("{} is overdue (due {})", t.title, due),
                                "actor": t.assignee,
                                "time_ago": format!("{} days ago", (today - due).num_days()),
                                "read": false,
                                "kind": "overdue",
                            }));
                        } else if (due - today).num_days() <= 2 {
                            notifs.push(serde_json::json!({
                                "id": format!("due-soon-{}", t.title),
                                "message": format!("{} is due {}", t.title, if due == today { "today".to_string() } else { format!("in {} day(s)", (due - today).num_days()) }),
                                "actor": t.assignee,
                                "time_ago": "upcoming",
                                "read": false,
                                "kind": "due_reminder",
                            }));
                        }
                    }
                    if t.priority == "Urgent" {
                        notifs.push(serde_json::json!({
                            "id": format!("urgent-{}", t.title),
                            "message": format!("{} is marked urgent", t.title),
                            "actor": t.assignee,
                            "time_ago": "active",
                            "read": true,
                            "kind": "urgent",
                        }));
                    }
                }
            }
            notifs.truncate(20);
            serde_json::json!({ "result": notifs, "error": null })
        }
        "activity" => {
            let limit = request.get("params")
                .and_then(|p| p.get("limit"))
                .and_then(|l| l.as_u64())
                .unwrap_or(50);
            match activity::Entity::find()
                .order_by_desc(activity::Column::CreatedAt)
                .paginate(db, limit)
                .fetch_page(0).await
            {
                Ok(activities) => {
                    let items: Vec<serde_json::Value> = activities.iter().map(|a| {
                        serde_json::json!({
                            "entity_type": a.entity_type,
                            "entity_id": a.entity_id.to_string(),
                            "field": a.field,
                            "old_value": a.old_value,
                            "new_value": a.new_value,
                            "changed_by": a.actor,
                            "changed_at": a.created_at.to_rfc3339(),
                        })
                    }).collect();
                    serde_json::json!({ "result": items, "error": null })
                }
                Err(e) => serde_json::json!({ "error": e.to_string() }),
            }
        }

        // ── Auth-related RPC methods ────────────────────────────────
        "list_orgs" => {
            let auth_db = auth.database();
            let users_result = auth_db.list_users(better_auth_core::types::ListUsersParams::default()).await;
            let orgs = if let Ok((users, _)) = users_result {
                if let Some(user) = users.first() {
                    auth_db.list_user_organizations(&user.id).await.unwrap_or_default()
                } else {
                    vec![]
                }
            } else {
                vec![]
            };
            let mut items: Vec<serde_json::Value> = Vec::new();
            for org in &orgs {
                let meta = org.metadata.as_ref();
                // Fetch members for this org to get usernames
                let members = auth_db.list_organization_members(&org.id).await.unwrap_or_default();
                // Resolve member user_ids to usernames
                let mut member_names: Vec<String> = Vec::new();
                for member in &members {
                    use better_auth_core::entity::AuthMember;
                    if let Ok(Some(user)) = auth_db.get_user_by_id(member.user_id()).await {
                        use better_auth_core::entity::AuthUser;
                        member_names.push(user.username().unwrap_or("unknown").to_string());
                    }
                }
                items.push(serde_json::json!({
                    "slug": org.slug,
                    "name": org.name,
                    "emoji": meta.and_then(|m| m.get("emoji")).and_then(|e| e.as_str()).unwrap_or(""),
                    "hue": meta.and_then(|m| m.get("hue")).and_then(|h| h.as_u64()).unwrap_or(0),
                    "description": meta.and_then(|m| m.get("description")).and_then(|d| d.as_str()).unwrap_or(""),
                    "owner": meta.and_then(|m| m.get("owner")).and_then(|o| o.as_str()).unwrap_or(""),
                    "members": member_names,
                }));
            }
            serde_json::json!({ "result": items, "error": null })
        }
        "list_team" => {
            let auth_db = auth.database();
            let users = auth_db.list_users(better_auth_core::types::ListUsersParams::default())
                .await
                .map(|(users, _)| users)
                .unwrap_or_default();
            let items: Vec<serde_json::Value> = users.iter().map(|u| {
                let meta = &u.metadata;
                serde_json::json!({
                    "username": u.username,
                    "name": u.name,
                    "role": meta.get("role_title").and_then(|v| v.as_str()).unwrap_or(""),
                    "department": meta.get("department").and_then(|v| v.as_str()).unwrap_or(""),
                    "email": u.email,
                    "account_status": meta.get("account_status").and_then(|v| v.as_str()).unwrap_or("claimed"),
                })
            }).collect();
            serde_json::json!({ "result": items, "error": null })
        }

        // ── Auth RPC methods ────────────────────────────────────────
        "auth.sign_up" => {
            let params = request.get("params").cloned().unwrap_or_default();
            let email = params.get("email").and_then(|v| v.as_str()).unwrap_or("");
            let password = params.get("password").and_then(|v| v.as_str()).unwrap_or("");
            let name = params.get("name").and_then(|v| v.as_str()).unwrap_or("");
            if email.is_empty() || password.is_empty() || name.is_empty() {
                return serde_json::json!({ "error": "missing email, password, or name" });
            }
            let body = serde_json::json!({ "email": email, "password": password, "name": name });
            let auth_req = better_auth_core::types::AuthRequest::from_parts(
                better_auth_core::types::HttpMethod::Post,
                "/sign-up/email".to_string(),
                std::collections::HashMap::from([("content-type".to_string(), "application/json".to_string())]),
                Some(serde_json::to_vec(&body).unwrap()),
                std::collections::HashMap::new(),
            );
            match auth.handle_request(auth_req).await {
                Ok(resp) => {
                    let body_str = String::from_utf8_lossy(&resp.body);
                    match serde_json::from_str::<serde_json::Value>(&body_str) {
                        Ok(v) => serde_json::json!({ "result": v, "error": null }),
                        Err(_) => serde_json::json!({ "result": body_str.to_string(), "error": null }),
                    }
                }
                Err(e) => serde_json::json!({ "error": e.to_string() }),
            }
        }
        "auth.sign_in" => {
            let params = request.get("params").cloned().unwrap_or_default();
            let email = params.get("email").and_then(|v| v.as_str()).unwrap_or("");
            let password = params.get("password").and_then(|v| v.as_str()).unwrap_or("");
            if email.is_empty() || password.is_empty() {
                return serde_json::json!({ "error": "missing email or password" });
            }
            let body = serde_json::json!({ "email": email, "password": password });
            let auth_req = better_auth_core::types::AuthRequest::from_parts(
                better_auth_core::types::HttpMethod::Post,
                "/sign-in/email".to_string(),
                std::collections::HashMap::from([("content-type".to_string(), "application/json".to_string())]),
                Some(serde_json::to_vec(&body).unwrap()),
                std::collections::HashMap::new(),
            );
            match auth.handle_request(auth_req).await {
                Ok(resp) => {
                    let body_str = String::from_utf8_lossy(&resp.body);
                    match serde_json::from_str::<serde_json::Value>(&body_str) {
                        Ok(v) => serde_json::json!({ "result": v, "error": null }),
                        Err(_) => serde_json::json!({ "result": body_str.to_string(), "error": null }),
                    }
                }
                Err(e) => serde_json::json!({ "error": e.to_string() }),
            }
        }
        "auth.get_session" => {
            let params = request.get("params").cloned().unwrap_or_default();
            let token = params.get("token").and_then(|v| v.as_str()).unwrap_or("");
            if token.is_empty() {
                return serde_json::json!({ "error": "missing token" });
            }
            let mut headers = std::collections::HashMap::new();
            headers.insert("authorization".to_string(), format!("Bearer {token}"));
            let auth_req = better_auth_core::types::AuthRequest::from_parts(
                better_auth_core::types::HttpMethod::Get,
                "/get-session".to_string(),
                headers,
                None,
                std::collections::HashMap::new(),
            );
            match auth.handle_request(auth_req).await {
                Ok(resp) => {
                    let body_str = String::from_utf8_lossy(&resp.body);
                    match serde_json::from_str::<serde_json::Value>(&body_str) {
                        Ok(v) => serde_json::json!({ "result": v, "error": null }),
                        Err(_) => serde_json::json!({ "result": body_str.to_string(), "error": null }),
                    }
                }
                Err(e) => serde_json::json!({ "error": e.to_string() }),
            }
        }
        "auth.sign_out" => {
            let params = request.get("params").cloned().unwrap_or_default();
            let token = params.get("token").and_then(|v| v.as_str()).unwrap_or("");
            if token.is_empty() {
                return serde_json::json!({ "error": "missing token" });
            }
            let mut headers = std::collections::HashMap::new();
            headers.insert("authorization".to_string(), format!("Bearer {token}"));
            let auth_req = better_auth_core::types::AuthRequest::from_parts(
                better_auth_core::types::HttpMethod::Post,
                "/sign-out".to_string(),
                headers,
                None,
                std::collections::HashMap::new(),
            );
            match auth.handle_request(auth_req).await {
                Ok(_) => serde_json::json!({ "result": true, "error": null }),
                Err(e) => serde_json::json!({ "error": e.to_string() }),
            }
        }
        "auth.list_sessions" => {
            let params = request.get("params").cloned().unwrap_or_default();
            let token = params.get("token").and_then(|v| v.as_str()).unwrap_or("");
            if token.is_empty() {
                return serde_json::json!({ "error": "missing token" });
            }
            let mut headers = std::collections::HashMap::new();
            headers.insert("authorization".to_string(), format!("Bearer {token}"));
            let auth_req = better_auth_core::types::AuthRequest::from_parts(
                better_auth_core::types::HttpMethod::Get,
                "/list-sessions".to_string(),
                headers,
                None,
                std::collections::HashMap::new(),
            );
            match auth.handle_request(auth_req).await {
                Ok(resp) => {
                    let body_str = String::from_utf8_lossy(&resp.body);
                    match serde_json::from_str::<serde_json::Value>(&body_str) {
                        Ok(v) => serde_json::json!({ "result": v, "error": null }),
                        Err(_) => serde_json::json!({ "result": body_str.to_string(), "error": null }),
                    }
                }
                Err(e) => serde_json::json!({ "error": e.to_string() }),
            }
        }

        "trigger_sync" => {
            // No-op — sync is not applicable with in-memory DB
            serde_json::json!({ "result": "ok", "error": null })
        }

        _ => {
            warn!(method = method, "Unknown RPC method");
            serde_json::json!({ "error": format!("unknown method: {method}") })
        }
    }
}

// ── REST Handlers ───────────────────────────────────────────────────────────

async fn health() -> &'static str {
    "ok"
}

async fn server_info(State(state): State<AppState>) -> impl IntoResponse {
    Json(state.info)
}

#[derive(Deserialize)]
struct ProjectFilter {
    area: Option<String>,
    status: Option<String>,
    project_type: Option<String>,
}

async fn list_projects_handler(
    State(state): State<AppState>,
    AxumQuery(filter): AxumQuery<ProjectFilter>,
) -> impl IntoResponse {
    let mut query = project::Entity::find();
    if let Some(ref area) = filter.area {
        query = query.filter(project::Column::Area.eq(area));
    }
    if let Some(ref status) = filter.status {
        query = query.filter(project::Column::Status.eq(status));
    }
    if let Some(ref pt) = filter.project_type {
        query = query.filter(project::Column::ProjectType.eq(pt));
    }
    let projects = query.all(&state.db).await.unwrap_or_default();
    Json(projects.iter().map(project_to_api).collect::<Vec<_>>())
}

async fn list_active_projects_handler(State(state): State<AppState>) -> impl IntoResponse {
    let projects = project::Entity::find()
        .filter(project::Column::Status.eq("Active"))
        .filter(project::Column::DeletedAt.is_null())
        .filter(project::Column::ArchivedAt.is_null())
        .all(&state.db).await.unwrap_or_default();
    Json(projects.iter().map(project_to_api).collect::<Vec<_>>())
}

async fn list_tasks_handler(State(state): State<AppState>) -> impl IntoResponse {
    let tasks = task::Entity::find().all(&state.db).await.unwrap_or_default();
    Json(tasks.iter().map(task_to_api).collect::<Vec<_>>())
}

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
    let now = Utc::now();
    let model = task::ActiveModel {
        id: Set(Uuid::new_v4()),
        sequence_id: Set(None),
        title: Set(req.title),
        status: Set(req.status.unwrap_or_else(|| "Open".to_string())),
        priority: Set(req.priority.unwrap_or_else(|| "Normal".to_string())),
        issue_type: Set(None),
        project: Set(req.project),
        assignee: Set(req.assignee),
        assignees: Set(serde_json::json!([]).into()),
        created_by: Set(None),
        due: Set(req.due.and_then(|d| chrono::NaiveDate::parse_from_str(&d, "%Y-%m-%d").ok())),
        scheduled: Set(None),
        start: Set(None),
        completed_date: Set(None),
        tags: Set(serde_json::json!(req.tags).into()),
        time_estimate: Set(None),
        sort_order: Set(None),
        body: Set(if req.body.is_empty() { None } else { Some(req.body) }),
        file_path: Set(None),
        is_draft: Set(false),
        deleted_at: Set(None),
        created_at: Set(now),
        updated_at: Set(now),
    };
    match model.insert(&state.db).await {
        Ok(created) => Json(serde_json::json!({"task": task_to_api(&created)})),
        Err(e) => Json(serde_json::json!({"error": e.to_string()})),
    }
}

async fn complete_task_api(
    State(state): State<AppState>,
    axum::extract::Path(title): axum::extract::Path<String>,
) -> impl IntoResponse {
    match task::Entity::find().filter(task::Column::Title.eq(&title)).one(&state.db).await {
        Ok(Some(existing)) => {
            let mut active: task::ActiveModel = existing.into();
            active.status = Set("Done".to_string());
            active.completed_date = Set(Some(chrono::Local::now().date_naive()));
            active.updated_at = Set(Utc::now());
            match active.update(&state.db).await {
                Ok(updated) => Json(serde_json::json!({"task": task_to_api(&updated)})),
                Err(e) => Json(serde_json::json!({"error": e.to_string()})),
            }
        }
        Ok(None) => Json(serde_json::json!({"error": "not found"})),
        Err(e) => Json(serde_json::json!({"error": e.to_string()})),
    }
}

async fn get_task(
    State(state): State<AppState>,
    axum::extract::Path(title): axum::extract::Path<String>,
) -> impl IntoResponse {
    match task::Entity::find().filter(task::Column::Title.eq(&title)).one(&state.db).await {
        Ok(Some(t)) => Json(serde_json::json!({"task": task_to_api(&t)})),
        Ok(None) => Json(serde_json::json!({"error": "not found"})),
        Err(e) => Json(serde_json::json!({"error": e.to_string()})),
    }
}

async fn tasks_by_user(
    State(state): State<AppState>,
    axum::extract::Path(username): axum::extract::Path<String>,
) -> impl IntoResponse {
    let tasks = task::Entity::find()
        .filter(task::Column::Assignee.eq(&username))
        .all(&state.db).await.unwrap_or_default();
    Json(serde_json::json!({"tasks": tasks.iter().map(task_to_api).collect::<Vec<_>>()}))
}

#[derive(Deserialize)]
struct ActivityFilter {
    limit: Option<u64>,
    #[allow(dead_code)]
    entity_type: Option<String>,
    #[allow(dead_code)]
    entity_id: Option<String>,
}

async fn activity_feed(
    State(state): State<AppState>,
    AxumQuery(filter): AxumQuery<ActivityFilter>,
) -> impl IntoResponse {
    let limit = filter.limit.unwrap_or(50);
    let activities = activity::Entity::find()
        .order_by_desc(activity::Column::CreatedAt)
        .paginate(&state.db, limit)
        .fetch_page(0).await.unwrap_or_default();
    let items: Vec<serde_json::Value> = activities.iter().map(|a| {
        serde_json::json!({
            "entity_type": a.entity_type,
            "entity_id": a.entity_id.to_string(),
            "field": a.field,
            "old_value": a.old_value,
            "new_value": a.new_value,
            "changed_by": a.actor,
            "changed_at": a.created_at.to_rfc3339(),
        })
    }).collect();
    Json(serde_json::json!({"changes": items}))
}
