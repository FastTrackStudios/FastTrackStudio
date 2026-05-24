//! `task-server` — minimal vox endpoint.
//!
//! Surface after the knowledge + project-CRDT rip:
//! - `/health` — liveness probe.
//! - `/vox` — architect/vox WebSocket endpoint hosting three
//!   services: `AuthService` (architect-auth),
//!   `AttachmentService` (signed upload/download), and
//!   `VaultSyncRpc` (file replication backed by
//!   `vault::Backend`).
//! - `/blobs/*` — signed-URL endpoint for attachment uploads
//!   and downloads, mounted via `attachments::routes`.
//!
//! The previous CRDT machinery (`DocRegistry`, `OpenDoc`,
//! `WorkspaceSyncImpl`, `task-db` / `crdt-seaorm` persistence,
//! `*RepoLoro` dispatchers, capability / share-link / claim
//! services) was ripped along with the `project-proto` /
//! `project-crdt` crates. CRDT now lives only at the per-file
//! editor layer (future); vault is the sole storage path.

pub mod attachments;
pub mod capability;
pub mod server_mgmt;

use std::path::PathBuf;
use std::sync::Arc;

use architect_auth::{
    ArchitectAuth, AuthServiceDispatcher,
    db::{AuthSeaOrmStorage, Migrator as AuthMigrator},
    transport::vox::{AuthServerMiddleware, AuthVoxService},
};
use axum::Router;
use axum::extract::State;
use axum::extract::ws::WebSocketUpgrade;
use axum::response::IntoResponse;
use axum::routing::get;
use sea_orm::Database;
use sea_orm_migration::MigratorTrait;

use crate::capability::ServerKeypair;

#[derive(Clone)]
pub struct AuthState {
    pub auth: ArchitectAuth<AuthSeaOrmStorage>,
}

impl AuthState {
    pub async fn open(db_url: &str, secret: &str) -> eyre::Result<Self> {
        let db = Database::connect(db_url)
            .await
            .map_err(|e| eyre::eyre!("connect auth db `{db_url}`: {e}"))?;
        AuthMigrator::up(&db, None)
            .await
            .map_err(|e| eyre::eyre!("auth migrations: {e}"))?;
        let storage = AuthSeaOrmStorage::new(db);
        let auth = ArchitectAuth::builder()
            .secret(secret)
            .storage(storage)
            .build()
            .map_err(|e| eyre::eyre!("build ArchitectAuth: {e}"))?;
        Ok(Self { auth })
    }
}

/// Per-org server state. One instance per org dir scanned
/// at boot. Holds every backend the vox dispatcher mounts
/// for that org (auth, attachments, vault, wiki, agent
/// tasks, timer, finance).
///
/// Shared across orgs (the blob signing keypair, the data
/// root) lives on the parent [`AppState`].
#[derive(Clone)]
pub struct OrgAppState {
    /// Org's slug — matches the `<data_root>/orgs/<slug>/`
    /// dir and the URL prefix the vox handler routes from.
    pub slug: String,
    /// Org-scoped architect-auth instance opened against
    /// this org's `auth.sqlite`.
    pub auth: AuthState,
    pub attachments: Arc<attachments::AttachmentServiceImpl>,
    /// File-replication backend rooted at this org's
    /// `vault/` dir.
    pub vault_sync: vault::Backend,
    /// Wiki feature backend rooted at this org's `vault/`.
    pub wiki: wiki_live::WikiBackend,
    /// Project list / get backend — walks `vault/Projects/*.md`.
    pub projects: project::ProjectBackend,
    /// Goal list / get backend — walks `vault/Goals/**/*.md`.
    pub goals: goal::GoalBackend,
    /// Milestone backend — project-scoped checkpoints, walks
    /// `vault/Projects/<slug>/milestones/*.md`.
    pub milestones: milestone::MilestoneBackend,
    /// Task backend — walks every `type: task` page in the
    /// vault.
    pub tasks: task::TaskBackend,
    /// Locations backend — `type: location` pages.
    pub locations: locations::Store,
    /// Cookbook (cooklang recipes under `Wiki/Cookbook/`).
    pub cookbook: cookbook::Store,
    /// Mealplan — scheduled meals + their fulfillment math.
    pub mealplan: mealplan::Store,
    /// Pantry — stocked ingredients + barcode lookup.
    pub pantry: pantry::Store,
    /// Body metrics — weight / body-fat / measurements log.
    pub body: body::Store,
    /// Exercise library — movement definitions referenced by
    /// routines + sessions.
    pub exercises: exercises::Store,
    /// Workout routines + sessions (planned + completed lifts).
    pub workouts: workouts::Store,
    /// Food intake — per-day calorie + macro log.
    pub intake: intake::Store,
    pub agent_tasks: agent_tasks::Store,
    pub agent_dispatch_vault_root: PathBuf,
    pub timer: timer::Store,
    pub finance_conn: sea_orm::DatabaseConnection,
}

/// Top-level server state. Scans `<data_root>/orgs/` at
/// boot and builds one [`OrgAppState`] per discovered org.
/// The vox + blob handlers dispatch by slug (URL path).
///
/// The Ed25519 blob signing keypair is shared across orgs —
/// the server-side identity is one keypair per process.
#[derive(Clone)]
pub struct AppState {
    /// Ed25519 keypair used to sign blob URLs. Loaded from
    /// `<data_root>/server-key.ed25519`, generated on first
    /// boot. Tests use `ServerKeypair::generate_ephemeral()`.
    pub keypair: ServerKeypair,
    /// Slug → per-org state. Built by scanning
    /// `<data_root>/orgs/` at boot and mutated at runtime by
    /// the server-management `create_org` RPC. `RwLock` so
    /// reads on the request hot path stay parallel; writes
    /// happen only when an admin scaffolds a new org.
    pub orgs: Arc<std::sync::RwLock<std::collections::HashMap<String, OrgAppState>>>,
    /// Source data root. Held for `.well-known/task-server.json`
    /// discovery, manifest re-scans, and the keypair path.
    pub data_root: org_proto::DataRoot,
}

impl AppState {
    /// Look up an org by slug. Convenience for routes that
    /// have extracted the slug from the URL path. Clones the
    /// matched [`OrgAppState`] (`Clone` is cheap — all fields
    /// are `Arc`/`Database` handles).
    #[must_use]
    pub fn org(&self, slug: &str) -> Option<OrgAppState> {
        self.orgs.read().ok()?.get(slug).cloned()
    }

    /// Slugs of every hosted org, sorted for deterministic
    /// `.well-known` output.
    #[must_use]
    pub fn org_slugs(&self) -> Vec<String> {
        let guard = match self.orgs.read() {
            Ok(g) => g,
            Err(_) => return Vec::new(),
        };
        let mut slugs: Vec<String> = guard.keys().cloned().collect();
        slugs.sort_unstable();
        slugs
    }

    /// True when the server has no hosted orgs. Used by the
    /// server-management RPC to decide whether to accept an
    /// unauthenticated bootstrap `create_org`.
    #[must_use]
    pub fn is_bootstrap(&self) -> bool {
        self.orgs.read().is_ok_and(|g| g.is_empty())
    }

    /// Slug of the home org, if exactly one is hosted. Used to
    /// gate `create_org` after bootstrap — only home-org users
    /// can mint new federated orgs.
    #[must_use]
    pub fn home_slug(&self) -> Option<String> {
        let guard = self.orgs.read().ok()?;
        for slug in guard.keys() {
            // `is_home` lives in the manifest, not the runtime
            // state — re-read from disk.
            if let Ok(manifest) = self.data_root.org(slug.as_str()).manifest() {
                if manifest.is_home {
                    return Some(slug.clone());
                }
            }
        }
        None
    }

    /// Hot-add a freshly scaffolded org to the live dispatcher.
    /// The server-management RPC calls this after writing the
    /// org's dir + initializing its DBs.
    pub fn insert_org(&self, slug: String, state: OrgAppState) -> Result<(), &'static str> {
        self.orgs
            .write()
            .map_err(|_| "orgs lock poisoned")?
            .insert(slug, state);
        Ok(())
    }
}

impl AppState {
    /// Boot path: scan `<data_root>/orgs/` and build one
    /// [`OrgAppState`] per discovered org. Hosts all of them
    /// at `/org/<slug>/...`. If `slug_filter` is `Some`,
    /// only that one org is hosted (matches the
    /// single-org-process pattern earlier PRs used).
    ///
    /// When no orgs are present the server boots empty — the
    /// `/server/vox` `OrgManagementService` accepts an
    /// unauthenticated `create_org` in that state so the CLI
    /// can bootstrap the first org without touching the
    /// server's filesystem.
    pub async fn new(slug_filter: Option<&str>) -> eyre::Result<Self> {
        let data_root =
            org_proto::DataRoot::from_env().map_err(|e| eyre::eyre!("data root: {e}"))?;
        data_root
            .ensure()
            .map_err(|e| eyre::eyre!("ensure data root: {e}"))?;
        let keypair = ServerKeypair::load_or_generate(&data_root.server_keypair_path())
            .map_err(|e| eyre::eyre!("load server keypair: {e}"))?;

        let org_roots = pick_server_orgs(&data_root, slug_filter)?;
        let mut orgs = std::collections::HashMap::new();
        for org_root in org_roots {
            let slug = org_root.slug().to_owned();
            let auth_db_url = format!("sqlite://{}?mode=rwc", org_root.auth_db().display());
            let auth = AuthState::open(&auth_db_url, DEFAULT_AUTH_SECRET).await?;
            let org_state = build_org_state(auth, &keypair, org_root).await?;
            orgs.insert(slug, org_state);
        }

        Ok(Self {
            keypair,
            orgs: Arc::new(std::sync::RwLock::new(orgs)),
            data_root,
        })
    }

    /// Test helper. Build a one-org `AppState` from an
    /// explicit auth + keypair (e.g. in-memory `AuthState`
    /// plus ephemeral keypair). Picks the org root the same
    /// way [`Self::new`] does.
    pub async fn new_with_auth(auth: AuthState, keypair: ServerKeypair) -> eyre::Result<Self> {
        let data_root =
            org_proto::DataRoot::from_env().map_err(|e| eyre::eyre!("data root: {e}"))?;
        data_root
            .ensure()
            .map_err(|e| eyre::eyre!("ensure data root: {e}"))?;
        let mut org_roots = pick_server_orgs(&data_root, None)?;
        let org_root = org_roots
            .pop()
            .ok_or_else(|| eyre::eyre!("no org to host"))?;
        let slug = org_root.slug().to_owned();
        let org_state = build_org_state(auth, &keypair, org_root).await?;
        let mut orgs = std::collections::HashMap::new();
        orgs.insert(slug, org_state);
        Ok(Self {
            keypair,
            orgs: Arc::new(std::sync::RwLock::new(orgs)),
            data_root,
        })
    }

    /// Test helper: same as `new_with_auth` but takes an
    /// explicit [`OrgRoot`] (tempdir-backed in tests) instead
    /// of scanning the data root.
    pub async fn new_with_auth_and_org(
        auth: AuthState,
        keypair: ServerKeypair,
        org_root: org_proto::OrgRoot,
    ) -> eyre::Result<Self> {
        let data_root =
            org_proto::DataRoot::from_env().map_err(|e| eyre::eyre!("data root: {e}"))?;
        let slug = org_root.slug().to_owned();
        let org_state = build_org_state(auth, &keypair, org_root).await?;
        let mut orgs = std::collections::HashMap::new();
        orgs.insert(slug, org_state);
        Ok(Self {
            keypair,
            orgs: Arc::new(std::sync::RwLock::new(orgs)),
            data_root,
        })
    }
}

/// Build one [`OrgAppState`] for a single org's
/// [`OrgRoot`]. Opens every backend the vox dispatcher
/// will mount.
pub(crate) async fn build_org_state(
    auth: AuthState,
    keypair: &ServerKeypair,
    org_root: org_proto::OrgRoot,
) -> eyre::Result<OrgAppState> {
    {
        // Attachments — local blob store under the standard XDG
        // path; the keypair signs upload/download URLs.
        let blob_root =
            attachments::default_blob_root().map_err(|e| eyre::eyre!("blob root: {e}"))?;
        let object_store: Arc<dyn attachments::ObjectStore> =
            Arc::new(attachments::LocalFsStore::new(blob_root));
        let public_base_url = std::env::var("TASK_SERVER_PUBLIC_URL").unwrap_or_default();
        let attachment_service = Arc::new(attachments::AttachmentServiceImpl::new(
            keypair.clone(),
            object_store,
            public_base_url,
        ));

        // Vault file-replication. Org-scoped: each org's
        // vault lives under `<data_root>/orgs/<slug>/vault/`.
        // `TASK_SERVER_VAULT_ROOT` still wins as a hard
        // override (for tests / containers that want a
        // flat parent dir).
        let vault_root = std::env::var("TASK_SERVER_VAULT_ROOT")
            .map_or_else(|_| org_root.vault_dir(), PathBuf::from);
        // `single("default", vault_root)` — one vault per org,
        // and `vault_id = "default"` resolves *to the org's vault
        // root directly*. Earlier we used `under_parent`, which
        // routed writes into `vault_root/default/…` — and every
        // `ProjectBackend` / `GoalBackend` scan then saw each
        // file twice (once at the real path, once under the
        // ghost `default/` subdir). Same `wiki_id = "default"`
        // convention the wiki backend already uses on line 304.
        let vault_sync_state = vault::Backend::single("default", vault_root.clone())
            .map_err(|e| eyre::eyre!("vault backend: {e}"))?;
        // Wiki rooted at `<org>/wiki/Knowledge/` (the curated
        // tier). `LLM/` scratch is a sibling subtree the
        // wiki backend doesn't touch — agents read/write it
        // through plain filesystem ops. `wiki_id = "default"`
        // is conventional for the one-wiki-per-org case;
        // future federation may surface multiple ids.
        let wiki_root = std::env::var("TASK_SERVER_WIKI_ROOT")
            .map_or_else(|_| org_root.wiki_knowledge_dir(), PathBuf::from);
        let wiki = wiki_live::WikiBackend::single("default", wiki_root.clone())
            .map_err(|e| eyre::eyre!("wiki backend: {e}"))?;

        // Agent-task queue. SQLite under the org root
        // (override via `TASK_SERVER_AGENT_TASKS_URL`).
        // `OrgRoot` doesn't yet have an `agent_tasks_db()`
        // helper — we co-locate it alongside the other org
        // dbs by hand for now. PR 4 promotes this to a
        // first-class resolver.
        let agent_tasks_url = std::env::var("TASK_SERVER_AGENT_TASKS_URL").unwrap_or_else(|_| {
            format!(
                "sqlite://{}?mode=rwc",
                org_root.path().join("agent-tasks.sqlite").display()
            )
        });
        let agent_tasks_conn = Database::connect(&agent_tasks_url)
            .await
            .map_err(|e| eyre::eyre!("connect agent-tasks db `{agent_tasks_url}`: {e}"))?;
        agent_tasks::Migrator::up(&agent_tasks_conn, None)
            .await
            .map_err(|e| eyre::eyre!("agent-tasks migrations: {e}"))?;
        let agent_tasks = agent_tasks::Store::new(agent_tasks_conn);

        // Timer store. SQLite at
        // `<data_root>/orgs/<slug>/timer.sqlite`
        // (override via `TASK_SERVER_TIMER_URL`). Project
        // defaults are resolved off the same vault root the
        // rest of the server uses — the rate cascade calls
        // `VaultProjectDefaults::lookup` to read each
        // session's project markdown on close.
        let timer_url = std::env::var("TASK_SERVER_TIMER_URL")
            .unwrap_or_else(|_| format!("sqlite://{}?mode=rwc", org_root.timer_db().display()));
        let timer_conn = Database::connect(&timer_url)
            .await
            .map_err(|e| eyre::eyre!("connect timer db `{timer_url}`: {e}"))?;
        timer::Migrator::up(&timer_conn, None)
            .await
            .map_err(|e| eyre::eyre!("timer migrations: {e}"))?;
        let timer_defaults = std::sync::Arc::new(timer::store::VaultProjectDefaults {
            vault_root: vault_root.clone(),
        });
        let timer = timer::Store::new(timer_conn, timer_defaults);

        // Finance store. SQLite at
        // `<data_root>/orgs/<slug>/finance.sqlite`
        // (override via `TASK_SERVER_FINANCE_URL`). Services
        // (Invoicing / Ledger) are not mounted yet — only
        // the migrated DB connection is exposed; the
        // task-cli `finance invoice` flow writes against it
        // when that feature lands.
        let finance_url = std::env::var("TASK_SERVER_FINANCE_URL")
            .unwrap_or_else(|_| format!("sqlite://{}?mode=rwc", org_root.finance_db().display()));
        let finance_conn = Database::connect(&finance_url)
            .await
            .map_err(|e| eyre::eyre!("connect finance db `{finance_url}`: {e}"))?;
        finance_db::Migrator::up(&finance_conn, None)
            .await
            .map_err(|e| eyre::eyre!("finance migrations: {e}"))?;

        // Auto-retry any wiki ingest tasks the previous
        // backend left stuck mid-flight. Best-effort —
        // failures here shouldn't block startup.
        if let Ok(entries) = std::fs::read_dir(&vault_root) {
            for entry in entries.flatten() {
                if !entry.path().is_dir() {
                    continue;
                }
                let wiki_handle = wiki_live::WikiLive::open(entry.path());
                if !wiki_handle.is_bootstrapped() {
                    continue;
                }
                if let Ok((retried, failed)) = wiki_handle.auto_retry_stuck_tasks(3) {
                    if !retried.is_empty() || !failed.is_empty() {
                        tracing::info!(
                            vault = %entry.path().display(),
                            retried = retried.len(),
                            failed = failed.len(),
                            "wiki auto-retry: revived stuck tasks"
                        );
                    }
                }
            }
        }

        // Project + Goal readers. Both walk
        // `<org>/vault/` on each call; cheap-clone PathBuf
        // wrappers, no shared mutable state.
        let projects = project::ProjectBackend::new(vault_root.clone());
        let goals = goal::GoalBackend::new(vault_root.clone());
        let milestones = milestone::MilestoneBackend::new(vault_root.clone());
        let tasks = task::TaskBackend::new(vault_root.clone());
        // Locations + mealplan / pantry each hold their own
        // `vault::Vault` snapshot behind an `Arc<Mutex<…>>`.
        // We open the vault once per store — they're independent
        // mutable views; cross-coordination happens at the
        // service level. `Vault::open` is cheap (no parsing
        // beyond directory walk).
        let locations_vault = vault::Vault::open(&vault_root)
            .map_err(|e| eyre::eyre!("open locations vault: {e}"))?;
        let locations = locations::Store::new(locations_vault);
        // Cookbook lives at `<wiki_root>/Cookbook/*.cook` —
        // typically `<org>/wiki/Knowledge/Cookbook/`, NOT the
        // vault root. Match the wiki backend's anchor.
        let cookbook = cookbook::Store::new(wiki_root.clone());
        let mealplan_vault =
            vault::Vault::open(&vault_root).map_err(|e| eyre::eyre!("open mealplan vault: {e}"))?;
        let mealplan = mealplan::Store::new(mealplan_vault);
        let pantry_vault =
            vault::Vault::open(&vault_root).map_err(|e| eyre::eyre!("open pantry vault: {e}"))?;
        let pantry = pantry::Store::new(pantry_vault);
        // Fitness suite. Each takes its own vault snapshot.
        let body_vault =
            vault::Vault::open(&vault_root).map_err(|e| eyre::eyre!("open body vault: {e}"))?;
        let body = body::Store::new(body_vault);
        let exercises_vault = vault::Vault::open(&vault_root)
            .map_err(|e| eyre::eyre!("open exercises vault: {e}"))?;
        let exercises = exercises::Store::new(exercises_vault);
        let workouts_vault =
            vault::Vault::open(&vault_root).map_err(|e| eyre::eyre!("open workouts vault: {e}"))?;
        let workouts = workouts::Store::new(workouts_vault);
        let intake_vault =
            vault::Vault::open(&vault_root).map_err(|e| eyre::eyre!("open intake vault: {e}"))?;
        let intake = intake::Store::new(intake_vault);

        Ok(OrgAppState {
            slug: org_root.slug().to_owned(),
            auth,
            attachments: attachment_service,
            vault_sync: vault_sync_state,
            wiki,
            projects,
            goals,
            milestones,
            tasks,
            locations,
            cookbook,
            mealplan,
            pantry,
            body,
            exercises,
            workouts,
            intake,
            agent_tasks,
            agent_dispatch_vault_root: vault_root,
            timer,
            finance_conn,
        })
    }
}

/// Dev default — replace via config in a later phase. Length-checked
/// at build time so this fails loudly if shortened.
const DEFAULT_AUTH_SECRET: &str = "task-server-auth-dev-secret-32+!";

/// Pick the org root this server should serve.
///
/// Order: explicit `slug` arg → `TASK_SERVER_ORG` env →
/// scan-and-disambiguate. If `<data_root>/orgs/` has one
/// loadable org, use it. If none, auto-bootstrap `default`
/// so a fresh install boots. If many, refuse — operator
/// must pick one (PR 4 lifts this and serves all of them).
/// Pick the [`OrgRoot`]s this server should host.
///
/// - `slug_filter = Some` → host exactly that one org
///   (rejects on missing dir).
/// - `slug_filter = None` + `$TASK_SERVER_ORG` set → host
///   just that env-selected org (legacy single-org boot).
/// - `slug_filter = None`, env unset → host every loadable
///   org under `<data_root>/orgs/`. Returns an empty vec
///   when none exist; the server-management RPC handles
///   first-org bootstrap from there.
fn pick_server_orgs(
    data_root: &org_proto::DataRoot,
    slug_filter: Option<&str>,
) -> eyre::Result<Vec<org_proto::OrgRoot>> {
    let explicit = slug_filter
        .map(str::to_owned)
        .or_else(|| std::env::var("TASK_SERVER_ORG").ok())
        .filter(|s| !s.is_empty());
    if let Some(slug) = explicit {
        let (org_root, _) = data_root
            .load_org(&slug)
            .map_err(|e| eyre::eyre!("load org `{slug}`: {e}"))?;
        return Ok(vec![org_root]);
    }
    let scanned = data_root
        .scan_orgs()
        .map_err(|e| eyre::eyre!("scan orgs: {e}"))?;
    // Empty data root is no longer auto-bootstrapped. The
    // `/server/vox` `OrgManagementService` accepts an
    // unauthenticated `create_org` while in this state, so
    // the CLI flow `task org create … --home` mints the
    // first org without anyone touching the server's
    // filesystem directly.
    Ok(scanned.into_iter().map(|(org, _)| org).collect())
}

pub fn router(state: AppState) -> Router {
    use attachments::routes::AttachmentRouteState;
    use axum::routing::any;

    // Mount the /blobs/* HTTP routes against the FIRST org's
    // attachment service. Multi-org blob routing
    // (`/org/<slug>/blobs/...`) is a follow-up — the existing
    // path stays for single-org back-compat. When no org is
    // hosted (test boot path), fall back to a synthetic
    // empty router so axum doesn't choke.
    let blob_router = state
        .orgs
        .read()
        .ok()
        .and_then(|guard| guard.values().next().cloned())
        .map(|org| {
            let blob_state = AttachmentRouteState {
                service: org.attachments.clone(),
            };
            attachments::attachment_router().with_state(blob_state)
        })
        .unwrap_or_default();

    // Per-org vox at `/org/{slug}/vox`. Also keep `/vox`
    // and `/health` at the top level for back-compat —
    // `/vox` dispatches into the first hosted org so
    // single-org clients keep working without a URL change.
    let well_known = Router::new()
        .route("/.well-known/task-server.json", get(well_known_handler))
        .with_state(state.clone());
    let per_org = Router::new()
        .route("/org/{slug}/health", get(per_org_health_handler))
        .route("/org/{slug}/vox", any(per_org_vox_handler))
        .with_state(state.clone());

    // Server-management vox: `OrgManagementService` mounted on
    // a top-level endpoint (not per-org). Lets a CLI connect
    // once and ask the server to scaffold new orgs without
    // touching the data root locally.
    let server_mgmt = Router::new()
        .route("/server/vox", any(server_vox_handler))
        .with_state(state.clone());

    Router::new()
        .route("/health", get(|| async { "ok" }))
        .route("/vox", get(legacy_vox_handler))
        .merge(well_known)
        .merge(per_org)
        .merge(server_mgmt)
        .merge(blob_router)
        .layer(tower_http::cors::CorsLayer::permissive())
        .with_state(state)
}

/// `.well-known/task-server.json` — federation discovery.
/// Lists every org this server hosts plus its routing URL
/// suffix. Public, no auth required.
///
/// Per `plans/federated-task-platform.md`: peers fetch this
/// to learn what slugs are available on a federation host
/// before opening a vox connection.
async fn well_known_handler(State(state): State<AppState>) -> axum::Json<serde_json::Value> {
    let orgs: Vec<serde_json::Value> = state
        .org_slugs()
        .into_iter()
        .filter_map(|slug| {
            // We only have the slug here — the display
            // name + federation URL live in `org.toml`,
            // re-loaded for each entry. Cheap (TOML parse
            // of a tiny file) and avoids holding manifest
            // copies on every dispatched request.
            let manifest = state.data_root.org(slug.as_str()).manifest().ok()?;
            Some(serde_json::json!({
                "slug": slug,
                "display_name": manifest.display_name,
                "is_home": manifest.is_home,
                "federation_url": manifest.federation_url,
                "vox": format!("/org/{slug}/vox"),
                "health": format!("/org/{slug}/health"),
            }))
        })
        .collect();
    axum::Json(serde_json::json!({
        "version": 1,
        "orgs": orgs,
    }))
}

/// `/org/<slug>/health` — per-org liveness probe. `200 ok`
/// when the slug is hosted, `404` otherwise.
async fn per_org_health_handler(
    State(state): State<AppState>,
    axum::extract::Path(slug): axum::extract::Path<String>,
) -> axum::response::Response {
    if state.org(&slug).is_some() {
        axum::response::IntoResponse::into_response("ok")
    } else {
        axum::response::IntoResponse::into_response((
            axum::http::StatusCode::NOT_FOUND,
            format!("org `{slug}` not hosted"),
        ))
    }
}

/// `/org/<slug>/vox` — per-org vox WebSocket. Looks up the
/// slug in the AppState's org map; rejects with 404 if the
/// org isn't hosted.
async fn per_org_vox_handler(
    State(state): State<AppState>,
    axum::extract::Path(slug): axum::extract::Path<String>,
    ws: WebSocketUpgrade,
) -> axum::response::Response {
    let Some(org) = state.org(&slug) else {
        return axum::response::IntoResponse::into_response((
            axum::http::StatusCode::NOT_FOUND,
            format!("org `{slug}` not hosted"),
        ));
    };
    serve_org_vox(org, ws)
}

/// `/server/vox` — server-management WebSocket. Hosts the
/// `OrgManagementService`. Unauthenticated requests are
/// allowed in bootstrap mode (no orgs hosted yet); after that
/// the service itself rejects requests whose `session_token`
/// doesn't validate against the home org.
async fn server_vox_handler(
    State(state): State<AppState>,
    ws: WebSocketUpgrade,
) -> axum::response::Response {
    let mgmt = crate::server_mgmt::OrgManagementImpl::new(state);
    ws.on_upgrade(move |socket| async move {
        let mgmt = mgmt.clone();
        let acceptor =
            architect::axum_ws::acceptor_fn(move |req, connection| match req.service() {
                name if name == org_proto::org_management_descriptor().service_name => {
                    connection.handle_with(org_proto::serve_org_management(mgmt.clone()));
                    Ok(())
                }
                other => {
                    tracing::info!(
                        service = %other,
                        "server-vox: unknown service requested"
                    );
                    Err(Vec::new())
                }
            });
        architect::axum_ws::serve(socket, acceptor).await;
    })
    .into_response()
}

/// `/vox` — legacy single-org alias. Dispatches into the
/// first hosted org so clients written against the
/// pre-multi-org URL keep working without a redirect.
/// Returns 503 when no org is hosted (which shouldn't
/// happen post-boot but is a sane fallback).
async fn legacy_vox_handler(
    State(state): State<AppState>,
    ws: WebSocketUpgrade,
) -> axum::response::Response {
    let Some(org) = state
        .orgs
        .read()
        .ok()
        .and_then(|guard| guard.values().next().cloned())
    else {
        return axum::response::IntoResponse::into_response((
            axum::http::StatusCode::SERVICE_UNAVAILABLE,
            "no org hosted on this server",
        ));
    };
    serve_org_vox(org, ws)
}

fn serve_org_vox(org: OrgAppState, ws: WebSocketUpgrade) -> axum::response::Response {
    ws.on_upgrade(move |socket| async move {
        let auth = org.auth.auth.clone();
        let attachment_service = org.attachments.clone();
        let vault_sync_state = org.vault_sync.clone();
        let wiki = org.wiki.clone();
        let projects_backend = org.projects.clone();
        let goals_backend = org.goals.clone();
        let milestones_backend = org.milestones.clone();
        let tasks_backend = org.tasks.clone();
        let locations_backend = org.locations.clone();
        let cookbook_backend = org.cookbook.clone();
        let mealplan_backend = org.mealplan.clone();
        let pantry_backend = org.pantry.clone();
        let body_backend = org.body.clone();
        let exercises_backend = org.exercises.clone();
        let workouts_backend = org.workouts.clone();
        let intake_backend = org.intake.clone();
        let agent_tasks_store = org.agent_tasks.clone();
        let timer_store = org.timer.clone();
        let acceptor =
            architect::axum_ws::acceptor_fn(move |req, connection| match req.service() {
                "AuthService" => {
                    connection.handle_with(
                        AuthServiceDispatcher::new(AuthVoxService::new(auth.clone()))
                            .with_middleware(AuthServerMiddleware),
                    );
                    Ok(())
                }
                "AttachmentService" => {
                    use attachments_proto::AttachmentServiceDispatcher;
                    connection.handle_with(AttachmentServiceDispatcher::new(
                        (*attachment_service).clone(),
                    ));
                    Ok(())
                }
                // architect-emitted mount: `serve` wraps the
                // backend in `VaultSyncRpcDispatcher` and pulls
                // its `TokioBlockingDispatcher` via
                // `HasDispatcher`. Wire-level service name from
                // `vault_proto::descriptor()`.
                name if name == vault_proto::descriptor().service_name => {
                    connection.handle_with(vault_proto::serve(vault_sync_state.clone()));
                    Ok(())
                }
                // Agent-task queue — slim domain trait (claim,
                // complete, set-status). Plain CRUD is the
                // architect-emitted per-entity Repo traits, not
                // mounted here yet.
                name if name
                    == agent_proto::service::tasks::agent_task_queue_rpc_service_descriptor()
                        .service_name =>
                {
                    connection.handle_with(agent_proto::service::tasks::serve(
                        agent_tasks_store.clone(),
                    ));
                    Ok(())
                }
                // Timer — billable time tracking. The slim
                // TimerService trait (start/stop/active/
                // switch/log/resolve_rate). Plain CRUD on
                // Client/Tag/Rate/WorkSession entities goes
                // through their architect-emitted Repo
                // traits, not mounted here yet.
                name if name
                    == timer_proto::service::timer_service_rpc_service_descriptor()
                        .service_name =>
                {
                    connection.handle_with(timer_proto::service::serve(timer_store.clone()));
                    Ok(())
                }
                // Wiki feature — 13 per-capability traits, one
                // descriptor each. `wiki_proto::service::*`.
                name if name
                    == wiki_proto::service::schema::schema_rpc_service_descriptor()
                        .service_name =>
                {
                    connection.handle_with(wiki_proto::service::schema::serve(wiki.clone()));
                    Ok(())
                }
                name if name
                    == wiki_proto::service::catalog::catalog_rpc_service_descriptor()
                        .service_name =>
                {
                    connection.handle_with(wiki_proto::service::catalog::serve(wiki.clone()));
                    Ok(())
                }
                name if name
                    == wiki_proto::service::raw_layer::raw_layer_rpc_service_descriptor()
                        .service_name =>
                {
                    connection.handle_with(wiki_proto::service::raw_layer::serve(wiki.clone()));
                    Ok(())
                }
                name if name
                    == wiki_proto::service::graph::graph_rpc_service_descriptor().service_name =>
                {
                    connection.handle_with(wiki_proto::service::graph::serve(wiki.clone()));
                    Ok(())
                }
                name if name
                    == wiki_proto::service::ingest::ingest_rpc_service_descriptor()
                        .service_name =>
                {
                    connection.handle_with(wiki_proto::service::ingest::serve(wiki.clone()));
                    Ok(())
                }
                name if name
                    == wiki_proto::service::lint::lint_rpc_service_descriptor().service_name =>
                {
                    connection.handle_with(wiki_proto::service::lint::serve(wiki.clone()));
                    Ok(())
                }
                name if name
                    == wiki_proto::service::search::search_rpc_service_descriptor()
                        .service_name =>
                {
                    connection.handle_with(wiki_proto::service::search::serve(wiki.clone()));
                    Ok(())
                }
                name if name
                    == wiki_proto::service::watcher::watcher_rpc_service_descriptor()
                        .service_name =>
                {
                    connection.handle_with(wiki_proto::service::watcher::serve(wiki.clone()));
                    Ok(())
                }
                name if name
                    == wiki_proto::service::multimodal::multimodal_rpc_service_descriptor()
                        .service_name =>
                {
                    connection.handle_with(wiki_proto::service::multimodal::serve(wiki.clone()));
                    Ok(())
                }
                name if name
                    == wiki_proto::service::review::review_rpc_service_descriptor()
                        .service_name =>
                {
                    connection.handle_with(wiki_proto::service::review::serve(wiki.clone()));
                    Ok(())
                }
                // Project + Goal services — file-backed
                // readers that walk the org's vault on each
                // request. UI surfaces consume the
                // architect-generated `ProjectServiceClient`
                // and `GoalServiceClient`.
                name if name == project::project_service_descriptor().service_name => {
                    connection
                        .handle_with(project::serve_project_service(projects_backend.clone()));
                    Ok(())
                }
                name if name == goal::goal_service_descriptor().service_name => {
                    connection.handle_with(goal::serve_goal_service(goals_backend.clone()));
                    Ok(())
                }
                name if name == milestone::milestone_service_descriptor().service_name => {
                    connection.handle_with(milestone::serve_milestone_service(
                        milestones_backend.clone(),
                    ));
                    Ok(())
                }
                name if name == task::task_service_descriptor().service_name => {
                    connection.handle_with(task::serve_task_service(tasks_backend.clone()));
                    Ok(())
                }
                // Entity-CRUD services: locations + the
                // mealplan trio (recipes, scheduled meals,
                // pantry). All four expose the same
                // `list / get / create / update / rename /
                // delete` shape; pantry + mealplan
                // additionally expose domain verbs
                // (`consume`, `cook`, …) at the trait level.
                name if name == locations::locations_service_descriptor().service_name => {
                    connection.handle_with(locations::serve_locations_service(
                        locations_backend.clone(),
                    ));
                    Ok(())
                }
                name if name == cookbook::cookbook_service_descriptor().service_name => {
                    connection
                        .handle_with(cookbook::serve_cookbook_service(cookbook_backend.clone()));
                    Ok(())
                }
                name if name == mealplan::mealplan_service_descriptor().service_name => {
                    connection
                        .handle_with(mealplan::serve_mealplan_service(mealplan_backend.clone()));
                    Ok(())
                }
                name if name == pantry::pantry_service_descriptor().service_name => {
                    connection.handle_with(pantry::serve_pantry_service(pantry_backend.clone()));
                    Ok(())
                }
                // Fitness suite — body / exercises / workouts /
                // intake. All four mounted per-org alongside
                // the entity-CRUD services above.
                name if name == body::body_service_descriptor().service_name => {
                    connection.handle_with(body::serve_body_service(body_backend.clone()));
                    Ok(())
                }
                name if name == exercises::exercises_service_descriptor().service_name => {
                    connection.handle_with(exercises::serve_exercises_service(
                        exercises_backend.clone(),
                    ));
                    Ok(())
                }
                name if name == workouts::workouts_service_descriptor().service_name => {
                    connection
                        .handle_with(workouts::serve_workouts_service(workouts_backend.clone()));
                    Ok(())
                }
                name if name == intake::intake_service_descriptor().service_name => {
                    connection.handle_with(intake::serve_intake_service(intake_backend.clone()));
                    Ok(())
                }
                other => {
                    tracing::info!(
                        service = %other,
                        "vox session: unknown service requested"
                    );
                    Err(Vec::new())
                }
            });
        architect::axum_ws::serve(socket, acceptor).await;
    })
    .into_response()
}
