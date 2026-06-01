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
pub mod webhooks;

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
    /// Scheduling backend — day templates / availability under
    /// `vault/Projects/Scheduling/`. Mounted for `DayTemplates` so
    /// the app can overlay the daily plan on the calendar.
    pub scheduling: scheduling::VaultScheduler,
    /// Inbox backend — captured items under `vault/Records/inbox/`.
    /// Mounted for `Inbox` so the capture UIs + daily review can
    /// round-trip fleeting notes.
    pub inbox: inbox::VaultInbox,
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
    /// Construction scope for every backend resource (DB pools).
    /// Each org's SQLite pools register a finalizer here via
    /// architect's [`Resource::acquire_release`]; [`Scope::close`]
    /// at shutdown tears them down in LIFO order. Shared across all
    /// hosted orgs.
    pub scope: std::sync::Arc<architect::Scope>,
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

    /// Serve an org's full [`LayerRouter`] over an **in-process**
    /// vox link (no socket, no TCP). Returns a [`LocalServer`] whose
    /// `.establish::<C>()` yields the *same* service client types the
    /// WebSocket transport produces — so a native binary (CLI, desktop)
    /// can drive the backend directly without a running `task-server`.
    /// This is architect's "inject remote vs local, one client".
    ///
    /// The acceptor task lives until `scope` is closed; keep the scope
    /// alive for as long as the clients are used, then `scope.close()`.
    /// `None` if the slug isn't hosted.
    #[must_use]
    pub fn local_server(
        &self,
        slug: &str,
        scope: &std::sync::Arc<architect::Scope>,
    ) -> Option<architect::LocalServer> {
        let org = self.org(slug)?;
        Some(architect::LocalServer::serve(
            org_layer_router(&org),
            std::sync::Arc::clone(scope),
        ))
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

        let scope = architect::Scope::new();
        let org_roots = pick_server_orgs(&data_root, slug_filter)?;
        let mut orgs = std::collections::HashMap::new();
        for org_root in org_roots {
            let slug = org_root.slug().to_owned();
            let auth_db_url = format!("sqlite://{}?mode=rwc", org_root.auth_db().display());
            let auth = AuthState::open(&auth_db_url, DEFAULT_AUTH_SECRET).await?;
            let org_state = build_org_state(auth, &keypair, org_root, &scope).await?;
            orgs.insert(slug, org_state);
        }

        Ok(Self {
            keypair,
            orgs: Arc::new(std::sync::RwLock::new(orgs)),
            data_root,
            scope,
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
        let scope = architect::Scope::new();
        let mut org_roots = pick_server_orgs(&data_root, None)?;
        let org_root = org_roots
            .pop()
            .ok_or_else(|| eyre::eyre!("no org to host"))?;
        let slug = org_root.slug().to_owned();
        let org_state = build_org_state(auth, &keypair, org_root, &scope).await?;
        let mut orgs = std::collections::HashMap::new();
        orgs.insert(slug, org_state);
        Ok(Self {
            keypair,
            orgs: Arc::new(std::sync::RwLock::new(orgs)),
            data_root,
            scope,
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
        let scope = architect::Scope::new();
        let slug = org_root.slug().to_owned();
        let org_state = build_org_state(auth, &keypair, org_root, &scope).await?;
        let mut orgs = std::collections::HashMap::new();
        orgs.insert(slug, org_state);
        Ok(Self {
            keypair,
            orgs: Arc::new(std::sync::RwLock::new(orgs)),
            data_root,
            scope,
        })
    }
}

/// Open a migrated SQLite pool as an architect [`Resource`] tied to
/// `scope`: connect, run `migrate`, and register a finalizer that
/// closes the pool. On [`Scope::close`] (graceful shutdown) every pool
/// opened this way is torn down in LIFO order instead of relying on
/// `Drop`. `migrate` receives the fresh connection and returns it after
/// running its migrator.
async fn open_sqlite_pool<F>(
    scope: &std::sync::Arc<architect::Scope>,
    url: String,
    label: &'static str,
    migrate: F,
) -> eyre::Result<sea_orm::DatabaseConnection>
where
    F: FnOnce(
            sea_orm::DatabaseConnection,
        ) -> std::pin::Pin<
            Box<
                dyn std::future::Future<
                        Output = Result<sea_orm::DatabaseConnection, sea_orm::DbErr>,
                    > + Send,
            >,
        > + Send
        + 'static,
{
    architect::Resource::acquire_release(
        architect::Resource::from_fn(move |_| async move {
            let db = Database::connect(&url)
                .await
                .map_err(|e| eyre::eyre!("connect {label} db `{url}`: {e}"))?;
            let db = migrate(db)
                .await
                .map_err(|e| eyre::eyre!("{label} migrations: {e}"))?;
            Ok(db)
        }),
        |db: sea_orm::DatabaseConnection| async move {
            if let Err(e) = db.close().await {
                tracing::warn!(error = %e, "closing sqlite pool");
            }
        },
    )
    .build(scope)
    .await
}

/// Build one [`OrgAppState`] for a single org's
/// [`OrgRoot`]. Opens every backend the vox dispatcher
/// will mount.
pub(crate) async fn build_org_state(
    auth: AuthState,
    keypair: &ServerKeypair,
    org_root: org_proto::OrgRoot,
    scope: &std::sync::Arc<architect::Scope>,
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
        let agent_tasks_conn = open_sqlite_pool(scope, agent_tasks_url, "agent-tasks", |db| {
            Box::pin(async move { agent_tasks::Migrator::up(&db, None).await.map(|()| db) })
        })
        .await?;
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
        let timer_conn = open_sqlite_pool(scope, timer_url, "timer", |db| {
            Box::pin(async move { timer::Migrator::up(&db, None).await.map(|()| db) })
        })
        .await?;
        let timer_defaults = std::sync::Arc::new(timer::store::VaultProjectDefaults {
            vault_root: vault_root.clone(),
        });
        let timer = timer::Store::new(timer_conn, timer_defaults);

        // Scheduling backend rooted at the same vault. Day templates
        // live under `Projects/Scheduling/templates/`; the kv/log
        // stores back bookings + slot caches we don't surface yet, so
        // an in-memory pair suffices for the mounted `DayTemplates`.
        let scheduling = scheduling::VaultScheduler::new(
            vault_root.clone(),
            Box::new(store_proto::mem::MemStore::new()),
            Box::new(store_proto::mem::MemStore::new()),
        )
        .map_err(|e| eyre::eyre!("scheduling backend: {e}"))?;

        // Inbox backend rooted at the same vault — captured items
        // live under `Records/inbox/`.
        let inbox = inbox::VaultInbox::new(vault_root.clone())
            .map_err(|e| eyre::eyre!("inbox backend: {e}"))?;

        // Finance store. SQLite at
        // `<data_root>/orgs/<slug>/finance.sqlite`
        // (override via `TASK_SERVER_FINANCE_URL`). Services
        // (Invoicing / Ledger) are not mounted yet — only
        // the migrated DB connection is exposed; the
        // task-cli `finance invoice` flow writes against it
        // when that feature lands.
        let finance_url = std::env::var("TASK_SERVER_FINANCE_URL")
            .unwrap_or_else(|_| format!("sqlite://{}?mode=rwc", org_root.finance_db().display()));
        let finance_conn = open_sqlite_pool(scope, finance_url, "finance", |db| {
            Box::pin(async move { finance_db::Migrator::up(&db, None).await.map(|()| db) })
        })
        .await?;

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
            scheduling,
            inbox,
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
        .route(
            "/org/{slug}/webhooks/forge",
            axum::routing::post(webhooks::forge_webhook_handler),
        )
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
                "id": manifest.id,
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
    let router = architect::LayerRouter::new().with(
        org_proto::org_management_descriptor(),
        org_proto::serve_org_management(mgmt),
    );
    ws.on_upgrade(move |socket| async move {
        let acceptor = architect::axum_ws::acceptor_fn(move |_req, connection| {
            connection.handle_with(router.clone());
            Ok(())
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

/// Build the per-org [`LayerRouter`]: every service this org hosts,
/// mounted by its descriptor onto one router. One connection then
/// multiplexes all of them — the client's establish handshake names
/// the service and the router dispatches by method id.
///
/// This replaces the old per-connection `match req.service()` acceptor
/// with architect's composable layer system; the same router is reused
/// for the WebSocket transport here and the in-process `LocalServer`
/// transport (see [`org_local_server`]).
pub fn org_layer_router(org: &OrgAppState) -> architect::LayerRouter {
    use architect::LayerRouter;

    let mut router = LayerRouter::new()
        // Auth — wrapped with the server middleware that validates
        // session tokens before the inner service sees the request.
        .with(
            architect_auth::auth_service_service_descriptor(),
            AuthServiceDispatcher::new(AuthVoxService::new(org.auth.auth.clone()))
                .with_middleware(AuthServerMiddleware),
        )
        // Attachments — signed blob upload/download.
        .with(
            attachments_proto::attachment_service_service_descriptor(),
            attachments_proto::AttachmentServiceDispatcher::new((*org.attachments).clone()),
        )
        // Vault file replication (manifest / get / put / delete / subscribe).
        .with(
            vault_proto::descriptor(),
            vault_proto::serve(org.vault_sync.clone()),
        )
        // Agent-task queue — slim domain trait (claim / complete / set-status).
        .with(
            agent_proto::service::tasks::agent_task_queue_rpc_service_descriptor(),
            agent_proto::service::tasks::serve(org.agent_tasks.clone()),
        )
        // Timer — billable time tracking.
        .with(
            timer_proto::service::timer_service_rpc_service_descriptor(),
            timer_proto::service::serve(org.timer.clone()),
        )
        // Scheduling — day templates (drives the calendar overlay)
        // + per-date day plans (the day-by-day editor).
        .with(
            scheduling_proto::service::day_templates::day_templates_rpc_service_descriptor(),
            scheduling_proto::service::day_templates::serve(org.scheduling.clone()),
        )
        .with(
            scheduling_proto::service::day_plans::day_plans_rpc_service_descriptor(),
            scheduling_proto::service::day_plans::serve(org.scheduling.clone()),
        )
        .with(
            scheduling_proto::service::calendar_events::calendar_events_rpc_service_descriptor(),
            scheduling_proto::service::calendar_events::serve(org.scheduling.clone()),
        )
        .with(
            inbox_proto::service::inbox::inbox_rpc_service_descriptor(),
            inbox_proto::service::inbox::serve(org.inbox.clone()),
        );

    // Wiki feature — 11 per-capability traits, one descriptor each.
    let wiki = org.wiki.clone();
    router = router
        .with(
            wiki_proto::service::schema::schema_rpc_service_descriptor(),
            wiki_proto::service::schema::serve(wiki.clone()),
        )
        .with(
            wiki_proto::service::catalog::catalog_rpc_service_descriptor(),
            wiki_proto::service::catalog::serve(wiki.clone()),
        )
        .with(
            wiki_proto::service::raw_layer::raw_layer_rpc_service_descriptor(),
            wiki_proto::service::raw_layer::serve(wiki.clone()),
        )
        .with(
            wiki_proto::service::graph::graph_rpc_service_descriptor(),
            wiki_proto::service::graph::serve(wiki.clone()),
        )
        .with(
            wiki_proto::service::ingest::ingest_rpc_service_descriptor(),
            wiki_proto::service::ingest::serve(wiki.clone()),
        )
        .with(
            wiki_proto::service::lint::lint_rpc_service_descriptor(),
            wiki_proto::service::lint::serve(wiki.clone()),
        )
        .with(
            wiki_proto::service::search::search_rpc_service_descriptor(),
            wiki_proto::service::search::serve(wiki.clone()),
        )
        .with(
            wiki_proto::service::watcher::watcher_rpc_service_descriptor(),
            wiki_proto::service::watcher::serve(wiki.clone()),
        )
        .with(
            wiki_proto::service::multimodal::multimodal_rpc_service_descriptor(),
            wiki_proto::service::multimodal::serve(wiki.clone()),
        )
        .with(
            wiki_proto::service::review::review_rpc_service_descriptor(),
            wiki_proto::service::review::serve(wiki.clone()),
        );

    // Project / Goal / Milestone / Task readers (vault-backed).
    router = router
        .with(
            project::project_service_descriptor(),
            project::serve_project_service(org.projects.clone()),
        )
        .with(
            goal::goal_service_descriptor(),
            goal::serve_goal_service(org.goals.clone()),
        )
        .with(
            milestone::milestone_service_descriptor(),
            milestone::serve_milestone_service(org.milestones.clone()),
        )
        .with(
            task::task_service_descriptor(),
            task::serve_task_service(org.tasks.clone()),
        );

    // Entity-CRUD services: locations + the mealplan trio.
    router = router
        .with(
            locations::locations_service_descriptor(),
            locations::serve_locations_service(org.locations.clone()),
        )
        .with(
            cookbook::cookbook_service_descriptor(),
            cookbook::serve_cookbook_service(org.cookbook.clone()),
        )
        .with(
            mealplan::mealplan_service_descriptor(),
            mealplan::serve_mealplan_service(org.mealplan.clone()),
        )
        .with(
            pantry::pantry_service_descriptor(),
            pantry::serve_pantry_service(org.pantry.clone()),
        );

    // Fitness suite — body / exercises / workouts / intake.
    router
        .with(
            body::body_service_descriptor(),
            body::serve_body_service(org.body.clone()),
        )
        .with(
            exercises::exercises_service_descriptor(),
            exercises::serve_exercises_service(org.exercises.clone()),
        )
        .with(
            workouts::workouts_service_descriptor(),
            workouts::serve_workouts_service(org.workouts.clone()),
        )
        .with(
            intake::intake_service_descriptor(),
            intake::serve_intake_service(org.intake.clone()),
        )
}

fn serve_org_vox(org: OrgAppState, ws: WebSocketUpgrade) -> axum::response::Response {
    let router = org_layer_router(&org);
    ws.on_upgrade(move |socket| async move {
        let acceptor = architect::axum_ws::acceptor_fn(move |_req, connection| {
            connection.handle_with(router.clone());
            Ok(())
        });
        architect::axum_ws::serve(socket, acceptor).await;
    })
    .into_response()
}
