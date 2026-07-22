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
pub mod media;
pub mod capability;
pub mod connections;
pub mod forge_sync;
pub mod identity_mgmt;
pub mod link_sync;
pub mod presence;
pub mod server_mgmt;
pub mod snapshot;
pub mod watch_bridge;
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
    /// The underlying pool, kept alongside the storage wrapper so
    /// the snapshot engine can `PRAGMA wal_checkpoint(TRUNCATE)` the
    /// auth db with the rest of the org's sqlites.
    pub db: sea_orm::DatabaseConnection,
}

impl AuthState {
    pub async fn open(db_url: &str, secret: &str) -> eyre::Result<Self> {
        let db = Database::connect(db_url)
            .await
            .map_err(|e| eyre::eyre!("connect auth db `{db_url}`: {e}"))?;
        enable_wal(&db, "auth").await;
        AuthMigrator::up(&db, None)
            .await
            .map_err(|e| eyre::eyre!("auth migrations: {e}"))?;
        let storage = AuthSeaOrmStorage::new(db.clone());
        let auth = ArchitectAuth::builder()
            .secret(secret)
            .storage(storage)
            .build()
            .map_err(|e| eyre::eyre!("build ArchitectAuth: {e}"))?;
        Ok(Self { auth, db })
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
    /// Vault-file ⇄ CRDT reconciliation: the per-file doc registry
    /// (lazily opened, seeded from vault files) + write-behind into
    /// [`Self::vault_sync`] + inbound merge of external writes.
    /// Mounted as the `DocSync` service; per-file presence routes
    /// into it through [`presence::PresenceRouter`]. Docs persist
    /// under `<org>/crdt/` (override: `TASK_SERVER_CRDT_ROOT`).
    pub vault_collab: vault_collab::VaultCollab,
    /// FS watcher over the org's vault root — external disk edits
    /// (vim, Obsidian, `git pull`) broadcast the same `VaultEvent`s
    /// wire writes do, which both `subscribe` clients and the
    /// vault-collab inbound listener consume. Held for its lifetime;
    /// `None` when attaching failed (warned, non-fatal).
    pub vault_watcher: Option<Arc<vault::sync::WatcherHandle>>,
    /// Wiki feature backend rooted at this org's `vault/`.
    pub wiki: wiki_live::WikiBackend,
    /// Project list / get backend — walks `vault/Projects/*.md`.
    pub projects: project::ProjectBackend,
    /// Goal list / get backend — walks `vault/Goals/**/*.md`.
    pub goals: goal::GoalBackend,
    /// Milestone backend — project-scoped checkpoints, walks
    /// `vault/Projects/<slug>/milestones/*.md`.
    pub milestones: milestone::MilestoneBackend,
    /// Workstream backend — the parent-with-swarm construct,
    /// walks `vault/Projects/<slug>/workstreams/*.md`. Also
    /// hosts the `WorkstreamService` event-stream hub.
    pub workstreams: workstream::WorkstreamBackend,
    /// Task backend — walks every `type: task` page in the
    /// vault.
    pub tasks: task::TaskBackend,
    /// Locations backend — `type: location` pages.
    pub locations: locations::Store,
    /// Inventory backend — `type: item` gear/equipment pages.
    pub inventory: inventory::Store,
    /// Scripture backend — read-only Bible spine from the resource
    /// library (`<org>/resources/bible/<TX>/`).
    pub scripture: scripture::Store,
    /// Typed-link store — verse↔verse, note↔verse, idea↔wiki links with
    /// confidence + visibility (`<org>/links.jsonl`).
    pub links: links::Store,
    /// Ordered-collection store — song Library / Setlist / Show / Playlist,
    /// one `CollectionService` per org (`<org>/collections.jsonl`; override
    /// `TASK_SERVER_COLLECTIONS_PATH`). JSONL-backed, lexorank-ordered.
    pub collections: collection::Store,
    /// Resource Library reader — serves transcript sidecars under
    /// `<org>/resources/` to the watch/reader UI.
    pub resources: resources::ResourcesBackend,
    /// Cookbook (cooklang recipes under `Wiki/Cookbook/`).
    pub cookbook: cookbook::Store,
    /// Mealplan — scheduled meals + their fulfillment math.
    pub mealplan: mealplan::Store,
    /// Shopping-list service — generated/curated shopping lists.
    pub shopping: mealplan::shopping::Store,
    /// Substitution-rule service — ingredient alternatives.
    pub substitutions: mealplan::substitutions::Store,
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
    /// Codex agent backend — in-process session registry + turn
    /// dispatch. Hosts the `Sessions` + `TurnDispatch` vox services
    /// that back the `/agents` UI. Cheaply clonable (Arc-backed).
    pub agent_codex: agent_codex::CodexBackend,
    pub agent_dispatch_vault_root: PathBuf,
    pub timer: timer::Store,
    /// Threads backend — conversations/topics anchored to any entity
    /// (`(entity_type, entity_id)`); SeaORM-backed. Mounted for the
    /// `ThreadsService` RPC surface.
    pub threads: threads::Store,
    /// Per-user preferences — default page, task-board filter
    /// defaults, last "I'm at" location; SeaORM-backed. Mounted for
    /// the `PrefsService` RPC surface.
    pub prefs: prefs::Store,
    /// Identity locker — per-user encrypted session tokens for
    /// linked remote servers. `Some` only for the **home** org
    /// (the identity anchor); `None` for every federated org.
    /// Backed by `<org>/identity.sqlite`. Mounted for the
    /// server-level `IdentityService` RPC.
    pub identity: Option<identity::Store>,
    /// Scheduling backend — day templates / availability under
    /// `vault/Projects/Scheduling/`. Mounted for `DayTemplates` so
    /// the app can overlay the daily plan on the calendar.
    pub scheduling: scheduling::VaultScheduler,
    /// Inbox backend — captured items under `vault/Records/inbox/`.
    /// Mounted for `Inbox` so the capture UIs + daily review can
    /// round-trip fleeting notes.
    pub inbox: inbox::VaultInbox,
    /// Recall backend — spaced-repetition learning cards under
    /// `vault/Records/recall/`. Mounted for `Recall` so the deck UI +
    /// flashcard review round-trip FSRS-scheduled cards.
    pub recall: recall::VaultRecall,
    /// Contacts backend — vault-backed people directory under
    /// `vault/Records/contacts/`. Mounted for `Contacts` so the
    /// directory UI + CardDAV sync accounts round-trip.
    pub contacts: contacts::VaultContacts,
    /// Tag registry — name → icon/color decorations at
    /// `vault/Records/tags.json`. Mounted for `TagService` so the
    /// calendar / lists decorate markdown tag names with an icon.
    pub tags: tag::VaultTags,
    pub finance_conn: sea_orm::DatabaseConnection,
    /// Invoicing backend — persists invoices in `finance.sqlite` and
    /// links billed sessions in the timer DB. Mounted for `Invoicing`.
    pub finance_backend: finance::FinanceBackend,
    /// Ledger backend — double-entry journal over the same
    /// `finance.sqlite`. Mounted for `Ledger` (post / balances /
    /// account history). The invoicing flow posts into it on
    /// mark-sent + payment.
    pub ledger_backend: finance::LedgerService,
    /// Email backend — a Maildir-backed `email_proto::EmailSync`
    /// impl rooted at `<org>/vault/Mail/`. Serves whatever
    /// accounts that tree contains (one per top-level mailbox
    /// dir); an org with no mail yet serves an empty account
    /// list, which the `/email` UI renders gracefully. Mounted
    /// for the `EmailSync` RPC surface (accounts / folders /
    /// envelopes).
    pub email: email_maildir::Backend,
    /// Forge backend (Forgejo) serving `RepoCatalog` +
    /// `IssueTracker` + `ReviewSurface`. Built from
    /// `TASK_FORGEJO_BASE_URL` + `TASK_FORGEJO_TOKEN`; when either
    /// is absent it's constructed with empty credentials and the
    /// forge calls degrade to auth/forge errors the UI tolerates
    /// (empty list) rather than blocking server startup.
    pub forge: git_forgejo::Backend,
    /// Forge backend authenticated as the agent/bot identity
    /// (`TASK_FORGEJO_BOT_TOKEN`). The forge-sync path routes
    /// agent-owned tasks through this so their issues are
    /// attributed to the bot account, distinct from human work.
    /// Falls back to [`Self::forge`] when no bot token is set.
    pub forge_agent: git_forgejo::Backend,
    /// Path to this org's `issue-links.json` (the `git_config`
    /// `FileStore` shared with the CLI). Held so the forge-sync
    /// decorator + poll loop can open it without re-deriving the
    /// org dir from the data root.
    pub issue_links_path: PathBuf,
    /// Org-wide presence channel host — the Discord-style "who's
    /// online" roster. One per org (the fan-out hub + mirror
    /// `EphemeralStore` live inside; per-connection routers share
    /// it through cheap clones). Serves `DocPresence` on the fixed
    /// [`presence::PRESENCE_DOC_ID`]; nothing is persisted —
    /// states expire on their own when a peer goes quiet.
    pub presence: crdt::sync::PresenceHost,
    /// Link-graph read service (`VaultGraph`) over the same vault
    /// root as [`Self::vault_sync`] — backlinks / links / orphans /
    /// unresolved / deadends / tags for the web vault page.
    pub vault_graph: vault::GraphBackend,
    /// Every open sqlite pool of this org (auth, agent-tasks, timer,
    /// threads, finance). The snapshot engine walks these to
    /// `PRAGMA wal_checkpoint(TRUNCATE)` under the write gate, so
    /// the committed `.sqlite` files are complete + consistent.
    pub sqlite_conns: Vec<sea_orm::DatabaseConnection>,
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
    /// Global write gate for snapshot cycles. Every vox request
    /// (per-org and `/server/vox`) parks at this gate on dispatch
    /// entry ([`snapshot::GatedRouter`]); a snapshot holds it
    /// closed across checkpoint + commit so the on-disk state it
    /// records is quiesced.
    pub write_gate: snapshot::WriteGate,
    /// Serializes snapshot/restore cycles (`try_lock` → `Busy`).
    pub snapshot_cycle: Arc<tokio::sync::Mutex<()>>,
    /// Last (or in-flight) async snapshot's status — polled via
    /// `GET /server/snapshot/status` after a `POST /server/snapshot?wait=0`
    /// kick-off. The synchronous trigger doesn't touch it.
    pub snapshot_status: Arc<std::sync::RwLock<snapshot::SnapshotStatus>>,
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

    /// Serve the **server-management** router (`/server/vox` —
    /// `OrgManagementService` + `SnapshotService`) over an in-process
    /// vox link, the server-level counterpart of [`Self::local_server`].
    /// No per-org slug: this is the transport `task org create/list`
    /// and `task admin *` speak. Embedded restores keep the process
    /// alive (`SnapshotImpl::new_without_exit`) — the CLI is ephemeral
    /// and exits after the verb anyway.
    #[must_use]
    pub fn server_local_server(
        &self,
        scope: &std::sync::Arc<architect::Scope>,
    ) -> architect::LocalServer {
        architect::LocalServer::serve(
            server_layer_router(self, true),
            std::sync::Arc::clone(scope),
        )
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
            let auth = AuthState::open(&auth_db_url, &auth_secret()).await?;
            let org_state = build_org_state(auth, &keypair, org_root, &scope).await?;
            orgs.insert(slug, org_state);
        }

        let state = Self {
            keypair,
            orgs: Arc::new(std::sync::RwLock::new(orgs)),
            data_root,
            scope,
            write_gate: snapshot::WriteGate::new(),
            snapshot_cycle: Arc::new(tokio::sync::Mutex::new(())),
            snapshot_status: Arc::new(std::sync::RwLock::new(snapshot::SnapshotStatus::default())),
        };
        // Background forge-sync: pull codeberg/Forgejo issue changes
        // back into linked tasks on an interval (outbound push is
        // handled inline by the `ForgeSyncTaskService` decorator).
        forge_sync::spawn_poll_loop(state.clone());
        Ok(state)
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
            write_gate: snapshot::WriteGate::new(),
            snapshot_cycle: Arc::new(tokio::sync::Mutex::new(())),
            snapshot_status: Arc::new(std::sync::RwLock::new(snapshot::SnapshotStatus::default())),
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
            write_gate: snapshot::WriteGate::new(),
            snapshot_cycle: Arc::new(tokio::sync::Mutex::new(())),
            snapshot_status: Arc::new(std::sync::RwLock::new(snapshot::SnapshotStatus::default())),
        })
    }
}

/// Put a sqlite pool into WAL journal mode. WAL is what makes the
/// server-native snapshot story work: writers append to the `-wal`
/// sidecar (excluded from snapshots) while the main `.sqlite` file
/// stays stable + consistent on disk, so `PRAGMA wal_checkpoint
/// (TRUNCATE)` followed by `git add` captures a complete database.
/// The mode is persistent (recorded in the db file), so script-era
/// DELETE-mode databases are upgraded on first boot. Best-effort:
/// sqlx leaves the journal mode untouched by default, and a failure
/// here only degrades snapshot consistency back to the old
/// best-effort behavior.
async fn enable_wal(db: &sea_orm::DatabaseConnection, label: &str) {
    use sea_orm::ConnectionTrait as _;
    if let Err(e) = db.execute_unprepared("PRAGMA journal_mode=WAL;").await {
        tracing::warn!(db = label, error = %e, "could not enable WAL journal mode");
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
            enable_wal(&db, label).await;
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
        // Per-file CRDT collaboration over the same backend. Doc
        // persistence (snapshot + update log, one dir per doc id)
        // lives at `<org>/crdt/` — file-per-doc fits the plain-text
        // ethos; `crdt-seaorm` is the drop-in alternative if the org
        // dirs ever move into a database. The inbound listener folds
        // every `VaultEvent::Put` (non-CRDT `put_file` callers AND
        // the watcher below) into whichever per-file docs are open.
        let crdt_root = std::env::var("TASK_SERVER_CRDT_ROOT")
            .map_or_else(|_| org_root.path().join("crdt"), PathBuf::from);
        let vault_collab = vault_collab::VaultCollab::new(vault_sync_state.clone(), crdt_root);
        vault_collab.watch_vault("default");
        // External disk edits (vim, Obsidian, git) → VaultEvents.
        // Best-effort: a vault on a filesystem without notify support
        // still serves wire traffic, just without live disk pickup.
        let vault_watcher = match vault_sync_state.start_watcher("default").await {
            Ok(handle) => Some(Arc::new(handle)),
            Err(e) => {
                tracing::warn!(org = %org_root.slug(), "vault watcher not attached: {e}");
                None
            }
        };
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

        // Codex agent backend. In-process, in-memory session
        // registry + turn dispatch — hosts the `Sessions` +
        // `TurnDispatch` vox services behind the `/agents` UI.
        let agent_codex = agent_codex::CodexBackend::new();

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

        // Threads — conversations/topics anchored to tasks/projects.
        // SeaORM-backed (DB swappable); migrations run on open. Override
        // via `TASK_SERVER_THREADS_URL`.
        let threads_url = std::env::var("TASK_SERVER_THREADS_URL")
            .unwrap_or_else(|_| format!("sqlite://{}?mode=rwc", org_root.threads_db().display()));
        let threads_conn = open_sqlite_pool(scope, threads_url, "threads", |db| {
            Box::pin(async move { threads::Migrator::up(&db, None).await.map(|()| db) })
        })
        .await?;
        let threads = threads::Store::new(threads_conn);

        // Per-user preferences. SQLite at
        // `<data_root>/orgs/<slug>/prefs.sqlite` (override via
        // `TASK_SERVER_PREFS_URL`); migrations run on open.
        let prefs_url = std::env::var("TASK_SERVER_PREFS_URL")
            .unwrap_or_else(|_| format!("sqlite://{}?mode=rwc", org_root.prefs_db().display()));
        let prefs_conn = open_sqlite_pool(scope, prefs_url, "prefs", |db| {
            Box::pin(async move { prefs::Migrator::up(&db, None).await.map(|()| db) })
        })
        .await?;
        let prefs = prefs::Store::new(prefs_conn);

        // Identity locker — only the **home** org anchors it. Opened
        // at `<org>/identity.sqlite` (per `OrgRoot::identity_db`);
        // `is_home` comes from the on-disk manifest (same source
        // `AppState::home_slug` reads). Tokens are (de)crypted with the
        // shared AEAD secret. Federated orgs get `None`.
        let is_home = org_root.manifest().map(|m| m.is_home).unwrap_or(false);
        let identity = if is_home {
            let identity_url = format!("sqlite://{}?mode=rwc", org_root.identity_db().display());
            let identity_conn = open_sqlite_pool(scope, identity_url, "identity", |db| {
                Box::pin(async move { identity::Migrator::up(&db, None).await.map(|()| db) })
            })
            .await?;
            Some(identity::Store::new(identity_conn, auth_secret()))
        } else {
            None
        };

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

        // Recall backend rooted at the same vault — learning cards
        // live under `Records/recall/`.
        let recall = recall::VaultRecall::new(vault_root.clone())
            .map_err(|e| eyre::eyre!("recall backend: {e}"))?;

        // Contacts backend rooted at the same vault — people live
        // under `Records/contacts/`.
        let contacts = contacts::VaultContacts::new(vault_root.clone())
            .map_err(|e| eyre::eyre!("contacts backend: {e}"))?;

        // Tag registry rooted at the same vault — `Records/tags.json`.
        let tags =
            tag::VaultTags::new(vault_root.clone()).map_err(|e| eyre::eyre!("tag backend: {e}"))?;

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

        // Ledger service — double-entry journal over the same
        // finance.sqlite connection. Shared with the invoicing
        // backend so mark-sent / payment post into it.
        let ledger_backend = finance::LedgerService::new(finance_conn.clone())
            .map_err(|e| eyre::eyre!("ledger backend: {e}"))?;

        // Invoicing service — persists invoices in finance.sqlite and
        // marks billed sessions in the timer DB, so it needs both.
        // It also posts double-entry journal entries to the ledger on
        // mark-sent + payment, so it gets a clone of the ledger.
        let finance_backend = finance::FinanceBackend::new(
            finance_conn.clone(),
            timer.conn().clone(),
            org_root
                .manifest()
                .map_or_else(|_| "Business".into(), |m| m.display_name),
            ledger_backend.clone(),
        )
        .map_err(|e| eyre::eyre!("finance backend: {e}"))?;

        // Email backend — Maildir-backed `EmailSync`. The mail
        // root lives at `<org>/vault/Mail/` (override via
        // `TASK_SERVER_MAIL_ROOT`); each top-level subdir there
        // is one account (its dir name is the account id). No
        // IMAP creds are wired in this slice, so an org with no
        // `Mail/` tree just serves an empty account list — the
        // `/email` UI tolerates that. Each discovered account
        // maps to a Maildir++ root; `Backend::with_accounts`
        // creates the `cur/new/tmp` dirs on demand.
        let mail_root = std::env::var("TASK_SERVER_MAIL_ROOT")
            .map_or_else(|_| vault_root.join("Mail"), PathBuf::from);
        let email = email_maildir::Backend::with_accounts(discover_mail_accounts(&mail_root));

        // Forge backend — Forgejo, the org's primary forge. Base
        // URL + token come from the same env vars the CLI's forge
        // sync uses (`TASK_FORGEJO_BASE_URL` / `TASK_FORGEJO_TOKEN`,
        // falling back to `FORGEJO_TOKEN`). Both are optional: when
        // unset we build with empty strings so startup never fails on
        // a missing credential — the forge methods then return an
        // auth/forge `GitError` the /repos UI renders as an empty
        // list. `from_token` only errors when called outside a tokio
        // runtime, which `build_org_state` always is.
        let forgejo_base = std::env::var("TASK_FORGEJO_BASE_URL").unwrap_or_default();
        let forgejo_token = std::env::var("TASK_FORGEJO_TOKEN")
            .ok()
            .filter(|t| !t.is_empty())
            .or_else(|| std::env::var("FORGEJO_TOKEN").ok())
            .unwrap_or_default();
        let forge = git_forgejo::Backend::from_token(forgejo_base.clone(), forgejo_token)
            .map_err(|e| eyre::eyre!("forge backend: {e}"))?;
        // Agent/bot identity for forge-sync attribution. Token from
        // `TASK_FORGEJO_BOT_TOKEN`, or `FTS_CODEBERG_ACCESS_TOKEN`
        // (the var name the sops-rendered `fts-codeberg.env` carries,
        // so a service `EnvironmentFile=` works without remapping).
        // When neither is set we reuse the human backend, so
        // agent-owned tasks still sync — just under the human
        // identity until the bot token is configured.
        let forge_agent = match std::env::var("TASK_FORGEJO_BOT_TOKEN")
            .ok()
            .or_else(|| std::env::var("FTS_CODEBERG_ACCESS_TOKEN").ok())
            .filter(|t| !t.is_empty())
        {
            Some(bot_token) => git_forgejo::Backend::from_token(forgejo_base, bot_token)
                .map_err(|e| eyre::eyre!("forge agent backend: {e}"))?,
            None => forge.clone(),
        };

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
        let workstreams = workstream::WorkstreamBackend::new(vault_root.clone());
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
        // Inventory — `type: item` gear/equipment pages. Its own
        // `vault::Vault` snapshot behind an `Arc<Mutex<…>>`, like
        // locations.
        let inventory_vault = vault::Vault::open(&vault_root)
            .map_err(|e| eyre::eyre!("open inventory vault: {e}"))?;
        let inventory = inventory::Store::new(inventory_vault);
        // Scripture — read-only Bible spine loaded from the resource
        // library (`<org>/resources/bible/<TX>/`). A missing root yields
        // an empty store, so orgs without an installed corpus just show
        // no translations.
        // Copyright-restricted editions, fetched live with the user's key
        // (never bundled). ESV needs a Crossway key; NIV rides API.Bible
        // and additionally needs that edition's `bible_id` (NIV is tightly
        // licensed — only works if the key has NIV access).
        let mut scripture_api = Vec::new();
        if let Ok(key) = std::env::var("TASK_ESV_API_KEY").or_else(|_| std::env::var("ESV_API_KEY"))
        {
            if !key.is_empty() {
                scripture_api.push(scripture::ApiTranslation::esv(key));
            }
        }
        if let (Ok(key), Ok(bible_id)) = (
            std::env::var("TASK_API_BIBLE_KEY").or_else(|_| std::env::var("API_BIBLE_KEY")),
            std::env::var("TASK_API_BIBLE_NIV_ID"),
        ) {
            if !key.is_empty() && !bible_id.is_empty() {
                scripture_api.push(scripture::ApiTranslation::api_bible(
                    "NIV",
                    "New International Version",
                    bible_id,
                    key,
                ));
            }
        }
        // Strong's lexicon for word study (`<org>/resources/lexicon/strongs/`);
        // empty if not installed.
        let scripture_lexicon =
            scripture::Lexicon::load_dir(&org_root.resources_dir().join("lexicon").join("strongs"))
                .map_err(|e| eyre::eyre!("load lexicon: {e}"))?;
        let scripture =
            scripture::Store::load_resource_root(&org_root.resources_dir().join("bible"))
                .map_err(|e| eyre::eyre!("load scripture: {e}"))?
                // The vault powers per-verse backlinks: notes that link
                // `[[John 3:16]]` surface in the reader.
                .with_vault(vault_root.clone())
                .with_api(scripture_api)
                .with_lexicon(scripture_lexicon)
                // Original-language editions (TAGNT/TAHOT/SBLGNT/OSHB),
                // loaded lazily per edition on first interlinear request.
                .with_originals_root(org_root.resources_dir().join("original"))
                // Versification mappings reconcile Hebrew vs English
                // verse numbering for the interlinear.
                .with_versification(
                    scripture::Versification::load_dir(
                        &org_root.resources_dir().join("versification"),
                    )
                    .map_err(|e| eyre::eyre!("load versification: {e}"))?,
                )
                // OpenBible cross-references + topical tags (CC BY,
                // vote-weighted), lazy-loaded on first query.
                .with_crossref(
                    org_root
                        .resources_dir()
                        .join("crossref")
                        .join("cross_references.txt"),
                )
                .with_topics(
                    org_root
                        .resources_dir()
                        .join("topics")
                        .join("topic-votes.txt"),
                );
        // Typed-link store (user-asserted verse/note/wiki links).
        let links = links::Store::open(org_root.path().join("links.jsonl"));
        // Ordered-collection store — Library / Setlist / Show / Playlist.
        // JSONL at `<org>/collections.jsonl` (override via
        // `TASK_SERVER_COLLECTIONS_PATH`, mirroring the vault-root override
        // so tests can isolate it). A missing file is an empty store.
        let collections_path = std::env::var("TASK_SERVER_COLLECTIONS_PATH")
            .map_or_else(|_| org_root.path().join("collections.jsonl"), PathBuf::from);
        let collections = collection::Store::open(collections_path);
        // Link-graph reader over the same `"default"` vault root
        // the sync backend serves — read-only, so no dir creation.
        let vault_graph = vault::GraphBackend::single("default", vault_root.clone());
        // Keep `note → verse` + `note → note` links live as notes are
        // saved: a background task syncs each changed note's
        // `[[wikilinks]]` into the store.
        crate::link_sync::spawn(
            links.clone(),
            vault_root.clone(),
            vault_graph.clone(),
            vault_sync_state.channel("default").await.subscribe(),
        );
        // Resource Library reader (transcript sidecars under resources/).
        let resources = resources::ResourcesBackend::new(org_root.resources_dir());
        // Cookbook lives at `<wiki_root>/Cookbook/*.cook` —
        // typically `<org>/wiki/Knowledge/Cookbook/`, NOT the
        // vault root. Match the wiki backend's anchor.
        let cookbook = cookbook::Store::new(wiki_root.clone());
        let mealplan_vault =
            vault::Vault::open(&vault_root).map_err(|e| eyre::eyre!("open mealplan vault: {e}"))?;
        // `with_cookbook`: meals live in the vault, but their
        // recipe paths resolve against the wiki-rooted
        // cookbook above — without it `can_cook` / cook
        // deductions look for `.cook` files under the vault
        // root and never find them.
        let mealplan = mealplan::Store::new(mealplan_vault).with_cookbook(cookbook.clone());
        // Shopping list + substitution-rule services — sibling
        // mealplan stores, each its own vault snapshot.
        let shopping_vault =
            vault::Vault::open(&vault_root).map_err(|e| eyre::eyre!("open shopping vault: {e}"))?;
        let shopping =
            mealplan::shopping::Store::new(shopping_vault).with_cookbook(cookbook.clone());
        let substitutions_vault = vault::Vault::open(&vault_root)
            .map_err(|e| eyre::eyre!("open substitutions vault: {e}"))?;
        let substitutions = mealplan::substitutions::Store::new(substitutions_vault);
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

        // Every open sqlite pool, for the snapshot engine's
        // wal_checkpoint pass. Keep in lockstep with the pools
        // opened above — a missing entry only costs checkpoint
        // coverage for that db, never correctness of live serving.
        let mut sqlite_conns = vec![
            auth.db.clone(),
            agent_tasks.conn().clone(),
            timer.conn().clone(),
            threads.conn().clone(),
            prefs.conn().clone(),
            finance_conn.clone(),
        ];
        // Identity locker only exists on the home org; include its pool
        // in the snapshot checkpoint set when it was opened.
        if let Some(store) = &identity {
            sqlite_conns.push(store.conn().clone());
        }

        Ok(OrgAppState {
            slug: org_root.slug().to_owned(),
            auth,
            attachments: attachment_service,
            vault_sync: vault_sync_state,
            vault_collab,
            vault_watcher,
            scripture,
            links,
            collections,
            resources,
            wiki,
            projects,
            goals,
            milestones,
            workstreams,
            tasks,
            locations,
            inventory,
            cookbook,
            mealplan,
            shopping,
            substitutions,
            pantry,
            body,
            exercises,
            workouts,
            intake,
            agent_tasks,
            agent_codex,
            agent_dispatch_vault_root: vault_root,
            timer,
            threads,
            prefs,
            identity,
            scheduling,
            inbox,
            recall,
            contacts,
            tags,
            finance_conn,
            finance_backend,
            ledger_backend,
            email,
            forge,
            forge_agent,
            issue_links_path: org_root.path().join("issue-links.json"),
            presence: crdt::sync::PresenceHost::new(
                presence::PRESENCE_DOC_ID,
                presence::PRESENCE_TIMEOUT_MS,
            ),
            vault_graph,
            sqlite_conns,
        })
    }
}

/// Discover Maildir accounts under `mail_root`. Each immediate
/// subdirectory is one account: its dir name is the account id +
/// display name, and the address defaults to the same (the
/// IMAP/JMAP config that would carry a real address isn't wired
/// in this slice). Returns the `(Account, root, aliases)` tuples
/// `email_maildir::Backend::with_accounts` consumes. An absent
/// or empty `mail_root` yields an empty vec — the backend then
/// serves no accounts, which is a valid "operational but
/// unconfigured" state.
fn discover_mail_accounts(
    mail_root: &std::path::Path,
) -> Vec<(email_proto::Account, PathBuf, email_config::FolderAliases)> {
    let Ok(entries) = std::fs::read_dir(mail_root) else {
        return Vec::new();
    };
    let mut accounts = Vec::new();
    for entry in entries.flatten() {
        let path = entry.path();
        if !path.is_dir() {
            continue;
        }
        let Some(name) = path.file_name().and_then(|s| s.to_str()) else {
            continue;
        };
        let account = email_proto::Account {
            id: email_proto::AccountId(name.to_owned()),
            name: name.to_owned(),
            address: name.to_owned(),
            display_name: None,
        };
        accounts.push((account, path, email_config::FolderAliases::new()));
    }
    accounts
}

/// Dev default — replace via config in a later phase. Length-checked
/// at build time so this fails loudly if shortened.
const DEFAULT_AUTH_SECRET: &str = "task-server-auth-dev-secret-32+!";

/// The secret that signs every org's session tokens. A real deployment
/// MUST set `TASK_AUTH_SECRET` (a high-entropy 32+ char value) — the
/// hardcoded [`DEFAULT_AUTH_SECRET`] is a dev convenience and makes
/// tokens forgeable. Falls back to it (with a warning) when unset.
pub(crate) fn auth_secret() -> String {
    match std::env::var("TASK_AUTH_SECRET") {
        Ok(s) if !s.is_empty() => s,
        _ => {
            tracing::warn!(
                "TASK_AUTH_SECRET unset — using the dev auth secret (tokens are forgeable)"
            );
            DEFAULT_AUTH_SECRET.to_owned()
        }
    }
}

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

    // Server-management vox: `OrgManagementService` +
    // `SnapshotService` mounted on a top-level endpoint (not
    // per-org). Lets a CLI connect once and ask the server to
    // scaffold new orgs / run data snapshots without touching the
    // data root locally. `POST /server/snapshot` is the HTTP
    // trigger for the chart's backup CronJob (Bearer
    // `TASK_BACKUP_GIT_TOKEN`).
    let server_mgmt = Router::new()
        .route("/server/vox", any(server_vox_handler))
        .route(
            "/server/snapshot",
            axum::routing::post(snapshot::http_snapshot_handler),
        )
        .route(
            "/server/snapshot/status",
            get(snapshot::http_snapshot_status_handler),
        )
        .with_state(state.clone());

    Router::new()
        .route("/health", get(|| async { "ok" }))
        .route("/vox", get(legacy_vox_handler))
        .merge(well_known)
        .merge(per_org)
        .merge(server_mgmt)
        .merge(watch_bridge::watch_router())
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
    // Schema stamps — the proto/server skew guard. Clients
    // (`task doctor`, ui-lab smoke) compare these against their
    // own build; see `schema_stamps`.
    let stamps: serde_json::Map<String, serde_json::Value> = schema_stamps()
        .into_iter()
        .map(|(name, stamp)| (name.to_owned(), serde_json::Value::String(stamp)))
        .collect();
    axum::Json(serde_json::json!({
        "version": 1,
        // Git rev this binary was built from (baked into the container
        // image env by the flake; "unknown" outside that path). CI's
        // verify-live step polls this until it matches the pushed sha —
        // a green run means the deployment is actually serving it.
        "build": std::env::var("TASK_BUILD_REV").unwrap_or_else(|_| "unknown".to_owned()),
        "orgs": orgs,
        "schema_stamps": stamps,
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
    serve_org_vox(org, state.write_gate.clone(), ws)
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
    let gate = state.write_gate.clone();
    let router = server_layer_router(&state, false);
    // Gated like the per-org endpoints: `create_org` writes to the
    // data root, so it must quiesce during a snapshot too. The
    // snapshot verbs themselves pass the entry gate before closing
    // it — no self-deadlock.
    let router = crate::snapshot::GatedRouter::new(router, gate);
    ws.on_upgrade(move |socket| architect::axum_ws::serve_router(socket, router))
        .into_response()
}

/// Build the server-management [`LayerRouter`] (`OrgManagementService`
/// and `SnapshotService`) — the `/server/vox` service set. Shared by the
/// WebSocket handler above (which additionally wraps it in the
/// snapshot [`GatedRouter`](crate::snapshot::GatedRouter)) and the
/// in-process transport ([`AppState::server_local_server`]).
///
/// `local_trusted`: false for the network-facing WebSocket (session
/// auth enforced, restore exits so the supervisor restarts on the
/// restored data); true for the in-process transport (the caller
/// already owns the data root — no session check, no exit-on-restore
/// since the embedded CLI process is ephemeral).
#[must_use]
pub fn server_layer_router(state: &AppState, local_trusted: bool) -> architect::LayerRouter {
    let (mgmt, snap, identity) = if local_trusted {
        (
            crate::server_mgmt::OrgManagementImpl::new_local_trusted(state.clone()),
            crate::snapshot::SnapshotImpl::new_local_trusted(state.clone()),
            crate::identity_mgmt::IdentityServiceImpl::new_local_trusted(state.clone()),
        )
    } else {
        (
            crate::server_mgmt::OrgManagementImpl::new(state.clone()),
            crate::snapshot::SnapshotImpl::new(state.clone()),
            crate::identity_mgmt::IdentityServiceImpl::new(state.clone()),
        )
    };
    architect::LayerRouter::new()
        .with(
            org_proto::org_management_descriptor(),
            org_proto::serve_org_management(mgmt),
        )
        .with(
            org_proto::snapshot_descriptor(),
            org_proto::serve_snapshot(snap),
        )
        .with(
            identity_proto::identity_descriptor(),
            identity_proto::serve_identity(identity),
        )
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
    serve_org_vox(org, state.write_gate.clone(), ws)
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
/// Schema stamps for every vox service [`org_layer_router`]
/// mounts — the dev guard against proto/server skew (served in
/// `/.well-known/task-server.json` as `schema_stamps`). A vox
/// method id hashes the method's name + payload shapes, so a
/// stamp diff between a client's build and the *running* server
/// binary means one of them predates a `*-proto` change — the
/// "structural mismatch / InvalidPayload out of nowhere" failure
/// mode. `task doctor` (which links this very function through
/// the task-server crate, so the two lists can't drift) and the
/// ui-lab smoke compare against this map and say "rebuild
/// task-server" instead of letting the skew surface as decode
/// errors.
///
/// Keep in lockstep with [`org_layer_router`] below — a missing
/// entry only costs coverage for that service, never
/// correctness.
#[must_use]
pub fn schema_stamps() -> Vec<(&'static str, String)> {
    org_proto::schema_stamp::stamp_services([
        architect_auth::auth_service_service_descriptor(),
        attachments_proto::attachment_service_service_descriptor(),
        vault_proto::descriptor(),
        agent_proto::service::tasks::agent_task_queue_rpc_service_descriptor(),
        agent_proto::service::sessions::sessions_rpc_service_descriptor(),
        agent_proto::service::turn_dispatch::turn_dispatch_rpc_service_descriptor(),
        agent_proto::service::threads::threads_rpc_service_descriptor(),
        timer_proto::service::timer_service_rpc_service_descriptor(),
        threads::service::threads_service_rpc_service_descriptor(),
        prefs_proto::service::prefs_service_rpc_service_descriptor(),
        scheduling_proto::service::day_templates::day_templates_rpc_service_descriptor(),
        scheduling_proto::service::day_plans::day_plans_rpc_service_descriptor(),
        scheduling_proto::service::calendar_events::calendar_events_rpc_service_descriptor(),
        scheduling_proto::service::event_types::event_types_rpc_service_descriptor(),
        scheduling_proto::service::schedules::schedules_rpc_service_descriptor(),
        scheduling_proto::service::slots::slots_rpc_service_descriptor(),
        scheduling_proto::service::bookings::bookings_rpc_service_descriptor(),
        inbox_proto::service::inbox::inbox_rpc_service_descriptor(),
        recall_proto::service::recall::recall_rpc_service_descriptor(),
        contacts_proto::service::contacts::contacts_rpc_service_descriptor(),
        tag_proto::service::tags::tag_service_rpc_service_descriptor(),
        finance_proto::service::invoicing::invoicing_rpc_service_descriptor(),
        finance_proto::service::ledger::ledger_rpc_service_descriptor(),
        wiki_proto::service::schema::schema_rpc_service_descriptor(),
        wiki_proto::service::catalog::catalog_rpc_service_descriptor(),
        wiki_proto::service::raw_layer::raw_layer_rpc_service_descriptor(),
        wiki_proto::service::graph::graph_rpc_service_descriptor(),
        wiki_proto::service::pages::pages_rpc_service_descriptor(),
        wiki_proto::service::ingest::ingest_rpc_service_descriptor(),
        wiki_proto::service::lint::lint_rpc_service_descriptor(),
        wiki_proto::service::search::search_rpc_service_descriptor(),
        wiki_proto::service::watcher::watcher_rpc_service_descriptor(),
        wiki_proto::service::multimodal::multimodal_rpc_service_descriptor(),
        wiki_proto::service::review::review_rpc_service_descriptor(),
        project::project_service_descriptor(),
        goal::goal_service_descriptor(),
        milestone::milestone_service_descriptor(),
        workstream::workstream_service_descriptor(),
        workstream::workstream_stream_descriptor(),
        task::task_service_descriptor(),
        task::task_stream_descriptor(),
        locations::locations_service_descriptor(),
        inventory::inventory_service_descriptor(),
        scripture::scripture_service_descriptor(),
        links::links_service_descriptor(),
        collection::collection_service_descriptor(),
        resources_proto::resources_service_rpc_service_descriptor(),
        cookbook::cookbook_service_descriptor(),
        mealplan::mealplan_service_descriptor(),
        pantry::pantry_service_descriptor(),
        mealplan::shopping::shopping_service_rpc_service_descriptor(),
        mealplan::substitutions::substitution_service_rpc_service_descriptor(),
        body::body_service_descriptor(),
        exercises::exercises_service_descriptor(),
        workouts::workouts_service_descriptor(),
        intake::intake_service_descriptor(),
        email_proto::descriptor(),
        git_proto::repo::repo_catalog_rpc_service_descriptor(),
        git_proto::issues::issue_tracker_rpc_service_descriptor(),
        git_proto::reviews::review_surface_rpc_service_descriptor(),
        git_proto::connections::repo_connections_rpc_service_descriptor(),
        crdt::sync::doc_sync_service_descriptor(),
        crdt::sync::doc_presence_service_descriptor(),
        vault_proto::vault_graph_rpc_service_descriptor(),
    ])
}

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
        // Media — the same blobs streamed over vox (Tx<MediaChunk>),
        // no HTTP side-channel. Read-side view over the attachment
        // store for the session player's stems + large media.
        .with(
            media_proto::media_service_service_descriptor(),
            media_proto::MediaServiceDispatcher::new(crate::media::MediaServiceImpl::new(
                org.attachments.clone(),
            )),
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
        // Agent sessions — conversation lifecycle (list / read /
        // create / rename / pin / archive). Backs the `/agents`
        // sidebar listing. Served by the in-process Codex backend.
        .with(
            agent_proto::service::sessions::sessions_rpc_service_descriptor(),
            agent_proto::service::sessions::serve(org.agent_codex.clone()),
        )
        // Agent turn dispatch — kick off / cancel / resume a turn
        // on a session. Served by the same Codex backend.
        .with(
            agent_proto::service::turn_dispatch::turn_dispatch_rpc_service_descriptor(),
            agent_proto::service::turn_dispatch::serve(org.agent_codex.clone()),
        )
        // Agent threads — conversation threading within a session.
        // Served by the same Codex backend (impls Threads).
        .with(
            agent_proto::service::threads::threads_rpc_service_descriptor(),
            agent_proto::service::threads::serve(org.agent_codex.clone()),
        )
        // Timer — billable time tracking.
        .with(
            timer_proto::service::timer_service_rpc_service_descriptor(),
            timer_proto::service::serve(org.timer.clone()),
        )
        .with(
            threads::service::threads_service_rpc_service_descriptor(),
            threads::service::serve(org.threads.clone()),
        )
        // Per-user preferences — get-with-defaults / upsert set.
        .with(
            prefs_proto::service::prefs_service_rpc_service_descriptor(),
            prefs_proto::service::serve(org.prefs.clone()),
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
        // Scheduling — booking half (Cal.com-style): event types,
        // availability schedules, open-slot listing, and bookings.
        // All four are served by the same `VaultScheduler`.
        .with(
            scheduling_proto::service::event_types::event_types_rpc_service_descriptor(),
            scheduling_proto::service::event_types::serve(org.scheduling.clone()),
        )
        .with(
            scheduling_proto::service::schedules::schedules_rpc_service_descriptor(),
            scheduling_proto::service::schedules::serve(org.scheduling.clone()),
        )
        .with(
            scheduling_proto::service::slots::slots_rpc_service_descriptor(),
            scheduling_proto::service::slots::serve(org.scheduling.clone()),
        )
        .with(
            scheduling_proto::service::bookings::bookings_rpc_service_descriptor(),
            scheduling_proto::service::bookings::serve(org.scheduling.clone()),
        )
        .with(
            inbox_proto::service::inbox::inbox_rpc_service_descriptor(),
            inbox_proto::service::inbox::serve(org.inbox.clone()),
        )
        .with(
            recall_proto::service::recall::recall_rpc_service_descriptor(),
            recall_proto::service::recall::serve(org.recall.clone()),
        )
        .with(
            contacts_proto::service::contacts::contacts_rpc_service_descriptor(),
            contacts_proto::service::contacts::serve(org.contacts.clone()),
        )
        .with(
            tag_proto::service::tags::tag_service_rpc_service_descriptor(),
            tag_proto::service::tags::serve(org.tags.clone()),
        )
        .with(
            finance_proto::service::invoicing::invoicing_rpc_service_descriptor(),
            finance_proto::service::invoicing::serve(org.finance_backend.clone()),
        )
        .with(
            finance_proto::service::ledger::ledger_rpc_service_descriptor(),
            finance_proto::service::ledger::serve(org.ledger_backend.clone()),
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
            wiki_proto::service::pages::pages_rpc_service_descriptor(),
            wiki_proto::service::pages::serve(wiki.clone()),
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
            workstream::workstream_service_descriptor(),
            workstream::serve_workstream_service(org.workstreams.clone()),
        )
        .with(
            task::task_service_descriptor(),
            task::serve_task_service(forge_sync::ForgeSyncTaskService::new(
                org.tasks.clone(),
                org.forge.clone(),
                org.forge_agent.clone(),
                org.slug.clone(),
                org.issue_links_path.clone(),
            )),
        )
        // Live task changes — the `#[subscribe]` stream sibling of
        // `TaskService`. The hub lives on the raw `TaskBackend`, so
        // every write path publishes into it: vox calls through the
        // forge-sync decorator above (it delegates to `org.tasks`),
        // CLI/agent mutations over this same router, and the forge
        // poll loop (it writes via `org.tasks.update`).
        .merge(task::task_service_stream_layer(org.tasks.clone()))
        // Live workstream changes — `WorkstreamService`'s
        // `#[subscribe]` stream sibling. The hub lives on the
        // `WorkstreamBackend` above; every CRUD path publishes
        // into it.
        .merge(workstream::workstream_service_stream_layer(
            org.workstreams.clone(),
        ));

    // Entity-CRUD services: locations + the mealplan trio.
    router = router
        .with(
            locations::locations_service_descriptor(),
            locations::serve_locations_service(org.locations.clone()),
        )
        .with(
            inventory::inventory_service_descriptor(),
            inventory::serve_inventory_service(org.inventory.clone()),
        )
        .with(
            scripture::scripture_service_descriptor(),
            scripture::serve_scripture_service(org.scripture.clone()),
        )
        .with(
            links::links_service_descriptor(),
            links::serve_links_service(org.links.clone()),
        )
        // Ordered collections — Library / Setlist / Show / Playlist.
        .with(
            collection::collection_service_descriptor(),
            collection::serve_collection_service(org.collections.clone()),
        )
        .with(
            resources_proto::resources_service_rpc_service_descriptor(),
            resources_proto::serve(org.resources.clone()),
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
        )
        .with(
            mealplan::shopping::shopping_service_rpc_service_descriptor(),
            mealplan::shopping::serve(org.shopping.clone()),
        )
        .with(
            mealplan::substitutions::substitution_service_rpc_service_descriptor(),
            mealplan::substitutions::serve(org.substitutions.clone()),
        );

    // Fitness suite — body / exercises / workouts / intake.
    router = router
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
        );

    // Email — `EmailSync` (accounts / folders / envelopes /
    // fetch / send / flag / subscribe), served by the per-org
    // Maildir backend.
    // Forge — RepoCatalog + IssueTracker + ReviewSurface, all
    // served by the org's single Forgejo `Backend`. The /repos UI
    // binds RepoCatalog (list repos) + IssueTracker (list issues
    // per repo); ReviewSurface rounds out the surface so PR views
    // can bind without another mount pass.
    router
        .with(
            email_proto::descriptor(),
            email_proto::serve(org.email.clone()),
        )
        .with(
            git_proto::repo::repo_catalog_rpc_service_descriptor(),
            git_proto::repo::serve(org.forge.clone()),
        )
        .with(
            git_proto::issues::issue_tracker_rpc_service_descriptor(),
            git_proto::issues::serve(org.forge.clone()),
        )
        .with(
            git_proto::reviews::review_surface_rpc_service_descriptor(),
            git_proto::reviews::serve(org.forge.clone()),
        )
        .with(
            git_proto::connections::repo_connections_rpc_service_descriptor(),
            git_proto::connections::serve(connections::ConnectionsBackend::new(
                org.issue_links_path.clone(),
            )),
        )
        // Per-file collaborative editing — the `DocSync` service over
        // the vault-collab `DocRegistry`: one mounted dispatcher
        // serves every vault-file doc (admission: ids registered via
        // `VaultSync::open_collab`), with the write-behind keeping
        // the plain files on disk authoritative for everyone else.
        .with(
            crdt::sync::doc_sync_service_descriptor(),
            crdt::sync::DocSyncDispatcher::new(org.vault_collab.registry().clone()),
        )
        // Presence — ONE mounted `DocPresence` service, routed by doc
        // id: the fixed `presence::PRESENCE_DOC_ID` reaches the
        // org-wide "who's online" host; any other id reaches the
        // vault-collab registry (per-file cursor channels). States
        // ride Loro's `EphemeralStore` and expire when a peer goes
        // quiet; nothing is persisted.
        .with(
            crdt::sync::doc_presence_service_descriptor(),
            crdt::sync::DocPresenceDispatcher::new(presence::PresenceRouter::new(
                org.presence.clone(),
                org.vault_collab.registry().clone(),
            )),
        )
        // Vault link-graph (backlinks / links / orphans / unresolved /
        // deadends / tags) — the read-only sibling of the vault sync
        // service mounted above, over the same per-org `"default"`
        // vault. Backs the vault page's backlinks panel + the
        // editor's tag-autocomplete candidates.
        .with(
            vault_proto::vault_graph_rpc_service_descriptor(),
            vault_proto::vault_graph_serve(org.vault_graph.clone()),
        )
}

fn serve_org_vox(
    org: OrgAppState,
    gate: snapshot::WriteGate,
    ws: WebSocketUpgrade,
) -> axum::response::Response {
    // Every request parks at the snapshot write gate on dispatch
    // entry — see `snapshot::GatedRouter`. Free when no snapshot is
    // running.
    let router = snapshot::GatedRouter::new(org_layer_router(&org), gate);
    ws.on_upgrade(move |socket| architect::axum_ws::serve_router(socket, router))
        .into_response()
}
