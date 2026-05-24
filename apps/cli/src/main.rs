//! `task` CLI — vertical-slice scaffold.
//!
//! After the Loro entity layer was ripped, the surface is:
//! - `task doctor` — print resolved vox endpoint URL.
//! - `task vault <cmd>` — filesystem-native vault queries +
//!   mutations (open / pages / tags / tasks / backlinks /
//!   outline / grep / property-{read,set,remove} / create /
//!   append / delete / move / base-query).
//!
//! Task / project commands (`list`, `set-done`, `new-task`,
//! `new-project`) went away with `project-crdt`. Rebuild them
//! against `vault::Vault` once the on-disk task convention is
//! pinned down (frontmatter shape, folder layout).
//!
//! Endpoint resolution for `task doctor` (first match wins):
//! 1. `--server <url>` flag.
//! 2. `TASK_VOX_URL` env var (loaded from `.env` if present).
//! 3. `ws://127.0.0.1:9090/vox` default.

mod org_ctx;
mod session_store;
mod shared;

use clap::{Parser, Subcommand};
use shared::RemoteVoxConfig;
use std::collections::HashMap;
#[derive(Parser)]
#[command(name = "task", about = "Task management CLI", version)]
struct Cli {
    /// Vox WebSocket URL (e.g. <ws://127.0.0.1:9090/vox>). Falls back
    /// to `TASK_VOX_URL` (loaded from .env) then to the localhost
    /// default.
    #[arg(
        long,
        env = "TASK_VOX_URL",
        default_value = "ws://127.0.0.1:9090/vox",
        global = true
    )]
    server: String,

    /// Architect Auth session token for remote vox.
    #[arg(long, env = "TASK_SESSION_TOKEN", global = true)]
    session_token: Option<String>,

    /// Organization id to route remote vox requests.
    #[arg(long, env = "TASK_ORGANIZATION_ID", global = true)]
    organization_id: Option<String>,

    /// Override the active org for this invocation only.
    /// Slug must match a dir under `<data_root>/orgs/`.
    /// Precedence: this flag > `session.json` active >
    /// single-org disambiguation > auto-bootstrap `default`.
    #[arg(long, global = true)]
    org: Option<String>,

    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Probe the configured vox endpoint.
    Doctor,
    /// FS-native vault queries — no server, no CRDT.
    Vault {
        #[command(subcommand)]
        cmd: VaultCmd,
    },
    /// First-party task management. Tasks are markdown pages
    /// with TaskNotes-shape frontmatter (mirrors
    /// callumalpass/tasknotes). Files live at
    /// `<vault>/tasks/<slug>.md` by default.
    #[command(subcommand)]
    Task(TaskCmd),
    /// LLM-agent integration. Codex backend drives `chat`
    /// (one-shot) + `wiki ingest` (two-step `CoT` against a
    /// vault's `Wiki/raw/sources/`).
    #[command(subcommand)]
    Agent(AgentCmd),
    /// `Wiki/` operations — currently the LLM-driven
    /// ingest pipeline. Sister surface to `agent`; the
    /// command itself routes through `agent-wiki::bridge`.
    #[command(subcommand)]
    Wiki(WikiCmd),
    /// Billable time tracking. Local SQLite backed (no
    /// server needed); same `timer::Store` the server
    /// mounts. Project lookup reads `Projects/*.md` for the
    /// rate cascade.
    #[command(subcommand)]
    Timer(TimerCmd),
    /// Finance — reports + invoice generation from billable
    /// sessions, PDF rendering via fulgur.
    #[command(subcommand)]
    Finance(FinanceCmd),
    /// Architect-auth flows — local sign-in, session
    /// management, org selection. Writes the persistent
    /// session file consumed by `timer` / `finance`.
    #[command(subcommand)]
    Auth(AuthCmd),
    /// Federated org-root layout — scaffold, list, and (later)
    /// export/claim on-disk org directories under the data
    /// root. Distinct from `auth org`, which is about
    /// membership in architect-auth orgs. See
    /// `plans/federated-task-platform.md` Phase 1.
    #[command(subcommand)]
    Org(OrgCmd),
    /// Per-machine project content mounts — register where each
    /// project's bytes live on this box. Reads/writes
    /// `$XDG_CONFIG_HOME/task/mounts.toml` (override via
    /// `TASK_MOUNTS_TOML`). See
    /// `plans/federated-task-platform.md` Phase 2.
    #[command(subcommand)]
    Mount(MountCmd),
    /// Cyclic life-calendar — show / list the 4-week cycles
    /// that anchor long-term planning. See
    /// `plans/cyclic-life-calendar.md`.
    #[command(subcommand)]
    Cycle(CycleCmd),
    /// Projects served by the active org. Talks to
    /// `/org/<slug>/vox` via the architect-generated
    /// `ProjectServiceClient`.
    #[command(subcommand)]
    Project(ProjectCmd),
    /// Goals (with cycle anchoring) served by the active
    /// org. Talks to `/org/<slug>/vox` via the architect-
    /// generated `GoalServiceClient`.
    #[command(subcommand)]
    Goal(GoalCmd),
    /// Project milestones — GitHub-Projects-style checkpoints.
    /// Tasks roll up via `milestoneId`; milestones can ladder
    /// up to life-goals via `goalId`. Designed to sync 1:1
    /// with Forgejo / GitHub milestones in the future.
    #[command(subcommand)]
    Milestone(MilestoneCmd),
    /// Physical places — studios, rooms, venues, storage.
    /// Pantry + inventory reference these by id.
    #[command(subcommand)]
    Location(LocationCmd),
    /// Cookbook recipes (cooklang `.cook` files under
    /// `Wiki/Cookbook/`).
    #[command(subcommand)]
    Recipe(RecipeCmd),
    /// Scheduled meals + cooking lifecycle (planned →
    /// cooked → pantry deductions).
    #[command(subcommand)]
    Meal(MealCmd),
    /// Pantry — stocked food items, qty + unit tracking,
    /// barcode resolution.
    #[command(subcommand)]
    Pantry(PantryCmd),
}

#[derive(Subcommand)]
enum LocationCmd {
    /// List every location in the active org's vault.
    List {
        #[arg(long)]
        kind: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    Get {
        target: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    Create {
        name: String,
        /// `studio|room|storage|venue|home|other`. Default
        /// `other`.
        #[arg(long)]
        kind: Option<String>,
        /// Parent location (id or path) for nested places.
        #[arg(long)]
        parent: Option<String>,
        #[arg(long)]
        address: Option<String>,
        #[arg(long, value_delimiter = ',')]
        tags: Vec<String>,
        #[arg(long)]
        details: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    Rename {
        target: String,
        new_path: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    Delete {
        target: String,
        #[arg(long, short = 'y')]
        yes: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
}

#[derive(Subcommand)]
enum RecipeCmd {
    /// List every recipe in the active org's cookbook.
    List {
        /// Substring filter on title.
        #[arg(long)]
        query: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    Get {
        /// Recipe path (e.g. `Wiki/Cookbook/Oatmeal.cook`).
        path: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    Delete {
        path: String,
        #[arg(long, short = 'y')]
        yes: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
}

#[derive(Subcommand)]
enum MealCmd {
    /// List meals. Filters compose (AND).
    List {
        /// Only meals scheduled on this date (`YYYY-MM-DD`).
        #[arg(long)]
        date: Option<String>,
        /// `planned|cooked|skipped|eating-out`.
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    Get {
        target: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    Create {
        name: String,
        /// `YYYY-MM-DD`. Required.
        #[arg(long)]
        date: String,
        /// `breakfast|lunch|dinner|snack`. Default `dinner`.
        #[arg(long)]
        slot: Option<String>,
        /// Recipe paths (repeatable or comma-separated).
        #[arg(long, value_delimiter = ',')]
        recipe: Vec<String>,
        #[arg(long, default_value_t = 1)]
        servings: u32,
        #[arg(long, value_delimiter = ',')]
        tags: Vec<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    SetStatus {
        target: String,
        status: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Mark cooked. Pantry deductions auto-computed from
    /// the recipe's `can_cook` check; pass `--no-deduct`
    /// to skip pantry adjustment (e.g. ate-out leftovers).
    Cook {
        target: String,
        #[arg(long)]
        no_deduct: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    Skip {
        target: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    Rename {
        target: String,
        new_path: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    Delete {
        target: String,
        #[arg(long, short = 'y')]
        yes: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
}

#[derive(Subcommand)]
enum PantryCmd {
    List {
        #[arg(long)]
        low_stock: bool,
        #[arg(long)]
        expired: bool,
        /// Only items expiring within N days (uses
        /// `best_before` stock entries).
        #[arg(long)]
        expiring_in: Option<i64>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    Get {
        target: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    Create {
        name: String,
        #[arg(long)]
        qty: Option<f64>,
        /// Unit slug (`g` / `ml` / `each` / `cup` / `clove`).
        #[arg(long)]
        unit: Option<String>,
        /// Location id or path.
        #[arg(long)]
        location: Option<String>,
        /// Free-form food category.
        #[arg(long)]
        food_category: Option<String>,
        #[arg(long, value_delimiter = ',')]
        tags: Vec<String>,
        #[arg(long)]
        details: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Decrement `qty` by `amount`.
    Consume {
        target: String,
        amount: f64,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Increment `qty` by `amount`.
    Restock {
        target: String,
        amount: f64,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Mark opened; stamps today onto `openedDate`.
    Open {
        target: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Find a pantry row by barcode. `--resolve` falls back
    /// to OpenFoodFacts if no local match.
    FindByBarcode {
        barcode: String,
        #[arg(long)]
        resolve: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    Rename {
        target: String,
        new_path: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    Delete {
        target: String,
        #[arg(long, short = 'y')]
        yes: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
}

#[derive(Subcommand)]
enum ProjectCmd {
    /// List every project the active org's vault carries.
    /// Output: one row per project with status + parent
    /// breadcrumb. Pass `--json` for machine-readable output.
    List {
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Emit JSON instead of the human-readable table.
        #[arg(long)]
        json: bool,
    },
    /// Fetch one project by id or by vault-relative path.
    /// Prints title + status + tags + the full details body.
    Get {
        /// Project UUID OR vault-relative path
        /// (`Projects/Health/Fitness/Fitness.md`).
        target: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Create a new project. Title is the only required
    /// argument; sensible defaults fill the rest. The backend
    /// chooses a `Projects/<slug>.md` path unless `--path`
    /// overrides it.
    Create {
        title: String,
        /// Vault-relative path. Default: `Projects/<slug>.md`.
        #[arg(long)]
        path: Option<String>,
        /// Parent project id OR vault-relative path. Resolved
        /// against `list()` before the create call so paths
        /// work too.
        #[arg(long)]
        parent: Option<String>,
        /// One of `active|on_hold|done|cancelled`. Default
        /// `active`.
        #[arg(long)]
        status: Option<String>,
        /// `p0..p4` / `urgent|high|normal|low|lowest`. Default
        /// `normal`.
        #[arg(long)]
        priority: Option<String>,
        /// Comma-separated tag list.
        #[arg(long, value_delimiter = ',')]
        tags: Vec<String>,
        /// Body / details (markdown). Reads stdin when `-`.
        #[arg(long)]
        details: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Set the project status. Convenience over `update`.
    SetStatus {
        target: String,
        status: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Set the project priority. Convenience over `update`.
    SetPriority {
        target: String,
        priority: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Set or clear the project parent. Pass `none` / `null`
    /// to unparent.
    SetParent {
        target: String,
        /// `none`, `null`, a project UUID, or a vault-relative
        /// path.
        parent: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Mark the project archived (kept on disk; timer refuses
    /// new sessions against it).
    Archive {
        target: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Reverse of `archive`.
    Unarchive {
        target: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Move the backing markdown file. Preserves `id` so
    /// downstream FKs (timer rows, links) survive.
    Rename {
        target: String,
        new_path: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Delete the project. Refuses if any other project lists
    /// it as parent — reparent or delete children first.
    Delete {
        target: String,
        /// Skip the y/N prompt.
        #[arg(long, short = 'y')]
        yes: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
}

#[derive(Subcommand)]
enum GoalCmd {
    /// List every goal the active org's vault carries.
    /// Output groups by lifetime root, shows the kind chip
    /// (lifetime / yearly / cycle / …) and cycle anchor when
    /// present.
    List {
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Only goals scoped to the current cycle (per
        /// `cycle::cycle_for_date(today)`).
        #[arg(long)]
        current_cycle: bool,
        #[arg(long)]
        json: bool,
    },
    /// Fetch one goal by id or by vault-relative path.
    Get {
        target: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Create a new goal. Title is the only required arg.
    Create {
        title: String,
        /// `lifetime|yearly|quarterly|cycle|weekly`. Default
        /// `lifetime` for top-level, `cycle` when `--cycle`
        /// (or `--cycle-current`) is set, else `lifetime`.
        #[arg(long)]
        kind: Option<String>,
        /// Status slug. Default `aspiration`.
        #[arg(long)]
        status: Option<String>,
        /// Vault-relative path. Default `Goals/<slug>.md`.
        #[arg(long)]
        path: Option<String>,
        /// Parent goal id or path.
        #[arg(long)]
        parent: Option<String>,
        /// ISO date `YYYY-MM-DD`. Required for `yearly` goals
        /// by convention but not enforced.
        #[arg(long)]
        target_date: Option<String>,
        /// Cycle UUID. Mutually exclusive with
        /// `--cycle-current`.
        #[arg(long)]
        cycle: Option<String>,
        /// Anchor to today's cycle.
        #[arg(long)]
        cycle_current: bool,
        #[arg(long, value_delimiter = ',')]
        tags: Vec<String>,
        #[arg(long)]
        details: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Set the goal status.
    SetStatus {
        target: String,
        status: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Set or clear the parent goal (`none` clears).
    SetParent {
        target: String,
        parent: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Anchor a goal to a specific cycle (by UUID, by
    /// `YYYY:Qn:Cm`, or `current` for today's cycle). Pass
    /// `none` / `null` to clear.
    SetCycle {
        target: String,
        cycle: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Move the backing markdown file. `id` is preserved.
    Rename {
        target: String,
        new_path: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Delete the goal. Refuses if any other goal lists it as
    /// parent.
    Delete {
        target: String,
        #[arg(long, short = 'y')]
        yes: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
}

#[derive(Subcommand)]
enum CycleCmd {
    /// Show the cycle that today's date sits inside. Prints
    /// year / quarter / cycle ordinal + cycle bounds + how
    /// far through it we are. Returns "(reset / bonus week)"
    /// when today is between cycles.
    Current,
    /// List every quarter + cycle for a given cyclic year.
    /// Defaults to the current calendar year.
    List {
        #[arg(long)]
        year: Option<i32>,
        /// Week-start day. Default: Monday.
        #[arg(long, default_value = "mon")]
        week_start: String,
    },
    /// Capture a reflection note for a cycle. Writes a
    /// templated page at
    /// `<org>/wiki/Knowledge/cycles/<year>-Q<q>-C<n>.md`.
    /// Idempotent: if the file exists, prints its path
    /// instead of overwriting.
    ///
    /// Defaults to today's cycle (or the previous one when
    /// today is inside a reset week). Override with
    /// `--year/--quarter/--cycle`.
    Reflect {
        #[arg(long)]
        year: Option<i32>,
        #[arg(long)]
        quarter: Option<u8>,
        #[arg(long)]
        cycle: Option<u8>,
    },
}

#[derive(Subcommand)]
enum MountCmd {
    /// Register a project's content path on this machine. By
    /// default the path is taken literally; pass `--under-vault`
    /// to resolve it against [`org_proto::default_client_vault_root`]
    /// (`$TASK_VAULT_ROOT` → `$HOME/Documents/Task`).
    Add {
        /// Project UUID (the federation-stable id).
        project_id: uuid::Uuid,
        /// Local path the project's content lives at.
        path: std::path::PathBuf,
        /// Resolve `path` against the client vault root instead
        /// of treating it as already absolute.
        #[arg(long)]
        under_vault: bool,
        /// Optional human-facing label.
        #[arg(long, default_value = "")]
        label: String,
        /// Overwrite an existing mount for this project.
        #[arg(long)]
        replace: bool,
    },
    /// Print every mount in the registry, sorted by project id.
    List,
    /// Remove the mount for a project. Idempotent — removing an
    /// unknown id is not an error.
    Rm { project_id: uuid::Uuid },
    /// Print the resolved path of `mounts.toml`. Useful for
    /// scripting + smoke tests.
    Path,
}

#[derive(Subcommand)]
enum OrgCmd {
    /// Ask the server to scaffold a new org. Connects to
    /// `<server>/server/vox` (`task-server` exposes the
    /// `OrgManagementService` RPC there) and the server
    /// writes the `<data_root>/orgs/<slug>/` dir + opens its
    /// per-org SQLite DBs + hot-adds it to the live
    /// dispatcher. No filesystem mutation runs on the client.
    ///
    /// Authorization: when the server has no orgs hosted yet
    /// it's in bootstrap mode and accepts this call
    /// unauthenticated. Otherwise the active session token
    /// must be a valid session against the server's home org.
    Create {
        /// `[a-z0-9-]`, 1-64 chars, no leading/trailing `-`.
        slug: String,
        /// Human-facing display name. Free-form UTF-8.
        #[arg(long)]
        name: String,
        /// Mark this org as the identity anchor (home).
        /// Only one home per server is allowed.
        #[arg(long)]
        home: bool,
        /// Server URL. Defaults to the active session's home
        /// server URL when set, else `http://127.0.0.1:18080`.
        #[arg(long)]
        server: Option<String>,
    },
    /// Local fallback: scaffold an org by writing directly to
    /// `<data-root>/orgs/<slug>/`. Bypasses the server — only
    /// useful when administering the server's filesystem
    /// out-of-band (recovery, migration). Prefer
    /// `task org create` for normal seeding.
    Init {
        /// `[a-z0-9-]`, 1-64 chars, no leading/trailing `-`.
        slug: String,
        /// Human-facing display name. Free-form UTF-8.
        #[arg(long)]
        name: String,
        /// Mark this org as the identity anchor (home).
        #[arg(long)]
        home: bool,
    },
    /// Ask the server to list its hosted orgs (the wire
    /// equivalent of `/.well-known/task-server.json`).
    /// Defaults to the active session's home server URL.
    List {
        #[arg(long)]
        server: Option<String>,
    },
}

#[derive(Subcommand)]
enum AuthCmd {
    /// Sign in against the local `auth.sqlite`. Persists a
    /// session token (+ `user_id`, `active_organization_id`)
    /// to `$XDG_DATA_HOME/task/session.json` so future
    /// commands no longer need `TASK_USER_ID` / `TASK_ORG_ID`.
    Login {
        #[arg(long)]
        email: String,
        #[arg(long)]
        password: String,
    },
    /// Create a new email/password user in the active org's
    /// `auth.sqlite` and persist the resulting session. The
    /// first user signed up in a fresh org is its de-facto
    /// owner — architect-auth has no separate ownership
    /// concept yet. Use `--org <slug>` to target a specific
    /// on-disk org without `task auth org use` first.
    Signup {
        #[arg(long)]
        email: String,
        #[arg(long)]
        password: String,
        /// Optional username — needed if you want
        /// `SignInUsername` to work later. Free-form, but the
        /// architect-auth username uniqueness constraint
        /// applies per `auth.sqlite`.
        #[arg(long)]
        username: Option<String>,
        /// Optional display name. Falls back to the email
        /// localpart in the UI when empty.
        #[arg(long)]
        name: Option<String>,
    },
    /// Print the active session (email, user id, org id).
    Whoami,
    /// Invalidate the active session server-side AND remove
    /// the local session file.
    Logout,
    /// Org membership + selection.
    #[command(subcommand)]
    Org(AuthOrgCmd),
}

#[derive(Subcommand)]
enum AuthOrgCmd {
    /// List orgs the signed-in user is a member of.
    List,
    /// Set the active org for subsequent commands. Updates
    /// both the local session file and the server-side
    /// `auth_session.active_organization_id`.
    Use { org_id: uuid::Uuid },
}

#[derive(Subcommand)]
enum WikiCmd {
    /// Build the 4-signal wiki graph and dump it as JSON or
    /// a terse text summary. No LLM — pure walk + compute.
    Graph {
        #[arg(short, long, default_value = "examples/vault")]
        vault: std::path::PathBuf,
        /// Filter by substring (matches title or path).
        #[arg(long, default_value = "")]
        query: String,
        /// Filter by `type:` frontmatter (`concept`,
        /// `entity`, ...).
        #[arg(long, default_value = "")]
        node_type: String,
        /// Cap on node count. `0` = no cap.
        #[arg(long, default_value_t = 0)]
        limit: u32,
        /// Emit JSON instead of the text summary.
        #[arg(long)]
        json: bool,
    },
    /// Surface knowledge gaps — orphan pages (degree ≤ 1)
    /// and missing-page wikilinks. No LLM.
    Gaps {
        #[arg(short, long, default_value = "examples/vault")]
        vault: std::path::PathBuf,
        #[arg(long)]
        json: bool,
    },
    /// Louvain communities — partition the wiki graph and
    /// print each cluster with its cohesion score.
    Clusters {
        #[arg(short, long, default_value = "examples/vault")]
        vault: std::path::PathBuf,
    },
    /// Token search over `Wiki/`. TF-IDF over page bodies;
    /// `--hybrid` opts into vector retrieval where the
    /// `vector` feature has been built in (else
    /// downgrades to token).
    Search {
        #[arg(short, long, default_value = "examples/vault")]
        vault: std::path::PathBuf,
        /// Query string.
        query: String,
        /// Filter by `type:` frontmatter.
        #[arg(long, default_value = "")]
        node_type: String,
        /// Result cap. `0` = unbounded.
        #[arg(long, default_value_t = 10)]
        top_k: u32,
        /// Include full page content in the response
        /// (skip for normal listings — the snippet usually
        /// suffices).
        #[arg(long)]
        include_content: bool,
        /// Use hybrid (token + vector) mode. Requires the
        /// `vector` feature build; otherwise downgrades
        /// transparently.
        #[arg(long)]
        hybrid: bool,
    },
    /// Watch `Wiki/raw/sources/` for FS events; on each
    /// debounced burst, rescan + (optionally) enqueue.
    /// Runs until interrupted.
    WatchSources {
        #[arg(short, long, default_value = "examples/vault")]
        vault: std::path::PathBuf,
        #[arg(long, default_value_t = 2)]
        debounce_secs: u64,
        /// Don't auto-enqueue diffs — just print them.
        #[arg(long)]
        dry_run: bool,
    },
    /// One-shot wiki health snapshot — queue depth, open
    /// findings, source count, last ingest/rescan.
    Health {
        #[arg(short, long, default_value = "examples/vault")]
        vault: std::path::PathBuf,
    },
    /// Recursively import a directory of files into
    /// `Wiki/raw/sources/`. Doesn't enqueue ingest tasks —
    /// follow with `task wiki rescan` to do that.
    Import {
        #[arg(short, long, default_value = "examples/vault")]
        vault: std::path::PathBuf,
        /// Directory to walk.
        #[arg(short, long)]
        dir: std::path::PathBuf,
        /// Flatten (drop subdirectory structure).
        #[arg(long)]
        flatten: bool,
        /// Extensions to include (comma-separated, no dot).
        /// Default: `md,txt,pdf`.
        #[arg(long, default_value = "md,txt,pdf")]
        ext: String,
    },
    /// Walk `Wiki/raw/sources/`, diff against the
    /// `snapshot.json`, and report new/modified/deleted
    /// files. Doesn't enqueue (yet) — pass `--enqueue` to
    /// also push diffs into the ingest queue.
    Rescan {
        #[arg(short, long, default_value = "examples/vault")]
        vault: std::path::PathBuf,
        #[arg(long)]
        enqueue: bool,
    },
    /// Run one semantic lint pass via the LLM. Persists
    /// new findings under `Wiki/_state/lint_findings.json`.
    Lint {
        #[arg(short, long, default_value = "examples/vault")]
        vault: std::path::PathBuf,
        #[arg(short, long)]
        model: Option<String>,
        #[arg(long, default_value_t = 180)]
        timeout_secs: u64,
        #[arg(long, default_value = "English")]
        language: String,
    },
    /// List open lint findings.
    Findings {
        #[arg(short, long, default_value = "examples/vault")]
        vault: std::path::PathBuf,
    },
    /// Layered-access wikilink lint. Scans
    /// `<org_root>/wiki/Knowledge/` + `wiki/LLM/` for
    /// wikilinks that escape their tier (Knowledge linking
    /// out, LLM linking into vault). Pure walk, no LLM.
    /// Exit code is non-zero when violations are present
    /// so the command works in pre-commit / CI.
    LintTiers {
        /// `<data_root>/orgs/<slug>/`. Defaults to the
        /// active session's org root.
        #[arg(long)]
        org_root: Option<std::path::PathBuf>,
    },
    /// Detect duplicate pages via the LLM. Prints groups;
    /// pass `--merge <slug-csv>` to merge one (writes via
    /// `record_pages`).
    Dedup {
        #[arg(short, long, default_value = "examples/vault")]
        vault: std::path::PathBuf,
        #[arg(short, long)]
        model: Option<String>,
        #[arg(long, default_value_t = 180)]
        timeout_secs: u64,
    },
    /// Propose a research plan for a knowledge gap (output
    /// of `task wiki gaps`).
    Research {
        #[arg(short, long, default_value = "examples/vault")]
        vault: std::path::PathBuf,
        /// Gap kind (`Orphan`, `MissingPage`, `SparseCluster`, `Bridge`).
        #[arg(long, default_value = "MissingPage")]
        gap_kind: String,
        /// Short gap title.
        #[arg(long)]
        gap_title: String,
        /// Gap description.
        #[arg(long, default_value = "")]
        gap_description: String,
        #[arg(short, long)]
        model: Option<String>,
        #[arg(long, default_value_t = 120)]
        timeout_secs: u64,
        #[arg(long, default_value = "English")]
        language: String,
    },
    /// Ingest one source file into `<vault>/Wiki/` via the
    /// two-step `CoT` pipeline (analyze → generate). Drops
    /// the source under `Wiki/raw/sources/`, runs the
    /// agent, parses FILE/REVIEW blocks, writes pages,
    /// updates `index.md` + `log.md`.
    ///
    /// Example:
    ///   task wiki ingest \
    ///     -v examples/vault \
    ///     -s examples/vault/Wiki/raw/sources/karpathy-llm-wiki.md \
    ///     -m gpt-5.4-mini
    Ingest {
        /// Vault root.
        #[arg(short, long, default_value = "examples/vault")]
        vault: std::path::PathBuf,
        /// Path to the source file to ingest. Bytes get
        /// copied into `Wiki/raw/sources/<filename>`.
        #[arg(short, long)]
        source: std::path::PathBuf,
        /// Override the filename used under `raw/sources/`.
        /// Default: source's basename.
        #[arg(long)]
        filename: Option<String>,
        /// MIME type. Default `text/markdown`.
        #[arg(long, default_value = "text/markdown")]
        mime: String,
        /// Human title for the log entry.
        #[arg(long)]
        title: Option<String>,
        /// Model id.
        #[arg(short, long)]
        model: Option<String>,
        /// Output language. Default `English`.
        #[arg(long, default_value = "English")]
        language: String,
        /// Per-turn timeout (seconds). Default 300.
        #[arg(long, default_value_t = 300)]
        timeout_secs: u64,
    },
}

#[derive(Subcommand)]
enum AgentCmd {
    /// One-shot chat against `codex app-server`. Spawns the
    /// daemon rooted at `--workspace`, sends `thread/start` +
    /// `turn/start`, prints streamed assistant text until the
    /// turn completes.
    ///
    /// Example:
    ///   task agent chat -w . -m gpt-5.4-mini "summarize this repo"
    Chat {
        /// Workspace root the agent runs in. Default: cwd.
        #[arg(short, long, default_value = ".")]
        workspace: std::path::PathBuf,
        /// Model id (e.g. `gpt-5.4-mini`, `o3`). Default:
        /// daemon's configured default.
        #[arg(short, long)]
        model: Option<String>,
        /// Reasoning effort hint
        /// (`none|minimal|low|medium|high`).
        #[arg(long)]
        effort: Option<String>,
        /// Sandbox / access mode
        /// (`read-only|current|full-access`). Default
        /// `current` (matches `CodexMonitor`).
        #[arg(long)]
        access_mode: Option<String>,
        /// Override `codex` binary path. Falls back to
        /// `$PATH` lookup.
        #[arg(long)]
        codex_bin: Option<String>,
        /// `$CODEX_HOME` override.
        #[arg(long)]
        codex_home: Option<std::path::PathBuf>,
        /// Max time to wait for the turn to complete
        /// (seconds). Default 120.
        #[arg(long, default_value_t = 120)]
        timeout_secs: u64,
        /// The user message. Quote it.
        message: String,
    },
}

#[derive(Subcommand)]
enum TaskCmd {
    /// Create a new task from a natural-language line.
    /// Extracts `#tag`s, `@context`s, `[[Project]]`s,
    /// `!priority`, and date keywords (`today`, `tomorrow`,
    /// `next monday`, `mon`, `YYYY-MM-DD`). Title = the
    /// remaining text. Pushes the result through the
    /// per-org RPC.
    Capture {
        text: String,
        /// Project id or vault-relative path. Sets
        /// `projectId` on the resulting task.
        #[arg(long)]
        project: Option<String>,
        /// Milestone id or path. Sets `milestoneId`. If both
        /// `--project` and `--milestone` are passed they must
        /// agree (CLI-side check).
        #[arg(long)]
        milestone: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// List tasks. Filters compose (AND).
    List {
        /// Status slug (`open`, `in-progress`, `done`, …).
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        tag: Option<String>,
        /// `@`-prefix optional.
        #[arg(long)]
        context: Option<String>,
        /// Restrict to one project (id or path).
        #[arg(long)]
        project: Option<String>,
        /// Restrict to one milestone (id or path). `none`
        /// lists tasks with no milestone.
        #[arg(long)]
        milestone: Option<String>,
        /// Only tasks whose status is not done.
        #[arg(long)]
        open: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Fetch one task by id or path.
    Get {
        target: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Create a task with explicit fields (no NLP parsing).
    /// Use `capture` for the conversational form.
    Create {
        title: String,
        #[arg(long)]
        path: Option<String>,
        #[arg(long)]
        status: Option<String>,
        #[arg(long)]
        priority: Option<String>,
        #[arg(long)]
        due: Option<String>,
        #[arg(long)]
        scheduled: Option<String>,
        #[arg(long, value_delimiter = ',')]
        tags: Vec<String>,
        #[arg(long, value_delimiter = ',')]
        contexts: Vec<String>,
        #[arg(long)]
        project: Option<String>,
        #[arg(long)]
        milestone: Option<String>,
        #[arg(long)]
        details: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Mark done. Sets `status: done` + `completedDate`.
    Done {
        target: String,
        /// Reopen instead (clears `completedDate`, status
        /// = `open`).
        #[arg(long)]
        undo: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    SetStatus {
        target: String,
        status: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    SetPriority {
        target: String,
        priority: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Set or clear (`none`) the due date.
    SetDue {
        target: String,
        due: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Set or clear (`none`) the scheduled date.
    SetScheduled {
        target: String,
        scheduled: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Set or clear (`none`) the owning project.
    SetProject {
        target: String,
        project: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Set or clear (`none`) the milestone link.
    SetMilestone {
        target: String,
        milestone: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Replace the tag list.
    SetTags {
        target: String,
        #[arg(value_delimiter = ',')]
        tags: Vec<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Move backing markdown file. `id` preserved.
    Rename {
        target: String,
        new_path: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    Delete {
        target: String,
        #[arg(long, short = 'y')]
        yes: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
}

#[derive(Subcommand)]
enum MilestoneCmd {
    /// List every milestone in the active org's vault.
    List {
        /// Restrict to one project (id or path).
        #[arg(long)]
        project: Option<String>,
        /// Restrict to one goal (id or path).
        #[arg(long)]
        goal: Option<String>,
        /// Only milestones whose status is not closed.
        #[arg(long)]
        open: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    Get {
        target: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Create a milestone. `--project` is required.
    Create {
        title: String,
        /// Project id or path. Required.
        #[arg(long)]
        project: String,
        /// Optional life-goal link (id or path).
        #[arg(long)]
        goal: Option<String>,
        #[arg(long)]
        path: Option<String>,
        /// `open` / `closed`. Default `open`.
        #[arg(long)]
        status: Option<String>,
        /// YYYY-MM-DD.
        #[arg(long)]
        due: Option<String>,
        #[arg(long, value_delimiter = ',')]
        tags: Vec<String>,
        /// External reference for future Forgejo / GitHub
        /// sync (e.g. `forgejo:starcommand.live/foo/bar#7`).
        #[arg(long)]
        forge_ref: Option<String>,
        #[arg(long)]
        details: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// `open` or `closed`. Convenience over `update`.
    SetStatus {
        target: String,
        status: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Set or clear (`none`) the due date.
    SetDue {
        target: String,
        due: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Set or clear (`none`) the life-goal link.
    SetGoal {
        target: String,
        goal: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Set or clear (`none`) the forge sync ref.
    SetForgeRef {
        target: String,
        forge_ref: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// `closed`. Just `set-status <target> closed`.
    Close {
        target: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Reopen (status = open).
    Reopen {
        target: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    Rename {
        target: String,
        new_path: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    Delete {
        target: String,
        #[arg(long, short = 'y')]
        yes: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
}

#[derive(Subcommand)]
enum VaultCmd {
    /// Open a vault and print a one-line summary.
    Open { path: std::path::PathBuf },
    /// List pages in a vault, optionally filtered by folder, tag, or
    /// `key=value` frontmatter match. Prints one vault-relative path
    /// per line so output is pipe-friendly.
    Pages {
        path: std::path::PathBuf,
        /// Restrict to pages whose folder equals (or starts with) this.
        #[arg(long)]
        folder: Option<String>,
        /// Restrict to pages tagged `<tag>` (frontmatter `tags` or
        /// inline `#tag`).
        #[arg(long)]
        tag: Option<String>,
        /// `key=value` frontmatter match. Value is parsed as JSON,
        /// falling back to a string. Repeatable; all must match.
        #[arg(long = "fm", value_name = "KEY=VAL")]
        fm: Vec<String>,
        /// Emit one JSON object per line instead of the path.
        #[arg(long)]
        json: bool,
    },
    /// List every tag in the vault with a count.
    Tags { path: std::path::PathBuf },
    /// List `.base` files and (if parsed cleanly) their view names.
    Bases { path: std::path::PathBuf },
    /// Print a single page's raw markdown.
    Cat {
        path: std::path::PathBuf,
        /// Vault-relative path of the page, e.g. `Music/Charts.md`.
        rel_path: String,
    },
    /// Substring search across page bodies (case-insensitive).
    /// Prints `path:line:content` like grep.
    Grep {
        path: std::path::PathBuf,
        pattern: String,
    },
    /// Pages that link TO the given page.
    Backlinks {
        path: std::path::PathBuf,
        rel_path: String,
    },
    /// Outgoing wikilinks from a page (resolved + raw target).
    Links {
        path: std::path::PathBuf,
        rel_path: String,
    },
    /// Pages with no incoming links.
    Orphans { path: std::path::PathBuf },
    /// Pages with no outgoing links.
    Deadends { path: std::path::PathBuf },
    /// Wikilink targets that don't resolve to any page.
    Unresolved { path: std::path::PathBuf },
    /// Heading outline of a single page.
    Outline {
        path: std::path::PathBuf,
        rel_path: String,
    },
    /// List distinct frontmatter property keys across the vault.
    Properties { path: std::path::PathBuf },
    /// Read one frontmatter property from a page.
    PropertyRead {
        path: std::path::PathBuf,
        rel_path: String,
        key: String,
    },
    /// Set a frontmatter property on a page (creates key if absent).
    PropertySet {
        path: std::path::PathBuf,
        rel_path: String,
        key: String,
        /// Value parsed as JSON; falls back to a string literal.
        value: String,
    },
    /// Remove a frontmatter property from a page (no-op if absent).
    PropertyRemove {
        path: std::path::PathBuf,
        rel_path: String,
        key: String,
    },
    /// All aliases declared via frontmatter, sorted.
    Aliases { path: std::path::PathBuf },
    /// All `- [ ]` task items across the vault. `path:line marker text`.
    Tasks { path: std::path::PathBuf },
    /// Word + character count for the vault (or one page when `--page` set).
    Wordcount {
        path: std::path::PathBuf,
        #[arg(long)]
        page: Option<String>,
    },
    /// Run all views in a `.base` file over the vault's pages.
    /// `--view <name>` runs only the matching view (case-insensitive).
    BaseQuery {
        path: std::path::PathBuf,
        /// Vault-relative path to the `.base` file.
        base: String,
        #[arg(long)]
        view: Option<String>,
    },
    /// Create a new `.md` page. Fails if the file already exists.
    Create {
        path: std::path::PathBuf,
        rel_path: String,
        /// Optional initial body. Reads stdin when omitted.
        #[arg(long)]
        body: Option<String>,
    },
    /// Append text to an existing page.
    Append {
        path: std::path::PathBuf,
        rel_path: String,
        text: String,
        /// No leading newline.
        #[arg(long)]
        inline: bool,
    },
    /// Prepend text immediately after the frontmatter (or at top
    /// when none).
    Prepend {
        path: std::path::PathBuf,
        rel_path: String,
        text: String,
        #[arg(long)]
        inline: bool,
    },
    /// Delete a page.
    Delete {
        path: std::path::PathBuf,
        rel_path: String,
    },
    /// Move / rename a page.
    Move {
        path: std::path::PathBuf,
        from: String,
        to: String,
    },
    /// Sync a local vault directory against the active org's
    /// vault on the server. Pulls remote-only files, pushes
    /// local-only files, and resolves conflicts via
    /// newer-mtime-wins. See `features/vault/vault-sync-client/`
    /// for the orchestrator.
    Sync {
        /// Local vault root. Defaults to the active org's
        /// `vault/` dir under the data root.
        #[arg(long)]
        local: Option<std::path::PathBuf>,
        /// Server URL. Falls back to `ws://127.0.0.1:18080`.
        #[arg(long)]
        server: Option<String>,
        /// Org slug to sync against. Defaults to the active
        /// org from the session.
        #[arg(long)]
        org: Option<String>,
        /// Remote vault id under that org. Server-side currently
        /// runs one vault per org keyed by `"default"`.
        #[arg(long, default_value = "default")]
        vault_id: String,
        /// Show the plan but don't apply it.
        #[arg(long)]
        dry_run: bool,
    },
    /// One-way pull — download every server-only file. Local
    /// files that already match the server are skipped; local
    /// files not on the server are left in place.
    Pull {
        #[arg(long)]
        local: Option<std::path::PathBuf>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long, default_value = "default")]
        vault_id: String,
        #[arg(long)]
        dry_run: bool,
    },
    /// One-way push — upload every local-only file. Remote
    /// files not present locally are left alone (no delete).
    Push {
        #[arg(long)]
        local: Option<std::path::PathBuf>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long, default_value = "default")]
        vault_id: String,
        #[arg(long)]
        dry_run: bool,
    },
}

#[derive(Subcommand)]
enum TimerCmd {
    /// Start the timer for the configured user. Fails if a
    /// session is already open.
    Start {
        /// Free-text description. Quoted to allow spaces.
        description: String,
        /// Project frontmatter id (uuid) the session is
        /// logged against. Empty = uncategorized.
        #[arg(long)]
        project: Option<uuid::Uuid>,
        /// Vault-relative path to the task note this
        /// session is for.
        #[arg(long, default_value = "")]
        task_note: String,
        /// Tag names to attach to the session. Tags are
        /// auto-created in the calling user's org if they
        /// don't already exist. Pass `--tag focus --tag review`
        /// to attach two.
        #[arg(long = "tag")]
        tags: Vec<String>,
    },
    /// Stop the current session. Snapshots `rate_cents` +
    /// `currency` via the rate cascade and writes the closed
    /// row.
    Stop,
    /// Show the active session, if any.
    Active,
    /// Atomic stop-then-start. Same args as `start`.
    Switch {
        description: String,
        #[arg(long)]
        project: Option<uuid::Uuid>,
        #[arg(long, default_value = "")]
        task_note: String,
        #[arg(long = "tag")]
        tags: Vec<String>,
    },
    /// Retro-log a past session: `--from` / `--to` ISO 8601
    /// timestamps + description. Skips the active-timer
    /// invariant.
    Log {
        description: String,
        #[arg(long)]
        from: chrono::DateTime<chrono::Utc>,
        #[arg(long)]
        to: chrono::DateTime<chrono::Utc>,
        #[arg(long)]
        project: Option<uuid::Uuid>,
        #[arg(long, default_value = "")]
        task_note: String,
        /// `true` / `false` to override the project default.
        /// Omit to inherit.
        #[arg(long)]
        billable: Option<bool>,
        #[arg(long = "tag")]
        tags: Vec<String>,
    },
    /// List sessions. Defaults to the last 7 days.
    List {
        /// Only sessions on this project (frontmatter uuid).
        #[arg(long)]
        project: Option<uuid::Uuid>,
        /// Inclusive since-date. Defaults to 7 days ago.
        #[arg(long)]
        since: Option<chrono::DateTime<chrono::Utc>>,
        /// Exclusive until-date. Defaults to "now".
        #[arg(long)]
        until: Option<chrono::DateTime<chrono::Utc>>,
        /// Filter open / closed sessions; omit for both.
        #[arg(long)]
        open: Option<bool>,
        /// Filter billable / non-billable; omit for both.
        #[arg(long)]
        billable: Option<bool>,
    },
    /// Resolve the rate cascade for the configured user +
    /// project. Useful to preview "what will this session
    /// bill at" before stopping.
    Resolve {
        #[arg(long)]
        project: Option<uuid::Uuid>,
    },
    /// Tag CRUD + attach to existing sessions.
    #[command(subcommand)]
    Tag(TimerTagCmd),
}

#[derive(Subcommand)]
enum TimerTagCmd {
    /// List tags in the calling user's org.
    List,
    /// Create a tag. Idempotent — no-op if a tag with that
    /// name already exists.
    Create {
        name: String,
        /// Hex `#RRGGBB` (UI hint). Empty = auto-pick.
        #[arg(long, default_value = "")]
        color: String,
    },
    /// Delete a tag by name. Removes the join rows on every
    /// session via FK cascade.
    Rm { name: String },
    /// Attach tags to an existing session.
    Attach {
        session_id: uuid::Uuid,
        #[arg(long = "tag", required = true)]
        tags: Vec<String>,
    },
    /// Detach tags from a session. `--tag <name>` removes
    /// that tag; `--all` removes every tag.
    Detach {
        session_id: uuid::Uuid,
        #[arg(long = "tag")]
        tags: Vec<String>,
        #[arg(long)]
        all: bool,
    },
}

#[derive(Subcommand)]
enum FinanceCmd {
    /// Print the weekly summary (hours + billable amount per
    /// project) as markdown. Reads the timer DB.
    Weekly {
        /// Any date inside the target week. Defaults to today.
        #[arg(long)]
        week_of: Option<chrono::NaiveDate>,
    },
    /// Per-project hours rollup for a range. Defaults to
    /// the last 7 days.
    Project {
        #[arg(long)]
        since: Option<chrono::DateTime<chrono::Utc>>,
        #[arg(long)]
        until: Option<chrono::DateTime<chrono::Utc>>,
    },
    /// Build + render an invoice from billable sessions on
    /// one project. By default writes both a PDF and a
    /// markdown stub into the vault's `Reports/Invoices/`
    /// directory (PDF under `Reports/Invoices/pdfs/`, MD at
    /// `Reports/Invoices/<num>.md` wikilinking the PDF).
    /// Use `--out` to override the PDF location and skip the
    /// vault export.
    Invoice {
        /// Project frontmatter uuid.
        #[arg(long)]
        project: uuid::Uuid,
        /// Inclusive lower bound on `start_time`.
        #[arg(long)]
        since: chrono::DateTime<chrono::Utc>,
        /// Exclusive upper bound on `start_time`.
        #[arg(long)]
        until: chrono::DateTime<chrono::Utc>,
        /// Invoice number, e.g. `INV-2026-0042`.
        #[arg(long)]
        number: String,
        /// Net N days for due date. Default 30.
        #[arg(long, default_value_t = 30)]
        net_days: i64,
        /// Free-text bill-to (display name). Used because we
        /// don't have a Party row yet in the local CLI flow.
        /// Once finance-db is mounted this becomes
        /// `--party-id <uuid>`.
        #[arg(long, default_value = "Bill-to")]
        client_name: String,
        /// Override PDF path. When set, skips the vault
        /// export and writes only this file. When omitted,
        /// the PDF lands at
        /// `<vault>/Reports/Invoices/pdfs/<num>.pdf` and a
        /// companion markdown stub goes to
        /// `<vault>/Reports/Invoices/<num>.md`.
        #[arg(long, short)]
        out: Option<std::path::PathBuf>,
    },
}

#[tokio::main]
async fn main() -> eyre::Result<()> {
    // Best-effort .env load before clap reads env. Missing file is
    // not an error — we just fall through to the hard-coded default.
    let _ = dotenvy::dotenv();
    let cli = Cli::parse();
    match cli.command {
        Commands::Doctor => {
            let remote =
                RemoteVoxConfig::from_args(cli.server, cli.session_token, cli.organization_id)?;
            println!("Vox endpoint: {}", remote.display_url);
        }
        Commands::Vault { cmd } => match cmd {
            // Sync ops touch vox and need async; everything
            // else stays in the sync path.
            VaultCmd::Sync { .. } | VaultCmd::Pull { .. } | VaultCmd::Push { .. } => {
                return Box::pin(run_vault_sync(cmd)).await;
            }
            other => {
                return run_vault(other);
            }
        },
        Commands::Task(cmd) => {
            return Box::pin(run_task(cmd)).await;
        }
        Commands::Agent(cmd) => {
            return run_agent(cmd).await;
        }
        Commands::Wiki(cmd) => {
            return run_wiki(cmd).await;
        }
        Commands::Timer(cmd) => {
            return run_timer(cmd, cli.org.as_deref()).await;
        }
        Commands::Finance(cmd) => {
            return run_finance(cmd, cli.org.as_deref()).await;
        }
        Commands::Auth(cmd) => {
            return run_auth(cmd, cli.org.as_deref()).await;
        }
        Commands::Org(cmd) => {
            return Box::pin(run_org(cmd)).await;
        }
        Commands::Mount(cmd) => {
            return run_mount(cmd);
        }
        Commands::Cycle(cmd) => {
            return run_cycle(cmd);
        }
        Commands::Project(cmd) => {
            return Box::pin(run_project(cmd)).await;
        }
        Commands::Goal(cmd) => {
            return Box::pin(run_goal(cmd)).await;
        }
        Commands::Milestone(cmd) => {
            return Box::pin(run_milestone(cmd)).await;
        }
        Commands::Location(cmd) => {
            return Box::pin(run_location(cmd)).await;
        }
        Commands::Recipe(cmd) => {
            return Box::pin(run_recipe(cmd)).await;
        }
        Commands::Meal(cmd) => {
            return Box::pin(run_meal(cmd)).await;
        }
        Commands::Pantry(cmd) => {
            return Box::pin(run_pantry(cmd)).await;
        }
    }
    Ok(())
}

/// Resolve the per-org vox URL from CLI flags + env.
/// Mirror of the helper inside `run_vault_sync`, lifted out
/// because project + goal share the same routing surface.
fn resolve_org_vox_url(server: Option<String>, org_slug: &str) -> String {
    let base = server.unwrap_or_else(|| {
        std::env::var("TASK_VOX_URL").unwrap_or_else(|_| "ws://127.0.0.1:18080".to_owned())
    });
    let stripped = base.trim_end_matches("/vox").trim_end_matches('/');
    format!("{stripped}/org/{org_slug}/vox")
}

/// Resolve the active org slug from `--org` flag or the
/// stored session. Returns a friendly error if neither
/// resolves.
fn resolve_active_org(override_slug: Option<String>) -> eyre::Result<String> {
    if let Some(s) = override_slug {
        return Ok(s);
    }
    session_store::load()?
        .map(|s| s.active)
        .ok_or_else(|| eyre::eyre!("no active org — pass --org or sign in first"))
}

async fn run_project(cmd: ProjectCmd) -> eyre::Result<()> {
    use project::ProjectServiceClient;

    match cmd {
        ProjectCmd::List { org, server, json } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client: ProjectServiceClient = Box::pin(vox::connect(&url).establish())
                .await
                .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))?;
            let rows = client
                .list()
                .await
                .map_err(|e| eyre::eyre!("list: {e:?}"))?;

            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&rows).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }

            // Group by parent for human readability: roots
            // first, then each root's subprojects indented.
            let total = rows.len();
            let roots: Vec<&project::ProjectInfo> =
                rows.iter().filter(|p| p.parent_id.is_none()).collect();
            println!("{} projects ({} top-level)\n", total, roots.len());
            for root in roots {
                print_project_row(root, 0);
                let kids: Vec<&project::ProjectInfo> = rows
                    .iter()
                    .filter(|p| p.parent_id == Some(root.id))
                    .collect();
                for k in kids {
                    print_project_row(k, 2);
                }
            }
        }
        ProjectCmd::Get {
            target,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client: ProjectServiceClient = Box::pin(vox::connect(&url).establish())
                .await
                .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))?;
            let p = if let Ok(id) = uuid::Uuid::parse_str(&target) {
                client
                    .get(id)
                    .await
                    .map_err(|e| eyre::eyre!("get(id): {e:?}"))?
            } else {
                client
                    .get_by_path(target.clone())
                    .await
                    .map_err(|e| eyre::eyre!("get(path): {e:?}"))?
            };

            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&p).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }

            println!("{} [{}]\n", p.title, p.status);
            println!("  id:       {}", p.id);
            println!("  path:     {}", p.path);
            println!("  priority: {}", p.priority);
            if let Some(parent) = p.parent_id {
                println!("  parent:   {parent}");
            }
            if !p.tags.0.is_empty() {
                println!("  tags:     {}", p.tags.0.join(", "));
            }
            if !p.details.is_empty() {
                println!("\n{}", p.details);
            }
        }
        ProjectCmd::Create {
            title,
            path,
            parent,
            status,
            priority,
            tags,
            details,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_project_client(&url).await?;

            let parent_id = match parent {
                None => None,
                Some(s) => Some(resolve_project_target(&client, &s).await?.id),
            };
            let details = resolve_body(details)?;
            let new_project = project::ProjectInfo {
                id: uuid::Uuid::nil(),
                path: path.unwrap_or_default(),
                title,
                status: status.unwrap_or_else(|| "active".into()),
                priority: priority.unwrap_or_else(|| "normal".into()),
                lead: String::new(),
                tags: project::model::Tags(tags),
                parent_id,
                same_as: None,
                details,
                client_id: None,
                billable_default: false,
                currency: String::new(),
                default_rate_cents: 0,
                estimated_seconds: 0,
                agent_profile: String::new(),
                color: String::new(),
                archived: false,
                date_created: None,
                date_modified: None,
            };
            let created = client
                .create(new_project)
                .await
                .map_err(|e| eyre::eyre!("create: {e:?}"))?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&created).map_err(|e| eyre::eyre!("json: {e}"))?
                );
            } else {
                println!("created {} ({})", created.title, created.path);
                println!("  id: {}", created.id);
            }
        }
        ProjectCmd::SetStatus {
            target,
            status,
            org,
            server,
        } => {
            mutate_project(target, org, server, |p| p.status = status).await?;
        }
        ProjectCmd::SetPriority {
            target,
            priority,
            org,
            server,
        } => {
            mutate_project(target, org, server, |p| p.priority = priority).await?;
        }
        ProjectCmd::SetParent {
            target,
            parent,
            org,
            server,
        } => {
            let slug = resolve_active_org(org.clone())?;
            let url = resolve_org_vox_url(server.clone(), &slug);
            let client = connect_project_client(&url).await?;
            let new_parent = if matches!(parent.as_str(), "none" | "null" | "") {
                None
            } else {
                Some(resolve_project_target(&client, &parent).await?.id)
            };
            mutate_project(target, org, server, |p| p.parent_id = new_parent).await?;
        }
        ProjectCmd::Archive {
            target,
            org,
            server,
        } => {
            mutate_project(target, org, server, |p| p.archived = true).await?;
        }
        ProjectCmd::Unarchive {
            target,
            org,
            server,
        } => {
            mutate_project(target, org, server, |p| p.archived = false).await?;
        }
        ProjectCmd::Rename {
            target,
            new_path,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_project_client(&url).await?;
            let p = resolve_project_target(&client, &target).await?;
            let renamed = client
                .rename(p.id, new_path.clone())
                .await
                .map_err(|e| eyre::eyre!("rename: {e:?}"))?;
            println!("renamed → {}", renamed.path);
        }
        ProjectCmd::Delete {
            target,
            yes,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_project_client(&url).await?;
            let p = resolve_project_target(&client, &target).await?;
            if !yes && !confirm(&format!("delete `{}` ({})?", p.title, p.path))? {
                println!("aborted");
                return Ok(());
            }
            client
                .delete(p.id)
                .await
                .map_err(|e| eyre::eyre!("delete: {e:?}"))?;
            println!("deleted {}", p.path);
        }
    }
    Ok(())
}

async fn connect_project_client(url: &str) -> eyre::Result<project::ProjectServiceClient> {
    Box::pin(vox::connect(url).establish())
        .await
        .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))
}

async fn resolve_project_target(
    client: &project::ProjectServiceClient,
    target: &str,
) -> eyre::Result<project::ProjectInfo> {
    if let Ok(id) = uuid::Uuid::parse_str(target) {
        return client
            .get(id)
            .await
            .map_err(|e| eyre::eyre!("get(id): {e:?}"));
    }
    client
        .get_by_path(target.to_owned())
        .await
        .map_err(|e| eyre::eyre!("get(path): {e:?}"))
}

async fn mutate_project<F>(
    target: String,
    org: Option<String>,
    server: Option<String>,
    apply: F,
) -> eyre::Result<()>
where
    F: FnOnce(&mut project::ProjectInfo),
{
    let slug = resolve_active_org(org)?;
    let url = resolve_org_vox_url(server, &slug);
    let client = connect_project_client(&url).await?;
    let mut p = resolve_project_target(&client, &target).await?;
    apply(&mut p);
    let updated = client
        .update(p)
        .await
        .map_err(|e| eyre::eyre!("update: {e:?}"))?;
    println!("{}  [{}]  {}", updated.title, updated.status, updated.path);
    Ok(())
}

fn resolve_body(arg: Option<String>) -> eyre::Result<String> {
    use std::io::Read;
    match arg {
        None => Ok(String::new()),
        Some(s) if s == "-" => {
            let mut buf = String::new();
            std::io::stdin().read_to_string(&mut buf)?;
            Ok(buf)
        }
        Some(s) => Ok(s),
    }
}

fn confirm(prompt: &str) -> eyre::Result<bool> {
    use std::io::{BufRead, Write};
    let stdin = std::io::stdin();
    let mut out = std::io::stdout();
    write!(out, "{prompt} [y/N] ")?;
    out.flush()?;
    let mut line = String::new();
    stdin.lock().read_line(&mut line)?;
    Ok(matches!(
        line.trim().to_ascii_lowercase().as_str(),
        "y" | "yes"
    ))
}

fn print_project_row(p: &project::ProjectInfo, indent: usize) {
    let pad = " ".repeat(indent);
    let tags = if p.tags.0.is_empty() {
        String::new()
    } else {
        format!("  [{}]", p.tags.0.join(", "))
    };
    println!(
        "{pad}{:<28}  {:<10}  {:<8}{tags}",
        p.title, p.status, p.priority
    );
}

async fn run_goal(cmd: GoalCmd) -> eyre::Result<()> {
    use chrono::Weekday;
    use cycle::FirstWeekRule;
    use goal::GoalServiceClient;

    match cmd {
        GoalCmd::List {
            org,
            server,
            current_cycle,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client: GoalServiceClient = Box::pin(vox::connect(&url).establish())
                .await
                .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))?;
            let mut rows = client
                .list()
                .await
                .map_err(|e| eyre::eyre!("list: {e:?}"))?;

            if current_cycle {
                let today = chrono::Local::now().date_naive();
                let now = cycle::cycle_for_date(
                    today,
                    Weekday::Mon,
                    FirstWeekRule::AtLeastFourDaysInYear,
                );
                if let Some(c) = now {
                    rows.retain(|g| g.cycle_id == Some(c.id));
                } else {
                    println!("today is between cycles — nothing to show");
                    return Ok(());
                }
            }

            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&rows).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }

            // Resolve cycle id → label once, reused across rows.
            let cycle_label = |g: &goal::Goal| -> Option<String> {
                use chrono::Datelike;
                let id = g.cycle_id?;
                let base = chrono::Local::now().date_naive().year();
                for off in [-1, 0, 1, 2] {
                    let qs = cycle::generate_year(
                        base + off,
                        Weekday::Mon,
                        FirstWeekRule::AtLeastFourDaysInYear,
                    );
                    for q in qs {
                        for c in q.cycles.iter() {
                            if c.id == id {
                                return Some(format!("{} Q{} C{}", c.year, c.quarter, c.ordinal));
                            }
                        }
                    }
                }
                None
            };

            println!("{} goals\n", rows.len());
            let roots: Vec<&goal::Goal> = rows.iter().filter(|g| g.parent_id.is_none()).collect();
            for root in roots {
                print_goal_row(root, 0, cycle_label(root));
                for kid in rows.iter().filter(|g| g.parent_id == Some(root.id)) {
                    print_goal_row(kid, 2, cycle_label(kid));
                    for gc in rows.iter().filter(|g| g.parent_id == Some(kid.id)) {
                        print_goal_row(gc, 4, cycle_label(gc));
                    }
                }
            }
        }
        GoalCmd::Get {
            target,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client: GoalServiceClient = Box::pin(vox::connect(&url).establish())
                .await
                .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))?;
            let g = if let Ok(id) = uuid::Uuid::parse_str(&target) {
                client
                    .get(id)
                    .await
                    .map_err(|e| eyre::eyre!("get(id): {e:?}"))?
            } else {
                client
                    .get_by_path(target.clone())
                    .await
                    .map_err(|e| eyre::eyre!("get(path): {e:?}"))?
            };

            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&g).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }

            println!("{} [{}]\n", g.title, g.status);
            println!("  id:       {}", g.id);
            println!("  path:     {}", g.path);
            println!("  kind:     {}", g.kind);
            if let Some(parent) = g.parent_id {
                println!("  parent:   {parent}");
            }
            if let Some(td) = g.target_date {
                println!("  target:   {td}");
            }
            if let Some(cid) = g.cycle_id {
                println!("  cycle:    {cid}");
            }
            if !g.tags.0.is_empty() {
                println!("  tags:     {}", g.tags.0.join(", "));
            }
            if !g.details.is_empty() {
                println!("\n{}", g.details);
            }
        }
        GoalCmd::Create {
            title,
            kind,
            status,
            path,
            parent,
            target_date,
            cycle,
            cycle_current,
            tags,
            details,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_goal_client(&url).await?;

            let parent_id = match parent {
                None => None,
                Some(s) => Some(resolve_goal_target(&client, &s).await?.id),
            };
            let cycle_id = resolve_cycle_arg(cycle, cycle_current)?;
            let kind_str = kind.unwrap_or_else(|| {
                if cycle_id.is_some() {
                    "cycle".into()
                } else {
                    "lifetime".into()
                }
            });
            let target_date = match target_date {
                None => None,
                Some(s) => Some(
                    chrono::NaiveDate::parse_from_str(&s, "%Y-%m-%d")
                        .map_err(|e| eyre::eyre!("--target-date: {e}"))?,
                ),
            };
            let details = resolve_body(details)?;
            let new_goal = goal::Goal {
                id: uuid::Uuid::nil(),
                path: path.unwrap_or_default(),
                title,
                kind: kind_str,
                status: status.unwrap_or_else(|| "aspiration".into()),
                parent_id,
                target_date,
                cycle_id,
                tags: goal::Tags(tags),
                date_created: None,
                date_modified: None,
                details,
            };
            let created = client
                .create(new_goal)
                .await
                .map_err(|e| eyre::eyre!("create: {e:?}"))?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&created).map_err(|e| eyre::eyre!("json: {e}"))?
                );
            } else {
                println!("created {} ({})", created.title, created.path);
                println!("  id: {}", created.id);
            }
        }
        GoalCmd::SetStatus {
            target,
            status,
            org,
            server,
        } => {
            mutate_goal(target, org, server, |g| g.status = status).await?;
        }
        GoalCmd::SetParent {
            target,
            parent,
            org,
            server,
        } => {
            let slug = resolve_active_org(org.clone())?;
            let url = resolve_org_vox_url(server.clone(), &slug);
            let client = connect_goal_client(&url).await?;
            let new_parent = if matches!(parent.as_str(), "none" | "null" | "") {
                None
            } else {
                Some(resolve_goal_target(&client, &parent).await?.id)
            };
            mutate_goal(target, org, server, |g| g.parent_id = new_parent).await?;
        }
        GoalCmd::SetCycle {
            target,
            cycle,
            org,
            server,
        } => {
            let is_current = cycle == "current";
            let arg = if is_current { None } else { Some(cycle) };
            let new_cycle = resolve_cycle_arg(arg, is_current)?;
            mutate_goal(target, org, server, |g| g.cycle_id = new_cycle).await?;
        }
        GoalCmd::Rename {
            target,
            new_path,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_goal_client(&url).await?;
            let g = resolve_goal_target(&client, &target).await?;
            let renamed = client
                .rename(g.id, new_path)
                .await
                .map_err(|e| eyre::eyre!("rename: {e:?}"))?;
            println!("renamed → {}", renamed.path);
        }
        GoalCmd::Delete {
            target,
            yes,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_goal_client(&url).await?;
            let g = resolve_goal_target(&client, &target).await?;
            if !yes && !confirm(&format!("delete `{}` ({})?", g.title, g.path))? {
                println!("aborted");
                return Ok(());
            }
            client
                .delete(g.id)
                .await
                .map_err(|e| eyre::eyre!("delete: {e:?}"))?;
            println!("deleted {}", g.path);
        }
    }
    Ok(())
}

async fn connect_goal_client(url: &str) -> eyre::Result<goal::GoalServiceClient> {
    Box::pin(vox::connect(url).establish())
        .await
        .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))
}

async fn resolve_goal_target(
    client: &goal::GoalServiceClient,
    target: &str,
) -> eyre::Result<goal::Goal> {
    if let Ok(id) = uuid::Uuid::parse_str(target) {
        return client
            .get(id)
            .await
            .map_err(|e| eyre::eyre!("get(id): {e:?}"));
    }
    client
        .get_by_path(target.to_owned())
        .await
        .map_err(|e| eyre::eyre!("get(path): {e:?}"))
}

async fn mutate_goal<F>(
    target: String,
    org: Option<String>,
    server: Option<String>,
    apply: F,
) -> eyre::Result<()>
where
    F: FnOnce(&mut goal::Goal),
{
    let slug = resolve_active_org(org)?;
    let url = resolve_org_vox_url(server, &slug);
    let client = connect_goal_client(&url).await?;
    let mut g = resolve_goal_target(&client, &target).await?;
    apply(&mut g);
    let updated = client
        .update(g)
        .await
        .map_err(|e| eyre::eyre!("update: {e:?}"))?;
    println!("{}  [{}]  {}", updated.title, updated.status, updated.path);
    Ok(())
}

/// Resolve a `--cycle` flag / argument into a concrete cycle
/// UUID. Accepts:
/// - a literal UUID
/// - `YYYY:Qn:Cm` (e.g. `2026:Q3:C1`)
/// - the cycle-current shortcut (when `current = true` or arg
///   is `current`)
/// - `none` / `null` / "" → clear
fn resolve_cycle_arg(arg: Option<String>, current: bool) -> eyre::Result<Option<uuid::Uuid>> {
    use chrono::{Datelike, Local, Weekday};
    use cycle::FirstWeekRule;

    if current {
        let today = Local::now().date_naive();
        return Ok(cycle::cycle_for_date(
            today,
            Weekday::Mon,
            FirstWeekRule::AtLeastFourDaysInYear,
        )
        .map(|c| c.id));
    }
    let Some(s) = arg else {
        return Ok(None);
    };
    if matches!(s.as_str(), "none" | "null" | "") {
        return Ok(None);
    }
    if let Ok(id) = uuid::Uuid::parse_str(&s) {
        return Ok(Some(id));
    }
    // Parse `YYYY:Qn:Cm`.
    let parts: Vec<&str> = s.split(':').collect();
    if parts.len() == 3 {
        let year = parts[0].parse::<i32>().ok();
        let q = parts[1]
            .strip_prefix('Q')
            .and_then(|n| n.parse::<u8>().ok());
        let ord = parts[2]
            .strip_prefix('C')
            .and_then(|n| n.parse::<u8>().ok());
        if let (Some(year), Some(q), Some(ord)) = (year, q, ord) {
            let base = Local::now().date_naive().year();
            for off in [-1_i32, 0, 1, 2] {
                let qs = cycle::generate_year(
                    base + off,
                    Weekday::Mon,
                    FirstWeekRule::AtLeastFourDaysInYear,
                );
                for qq in qs {
                    if qq.year == year && qq.ordinal == q {
                        for c in qq.cycles.iter() {
                            if c.ordinal == ord {
                                return Ok(Some(c.id));
                            }
                        }
                    }
                }
            }
            return Err(eyre::eyre!(
                "cycle `{s}` not found in surrounding years ({}..={})",
                base - 1,
                base + 2
            ));
        }
    }
    Err(eyre::eyre!(
        "--cycle: expected UUID, `YYYY:Qn:Cm`, `current`, or `none` (got `{s}`)"
    ))
}

fn print_goal_row(g: &goal::Goal, indent: usize, cycle: Option<String>) {
    let pad = " ".repeat(indent);
    let cycle_str = cycle.map(|c| format!("  @{c}")).unwrap_or_default();
    let target = g
        .target_date
        .map(|d| format!("  (target {d})"))
        .unwrap_or_default();
    println!(
        "{pad}{:<32}  {:<10}  {:<10}{cycle_str}{target}",
        g.title, g.kind, g.status
    );
}

fn run_cycle(cmd: CycleCmd) -> eyre::Result<()> {
    use chrono::Datelike;
    let rule = cycle::FirstWeekRule::AtLeastFourDaysInYear;
    match cmd {
        CycleCmd::Current => {
            let today = chrono::Local::now().date_naive();
            // Walk the year (and its neighbors) to find whether
            // we're inside a cycle or in a reset / bonus week.
            if let Some(c) = cycle::cycle_for_date(today, chrono::Weekday::Mon, rule) {
                let total = (c.end_date - c.start_date).num_days() + 1;
                let elapsed = (today - c.start_date).num_days() + 1;
                let pct = (elapsed as f64) * 100.0 / (total as f64);
                println!(
                    "{}-Q{}-C{}  ({} → {})",
                    c.year, c.quarter, c.ordinal, c.start_date, c.end_date,
                );
                println!("today:   {today}");
                println!("day {elapsed} of {total}   ({pct:.0}%)");
                println!("id:      {}", c.id);
            } else {
                println!("today ({today}) is between cycles — reset or bonus week");
            }
        }
        CycleCmd::List { year, week_start } => {
            let year = year.unwrap_or_else(|| chrono::Local::now().year());
            let wd = cycle::weekday_from_short(&week_start)
                .ok_or_else(|| eyre::eyre!("bad --week-start `{week_start}`"))?;
            let qs = cycle::generate_year(year, wd, rule);
            let bonus = cycle::has_bonus_week(year, wd, rule);
            println!(
                "Cyclic year {year}  week-start={week_start}  {}",
                if bonus { "[cyclic-leap]" } else { "" }
            );
            for q in qs {
                println!("\nQ{}  {} → {}", q.ordinal, q.start_date, q.end_date,);
                for c in q.cycles.iter() {
                    println!(
                        "  C{}   {} → {}   ({} days)",
                        c.ordinal,
                        c.start_date,
                        c.end_date,
                        (c.end_date - c.start_date).num_days() + 1,
                    );
                }
                println!("  reset  {} → {}", q.reset_week_start, q.reset_week_end,);
                if let (Some(s), Some(e)) = (q.bonus_week_start, q.bonus_week_end) {
                    println!("  bonus  {s} → {e}   (week zero for {})", year + 1);
                }
            }
        }
        CycleCmd::Reflect {
            year,
            quarter,
            cycle,
        } => {
            let ctx = org_ctx::resolve_active(None)?;
            let target = pick_reflection_cycle(year, quarter, cycle, rule)
                .ok_or_else(|| eyre::eyre!("no matching cycle"))?;
            let cycles_dir = ctx.root.wiki_knowledge_dir().join("cycles");
            std::fs::create_dir_all(&cycles_dir)
                .map_err(|e| eyre::eyre!("create {}: {e}", cycles_dir.display()))?;
            let filename = format!("{}-Q{}-C{}.md", target.year, target.quarter, target.ordinal);
            let path = cycles_dir.join(&filename);
            if path.exists() {
                println!("(reflection already exists)");
                println!("  {}", path.display());
                return Ok(());
            }
            let now = chrono::Utc::now();
            let body = format!(
                "---\n\
                 type: cycle-reflection\n\
                 id: {id}\n\
                 cycleId: {id}\n\
                 year: {year}\n\
                 quarter: {quarter}\n\
                 cycle: {ordinal}\n\
                 start: {start}\n\
                 end: {end}\n\
                 dateCreated: {created}\n\
                 ---\n\
                 \n\
                 # {year} Q{quarter} C{ordinal} reflection\n\
                 \n\
                 Cycle window: **{start} → {end}** (4 weeks, 25% each).\n\
                 \n\
                 ## What worked\n\
                 \n\
                 - \n\
                 \n\
                 ## What didn't\n\
                 \n\
                 - \n\
                 \n\
                 ## Lessons\n\
                 \n\
                 - \n\
                 \n\
                 ## Going into the next cycle\n\
                 \n\
                 - \n",
                id = target.id,
                year = target.year,
                quarter = target.quarter,
                ordinal = target.ordinal,
                start = target.start_date,
                end = target.end_date,
                created = now.to_rfc3339(),
            );
            std::fs::write(&path, body)
                .map_err(|e| eyre::eyre!("write {}: {e}", path.display()))?;
            println!("Created cycle reflection at:");
            println!("  {}", path.display());
            println!(
                "  for {}-Q{}-C{} ({} → {})",
                target.year, target.quarter, target.ordinal, target.start_date, target.end_date,
            );
        }
    }
    Ok(())
}

/// Resolve which `cycle::Cycle` a reflection should target. If any
/// of (year, quarter, cycle) are explicit, walk the generator and
/// look it up. Otherwise pick the cycle that today's date lands in;
/// if today is in a reset week, pick the cycle that just ended
/// (the most recent C3 of that quarter).
fn pick_reflection_cycle(
    year: Option<i32>,
    quarter: Option<u8>,
    cycle_ord: Option<u8>,
    rule: cycle::FirstWeekRule,
) -> Option<cycle::Cycle> {
    use chrono::Datelike;
    let today = chrono::Local::now().date_naive();
    if let (Some(y), Some(q), Some(c)) = (year, quarter, cycle_ord) {
        let quarters = cycle::generate_year(y, chrono::Weekday::Mon, rule);
        let qrec = quarters.iter().find(|qr| qr.ordinal == q)?;
        return qrec.cycles.iter().find(|cy| cy.ordinal == c).cloned();
    }
    if let Some(c) = cycle::cycle_for_date(today, chrono::Weekday::Mon, rule) {
        return Some(c);
    }
    // Reset week → walk this year's quarters and grab the most
    // recent C3 that ended before today.
    for cyclic_year in [today.year(), today.year() - 1] {
        let quarters = cycle::generate_year(cyclic_year, chrono::Weekday::Mon, rule);
        let mut latest: Option<cycle::Cycle> = None;
        for q in quarters {
            for c in q.cycles.iter() {
                if c.end_date < today && latest.as_ref().is_none_or(|l| c.end_date > l.end_date) {
                    latest = Some(c.clone());
                }
            }
        }
        if latest.is_some() {
            return latest;
        }
    }
    None
}

fn run_mount(cmd: MountCmd) -> eyre::Result<()> {
    let mut reg =
        mount::MountRegistry::from_env().map_err(|e| eyre::eyre!("load mounts.toml: {e}"))?;
    match cmd {
        MountCmd::Add {
            project_id,
            path,
            under_vault,
            label,
            replace,
        } => {
            let resolved = if under_vault {
                org_proto::default_client_vault_root()
                    .map_err(|e| eyre::eyre!("client vault root: {e}"))?
                    .join(&path)
            } else {
                path
            };
            let display = resolved.display().to_string();
            let mut mount = mount::Mount::filesystem(project_id, &display);
            mount.label = label;
            reg.add(mount, replace)
                .map_err(|e| eyre::eyre!("register mount: {e}"))?;
            reg.save()
                .map_err(|e| eyre::eyre!("save mounts.toml: {e}"))?;
            println!("Mounted project {project_id}");
            println!("  path:     {display}");
            println!("  registry: {}", reg.path().display());
        }
        MountCmd::List => {
            if reg.is_empty() {
                println!("(no mounts registered at {})", reg.path().display());
                return Ok(());
            }
            println!("registry: {}", reg.path().display());
            for mount in reg.iter() {
                let label = if mount.label.is_empty() {
                    String::new()
                } else {
                    format!("  ({})", mount.label)
                };
                println!(
                    "  {} [{:?}] {}{label}",
                    mount.project_id, mount.backend, mount.path
                );
            }
        }
        MountCmd::Rm { project_id } => match reg.remove(project_id) {
            Some(prev) => {
                reg.save()
                    .map_err(|e| eyre::eyre!("save mounts.toml: {e}"))?;
                println!("Removed mount for {project_id} ({})", prev.path);
            }
            None => {
                println!("(no mount registered for {project_id})");
            }
        },
        MountCmd::Path => {
            println!("{}", reg.path().display());
        }
    }
    Ok(())
}

async fn run_org(cmd: OrgCmd) -> eyre::Result<()> {
    match cmd {
        OrgCmd::Create {
            slug,
            name,
            home,
            server,
        } => {
            let url = resolve_server_vox_url(server.as_deref())?;
            let token = session_store::load()?
                .and_then(|s| s.servers.get(&s.active).map(|e| e.token.clone()))
                .unwrap_or_default();
            let client: org_proto::OrgManagementServiceClient =
                Box::pin(vox::connect(&url).establish())
                    .await
                    .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))?;
            let manifest = client
                .create_org(org_proto::CreateOrgRequest {
                    session_token: token,
                    slug: slug.clone(),
                    display_name: name,
                    is_home: home,
                })
                .await
                .map_err(|e| eyre::eyre!("create_org: {e:?}"))?;
            println!("Server created org `{slug}`");
            println!("  id:         {}", manifest.id);
            println!("  name:       {}", manifest.display_name);
            println!("  is_home:    {}", manifest.is_home);
            println!("  server vox: {url}");
        }
        OrgCmd::Init { slug, name, home } => {
            let root = org_proto::DataRoot::from_env()
                .map_err(|e| eyre::eyre!("resolve data root: {e}"))?;
            root.ensure()
                .map_err(|e| eyre::eyre!("ensure data root: {e}"))?;
            let org = root
                .init_org(&slug, &name, home)
                .map_err(|e| eyre::eyre!("init org: {e}"))?;
            let manifest = org
                .manifest()
                .map_err(|e| eyre::eyre!("load fresh manifest: {e}"))?;
            println!(
                "Initialized org `{}` at {} (LOCAL — bypassing server)",
                slug,
                org.path().display()
            );
            println!("  id:         {}", manifest.id);
            println!("  name:       {}", manifest.display_name);
            println!("  is_home:    {}", manifest.is_home);
            println!("  vault:      {}", org.vault_dir().display());
            println!("  auth.db:    {}", org.auth_db().display());
            println!("  timer.db:   {}", org.timer_db().display());
            println!("  finance.db: {}", org.finance_db().display());
            println!("\nNote: prefer `task org create` so the server is the source of truth.");
        }
        OrgCmd::List { server } => {
            let url = resolve_server_vox_url(server.as_deref())?;
            let client: org_proto::OrgManagementServiceClient =
                Box::pin(vox::connect(&url).establish())
                    .await
                    .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))?;
            let orgs = client
                .list_orgs()
                .await
                .map_err(|e| eyre::eyre!("list_orgs: {e:?}"))?;
            if orgs.is_empty() {
                println!("(server has no orgs hosted at {url})");
                return Ok(());
            }
            for m in orgs {
                let badge = if m.is_home { " [home]" } else { "" };
                println!("{}{}  {}  ({})", m.slug, badge, m.display_name, m.id);
                if !m.federation_url.is_empty() {
                    println!("    federation: {}", m.federation_url);
                }
            }
        }
    }
    Ok(())
}

/// Resolve the server-management vox URL:
/// - explicit `--server <ws://...>` flag wins
/// - else honor `TASK_SERVER_VOX_URL`
/// - else fall back to `ws://127.0.0.1:18080/server/vox`
fn resolve_server_vox_url(override_url: Option<&str>) -> eyre::Result<String> {
    if let Some(u) = override_url {
        return Ok(normalize_server_vox(u));
    }
    if let Ok(env) = std::env::var("TASK_SERVER_VOX_URL") {
        if !env.is_empty() {
            return Ok(normalize_server_vox(&env));
        }
    }
    Ok("ws://127.0.0.1:18080/server/vox".into())
}

fn normalize_server_vox(raw: &str) -> String {
    // Already pointed at the right endpoint.
    if raw.ends_with("/server/vox") {
        return raw.to_owned();
    }
    // Map http(s) → ws(s).
    let ws: String = if let Some(rest) = raw.strip_prefix("http://") {
        format!("ws://{rest}")
    } else if let Some(rest) = raw.strip_prefix("https://") {
        format!("wss://{rest}")
    } else if raw.starts_with("ws://") || raw.starts_with("wss://") {
        raw.to_owned()
    } else {
        format!("ws://{raw}")
    };
    // Strip legacy `/vox` suffix (the per-org URL hint that
    // `TASK_VOX_URL` sometimes points at) so we don't end up
    // with `…/vox/server/vox`. Then attach the canonical
    // server-mgmt path.
    let trimmed = ws.trim_end_matches('/').trim_end_matches("/vox");
    format!("{trimmed}/server/vox")
}

/// Open `ArchitectAuth` against a specific org's
/// `auth.sqlite` — same DB the server uses for that org.
/// CLI ↔ server interop hinges on matching the
/// `<data_root>/orgs/<slug>/auth.sqlite` resolver plus
/// `DEFAULT_AUTH_SECRET`.
async fn open_local_auth(
    auth_db_path: &std::path::Path,
) -> eyre::Result<architect_auth::ArchitectAuth<architect_auth::db::AuthSeaOrmStorage>> {
    use architect_auth::db::{AuthSeaOrmStorage, Migrator as AuthMigrator};
    use sea_orm::Database;
    use sea_orm_migration::MigratorTrait;
    let db_url = format!("sqlite://{}?mode=rwc", auth_db_path.display());
    let db = Database::connect(&db_url)
        .await
        .map_err(|e| eyre::eyre!("connect auth db `{db_url}`: {e}"))?;
    AuthMigrator::up(&db, None)
        .await
        .map_err(|e| eyre::eyre!("auth migrations: {e}"))?;
    let storage = AuthSeaOrmStorage::new(db);
    architect_auth::ArchitectAuth::builder()
        .secret(session_store::DEFAULT_AUTH_SECRET)
        .storage(storage)
        .build()
        .map_err(|e| eyre::eyre!("build ArchitectAuth: {e}"))
}

async fn run_auth(cmd: AuthCmd, org_override: Option<&str>) -> eyre::Result<()> {
    use architect_auth::commands::CreateEmailPasswordUser;
    use architect_auth::commands::{CurrentSession, SignOut};
    use architect_auth::proto::SignInEmailPassword;
    let ctx = org_ctx::resolve_active(org_override)?;
    let auth_db_path = ctx.root.auth_db();
    match cmd {
        AuthCmd::Signup {
            email,
            password,
            username,
            name,
        } => {
            let auth = open_local_auth(&auth_db_path).await?;
            let bundle = auth
                .create_email_password_user(CreateEmailPasswordUser {
                    email: email.clone(),
                    password,
                    name: name.clone(),
                    username: username.clone(),
                    image: None,
                    metadata_json: None,
                    ip_address: None,
                    user_agent: Some("task-cli".into()),
                })
                .await
                .map_err(|e| eyre::eyre!("create user: {e}"))?;
            let resolved_email = bundle.user.email.clone().unwrap_or_else(|| email.clone());
            // Persist session under this org's slug — same
            // shape as `Login` so subsequent commands work
            // without a follow-up `task auth login`.
            let mut sess = session_store::load()?.unwrap_or_else(|| session_store::CliSession {
                home: ctx.root.slug().to_owned(),
                active: ctx.root.slug().to_owned(),
                servers: std::collections::BTreeMap::new(),
            });
            sess.active = ctx.root.slug().to_owned();
            if sess.home.is_empty() {
                sess.home = ctx.root.slug().to_owned();
            }
            sess.servers.insert(
                ctx.root.slug().to_owned(),
                session_store::ServerEntry {
                    url: "local".into(),
                    user_id: bundle.user.id,
                    email: resolved_email.clone(),
                    token: bundle.token.clone(),
                },
            );
            session_store::save(&sess)?;
            println!(
                "Created user {} ({}) in org `{}`",
                resolved_email,
                bundle.user.id,
                ctx.root.slug(),
            );
            if let Some(u) = username {
                println!("  username: {u}");
            }
            if let Some(n) = name {
                println!("  name:     {n}");
            }
            println!("  auth db:  {}", auth_db_path.display());
        }
        AuthCmd::Login { email, password } => {
            let auth = open_local_auth(&auth_db_path).await?;
            let bundle = auth
                .sign_in_email_password(SignInEmailPassword {
                    email: email.clone(),
                    password,
                    ip_address: None,
                    user_agent: Some("task-cli".into()),
                })
                .await
                .map_err(|e| eyre::eyre!("sign in: {e}"))?;
            let resolved_email = bundle.user.email.clone().unwrap_or_else(|| email.clone());
            // Multi-server session shape: insert/update the
            // entry under this org's slug and set it active.
            // `home` defaults to the first server signed into
            // (the personal-org-as-home pattern).
            let mut sess = session_store::load()?.unwrap_or_else(|| session_store::CliSession {
                home: ctx.root.slug().to_owned(),
                active: ctx.root.slug().to_owned(),
                servers: std::collections::BTreeMap::new(),
            });
            sess.active = ctx.root.slug().to_owned();
            if sess.home.is_empty() {
                sess.home = ctx.root.slug().to_owned();
            }
            sess.servers.insert(
                ctx.root.slug().to_owned(),
                session_store::ServerEntry {
                    url: "local".into(),
                    user_id: bundle.user.id,
                    email: resolved_email.clone(),
                    token: bundle.token.clone(),
                },
            );
            session_store::save(&sess)?;
            println!(
                "Signed in as {} ({}) on org `{}`",
                resolved_email,
                bundle.user.id,
                ctx.root.slug(),
            );
            if let Some(member_org) = bundle.session.active_organization_id {
                println!("Architect-auth active membership: {member_org}");
            }
        }
        AuthCmd::Whoami => match session_store::load()? {
            Some(s) => {
                println!(
                    "home:   {}",
                    if s.home.is_empty() {
                        "(none)"
                    } else {
                        s.home.as_str()
                    }
                );
                println!("active: {}", s.active);
                for (slug, entry) in &s.servers {
                    let marker = if *slug == s.active { "*" } else { " " };
                    println!(
                        "{marker} {slug:<20}  {}  {}  url={}",
                        entry.email, entry.user_id, entry.url
                    );
                }
                println!("session: {}", session_store::session_path()?.display());
            }
            None => {
                println!("Not signed in. Run `task auth login --email … --password …`.");
            }
        },
        AuthCmd::Logout => {
            if let Some(mut sess) = session_store::load()? {
                // Sign out only the active org's session
                // server-side. Other servers stay linked.
                if let Some(entry) = sess.servers.remove(&sess.active) {
                    let auth = open_local_auth(&auth_db_path).await?;
                    if let Err(e) = auth.sign_out(SignOut { token: entry.token }).await {
                        eprintln!("warning: server-side sign out failed: {e}");
                    }
                }
                // If no servers left, clear the file entirely;
                // else write the shrunken session back.
                if sess.servers.is_empty() {
                    session_store::clear()?;
                } else {
                    // Active falls back to home if home is
                    // still present, otherwise pick the first
                    // remaining server.
                    if !sess.servers.contains_key(&sess.active) {
                        sess.active = if sess.servers.contains_key(&sess.home) {
                            sess.home.clone()
                        } else {
                            sess.servers.keys().next().cloned().unwrap_or_default()
                        };
                    }
                    session_store::save(&sess)?;
                }
            }
            println!("Signed out of `{}`.", ctx.root.slug());
        }
        AuthCmd::Org(AuthOrgCmd::List) => {
            let Some(sess) = session_store::load()? else {
                return Err(eyre::eyre!("not signed in — run `task auth login` first"));
            };
            let Some(active_entry) = sess.active_server() else {
                return Err(eyre::eyre!(
                    "no active server entry in session — run `task auth login --org {} …` first",
                    ctx.root.slug()
                ));
            };
            let auth = open_local_auth(&auth_db_path).await?;
            // Verify session still valid + refresh user_id.
            let bundle = auth
                .current_session(CurrentSession {
                    token: active_entry.token.clone(),
                })
                .await
                .map_err(|e| eyre::eyre!("session: {e}"))?;
            let memberships = list_user_memberships(bundle.user.id, &auth_db_path).await?;
            if memberships.is_empty() {
                println!("(no org memberships)");
            }
            for (member, org) in memberships {
                println!(
                    "  {}  {}  ({})",
                    member.organization_id, org.name, member.role
                );
            }
        }
        AuthCmd::Org(AuthOrgCmd::Use { org_id }) => {
            let Some(sess) = session_store::load()? else {
                return Err(eyre::eyre!("not signed in — run `task auth login` first"));
            };
            let Some(active_entry) = sess.active_server() else {
                return Err(eyre::eyre!("no active server in session"));
            };
            // Membership check.
            let memberships = list_user_memberships(active_entry.user_id, &auth_db_path).await?;
            if !memberships.iter().any(|(m, _)| m.organization_id == org_id) {
                return Err(eyre::eyre!("user is not a member of org {org_id}"));
            }
            update_session_active_org(&active_entry.token, Some(org_id), &auth_db_path).await?;
            println!("Architect-auth active membership set to {org_id}");
        }
    }
    Ok(())
}

async fn open_auth_db(auth_db_path: &std::path::Path) -> eyre::Result<sea_orm::DatabaseConnection> {
    use sea_orm::Database;
    use sea_orm_migration::MigratorTrait;
    let db = Database::connect(format!("sqlite://{}?mode=rwc", auth_db_path.display()))
        .await
        .map_err(|e| eyre::eyre!("connect auth db: {e}"))?;
    architect_auth::db::Migrator::up(&db, None)
        .await
        .map_err(|e| eyre::eyre!("auth migrations: {e}"))?;
    Ok(db)
}

async fn list_user_memberships(
    user_id: uuid::Uuid,
    auth_db_path: &std::path::Path,
) -> eyre::Result<
    Vec<(
        architect_auth::db::AuthMemberModel,
        architect_auth::db::AuthOrganizationModel,
    )>,
> {
    use architect_auth::db::{AuthMemberColumn, AuthMemberEntity, AuthOrganizationEntity};
    use sea_orm::{ColumnTrait, EntityTrait, QueryFilter};
    let db = open_auth_db(auth_db_path).await?;
    let members = AuthMemberEntity::find()
        .filter(AuthMemberColumn::UserId.eq(user_id))
        .all(&db)
        .await
        .map_err(|e| eyre::eyre!("list members: {e}"))?;
    let mut out = Vec::with_capacity(members.len());
    for m in members {
        let Some(org) = AuthOrganizationEntity::find_by_id(m.organization_id)
            .one(&db)
            .await
            .map_err(|e| eyre::eyre!("find org {}: {e}", m.organization_id))?
        else {
            continue;
        };
        out.push((m, org));
    }
    Ok(out)
}

async fn update_session_active_org(
    token: &str,
    org_id: Option<uuid::Uuid>,
    auth_db_path: &std::path::Path,
) -> eyre::Result<()> {
    use architect_auth::db::{AuthSessionActiveModel, AuthSessionColumn, AuthSessionEntity};
    use sea_orm::{ActiveModelTrait, ColumnTrait, EntityTrait, IntoActiveModel, QueryFilter, Set};
    let token_hash = hash_session_token(session_store::DEFAULT_AUTH_SECRET, token);
    let db = open_auth_db(auth_db_path).await?;
    let row = AuthSessionEntity::find()
        .filter(AuthSessionColumn::TokenHash.eq(token_hash))
        .one(&db)
        .await
        .map_err(|e| eyre::eyre!("find session: {e}"))?
        .ok_or_else(|| eyre::eyre!("session not found — session file may be stale"))?;
    let mut am: AuthSessionActiveModel = row.into_active_model();
    am.active_organization_id = Set(org_id);
    am.update(&db)
        .await
        .map_err(|e| eyre::eyre!("update session: {e}"))?;
    Ok(())
}

/// Reproduce `architect-auth::crypto::hash_token`. The auth
/// crate keeps the helper crate-private; we re-implement the
/// exact same recipe so the CLI can look up its own session
/// row by token hash without depending on auth internals.
///
/// **Recipe (must match `architect-auth/crypto.rs`):**
/// `base64url-no-pad(SHA256(secret || ":" || token))`.
fn hash_session_token(secret: &str, token: &str) -> String {
    use base64::Engine;
    use base64::engine::general_purpose::URL_SAFE_NO_PAD;
    use sha2::{Digest, Sha256};
    let mut h = Sha256::new();
    h.update(secret.as_bytes());
    h.update(b":");
    h.update(token.as_bytes());
    URL_SAFE_NO_PAD.encode(h.finalize())
}

async fn run_finance(cmd: FinanceCmd, org_override: Option<&str>) -> eyre::Result<()> {
    use sea_orm::Database;
    use sea_orm_migration::MigratorTrait;

    let ctx = org_ctx::resolve_active(org_override)?;
    // `TASK_TIMER_DB` still wins as a hard override (lets a
    // fixture point at a fresh sqlite); else use the org's
    // resolver.
    let db_url = std::env::var("TASK_TIMER_DB")
        .unwrap_or_else(|_| format!("sqlite://{}?mode=rwc", ctx.root.timer_db().display()));
    let timer_conn = Database::connect(&db_url)
        .await
        .map_err(|e| eyre::eyre!("connect timer db `{db_url}`: {e}"))?;
    timer::Migrator::up(&timer_conn, None).await.ok();

    match cmd {
        FinanceCmd::Weekly { week_of } => {
            let day = week_of.unwrap_or_else(|| chrono::Utc::now().date_naive());
            let summary = finance::reports::weekly_summary(&timer_conn, None, day)
                .await
                .map_err(|e| eyre::eyre!("weekly: {e}"))?;
            print!("{}", summary.to_markdown());
        }
        FinanceCmd::Project { since, until } => {
            use finance::reports::DateRange;
            let range = if let (Some(s), Some(u)) = (since, until) {
                DateRange { since: s, until: u }
            } else {
                DateRange::last_7_days()
            };
            let rows = finance::reports::hours_by_project(&timer_conn, None, range)
                .await
                .map_err(|e| eyre::eyre!("project: {e}"))?;
            if rows.is_empty() {
                println!("(no closed sessions in range)");
            }
            for r in rows {
                let project = if r.project_path.is_empty() {
                    "(unscoped)".to_string()
                } else {
                    r.project_path.clone()
                };
                println!(
                    "{project}\n  sessions: {}\n  total:    {}\n  billable: {} ({} {})",
                    r.session_count,
                    fmt_seconds(r.total_seconds),
                    fmt_seconds(r.billable_seconds),
                    fmt_minor(r.billable_amount_minor),
                    if r.currency.is_empty() {
                        "(no currency)".to_string()
                    } else {
                        r.currency
                    },
                );
            }
        }
        FinanceCmd::Invoice {
            project,
            since,
            until,
            number,
            net_days,
            client_name,
            out,
        } => {
            let book = finance_proto::book::Book {
                id: uuid::Uuid::nil(),
                name: "CLI Book".into(),
                kind: finance_proto::book::BookKind::Personal,
                base_currency: "USD".into(),
                settings_json: "{}".into(),
                created_at: chrono::Utc::now(),
                updated_at: chrono::Utc::now(),
            };
            let party = finance_proto::party::Party {
                id: uuid::Uuid::nil(),
                book_id: book.id,
                kind: finance_proto::party::PartyKind::Client,
                display_name: client_name.clone(),
                legal_name: client_name.clone(),
                email: String::new(),
                phone: String::new(),
                address: String::new(),
                tax_id: String::new(),
                default_currency: "USD".into(),
                default_net_days: net_days.try_into().unwrap_or(30),
                default_rate_minor_per_hour: 0,
                notes: String::new(),
                is_archived: false,
                created_at: chrono::Utc::now(),
                updated_at: chrono::Utc::now(),
            };
            let build = finance::invoice_from_sessions::build_invoice_from_sessions(
                &timer_conn,
                finance::invoice_from_sessions::BuildInvoiceArgs {
                    book,
                    party: party.clone(),
                    project_id: project,
                    since,
                    until,
                    net_days,
                    number,
                    notes_public: "Thank you for your business.".into(),
                    notes_private: String::new(),
                    terms: format!("Net {net_days} from issue date."),
                },
            )
            .await
            .map_err(|e| eyre::eyre!("build invoice: {e}"))?;

            let issuer = finance::pdf_adapter::IssuerProfile {
                name: std::env::var("TASK_ISSUER_NAME").unwrap_or_else(|_| "Your Name".into()),
                address: std::env::var("TASK_ISSUER_ADDRESS").unwrap_or_default(),
                email: std::env::var("TASK_ISSUER_EMAIL").unwrap_or_default(),
                phone: String::new(),
                tax_id: String::new(),
            };
            let ifp = finance::pdf_adapter::invoice_for_pdf(&build.invoice, &issuer, &party);
            // Decide PDF path: explicit --out wins; else vault-export under
            // `<vault>/Reports/Invoices/pdfs/<num>.pdf`.
            let vault_root = std::env::var("TASK_VAULT_ROOT").map_or_else(
                |_| std::path::PathBuf::from("examples/vault"),
                std::path::PathBuf::from,
            );
            let do_vault_export = out.is_none();
            let pdf_path: std::path::PathBuf = if let Some(p) = out {
                p
            } else {
                let dir = vault_root.join("Reports").join("Invoices").join("pdfs");
                std::fs::create_dir_all(&dir)
                    .map_err(|e| eyre::eyre!("create {}: {e}", dir.display()))?;
                dir.join(format!("{}.pdf", build.invoice.number))
            };
            // Shell out to the `task-pdf-render` binary (in
            // libs/pdf). Fulgur's compile tree triggers a
            // stylo recursion-limit issue when pulled into
            // the CLI's larger graph; isolating it to a
            // standalone binary keeps both compiles clean.
            let request = serde_json::json!({
                "mode": "invoice",
                "data": ifp,
            });
            let render_bin = std::env::var("TASK_PDF_RENDER_BIN")
                .unwrap_or_else(|_| "task-pdf-render".to_string());
            let mut child = std::process::Command::new(&render_bin)
                .arg("--out")
                .arg(&pdf_path)
                .stdin(std::process::Stdio::piped())
                .stderr(std::process::Stdio::inherit())
                .spawn()
                .map_err(|e| {
                    eyre::eyre!(
                        "spawn `{render_bin}`: {e}. Build with `cargo build -p pdf` and put it on PATH, or set TASK_PDF_RENDER_BIN."
                    )
                })?;
            {
                let stdin = child
                    .stdin
                    .as_mut()
                    .ok_or_else(|| eyre::eyre!("render: no stdin"))?;
                serde_json::to_writer(stdin, &request)
                    .map_err(|e| eyre::eyre!("write request: {e}"))?;
            }
            let status = child.wait().map_err(|e| eyre::eyre!("wait: {e}"))?;
            if !status.success() {
                return Err(eyre::eyre!("`{render_bin}` exited with {status}"));
            }
            let bytes_len = std::fs::metadata(&pdf_path).map(|m| m.len()).unwrap_or(0);

            // Vault export: companion markdown stub at
            // `Reports/Invoices/<num>.md` wikilinking the
            // PDF. Skipped when caller passes --out.
            if do_vault_export {
                let md_path = vault_root
                    .join("Reports")
                    .join("Invoices")
                    .join(format!("{}.md", build.invoice.number));
                if let Some(parent) = md_path.parent() {
                    std::fs::create_dir_all(parent)
                        .map_err(|e| eyre::eyre!("create {}: {e}", parent.display()))?;
                }
                let rel_pdf = format!("pdfs/{}.pdf", build.invoice.number);
                let md = render_invoice_markdown(
                    &build.invoice,
                    &party,
                    &rel_pdf,
                    build.source_session_ids.len(),
                );
                std::fs::write(&md_path, md)
                    .map_err(|e| eyre::eyre!("write {}: {e}", md_path.display()))?;
                println!("Wrote {}", md_path.display());
            }
            println!(
                "Wrote {} ({bytes_len} bytes, {} sessions, {} {})",
                pdf_path.display(),
                build.source_session_ids.len(),
                fmt_minor(build.invoice.total_minor),
                build.invoice.currency,
            );
        }
    }
    Ok(())
}

fn fmt_seconds(s: i64) -> String {
    let h = s / 3600;
    let m = (s % 3600) / 60;
    if h > 0 {
        format!("{h}h{m:02}m")
    } else {
        format!("{m}m")
    }
}

fn fmt_minor(c: i64) -> String {
    let neg = c < 0;
    let abs = c.unsigned_abs();
    format!(
        "{}{}.{:02}",
        if neg { "-" } else { "" },
        abs / 100,
        abs % 100
    )
}

/// Companion markdown stub for an invoice. Wikilinks the
/// PDF (Obsidian-style `![[pdfs/INV-...pdf]]` embed) so a
/// vault viewer can open the file inline. Frontmatter makes
/// the page queryable in `Reports/Invoices/*.base`.
fn render_invoice_markdown(
    invoice: &finance_proto::invoice::Invoice,
    party: &finance_proto::party::Party,
    rel_pdf_path: &str,
    session_count: usize,
) -> String {
    let mut out = String::new();
    out.push_str("---\n");
    out.push_str("type: invoice\n");
    out.push_str(&format!("number: {}\n", invoice.number));
    out.push_str(&format!("status: {:?}\n", invoice.status).to_lowercase());
    out.push_str(&format!("issueDate: {}\n", invoice.issue_date));
    out.push_str(&format!("dueDate: {}\n", invoice.due_date));
    out.push_str(&format!("currency: {}\n", invoice.currency));
    out.push_str(&format!("totalMinor: {}\n", invoice.total_minor));
    out.push_str(&format!("balanceMinor: {}\n", invoice.balance_minor));
    out.push_str(&format!("party: \"{}\"\n", party.display_name));
    out.push_str(&format!("sessions: {session_count}\n"));
    out.push_str(&format!("pdf: \"{rel_pdf_path}\"\n"));
    out.push_str("tags: [invoice]\n");
    out.push_str("---\n\n");
    out.push_str(&format!("# Invoice {}\n\n", invoice.number));
    out.push_str(&format!(
        "**To:** {}  \n**Issued:** {}  \n**Due:** {}  \n**Total:** {} {}\n\n",
        party.display_name,
        invoice.issue_date,
        invoice.due_date,
        fmt_minor(invoice.total_minor),
        invoice.currency,
    ));
    out.push_str("## PDF\n\n");
    out.push_str(&format!("![[{rel_pdf_path}]]\n\n"));
    out.push_str("## Line items\n\n");
    out.push_str("| Description | Quantity | Unit price | Amount |\n");
    out.push_str("|---|---:|---:|---:|\n");
    for li in &invoice.line_items.0 {
        let qty_hours = (li.quantity_milli as f64) / 1000.0;
        out.push_str(&format!(
            "| {} | {:.2} hr | {} | {} |\n",
            li.description,
            qty_hours,
            fmt_minor(li.unit_price_minor),
            fmt_minor(li.line_total_minor),
        ));
    }
    out.push_str(&format!(
        "\n**Subtotal:** {} {}  \n",
        fmt_minor(invoice.subtotal_minor),
        invoice.currency,
    ));
    if invoice.tax_total_minor != 0 {
        out.push_str(&format!(
            "**Tax:** {} {}  \n",
            fmt_minor(invoice.tax_total_minor),
            invoice.currency,
        ));
    }
    out.push_str(&format!(
        "**Total:** {} {}\n",
        fmt_minor(invoice.total_minor),
        invoice.currency,
    ));
    if !invoice.notes_public.is_empty() {
        out.push_str(&format!("\n## Notes\n\n{}\n", invoice.notes_public));
    }
    out
}

async fn run_timer(cmd: TimerCmd, org_override: Option<&str>) -> eyre::Result<()> {
    use sea_orm::Database;
    use sea_orm::{ColumnTrait, EntityTrait, QueryFilter};
    use sea_orm_migration::MigratorTrait;
    use std::sync::Arc;
    use timer::entity::{TagColumn, TagEntity, WorkSessionTagColumn, WorkSessionTagEntity};
    use timer::store::{Store, VaultProjectDefaults};
    use timer_proto::service::{LogSessionRequest, StartTimerRequest, TimerService};

    // OrgRoot-driven path resolution. `TASK_TIMER_DB` /
    // `TASK_VAULT_ROOT` still win as hard overrides for
    // test fixtures. User/org ids come from the active
    // server entry in `session.json`; falls back to env vars
    // and finally dev nil-uuids for fresh setups.
    let ctx = org_ctx::resolve_active(org_override)?;
    let db_url = std::env::var("TASK_TIMER_DB")
        .unwrap_or_else(|_| format!("sqlite://{}?mode=rwc", ctx.root.timer_db().display()));
    let vault_root = std::env::var("TASK_VAULT_ROOT")
        .map_or_else(|_| ctx.root.vault_dir(), std::path::PathBuf::from);
    let stored_session = session_store::load().ok().flatten();
    let session_user_id = stored_session
        .as_ref()
        .and_then(|s| s.active_server().map(|e| e.user_id));
    let user_id = session_user_id
        .or_else(|| {
            std::env::var("TASK_USER_ID")
                .ok()
                .and_then(|s| s.parse::<uuid::Uuid>().ok())
        })
        .unwrap_or_else(|| uuid::Uuid::parse_str("00000000-0000-0000-0000-000000000001").unwrap());
    // `org_id` here is the architect-auth org membership id
    // (different from the on-disk org slug). Not currently
    // surfaced in the multi-server session shape; only an
    // env-var override is honored. Phase 3 federation can
    // promote this onto `ServerEntry`.
    let org_id = std::env::var("TASK_ORG_ID")
        .ok()
        .and_then(|s| s.parse::<uuid::Uuid>().ok())
        .unwrap_or_else(|| uuid::Uuid::parse_str("00000000-0000-0000-0000-00000000000a").unwrap());

    let conn = Database::connect(&db_url)
        .await
        .map_err(|e| eyre::eyre!("connect timer db `{db_url}`: {e}"))?;
    timer::Migrator::up(&conn, None)
        .await
        .map_err(|e| eyre::eyre!("timer migrations: {e}"))?;
    let defaults = Arc::new(VaultProjectDefaults {
        vault_root: vault_root.clone(),
    });
    let store = Store::new(conn, defaults);

    match cmd {
        TimerCmd::Start {
            description,
            project,
            task_note,
            tags,
        } => {
            let project_path = project_path_for(&vault_root, project);
            let session = store
                .start_timer(StartTimerRequest {
                    user_id,
                    org_id,
                    project_id: project,
                    project_path,
                    task_note_path: task_note,
                    description,
                })
                .await
                .map_err(|e| eyre::eyre!("start: {e}"))?;
            attach_tags_by_name(store.conn(), org_id, session.id, &tags).await?;
            println!("Started {} at {}", session.id, session.start_time);
            println!("  description: {}", session.description);
            if !session.project_path.is_empty() {
                println!("  project:     {}", session.project_path);
            }
            println!("  billable:    {}", session.billable);
            if !tags.is_empty() {
                println!("  tags:        {}", tags.join(", "));
            }
        }
        TimerCmd::Stop => {
            let session = store
                .stop_timer(user_id)
                .await
                .map_err(|e| eyre::eyre!("stop: {e}"))?;
            let elapsed = session
                .end_time
                .unwrap_or_else(chrono::Utc::now)
                .signed_duration_since(session.start_time);
            println!("Stopped {}", session.id);
            println!("  description: {}", session.description);
            println!("  elapsed:     {}", fmt_duration(elapsed));
            if session.billable {
                println!(
                    "  billed:      {} {} (rate: {} {}/h)",
                    fmt_money(billed_cents(&session, elapsed)),
                    session.currency,
                    fmt_money(session.rate_cents),
                    session.currency,
                );
            }
        }
        TimerCmd::Active => {
            match store
                .active_timer(user_id)
                .await
                .map_err(|e| eyre::eyre!("{e}"))?
            {
                Some(s) => {
                    let elapsed = chrono::Utc::now().signed_duration_since(s.start_time);
                    println!("Running for {} ({})", fmt_duration(elapsed), s.id);
                    if !s.description.is_empty() {
                        println!("  description: {}", s.description);
                    }
                    if !s.project_path.is_empty() {
                        println!("  project:     {}", s.project_path);
                    }
                    if !s.task_note_path.is_empty() {
                        println!("  task:        {}", s.task_note_path);
                    }
                }
                None => println!("No active timer."),
            }
        }
        TimerCmd::Switch {
            description,
            project,
            task_note,
            tags,
        } => {
            let project_path = project_path_for(&vault_root, project);
            let (closed, started) = store
                .switch_timer(StartTimerRequest {
                    user_id,
                    org_id,
                    project_id: project,
                    project_path,
                    task_note_path: task_note,
                    description,
                })
                .await
                .map_err(|e| eyre::eyre!("switch: {e}"))?;
            attach_tags_by_name(store.conn(), org_id, started.id, &tags).await?;
            if let Some(prev) = closed {
                let elapsed = prev
                    .end_time
                    .unwrap_or_else(chrono::Utc::now)
                    .signed_duration_since(prev.start_time);
                println!("Stopped {} after {}", prev.id, fmt_duration(elapsed));
            }
            println!("Started {} at {}", started.id, started.start_time);
            if !tags.is_empty() {
                println!("  tags: {}", tags.join(", "));
            }
        }
        TimerCmd::Log {
            description,
            from,
            to,
            project,
            task_note,
            billable,
            tags,
        } => {
            let project_path = project_path_for(&vault_root, project);
            let session = store
                .log_session(LogSessionRequest {
                    user_id,
                    org_id,
                    project_id: project,
                    project_path,
                    task_note_path: task_note,
                    description,
                    start_time: from,
                    end_time: to,
                    billable_override: billable,
                })
                .await
                .map_err(|e| eyre::eyre!("log: {e}"))?;
            attach_tags_by_name(store.conn(), org_id, session.id, &tags).await?;
            println!("Logged {} ({})", session.id, fmt_duration(to - from));
        }
        TimerCmd::List {
            project,
            since,
            until,
            open,
            billable,
        } => {
            let filter = timer_proto::WorkSessionFilter {
                user_id: Some(user_id),
                project_id: project,
                since: Some(
                    since.unwrap_or_else(|| chrono::Utc::now() - chrono::Duration::days(7)),
                ),
                until,
                billable,
                open,
            };
            let rows = store
                .list_sessions(&filter)
                .await
                .map_err(|e| eyre::eyre!("list: {e}"))?;
            if rows.is_empty() {
                println!("(no sessions)");
            }
            for s in rows {
                let end = s
                    .end_time
                    .map_or_else(|| "open".to_string(), |t| t.to_rfc3339());
                let elapsed = s
                    .end_time
                    .unwrap_or_else(chrono::Utc::now)
                    .signed_duration_since(s.start_time);
                println!(
                    "{}  {:>8}  {}  {} {}",
                    s.start_time.format("%Y-%m-%d %H:%M"),
                    fmt_duration(elapsed),
                    if s.billable { "billable" } else { "        " },
                    s.description,
                    if end == "open" {
                        "[OPEN]".to_string()
                    } else {
                        String::new()
                    },
                );
            }
        }
        TimerCmd::Resolve { project } => {
            let resolved = store
                .resolve_rate(user_id, project)
                .await
                .map_err(|e| eyre::eyre!("resolve: {e}"))?;
            println!(
                "rate: {} {}/h  source: {:?}",
                fmt_money(resolved.hourly_cents),
                if resolved.currency.is_empty() {
                    "(none)".to_string()
                } else {
                    resolved.currency
                },
                resolved.source,
            );
        }
        TimerCmd::Tag(sub) => match sub {
            TimerTagCmd::List => {
                let rows = TagEntity::find()
                    .filter(TagColumn::OrgId.eq(org_id))
                    .all(store.conn())
                    .await
                    .map_err(|e| eyre::eyre!("list tags: {e}"))?;
                if rows.is_empty() {
                    println!("(no tags)");
                }
                for t in rows {
                    let color = if t.color.is_empty() {
                        "(auto)"
                    } else {
                        t.color.as_str()
                    };
                    println!("{}  {}  {}", t.id, t.name, color);
                }
            }
            TimerTagCmd::Create { name, color } => {
                let tag = ensure_tag(store.conn(), org_id, &name, &color).await?;
                println!("{}  {}", tag.id, tag.name);
            }
            TimerTagCmd::Rm { name } => {
                let existing = TagEntity::find()
                    .filter(TagColumn::OrgId.eq(org_id))
                    .filter(TagColumn::Name.eq(name.clone()))
                    .one(store.conn())
                    .await
                    .map_err(|e| eyre::eyre!("find tag: {e}"))?;
                let Some(tag) = existing else {
                    return Err(eyre::eyre!("no such tag: {name}"));
                };
                TagEntity::delete_by_id(tag.id)
                    .exec(store.conn())
                    .await
                    .map_err(|e| eyre::eyre!("delete tag: {e}"))?;
                println!("Deleted tag {} ({})", tag.name, tag.id);
            }
            TimerTagCmd::Attach { session_id, tags } => {
                attach_tags_by_name(store.conn(), org_id, session_id, &tags).await?;
                println!("Attached {} to {session_id}", tags.join(", "));
            }
            TimerTagCmd::Detach {
                session_id,
                tags,
                all,
            } => {
                if all {
                    WorkSessionTagEntity::delete_many()
                        .filter(WorkSessionTagColumn::WorkSessionId.eq(session_id))
                        .exec(store.conn())
                        .await
                        .map_err(|e| eyre::eyre!("detach all: {e}"))?;
                    println!("Detached all tags from {session_id}");
                } else if tags.is_empty() {
                    return Err(eyre::eyre!("pass --tag <name> or --all"));
                } else {
                    let tag_rows = TagEntity::find()
                        .filter(TagColumn::OrgId.eq(org_id))
                        .filter(TagColumn::Name.is_in(tags.clone()))
                        .all(store.conn())
                        .await
                        .map_err(|e| eyre::eyre!("lookup tags: {e}"))?;
                    let ids: Vec<uuid::Uuid> = tag_rows.iter().map(|t| t.id).collect();
                    if ids.is_empty() {
                        return Err(eyre::eyre!("no matching tags"));
                    }
                    WorkSessionTagEntity::delete_many()
                        .filter(WorkSessionTagColumn::WorkSessionId.eq(session_id))
                        .filter(WorkSessionTagColumn::TagId.is_in(ids))
                        .exec(store.conn())
                        .await
                        .map_err(|e| eyre::eyre!("detach: {e}"))?;
                    println!("Detached {} from {session_id}", tags.join(", "));
                }
            }
        },
    }
    Ok(())
}

/// Idempotent tag upsert by `(org_id, name)`. Returns the
/// existing or freshly inserted row.
async fn ensure_tag(
    conn: &sea_orm::DatabaseConnection,
    org_id: uuid::Uuid,
    name: &str,
    color: &str,
) -> eyre::Result<timer::entity::TagModel> {
    use sea_orm::{ActiveModelTrait, ColumnTrait, EntityTrait, QueryFilter, Set};
    if let Some(existing) = timer::entity::TagEntity::find()
        .filter(timer::entity::TagColumn::OrgId.eq(org_id))
        .filter(timer::entity::TagColumn::Name.eq(name.to_string()))
        .one(conn)
        .await
        .map_err(|e| eyre::eyre!("find tag: {e}"))?
    {
        return Ok(existing);
    }
    let now = chrono::Utc::now();
    let am = timer::entity::TagActive {
        id: Set(uuid::Uuid::new_v4()),
        org_id: Set(org_id),
        name: Set(name.to_string()),
        color: Set(color.to_string()),
        created_at: Set(now),
        updated_at: Set(now),
    };
    am.insert(conn)
        .await
        .map_err(|e| eyre::eyre!("insert tag: {e}"))
}

/// Ensure each tag in `names` exists in `org_id` and attach
/// it to `session_id`. Already-attached pairs are skipped
/// (uniqueness index guards the join).
async fn attach_tags_by_name(
    conn: &sea_orm::DatabaseConnection,
    org_id: uuid::Uuid,
    session_id: uuid::Uuid,
    names: &[String],
) -> eyre::Result<()> {
    use sea_orm::{ActiveModelTrait, ColumnTrait, EntityTrait, QueryFilter, Set};
    for name in names {
        let tag = ensure_tag(conn, org_id, name, "").await?;
        let already = timer::entity::WorkSessionTagEntity::find()
            .filter(timer::entity::WorkSessionTagColumn::WorkSessionId.eq(session_id))
            .filter(timer::entity::WorkSessionTagColumn::TagId.eq(tag.id))
            .one(conn)
            .await
            .map_err(|e| eyre::eyre!("check join: {e}"))?;
        if already.is_some() {
            continue;
        }
        let am = timer::entity::WorkSessionTagActive {
            id: Set(uuid::Uuid::new_v4()),
            work_session_id: Set(session_id),
            tag_id: Set(tag.id),
            created_at: Set(chrono::Utc::now()),
        };
        am.insert(conn)
            .await
            .map_err(|e| eyre::eyre!("insert join: {e}"))?;
    }
    Ok(())
}

/// Resolve the project markdown path from its frontmatter
/// id by scanning `Projects/*.md`. `None` project_id → empty.
fn project_path_for(vault_root: &std::path::Path, project_id: Option<uuid::Uuid>) -> String {
    let Some(pid) = project_id else {
        return String::new();
    };
    let dir = vault_root.join("Projects");
    let Ok(entries) = std::fs::read_dir(&dir) else {
        return String::new();
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) != Some("md") {
            continue;
        }
        let Ok(raw) = std::fs::read_to_string(&path) else {
            continue;
        };
        let rel = path
            .strip_prefix(vault_root)
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_default();
        let basename = path.file_stem().and_then(|s| s.to_str()).unwrap_or("");
        let Ok(p) = project::parse_str(&rel, basename, &raw) else {
            continue;
        };
        if p.id == pid {
            return rel;
        }
    }
    String::new()
}

fn fmt_duration(d: chrono::Duration) -> String {
    let secs = d.num_seconds().max(0);
    let h = secs / 3600;
    let m = (secs % 3600) / 60;
    let s = secs % 60;
    if h > 0 {
        format!("{h}h{m:02}m{s:02}s")
    } else if m > 0 {
        format!("{m}m{s:02}s")
    } else {
        format!("{s}s")
    }
}

fn fmt_money(cents: i64) -> String {
    let neg = cents < 0;
    let abs = cents.unsigned_abs();
    let dollars = abs / 100;
    let frac = abs % 100;
    format!("{}{dollars}.{frac:02}", if neg { "-" } else { "" })
}

fn billed_cents(s: &timer_proto::WorkSession, elapsed: chrono::Duration) -> i64 {
    let secs = elapsed.num_seconds().max(0);
    // rate_cents is per hour; convert seconds → hours via i128 to dodge overflow.
    let cents = (secs as i128) * (s.rate_cents as i128) / 3600_i128;
    cents.try_into().unwrap_or(i64::MAX)
}

async fn run_wiki(cmd: WikiCmd) -> eyre::Result<()> {
    use agent_codex::CodexBackend;
    use agent_wiki::bridge::{IngestRequest, run_ingest};
    use std::time::Duration;
    use wiki_live::WikiLive;

    match cmd {
        WikiCmd::Graph {
            vault,
            query,
            node_type,
            limit,
            json,
        } => {
            let vault = vault
                .canonicalize()
                .map_err(|e| eyre::eyre!("vault {}: {e}", vault.display()))?;
            let opts = wiki_proto::graph::GraphOpts {
                query,
                node_type,
                limit,
                weights: None,
            };
            let graph = wiki_graph::build_graph(&vault, opts)
                .map_err(|e| eyre::eyre!("build_graph: {e}"))?;
            if json {
                let payload = serde_json::json!({
                    "nodes": graph.nodes.iter().map(|n| serde_json::json!({
                        "id": n.id,
                        "label": n.label,
                        "type": n.node_type,
                        "link_count": n.link_count,
                    })).collect::<Vec<_>>(),
                    "edges": graph.edges.iter().map(|e| serde_json::json!({
                        "source": e.source,
                        "target": e.target,
                        "weight": e.weight,
                        "signals": {
                            "direct_link": e.signals.direct_link,
                            "source_overlap": e.signals.source_overlap,
                            "adamic_adar": e.signals.adamic_adar,
                            "type_affinity": e.signals.type_affinity,
                        },
                    })).collect::<Vec<_>>(),
                });
                println!("{}", serde_json::to_string_pretty(&payload)?);
            } else {
                println!("nodes={}  edges={}", graph.nodes.len(), graph.edges.len());
                let mut top = graph.nodes.clone();
                top.sort_by(|a, b| b.link_count.cmp(&a.link_count));
                for n in top.iter().take(20) {
                    println!("  {:3}  [{}]  {}", n.link_count, n.node_type, n.label);
                }
                if graph.nodes.len() > 20 {
                    println!("  … {} more", graph.nodes.len() - 20);
                }
            }
            Ok(())
        }
        WikiCmd::Gaps { vault, json } => {
            let vault = vault
                .canonicalize()
                .map_err(|e| eyre::eyre!("vault {}: {e}", vault.display()))?;
            let gaps = wiki_graph::find_gaps(&vault).map_err(|e| eyre::eyre!("find_gaps: {e}"))?;
            if json {
                let payload: Vec<_> = gaps
                    .iter()
                    .map(|g| {
                        serde_json::json!({
                            "id": g.id,
                            "kind": format!("{:?}", g.kind),
                            "subjects": g.subjects,
                            "explanation": g.explanation,
                        })
                    })
                    .collect();
                println!("{}", serde_json::to_string_pretty(&payload)?);
            } else {
                let orphans = gaps
                    .iter()
                    .filter(|g| matches!(g.kind, wiki_proto::graph::GapKind::Orphan))
                    .count();
                let missing = gaps
                    .iter()
                    .filter(|g| matches!(g.kind, wiki_proto::graph::GapKind::MissingPage))
                    .count();
                println!(
                    "gaps={} (orphans={} missing-pages={})",
                    gaps.len(),
                    orphans,
                    missing
                );
                for g in &gaps {
                    println!("  [{:?}] {}", g.kind, g.explanation);
                }
            }
            Ok(())
        }
        WikiCmd::Search {
            vault,
            query,
            node_type,
            top_k,
            include_content,
            hybrid,
        } => {
            let vault = vault
                .canonicalize()
                .map_err(|e| eyre::eyre!("vault {}: {e}", vault.display()))?;
            let opts = wiki_proto::search::SearchOpts {
                query: query.clone(),
                top_k,
                include_content,
                mode: if hybrid {
                    wiki_proto::search::SearchMode::Hybrid
                } else {
                    wiki_proto::search::SearchMode::Token
                },
                node_type,
            };
            let hits = wiki_search::search(&vault, opts).map_err(|e| eyre::eyre!("search: {e}"))?;
            println!(
                "mode={:?}  token={}  vector={}  total={}",
                hits.mode,
                hits.token_count,
                hits.vector_count,
                hits.hits.len()
            );
            for h in &hits.hits {
                println!("  {:>5.2}  [{}]  {}", h.score, h.path, h.title);
                if !h.snippet.is_empty() {
                    println!("         {}", h.snippet);
                }
            }
            Ok(())
        }
        WikiCmd::Clusters { vault } => {
            let vault = vault
                .canonicalize()
                .map_err(|e| eyre::eyre!("vault {}: {e}", vault.display()))?;
            let clusters = wiki_graph::build_clusters(&vault)
                .map_err(|e| eyre::eyre!("build_clusters: {e}"))?;
            println!("clusters: {}", clusters.len());
            for c in &clusters {
                println!(
                    "  {}  ({:>3} members, cohesion {:.2})  — {}",
                    c.id,
                    c.members.len(),
                    c.cohesion,
                    c.name
                );
            }
            Ok(())
        }
        WikiCmd::WatchSources {
            vault,
            debounce_secs,
            dry_run,
        } => {
            let vault = vault
                .canonicalize()
                .map_err(|e| eyre::eyre!("vault {}: {e}", vault.display()))?;
            let wiki = wiki_live::WikiLive::open(&vault);
            wiki.bootstrap()
                .map_err(|e| eyre::eyre!("bootstrap: {e}"))?;
            let opts = wiki_live::WatchSourcesOpts {
                debounce: Duration::from_secs(debounce_secs),
                auto_enqueue: !dry_run,
            };
            let (rx, _guard) = wiki
                .watch_sources(opts)
                .map_err(|e| eyre::eyre!("watch_sources: {e}"))?;
            eprintln!(
                "Watching {}/Wiki/raw/sources/ (debounce {}s, auto_enqueue={})",
                vault.display(),
                debounce_secs,
                !dry_run
            );
            for event in rx {
                println!(
                    "diff: +{} ~{} -{}  enqueued={}",
                    event.diff.created.len(),
                    event.diff.modified.len(),
                    event.diff.deleted.len(),
                    event.enqueued_task_ids.len(),
                );
                for c in &event.diff.created {
                    println!("  + {c}");
                }
                for m in &event.diff.modified {
                    println!("  ~ {m}");
                }
                for d in &event.diff.deleted {
                    println!("  - {d}");
                }
            }
            Ok(())
        }
        WikiCmd::Health { vault } => {
            let vault = vault
                .canonicalize()
                .map_err(|e| eyre::eyre!("vault {}: {e}", vault.display()))?;
            let wiki = wiki_live::WikiLive::open(&vault);
            let h = wiki.health().map_err(|e| eyre::eyre!("health: {e}"))?;
            println!("bootstrapped:    {}", h.bootstrap_done);
            println!("schema_present:  {}", h.schema_present);
            println!("purpose_present: {}", h.purpose_present);
            println!("pages:           {}", h.page_count);
            println!("sources:         {}", h.source_count);
            println!("queue_depth:     {}", h.queue_depth);
            println!("queue_failed:    {}", h.queue_failed);
            if let Some(t) = h.last_ingest_at {
                println!("last_ingest_at:  {t}");
            }
            if let Some(t) = h.last_rescan_at {
                println!("last_rescan_at:  {t}");
            }
            Ok(())
        }
        WikiCmd::Import {
            vault,
            dir,
            flatten,
            ext,
        } => {
            let vault = vault
                .canonicalize()
                .map_err(|e| eyre::eyre!("vault {}: {e}", vault.display()))?;
            let wiki = wiki_live::WikiLive::open(&vault);
            wiki.bootstrap()
                .map_err(|e| eyre::eyre!("bootstrap: {e}"))?;
            let opts = wiki_live::ImportFolderOpts {
                preserve_structure: !flatten,
                include_exts: ext.split(',').map(|s| s.trim().to_lowercase()).collect(),
                exclude_substrings: vec![".git/".into(), "node_modules/".into(), "target/".into()],
            };
            let refs = wiki
                .import_folder(&dir, opts)
                .map_err(|e| eyre::eyre!("import_folder: {e}"))?;
            println!("Imported {} file(s) from {}", refs.len(), dir.display());
            for r in refs.iter().take(40) {
                println!("  {} ({} bytes)", r.path, r.size);
            }
            if refs.len() > 40 {
                println!("  … {} more", refs.len() - 40);
            }
            Ok(())
        }
        WikiCmd::Rescan { vault, enqueue } => {
            let vault = vault
                .canonicalize()
                .map_err(|e| eyre::eyre!("vault {}: {e}", vault.display()))?;
            let wiki = wiki_live::WikiLive::open(&vault);
            let diff = wiki
                .rescan_sources()
                .map_err(|e| eyre::eyre!("rescan: {e}"))?;
            println!(
                "created={} modified={} deleted={}",
                diff.created.len(),
                diff.modified.len(),
                diff.deleted.len()
            );
            for c in &diff.created {
                println!("  + {c}");
            }
            for m in &diff.modified {
                println!("  ~ {m}");
            }
            for d in &diff.deleted {
                println!("  - {d}");
            }
            if enqueue {
                let mut count = 0;
                for c in diff.created.iter().chain(diff.modified.iter()) {
                    let abs = wiki.wiki_root().join(c);
                    let bytes = std::fs::read(&abs)?;
                    let kind = if diff.created.contains(c) {
                        wiki_live::queue::SourceChange::Created
                    } else {
                        wiki_live::queue::SourceChange::Modified
                    };
                    wiki.enqueue_ingest(c, kind, &bytes)
                        .map_err(|e| eyre::eyre!("enqueue {c}: {e}"))?;
                    count += 1;
                }
                println!("enqueued {count} ingest task(s)");
            }
            Ok(())
        }
        WikiCmd::Lint {
            vault,
            model,
            timeout_secs,
            language,
        } => {
            let vault = vault
                .canonicalize()
                .map_err(|e| eyre::eyre!("vault {}: {e}", vault.display()))?;
            let wiki = wiki_live::WikiLive::open(&vault);
            wiki.bootstrap()
                .map_err(|e| eyre::eyre!("bootstrap: {e}"))?;
            let backend = agent_codex::CodexBackend::new();
            let req = agent_wiki::bridge::LintRequest {
                model,
                timeout: Duration::from_secs(timeout_secs),
                language,
            };
            let raised = agent_wiki::bridge::run_lint(&backend, &wiki, req)
                .await
                .map_err(|e| eyre::eyre!("lint: {e}"))?;
            println!("New findings: {}", raised.len());
            for f in &raised {
                println!("  [{:?} {:?}] {}", f.kind, f.severity, f.title);
            }
            Ok(())
        }
        WikiCmd::Findings { vault } => {
            let vault = vault
                .canonicalize()
                .map_err(|e| eyre::eyre!("vault {}: {e}", vault.display()))?;
            let wiki = wiki_live::WikiLive::open(&vault);
            let open = wiki
                .list_findings(Some(wiki_live::FindingStatus::Open))
                .map_err(|e| eyre::eyre!("list_findings: {e}"))?;
            println!("Open findings: {}", open.len());
            for f in &open {
                println!("  {}  [{:?} {:?}] {}", f.id, f.kind, f.severity, f.title);
                if !f.pages.is_empty() {
                    println!("      pages: {}", f.pages.join(", "));
                }
            }
            Ok(())
        }
        WikiCmd::LintTiers { org_root } => {
            let org_root = if let Some(p) = org_root {
                p
            } else {
                let ctx = org_ctx::resolve_active(None)?;
                ctx.root.path().to_path_buf()
            };
            let violations = wiki_graph::lint_org_tree(&org_root);
            if violations.is_empty() {
                println!("OK — no tier violations in {}", org_root.display());
                return Ok(());
            }
            println!(
                "{} violation{} in {}\n",
                violations.len(),
                if violations.len() == 1 { "" } else { "s" },
                org_root.display(),
            );
            for v in &violations {
                println!(
                    "  {} ({}) → [[{}]] resolves to {} ({})",
                    v.source.display(),
                    v.source_tier.as_str(),
                    v.target_link,
                    v.resolved.display(),
                    v.resolved_tier.as_str(),
                );
            }
            std::process::exit(1);
        }
        WikiCmd::Dedup {
            vault,
            model,
            timeout_secs,
        } => {
            let vault = vault
                .canonicalize()
                .map_err(|e| eyre::eyre!("vault {}: {e}", vault.display()))?;
            let wiki = wiki_live::WikiLive::open(&vault);
            let backend = agent_codex::CodexBackend::new();
            let groups = agent_wiki::bridge::run_dedup_detect(
                &backend,
                &wiki,
                model,
                Duration::from_secs(timeout_secs),
            )
            .await
            .map_err(|e| eyre::eyre!("dedup_detect: {e}"))?;
            println!("Dedup groups: {}", groups.len());
            for g in &groups {
                println!(
                    "  [{:?}] {} — {}",
                    g.confidence,
                    g.slugs.join(", "),
                    g.reason
                );
            }
            Ok(())
        }
        WikiCmd::Research {
            vault,
            gap_kind,
            gap_title,
            gap_description,
            model,
            timeout_secs,
            language,
        } => {
            let vault = vault
                .canonicalize()
                .map_err(|e| eyre::eyre!("vault {}: {e}", vault.display()))?;
            let wiki = wiki_live::WikiLive::open(&vault);
            let backend = agent_codex::CodexBackend::new();
            let plan = agent_wiki::bridge::run_propose_research(
                &backend,
                &wiki,
                &gap_kind,
                &gap_title,
                &gap_description,
                model,
                Duration::from_secs(timeout_secs),
                &language,
            )
            .await
            .map_err(|e| eyre::eyre!("propose_research: {e}"))?;
            println!("TOPIC: {}", plan.topic);
            for q in &plan.queries {
                println!("QUERY: {q}");
            }
            Ok(())
        }
        WikiCmd::Ingest {
            vault,
            source,
            filename,
            mime,
            title,
            model,
            language,
            timeout_secs,
        } => {
            let vault = vault
                .canonicalize()
                .map_err(|e| eyre::eyre!("vault {}: {e}", vault.display()))?;
            let bytes = std::fs::read(&source)
                .map_err(|e| eyre::eyre!("read source {}: {e}", source.display()))?;
            let fname = filename.unwrap_or_else(|| {
                source
                    .file_name()
                    .and_then(|s| s.to_str())
                    .unwrap_or("source.md")
                    .to_string()
            });
            let wiki = WikiLive::open(&vault);
            let backend = CodexBackend::new();
            let req = IngestRequest {
                source_filename: fname,
                source_mime: mime,
                source_title: title.unwrap_or_default(),
                source_bytes: bytes,
                model: model.clone(),
                timeout: Duration::from_secs(timeout_secs),
                language,
            };
            eprintln!(
                "› ingest@{} vault={}",
                model.as_deref().unwrap_or("default"),
                vault.display()
            );
            let result = run_ingest(&backend, &wiki, req)
                .await
                .map_err(|e| eyre::eyre!("ingest: {e}"))?;
            println!("Ingest done.");
            println!("  task:   {}", result.task_id);
            println!("  source: {}", result.raw_source_path);
            println!("  pages:  {}", result.pages_written.len());
            for p in &result.pages_written {
                println!("    - {p}");
            }
            if !result.reviews_raised.is_empty() {
                println!("  reviews:");
                for r in &result.reviews_raised {
                    println!("    - {r}");
                }
            }
            Ok(())
        }
    }
}

async fn run_agent(cmd: AgentCmd) -> eyre::Result<()> {
    use std::io::Write;

    use agent_codex::{ChatOpts, CodexBackend};
    use agent_proto::event::AgentEvent;
    use futures::StreamExt;

    match cmd {
        AgentCmd::Chat {
            workspace,
            model,
            effort,
            access_mode,
            codex_bin,
            codex_home,
            timeout_secs,
            message,
        } => {
            let workspace = workspace
                .canonicalize()
                .map_err(|e| eyre::eyre!("workspace {}: {e}", workspace.display()))?;
            let backend = CodexBackend::new();
            let opts = ChatOpts {
                codex_bin,
                codex_args: None,
                codex_home,
                model: model.clone(),
                effort,
                access_mode,
            };
            eprintln!(
                "› codex@{} workspace={}",
                model.as_deref().unwrap_or("default"),
                workspace.display()
            );
            let handle = backend
                .chat(workspace, message, opts)
                .await
                .map_err(|e| eyre::eyre!("chat: {e}"))?;
            eprintln!(
                "  session={} thread={}",
                handle.session_id, handle.thread_id
            );
            let mut events = handle.events;
            let mut stdout = std::io::stdout().lock();
            let deadline =
                tokio::time::Instant::now() + tokio::time::Duration::from_secs(timeout_secs);
            loop {
                let next = tokio::time::timeout_at(deadline, events.next()).await;
                match next {
                    Err(_) => {
                        eprintln!("\n(turn timed out after {timeout_secs}s)");
                        break;
                    }
                    Ok(None) => break,
                    Ok(Some(AgentEvent::MessageDelta { content_delta, .. })) => {
                        write!(stdout, "{content_delta}")?;
                        stdout.flush()?;
                    }
                    Ok(Some(AgentEvent::TurnFinished { .. })) => {
                        writeln!(stdout)?;
                        break;
                    }
                    Ok(Some(AgentEvent::TurnErrored { kind, message, .. })) => {
                        writeln!(stdout)?;
                        eprintln!("(turn error: {kind}: {message})");
                        break;
                    }
                    Ok(Some(_)) => {}
                }
            }
            Ok(())
        }
    }
}

async fn run_task(cmd: TaskCmd) -> eyre::Result<()> {
    match cmd {
        TaskCmd::Capture {
            text,
            project,
            milestone,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let mut info = task::capture(&text);
            info.path = task::write::default_task_path(&info.title, None);
            if let Some(p) = project {
                let pc = connect_project_client(&url).await?;
                info.project_id = Some(resolve_project_target(&pc, &p).await?.id);
            }
            if let Some(m) = milestone {
                let mc = connect_milestone_client(&url).await?;
                let ms = resolve_milestone_target(&mc, &m).await?;
                info.milestone_id = Some(ms.id);
                if info.project_id.is_none() {
                    info.project_id = Some(ms.project_id);
                }
            }
            let client = connect_task_client(&url).await?;
            let created = client
                .create(info)
                .await
                .map_err(|e| eyre::eyre!("create: {e:?}"))?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&created).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            println!("captured {} ({})", created.title, created.path);
            println!("  id:       {}", created.id);
            println!("  status:   {}", created.status);
            println!("  priority: {}", created.priority);
            if let Some(d) = &created.due {
                println!("  due:      {d}");
            }
        }
        TaskCmd::List {
            status,
            tag,
            context,
            project,
            milestone,
            open,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&url).await?;
            let rows = client
                .list()
                .await
                .map_err(|e| eyre::eyre!("list: {e:?}"))?;
            let ctx_filter = context.map(|c| {
                if c.starts_with('@') {
                    c
                } else {
                    format!("@{c}")
                }
            });
            let project_id = match project {
                Some(p) => {
                    let pc = connect_project_client(&url).await?;
                    Some(resolve_project_target(&pc, &p).await?.id)
                }
                None => None,
            };
            let milestone_filter = match milestone.as_deref() {
                Some("none" | "null") => Some(None),
                Some(m) => {
                    let mc = connect_milestone_client(&url).await?;
                    Some(Some(resolve_milestone_target(&mc, m).await?.id))
                }
                None => None,
            };

            let mut rows: Vec<_> = rows
                .into_iter()
                .filter(|t| {
                    status
                        .as_deref()
                        .is_none_or(|s| t.status.eq_ignore_ascii_case(s))
                })
                .filter(|t| {
                    tag.as_deref()
                        .is_none_or(|tg| t.tags.iter().any(|x| x == tg))
                })
                .filter(|t| {
                    ctx_filter
                        .as_deref()
                        .is_none_or(|c| t.contexts.iter().any(|x| x == c))
                })
                .filter(|t| project_id.is_none_or(|pid| t.project_id == Some(pid)))
                .filter(|t| match &milestone_filter {
                    None => true,
                    Some(want) => &t.milestone_id == want,
                })
                .filter(|t| {
                    !open || !task::Status::from_str(&t.status).is_some_and(task::Status::is_done)
                })
                .collect();
            rows.sort_by(|a, b| {
                let a_done = task::Status::from_str(&a.status).is_some_and(task::Status::is_done);
                let b_done = task::Status::from_str(&b.status).is_some_and(task::Status::is_done);
                a_done
                    .cmp(&b_done)
                    .then_with(|| a.due.is_none().cmp(&b.due.is_none()))
                    .then_with(|| a.due.cmp(&b.due))
                    .then_with(|| a.title.cmp(&b.title))
            });

            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&rows).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            if rows.is_empty() {
                println!("(no tasks)");
                return Ok(());
            }
            for t in &rows {
                let marker = if task::Status::from_str(&t.status).is_some_and(task::Status::is_done)
                {
                    "[x]"
                } else {
                    "[ ]"
                };
                let due = t
                    .due
                    .as_deref()
                    .map(|d| format!(" (due {d})"))
                    .unwrap_or_default();
                let prio = match t.priority.as_str() {
                    "critical" => " !!",
                    "high" => " !",
                    _ => "",
                };
                let ms = if t.milestone_id.is_some() { " *" } else { "" };
                println!("{marker} {}{prio}{due}{ms}    {}", t.title, t.path);
            }
        }
        TaskCmd::Get {
            target,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&url).await?;
            let t = resolve_task_target(&client, &target).await?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&t).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            println!("{} [{}]\n", t.title, t.status);
            println!("  id:       {}", t.id);
            println!("  path:     {}", t.path);
            println!("  priority: {}", t.priority);
            if let Some(d) = &t.due {
                println!("  due:      {d}");
            }
            if let Some(s) = &t.scheduled {
                println!("  sched:    {s}");
            }
            if let Some(p) = t.project_id {
                println!("  project:  {p}");
            }
            if let Some(m) = t.milestone_id {
                println!("  milestone:{m}");
            }
            if !t.tags.is_empty() {
                println!("  tags:     {}", t.tags.join(", "));
            }
            if !t.contexts.is_empty() {
                println!("  contexts: {}", t.contexts.join(", "));
            }
            if !t.details.is_empty() {
                println!("\n{}", t.details);
            }
        }
        TaskCmd::Create {
            title,
            path,
            status,
            priority,
            due,
            scheduled,
            tags,
            contexts,
            project,
            milestone,
            details,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let project_id = match project {
                Some(p) => {
                    let pc = connect_project_client(&url).await?;
                    Some(resolve_project_target(&pc, &p).await?.id)
                }
                None => None,
            };
            let (milestone_id, project_id) = match milestone {
                Some(m) => {
                    let mc = connect_milestone_client(&url).await?;
                    let ms = resolve_milestone_target(&mc, &m).await?;
                    (Some(ms.id), project_id.or(Some(ms.project_id)))
                }
                None => (None, project_id),
            };
            let details = resolve_body(details)?;
            let new_task = task::TaskInfo {
                id: uuid::Uuid::nil(),
                path: path.unwrap_or_default(),
                title,
                status: status.unwrap_or_else(|| "open".into()),
                priority: priority.unwrap_or_else(|| "normal".into()),
                due,
                scheduled,
                tags: task::model::StringList(tags),
                contexts: task::model::StringList(contexts),
                projects: task::model::StringList::default(),
                project_id,
                milestone_id,
                time_estimate: None,
                time_entries: task::model::TimeEntries::default(),
                recurrence: None,
                recurrence_anchor: None,
                complete_instances: task::model::StringList::default(),
                completed_date: None,
                agent_profile: String::new(),
                dispatched_agent_tasks: task::model::StringList::default(),
                date_created: None,
                date_modified: None,
                details,
            };
            let client = connect_task_client(&url).await?;
            let created = client
                .create(new_task)
                .await
                .map_err(|e| eyre::eyre!("create: {e:?}"))?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&created).map_err(|e| eyre::eyre!("json: {e}"))?
                );
            } else {
                println!("created {} ({})", created.title, created.path);
                println!("  id: {}", created.id);
            }
        }
        TaskCmd::Done {
            target,
            undo,
            org,
            server,
        } => {
            mutate_task(target, org, server, |t| {
                if undo {
                    t.status = "open".into();
                    t.completed_date = None;
                } else {
                    t.status = "done".into();
                    t.completed_date = Some(chrono::Local::now().date_naive());
                }
            })
            .await?;
        }
        TaskCmd::SetStatus {
            target,
            status,
            org,
            server,
        } => mutate_task(target, org, server, |t| t.status = status).await?,
        TaskCmd::SetPriority {
            target,
            priority,
            org,
            server,
        } => mutate_task(target, org, server, |t| t.priority = priority).await?,
        TaskCmd::SetDue {
            target,
            due,
            org,
            server,
        } => {
            let v = if matches!(due.as_str(), "none" | "null" | "") {
                None
            } else {
                Some(due)
            };
            mutate_task(target, org, server, |t| t.due = v).await?;
        }
        TaskCmd::SetScheduled {
            target,
            scheduled,
            org,
            server,
        } => {
            let v = if matches!(scheduled.as_str(), "none" | "null" | "") {
                None
            } else {
                Some(scheduled)
            };
            mutate_task(target, org, server, |t| t.scheduled = v).await?;
        }
        TaskCmd::SetProject {
            target,
            project,
            org,
            server,
        } => {
            let slug = resolve_active_org(org.clone())?;
            let url = resolve_org_vox_url(server.clone(), &slug);
            let new_proj = if matches!(project.as_str(), "none" | "null" | "") {
                None
            } else {
                let pc = connect_project_client(&url).await?;
                Some(resolve_project_target(&pc, &project).await?.id)
            };
            mutate_task(target, org, server, |t| t.project_id = new_proj).await?;
        }
        TaskCmd::SetMilestone {
            target,
            milestone,
            org,
            server,
        } => {
            let slug = resolve_active_org(org.clone())?;
            let url = resolve_org_vox_url(server.clone(), &slug);
            let (new_ms, new_proj) = if matches!(milestone.as_str(), "none" | "null" | "") {
                (None, None)
            } else {
                let mc = connect_milestone_client(&url).await?;
                let ms = resolve_milestone_target(&mc, &milestone).await?;
                (Some(ms.id), Some(ms.project_id))
            };
            mutate_task(target, org, server, |t| {
                t.milestone_id = new_ms;
                if let Some(p) = new_proj {
                    // Auto-fix project link when it's missing or
                    // points elsewhere — milestone is the
                    // narrower truth.
                    t.project_id = Some(p);
                }
            })
            .await?;
        }
        TaskCmd::SetTags {
            target,
            tags,
            org,
            server,
        } => {
            mutate_task(target, org, server, |t| {
                t.tags = task::model::StringList(tags);
            })
            .await?;
        }
        TaskCmd::Rename {
            target,
            new_path,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&url).await?;
            let t = resolve_task_target(&client, &target).await?;
            let renamed = client
                .rename(t.id, new_path)
                .await
                .map_err(|e| eyre::eyre!("rename: {e:?}"))?;
            println!("renamed → {}", renamed.path);
        }
        TaskCmd::Delete {
            target,
            yes,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&url).await?;
            let t = resolve_task_target(&client, &target).await?;
            if !yes && !confirm(&format!("delete `{}` ({})?", t.title, t.path))? {
                println!("aborted");
                return Ok(());
            }
            client
                .delete(t.id)
                .await
                .map_err(|e| eyre::eyre!("delete: {e:?}"))?;
            println!("deleted {}", t.path);
        }
    }
    Ok(())
}

async fn connect_task_client(url: &str) -> eyre::Result<task::TaskServiceClient> {
    Box::pin(vox::connect(url).establish())
        .await
        .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))
}

async fn resolve_task_target(
    client: &task::TaskServiceClient,
    target: &str,
) -> eyre::Result<task::TaskInfo> {
    if let Ok(id) = uuid::Uuid::parse_str(target) {
        return client
            .get(id)
            .await
            .map_err(|e| eyre::eyre!("get(id): {e:?}"));
    }
    client
        .get_by_path(target.to_owned())
        .await
        .map_err(|e| eyre::eyre!("get(path): {e:?}"))
}

async fn mutate_task<F>(
    target: String,
    org: Option<String>,
    server: Option<String>,
    apply: F,
) -> eyre::Result<()>
where
    F: FnOnce(&mut task::TaskInfo),
{
    let slug = resolve_active_org(org)?;
    let url = resolve_org_vox_url(server, &slug);
    let client = connect_task_client(&url).await?;
    let mut t = resolve_task_target(&client, &target).await?;
    apply(&mut t);
    let updated = client
        .update(t)
        .await
        .map_err(|e| eyre::eyre!("update: {e:?}"))?;
    println!("{}  [{}]  {}", updated.title, updated.status, updated.path);
    Ok(())
}

async fn run_milestone(cmd: MilestoneCmd) -> eyre::Result<()> {
    match cmd {
        MilestoneCmd::List {
            project,
            goal,
            open,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_milestone_client(&url).await?;
            let project_id = match project {
                Some(p) => {
                    let pc = connect_project_client(&url).await?;
                    Some(resolve_project_target(&pc, &p).await?.id)
                }
                None => None,
            };
            let goal_id = match goal {
                Some(g) => {
                    let gc = connect_goal_client(&url).await?;
                    Some(resolve_goal_target(&gc, &g).await?.id)
                }
                None => None,
            };
            let rows: Vec<_> = client
                .list()
                .await
                .map_err(|e| eyre::eyre!("list: {e:?}"))?
                .into_iter()
                .filter(|m| project_id.is_none_or(|pid| m.project_id == pid))
                .filter(|m| goal_id.is_none_or(|gid| m.goal_id == Some(gid)))
                .filter(|m| !open || m.status != "closed")
                .collect();
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&rows).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            if rows.is_empty() {
                println!("(no milestones)");
                return Ok(());
            }
            println!("{} milestones\n", rows.len());
            for m in &rows {
                let due = m
                    .due_date
                    .map(|d| format!("  (due {d})"))
                    .unwrap_or_default();
                let goal = m.goal_id.map(|_| "  →goal".to_string()).unwrap_or_default();
                println!("{:<32}  {:<8}{due}{goal}    {}", m.title, m.status, m.path);
            }
        }
        MilestoneCmd::Get {
            target,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_milestone_client(&url).await?;
            let m = resolve_milestone_target(&client, &target).await?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&m).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            println!("{} [{}]\n", m.title, m.status);
            println!("  id:       {}", m.id);
            println!("  path:     {}", m.path);
            println!("  project:  {}", m.project_id);
            if let Some(g) = m.goal_id {
                println!("  goal:     {g}");
            }
            if let Some(d) = m.due_date {
                println!("  due:      {d}");
            }
            if let Some(r) = &m.forge_ref {
                println!("  forge:    {r}");
            }
            if !m.tags.is_empty() {
                println!("  tags:     {}", m.tags.0.join(", "));
            }
            if !m.details.is_empty() {
                println!("\n{}", m.details);
            }
        }
        MilestoneCmd::Create {
            title,
            project,
            goal,
            path,
            status,
            due,
            tags,
            forge_ref,
            details,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let pc = connect_project_client(&url).await?;
            let project_id = resolve_project_target(&pc, &project).await?.id;
            let goal_id = match goal {
                None => None,
                Some(g) => {
                    let gc = connect_goal_client(&url).await?;
                    Some(resolve_goal_target(&gc, &g).await?.id)
                }
            };
            let due_date = match due {
                None => None,
                Some(s) => Some(
                    chrono::NaiveDate::parse_from_str(&s, "%Y-%m-%d")
                        .map_err(|e| eyre::eyre!("--due: {e}"))?,
                ),
            };
            let details = resolve_body(details)?;
            let new_ms = milestone::Milestone {
                id: uuid::Uuid::nil(),
                path: path.unwrap_or_default(),
                title,
                project_id,
                goal_id,
                status: status.unwrap_or_else(|| "open".into()),
                due_date,
                tags: milestone::Tags(tags),
                forge_ref,
                date_created: None,
                date_modified: None,
                details,
            };
            let client = connect_milestone_client(&url).await?;
            let created = client
                .create(new_ms)
                .await
                .map_err(|e| eyre::eyre!("create: {e:?}"))?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&created).map_err(|e| eyre::eyre!("json: {e}"))?
                );
            } else {
                println!("created {} ({})", created.title, created.path);
                println!("  id: {}", created.id);
            }
        }
        MilestoneCmd::SetStatus {
            target,
            status,
            org,
            server,
        } => mutate_milestone(target, org, server, |m| m.status = status).await?,
        MilestoneCmd::SetDue {
            target,
            due,
            org,
            server,
        } => {
            let v = if matches!(due.as_str(), "none" | "null" | "") {
                None
            } else {
                Some(
                    chrono::NaiveDate::parse_from_str(&due, "%Y-%m-%d")
                        .map_err(|e| eyre::eyre!("--due: {e}"))?,
                )
            };
            mutate_milestone(target, org, server, |m| m.due_date = v).await?;
        }
        MilestoneCmd::SetGoal {
            target,
            goal,
            org,
            server,
        } => {
            let slug = resolve_active_org(org.clone())?;
            let url = resolve_org_vox_url(server.clone(), &slug);
            let new_goal = if matches!(goal.as_str(), "none" | "null" | "") {
                None
            } else {
                let gc = connect_goal_client(&url).await?;
                Some(resolve_goal_target(&gc, &goal).await?.id)
            };
            mutate_milestone(target, org, server, |m| m.goal_id = new_goal).await?;
        }
        MilestoneCmd::SetForgeRef {
            target,
            forge_ref,
            org,
            server,
        } => {
            let v = if matches!(forge_ref.as_str(), "none" | "null" | "") {
                None
            } else {
                Some(forge_ref)
            };
            mutate_milestone(target, org, server, |m| m.forge_ref = v).await?;
        }
        MilestoneCmd::Close {
            target,
            org,
            server,
        } => mutate_milestone(target, org, server, |m| m.status = "closed".into()).await?,
        MilestoneCmd::Reopen {
            target,
            org,
            server,
        } => mutate_milestone(target, org, server, |m| m.status = "open".into()).await?,
        MilestoneCmd::Rename {
            target,
            new_path,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_milestone_client(&url).await?;
            let m = resolve_milestone_target(&client, &target).await?;
            let renamed = client
                .rename(m.id, new_path)
                .await
                .map_err(|e| eyre::eyre!("rename: {e:?}"))?;
            println!("renamed → {}", renamed.path);
        }
        MilestoneCmd::Delete {
            target,
            yes,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_milestone_client(&url).await?;
            let m = resolve_milestone_target(&client, &target).await?;
            if !yes && !confirm(&format!("delete `{}` ({})?", m.title, m.path))? {
                println!("aborted");
                return Ok(());
            }
            client
                .delete(m.id)
                .await
                .map_err(|e| eyre::eyre!("delete: {e:?}"))?;
            println!("deleted {}", m.path);
        }
    }
    Ok(())
}

async fn connect_milestone_client(url: &str) -> eyre::Result<milestone::MilestoneServiceClient> {
    Box::pin(vox::connect(url).establish())
        .await
        .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))
}

async fn resolve_milestone_target(
    client: &milestone::MilestoneServiceClient,
    target: &str,
) -> eyre::Result<milestone::Milestone> {
    if let Ok(id) = uuid::Uuid::parse_str(target) {
        return client
            .get(id)
            .await
            .map_err(|e| eyre::eyre!("get(id): {e:?}"));
    }
    client
        .get_by_path(target.to_owned())
        .await
        .map_err(|e| eyre::eyre!("get(path): {e:?}"))
}

async fn mutate_milestone<F>(
    target: String,
    org: Option<String>,
    server: Option<String>,
    apply: F,
) -> eyre::Result<()>
where
    F: FnOnce(&mut milestone::Milestone),
{
    let slug = resolve_active_org(org)?;
    let url = resolve_org_vox_url(server, &slug);
    let client = connect_milestone_client(&url).await?;
    let mut m = resolve_milestone_target(&client, &target).await?;
    apply(&mut m);
    let updated = client
        .update(m)
        .await
        .map_err(|e| eyre::eyre!("update: {e:?}"))?;
    println!("{}  [{}]  {}", updated.title, updated.status, updated.path);
    Ok(())
}

// ── Location (locations::Store) ──────────────────────────────────────

async fn connect_locations_client(url: &str) -> eyre::Result<locations::LocationsServiceClient> {
    Box::pin(vox::connect(url).establish())
        .await
        .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))
}

async fn resolve_location_target(
    client: &locations::LocationsServiceClient,
    target: &str,
) -> eyre::Result<locations::Location> {
    if uuid::Uuid::parse_str(target).is_ok() {
        return client
            .get(target.to_owned())
            .await
            .map_err(|e| eyre::eyre!("get: {e:?}"));
    }
    let rows = client
        .list()
        .await
        .map_err(|e| eyre::eyre!("list: {e:?}"))?;
    rows.into_iter()
        .find(|l| l.path == target || l.name == target)
        .ok_or_else(|| eyre::eyre!("not found: {target}"))
}

async fn run_location(cmd: LocationCmd) -> eyre::Result<()> {
    match cmd {
        LocationCmd::List {
            kind,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_locations_client(&u).await?;
            let rows: Vec<_> = client
                .list()
                .await
                .map_err(|e| eyre::eyre!("list: {e:?}"))?
                .into_iter()
                .filter(|l| kind.as_deref().is_none_or(|k| l.kind == k))
                .collect();
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&rows).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            for l in &rows {
                let addr = l
                    .address
                    .as_deref()
                    .map(|a| format!("  ({a})"))
                    .unwrap_or_default();
                println!("{:<28}  {:<8}{addr}    {}", l.name, l.kind, l.path);
            }
        }
        LocationCmd::Get {
            target,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_locations_client(&u).await?;
            let l = resolve_location_target(&client, &target).await?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&l).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            println!("{} [{}]\n", l.name, l.kind);
            println!("  id:       {}", l.id);
            println!("  path:     {}", l.path);
            if let Some(a) = &l.address {
                println!("  address:  {a}");
            }
            if !l.tags.0.is_empty() {
                println!("  tags:     {}", l.tags.0.join(", "));
            }
            if !l.details.is_empty() {
                println!("\n{}", l.details);
            }
        }
        LocationCmd::Create {
            name,
            kind,
            parent,
            address,
            tags,
            details,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_locations_client(&u).await?;
            let parent_id = match parent {
                None => None,
                Some(p) => Some(resolve_location_target(&client, &p).await?.id),
            };
            let new_loc = locations::Location {
                id: uuid::Uuid::nil(),
                path: String::new(),
                name,
                kind: kind.unwrap_or_else(|| "other".into()),
                parent_id,
                address,
                tags: locations::model::Tags(tags),
                same_as: None,
                date_created: None,
                date_modified: None,
                details: resolve_body(details)?,
            };
            let created = client
                .create(new_loc)
                .await
                .map_err(|e| eyre::eyre!("create: {e:?}"))?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&created).map_err(|e| eyre::eyre!("json: {e}"))?
                );
            } else {
                println!("created {} ({})", created.name, created.path);
                println!("  id: {}", created.id);
            }
        }
        LocationCmd::Rename {
            target,
            new_path,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_locations_client(&u).await?;
            let l = resolve_location_target(&client, &target).await?;
            let renamed = client
                .rename(l.id.to_string(), new_path)
                .await
                .map_err(|e| eyre::eyre!("rename: {e:?}"))?;
            println!("renamed → {}", renamed.path);
        }
        LocationCmd::Delete {
            target,
            yes,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_locations_client(&u).await?;
            let l = resolve_location_target(&client, &target).await?;
            if !yes && !confirm(&format!("delete `{}` ({})?", l.name, l.path))? {
                println!("aborted");
                return Ok(());
            }
            client
                .delete(l.id.to_string())
                .await
                .map_err(|e| eyre::eyre!("delete: {e:?}"))?;
            println!("deleted {}", l.path);
        }
    }
    Ok(())
}

// ── Recipe (cookbook::Store) — read + delete only ─────────────────────

async fn connect_cookbook_client(url: &str) -> eyre::Result<cookbook::CookbookServiceClient> {
    Box::pin(vox::connect(url).establish())
        .await
        .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))
}

async fn run_recipe(cmd: RecipeCmd) -> eyre::Result<()> {
    match cmd {
        RecipeCmd::List {
            query,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_cookbook_client(&u).await?;
            let rows: Vec<_> = client
                .list()
                .await
                .map_err(|e| eyre::eyre!("list: {e:?}"))?
                .into_iter()
                .filter(|r| {
                    query
                        .as_deref()
                        .is_none_or(|q| r.name.to_lowercase().contains(&q.to_lowercase()))
                })
                .collect();
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&rows).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            println!("{} recipes\n", rows.len());
            for r in &rows {
                let s = r
                    .servings
                    .map(|n| format!("  ({n} srv)"))
                    .unwrap_or_default();
                println!("{:<40}{s}    {}", r.name, r.path);
            }
        }
        RecipeCmd::Get {
            path,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_cookbook_client(&u).await?;
            let r = client
                .get(path)
                .await
                .map_err(|e| eyre::eyre!("get: {e:?}"))?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&r).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            println!("{}\n", r.name);
            println!("  path:     {}", r.path);
            if let Some(s) = r.servings {
                println!("  servings: {s}");
            }
            if !r.ingredients.0.is_empty() {
                println!("  ingredients ({} items):", r.ingredients.0.len());
                for i in r.ingredients.0.iter().take(20) {
                    println!("    - {} {} {}", i.qty.unwrap_or(0.0), i.unit, i.name);
                }
            }
        }
        RecipeCmd::Delete {
            path,
            yes,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_cookbook_client(&u).await?;
            if !yes && !confirm(&format!("delete `{path}`?"))? {
                println!("aborted");
                return Ok(());
            }
            client
                .delete(path.clone())
                .await
                .map_err(|e| eyre::eyre!("delete: {e:?}"))?;
            println!("deleted {path}");
        }
    }
    Ok(())
}

// ── Meal (mealplan::Store) ───────────────────────────────────────────

async fn connect_mealplan_client(url: &str) -> eyre::Result<mealplan::MealplanServiceClient> {
    Box::pin(vox::connect(url).establish())
        .await
        .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))
}

async fn resolve_meal_target(
    client: &mealplan::MealplanServiceClient,
    target: &str,
) -> eyre::Result<mealplan::Meal> {
    if uuid::Uuid::parse_str(target).is_ok() {
        return client
            .get(target.to_owned())
            .await
            .map_err(|e| eyre::eyre!("get: {e:?}"));
    }
    let rows = client
        .list()
        .await
        .map_err(|e| eyre::eyre!("list: {e:?}"))?;
    rows.into_iter()
        .find(|m| m.path == target)
        .ok_or_else(|| eyre::eyre!("not found: {target}"))
}

async fn run_meal(cmd: MealCmd) -> eyre::Result<()> {
    match cmd {
        MealCmd::List {
            date,
            status,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_mealplan_client(&u).await?;
            let parsed_date = match date {
                None => None,
                Some(s) => Some(
                    chrono::NaiveDate::parse_from_str(&s, "%Y-%m-%d")
                        .map_err(|e| eyre::eyre!("--date: {e}"))?,
                ),
            };
            let rows: Vec<_> = client
                .list()
                .await
                .map_err(|e| eyre::eyre!("list: {e:?}"))?
                .into_iter()
                .filter(|m| parsed_date.is_none_or(|d| m.scheduled_for == d))
                .filter(|m| status.as_deref().is_none_or(|s| m.status == s))
                .collect();
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&rows).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            for m in &rows {
                println!(
                    "{}  {}  {:<10}  {:<10}    {}",
                    m.scheduled_for, m.slot, m.status, m.name, m.path
                );
            }
        }
        MealCmd::Get {
            target,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_mealplan_client(&u).await?;
            let m = resolve_meal_target(&client, &target).await?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&m).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            println!("{} [{}]\n", m.name, m.status);
            println!("  id:       {}", m.id);
            println!("  path:     {}", m.path);
            println!("  date:     {}", m.scheduled_for);
            println!("  slot:     {}", m.slot);
            println!("  servings: {}", m.servings);
            for r in m.recipe_paths.iter() {
                println!("  recipe:   {r}");
            }
        }
        MealCmd::Create {
            name,
            date,
            slot,
            recipe,
            servings,
            tags,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_mealplan_client(&u).await?;
            let scheduled_for = chrono::NaiveDate::parse_from_str(&date, "%Y-%m-%d")
                .map_err(|e| eyre::eyre!("--date: {e}"))?;
            let new_meal = mealplan::Meal {
                id: uuid::Uuid::nil(),
                path: String::new(),
                name,
                scheduled_for,
                slot: slot.unwrap_or_else(|| "dinner".into()),
                servings,
                recipe_paths: mealplan::model::StringList(recipe),
                status: "planned".into(),
                pantry_deductions: mealplan::model::PantryDeductions::default(),
                tags: mealplan::model::StringList(tags),
                date_created: None,
                date_modified: None,
                details: String::new(),
            };
            let created = client
                .create(new_meal)
                .await
                .map_err(|e| eyre::eyre!("create: {e:?}"))?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&created).map_err(|e| eyre::eyre!("json: {e}"))?
                );
            } else {
                println!(
                    "created {} for {} ({})",
                    created.name, created.scheduled_for, created.path
                );
                println!("  id: {}", created.id);
            }
        }
        MealCmd::SetStatus {
            target,
            status,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_mealplan_client(&u).await?;
            let mut m = resolve_meal_target(&client, &target).await?;
            m.status = status;
            let updated = client
                .update(m)
                .await
                .map_err(|e| eyre::eyre!("update: {e:?}"))?;
            println!("{}  [{}]  {}", updated.name, updated.status, updated.path);
        }
        MealCmd::Cook {
            target,
            no_deduct,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_mealplan_client(&u).await?;
            let m = resolve_meal_target(&client, &target).await?;
            // Auto-deduction logic lives server-side under
            // `can_cook` / `cook` — we pass an empty list and
            // let the server fill in from the recipes. The
            // `--no-deduct` flag is reserved for the future
            // ate-out-leftovers path; today both routes pass
            // the same empty list.
            let _ = no_deduct;
            let deductions = Vec::new();
            let cooked = client
                .cook(m.id.to_string(), deductions)
                .await
                .map_err(|e| eyre::eyre!("cook: {e:?}"))?;
            println!("cooked {}  ({})", cooked.name, cooked.path);
        }
        MealCmd::Skip {
            target,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_mealplan_client(&u).await?;
            let m = resolve_meal_target(&client, &target).await?;
            let skipped = client
                .skip(m.id.to_string())
                .await
                .map_err(|e| eyre::eyre!("skip: {e:?}"))?;
            println!("skipped {}  ({})", skipped.name, skipped.path);
        }
        MealCmd::Rename {
            target,
            new_path,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_mealplan_client(&u).await?;
            let m = resolve_meal_target(&client, &target).await?;
            let renamed = client
                .rename(m.id.to_string(), new_path)
                .await
                .map_err(|e| eyre::eyre!("rename: {e:?}"))?;
            println!("renamed → {}", renamed.path);
        }
        MealCmd::Delete {
            target,
            yes,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_mealplan_client(&u).await?;
            let m = resolve_meal_target(&client, &target).await?;
            if !yes && !confirm(&format!("delete `{}` ({})?", m.name, m.path))? {
                println!("aborted");
                return Ok(());
            }
            client
                .delete(m.id.to_string())
                .await
                .map_err(|e| eyre::eyre!("delete: {e:?}"))?;
            println!("deleted {}", m.path);
        }
    }
    Ok(())
}

// ── Pantry (pantry::Store) ───────────────────────────────────────────

async fn connect_pantry_client(url: &str) -> eyre::Result<pantry::PantryServiceClient> {
    Box::pin(vox::connect(url).establish())
        .await
        .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))
}

async fn resolve_pantry_target(
    client: &pantry::PantryServiceClient,
    target: &str,
) -> eyre::Result<pantry::PantryItem> {
    if uuid::Uuid::parse_str(target).is_ok() {
        return client
            .get(target.to_owned())
            .await
            .map_err(|e| eyre::eyre!("get: {e:?}"));
    }
    let rows = client
        .list()
        .await
        .map_err(|e| eyre::eyre!("list: {e:?}"))?;
    rows.into_iter()
        .find(|p| p.path == target || p.name == target)
        .ok_or_else(|| eyre::eyre!("not found: {target}"))
}

async fn run_pantry(cmd: PantryCmd) -> eyre::Result<()> {
    match cmd {
        PantryCmd::List {
            low_stock,
            expired,
            expiring_in,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_pantry_client(&u).await?;
            let today = chrono::Local::now().date_naive();
            let rows: Vec<_> = client
                .list()
                .await
                .map_err(|e| eyre::eyre!("list: {e:?}"))?
                .into_iter()
                .filter(|p| !low_stock || p.qty.is_some_and(|q| q < 1.0))
                .filter(|p| {
                    !expired
                        || p.stock_entries
                            .iter()
                            .any(|e| e.best_before.is_some_and(|d| d < today))
                })
                .filter(|p| {
                    expiring_in.is_none_or(|n| {
                        let cutoff = today + chrono::Duration::days(n);
                        p.stock_entries
                            .iter()
                            .any(|e| e.best_before.is_some_and(|d| d <= cutoff && d >= today))
                    })
                })
                .collect();
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&rows).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            for p in &rows {
                let q = p
                    .qty
                    .map_or_else(|| "?".into(), |n| format!("{n} {}", p.unit));
                println!("{:<32}  {:<12}    {}", p.name, q, p.path);
            }
        }
        PantryCmd::Get {
            target,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_pantry_client(&u).await?;
            let p = resolve_pantry_target(&client, &target).await?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&p).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            println!("{}\n", p.name);
            println!("  id:       {}", p.id);
            println!("  path:     {}", p.path);
            println!("  status:   {}", p.status);
            if let Some(q) = p.qty {
                println!("  qty:      {q} {}", p.unit);
            }
            if !p.food_category.is_empty() {
                println!("  food:     {}", p.food_category);
            }
            if let Some(l) = p.location_id {
                println!("  location: {l}");
            }
        }
        PantryCmd::Create {
            name,
            qty,
            unit,
            location,
            food_category,
            tags,
            details,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_pantry_client(&u).await?;
            let location_id = match location {
                None => None,
                Some(loc) => {
                    let lc = connect_locations_client(&u).await?;
                    Some(resolve_location_target(&lc, &loc).await?.id)
                }
            };
            // PantryItem has many fields; use the
            // `PantryItemDraft::into_item` helper to construct
            // a fully-defaulted item from a minimal draft.
            let draft = pantry::PantryItemDraft {
                barcode: String::new(),
                name,
                brand: None,
                food_category: food_category.unwrap_or_default(),
                unit: unit.unwrap_or_default(),
                nutrition_per_unit: None,
                nutrition_unit: None,
                image_url: None,
            };
            let mut new_item = draft.into_item(location_id);
            new_item.qty = qty;
            if !tags.is_empty() {
                new_item.tags = pantry::model::StringList(tags);
            }
            new_item.details = resolve_body(details)?;
            let created = client
                .create(new_item)
                .await
                .map_err(|e| eyre::eyre!("create: {e:?}"))?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&created).map_err(|e| eyre::eyre!("json: {e}"))?
                );
            } else {
                println!("created {} ({})", created.name, created.path);
                println!("  id: {}", created.id);
            }
        }
        PantryCmd::Consume {
            target,
            amount,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_pantry_client(&u).await?;
            let p = resolve_pantry_target(&client, &target).await?;
            let updated = client
                .consume(p.id.to_string(), amount)
                .await
                .map_err(|e| eyre::eyre!("consume: {e:?}"))?;
            let q = updated.qty.map_or_else(|| "?".into(), |n| n.to_string());
            println!("{}  qty={q} {}", updated.name, updated.unit);
        }
        PantryCmd::Restock {
            target,
            amount,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_pantry_client(&u).await?;
            let p = resolve_pantry_target(&client, &target).await?;
            let updated = client
                .restock(p.id.to_string(), amount)
                .await
                .map_err(|e| eyre::eyre!("restock: {e:?}"))?;
            let q = updated.qty.map_or_else(|| "?".into(), |n| n.to_string());
            println!("{}  qty={q} {}", updated.name, updated.unit);
        }
        PantryCmd::Open {
            target,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_pantry_client(&u).await?;
            let p = resolve_pantry_target(&client, &target).await?;
            let updated = client
                .open(p.id.to_string())
                .await
                .map_err(|e| eyre::eyre!("open: {e:?}"))?;
            println!("opened {}", updated.name);
        }
        PantryCmd::FindByBarcode {
            barcode,
            resolve,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_pantry_client(&u).await?;
            if resolve {
                let r = client
                    .resolve_barcode(barcode)
                    .await
                    .map_err(|e| eyre::eyre!("resolve_barcode: {e:?}"))?;
                if json {
                    println!(
                        "{}",
                        serde_json::to_string_pretty(&r).map_err(|e| eyre::eyre!("json: {e}"))?
                    );
                } else {
                    println!("{r:#?}");
                }
            } else {
                let p = client
                    .find_by_barcode(barcode)
                    .await
                    .map_err(|e| eyre::eyre!("find_by_barcode: {e:?}"))?;
                if json {
                    println!(
                        "{}",
                        serde_json::to_string_pretty(&p).map_err(|e| eyre::eyre!("json: {e}"))?
                    );
                } else {
                    println!("{} ({})", p.name, p.path);
                }
            }
        }
        PantryCmd::Rename {
            target,
            new_path,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_pantry_client(&u).await?;
            let p = resolve_pantry_target(&client, &target).await?;
            let renamed = client
                .rename(p.id.to_string(), new_path)
                .await
                .map_err(|e| eyre::eyre!("rename: {e:?}"))?;
            println!("renamed → {}", renamed.path);
        }
        PantryCmd::Delete {
            target,
            yes,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_pantry_client(&u).await?;
            let p = resolve_pantry_target(&client, &target).await?;
            if !yes && !confirm(&format!("delete `{}` ({})?", p.name, p.path))? {
                println!("aborted");
                return Ok(());
            }
            client
                .delete(p.id.to_string())
                .await
                .map_err(|e| eyre::eyre!("delete: {e:?}"))?;
            println!("deleted {}", p.path);
        }
    }
    Ok(())
}

// ── FS-native Obsidian vault subcommands ─────────────────────────────

fn run_vault(cmd: VaultCmd) -> eyre::Result<()> {
    use vault_obsidian::Vault;
    match cmd {
        VaultCmd::Open { path } => {
            let t0 = std::time::Instant::now();
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            println!(
                "vault: {}\n  pages:       {}\n  bases:       {}\n  attachments: {}\n  loaded in:   {:?}",
                v.root.display(),
                v.pages.len(),
                v.bases.len(),
                v.attachments.len(),
                t0.elapsed(),
            );
        }
        VaultCmd::Pages {
            path,
            folder,
            tag,
            fm,
            json,
        } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let fm_pairs = parse_fm_pairs(&fm)?;
            for page in &v.pages {
                if let Some(f) = &folder {
                    if !page.folder.starts_with(f) {
                        continue;
                    }
                }
                if let Some(t) = &tag {
                    if !page_matches_tag(page, t) {
                        continue;
                    }
                }
                if !fm_pairs.iter().all(|(k, v)| page_matches_fm(page, k, v)) {
                    continue;
                }
                if json {
                    let obj = serde_json::json!({
                        "path": page.rel_path,
                        "basename": page.basename,
                        "folder": page.folder,
                        "frontmatter": page
                            .parsed
                            .frontmatter
                            .iter()
                            .map(|e| (e.key.clone(), e.value.clone()))
                            .collect::<serde_json::Map<_, _>>(),
                    });
                    println!("{obj}");
                } else {
                    println!("{}", page.rel_path);
                }
            }
        }
        VaultCmd::Tags { path } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let mut counts: HashMap<String, usize> = HashMap::new();
            for page in &v.pages {
                for t in collect_page_tags(page) {
                    *counts.entry(t).or_insert(0) += 1;
                }
            }
            let mut rows: Vec<_> = counts.into_iter().collect();
            rows.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(&b.0)));
            for (tag, n) in rows {
                println!("{n:>5}  #{tag}");
            }
        }
        VaultCmd::Bases { path } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            for base in &v.bases {
                match &base.parsed {
                    Ok(p) => {
                        let views: Vec<&str> = p.views.iter().map(|v| v.name.as_str()).collect();
                        println!("{}  [{}]", base.rel_path, views.join(", "));
                    }
                    Err(e) => println!("{}  (parse error: {e})", base.rel_path),
                }
            }
        }
        VaultCmd::Cat { path, rel_path } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let page = v
                .page(&rel_path)
                .ok_or_else(|| eyre::eyre!("page not found: {rel_path}"))?;
            print!("{}", page.raw);
        }
        VaultCmd::Grep { path, pattern } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let needle = pattern.to_lowercase();
            for page in &v.pages {
                for (i, line) in page.raw.lines().enumerate() {
                    if line.to_lowercase().contains(&needle) {
                        println!("{}:{}:{}", page.rel_path, i + 1, line);
                    }
                }
            }
        }
        VaultCmd::Backlinks { path, rel_path } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let idx = vault_obsidian::LinkIndex::build(&v);
            for p in idx.backlinks(&rel_path) {
                println!("{p}");
            }
        }
        VaultCmd::Links { path, rel_path } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let idx = vault_obsidian::LinkIndex::build(&v);
            for link in idx.outgoing(&rel_path) {
                match link.resolved {
                    Some(target) => println!("{}\t→ {target}", link.linkpath),
                    None => println!("{}\t(unresolved)", link.linkpath),
                }
            }
        }
        VaultCmd::Orphans { path } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let idx = vault_obsidian::LinkIndex::build(&v);
            for p in idx.orphans() {
                println!("{p}");
            }
        }
        VaultCmd::Deadends { path } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let idx = vault_obsidian::LinkIndex::build(&v);
            for p in idx.deadends() {
                println!("{p}");
            }
        }
        VaultCmd::Unresolved { path } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let idx = vault_obsidian::LinkIndex::build(&v);
            for u in idx.unresolved() {
                println!("{}\t{}", u.source, u.linkpath);
            }
        }
        VaultCmd::Outline { path, rel_path } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let page = v
                .page(&rel_path)
                .ok_or_else(|| eyre::eyre!("page not found: {rel_path}"))?;
            for h in vault_obsidian::outline(page).headings {
                let bar = "#".repeat(h.level as usize);
                println!("{:>5}  {bar} {}", h.line, h.text);
            }
        }
        VaultCmd::Properties { path } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            for k in vault_obsidian::list_property_keys(&v) {
                println!("{k}");
            }
        }
        VaultCmd::PropertyRead {
            path,
            rel_path,
            key,
        } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let page = v
                .page(&rel_path)
                .ok_or_else(|| eyre::eyre!("page not found: {rel_path}"))?;
            if let Some(v) = vault_obsidian::read_property(page, &key) {
                println!("{v}");
            }
        }
        VaultCmd::PropertySet {
            path,
            rel_path,
            key,
            value,
        } => {
            let mut v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let parsed: serde_json::Value = serde_json::from_str(&value)
                .unwrap_or_else(|_| serde_json::Value::String(value.clone()));
            let guard = vault_obsidian::SelfWriteGuard::new();
            vault_obsidian::set_property(&mut v, &rel_path, &key, parsed, &guard)
                .map_err(|e| eyre::eyre!("set: {e}"))?;
        }
        VaultCmd::PropertyRemove {
            path,
            rel_path,
            key,
        } => {
            let mut v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let guard = vault_obsidian::SelfWriteGuard::new();
            vault_obsidian::remove_property(&mut v, &rel_path, &key, &guard)
                .map_err(|e| eyre::eyre!("remove: {e}"))?;
        }
        VaultCmd::Aliases { path } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            for a in vault_obsidian::list_aliases(&v) {
                println!("{}\t{}", a.alias, a.page);
            }
        }
        VaultCmd::Tasks { path } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            for t in vault_obsidian::list_tasks(&v) {
                println!("{}:{}\t[{}] {}", t.page, t.line, t.marker, t.text);
            }
        }
        VaultCmd::Wordcount { path, page } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let wc = match page {
                Some(rel) => {
                    let p = v
                        .page(&rel)
                        .ok_or_else(|| eyre::eyre!("page not found: {rel}"))?;
                    vault_obsidian::page_wordcount(p)
                }
                None => vault_obsidian::vault_wordcount(&v),
            };
            println!(
                "pages: {}\nwords: {}\ncharacters: {}",
                wc.pages, wc.words, wc.characters
            );
        }
        VaultCmd::BaseQuery { path, base, view } => {
            let v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            if let Some(view_name) = view {
                let ev = vault_obsidian::query_view(&v, &base, &view_name)
                    .map_err(|e| eyre::eyre!("query: {e}"))?;
                print_executed_view(&view_name, &ev);
            } else {
                let results = vault_obsidian::query_all_views(&v, &base)
                    .map_err(|e| eyre::eyre!("query: {e}"))?;
                for (name, ev) in results {
                    print_executed_view(&name, &ev);
                }
            }
        }
        VaultCmd::Create {
            path,
            rel_path,
            body,
        } => {
            let mut v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let body = if let Some(b) = body {
                b
            } else {
                use std::io::Read;
                let mut buf = String::new();
                std::io::stdin().read_to_string(&mut buf)?;
                buf
            };
            let guard = vault_obsidian::SelfWriteGuard::new();
            vault_obsidian::create_page(&mut v, &rel_path, &[], &body, &guard)
                .map_err(|e| eyre::eyre!("create: {e}"))?;
        }
        VaultCmd::Append {
            path,
            rel_path,
            text,
            inline,
        } => {
            let mut v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let guard = vault_obsidian::SelfWriteGuard::new();
            vault_obsidian::append_to_page(&mut v, &rel_path, &text, inline, &guard)
                .map_err(|e| eyre::eyre!("append: {e}"))?;
        }
        VaultCmd::Prepend {
            path,
            rel_path,
            text,
            inline,
        } => {
            let mut v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let guard = vault_obsidian::SelfWriteGuard::new();
            vault_obsidian::prepend_to_page(&mut v, &rel_path, &text, inline, &guard)
                .map_err(|e| eyre::eyre!("prepend: {e}"))?;
        }
        VaultCmd::Delete { path, rel_path } => {
            let mut v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let guard = vault_obsidian::SelfWriteGuard::new();
            vault_obsidian::delete_page(&mut v, &rel_path, &guard)
                .map_err(|e| eyre::eyre!("delete: {e}"))?;
        }
        VaultCmd::Move { path, from, to } => {
            let mut v = Vault::open(&path).map_err(|e| eyre::eyre!("open: {e}"))?;
            let guard = vault_obsidian::SelfWriteGuard::new();
            vault_obsidian::move_page(&mut v, &from, &to, &guard)
                .map_err(|e| eyre::eyre!("move: {e}"))?;
        }
        VaultCmd::Sync { .. } | VaultCmd::Pull { .. } | VaultCmd::Push { .. } => {
            // Routed to `run_vault_sync` from the async
            // dispatch above. Should never hit this arm.
            unreachable!("sync ops routed through run_vault_sync");
        }
    }
    Ok(())
}

/// Async vault sync handler — talks to `/org/<slug>/vox` and
/// applies pull/push/sync ops via the architect-generated
/// `VaultSyncClient`. Logic lives in `vault_sync_client`; this
/// wrapper handles the I/O + the CLI's flag plumbing.
async fn run_vault_sync(cmd: VaultCmd) -> eyre::Result<()> {
    use vault_proto::{IfMatch, VaultSyncClient};
    use vault_sync_client::{LocalEntry, Side, SyncOp, SyncSummary, index_local, plan_sync};

    enum Mode {
        Sync,
        Pull,
        Push,
    }

    let (mode, local, server, org_slug, vault_id, dry_run) = match cmd {
        VaultCmd::Sync {
            local,
            server,
            org,
            vault_id,
            dry_run,
        } => (Mode::Sync, local, server, org, vault_id, dry_run),
        VaultCmd::Pull {
            local,
            server,
            org,
            vault_id,
            dry_run,
        } => (Mode::Pull, local, server, org, vault_id, dry_run),
        VaultCmd::Push {
            local,
            server,
            org,
            vault_id,
            dry_run,
        } => (Mode::Push, local, server, org, vault_id, dry_run),
        _ => unreachable!("only sync ops reach this handler"),
    };

    // Resolve the org slug (active session if not overridden).
    let org_slug = match org_slug {
        Some(s) => s,
        None => session_store::load()?
            .map(|s| s.active)
            .ok_or_else(|| eyre::eyre!("no active org — pass --org or sign in first"))?,
    };

    // Resolve local vault root (org's `vault/` dir if not overridden).
    let local_root = if let Some(p) = local {
        p
    } else {
        let root = org_proto::DataRoot::from_env().map_err(|e| eyre::eyre!("data root: {e}"))?;
        root.org(&org_slug).vault_dir()
    };

    // Resolve the per-org vox URL.
    let base = server.unwrap_or_else(|| "ws://127.0.0.1:18080".to_owned());
    let url = if base.ends_with("/vox") {
        base
    } else {
        let stripped = base.trim_end_matches('/');
        format!("{stripped}/org/{org_slug}/vox")
    };

    println!("Local:  {}", local_root.display());
    println!("Server: {url}");
    println!("Vault:  {vault_id}\n");

    let client: VaultSyncClient = Box::pin(vox::connect(&url).establish())
        .await
        .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))?;

    // Index local + fetch remote manifest in parallel-ish (the
    // local walk is sync, but cheap; do it before the network
    // round-trip).
    let local_entries: Vec<LocalEntry> =
        index_local(&local_root).map_err(|e| eyre::eyre!("index local: {e}"))?;
    let remote_manifest = client
        .manifest(vault_id.clone())
        .await
        .map_err(|e| eyre::eyre!("fetch manifest: {e:?}"))?;

    println!(
        "indexed: local={} remote={}",
        local_entries.len(),
        remote_manifest.files.len()
    );

    // Plan, then filter by mode.
    let plan = plan_sync(&local_entries, &remote_manifest);
    let plan: Vec<SyncOp> = plan
        .into_iter()
        .filter(|op| {
            matches!(
                (op, &mode),
                (SyncOp::InSync { .. }, _)
                    | (SyncOp::Pull { .. }, Mode::Sync | Mode::Pull)
                    | (SyncOp::Push { .. }, Mode::Sync | Mode::Push)
                    | (SyncOp::Conflict { .. }, Mode::Sync)
            )
        })
        .collect();

    let mut summary = SyncSummary::default();
    for op in &plan {
        summary.record(op);
    }

    println!(
        "plan: {} push · {} pull · {} in-sync · {} conflicts (local/remote: {}/{})\n",
        summary.pushed,
        summary.pulled,
        summary.in_sync,
        summary.conflicts_local_won + summary.conflicts_remote_won,
        summary.conflicts_local_won,
        summary.conflicts_remote_won,
    );

    if dry_run {
        for op in &plan {
            describe_op(op);
        }
        return Ok(());
    }

    // Apply.
    for op in &plan {
        match op {
            SyncOp::InSync { .. } => {}
            SyncOp::Pull { path, .. } => {
                let bytes = client
                    .get_file(vault_id.clone(), path.clone())
                    .await
                    .map_err(|e| eyre::eyre!("get_file {path}: {e:?}"))?;
                write_local(&local_root, path, &bytes.0)?;
                println!("PULL  {path}");
            }
            SyncOp::Push { path, .. } => {
                let abs = local_root.join(path);
                let bytes =
                    std::fs::read(&abs).map_err(|e| eyre::eyre!("read {}: {e}", abs.display()))?;
                client
                    .put_file(vault_id.clone(), path.clone(), bytes, IfMatch::CreateOnly)
                    .await
                    .map_err(|e| eyre::eyre!("put_file {path}: {e:?}"))?;
                println!("PUSH  {path}");
            }
            SyncOp::Conflict {
                path,
                remote_sha,
                winning_side,
                ..
            } => match winning_side {
                Side::Local => {
                    let abs = local_root.join(path);
                    let bytes = std::fs::read(&abs)
                        .map_err(|e| eyre::eyre!("read {}: {e}", abs.display()))?;
                    client
                        .put_file(
                            vault_id.clone(),
                            path.clone(),
                            bytes,
                            IfMatch::Sha(remote_sha.clone()),
                        )
                        .await
                        .map_err(|e| eyre::eyre!("put_file (conflict) {path}: {e:?}"))?;
                    println!("PUSH! {path}  (conflict: local won)");
                }
                Side::Remote => {
                    let bytes = client
                        .get_file(vault_id.clone(), path.clone())
                        .await
                        .map_err(|e| eyre::eyre!("get_file (conflict) {path}: {e:?}"))?;
                    write_local(&local_root, path, &bytes.0)?;
                    println!("PULL! {path}  (conflict: remote won)");
                }
            },
        }
    }

    println!(
        "\ndone: {} pushed · {} pulled · {} in-sync",
        summary.pushed + summary.conflicts_local_won,
        summary.pulled + summary.conflicts_remote_won,
        summary.in_sync,
    );
    Ok(())
}

fn describe_op(op: &vault_sync_client::SyncOp) {
    use vault_sync_client::{Side, SyncOp};
    match op {
        SyncOp::InSync { path } => println!("OK    {path}"),
        SyncOp::Pull { path, .. } => println!("PULL  {path}"),
        SyncOp::Push { path, .. } => println!("PUSH  {path}"),
        SyncOp::Conflict {
            path, winning_side, ..
        } => {
            let side = match winning_side {
                Side::Local => "local",
                Side::Remote => "remote",
            };
            println!("CONF  {path}  (winner: {side})");
        }
    }
}

fn write_local(local_root: &std::path::Path, path: &str, bytes: &[u8]) -> eyre::Result<()> {
    if path.split(['/', '\\']).any(|seg| seg == "..") {
        return Err(eyre::eyre!("refused path with `..`: {path}"));
    }
    let abs = local_root.join(path);
    if let Some(parent) = abs.parent() {
        std::fs::create_dir_all(parent)
            .map_err(|e| eyre::eyre!("mkdir {}: {e}", parent.display()))?;
    }
    std::fs::write(&abs, bytes).map_err(|e| eyre::eyre!("write {}: {e}", abs.display()))?;
    Ok(())
}

fn print_executed_view(name: &str, ev: &vault_live::bases::ExecutedView) {
    let total: usize = ev.groups.iter().map(|(_, r)| r.len()).sum();
    println!("## {name}  ({total} rows)");
    for (bucket, rows) in &ev.groups {
        if !bucket.is_empty() {
            println!("  [{bucket}]");
        }
        for row in rows {
            println!("    {}", row.basename);
        }
    }
}

fn parse_fm_pairs(raw: &[String]) -> eyre::Result<Vec<(String, serde_json::Value)>> {
    raw.iter()
        .map(|s| {
            let (k, v) = s
                .split_once('=')
                .ok_or_else(|| eyre::eyre!("--fm expects KEY=VALUE, got `{s}`"))?;
            let parsed: serde_json::Value = serde_json::from_str(v)
                .unwrap_or_else(|_| serde_json::Value::String(v.to_string()));
            Ok((k.to_string(), parsed))
        })
        .collect()
}

fn page_matches_fm(page: &vault_obsidian::VaultPage, key: &str, value: &serde_json::Value) -> bool {
    page.parsed
        .frontmatter
        .iter()
        .any(|e| e.key == key && &e.value == value)
}

fn page_matches_tag(page: &vault_obsidian::VaultPage, tag: &str) -> bool {
    // Match Obsidian: a query for `#parent` also includes any
    // `#parent/child` nested tags.
    let prefix = format!("{tag}/");
    collect_page_tags(page)
        .into_iter()
        .any(|t| t == tag || t.starts_with(&prefix))
}

fn collect_page_tags(page: &vault_obsidian::VaultPage) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    for e in &page.parsed.frontmatter {
        if e.key == "tags" || e.key == "tag" {
            match &e.value {
                serde_json::Value::String(s) => {
                    for t in s.split([',', ' ']) {
                        let t = t.trim().trim_start_matches('#');
                        if !t.is_empty() {
                            out.push(t.to_string());
                        }
                    }
                }
                serde_json::Value::Array(arr) => {
                    for v in arr {
                        if let Some(s) = v.as_str() {
                            out.push(s.trim_start_matches('#').to_string());
                        }
                    }
                }
                _ => {}
            }
        }
    }
    for b in &page.parsed.blocks {
        for r in &b.refs {
            if let vault_live::refs::Ref::Tag(t) = r {
                out.push(t.path.join("/"));
            }
        }
    }
    out.sort();
    out.dedup();
    out
}
