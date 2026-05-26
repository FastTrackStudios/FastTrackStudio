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
    /// Linear-style issue surface over TaskInfo's
    /// `WorkflowAttrs` (workspace / cycle / project / estimate /
    /// assignees / blockers). The data still lives in TaskInfo —
    /// `task issue *` is just the workflow-aware view of it.
    /// `task work *` is an alias for ergonomic typing.
    #[command(subcommand, alias = "work")]
    Issue(IssueCmd),
    /// One-shot integration setup — connect a forge repo to a
    /// workspace: generate the webhook secret, register the
    /// webhook on the forge, record the repo binding.
    #[command(subcommand)]
    Setup(SetupCmd),
    /// The agent dev loop — git operations wrapped around the
    /// issue lifecycle. `start` branches + claims, `commit`
    /// stamps attribution trailers, `push` opens a linked PR,
    /// `finish` merges + closes. Infers the forge repo from the
    /// git remote, so it works on third-party repos too.
    #[command(subcommand)]
    Code(CodeCmd),
    /// Org-scoped labels — colored tags for triage + filtering.
    /// Persisted per-org as `labels.json`.
    #[command(subcommand)]
    Label(LabelCmd),
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
    /// Body metrics — weight / body-fat / measurements log.
    #[command(subcommand)]
    Body(BodyCmd),
    /// Exercise library — movement definitions referenced
    /// by routines + sessions.
    #[command(subcommand)]
    Exercise(ExerciseCmd),
    /// Workout routines + sessions.
    #[command(subcommand)]
    Workout(WorkoutCmd),
    /// Food intake log — daily calorie + macro tracking.
    #[command(subcommand)]
    Intake(IntakeCmd),
}

#[derive(Subcommand)]
enum BodyCmd {
    List {
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
        /// e.g. `weight`, `body_fat`, `waist`. Free-form;
        /// canonical set in `body::MetricKind`.
        #[arg(long)]
        kind: Option<String>,
        /// Default unit (`kg`, `%`, `cm`).
        #[arg(long)]
        unit: Option<String>,
        #[arg(long)]
        goal: Option<f64>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Append a measurement to a metric's time series.
    Log {
        target: String,
        value: f64,
        /// Date (`YYYY-MM-DD`). Defaults to today.
        #[arg(long)]
        date: Option<String>,
        #[arg(long)]
        unit: Option<String>,
        #[arg(long)]
        note: Option<String>,
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
enum ExerciseCmd {
    List {
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
        kind: Option<String>,
        #[arg(long)]
        muscle: Option<String>,
        #[arg(long, value_delimiter = ',')]
        tags: Vec<String>,
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
enum WorkoutCmd {
    /// Routines (the program — push/pull/legs, etc).
    #[command(subcommand)]
    Routine(WorkoutRoutineCmd),
    /// Sessions (one workout instance).
    #[command(subcommand)]
    Session(WorkoutSessionCmd),
}

#[derive(Subcommand)]
enum WorkoutRoutineCmd {
    List {
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
enum WorkoutSessionCmd {
    List {
        #[arg(long)]
        date: Option<String>,
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
    /// Start a fresh session from a routine + day.
    StartFromRoutine {
        routine: String,
        day: String,
        #[arg(long)]
        date: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Log a working set against a session.
    LogSet {
        session: String,
        exercise: String,
        reps: u32,
        weight: f64,
        #[arg(long)]
        rpe: Option<f64>,
        #[arg(long)]
        note: Option<String>,
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
enum IntakeCmd {
    List {
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
    /// Get the intake log for `YYYY-MM-DD`. Creates empty
    /// if missing.
    ForDay {
        date: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    LogRecipe {
        date: String,
        recipe: String,
        servings: f64,
        #[arg(long, default_value = "snack")]
        slot: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    LogPantry {
        date: String,
        item: String,
        qty: f64,
        #[arg(long, default_value = "snack")]
        slot: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    LogFreeform {
        date: String,
        name: String,
        #[arg(long)]
        kcal: Option<f64>,
        #[arg(long, default_value = "snack")]
        slot: String,
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
    /// Set the project's target completion date (YYYY-MM-DD, or
    /// `none`/`clear` to unset). The Linear-style roadmap field.
    SetTarget {
        target: String,
        date: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Recompute + show the project's progress from its tasks
    /// (done / total of tasks whose `projectId` is this project).
    /// Writes the rolled-up `progress_percent` back.
    Progress {
        target: String,
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

/// Linear-style issue surface over TaskInfo's WorkflowAttrs.
///
/// **Why this exists alongside `task task *`.** TaskInfo is the
/// canonical unit of work in Task — the same row underpins both
/// `task task *` (the TaskNotes-shape personal-task surface)
/// and `task issue *` (the Linear-shape work-tracking surface).
/// `issue` verbs operate through `WorkflowAttrs`: filter / show
/// / patch the workspace + cycle + project + estimate +
/// assignees + blockers triplet.
#[derive(Subcommand)]
enum IssueCmd {
    /// List tasks filtered by their workflow attributes.
    List {
        /// Filter by `workflow.cycle = <uuid>`.
        #[arg(long)]
        cycle: Option<uuid::Uuid>,
        /// Filter by `project_id = <uuid>`.
        #[arg(long)]
        project: Option<uuid::Uuid>,
        /// Filter by an `AgentRef` in `workflow.assignees`.
        /// Accepts `agent:name`, `agent:name@version`,
        /// `human:user_id`, or a bare name (defaults to `agent:`).
        #[arg(long)]
        assignee: Option<String>,
        /// Filter by `TaskInfo::status` (e.g. `open`, `in-progress`).
        #[arg(long)]
        status: Option<String>,
        /// Only show tasks with `workflow: Some(_)` set. Useful
        /// while migrating — keeps unmigrated personal tasks
        /// out of the issue view.
        #[arg(long)]
        has_workflow: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Emit JSON instead of the tabular default.
        #[arg(long)]
        json: bool,
    },

    /// Show a single issue. Accepts a full UUID or the first
    /// 8+ chars of one (`resolve_issue_id` does the prefix match).
    Show {
        id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },

    /// Patch the issue's `WorkflowAttrs` in place. Repeatable
    /// `--add-assignee` / `--add-blocker` for set operations.
    /// Pass `--clear` to drop the workflow block entirely (the
    /// task becomes a plain TaskNotes-shape task again).
    SetWorkflow {
        id: String,
        /// UUID, or `"none"` / `""` to clear.
        #[arg(long)]
        cycle: Option<String>,
        /// UUID, or `"none"` / `""` to clear.
        #[arg(long)]
        project: Option<String>,
        /// `xs`, `s`, `m`, `l`, `xl`, or a plain integer for
        /// `Estimate::Points`.
        #[arg(long)]
        estimate: Option<String>,
        #[arg(long = "add-assignee", value_name = "AGENT")]
        add_assignee: Vec<String>,
        #[arg(long = "remove-assignee", value_name = "AGENT")]
        remove_assignee: Vec<String>,
        #[arg(long = "add-blocker", value_name = "TASK_ID")]
        add_blocker: Vec<uuid::Uuid>,
        #[arg(long = "remove-blocker", value_name = "TASK_ID")]
        remove_blocker: Vec<uuid::Uuid>,
        /// Drop the workflow block entirely.
        #[arg(long)]
        clear: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },

    /// Atomically claim an issue for an agent — the core of the
    /// parallel-agent workflow. Fails if another agent already
    /// holds it (read → check-empty → write → re-read verify),
    /// so two agents racing for the same subtask can't both win.
    /// Pass `--force` to steal a claim.
    Claim {
        id: String,
        /// `name[@version]` — version omitted means "any version".
        #[arg(long = "as-agent")]
        as_agent: String,
        /// Steal the claim even if someone else holds it.
        #[arg(long)]
        force: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },

    /// Triage an issue (PRD) into agent-sized subtasks — the
    /// "it's time to start this" step. Creates one subtask per
    /// title under the parent, flips the parent to in-progress,
    /// and prints the board. Titles come from repeated
    /// `--subtask` flags and/or `--from` (one per line, `-` for
    /// stdin). After this, parallel agents `claim` + `code start`.
    Triage {
        /// Parent issue id (UUID or 8-char prefix).
        id: String,
        /// A subtask title. Repeatable.
        #[arg(long = "subtask", value_name = "TITLE")]
        subtasks: Vec<String>,
        /// Read additional subtask titles, one per line, from a
        /// file or `-` for stdin.
        #[arg(long)]
        from: Option<String>,
        /// Status to set on the parent after triage. Default
        /// `in-progress`.
        #[arg(long, default_value = "in-progress")]
        parent_status: String,
        /// Priority applied to every created subtask.
        #[arg(long, default_value = "normal")]
        priority: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },

    /// List the subtasks of a parent task with their claim +
    /// status, so you can see who's working what at a glance.
    Subtasks {
        /// Parent task id (UUID or 8-char prefix).
        id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },

    /// List the current assignees on an issue.
    Assignees {
        id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },

    /// Create a new issue. Workflow attrs can be set inline.
    /// Equivalent to `task task create` + `task issue
    /// set-workflow` in one call.
    Create {
        /// Title (positional). Body can be passed via --body.
        title: String,
        /// Vault-relative path; defaults to `Task/<slug>.md`.
        #[arg(long)]
        path: Option<String>,
        /// Initial status. Default `open`.
        #[arg(long)]
        status: Option<String>,
        /// Initial priority. Default `normal`.
        #[arg(long)]
        priority: Option<String>,
        /// Cycle UUID. Sets `workflow.cycle`.
        #[arg(long)]
        cycle: Option<uuid::Uuid>,
        /// Project UUID. Sets `project_id`.
        #[arg(long)]
        project: Option<uuid::Uuid>,
        /// Parent task UUID — makes this a subtask. Sets
        /// `workflow.parent`.
        #[arg(long)]
        parent: Option<uuid::Uuid>,
        /// Estimate (`xs` / `s` / `m` / `l` / `xl` / integer).
        #[arg(long)]
        estimate: Option<String>,
        /// Repeatable assignee. `agent:name[@ver]` or
        /// `human:user_id`. Bare names default to agent.
        #[arg(long = "assignee", value_name = "AGENT")]
        assignees: Vec<String>,
        /// Repeatable blocker — `task issue ready` won't
        /// surface this issue until each blocker closes.
        #[arg(long = "blocker", value_name = "TASK_ID")]
        blockers: Vec<uuid::Uuid>,
        /// Repeatable tag.
        #[arg(long = "tag", value_name = "TAG")]
        tags: Vec<String>,
        /// Body (markdown). Pass `-` for stdin, or a file path.
        #[arg(long)]
        body: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },

    /// Show issues ready to work — open, not done, with no
    /// unresolved blockers. The beads-equivalent of `bd ready`.
    Ready {
        #[arg(long)]
        cycle: Option<uuid::Uuid>,
        #[arg(long)]
        project: Option<uuid::Uuid>,
        /// Show only issues claimable by this agent (no
        /// assignee yet, OR this agent is already listed).
        #[arg(long)]
        as_agent: Option<String>,
        /// Max rows to show.
        #[arg(long, default_value = "20")]
        limit: usize,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },

    /// Claim an issue and flip its status to `in-progress`.
    /// The combined `bd update --claim` equivalent.
    Start {
        id: String,
        /// Agent to claim as — `name[@version]`. If omitted,
        /// only the status is changed (existing assignees
        /// are preserved).
        #[arg(long = "as-agent")]
        as_agent: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },

    /// Close an issue — flips status to `done` and stamps
    /// `completedDate`. Pass `--undo` to reopen.
    Close {
        id: String,
        #[arg(long)]
        undo: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },

    /// Migrate beads issues into Task. Reads a `bd list --json`
    /// export and creates a TaskInfo per issue (status + priority
    /// mapped, tagged `from-beads`). The "replace beads" step.
    ImportBeads {
        /// Source: `bd` (shell `bd list --json`), a file path, or
        /// `-` for stdin. Default `bd`.
        #[arg(long, default_value = "bd")]
        from: String,
        /// Parse + report what would be created without writing.
        #[arg(long)]
        dry_run: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },

    /// Project-level overview — counts grouped by status,
    /// priority, workspace, and assignee. Beads-equivalent of
    /// `bd stats`.
    Stats {
        /// Restrict to one project.
        #[arg(long)]
        project: Option<uuid::Uuid>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },

    /// Record an existing forge issue as the upstream of a
    /// local TaskInfo. Doesn't create either — just adds the
    /// IssueLink to the per-org link store. Use this when an
    /// issue already exists on both sides.
    LinkForge {
        /// Local TaskInfo id (UUID or prefix).
        id: String,
        /// `owner/repo` on the forge.
        repo: String,
        /// Forge-assigned issue number (or PR number with --kind pull).
        number: u64,
        /// Forge host base URL (e.g. `https://git.starcommand.live`).
        /// Defaults to the value in `TASK_FORGEJO_BASE_URL`.
        #[arg(long)]
        base_url: Option<String>,
        /// `issue` or `pull`. Default `issue`.
        #[arg(long, default_value = "issue")]
        kind: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },

    /// Push a local TaskInfo upstream — creates a Forgejo issue
    /// from `title` + `details`, records the link, exits. If the
    /// task already has a link to this repo, prints the existing
    /// link and exits without re-creating.
    Push {
        id: String,
        /// `owner/repo` on the forge.
        #[arg(long)]
        repo: String,
        /// Target GitHub instead of Forgejo. Uses TASK_GITHUB_TOKEN.
        #[arg(long)]
        github: bool,
        /// Forgejo host base URL. Falls back to `TASK_FORGEJO_BASE_URL`.
        /// Ignored when --github is set.
        #[arg(long)]
        base_url: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },

    /// On-demand bidirectional reconcile — no webhook needed.
    /// For every locally-linked issue in the repo, fetch its
    /// current forge state and apply it (forge wins for
    /// open/closed); then pull any new forge issues we don't
    /// track yet. Run manually or on a cron for third-party
    /// repos where you can't install a webhook.
    Sync {
        /// `owner/repo` on the forge.
        #[arg(long)]
        repo: String,
        /// Sync against GitHub instead of Forgejo.
        #[arg(long)]
        github: bool,
        /// Forgejo host base URL. Falls back to `TASK_FORGEJO_BASE_URL`.
        #[arg(long)]
        base_url: Option<String>,
        /// Optional project UUID to stamp on newly-pulled tasks.
        #[arg(long)]
        project: Option<uuid::Uuid>,
        /// Don't create local tasks for forge issues we don't
        /// track — only reconcile state of already-linked ones.
        #[arg(long)]
        no_pull: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },

    /// Sync every linked repo in the org in one pass — one
    /// cron line keeps all your tracked repos fresh without
    /// webhooks.
    SyncAll {
        /// Optional project UUID to stamp on newly-pulled tasks.
        #[arg(long)]
        project: Option<uuid::Uuid>,
        /// Only reconcile existing links; don't pull new issues.
        #[arg(long)]
        no_pull: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },

    /// List open pull requests on a repo.
    PrList {
        #[arg(long)]
        repo: String,
        #[arg(long)]
        github: bool,
        #[arg(long)]
        base_url: Option<String>,
        #[arg(long)]
        json: bool,
    },

    /// Open a pull request.
    PrCreate {
        #[arg(long)]
        repo: String,
        #[arg(long)]
        github: bool,
        #[arg(long)]
        base_url: Option<String>,
        #[arg(long)]
        title: String,
        /// Source branch.
        #[arg(long)]
        head: String,
        /// Target branch. Default `main`.
        #[arg(long, default_value = "main")]
        base: String,
        #[arg(long)]
        body: Option<String>,
        #[arg(long)]
        draft: bool,
        /// Forge issue number this PR closes. Injects
        /// `Closes #N` into the body so the forge auto-closes
        /// the issue when the PR merges.
        #[arg(long)]
        closes: Option<u64>,
        /// Local task whose linked forge issue this PR closes.
        /// Resolves the issue number from the link store and
        /// injects `Closes #N` — and records a PR link on the
        /// task so `pr-merge`/sync can finish the loop.
        #[arg(long)]
        close_task: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },

    /// Merge a pull request by number. With `--close-task`,
    /// closes the linked task afterward (which propagates the
    /// close back to its own forge issue) — the `task code
    /// merge` chain: merge PR → close task → done everywhere.
    PrMerge {
        #[arg(long)]
        repo: String,
        #[arg(long)]
        github: bool,
        #[arg(long)]
        base_url: Option<String>,
        number: u64,
        /// `merge` (default), `squash`, or `rebase`.
        #[arg(long, default_value = "merge")]
        method: String,
        /// After merging, close this task (UUID or prefix). Its
        /// own linked forge issue gets closed too via the normal
        /// close-propagation path.
        #[arg(long)]
        close_task: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },

    /// Fetch all issues from a Forgejo repo and create local
    /// TaskInfos for ones we don't already have linked. Existing
    /// linked issues are left alone (use `sync` to update).
    Pull {
        /// `owner/repo` on the forge.
        #[arg(long)]
        repo: String,
        /// Pull from GitHub instead of Forgejo. Uses TASK_GITHUB_TOKEN.
        #[arg(long)]
        github: bool,
        /// Forgejo host base URL. Falls back to `TASK_FORGEJO_BASE_URL`.
        /// Ignored when --github is set.
        #[arg(long)]
        base_url: Option<String>,
        /// Optional project UUID to stamp on pulled-in tasks.
        #[arg(long)]
        project: Option<uuid::Uuid>,
        /// Filter by issue state: `open` (default), `closed`, or `all`.
        #[arg(long, default_value = "open")]
        state: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
}

/// `task code *` — the agent dev loop over git + issues.
///
/// Branch convention: `task/<short-id>-<slug>`. The short id is
/// the first 8 chars of the task UUID; `commit`/`push`/`status`/
/// `finish` parse it back out of the current branch name, so the
/// branch is the only state these verbs need.
#[derive(Subcommand)]
enum CodeCmd {
    /// Claim a task, flip it to in-progress, and create a work
    /// branch off the current HEAD.
    Start {
        /// Task id (UUID or 8-char prefix).
        id: String,
        /// Claim as this agent (`name[@version]`).
        #[arg(long = "as-agent")]
        as_agent: Option<String>,
        /// Branch prefix. Default `task`.
        #[arg(long, default_value = "task")]
        prefix: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// `git commit` with attribution trailers (Task-Id,
    /// Task-Agent, Co-Authored-By) derived from the current
    /// branch's task.
    Commit {
        #[arg(short = 'm', long)]
        message: String,
        /// Attribute to this agent (`name[@version]`).
        #[arg(long = "as-agent")]
        as_agent: Option<String>,
        /// Stage everything first (`git add -A`).
        #[arg(long)]
        all: bool,
    },
    /// Push the current branch and open a linked PR that closes
    /// the branch's task's forge issue on merge.
    Push {
        /// Target GitHub instead of Forgejo.
        #[arg(long)]
        github: bool,
        /// Forgejo base URL (falls back to `TASK_FORGEJO_BASE_URL`).
        #[arg(long)]
        base_url: Option<String>,
        /// PR target branch. Default `main`.
        #[arg(long, default_value = "main")]
        base: String,
        /// Open as a draft.
        #[arg(long)]
        draft: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Show the current branch's task + its linked issue/PR.
    Status {
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Park the current branch's task — record a "where I left
    /// off" handoff, release the claim so another agent can pick
    /// it up. The branch + commits stay; resume picks up there.
    Park {
        /// Summary of where things stand (markdown).
        summary: String,
        /// Why parking: blocked / needs-input / context-limit /
        /// out-of-scope / end-of-chunk. Free-form.
        #[arg(long, default_value = "end-of-chunk")]
        reason: String,
        /// Open questions for the next agent (markdown bullets).
        #[arg(long)]
        open: Option<String>,
        /// Attribute the handoff to this agent.
        #[arg(long = "as-agent")]
        as_agent: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Resume a parked task: atomically claim it, print the
    /// handoff context (summary + open questions + recent
    /// commits), and switch to its branch.
    Resume {
        /// Task id (UUID or 8-char prefix). Omit to resume the
        /// current branch's task.
        id: Option<String>,
        /// Claim as this agent.
        #[arg(long = "as-agent")]
        as_agent: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// List parked tasks (open handoffs) available to pick up —
    /// the cross-agent work queue.
    Inbox {
        /// Only show handoffs addressed to (or open to) this agent.
        #[arg(long = "as-agent")]
        as_agent: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
}

/// `task label *` — org-scoped colored tags.
#[derive(Subcommand)]
enum LabelCmd {
    /// Create a label (idempotent on name within the org).
    Create {
        name: String,
        /// 6-char hex color without `#` (e.g. `d73a4a`).
        #[arg(long)]
        color: Option<String>,
        /// Optional group (e.g. `priority`, `area`).
        #[arg(long)]
        group: Option<String>,
        #[arg(long)]
        description: Option<String>,
        #[arg(long)]
        org: Option<String>,
    },
    /// List all labels in the org.
    List {
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Remove a label by name.
    Rm {
        name: String,
        #[arg(long)]
        org: Option<String>,
    },
}

/// `task setup *` — guided integration setup.
#[derive(Subcommand)]
enum SetupCmd {
    /// Connect a forge repo to this org: ensure a webhook
    /// secret exists, register the webhook on the forge, and
    /// record the repo binding. Idempotent — re-running updates
    /// the existing webhook rather than duplicating it.
    Forge {
        /// `owner/repo` on the forge.
        #[arg(long)]
        repo: String,
        /// Target GitHub instead of Forgejo.
        #[arg(long)]
        github: bool,
        /// Forgejo host base URL. Falls back to `TASK_FORGEJO_BASE_URL`.
        #[arg(long)]
        base_url: Option<String>,
        /// Public URL the forge should POST events to. Should end
        /// in `/org/<slug>/webhooks/forge`. If omitted, derived
        /// from `--public-base` + the active org slug.
        #[arg(long)]
        webhook_url: Option<String>,
        /// Public base of the task-server (e.g.
        /// `https://tasks.example.com`). Used to build
        /// `<base>/org/<slug>/webhooks/forge` when `--webhook-url`
        /// isn't given.
        #[arg(long)]
        public_base: Option<String>,
        /// Optional project UUID to associate this repo with.
        #[arg(long)]
        project: Option<uuid::Uuid>,
        #[arg(long)]
        org: Option<String>,
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
    /// Build a **token-budgeted context subgraph** for an
    /// LLM prompt. Returns relevance-ranked page summaries +
    /// outbound connections as markdown — paste the output
    /// straight into a system / user message. The agent
    /// gets a structural view of the wiki instead of having
    /// to grep raw files.
    ///
    /// Inspired by graphify's "query the graph instead of
    /// grepping" pitch, but for the entire wiki (concepts +
    /// entities + sources, not just code).
    Context {
        /// Free-text query. Empty = top-centrality view.
        query: String,
        #[arg(short, long, default_value = "examples/vault")]
        vault: std::path::PathBuf,
        /// `concept` / `entity` / `source` filter on
        /// `type:` frontmatter.
        #[arg(long, default_value = "")]
        node_type: String,
        /// Soft token cap. `chars/4` heuristic. Default
        /// 8000 (~32k chars, comfortably fits in any
        /// modern context window).
        #[arg(long, default_value_t = 8000)]
        budget_tokens: usize,
        /// Hard node ceiling. `0` = unlimited.
        #[arg(long, default_value_t = 0)]
        max_nodes: usize,
        /// Chars of body kept per page summary. Default
        /// 600 (~150 tokens).
        #[arg(long, default_value_t = 600)]
        summary_chars: usize,
    },
    /// Tree-sitter-extracted **code-symbol graph** for a
    /// project root. Walks `.rs` files (TS/JS/Python come in
    /// follow-up PRs), emits functions / structs / traits /
    /// impls as nodes, plus `Imports` / `Calls` /
    /// `Implements` / `Defines` edges with `extracted` /
    /// `inferred` confidence labels (matching graphify's
    /// schema).
    ///
    /// This is the structural complement to the markdown
    /// wiki — agents can ask "what calls `foo`?" and get a
    /// real graph, not a grep.
    Code {
        /// Project root. Default: current directory.
        #[arg(short, long, default_value = ".")]
        root: std::path::PathBuf,
        /// Emit JSON (the full graph) instead of the text
        /// summary.
        #[arg(long)]
        json: bool,
        /// Render the full `GRAPH_REPORT.md` shape (god
        /// nodes, fan-out hubs, kind histogram). Mutually
        /// exclusive with `--json`.
        #[arg(long, conflicts_with = "json")]
        report: bool,
        /// Top-N node summary in the text + report views.
        #[arg(long, default_value_t = 25)]
        top: usize,
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
    /// Rewrite a thin wiki page into a proper reference
    /// article. Reads the existing page + its `sources:`,
    /// prompts the LLM to expand with code examples + sharper
    /// structure, overwrites the page in place.
    Deepen {
        /// Vault root (the `Wiki/Knowledge/` parent).
        #[arg(short, long, default_value = "examples/vault")]
        vault: std::path::PathBuf,
        /// Page path relative to the wiki root, e.g.
        /// `wiki/concepts/borrowing-over-cloning.md`.
        #[arg(short, long)]
        page: String,
        #[arg(short, long)]
        model: Option<String>,
        #[arg(long, default_value_t = 600)]
        timeout_secs: u64,
        #[arg(long, default_value = "English")]
        language: String,
    },
    /// `Wiki/schema.md` + `Wiki/purpose.md` operations.
    /// Talks to the server over vox (per-org); these are
    /// the canonical authoring + bootstrap entry points.
    #[command(subcommand)]
    Schema(WikiSchemaCmd),
    /// `Wiki/index.md` catalog operations.
    #[command(subcommand)]
    Catalog(WikiCatalogCmd),
    /// `Wiki/raw/sources/` — listing + reading + deleting
    /// raw sources via the server.
    #[command(subcommand)]
    Raw(WikiRawCmd),
    /// LLM ingest queue — list, retry, cancel pending
    /// ingestion tasks. The actual `enqueue` happens via
    /// `wiki import` + `wiki rescan` (existing FS verbs).
    #[command(subcommand)]
    IngestQueue(WikiIngestCmd),
    /// Lint findings (RPC) — list + resolve via the server.
    /// The lint *runner* (LLM pass) is `wiki lint`. This
    /// sub-tree marks existing findings resolved /
    /// dismissed / deferred.
    #[command(subcommand)]
    LintFindings(WikiFindingsCmd),
    /// Review queue — LLM-proposed page changes await curator
    /// approval here. `list` shows pending items; `apply`
    /// accepts the proposal (rewrite-page / append-note / etc).
    #[command(subcommand)]
    Review(WikiReviewCmd),
    /// Research plans — list + status. The proposer is the
    /// existing flat `wiki research` (LLM call). This sub-tree
    /// manages the plans the server tracks.
    #[command(subcommand)]
    ResearchPlans(WikiResearchCmd),
    /// Filesystem watcher — re-ingest on external edits.
    #[command(subcommand)]
    Watch(WikiWatchCmd),
}

#[derive(Subcommand)]
enum WikiReviewCmd {
    /// List every open review item.
    List {
        #[arg(long, default_value = "default")]
        wiki_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Accept the LLM's proposed action on a review item.
    ///
    /// Actions:
    /// - `rewrite-page <path> <body|->`  — replace a page's body
    /// - `append-note <path> <body|->`   — append to the page
    /// - `research <query>`              — convert to a ResearchPlan
    ///
    /// Body args read stdin when given as `-`.
    Apply {
        item_id: String,
        /// `rewrite-page` / `append-note` / `research`.
        action: String,
        /// First positional arg: page path (for rewrite/append)
        /// or query text (for research).
        arg: String,
        /// Second positional: markdown body for rewrite /
        /// append (`-` = stdin). Unused for `research`.
        #[arg(default_value = "")]
        body: String,
        #[arg(long, default_value = "default")]
        wiki_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
}

#[derive(Subcommand)]
enum WikiResearchCmd {
    /// List every research plan and its status.
    List {
        #[arg(long, default_value = "default")]
        wiki_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Set the status of a research plan.
    /// `proposed|running|awaiting|integrated|cancelled`.
    SetStatus {
        plan_id: String,
        status: String,
        #[arg(long, default_value = "default")]
        wiki_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
}

#[derive(Subcommand)]
enum WikiWatchCmd {
    /// Enable filesystem watch on `Wiki/raw/sources/` so
    /// dropping a file there auto-enqueues an ingest.
    On {
        #[arg(long, default_value = "default")]
        wiki_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    Off {
        #[arg(long, default_value = "default")]
        wiki_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    Status {
        #[arg(long, default_value = "default")]
        wiki_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
}

#[derive(Subcommand)]
enum WikiSchemaCmd {
    /// Print `Wiki/schema.md`.
    Show {
        #[arg(long, default_value = "default")]
        wiki_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Print `Wiki/purpose.md`.
    Purpose {
        #[arg(long, default_value = "default")]
        wiki_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Replace `Wiki/schema.md`. Read body from `<path>` or
    /// `-` for stdin.
    WriteSchema {
        path: String,
        #[arg(long, default_value = "default")]
        wiki_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Replace `Wiki/purpose.md`.
    WritePurpose {
        path: String,
        #[arg(long, default_value = "default")]
        wiki_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Initialize `Wiki/` if missing. Idempotent.
    Bootstrap {
        #[arg(long, default_value = "default")]
        wiki_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Server-side health snapshot (orphan count, lint
    /// queue depth, last ingest mtime, …).
    Health {
        #[arg(long, default_value = "default")]
        wiki_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
}

#[derive(Subcommand)]
enum WikiCatalogCmd {
    /// Dump the catalog (`Wiki/index.md` parsed).
    Show {
        #[arg(long, default_value = "default")]
        wiki_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Force-rebuild the catalog by re-scanning the vault.
    Rebuild {
        #[arg(long, default_value = "default")]
        wiki_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
}

#[derive(Subcommand)]
enum WikiRawCmd {
    /// List every raw source the wiki carries.
    List {
        #[arg(long, default_value = "default")]
        wiki_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Print the raw source bytes to stdout.
    Read {
        path: String,
        #[arg(long, default_value = "default")]
        wiki_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Delete a raw source. Returns the review items the
    /// server enqueued for any pages that depended on it.
    Delete {
        path: String,
        #[arg(long, short = 'y')]
        yes: bool,
        #[arg(long, default_value = "default")]
        wiki_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Rescan `Wiki/raw/sources/`; enqueues fresh ingest
    /// tasks for any new files since last scan.
    Rescan {
        #[arg(long, default_value = "default")]
        wiki_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
}

#[derive(Subcommand)]
enum WikiIngestCmd {
    List {
        #[arg(long, default_value = "default")]
        wiki_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Retry a previously-failed ingest task.
    Retry {
        task_id: String,
        #[arg(long, default_value = "default")]
        wiki_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Cancel a pending or running ingest task.
    Cancel {
        task_id: String,
        #[arg(long, default_value = "default")]
        wiki_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
}

#[derive(Subcommand)]
enum WikiFindingsCmd {
    /// All open lint findings.
    List {
        #[arg(long, default_value = "default")]
        wiki_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Mark a finding resolved (or `dismiss` / `defer`).
    Resolve {
        finding_id: String,
        /// `resolved` / `dismissed` / `deferred`.
        action: String,
        #[arg(long, default_value = "default")]
        wiki_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
}

#[derive(Subcommand)]
enum AgentQueueCmd {
    /// Snapshot a queue's tasks + the latest event-log
    /// watermark in one round trip.
    Read {
        /// Queue id (slug). Defaults to the org slug.
        #[arg(long)]
        queue: Option<String>,
        /// Only my tasks (by handle).
        #[arg(long)]
        only_handle: Option<String>,
        #[arg(long)]
        include_archived: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Atomic claim — flips `ready` + unclaimed → `running`.
    Claim {
        task_id: String,
        /// Caller handle (e.g. `codex@host-1`). Defaults to
        /// `${USER}@${HOSTNAME}`.
        #[arg(long)]
        handle: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Set a non-`running` status. `running` rejected — use
    /// `claim`.
    SetStatus {
        task_id: String,
        new_status: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Mark `done` with a result blob (JSON-serialisable
    /// string; the queue stores it verbatim).
    Complete {
        task_id: String,
        /// Result payload (or `-` for stdin).
        result: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Link this agent task to an in-flight thread/session.
    Link {
        task_id: String,
        session_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// List edges where either endpoint belongs to `queue_id`.
    Links {
        #[arg(long)]
        queue: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        #[arg(long)]
        json: bool,
    },
}

#[derive(Subcommand)]
enum AgentCmd {
    /// Agent task queue lifecycle — read / claim / set-status
    /// / complete. Mirrors the `AgentTaskQueue` RPC the server
    /// mounts on `/org/<slug>/vox`.
    #[command(subcommand)]
    Queue(AgentQueueCmd),
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
        Commands::Issue(cmd) => {
            return Box::pin(run_issue(cmd)).await;
        }
        Commands::Setup(cmd) => {
            return Box::pin(run_setup(cmd)).await;
        }
        Commands::Code(cmd) => {
            return Box::pin(run_code(cmd)).await;
        }
        Commands::Label(cmd) => {
            return run_label(cmd);
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
        Commands::Body(cmd) => {
            return Box::pin(run_body(cmd)).await;
        }
        Commands::Exercise(cmd) => {
            return Box::pin(run_exercise(cmd)).await;
        }
        Commands::Workout(cmd) => {
            return Box::pin(run_workout(cmd)).await;
        }
        Commands::Intake(cmd) => {
            return Box::pin(run_intake(cmd)).await;
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
                target_date: None,
                progress_percent: -1,
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
        ProjectCmd::SetTarget {
            target,
            date,
            org,
            server,
        } => {
            let parsed = if matches!(
                date.trim().to_ascii_lowercase().as_str(),
                "none" | "clear" | "null" | ""
            ) {
                None
            } else {
                Some(
                    date.parse::<chrono::NaiveDate>()
                        .map_err(|e| eyre::eyre!("target date `{date}` (want YYYY-MM-DD): {e}"))?,
                )
            };
            mutate_project(target, org, server, |p| p.target_date = parsed).await?;
        }
        ProjectCmd::Progress {
            target,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let pc = connect_project_client(&url).await?;
            let proj = resolve_project_target(&pc, &target).await?;
            // Count tasks whose project_id == this project.
            let tc = connect_task_client(&url).await?;
            let tasks = tc
                .list()
                .await
                .map_err(|e| eyre::eyre!("list tasks: {e:?}"))?;
            let mine: Vec<&task::TaskInfo> = tasks
                .iter()
                .filter(|t| t.project_id == Some(proj.id))
                .collect();
            let total = mine.len();
            let done = mine
                .iter()
                .filter(|t| matches!(task::Status::from_str(&t.status), Some(task::Status::Done)))
                .count();
            let pct: i16 = if total == 0 {
                -1
            } else {
                i16::try_from((done * 100) / total).unwrap_or(100)
            };
            // Persist the rollup.
            let mut p = proj.clone();
            p.progress_percent = pct;
            pc.update(p)
                .await
                .map_err(|e| eyre::eyre!("update: {e:?}"))?;
            let shown = if pct < 0 {
                "—".to_string()
            } else {
                format!("{pct}%")
            };
            println!("{}  {}", proj.title, shown);
            println!("  {done}/{total} tasks done");
            if let Some(d) = proj.target_date {
                println!("  target: {d}");
            }
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
        WikiCmd::Context {
            query,
            vault,
            node_type,
            budget_tokens,
            max_nodes,
            summary_chars,
        } => {
            let vault = vault
                .canonicalize()
                .map_err(|e| eyre::eyre!("vault {}: {e}", vault.display()))?;
            let result = wiki_graph::build_context(
                &vault,
                wiki_graph::ContextOpts {
                    query,
                    node_type,
                    budget_tokens,
                    max_nodes,
                    summary_chars,
                },
            )
            .map_err(|e| eyre::eyre!("build_context: {e}"))?;
            // The whole point is that the markdown is the
            // output — print it to stdout so the caller can
            // pipe it directly into an LLM prompt.
            print!("{}", result.markdown);
            eprintln!(
                "\n[wiki context] {}/{} nodes, ~{} tokens (budget {})",
                result.included.len(),
                result.nodes_considered,
                result.tokens_estimate,
                budget_tokens
            );
            Ok(())
        }
        WikiCmd::Code {
            root,
            json,
            report,
            top,
        } => {
            let root = root
                .canonicalize()
                .map_err(|e| eyre::eyre!("root {}: {e}", root.display()))?;
            let extractions = wiki_graph::scan_code_tree(&root);

            if report {
                // Cross-file analysis + GRAPH_REPORT.md
                // render. This is the agent-context payload
                // for "give me a high-level map of this
                // codebase".
                let g = wiki_graph::analyze(&extractions);
                print!("{}", wiki_graph::render_report(&g, top));
                eprintln!(
                    "[wiki code report] {} nodes, {} edges ({} resolved, {} unresolved)",
                    g.nodes.len(),
                    g.edges.len(),
                    g.resolved_edges,
                    g.unresolved_edges
                );
                return Ok(());
            }
            let mut total_nodes = 0usize;
            let mut total_edges = 0usize;
            let mut by_kind: std::collections::HashMap<&str, usize> =
                std::collections::HashMap::new();
            let mut all_nodes: Vec<wiki_graph::CodeNode> = Vec::new();
            let mut all_edges: Vec<wiki_graph::CodeEdge> = Vec::new();
            let mut errors = Vec::new();
            for ex in extractions {
                total_nodes += ex.nodes.len();
                total_edges += ex.edges.len();
                for n in &ex.nodes {
                    *by_kind.entry(n.kind.as_str()).or_insert(0) += 1;
                }
                all_nodes.extend(ex.nodes);
                all_edges.extend(ex.edges);
                errors.extend(ex.errors);
            }
            if json {
                let payload = serde_json::json!({
                    "root": root.display().to_string(),
                    "totals": {
                        "nodes": total_nodes,
                        "edges": total_edges,
                        "errors": errors.len(),
                    },
                    "by_kind": by_kind,
                    "nodes": all_nodes.iter().map(|n| serde_json::json!({
                        "id": n.id,
                        "label": n.label,
                        "kind": n.kind.as_str(),
                        "language": n.language.tag(),
                        "source_file": n.source_file.display().to_string(),
                        "line_start": n.line_start,
                        "line_end": n.line_end,
                    })).collect::<Vec<_>>(),
                    "edges": all_edges.iter().map(|e| serde_json::json!({
                        "source": e.source,
                        "target": e.target,
                        "relation": e.relation.as_str(),
                        "confidence": e.confidence.as_str(),
                    })).collect::<Vec<_>>(),
                    "errors": errors,
                });
                println!("{}", serde_json::to_string_pretty(&payload)?);
            } else {
                println!("root: {}", root.display());
                println!("nodes: {total_nodes}, edges: {total_edges}");
                if !by_kind.is_empty() {
                    let mut kinds: Vec<_> = by_kind.iter().collect();
                    kinds.sort_by(|a, b| b.1.cmp(a.1));
                    println!("by kind:");
                    for (k, n) in kinds {
                        println!("  {n:>5} {k}");
                    }
                }
                // Top-N nodes by how often they appear as a
                // target — i.e. most-called / most-imported.
                use std::collections::HashMap;
                let mut indeg: HashMap<&str, usize> = HashMap::new();
                for e in &all_edges {
                    *indeg.entry(e.target.as_str()).or_insert(0) += 1;
                }
                let mut ranked: Vec<_> = all_nodes
                    .iter()
                    .map(|n| (n, indeg.get(n.id.as_str()).copied().unwrap_or(0)))
                    .collect();
                ranked.sort_by(|a, b| b.1.cmp(&a.1));
                println!("\ntop {top} by in-degree (high = referenced a lot):");
                for (n, deg) in ranked.iter().take(top) {
                    println!(
                        "  {deg:>3} {:<8} {} ({}:L{})",
                        n.kind.as_str(),
                        n.label,
                        n.source_file.display(),
                        n.line_start + 1
                    );
                }
                if !errors.is_empty() {
                    eprintln!("\n{} extraction error(s):", errors.len());
                    for e in errors.iter().take(5) {
                        eprintln!("  - {e}");
                    }
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
        WikiCmd::Deepen {
            vault,
            page,
            model,
            timeout_secs,
            language,
        } => {
            let vault = vault
                .canonicalize()
                .map_err(|e| eyre::eyre!("vault {}: {e}", vault.display()))?;
            let wiki = WikiLive::open(&vault);
            let backend = CodexBackend::new();
            eprintln!(
                "› deepen@{} page={page}",
                model.as_deref().unwrap_or("default")
            );
            let result = agent_wiki::bridge::run_deepen(
                &backend,
                &wiki,
                &page,
                model,
                Duration::from_secs(timeout_secs),
                &language,
            )
            .await
            .map_err(|e| eyre::eyre!("deepen: {e}"))?;
            println!("Deepen done.");
            println!("  page:  {}", result.page_path);
            println!(
                "  words: {} → {} ({:+})",
                result.before_words,
                result.after_words,
                result.after_words as i64 - result.before_words as i64
            );
            Ok(())
        }
        WikiCmd::Schema(c) => run_wiki_schema(c).await,
        WikiCmd::Catalog(c) => run_wiki_catalog(c).await,
        WikiCmd::Raw(c) => run_wiki_raw(c).await,
        WikiCmd::IngestQueue(c) => run_wiki_ingest(c).await,
        WikiCmd::LintFindings(c) => run_wiki_lint_findings(c).await,
        WikiCmd::Review(c) => run_wiki_review(c).await,
        WikiCmd::ResearchPlans(c) => run_wiki_research_plans(c).await,
        WikiCmd::Watch(c) => run_wiki_watch(c).await,
    }
}

// ── Wiki RPC handlers ────────────────────────────────────────────────

async fn run_wiki_schema(cmd: WikiSchemaCmd) -> eyre::Result<()> {
    use wiki_proto::service::schema::SchemaClient;
    async fn connect(url: &str) -> eyre::Result<SchemaClient> {
        Box::pin(vox::connect(url).establish())
            .await
            .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))
    }
    match cmd {
        WikiSchemaCmd::Show {
            wiki_id,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let c = connect(&u).await?;
            let doc = c
                .read_schema(wiki_id)
                .await
                .map_err(|e| eyre::eyre!("read_schema: {e:?}"))?;
            if json {
                println!("{doc:#?}");
            } else {
                println!("{}", doc.markdown);
            }
        }
        WikiSchemaCmd::Purpose {
            wiki_id,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let c = connect(&u).await?;
            let doc = c
                .read_purpose(wiki_id)
                .await
                .map_err(|e| eyre::eyre!("read_purpose: {e:?}"))?;
            if json {
                println!("{doc:#?}");
            } else {
                println!("{}", doc.markdown);
            }
        }
        WikiSchemaCmd::WriteSchema {
            path,
            wiki_id,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let c = connect(&u).await?;
            let body = if path == "-" {
                let mut s = String::new();
                std::io::Read::read_to_string(&mut std::io::stdin(), &mut s)?;
                s
            } else {
                std::fs::read_to_string(&path)?
            };
            c.write_schema(wiki_id, body)
                .await
                .map_err(|e| eyre::eyre!("write_schema: {e:?}"))?;
            println!("wrote schema");
        }
        WikiSchemaCmd::WritePurpose {
            path,
            wiki_id,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let c = connect(&u).await?;
            let body = if path == "-" {
                let mut s = String::new();
                std::io::Read::read_to_string(&mut std::io::stdin(), &mut s)?;
                s
            } else {
                std::fs::read_to_string(&path)?
            };
            c.write_purpose(wiki_id, body)
                .await
                .map_err(|e| eyre::eyre!("write_purpose: {e:?}"))?;
            println!("wrote purpose");
        }
        WikiSchemaCmd::Bootstrap {
            wiki_id,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let c = connect(&u).await?;
            c.bootstrap(wiki_id.clone())
                .await
                .map_err(|e| eyre::eyre!("bootstrap: {e:?}"))?;
            println!("bootstrapped {wiki_id}");
        }
        WikiSchemaCmd::Health {
            wiki_id,
            org,
            server,
            json: _,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let c = connect(&u).await?;
            let h = c
                .health(wiki_id)
                .await
                .map_err(|e| eyre::eyre!("health: {e:?}"))?;
            println!("{h:#?}");
        }
    }
    Ok(())
}

async fn run_wiki_catalog(cmd: WikiCatalogCmd) -> eyre::Result<()> {
    use wiki_proto::service::catalog::CatalogClient;
    async fn connect(url: &str) -> eyre::Result<CatalogClient> {
        Box::pin(vox::connect(url).establish())
            .await
            .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))
    }
    match cmd {
        WikiCatalogCmd::Show {
            wiki_id,
            org,
            server,
            json: _,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let c = connect(&u).await?;
            let idx = c
                .read_index(wiki_id)
                .await
                .map_err(|e| eyre::eyre!("read_index: {e:?}"))?;
            println!("{idx:#?}");
        }
        WikiCatalogCmd::Rebuild {
            wiki_id,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let c = connect(&u).await?;
            let idx = c
                .rebuild_index(wiki_id)
                .await
                .map_err(|e| eyre::eyre!("rebuild_index: {e:?}"))?;
            if json {
                println!("{idx:#?}");
            } else {
                println!("rebuilt catalog");
            }
        }
    }
    Ok(())
}

async fn run_wiki_raw(cmd: WikiRawCmd) -> eyre::Result<()> {
    use wiki_proto::service::raw_layer::RawLayerClient;
    async fn connect(url: &str) -> eyre::Result<RawLayerClient> {
        Box::pin(vox::connect(url).establish())
            .await
            .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))
    }
    match cmd {
        WikiRawCmd::List {
            wiki_id,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let c = connect(&u).await?;
            let rows = c
                .list_raw_sources(wiki_id)
                .await
                .map_err(|e| eyre::eyre!("list_raw_sources: {e:?}"))?;
            if json {
                println!("{rows:#?}");
            } else {
                for r in &rows {
                    println!("{r:?}");
                }
            }
        }
        WikiRawCmd::Read {
            path,
            wiki_id,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let c = connect(&u).await?;
            let bytes = c
                .read_raw_source(wiki_id, path)
                .await
                .map_err(|e| eyre::eyre!("read_raw_source: {e:?}"))?;
            std::io::Write::write_all(&mut std::io::stdout(), &bytes)?;
        }
        WikiRawCmd::Delete {
            path,
            yes,
            wiki_id,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let c = connect(&u).await?;
            if !yes && !confirm(&format!("delete raw source `{path}`?"))? {
                println!("aborted");
                return Ok(());
            }
            let reviews = c
                .delete_raw_source(wiki_id, path.clone())
                .await
                .map_err(|e| eyre::eyre!("delete_raw_source: {e:?}"))?;
            println!("deleted {path} ({} review items enqueued)", reviews.len());
        }
        WikiRawCmd::Rescan {
            wiki_id,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let c = connect(&u).await?;
            let tasks = c
                .rescan_sources(wiki_id)
                .await
                .map_err(|e| eyre::eyre!("rescan_sources: {e:?}"))?;
            println!("enqueued {} ingest task(s)", tasks.len());
        }
    }
    Ok(())
}

async fn run_wiki_ingest(cmd: WikiIngestCmd) -> eyre::Result<()> {
    use wiki_proto::service::ingest::IngestClient;
    async fn connect(url: &str) -> eyre::Result<IngestClient> {
        Box::pin(vox::connect(url).establish())
            .await
            .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))
    }
    match cmd {
        WikiIngestCmd::List {
            wiki_id,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let c = connect(&u).await?;
            let rows = c
                .list_ingest(wiki_id)
                .await
                .map_err(|e| eyre::eyre!("list_ingest: {e:?}"))?;
            if json {
                println!("{rows:#?}");
            } else {
                for t in &rows {
                    println!("{t:#?}");
                }
            }
        }
        WikiIngestCmd::Retry {
            task_id,
            wiki_id,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let c = connect(&u).await?;
            let t = c
                .retry_ingest(wiki_id, task_id)
                .await
                .map_err(|e| eyre::eyre!("retry_ingest: {e:?}"))?;
            println!("retrying {t:#?}");
        }
        WikiIngestCmd::Cancel {
            task_id,
            wiki_id,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let c = connect(&u).await?;
            c.cancel_ingest(wiki_id, task_id.clone())
                .await
                .map_err(|e| eyre::eyre!("cancel_ingest: {e:?}"))?;
            println!("cancelled {task_id}");
        }
    }
    Ok(())
}

async fn run_wiki_lint_findings(cmd: WikiFindingsCmd) -> eyre::Result<()> {
    use wiki_proto::service::lint::LintClient;
    async fn connect(url: &str) -> eyre::Result<LintClient> {
        Box::pin(vox::connect(url).establish())
            .await
            .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))
    }
    match cmd {
        WikiFindingsCmd::List {
            wiki_id,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let c = connect(&u).await?;
            let rows = c
                .list_findings(wiki_id)
                .await
                .map_err(|e| eyre::eyre!("list_findings: {e:?}"))?;
            if json {
                println!("{rows:#?}");
            } else {
                for f in &rows {
                    println!("{f:#?}");
                }
            }
        }
        WikiFindingsCmd::Resolve {
            finding_id,
            action,
            wiki_id,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let c = connect(&u).await?;
            let parsed = match action.as_str() {
                "resolve" | "resolved" => wiki_proto::lint::FindingAction::Resolve,
                "dismiss" | "dismissed" => wiki_proto::lint::FindingAction::Dismiss {
                    reason: String::new(),
                },
                "promote-review" | "review" => wiki_proto::lint::FindingAction::PromoteToReview,
                "promote-research" | "research" => {
                    wiki_proto::lint::FindingAction::PromoteToResearch
                }
                other => {
                    return Err(eyre::eyre!(
                        "unknown action `{other}` — try resolve / dismiss / promote-review / promote-research"
                    ));
                }
            };
            c.resolve_finding(wiki_id, finding_id.clone(), parsed)
                .await
                .map_err(|e| eyre::eyre!("resolve_finding: {e:?}"))?;
            println!("{finding_id} → {action}");
        }
    }
    Ok(())
}

async fn run_wiki_review(cmd: WikiReviewCmd) -> eyre::Result<()> {
    use wiki_proto::review::{ReviewAction, ReviewItem};
    use wiki_proto::service::review::ReviewClient;
    async fn connect(url: &str) -> eyre::Result<ReviewClient> {
        Box::pin(vox::connect(url).establish())
            .await
            .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))
    }
    let read_body = |s: String| -> eyre::Result<String> {
        if s == "-" {
            let mut buf = String::new();
            std::io::Read::read_to_string(&mut std::io::stdin(), &mut buf)?;
            Ok(buf)
        } else if std::path::Path::new(&s).exists() {
            Ok(std::fs::read_to_string(&s)?)
        } else {
            Ok(s)
        }
    };
    match cmd {
        WikiReviewCmd::List {
            wiki_id,
            org,
            server,
            json: _,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let c = connect(&u).await?;
            let rows: Vec<ReviewItem> = c
                .list_review(wiki_id)
                .await
                .map_err(|e| eyre::eyre!("list_review: {e:?}"))?;
            if rows.is_empty() {
                println!("(no pending review items)");
            }
            for r in &rows {
                println!("{r:#?}");
            }
        }
        WikiReviewCmd::Apply {
            item_id,
            action,
            arg,
            body,
            wiki_id,
            org,
            server,
        } => {
            let parsed = match action.as_str() {
                "rewrite-page" => ReviewAction::RewritePage {
                    path: arg.clone(),
                    markdown: read_body(body)?,
                },
                "append-note" => ReviewAction::AppendNote {
                    path: arg.clone(),
                    body: read_body(body)?,
                },
                "research" => ReviewAction::Research { query: arg.clone() },
                other => {
                    return Err(eyre::eyre!(
                        "unknown action `{other}` — try rewrite-page / append-note / research"
                    ));
                }
            };
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let c = connect(&u).await?;
            c.apply_review(wiki_id, item_id.clone(), parsed)
                .await
                .map_err(|e| eyre::eyre!("apply_review: {e:?}"))?;
            println!("applied {action} on {item_id}");
        }
    }
    Ok(())
}

async fn run_wiki_research_plans(cmd: WikiResearchCmd) -> eyre::Result<()> {
    use wiki_proto::research::ResearchStatus;
    use wiki_proto::service::research::ResearchClient;
    async fn connect(url: &str) -> eyre::Result<ResearchClient> {
        Box::pin(vox::connect(url).establish())
            .await
            .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))
    }
    match cmd {
        WikiResearchCmd::List {
            wiki_id,
            org,
            server,
            json: _,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let c = connect(&u).await?;
            let rows = c
                .list_research(wiki_id)
                .await
                .map_err(|e| eyre::eyre!("list_research: {e:?}"))?;
            if rows.is_empty() {
                println!("(no research plans)");
            }
            for p in &rows {
                println!("{p:#?}");
            }
        }
        WikiResearchCmd::SetStatus {
            plan_id,
            status,
            wiki_id,
            org,
            server,
        } => {
            let parsed = match status.to_ascii_lowercase().as_str() {
                "proposed" => ResearchStatus::Proposed,
                "running" => ResearchStatus::Running,
                "awaiting" => ResearchStatus::Awaiting,
                "submitted" => ResearchStatus::Submitted,
                "cancelled" | "canceled" => ResearchStatus::Cancelled,
                other => {
                    return Err(eyre::eyre!(
                        "unknown status `{other}` — try proposed / running / awaiting / submitted / cancelled"
                    ));
                }
            };
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let c = connect(&u).await?;
            c.set_research_status(wiki_id, plan_id.clone(), parsed)
                .await
                .map_err(|e| eyre::eyre!("set_research_status: {e:?}"))?;
            println!("{plan_id} → {status}");
        }
    }
    Ok(())
}

async fn run_wiki_watch(cmd: WikiWatchCmd) -> eyre::Result<()> {
    use wiki_proto::service::watcher::WatcherClient;
    async fn connect(url: &str) -> eyre::Result<WatcherClient> {
        Box::pin(vox::connect(url).establish())
            .await
            .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))
    }
    match cmd {
        WikiWatchCmd::On {
            wiki_id,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let c = connect(&u).await?;
            let r = c
                .set_watch(wiki_id, true)
                .await
                .map_err(|e| eyre::eyre!("set_watch: {e:?}"))?;
            println!("watch enabled: {r}");
        }
        WikiWatchCmd::Off {
            wiki_id,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let c = connect(&u).await?;
            let r = c
                .set_watch(wiki_id, false)
                .await
                .map_err(|e| eyre::eyre!("set_watch: {e:?}"))?;
            println!("watch disabled: {r}");
        }
        WikiWatchCmd::Status {
            wiki_id,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let c = connect(&u).await?;
            let r = c
                .is_watching(wiki_id)
                .await
                .map_err(|e| eyre::eyre!("is_watching: {e:?}"))?;
            println!("watching: {r}");
        }
    }
    Ok(())
}

async fn run_agent(cmd: AgentCmd) -> eyre::Result<()> {
    use std::io::Write;

    use agent_codex::{ChatOpts, CodexBackend};
    use agent_proto::event::AgentEvent;
    use futures::StreamExt;

    match cmd {
        AgentCmd::Queue(qc) => Box::pin(run_agent_queue(qc)).await,
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
                workflow: None,
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

/// Parse an `AgentRef` from CLI input. Accepted forms:
/// `agent:name`, `agent:name@version`, `human:user_id`, or
/// a bare `name` (defaults to an unversioned agent).
fn parse_agent_ref(s: &str) -> eyre::Result<workflows_proto::AgentRef> {
    let s = s.trim();
    if s.is_empty() {
        return Err(eyre::eyre!("empty agent ref"));
    }
    if let Some(rest) = s.strip_prefix("human:") {
        let rest = rest.trim();
        if rest.is_empty() {
            return Err(eyre::eyre!("human: prefix requires a user id"));
        }
        return Ok(workflows_proto::AgentRef::human(rest));
    }
    let body = s.strip_prefix("agent:").unwrap_or(s);
    let body = body.trim();
    if body.is_empty() {
        return Err(eyre::eyre!("agent: prefix requires a name"));
    }
    if let Some((name, ver)) = body.split_once('@') {
        let name = name.trim();
        let ver = ver.trim();
        if name.is_empty() {
            return Err(eyre::eyre!("agent name is empty"));
        }
        if ver.is_empty() {
            return Ok(workflows_proto::AgentRef::agent(name));
        }
        return Ok(workflows_proto::AgentRef::agent_versioned(name, ver));
    }
    Ok(workflows_proto::AgentRef::agent(body))
}

/// Parse `xs|s|m|l|xl` or a numeric points value into an
/// [`task::model::Estimate`].
fn parse_estimate(s: &str) -> eyre::Result<task::model::Estimate> {
    use task::model::Estimate;
    match s.trim().to_ascii_lowercase().as_str() {
        "xs" => Ok(Estimate::XS),
        "s" => Ok(Estimate::S),
        "m" => Ok(Estimate::M),
        "l" => Ok(Estimate::L),
        "xl" => Ok(Estimate::XL),
        other => {
            let value: u8 = other
                .parse()
                .map_err(|e| eyre::eyre!("bad estimate `{other}`: {e}"))?;
            Ok(Estimate::Points { value })
        }
    }
}

/// Resolve an issue id — accepts a full UUID or an 8-char
/// (or longer) prefix. Falls back to a list-scan for the
/// prefix case since the server only exposes exact lookups.
async fn resolve_issue_id(
    client: &task::TaskServiceClient,
    id: &str,
) -> eyre::Result<task::TaskInfo> {
    if let Ok(uuid) = uuid::Uuid::parse_str(id) {
        return client
            .get(uuid)
            .await
            .map_err(|e| eyre::eyre!("get(id): {e:?}"));
    }
    let prefix = id.trim().to_ascii_lowercase();
    if prefix.is_empty() {
        return Err(eyre::eyre!("empty id"));
    }
    let rows = client
        .list()
        .await
        .map_err(|e| eyre::eyre!("list: {e:?}"))?;
    let mut hits: Vec<task::TaskInfo> = rows
        .into_iter()
        .filter(|t| t.id.to_string().to_ascii_lowercase().starts_with(&prefix))
        .collect();
    match hits.len() {
        0 => Err(eyre::eyre!("no issue matches `{id}`")),
        1 => Ok(hits.remove(0)),
        n => Err(eyre::eyre!(
            "`{id}` matches {n} issues — disambiguate with the full UUID"
        )),
    }
}

fn short_uuid(u: &uuid::Uuid) -> String {
    let s = u.to_string();
    s.chars().take(8).collect()
}

#[allow(clippy::ref_option)] // ergonomic: callers pass `&t.workflow` directly
fn workflow_summary(w: &Option<task::model::WorkflowAttrs>) -> String {
    let Some(w) = w else {
        return "—".into();
    };
    let cy = w.cycle.as_ref().map_or("—".into(), short_uuid);
    format!("cy={cy}")
}

fn print_workflow_block(w: &task::model::WorkflowAttrs) {
    use task::model::Estimate;
    println!("  workflow:");
    if let Some(cy) = w.cycle {
        println!("    cycle:     {cy}");
    }
    if let Some(est) = &w.estimate {
        let rendered = match est {
            Estimate::XS => "xs".to_string(),
            Estimate::S => "s".to_string(),
            Estimate::M => "m".to_string(),
            Estimate::L => "l".to_string(),
            Estimate::XL => "xl".to_string(),
            Estimate::Points { value } => format!("{value} pts"),
        };
        println!("    estimate:  {rendered}");
    }
    if let Some(sid) = w.session {
        println!("    session:   {sid}");
    }
    if !w.assignees.is_empty() {
        println!("    assignees:");
        for a in w.assignees.iter() {
            println!("      - {}", a.short_label());
        }
    }
    if !w.blockers.is_empty() {
        println!("    blockers:");
        for b in w.blockers.iter() {
            println!("      - {b}");
        }
    }
    if !w.relates_to.is_empty() {
        println!("    relates_to:");
        for r in w.relates_to.iter() {
            println!("      - {r}");
        }
    }
}

/// Result of an atomic claim attempt.
enum ClaimOutcome {
    /// This agent now holds the claim.
    Won,
    /// This agent already held it (idempotent).
    AlreadyMine,
    /// Another actor holds it; carries their label.
    Lost(String),
}

/// Atomic claim via the server-side `try_claim` RPC. The backend
/// serializes the read-check-write under a process lock, so two
/// agents racing for the same task can't both win — no TOCTOU
/// window (unlike the old client-side optimistic version). The
/// agent is sent as a JSON-encoded `AgentRef`.
async fn try_claim(
    client: &task::TaskServiceClient,
    task_id: &uuid::Uuid,
    agent: &workflows_proto::AgentRef,
    force: bool,
) -> eyre::Result<ClaimOutcome> {
    let agent_json = serde_json::to_string(agent).map_err(|e| eyre::eyre!("encode agent: {e}"))?;
    let res = client
        .try_claim(*task_id, agent_json, force)
        .await
        .map_err(|e| eyre::eyre!("try_claim: {e:?}"))?;
    Ok(match res {
        task::service::ClaimResult::Won => ClaimOutcome::Won,
        task::service::ClaimResult::AlreadyMine => ClaimOutcome::AlreadyMine,
        task::service::ClaimResult::Lost { holder } => ClaimOutcome::Lost(holder),
    })
}

/// Apply `set-workflow` style edits to a `TaskInfo` in-place.
#[allow(clippy::too_many_arguments)]
fn apply_workflow_patch(
    t: &mut task::TaskInfo,
    cycle: Option<String>,
    project: Option<String>,
    estimate: Option<String>,
    add_assignee: Vec<workflows_proto::AgentRef>,
    remove_assignee: Vec<workflows_proto::AgentRef>,
    add_blocker: Vec<uuid::Uuid>,
    remove_blocker: Vec<uuid::Uuid>,
) -> eyre::Result<()> {
    fn parse_uuid_field(field: &str, raw: &str) -> eyre::Result<Option<uuid::Uuid>> {
        let r = raw.trim();
        if matches!(r, "" | "none" | "null") {
            return Ok(None);
        }
        uuid::Uuid::parse_str(r)
            .map(Some)
            .map_err(|e| eyre::eyre!("--{field} `{raw}`: {e}"))
    }

    // Project membership lives on TaskInfo.project_id (the
    // canonical Project link), not in WorkflowAttrs.
    if let Some(v) = project {
        t.project_id = parse_uuid_field("project", &v)?;
    }

    let w = t
        .workflow
        .get_or_insert_with(task::model::WorkflowAttrs::default);

    if let Some(v) = cycle {
        w.cycle = parse_uuid_field("cycle", &v)?;
    }
    if let Some(v) = estimate {
        w.estimate = Some(parse_estimate(&v)?);
    }
    for a in remove_assignee {
        w.assignees.0.retain(|x| x != &a);
    }
    for a in add_assignee {
        if !w.assignees.iter().any(|x| x == &a) {
            w.assignees.0.push(a);
        }
    }
    for b in remove_blocker {
        w.blockers.0.retain(|x| x != &b);
    }
    for b in add_blocker {
        if !w.blockers.iter().any(|x| x == &b) {
            w.blockers.0.push(b);
        }
    }
    Ok(())
}

async fn run_issue(cmd: IssueCmd) -> eyre::Result<()> {
    match cmd {
        IssueCmd::List {
            cycle,
            project,
            assignee,
            status,
            has_workflow,
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
            let assignee_ref = assignee.as_deref().map(parse_agent_ref).transpose()?;

            let mut rows: Vec<task::TaskInfo> = rows
                .into_iter()
                .filter(|t| {
                    status
                        .as_deref()
                        .is_none_or(|s| t.status.eq_ignore_ascii_case(s))
                })
                .filter(|t| !has_workflow || t.workflow.is_some())
                .filter(|t| match cycle {
                    None => true,
                    Some(c) => t.workflow.as_ref().and_then(|x| x.cycle) == Some(c),
                })
                .filter(|t| match project {
                    None => true,
                    Some(p) => t.project_id == Some(p),
                })
                .filter(|t| match &assignee_ref {
                    None => true,
                    Some(a) => t
                        .workflow
                        .as_ref()
                        .is_some_and(|w| w.assignees.iter().any(|x| x == a)),
                })
                .collect();
            rows.sort_by(|a, b| {
                let a_done = task::Status::from_str(&a.status).is_some_and(task::Status::is_done);
                let b_done = task::Status::from_str(&b.status).is_some_and(task::Status::is_done);
                a_done.cmp(&b_done).then_with(|| a.title.cmp(&b.title))
            });

            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&rows).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            if rows.is_empty() {
                println!("(no issues)");
                return Ok(());
            }
            for t in &rows {
                println!(
                    "{}  {:<10}  {:<8}  {}  {}",
                    short_uuid(&t.id),
                    t.status,
                    t.priority,
                    workflow_summary(&t.workflow),
                    t.title,
                );
            }
        }
        IssueCmd::Show {
            id,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&url).await?;
            let t = resolve_issue_id(&client, &id).await?;
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
            if let Some(p) = t.project_id {
                println!("  project:  {p}");
            }
            if let Some(m) = t.milestone_id {
                println!("  milestone:{m}");
            }
            match &t.workflow {
                Some(w) => print_workflow_block(w),
                None => println!("  workflow: (none)"),
            }
        }
        IssueCmd::SetWorkflow {
            id,
            cycle,
            project,
            estimate,
            add_assignee,
            remove_assignee,
            add_blocker,
            remove_blocker,
            clear,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&url).await?;
            let mut t = resolve_issue_id(&client, &id).await?;
            if clear {
                t.workflow = None;
            } else {
                let add: Vec<_> = add_assignee
                    .iter()
                    .map(|s| parse_agent_ref(s))
                    .collect::<eyre::Result<_>>()?;
                let rm: Vec<_> = remove_assignee
                    .iter()
                    .map(|s| parse_agent_ref(s))
                    .collect::<eyre::Result<_>>()?;
                apply_workflow_patch(
                    &mut t,
                    cycle,
                    project,
                    estimate,
                    add,
                    rm,
                    add_blocker,
                    remove_blocker,
                )?;
            }
            let updated = client
                .update(t)
                .await
                .map_err(|e| eyre::eyre!("update: {e:?}"))?;
            println!("{}  [{}]  {}", updated.title, updated.status, updated.path);
            if let Some(w) = &updated.workflow {
                print_workflow_block(w);
            } else {
                println!("  workflow: (none)");
            }
        }
        IssueCmd::Claim {
            id,
            as_agent,
            force,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&url).await?;
            let agent = parse_agent_ref(&format!("agent:{as_agent}"))?;
            let t = resolve_issue_id(&client, &id).await?;
            match try_claim(&client, &t.id, &agent, force).await? {
                ClaimOutcome::Won => {
                    println!("claimed {} by {}", short_uuid(&t.id), agent.short_label());
                }
                ClaimOutcome::AlreadyMine => {
                    println!(
                        "{} already claimed by {}",
                        short_uuid(&t.id),
                        agent.short_label()
                    );
                }
                ClaimOutcome::Lost(holder) => {
                    return Err(eyre::eyre!(
                        "{} is already claimed by {holder} — pass --force to steal",
                        short_uuid(&t.id)
                    ));
                }
            }
        }
        IssueCmd::Triage {
            id,
            subtasks,
            from,
            parent_status,
            priority,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&url).await?;
            let mut parent = resolve_issue_id(&client, &id).await?;

            // Collect subtask titles: --subtask flags + --from lines.
            let mut titles: Vec<String> = subtasks;
            if let Some(src) = from {
                let raw = if src == "-" {
                    use std::io::Read as _;
                    let mut s = String::new();
                    std::io::stdin()
                        .read_to_string(&mut s)
                        .map_err(|e| eyre::eyre!("stdin: {e}"))?;
                    s
                } else {
                    std::fs::read_to_string(&src).map_err(|e| eyre::eyre!("read {src}: {e}"))?
                };
                titles.extend(
                    raw.lines()
                        .map(str::trim)
                        .filter(|l| !l.is_empty())
                        .map(String::from),
                );
            }
            if titles.is_empty() {
                return Err(eyre::eyre!(
                    "no subtasks — pass --subtask <title> (repeatable) and/or --from <file|->"
                ));
            }

            // Create each subtask under the parent.
            for title in &titles {
                let sub = task::TaskInfo {
                    id: uuid::Uuid::nil(),
                    path: String::new(),
                    title: title.clone(),
                    status: "open".into(),
                    priority: priority.clone(),
                    due: None,
                    scheduled: None,
                    tags: task::model::StringList(vec!["task".into(), "subtask".into()]),
                    contexts: task::model::StringList::default(),
                    projects: task::model::StringList::default(),
                    project_id: parent.project_id,
                    milestone_id: None,
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
                    details: String::new(),
                    workflow: Some(task::model::WorkflowAttrs {
                        parent: Some(parent.id),
                        ..Default::default()
                    }),
                };
                client
                    .create(sub)
                    .await
                    .map_err(|e| eyre::eyre!("create subtask: {e:?}"))?;
            }

            // Flip the parent into the working state.
            parent.status = parent_status.clone();
            parent.completed_date = None;
            let parent_id = parent.id;
            client
                .update(parent)
                .await
                .map_err(|e| eyre::eyre!("update parent: {e:?}"))?;

            println!(
                "triaged {} into {} subtask(s) [parent → {parent_status}]\n",
                short_uuid(&parent_id),
                titles.len()
            );
            // Show the resulting board.
            let all = client
                .list()
                .await
                .map_err(|e| eyre::eyre!("list: {e:?}"))?;
            for t in all
                .iter()
                .filter(|t| t.workflow.as_ref().and_then(|w| w.parent) == Some(parent_id))
            {
                println!(
                    "  {}  {:<10} unclaimed   {}",
                    short_uuid(&t.id),
                    t.status,
                    t.title
                );
            }
            println!(
                "\nparallel agents now: `task issue ready --as-agent <name>` → `task issue claim <id> --as-agent <name>`"
            );
        }
        IssueCmd::Subtasks {
            id,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&url).await?;
            let parent = resolve_issue_id(&client, &id).await?;
            let all = client
                .list()
                .await
                .map_err(|e| eyre::eyre!("list: {e:?}"))?;
            let mut subs: Vec<&task::TaskInfo> = all
                .iter()
                .filter(|t| t.workflow.as_ref().and_then(|w| w.parent) == Some(parent.id))
                .collect();
            subs.sort_by(|a, b| a.status.cmp(&b.status).then_with(|| a.title.cmp(&b.title)));
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&subs).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            let done = subs
                .iter()
                .filter(|t| matches!(task::Status::from_str(&t.status), Some(task::Status::Done)))
                .count();
            println!(
                "{} [{}]  {}",
                short_uuid(&parent.id),
                parent.status,
                parent.title
            );
            println!("  {done}/{} subtasks done\n", subs.len());
            for t in &subs {
                let claim = t
                    .workflow
                    .as_ref()
                    .and_then(|w| w.assignees.0.first())
                    .map_or_else(
                        || "unclaimed".to_string(),
                        workflows_proto::AgentRef::short_label,
                    );
                println!(
                    "  {}  {:<12} {:<22} {}",
                    short_uuid(&t.id),
                    t.status,
                    claim,
                    t.title
                );
            }
        }
        IssueCmd::Assignees {
            id,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&url).await?;
            let t = resolve_issue_id(&client, &id).await?;
            let assignees: Vec<workflows_proto::AgentRef> = t
                .workflow
                .as_ref()
                .map(|w| w.assignees.0.clone())
                .unwrap_or_default();
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&assignees)
                        .map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            if assignees.is_empty() {
                println!("(no assignees)");
                return Ok(());
            }
            for a in &assignees {
                let kind = if a.is_agent() { "agent" } else { "human" };
                println!("{kind:<6}  {}", a.short_label());
            }
        }
        IssueCmd::Create {
            title,
            path,
            status,
            priority,
            cycle,
            project,
            parent,
            estimate,
            assignees,
            blockers,
            tags,
            body,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&url).await?;
            let body = resolve_body(body)?;

            // Build the WorkflowAttrs from inline flags. Skip if
            // nothing was set — leaves `workflow: None`, preserving
            // the TaskNotes-shape round-trip for plain tasks.
            let assignee_refs: Vec<workflows_proto::AgentRef> = assignees
                .iter()
                .map(|s| parse_agent_ref(s))
                .collect::<eyre::Result<_>>()?;
            let any_workflow = cycle.is_some()
                || parent.is_some()
                || estimate.is_some()
                || !assignee_refs.is_empty()
                || !blockers.is_empty();
            let workflow = if any_workflow {
                let estimate = match estimate {
                    Some(e) => Some(parse_estimate(&e)?),
                    None => None,
                };
                Some(task::model::WorkflowAttrs {
                    cycle,
                    parent,
                    estimate,
                    assignees: task::model::AgentRefList(assignee_refs),
                    blockers: task::model::UuidList(blockers),
                    ..Default::default()
                })
            } else {
                None
            };

            let new_task = task::TaskInfo {
                id: uuid::Uuid::nil(),
                path: path.unwrap_or_default(),
                title,
                status: status.unwrap_or_else(|| "open".into()),
                priority: priority.unwrap_or_else(|| "normal".into()),
                due: None,
                scheduled: None,
                tags: task::model::StringList(tags),
                contexts: task::model::StringList::default(),
                projects: task::model::StringList::default(),
                project_id: project,
                milestone_id: None,
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
                details: body,
                workflow,
            };
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
                if let Some(w) = &created.workflow {
                    print_workflow_block(w);
                }
            }
        }
        IssueCmd::Ready {
            cycle,
            project,
            as_agent,
            limit,
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

            // Index id → status so we can resolve blockers cheaply.
            let by_id: std::collections::HashMap<uuid::Uuid, &task::TaskInfo> =
                rows.iter().map(|t| (t.id, t)).collect();
            let agent_ref = as_agent
                .as_deref()
                .map(|s| parse_agent_ref(&format!("agent:{s}")))
                .transpose()?;

            let mut ready: Vec<&task::TaskInfo> = rows
                .iter()
                .filter(|t| {
                    // Status check — not done / cancelled.
                    let s = task::Status::from_str(&t.status);
                    !matches!(s, Some(task::Status::Done | task::Status::Cancelled))
                })
                .filter(|t| match cycle {
                    None => true,
                    Some(c) => t.workflow.as_ref().and_then(|x| x.cycle) == Some(c),
                })
                .filter(|t| match project {
                    None => true,
                    Some(p) => t.project_id == Some(p),
                })
                .filter(|t| match &agent_ref {
                    None => true,
                    Some(a) => {
                        // Available to this agent: either no
                        // assignees yet, or this agent is in the list.
                        let assignees = t.workflow.as_ref().map_or(&[][..], |w| &w.assignees.0[..]);
                        assignees.is_empty() || assignees.iter().any(|x| x == a)
                    }
                })
                .filter(|t| {
                    // No unresolved blockers — every blocker task
                    // must exist AND be in `done` / `cancelled`.
                    let blockers = t.workflow.as_ref().map_or(&[][..], |w| &w.blockers.0[..]);
                    blockers.iter().all(|bid| {
                        by_id.get(bid).is_some_and(|b| {
                            matches!(
                                task::Status::from_str(&b.status),
                                Some(task::Status::Done | task::Status::Cancelled)
                            )
                        })
                    })
                })
                .collect();

            // Priority desc, then title.
            ready.sort_by(|a, b| {
                let prio = |t: &task::TaskInfo| match task::Priority::from_str(&t.priority) {
                    Some(task::Priority::Critical) => 0,
                    Some(task::Priority::High) => 1,
                    Some(task::Priority::Normal) => 2,
                    Some(task::Priority::Low) => 3,
                    _ => 4,
                };
                prio(a).cmp(&prio(b)).then_with(|| a.title.cmp(&b.title))
            });
            ready.truncate(limit);

            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&ready).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            if ready.is_empty() {
                println!("(no ready issues)");
                return Ok(());
            }
            for t in &ready {
                println!(
                    "{}  {:<10}  {:<8}  {}  {}",
                    short_uuid(&t.id),
                    t.status,
                    t.priority,
                    workflow_summary(&t.workflow),
                    t.title,
                );
            }
        }
        IssueCmd::Start {
            id,
            as_agent,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&url).await?;
            let mut t = resolve_issue_id(&client, &id).await?;
            // Flip status; preserve completedDate semantics: if
            // re-opening from done, clear the date.
            t.status = "in-progress".into();
            t.completed_date = None;
            if let Some(name) = as_agent {
                let agent = parse_agent_ref(&format!("agent:{name}"))?;
                let w = t
                    .workflow
                    .get_or_insert_with(task::model::WorkflowAttrs::default);
                if !w.assignees.0.iter().any(|a| a == &agent) {
                    w.assignees.0.push(agent);
                }
            }
            let updated = client
                .update(t)
                .await
                .map_err(|e| eyre::eyre!("update: {e:?}"))?;
            println!(
                "started {}  [{}]  {}",
                short_uuid(&updated.id),
                updated.status,
                updated.title
            );
            if let Some(w) = &updated.workflow {
                print_workflow_block(w);
            }
        }
        IssueCmd::ImportBeads {
            from,
            dry_run,
            org,
            server,
        } => {
            // 1. Get the beads JSON.
            let raw = match from.as_str() {
                "bd" => {
                    let out = std::process::Command::new("bd")
                        .args(["list", "--json"])
                        .output()
                        .map_err(|e| eyre::eyre!("run `bd list --json`: {e}"))?;
                    if !out.status.success() {
                        return Err(eyre::eyre!(
                            "bd list --json failed: {}",
                            String::from_utf8_lossy(&out.stderr).trim()
                        ));
                    }
                    String::from_utf8_lossy(&out.stdout).to_string()
                }
                "-" => {
                    use std::io::Read as _;
                    let mut s = String::new();
                    std::io::stdin().read_to_string(&mut s)?;
                    s
                }
                path => {
                    std::fs::read_to_string(path).map_err(|e| eyre::eyre!("read {path}: {e}"))?
                }
            };

            // 2. Parse — beads `list --json` is either an array of
            //    issues or `{ "issues": [...] }`. Be lenient.
            let val: serde_json::Value =
                serde_json::from_str(&raw).map_err(|e| eyre::eyre!("parse beads json: {e}"))?;
            let items = val
                .get("issues")
                .and_then(|v| v.as_array())
                .or_else(|| val.as_array())
                .cloned()
                .ok_or_else(|| eyre::eyre!("beads json: expected an array or {{issues:[…]}}"))?;

            let map_status = |s: &str| match s.to_ascii_lowercase().as_str() {
                "closed" | "done" | "completed" => "done",
                "in_progress" | "in-progress" | "doing" => "in-progress",
                "blocked" | "waiting" => "waiting",
                _ => "open",
            };
            let map_priority = |p: &serde_json::Value| -> String {
                // beads priority is 0..4 (0=critical) or a string.
                if let Some(n) = p.as_u64() {
                    match n {
                        0 => "critical",
                        1 => "high",
                        2 => "normal",
                        3 => "low",
                        _ => "none",
                    }
                    .to_string()
                } else {
                    p.as_str().unwrap_or("normal").to_string()
                }
            };

            println!("{} beads issue(s) to import", items.len());
            if dry_run {
                for it in &items {
                    let title = it
                        .get("title")
                        .and_then(|v| v.as_str())
                        .unwrap_or("(untitled)");
                    let st = it.get("status").and_then(|v| v.as_str()).unwrap_or("open");
                    println!("  [{}] {title}", map_status(st));
                }
                println!("\n(dry run — nothing written)");
                return Ok(());
            }

            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&url).await?;
            let mut created = 0usize;
            for it in &items {
                let title = match it.get("title").and_then(|v| v.as_str()) {
                    Some(t) if !t.is_empty() => t.to_string(),
                    _ => continue,
                };
                let status =
                    map_status(it.get("status").and_then(|v| v.as_str()).unwrap_or("open"));
                let priority = it
                    .get("priority")
                    .map_or_else(|| "normal".to_string(), map_priority);
                let body = it
                    .get("description")
                    .and_then(|v| v.as_str())
                    .unwrap_or("")
                    .to_string();
                let new_task = task::TaskInfo {
                    id: uuid::Uuid::nil(),
                    path: String::new(),
                    title,
                    status: status.into(),
                    priority,
                    due: None,
                    scheduled: None,
                    tags: task::model::StringList(vec!["task".into(), "from-beads".into()]),
                    contexts: task::model::StringList::default(),
                    projects: task::model::StringList::default(),
                    project_id: None,
                    milestone_id: None,
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
                    details: body,
                    workflow: None,
                };
                client
                    .create(new_task)
                    .await
                    .map_err(|e| eyre::eyre!("create: {e:?}"))?;
                created += 1;
            }
            println!("imported {created} task(s) (tagged `from-beads`)");
            println!(
                "note: beads dependencies aren't mapped to blockers yet — \
                 the beads ids don't survive into TaskInfo uuids. Re-link by hand if needed."
            );
        }
        IssueCmd::Stats {
            project,
            org,
            server,
            json,
        } => {
            use std::collections::BTreeMap;

            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&url).await?;
            let rows = client
                .list()
                .await
                .map_err(|e| eyre::eyre!("list: {e:?}"))?;

            let mut filtered: Vec<&task::TaskInfo> = rows
                .iter()
                .filter(|t| match project {
                    None => true,
                    Some(p) => t.project_id == Some(p),
                })
                .collect();

            let total = filtered.len();
            let mut by_status: BTreeMap<String, usize> = BTreeMap::new();
            let mut by_priority: BTreeMap<String, usize> = BTreeMap::new();
            let mut by_project: BTreeMap<String, usize> = BTreeMap::new();
            let mut by_assignee: BTreeMap<String, usize> = BTreeMap::new();
            let mut blocked: usize = 0;
            let mut with_workflow: usize = 0;

            let by_id: std::collections::HashMap<uuid::Uuid, &task::TaskInfo> =
                rows.iter().map(|t| (t.id, t)).collect();

            for t in filtered.drain(..) {
                *by_status.entry(t.status.clone()).or_default() += 1;
                *by_priority.entry(t.priority.clone()).or_default() += 1;
                let p_label = t
                    .project_id
                    .map_or_else(|| "—".to_string(), |id| short_uuid(&id));
                *by_project.entry(p_label).or_default() += 1;
                if let Some(wf) = &t.workflow {
                    with_workflow += 1;
                    for a in &wf.assignees.0 {
                        *by_assignee.entry(a.short_label()).or_default() += 1;
                    }
                    // Blocked = has at least one blocker that
                    // is not done/cancelled (or that we can't
                    // resolve, which means it's still open).
                    let is_blocked = wf.blockers.0.iter().any(|bid| {
                        by_id.get(bid).is_none_or(|b| {
                            !matches!(
                                task::Status::from_str(&b.status),
                                Some(task::Status::Done | task::Status::Cancelled)
                            )
                        })
                    });
                    if is_blocked {
                        blocked += 1;
                    }
                }
            }

            if json {
                let payload = serde_json::json!({
                    "total": total,
                    "with_workflow": with_workflow,
                    "blocked": blocked,
                    "by_status": by_status,
                    "by_priority": by_priority,
                    "by_project": by_project,
                    "by_assignee": by_assignee,
                });
                println!(
                    "{}",
                    serde_json::to_string_pretty(&payload).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }

            println!("total:        {total}");
            println!("with workflow: {with_workflow}");
            println!("blocked:      {blocked}");
            println!();
            println!("by status:");
            for (k, v) in &by_status {
                println!("  {k:<14} {v}");
            }
            println!();
            println!("by priority:");
            for (k, v) in &by_priority {
                println!("  {k:<14} {v}");
            }
            if !by_project.is_empty() {
                println!();
                println!("by project:");
                for (k, v) in &by_project {
                    println!("  {k:<14} {v}");
                }
            }
            if !by_assignee.is_empty() {
                println!();
                println!("by assignee:");
                for (k, v) in &by_assignee {
                    println!("  {k:<28} {v}");
                }
            }
        }
        IssueCmd::Close {
            id,
            undo,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&url).await?;
            let mut t = resolve_issue_id(&client, &id).await?;
            if undo {
                t.status = "open".into();
                t.completed_date = None;
            } else {
                t.status = "done".into();
                t.completed_date = Some(chrono::Local::now().date_naive());
            }
            // Clear the active session pointer on close — work is
            // over; resume of this task starts a new session.
            if let Some(w) = t.workflow.as_mut() {
                w.session = None;
            }
            let updated = client
                .update(t)
                .await
                .map_err(|e| eyre::eyre!("update: {e:?}"))?;
            let verb = if undo { "reopened" } else { "closed" };
            println!(
                "{verb} {}  [{}]  {}",
                short_uuid(&updated.id),
                updated.status,
                updated.title
            );

            // Propagate to any linked forge issues. Best-effort:
            // a forge that's unreachable / unauthenticated logs
            // a warning but doesn't fail the local close.
            let new_state = if undo {
                git_proto::IssueState::Open
            } else {
                git_proto::IssueState::Closed
            };
            match propagate_state_to_forge(&slug, &updated.id, new_state).await {
                Ok(0) => {}
                Ok(n) => println!("  propagated to {n} linked forge issue(s)"),
                Err(e) => eprintln!("  warning: forge propagation failed: {e}"),
            }
        }
        IssueCmd::LinkForge {
            id,
            repo,
            number,
            base_url,
            kind,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&url).await?;
            let t = resolve_issue_id(&client, &id).await?;
            let (owner, repo_name) = parse_repo_slug(&repo)?;
            let base = forgejo_base_url(base_url)?;
            let link_kind = match kind.to_ascii_lowercase().as_str() {
                "issue" => git_config::LinkKind::Issue,
                "pull" | "pr" => git_config::LinkKind::Pull,
                _ => return Err(eyre::eyre!("--kind must be `issue` or `pull`")),
            };
            let store = forge_link_store(&slug)?;
            use git_config::BindingStore as _;
            store
                .add_issue_link(git_config::IssueLink {
                    task_id: t.id.to_string(),
                    repo: git_proto::RepoId {
                        forge: git_proto::Forge::Forgejo { base_url: base },
                        owner,
                        repo: repo_name,
                    },
                    number,
                    kind: link_kind,
                })
                .map_err(|e| eyre::eyre!("link store: {e}"))?;
            println!(
                "linked {} -> {}#{} ({:?})",
                short_uuid(&t.id),
                repo,
                number,
                link_kind,
            );
        }
        IssueCmd::Push {
            id,
            repo,
            github,
            base_url,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&url).await?;
            let t = resolve_issue_id(&client, &id).await?;
            let repo_id = build_repo_id(&repo, github, base_url)?;

            // Skip if we already have a link to this repo.
            let store = forge_link_store(&slug)?;
            use git_config::BindingStore as _;
            let existing = store
                .issues_for_task(&t.id.to_string())
                .map_err(|e| eyre::eyre!("link store: {e}"))?;
            if let Some(l) = existing.iter().find(|l| l.repo == repo_id) {
                println!(
                    "already linked: {} -> {}/{}#{} ({:?})",
                    short_uuid(&t.id),
                    l.repo.owner,
                    l.repo.repo,
                    l.number,
                    l.kind,
                );
                return Ok(());
            }

            // `IssueTracker` methods are sync but internally `block_on`
            // their HTTP call — we're inside tokio::main, so push them
            // onto the blocking pool to avoid the runtime-in-runtime
            // panic. `forge_backend_for` picks Forgejo/GitHub + token.
            let repo_c = repo_id.clone();
            let title = t.title.clone();
            let body = t.details.clone();
            let created = tokio::task::spawn_blocking(move || {
                let backend = forge_backend_for(&repo_c)?;
                backend
                    .create_issue(&repo_c, title, body)
                    .map_err(|e| eyre::eyre!("create_issue: {e:?}"))
            })
            .await
            .map_err(|e| eyre::eyre!("join: {e}"))??;
            store
                .add_issue_link(git_config::IssueLink {
                    task_id: t.id.to_string(),
                    repo: repo_id.clone(),
                    number: created.id.0,
                    kind: git_config::LinkKind::Issue,
                })
                .map_err(|e| eyre::eyre!("link store: {e}"))?;
            println!(
                "pushed {} -> {}#{}: {}",
                short_uuid(&t.id),
                repo,
                created.id.0,
                created.title,
            );
        }
        IssueCmd::Pull {
            repo,
            github,
            base_url,
            project,
            state,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&url).await?;
            let repo_id = build_repo_id(&repo, github, base_url)?;
            let filter_state = match state.to_ascii_lowercase().as_str() {
                "open" => Some(git_proto::IssueState::Open),
                "closed" => Some(git_proto::IssueState::Closed),
                "all" => None,
                _ => return Err(eyre::eyre!("--state must be `open`, `closed`, or `all`")),
            };

            let filter = git_proto::issues::IssueFilter {
                state: filter_state,
                ..Default::default()
            };
            let repo_c = repo_id.clone();
            let issues = tokio::task::spawn_blocking(move || {
                let backend = forge_backend_for(&repo_c)?;
                backend
                    .list_issues(&repo_c, filter)
                    .map_err(|e| eyre::eyre!("list_issues: {e:?}"))
            })
            .await
            .map_err(|e| eyre::eyre!("join: {e}"))??;

            let store = forge_link_store(&slug)?;
            use git_config::BindingStore as _;
            let mut created_n = 0usize;
            let mut skipped_n = 0usize;

            for ext in issues {
                let already = store
                    .tasks_for_issue(&repo_id, ext.id.0)
                    .map_err(|e| eyre::eyre!("link store: {e}"))?;
                if !already.is_empty() {
                    skipped_n += 1;
                    continue;
                }
                // Translate forge issue → TaskInfo (status derived
                // from the forge state inside build_pulled_task).
                let mut new_task = build_pulled_task(&ext, None::<task::model::WorkflowAttrs>);
                new_task.project_id = project;
                let created = client
                    .create(new_task)
                    .await
                    .map_err(|e| eyre::eyre!("create: {e:?}"))?;
                store
                    .add_issue_link(git_config::IssueLink {
                        task_id: created.id.to_string(),
                        repo: repo_id.clone(),
                        number: ext.id.0,
                        kind: git_config::LinkKind::Issue,
                    })
                    .map_err(|e| eyre::eyre!("link store: {e}"))?;
                created_n += 1;
                println!(
                    "pulled {}#{}: {}  -> {}",
                    repo,
                    ext.id.0,
                    ext.title,
                    short_uuid(&created.id),
                );
            }
            println!("\n{created_n} new, {skipped_n} already linked");
        }
        IssueCmd::Sync {
            repo,
            github,
            base_url,
            project,
            no_pull,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&url).await?;
            let repo_id = build_repo_id(&repo, github, base_url)?;
            let store = forge_link_store(&slug)?;
            let (reconciled, pulled) =
                sync_repo(&client, &store, &repo_id, project, no_pull).await?;
            println!("\nsync: {reconciled} reconciled, {pulled} pulled");
        }
        IssueCmd::SyncAll {
            project,
            no_pull,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&url).await?;
            let store = forge_link_store(&slug)?;
            let repos = store
                .distinct_repos()
                .map_err(|e| eyre::eyre!("link store: {e}"))?;
            if repos.is_empty() {
                println!("(no linked repos — run `task issue push` or `task setup forge` first)");
                return Ok(());
            }
            let mut total_r = 0usize;
            let mut total_p = 0usize;
            for repo_id in &repos {
                let label = format!("{}/{}", repo_id.owner, repo_id.repo);
                println!("=== {label} ===");
                match sync_repo(&client, &store, repo_id, project, no_pull).await {
                    Ok((r, p)) => {
                        total_r += r;
                        total_p += p;
                    }
                    // One unreachable / unauthorized repo shouldn't
                    // abort the whole sweep.
                    Err(e) => eprintln!("  skipped {label}: {e}"),
                }
            }
            println!(
                "\nsync-all: {} repo(s), {total_r} reconciled, {total_p} pulled",
                repos.len()
            );
        }
        IssueCmd::PrList {
            repo,
            github,
            base_url,
            json,
        } => {
            let repo_id = build_repo_id(&repo, github, base_url)?;
            let repo_c = repo_id.clone();
            let prs = tokio::task::spawn_blocking(move || {
                let backend = forge_backend_for(&repo_c)?;
                backend
                    .list_pull_requests(&repo_c)
                    .map_err(|e| eyre::eyre!("list_pull_requests: {e:?}"))
            })
            .await
            .map_err(|e| eyre::eyre!("join: {e}"))??;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&prs).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            if prs.is_empty() {
                println!("(no open PRs)");
                return Ok(());
            }
            for pr in &prs {
                println!(
                    "#{:<5} [{:?}] {} ({} <- {})",
                    pr.id.0, pr.state, pr.title, pr.base, pr.head
                );
            }
        }
        IssueCmd::PrCreate {
            repo,
            github,
            base_url,
            title,
            head,
            base,
            body,
            draft,
            closes,
            close_task,
            org,
            server,
        } => {
            let repo_id = build_repo_id(&repo, github, base_url)?;
            let mut body = resolve_body(body)?;

            // Resolve the issue number this PR should close:
            // explicit --closes wins; else look up the linked
            // forge issue for --close-task. Capture the task id
            // so we can record a PR link afterward.
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let store = forge_link_store(&slug)?;
            use git_config::BindingStore as _;
            let mut closes_number = closes;
            let mut linked_task: Option<uuid::Uuid> = None;
            if let Some(ref tid) = close_task {
                let client = connect_task_client(&url).await?;
                let t = resolve_issue_id(&client, tid).await?;
                linked_task = Some(t.id);
                let links = store
                    .issues_for_task(&t.id.to_string())
                    .map_err(|e| eyre::eyre!("link store: {e}"))?;
                match links
                    .iter()
                    .find(|l| l.repo == repo_id && l.kind == git_config::LinkKind::Issue)
                {
                    Some(l) => closes_number = Some(l.number),
                    None => {
                        return Err(eyre::eyre!(
                            "task {} has no linked issue on {}/{} — push it first (task issue push)",
                            short_uuid(&t.id),
                            repo_id.owner,
                            repo_id.repo
                        ));
                    }
                }
            }

            // Inject the forge's close-on-merge keyword if not
            // already present.
            if let Some(n) = closes_number {
                let kw = format!("Closes #{n}");
                if !body.contains(&kw) {
                    if body.is_empty() {
                        body = kw.clone();
                    } else {
                        body = format!("{body}\n\n{kw}");
                    }
                }
            }

            let repo_c = repo_id.clone();
            let pr = tokio::task::spawn_blocking(move || {
                let backend = forge_backend_for(&repo_c)?;
                let new = git_proto::reviews::NewPullRequest {
                    title,
                    body,
                    base,
                    head,
                    draft,
                };
                backend
                    .create_pull_request(&repo_c, new)
                    .map_err(|e| eyre::eyre!("create_pull_request: {e:?}"))
            })
            .await
            .map_err(|e| eyre::eyre!("join: {e}"))??;

            // Record a PR-kind link on the task so pr-merge/sync
            // can finish the loop.
            if let Some(tid) = linked_task {
                store
                    .add_issue_link(git_config::IssueLink {
                        task_id: tid.to_string(),
                        repo: repo_id.clone(),
                        number: pr.id.0,
                        kind: git_config::LinkKind::Pull,
                    })
                    .map_err(|e| eyre::eyre!("link store: {e}"))?;
            }

            println!(
                "opened PR #{}: {} ({} <- {})",
                pr.id.0, pr.title, pr.base, pr.head
            );
            if let Some(n) = closes_number {
                println!("  will close #{n} on merge (Closes #{n} in body)");
            }
        }
        IssueCmd::PrMerge {
            repo,
            github,
            base_url,
            number,
            method,
            close_task,
            org,
            server,
        } => {
            let repo_id = build_repo_id(&repo, github, base_url)?;
            let merge_method = match method.to_ascii_lowercase().as_str() {
                "merge" => git_proto::reviews::MergeMethod::Merge,
                "squash" => git_proto::reviews::MergeMethod::Squash,
                "rebase" => git_proto::reviews::MergeMethod::Rebase,
                _ => return Err(eyre::eyre!("--method must be merge, squash, or rebase")),
            };
            let repo_c = repo_id.clone();
            let sha = tokio::task::spawn_blocking(move || {
                let backend = forge_backend_for(&repo_c)?;
                backend
                    .merge_pull_request(&repo_c, git_proto::PullRequestId(number), merge_method)
                    .map_err(|e| eyre::eyre!("merge_pull_request: {e:?}"))
            })
            .await
            .map_err(|e| eyre::eyre!("join: {e}"))??;
            match sha {
                Some(s) => println!("merged PR #{number} ({s})"),
                None => println!("merged PR #{number}"),
            }

            // The `task code merge` chain: close the linked task,
            // which propagates the close to its own forge issue.
            if let Some(tid) = close_task {
                let slug = resolve_active_org(org)?;
                let url = resolve_org_vox_url(server, &slug);
                let client = connect_task_client(&url).await?;
                let mut t = resolve_issue_id(&client, &tid).await?;
                t.status = "done".into();
                t.completed_date = Some(chrono::Local::now().date_naive());
                if let Some(w) = t.workflow.as_mut() {
                    w.session = None;
                }
                let updated = client
                    .update(t)
                    .await
                    .map_err(|e| eyre::eyre!("update: {e:?}"))?;
                println!(
                    "  closed task {} ({})",
                    short_uuid(&updated.id),
                    updated.title
                );
                match propagate_state_to_forge(&slug, &updated.id, git_proto::IssueState::Closed)
                    .await
                {
                    Ok(0) => {}
                    Ok(n) => println!("  propagated close to {n} linked forge issue(s)"),
                    Err(e) => eprintln!("  warning: forge propagation failed: {e}"),
                }
            }
        }
    }
    Ok(())
}

/// Reconcile one repo's linked tasks against the forge, then
/// pull new issues. Returns `(reconciled, pulled)`. Shared by
/// `task issue sync` and `sync-all`.
async fn sync_repo(
    client: &task::TaskServiceClient,
    store: &git_config::FileStore,
    repo_id: &git_proto::RepoId,
    project: Option<uuid::Uuid>,
    no_pull: bool,
) -> eyre::Result<(usize, usize)> {
    use git_config::BindingStore as _;

    // 1. Reconcile already-linked issues (forge wins for open/closed).
    let local = client
        .list()
        .await
        .map_err(|e| eyre::eyre!("list: {e:?}"))?;
    let mut reconciled = 0usize;
    for t in &local {
        let links = store
            .issues_for_task(&t.id.to_string())
            .map_err(|e| eyre::eyre!("link store: {e}"))?;
        let Some(link) = links.iter().find(|l| &l.repo == repo_id) else {
            continue;
        };
        let number = link.number;
        let repo_c = repo_id.clone();
        let ext = tokio::task::spawn_blocking(move || {
            let backend = forge_backend_for(&repo_c)?;
            backend
                .get_issue(&repo_c, git_proto::IssueId(number))
                .map_err(|e| eyre::eyre!("get_issue #{number}: {e:?}"))
        })
        .await
        .map_err(|e| eyre::eyre!("join: {e}"))??;

        let forge_done = matches!(ext.state, git_proto::IssueState::Closed);
        let local_done = t.status == "done";
        if forge_done != local_done {
            let mut t2 = t.clone();
            if forge_done {
                t2.status = "done".into();
                t2.completed_date = Some(chrono::Local::now().date_naive());
                if let Some(w) = t2.workflow.as_mut() {
                    w.session = None;
                }
            } else {
                t2.status = "open".into();
                t2.completed_date = None;
            }
            client
                .update(t2)
                .await
                .map_err(|e| eyre::eyre!("update: {e:?}"))?;
            reconciled += 1;
            let s = if forge_done { "done" } else { "open" };
            println!("  reconciled {} #{number} -> {s}", short_uuid(&t.id));
        }
    }

    // 2. Pull new forge issues (unless suppressed).
    let mut pulled = 0usize;
    if !no_pull {
        let repo_c = repo_id.clone();
        let issues = tokio::task::spawn_blocking(move || {
            let backend = forge_backend_for(&repo_c)?;
            let filter = git_proto::issues::IssueFilter {
                state: Some(git_proto::IssueState::Open),
                ..Default::default()
            };
            backend
                .list_issues(&repo_c, filter)
                .map_err(|e| eyre::eyre!("list_issues: {e:?}"))
        })
        .await
        .map_err(|e| eyre::eyre!("join: {e}"))??;
        for ext in issues {
            let already = store
                .tasks_for_issue(repo_id, ext.id.0)
                .map_err(|e| eyre::eyre!("link store: {e}"))?;
            if !already.is_empty() {
                continue;
            }
            let mut new_task = build_pulled_task(&ext, None::<task::model::WorkflowAttrs>);
            new_task.project_id = project;
            let created = client
                .create(new_task)
                .await
                .map_err(|e| eyre::eyre!("create: {e:?}"))?;
            store
                .add_issue_link(git_config::IssueLink {
                    task_id: created.id.to_string(),
                    repo: repo_id.clone(),
                    number: ext.id.0,
                    kind: git_config::LinkKind::Issue,
                })
                .map_err(|e| eyre::eyre!("link store: {e}"))?;
            pulled += 1;
            println!(
                "  pulled {} #{}: {}",
                short_uuid(&created.id),
                ext.id.0,
                ext.title
            );
        }
    }
    Ok((reconciled, pulled))
}

/// Build a `TaskInfo` from a forge issue (used by pull + sync).
fn build_pulled_task(
    ext: &git_proto::issues::Issue,
    workflow: Option<task::model::WorkflowAttrs>,
) -> task::TaskInfo {
    let status = match ext.state {
        git_proto::IssueState::Open => "open",
        git_proto::IssueState::Closed => "done",
    };
    task::TaskInfo {
        id: uuid::Uuid::nil(),
        path: String::new(),
        title: ext.title.clone(),
        status: status.into(),
        priority: "normal".into(),
        due: None,
        scheduled: None,
        tags: task::model::StringList(vec!["task".into(), "from-forge".into()]),
        contexts: task::model::StringList::default(),
        projects: task::model::StringList::default(),
        project_id: None,
        milestone_id: None,
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
        details: ext.body.clone(),
        workflow,
    }
}

/// Build a `RepoId` for a forge from an `owner/repo` slug.
/// `github=true` → GitHub; else Forgejo with the resolved base
/// URL.
fn build_repo_id(
    repo_slug: &str,
    github: bool,
    base_url: Option<String>,
) -> eyre::Result<git_proto::RepoId> {
    let (owner, repo) = parse_repo_slug(repo_slug)?;
    let forge = if github {
        git_proto::Forge::Github
    } else {
        git_proto::Forge::Forgejo {
            base_url: forgejo_base_url(base_url)?,
        }
    };
    Ok(git_proto::RepoId { forge, owner, repo })
}

/// Parse `owner/repo` into a tuple.
fn parse_repo_slug(s: &str) -> eyre::Result<(String, String)> {
    let (owner, repo) = s
        .split_once('/')
        .ok_or_else(|| eyre::eyre!("expected `owner/repo`, got `{s}`"))?;
    if owner.is_empty() || repo.is_empty() {
        return Err(eyre::eyre!("owner/repo: empty part in `{s}`"));
    }
    Ok((owner.to_string(), repo.to_string()))
}

/// Resolve the Forgejo base URL: flag > env `TASK_FORGEJO_BASE_URL`
/// > error. Trims trailing slash.
fn forgejo_base_url(flag: Option<String>) -> eyre::Result<String> {
    let raw = flag
        .or_else(|| std::env::var("TASK_FORGEJO_BASE_URL").ok())
        .ok_or_else(|| {
            eyre::eyre!("no Forgejo base URL — pass --base-url or set TASK_FORGEJO_BASE_URL")
        })?;
    Ok(raw.trim_end_matches('/').to_string())
}

/// A constructed forge backend, picked by the repo's `Forge`
/// variant. `IssueTracker` carries an `async fn subscribe`, so
/// the trait isn't object-safe — enum dispatch instead of
/// `Box<dyn>`. Each method forwards to the matching backend's
/// sync `IssueTracker` impl.
enum ForgeBackend {
    Forgejo(git_forgejo::Backend),
    Github(git_github::Backend),
}

impl ForgeBackend {
    fn create_issue(
        &self,
        repo: &git_proto::RepoId,
        title: String,
        body: String,
    ) -> Result<git_proto::issues::Issue, git_proto::GitError> {
        use git_proto::issues::IssueTracker;
        match self {
            Self::Forgejo(b) => b.create_issue(repo, title, body),
            Self::Github(b) => b.create_issue(repo, title, body),
        }
    }

    fn update_issue(
        &self,
        repo: &git_proto::RepoId,
        issue: git_proto::IssueId,
        update: git_proto::issues::IssueUpdate,
    ) -> Result<git_proto::issues::Issue, git_proto::GitError> {
        use git_proto::issues::IssueTracker;
        match self {
            Self::Forgejo(b) => b.update_issue(repo, issue, update),
            Self::Github(b) => b.update_issue(repo, issue, update),
        }
    }

    fn list_issues(
        &self,
        repo: &git_proto::RepoId,
        filter: git_proto::issues::IssueFilter,
    ) -> Result<Vec<git_proto::issues::Issue>, git_proto::GitError> {
        use git_proto::issues::IssueTracker;
        match self {
            Self::Forgejo(b) => b.list_issues(repo, filter),
            Self::Github(b) => b.list_issues(repo, filter),
        }
    }

    fn get_issue(
        &self,
        repo: &git_proto::RepoId,
        issue: git_proto::IssueId,
    ) -> Result<git_proto::issues::Issue, git_proto::GitError> {
        use git_proto::issues::IssueTracker;
        match self {
            Self::Forgejo(b) => b.get_issue(repo, issue),
            Self::Github(b) => b.get_issue(repo, issue),
        }
    }

    fn list_pull_requests(
        &self,
        repo: &git_proto::RepoId,
    ) -> Result<Vec<git_proto::PullRequest>, git_proto::GitError> {
        use git_proto::reviews::ReviewSurface;
        match self {
            Self::Forgejo(b) => b.list_pull_requests(repo),
            Self::Github(b) => b.list_pull_requests(repo),
        }
    }

    fn create_pull_request(
        &self,
        repo: &git_proto::RepoId,
        new: git_proto::reviews::NewPullRequest,
    ) -> Result<git_proto::PullRequest, git_proto::GitError> {
        use git_proto::reviews::ReviewSurface;
        match self {
            Self::Forgejo(b) => b.create_pull_request(repo, new),
            Self::Github(b) => b.create_pull_request(repo, new),
        }
    }

    fn merge_pull_request(
        &self,
        repo: &git_proto::RepoId,
        pr: git_proto::PullRequestId,
        method: git_proto::reviews::MergeMethod,
    ) -> Result<Option<String>, git_proto::GitError> {
        use git_proto::reviews::ReviewSurface;
        match self {
            Self::Forgejo(b) => b.merge_pull_request(repo, pr, method),
            Self::Github(b) => b.merge_pull_request(repo, pr, method),
        }
    }
}

/// Build the right backend for a repo, reading the matching
/// token. Forgejo → `forgejo_token()`; GitHub → `github_token()`.
fn forge_backend_for(repo: &git_proto::RepoId) -> eyre::Result<ForgeBackend> {
    match &repo.forge {
        git_proto::Forge::Forgejo { base_url } => {
            let tok = forgejo_token()?;
            let base = if base_url.is_empty() {
                forgejo_base_url(None)?
            } else {
                base_url.clone()
            };
            let b = git_forgejo::Backend::from_token(&base, &tok)
                .map_err(|e| eyre::eyre!("forgejo backend: {e:?}"))?;
            Ok(ForgeBackend::Forgejo(b))
        }
        git_proto::Forge::Github => {
            let tok = github_token()?;
            let b = git_github::Backend::from_token(&tok)
                .map_err(|e| eyre::eyre!("github backend: {e:?}"))?;
            Ok(ForgeBackend::Github(b))
        }
    }
}

/// Resolve a GitHub personal-access token: env `TASK_GITHUB_TOKEN`
/// then `GITHUB_TOKEN`, then `~/.config/task/github-token`, then
/// error.
fn github_token() -> eyre::Result<String> {
    for var in ["TASK_GITHUB_TOKEN", "GITHUB_TOKEN"] {
        if let Ok(v) = std::env::var(var) {
            if !v.is_empty() {
                return Ok(v);
            }
        }
    }
    let home = std::env::var_os("HOME")
        .ok_or_else(|| eyre::eyre!("HOME not set; can't resolve fallback token path"))?;
    let p = std::path::Path::new(&home)
        .join(".config")
        .join("task")
        .join("github-token");
    if p.exists() {
        let s =
            std::fs::read_to_string(&p).map_err(|e| eyre::eyre!("read {}: {e}", p.display()))?;
        let t = s.trim();
        if !t.is_empty() {
            return Ok(t.to_string());
        }
    }
    Err(eyre::eyre!(
        "no GitHub token — set TASK_GITHUB_TOKEN (or GITHUB_TOKEN) or write one to ~/.config/task/github-token"
    ))
}

/// Resolve a Forgejo personal-access token: env `TASK_FORGEJO_TOKEN`
/// then `FORGEJO_TOKEN`, then `~/.config/task/forgejo-token`, then
/// error.
fn forgejo_token() -> eyre::Result<String> {
    if let Ok(v) = std::env::var("TASK_FORGEJO_TOKEN") {
        if !v.is_empty() {
            return Ok(v);
        }
    }
    if let Ok(v) = std::env::var("FORGEJO_TOKEN") {
        if !v.is_empty() {
            return Ok(v);
        }
    }
    let home = std::env::var_os("HOME")
        .ok_or_else(|| eyre::eyre!("HOME not set; can't resolve fallback token path"))?;
    let p = std::path::Path::new(&home)
        .join(".config")
        .join("task")
        .join("forgejo-token");
    if p.exists() {
        let s =
            std::fs::read_to_string(&p).map_err(|e| eyre::eyre!("read {}: {e}", p.display()))?;
        let t = s.trim();
        if !t.is_empty() {
            return Ok(t.to_string());
        }
    }
    Err(eyre::eyre!(
        "no Forgejo token — set TASK_FORGEJO_TOKEN (or FORGEJO_TOKEN) or write one to ~/.config/task/forgejo-token"
    ))
}

/// Push an `Open`/`Closed` state change to every forge issue
/// linked to `task_id`. Returns the number of links touched.
///
/// Best-effort per-link: a forge that's unreachable or that we
/// have no token for is logged + skipped, not propagated as an
/// error. The local close already landed; one flaky forge
/// shouldn't break it.
async fn propagate_state_to_forge(
    org_slug: &str,
    task_id: &uuid::Uuid,
    new_state: git_proto::IssueState,
) -> eyre::Result<usize> {
    use git_config::BindingStore as _;
    let store = forge_link_store(org_slug)?;
    let links = store
        .issues_for_task(&task_id.to_string())
        .map_err(|e| eyre::eyre!("link store: {e}"))?;
    if links.is_empty() {
        return Ok(0);
    }
    let mut touched = 0usize;
    for link in links {
        // Only issue links get state-propagated. PR links exist
        // for traceability, but "closing" a PR is a different
        // operation than closing an issue (and a merged PR is
        // already closed) — never route a PR number through
        // update_issue.
        if link.kind != git_config::LinkKind::Issue {
            continue;
        }
        let repo_c = link.repo.clone();
        let number = link.number;
        // Best-effort: a missing token for this forge family is a
        // skip-with-warning, not a hard error.
        let result = tokio::task::spawn_blocking(move || {
            let backend = forge_backend_for(&repo_c)?;
            let update = git_proto::issues::IssueUpdate {
                state: Some(new_state),
                ..Default::default()
            };
            backend
                .update_issue(&repo_c, git_proto::IssueId(number), update)
                .map_err(|e| eyre::eyre!("update_issue: {e:?}"))
        })
        .await
        .map_err(|e| eyre::eyre!("join: {e}"))?;
        match result {
            Ok(_) => touched += 1,
            Err(e) => eprintln!(
                "  skipping {}/{}#{}: {e}",
                link.repo.owner, link.repo.repo, link.number
            ),
        }
    }
    Ok(touched)
}

/// Open the per-org issue-link `FileStore` at
/// `~/.task/orgs/<slug>/issue-links.json`.
fn forge_link_store(org_slug: &str) -> eyre::Result<git_config::FileStore> {
    let home = std::env::var_os("HOME")
        .ok_or_else(|| eyre::eyre!("HOME not set; can't resolve issue-link store path"))?;
    let p = std::path::Path::new(&home)
        .join(".task")
        .join("orgs")
        .join(org_slug)
        .join("issue-links.json");
    git_config::FileStore::open(p).map_err(|e| eyre::eyre!("open link store: {e}"))
}

// ── git helpers for `task code` ──────────────────────────────

/// Run `git <args>` in the cwd, returning trimmed stdout.
fn git(args: &[&str]) -> eyre::Result<String> {
    let out = std::process::Command::new("git")
        .args(args)
        .output()
        .map_err(|e| eyre::eyre!("git {}: {e}", args.join(" ")))?;
    if !out.status.success() {
        return Err(eyre::eyre!(
            "git {} failed: {}",
            args.join(" "),
            String::from_utf8_lossy(&out.stderr).trim()
        ));
    }
    Ok(String::from_utf8_lossy(&out.stdout).trim().to_string())
}

fn current_branch() -> eyre::Result<String> {
    git(&["rev-parse", "--abbrev-ref", "HEAD"])
}

/// Parse the 8-char task prefix out of a `task/<short>-<slug>`
/// branch name.
fn task_short_from_branch(branch: &str) -> Option<String> {
    let after = branch.split_once('/').map_or(branch, |(_, r)| r);
    let short = after.split('-').next()?;
    if short.len() == 8 && short.chars().all(|c| c.is_ascii_hexdigit()) {
        Some(short.to_string())
    } else {
        None
    }
}

/// Derive a `RepoId` from `git remote get-url origin`. Handles
/// both SSH (`forgejo@host:owner/repo.git`,
/// `git@github.com:owner/repo.git`) and HTTPS forms.
fn repo_id_from_git_remote() -> eyre::Result<git_proto::RepoId> {
    let url = git(&["remote", "get-url", "origin"])?;
    let (host, owner, repo) =
        parse_remote_url(&url).ok_or_else(|| eyre::eyre!("can't parse origin remote `{url}`"))?;
    let forge = if host.contains("github.com") {
        git_proto::Forge::Github
    } else {
        git_proto::Forge::Forgejo {
            base_url: format!("https://{host}"),
        }
    };
    Ok(git_proto::RepoId { forge, owner, repo })
}

/// `(host, owner, repo)` from a git remote URL.
fn parse_remote_url(url: &str) -> Option<(String, String, String)> {
    let url = url.trim();
    // scp-like: user@host:owner/repo(.git)
    let rest = if let Some(idx) = url.find('@') {
        let after_at = &url[idx + 1..];
        if let Some((host, path)) = after_at.split_once(':') {
            let path = path.trim_end_matches(".git");
            let (owner, repo) = path.split_once('/')?;
            return Some((host.to_string(), owner.to_string(), repo.to_string()));
        }
        after_at.to_string()
    } else {
        url.to_string()
    };
    // https://host/owner/repo(.git)
    let rest = rest
        .strip_prefix("https://")
        .or_else(|| rest.strip_prefix("http://"))
        .unwrap_or(&rest);
    let (host, path) = rest.split_once('/')?;
    let path = path.trim_end_matches(".git");
    let (owner, repo) = path.split_once('/')?;
    Some((host.to_string(), owner.to_string(), repo.to_string()))
}

async fn run_code(cmd: CodeCmd) -> eyre::Result<()> {
    use git_config::BindingStore as _;
    match cmd {
        CodeCmd::Start {
            id,
            as_agent,
            prefix,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&url).await?;
            let mut t = resolve_issue_id(&client, &id).await?;
            let short = &t.id.simple().to_string()[..8];
            let title_slug: String = t
                .title
                .chars()
                .map(|c| {
                    if c.is_ascii_alphanumeric() {
                        c.to_ascii_lowercase()
                    } else {
                        '-'
                    }
                })
                .collect::<String>()
                .split('-')
                .filter(|s| !s.is_empty())
                .take(6)
                .collect::<Vec<_>>()
                .join("-");
            let branch = format!("{prefix}/{short}-{title_slug}");
            git(&["switch", "-c", &branch])?;

            t.status = "in-progress".into();
            t.completed_date = None;
            if let Some(name) = as_agent {
                let agent = parse_agent_ref(&format!("agent:{name}"))?;
                let w = t
                    .workflow
                    .get_or_insert_with(task::model::WorkflowAttrs::default);
                if !w.assignees.0.iter().any(|a| a == &agent) {
                    w.assignees.0.push(agent);
                }
            }
            client
                .update(t)
                .await
                .map_err(|e| eyre::eyre!("update: {e:?}"))?;
            println!("started {short} on branch {branch}");
        }
        CodeCmd::Commit {
            message,
            as_agent,
            all,
        } => {
            let branch = current_branch()?;
            let short = task_short_from_branch(&branch);
            let mut trailers = String::new();
            if let Some(s) = &short {
                trailers.push_str(&format!("\n\nTask-Id: {s}"));
            }
            let agent = as_agent.unwrap_or_else(|| "claude".to_string());
            trailers.push_str(&format!("\nTask-Agent: {agent}"));
            trailers.push_str("\nCo-Authored-By: Claude <noreply@anthropic.com>");
            let full = format!("{message}{trailers}");
            if all {
                git(&["add", "-A"])?;
            }
            git(&["commit", "-m", &full])?;
            let sha = git(&["rev-parse", "--short", "HEAD"])?;
            println!("committed {sha} on {branch}");
            if short.is_none() {
                eprintln!("  note: branch isn't a `task/<id>-…` branch — no Task-Id trailer");
            }
        }
        CodeCmd::Push {
            github,
            base_url,
            base,
            draft,
            org,
            server,
        } => {
            let branch = current_branch()?;
            let short = task_short_from_branch(&branch).ok_or_else(|| {
                eyre::eyre!(
                    "current branch `{branch}` isn't a `task/<id>-…` branch; can't link a PR"
                )
            })?;
            let slug = resolve_active_org(org)?;
            let vox = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&vox).await?;
            let t = resolve_issue_id(&client, &short).await?;

            // Forge repo inferred from the git remote (works on
            // third-party repos). --github / --base-url are
            // accepted for parity but the remote is authoritative.
            let _ = (github, &base_url);
            let repo_id = repo_id_from_git_remote()?;
            let repo_slug = format!("{}/{}", repo_id.owner, repo_id.repo);

            // Push the branch.
            git(&["push", "-u", "origin", &branch])?;
            println!("pushed {branch} → {repo_slug}");

            // Find the linked forge issue → inject Closes #N.
            let store = forge_link_store(&slug)?;
            let links = store
                .issues_for_task(&t.id.to_string())
                .map_err(|e| eyre::eyre!("link store: {e}"))?;
            let closes = links
                .iter()
                .find(|l| l.repo == repo_id && l.kind == git_config::LinkKind::Issue)
                .map(|l| l.number);
            let mut body = format!("Work for task {short}.");
            if let Some(n) = closes {
                body.push_str(&format!("\n\nCloses #{n}"));
            }

            let repo_c = repo_id.clone();
            let title = t.title.clone();
            let head = branch.clone();
            let pr = tokio::task::spawn_blocking(move || {
                let backend = forge_backend_for(&repo_c)?;
                backend
                    .create_pull_request(
                        &repo_c,
                        git_proto::reviews::NewPullRequest {
                            title,
                            body,
                            base,
                            head,
                            draft,
                        },
                    )
                    .map_err(|e| eyre::eyre!("create_pull_request: {e:?}"))
            })
            .await
            .map_err(|e| eyre::eyre!("join: {e}"))??;

            // Record a PR link on the task.
            store
                .add_issue_link(git_config::IssueLink {
                    task_id: t.id.to_string(),
                    repo: repo_id.clone(),
                    number: pr.id.0,
                    kind: git_config::LinkKind::Pull,
                })
                .map_err(|e| eyre::eyre!("link store: {e}"))?;
            println!("opened PR #{} ({repo_slug})", pr.id.0);
            if let Some(n) = closes {
                println!("  closes #{n} on merge");
            } else {
                eprintln!("  note: task has no linked forge issue — PR won't auto-close one");
            }
        }
        CodeCmd::Status { org, server } => {
            let branch = current_branch()?;
            println!("branch:  {branch}");
            let Some(short) = task_short_from_branch(&branch) else {
                println!("task:    (branch isn't a task/<id>-… branch)");
                return Ok(());
            };
            let slug = resolve_active_org(org)?;
            let vox = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&vox).await?;
            let t = resolve_issue_id(&client, &short).await?;
            println!("task:    {} [{}]  {}", short, t.status, t.title);
            let store = forge_link_store(&slug)?;
            let links = store
                .issues_for_task(&t.id.to_string())
                .map_err(|e| eyre::eyre!("link store: {e}"))?;
            for l in links {
                let kind = match l.kind {
                    git_config::LinkKind::Issue => "issue",
                    git_config::LinkKind::Pull => "pr",
                };
                println!("  {kind:<5} {}/{}#{}", l.repo.owner, l.repo.repo, l.number);
            }
        }
        CodeCmd::Park {
            summary,
            reason,
            open,
            as_agent,
            org,
            server,
        } => {
            let branch = current_branch()?;
            let short = task_short_from_branch(&branch).ok_or_else(|| {
                eyre::eyre!("current branch `{branch}` isn't a task/<id>-… branch")
            })?;
            let slug = resolve_active_org(org)?;
            let vox = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&vox).await?;
            let mut t = resolve_issue_id(&client, &short).await?;
            let agent = parse_agent_ref(&format!(
                "agent:{}",
                as_agent.as_deref().unwrap_or("claude")
            ))?;

            // Record the handoff (one active per task; supersede prior open ones).
            let mut hs = load_handoffs(&slug)?;
            for h in hs.iter_mut().filter(|h| {
                h.session_id == t.id && h.status == workflows_proto::HandoffStatus::Open
            }) {
                h.status = workflows_proto::HandoffStatus::Cancelled;
                h.resolved_at = Some(chrono::Utc::now());
            }
            let mut handoff = workflows_proto::Handoff::post(
                t.id, // session_id repurposed as the task id (no separate WorkSession yet)
                agent.clone(),
                workflows_proto::HandoffReason::Custom {
                    tag: reason.clone(),
                },
                summary,
            );
            handoff.open_questions = open.unwrap_or_default();
            hs.push(handoff);
            save_handoffs(&slug, &hs)?;

            // Release the claim + return to the ready queue.
            if let Some(w) = t.workflow.as_mut() {
                w.assignees = task::model::AgentRefList(vec![]);
            }
            t.status = "open".into();
            client
                .update(t)
                .await
                .map_err(|e| eyre::eyre!("update: {e:?}"))?;
            println!("parked {short} (reason: {reason}) — claim released, branch {branch} kept");
            println!("another agent: `task code resume {short} --as-agent <name>`");
        }
        CodeCmd::Resume {
            id,
            as_agent,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let vox = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&vox).await?;
            let target = match id {
                Some(i) => i,
                None => task_short_from_branch(&current_branch()?).ok_or_else(|| {
                    eyre::eyre!("no task id given and current branch isn't a task branch")
                })?,
            };
            let t = resolve_issue_id(&client, &target).await?;
            let agent = parse_agent_ref(&format!("agent:{as_agent}"))?;
            // Atomically claim it.
            if let ClaimOutcome::Lost(holder) = try_claim(&client, &t.id, &agent, false).await? {
                return Err(eyre::eyre!(
                    "{} is held by {holder} — can't resume",
                    short_uuid(&t.id)
                ));
            }
            // Flip to in-progress.
            let mut t2 = client
                .get(t.id)
                .await
                .map_err(|e| eyre::eyre!("get: {e:?}"))?;
            t2.status = "in-progress".into();
            client
                .update(t2)
                .await
                .map_err(|e| eyre::eyre!("update: {e:?}"))?;

            // Surface the latest open handoff context + mark it claimed.
            let mut hs = load_handoffs(&slug)?;
            let short = &t.id.simple().to_string()[..8];
            println!("resumed {short}: {}\n", t.title);
            if let Some(h) = hs
                .iter_mut()
                .filter(|h| {
                    h.session_id == t.id && h.status == workflows_proto::HandoffStatus::Open
                })
                .max_by_key(|h| h.created_at)
            {
                println!(
                    "── handoff from {} ({:?}) ──",
                    h.from_actor.short_label(),
                    h.reason
                );
                println!("{}", h.summary);
                if !h.open_questions.trim().is_empty() {
                    println!("\nopen questions:\n{}", h.open_questions);
                }
                h.status = workflows_proto::HandoffStatus::Claimed;
                save_handoffs(&slug, &hs)?;
            } else {
                println!("(no handoff note recorded)");
            }
            // Switch to the work branch if it exists locally.
            let want = format!("task/{short}-");
            let branches = git(&["branch", "--list", &format!("{want}*")]).unwrap_or_default();
            if let Some(line) = branches.lines().next() {
                let b = line.trim_start_matches('*').trim();
                if !b.is_empty() {
                    let _ = git(&["switch", b]);
                    println!("\nswitched to {b}");
                    if let Ok(log) = git(&["log", "--oneline", "-5"]) {
                        println!("recent commits:\n{log}");
                    }
                }
            } else {
                println!(
                    "\n(no local branch {want}* — `git fetch` then switch, or `task code start` to recreate)"
                );
            }
        }
        CodeCmd::Inbox {
            as_agent,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let vox = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&vox).await?;
            let me = as_agent
                .as_deref()
                .map(|s| parse_agent_ref(&format!("agent:{s}")))
                .transpose()?;
            let hs = load_handoffs(&slug)?;
            let open: Vec<&workflows_proto::Handoff> = hs
                .iter()
                .filter(|h| h.status == workflows_proto::HandoffStatus::Open)
                .filter(|h| match (&me, &h.to_actor) {
                    (Some(m), Some(to)) => to == m, // addressed to me
                    (_, None) => true,              // open to anyone
                    _ => true,
                })
                .collect();
            if open.is_empty() {
                println!("(no parked tasks)");
                return Ok(());
            }
            println!("{} parked task(s):", open.len());
            for h in open {
                let title = client
                    .get(h.session_id)
                    .await
                    .map_or_else(|_| "(task?)".into(), |t| t.title);
                println!(
                    "  {}  from {:<16} {:?}  {}",
                    &h.session_id.simple().to_string()[..8],
                    h.from_actor.short_label(),
                    h.reason,
                    title
                );
            }
        }
    }
    Ok(())
}

/// Per-org handoff store path.
fn handoff_store_path(org_slug: &str) -> eyre::Result<std::path::PathBuf> {
    let home = std::env::var_os("HOME").ok_or_else(|| eyre::eyre!("HOME not set"))?;
    Ok(std::path::Path::new(&home)
        .join(".task")
        .join("orgs")
        .join(org_slug)
        .join("handoffs.json"))
}

fn load_handoffs(org_slug: &str) -> eyre::Result<Vec<workflows_proto::Handoff>> {
    let p = handoff_store_path(org_slug)?;
    if !p.exists() {
        return Ok(Vec::new());
    }
    let bytes = std::fs::read(&p).map_err(|e| eyre::eyre!("read {}: {e}", p.display()))?;
    serde_json::from_slice(&bytes).map_err(|e| eyre::eyre!("parse handoffs.json: {e}"))
}

fn save_handoffs(org_slug: &str, hs: &[workflows_proto::Handoff]) -> eyre::Result<()> {
    let p = handoff_store_path(org_slug)?;
    if let Some(parent) = p.parent() {
        std::fs::create_dir_all(parent).map_err(|e| eyre::eyre!("mkdir: {e}"))?;
    }
    let tmp = p.with_extension("json.tmp");
    std::fs::write(&tmp, serde_json::to_vec_pretty(hs)?).map_err(|e| eyre::eyre!("write: {e}"))?;
    std::fs::rename(&tmp, &p).map_err(|e| eyre::eyre!("rename: {e}"))?;
    Ok(())
}

/// Path to the per-org label store JSON.
fn label_store_path(org_slug: &str) -> eyre::Result<std::path::PathBuf> {
    let home = std::env::var_os("HOME").ok_or_else(|| eyre::eyre!("HOME not set"))?;
    Ok(std::path::Path::new(&home)
        .join(".task")
        .join("orgs")
        .join(org_slug)
        .join("labels.json"))
}

fn load_labels(org_slug: &str) -> eyre::Result<Vec<label_proto::Label>> {
    let p = label_store_path(org_slug)?;
    if !p.exists() {
        return Ok(Vec::new());
    }
    let bytes = std::fs::read(&p).map_err(|e| eyre::eyre!("read {}: {e}", p.display()))?;
    serde_json::from_slice(&bytes).map_err(|e| eyre::eyre!("parse labels.json: {e}"))
}

fn save_labels(org_slug: &str, labels: &[label_proto::Label]) -> eyre::Result<()> {
    let p = label_store_path(org_slug)?;
    if let Some(parent) = p.parent() {
        std::fs::create_dir_all(parent).map_err(|e| eyre::eyre!("mkdir: {e}"))?;
    }
    let tmp = p.with_extension("json.tmp");
    std::fs::write(&tmp, serde_json::to_vec_pretty(labels)?)
        .map_err(|e| eyre::eyre!("write: {e}"))?;
    std::fs::rename(&tmp, &p).map_err(|e| eyre::eyre!("rename: {e}"))?;
    Ok(())
}

fn run_label(cmd: LabelCmd) -> eyre::Result<()> {
    match cmd {
        LabelCmd::Create {
            name,
            color,
            group,
            description,
            org,
        } => {
            let slug = resolve_active_org(org)?;
            let mut labels = load_labels(&slug)?;
            if let Some(existing) = labels
                .iter_mut()
                .find(|l| l.name.eq_ignore_ascii_case(&name))
            {
                // Idempotent: update color/group/description on re-create.
                existing.color = color.or(existing.color.take());
                existing.group = group.or(existing.group.take());
                existing.description = description.or(existing.description.take());
                existing.updated_at = chrono::Utc::now();
                save_labels(&slug, &labels)?;
                println!("updated label `{name}`");
                return Ok(());
            }
            // org-scoped: workspace_id is nil (no Workspace entity).
            let mut l = label_proto::Label::new(uuid::Uuid::nil(), &name);
            l.color = color;
            l.group = group;
            l.description = description;
            labels.push(l);
            save_labels(&slug, &labels)?;
            println!("created label `{name}`");
        }
        LabelCmd::List { org, json } => {
            let slug = resolve_active_org(org)?;
            let labels = load_labels(&slug)?;
            if json {
                println!("{}", serde_json::to_string_pretty(&labels)?);
                return Ok(());
            }
            if labels.is_empty() {
                println!("(no labels)");
                return Ok(());
            }
            for l in &labels {
                let color = l
                    .color
                    .as_deref()
                    .map_or(String::new(), |c| format!(" #{c}"));
                let group = l
                    .group
                    .as_deref()
                    .map_or(String::new(), |g| format!(" [{g}]"));
                println!("{}{group}{color}", l.name);
            }
        }
        LabelCmd::Rm { name, org } => {
            let slug = resolve_active_org(org)?;
            let mut labels = load_labels(&slug)?;
            let before = labels.len();
            labels.retain(|l| !l.name.eq_ignore_ascii_case(&name));
            if labels.len() == before {
                return Err(eyre::eyre!("no label named `{name}`"));
            }
            save_labels(&slug, &labels)?;
            println!("removed label `{name}`");
        }
    }
    Ok(())
}

async fn run_setup(cmd: SetupCmd) -> eyre::Result<()> {
    use git_config::BindingStore as _;
    match cmd {
        SetupCmd::Forge {
            repo,
            github,
            base_url,
            webhook_url,
            public_base,
            project,
            org,
        } => {
            let slug = resolve_active_org(org)?;
            let repo_id = build_repo_id(&repo, github, base_url.clone())?;
            let (owner, repo_name) = parse_repo_slug(&repo)?;

            // 1. Ensure a webhook secret exists for this org.
            let secret = ensure_webhook_secret(&slug)?;

            // 2. Resolve the webhook URL.
            let hook_url = if let Some(u) = webhook_url {
                u
            } else {
                let base = public_base.ok_or_else(|| {
                    eyre::eyre!(
                        "pass --webhook-url, or --public-base to derive <base>/org/{slug}/webhooks/forge"
                    )
                })?;
                format!("{}/org/{slug}/webhooks/forge", base.trim_end_matches('/'))
            };

            // 3. Register (or update) the webhook via the forge API.
            let token = if github {
                github_token()?
            } else {
                forgejo_token()?
            };
            let api_base = if github {
                "https://api.github.com".to_string()
            } else {
                forgejo_base_url(base_url)?
            };
            register_webhook(
                &api_base, github, &owner, &repo_name, &token, &hook_url, &secret,
            )
            .await?;

            // 4. Record the repo binding (project/org -> repo).
            let store = forge_link_store(&slug)?;
            let project_id = project.map_or_else(|| slug.clone(), |p| p.to_string());
            store
                .add_repo_binding(git_config::RepoBinding {
                    project_id,
                    repo: repo_id,
                })
                .map_err(|e| eyre::eyre!("repo binding: {e}"))?;

            let forge_label = if github { "github" } else { "forgejo" };
            println!("✓ {forge_label} integration ready for {repo}");
            println!("  webhook → {hook_url}");
            println!("  events: issues, pull_request (signed with the org webhook secret)");
            println!("  secret: ~/.task/orgs/{slug}/webhook-secret");
            if let Some(p) = project {
                println!("  bound to project {p}");
            }
            println!(
                "\nClosing an issue/PR on the forge will now close the linked task\n\
                 (once the task-server is reachable at the webhook URL)."
            );
        }
    }
    Ok(())
}

/// Read the per-org webhook secret, generating + persisting one
/// (64 hex chars) if it doesn't exist yet.
fn ensure_webhook_secret(org_slug: &str) -> eyre::Result<String> {
    let home = std::env::var_os("HOME").ok_or_else(|| eyre::eyre!("HOME not set"))?;
    let path = std::path::Path::new(&home)
        .join(".task")
        .join("orgs")
        .join(org_slug)
        .join("webhook-secret");
    if path.exists() {
        let s = std::fs::read_to_string(&path).map_err(|e| eyre::eyre!("read secret: {e}"))?;
        let t = s.trim();
        if !t.is_empty() {
            return Ok(t.to_string());
        }
    }
    // Generate 32 bytes of entropy as hex via two v4 UUIDs.
    let secret = format!(
        "{}{}",
        uuid::Uuid::new_v4().simple(),
        uuid::Uuid::new_v4().simple()
    );
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).ok();
    }
    std::fs::write(&path, &secret).map_err(|e| eyre::eyre!("write secret: {e}"))?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let _ = std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o600));
    }
    Ok(secret)
}

/// Register (or update if one already targets the same URL) a
/// webhook on a forge repo. Forgejo + GitHub hook APIs differ
/// only in the `type`/`name` field.
async fn register_webhook(
    api_base: &str,
    github: bool,
    owner: &str,
    repo: &str,
    token: &str,
    hook_url: &str,
    secret: &str,
) -> eyre::Result<()> {
    let client = reqwest::Client::builder()
        .user_agent("task/setup")
        .build()
        .map_err(|e| eyre::eyre!("http client: {e}"))?;
    let hooks_url = format!("{api_base}/api/v1/repos/{owner}/{repo}/hooks");
    let hooks_url = if github {
        format!("{api_base}/repos/{owner}/{repo}/hooks")
    } else {
        hooks_url
    };

    // Common config block both forges accept.
    let config = serde_json::json!({
        "url": hook_url,
        "content_type": "json",
        "secret": secret,
    });
    let mut body = serde_json::json!({
        "active": true,
        "events": ["issues", "pull_request"],
        "config": config,
    });
    // Forgejo wants `type`; GitHub wants `name: "web"`.
    if github {
        body["name"] = serde_json::json!("web");
    } else {
        body["type"] = serde_json::json!("forgejo");
    }

    // Idempotency: if a hook already targets this URL, PATCH it.
    let existing: Vec<serde_json::Value> = client
        .get(&hooks_url)
        .header("Authorization", format!("token {token}"))
        .send()
        .await
        .map_err(|e| eyre::eyre!("list hooks: {e}"))?
        .json()
        .await
        .unwrap_or_default();
    let existing_id = existing.iter().find_map(|h| {
        let url = h
            .get("config")
            .and_then(|c| c.get("url"))
            .and_then(|v| v.as_str());
        if url == Some(hook_url) {
            h.get("id").and_then(serde_json::Value::as_u64)
        } else {
            None
        }
    });

    let resp = if let Some(id) = existing_id {
        client
            .patch(format!("{hooks_url}/{id}"))
            .header("Authorization", format!("token {token}"))
            .json(&body)
            .send()
            .await
    } else {
        client
            .post(&hooks_url)
            .header("Authorization", format!("token {token}"))
            .json(&body)
            .send()
            .await
    }
    .map_err(|e| eyre::eyre!("register hook: {e}"))?;

    let status = resp.status();
    if !status.is_success() {
        let text = resp.text().await.unwrap_or_default();
        return Err(eyre::eyre!("forge rejected webhook ({status}): {text}"));
    }
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

// ── Agent task queue (agent-proto / agent-tasks) ─────────────────────

async fn run_agent_queue(cmd: AgentQueueCmd) -> eyre::Result<()> {
    use agent_proto::service::tasks::AgentTaskQueueClient;
    use agent_proto::tasks::QueueFilter;

    async fn connect_queue(url: String) -> eyre::Result<AgentTaskQueueClient> {
        Box::pin(vox::connect(&url).establish())
            .await
            .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))
    }
    let connect = |url: String| connect_queue(url);
    let default_handle = || {
        format!(
            "{}@{}",
            std::env::var("USER").unwrap_or_else(|_| "anon".into()),
            std::env::var("HOSTNAME")
                .or_else(|_| std::env::var("HOST"))
                .unwrap_or_else(|_| "host".into())
        )
    };
    let body = |s: String| -> eyre::Result<String> {
        if s == "-" {
            let mut buf = String::new();
            std::io::Read::read_to_string(&mut std::io::stdin(), &mut buf)?;
            Ok(buf)
        } else {
            Ok(s)
        }
    };

    match cmd {
        AgentQueueCmd::Read {
            queue,
            only_handle,
            include_archived,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect(url.clone()).await?;
            let queue_id = queue.unwrap_or_else(|| slug.clone());
            let filter = QueueFilter {
                assignee: String::new(),
                include_archived,
                only_handle: only_handle.unwrap_or_default(),
                linked_session_id: String::new(),
                agent_profile: String::new(),
            };
            let snap = client
                .read_queue(queue_id, filter)
                .await
                .map_err(|e| eyre::eyre!("read_queue: {e:?}"))?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&snap).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            println!(
                "queue {}  ({} tasks, watermark={})",
                snap.queue.id,
                snap.tasks.len(),
                snap.latest_event_id
            );
            for t in &snap.tasks {
                println!("  {:<10}  {:<32}  {}", t.status, t.title, t.id);
            }
        }
        AgentQueueCmd::Claim {
            task_id,
            handle,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect(url.clone()).await?;
            let h = handle.unwrap_or_else(default_handle);
            let t = client
                .claim_agent_task(task_id, h.clone())
                .await
                .map_err(|e| eyre::eyre!("claim: {e:?}"))?;
            println!("claimed {} as {h} → [{}]", t.title, t.status);
        }
        AgentQueueCmd::SetStatus {
            task_id,
            new_status,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect(url.clone()).await?;
            let t = client
                .set_agent_task_status(task_id, new_status)
                .await
                .map_err(|e| eyre::eyre!("set_status: {e:?}"))?;
            println!("{} → [{}]", t.title, t.status);
        }
        AgentQueueCmd::Complete {
            task_id,
            result,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect(url.clone()).await?;
            let result_blob = body(result)?;
            let t = client
                .complete_agent_task(task_id, result_blob)
                .await
                .map_err(|e| eyre::eyre!("complete: {e:?}"))?;
            println!("completed {} → [{}]", t.title, t.status);
        }
        AgentQueueCmd::Link {
            task_id,
            session_id,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect(url.clone()).await?;
            let t = client
                .link_agent_task_to_session(task_id, session_id.clone())
                .await
                .map_err(|e| eyre::eyre!("link: {e:?}"))?;
            println!("linked {} → session {session_id}", t.title);
        }
        AgentQueueCmd::Links {
            queue,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect(url.clone()).await?;
            let queue_id = queue.unwrap_or_else(|| slug.clone());
            let links = client
                .list_agent_task_links(queue_id)
                .await
                .map_err(|e| eyre::eyre!("links: {e:?}"))?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&links).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            for l in &links {
                println!("{}  →  {}  ({})", l.from_task, l.to_task, l.kind);
            }
        }
    }
    Ok(())
}

// ── Body metrics (body::Store) ───────────────────────────────────────

async fn connect_body_client(url: &str) -> eyre::Result<body::BodyServiceClient> {
    Box::pin(vox::connect(url).establish())
        .await
        .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))
}

async fn resolve_body_target(
    client: &body::BodyServiceClient,
    target: &str,
) -> eyre::Result<body::BodyMetric> {
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
        .find(|m| m.path == target || m.name == target || m.kind == target)
        .ok_or_else(|| eyre::eyre!("not found: {target}"))
}

async fn run_body(cmd: BodyCmd) -> eyre::Result<()> {
    match cmd {
        BodyCmd::List { org, server, json } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_body_client(&u).await?;
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
            for m in &rows {
                let goal = m.goal.map(|g| format!(" goal={g}")).unwrap_or_default();
                let latest = m
                    .entries
                    .0
                    .last()
                    .map(|e| format!("  last {}: {}{}", e.date, e.value, m.unit))
                    .unwrap_or_default();
                println!("{:<24}  {:<10}{goal}{latest}", m.name, m.kind);
            }
        }
        BodyCmd::Get {
            target,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_body_client(&u).await?;
            let m = resolve_body_target(&client, &target).await?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&m).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            println!("{} [{}]\n", m.name, m.kind);
            println!("  id:    {}", m.id);
            println!("  path:  {}", m.path);
            println!("  unit:  {}", m.unit);
            if let Some(g) = m.goal {
                println!("  goal:  {g}");
            }
            println!("  entries: {} (last 10)", m.entries.0.len());
            for e in m.entries.0.iter().rev().take(10) {
                println!("    {}  {}{}", e.date, e.value, m.unit);
            }
        }
        BodyCmd::Create {
            name,
            kind,
            unit,
            goal,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_body_client(&u).await?;
            let new_metric = body::BodyMetric {
                path: String::new(),
                id: uuid::Uuid::nil(),
                name,
                kind: kind.unwrap_or_else(|| "other".into()),
                unit: unit.unwrap_or_default(),
                goal,
                tags: body::model::Tags(Vec::new()),
                entries: body::model::Entries(Vec::new()),
                date_created: None,
                date_modified: None,
                details: String::new(),
            };
            let created = client
                .create(new_metric)
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
        BodyCmd::Log {
            target,
            value,
            date,
            unit,
            note,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_body_client(&u).await?;
            let m = resolve_body_target(&client, &target).await?;
            let day = match date {
                Some(s) => chrono::NaiveDate::parse_from_str(&s, "%Y-%m-%d")
                    .map_err(|e| eyre::eyre!("--date: {e}"))?,
                None => chrono::Local::now().date_naive(),
            };
            let entry = body::model::BodyEntry {
                id: uuid::Uuid::new_v4(),
                date: day,
                value,
                unit,
                note,
            };
            let updated = client
                .log_entry(m.id.to_string(), entry)
                .await
                .map_err(|e| eyre::eyre!("log_entry: {e:?}"))?;
            println!(
                "logged {} {} on {} for {}",
                value, updated.unit, day, updated.name
            );
        }
        BodyCmd::Delete {
            target,
            yes,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_body_client(&u).await?;
            let m = resolve_body_target(&client, &target).await?;
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

// ── Exercises (exercises::Store) ─────────────────────────────────────

async fn connect_exercises_client(url: &str) -> eyre::Result<exercises::ExercisesServiceClient> {
    Box::pin(vox::connect(url).establish())
        .await
        .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))
}

async fn resolve_exercise_target(
    client: &exercises::ExercisesServiceClient,
    target: &str,
) -> eyre::Result<exercises::Exercise> {
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
        .find(|e| e.path == target || e.name.eq_ignore_ascii_case(target))
        .ok_or_else(|| eyre::eyre!("not found: {target}"))
}

async fn run_exercise(cmd: ExerciseCmd) -> eyre::Result<()> {
    match cmd {
        ExerciseCmd::List {
            query,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_exercises_client(&u).await?;
            let rows: Vec<_> = client
                .list()
                .await
                .map_err(|e| eyre::eyre!("list: {e:?}"))?
                .into_iter()
                .filter(|e| {
                    query
                        .as_deref()
                        .is_none_or(|q| e.name.to_lowercase().contains(&q.to_lowercase()))
                })
                .collect();
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&rows).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            for e in &rows {
                println!("{:<32}  {:<12}    {}", e.name, e.category, e.path);
            }
        }
        ExerciseCmd::Get {
            target,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_exercises_client(&u).await?;
            let e = resolve_exercise_target(&client, &target).await?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&e).map_err(|err| eyre::eyre!("json: {err}"))?
                );
                return Ok(());
            }
            println!("{}\n", e.name);
            println!("  id:        {}", e.id);
            println!("  path:      {}", e.path);
            println!("  category:  {}", e.category);
            if !e.primary_muscles.is_empty() {
                println!("  muscles:   {}", e.primary_muscles.0.join(", "));
            }
            if !e.equipment.is_empty() {
                println!("  equipment: {}", e.equipment.0.join(", "));
            }
        }
        ExerciseCmd::Create {
            name,
            kind,
            muscle,
            tags,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_exercises_client(&u).await?;
            let primary_muscles = muscle
                .map(|m| exercises::model::StringList(vec![m]))
                .unwrap_or_default();
            let new_ex = exercises::Exercise {
                path: String::new(),
                id: uuid::Uuid::nil(),
                name,
                aliases: exercises::model::StringList::default(),
                description: None,
                category: kind.unwrap_or_else(|| "other".into()),
                primary_muscles,
                secondary_muscles: exercises::model::StringList::default(),
                equipment: exercises::model::StringList::default(),
                mechanics: None,
                force: None,
                instructions: exercises::model::StringList::default(),
                video_url: None,
                image_url: None,
                tags: exercises::model::StringList(tags),
                date_created: None,
                date_modified: None,
                details: String::new(),
            };
            let created = client
                .create(new_ex)
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
        ExerciseCmd::Rename {
            target,
            new_path,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_exercises_client(&u).await?;
            let e = resolve_exercise_target(&client, &target).await?;
            let renamed = client
                .rename(e.id.to_string(), new_path)
                .await
                .map_err(|e| eyre::eyre!("rename: {e:?}"))?;
            println!("renamed → {}", renamed.path);
        }
        ExerciseCmd::Delete {
            target,
            yes,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_exercises_client(&u).await?;
            let e = resolve_exercise_target(&client, &target).await?;
            if !yes && !confirm(&format!("delete `{}` ({})?", e.name, e.path))? {
                println!("aborted");
                return Ok(());
            }
            client
                .delete(e.id.to_string())
                .await
                .map_err(|e| eyre::eyre!("delete: {e:?}"))?;
            println!("deleted {}", e.path);
        }
    }
    Ok(())
}

// ── Workouts (routines + sessions) ───────────────────────────────────

async fn connect_workouts_client(url: &str) -> eyre::Result<workouts::WorkoutsServiceClient> {
    Box::pin(vox::connect(url).establish())
        .await
        .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))
}

async fn resolve_routine_target(
    client: &workouts::WorkoutsServiceClient,
    target: &str,
) -> eyre::Result<workouts::Routine> {
    if uuid::Uuid::parse_str(target).is_ok() {
        return client
            .get_routine(target.to_owned())
            .await
            .map_err(|e| eyre::eyre!("get_routine: {e:?}"));
    }
    let rows = client
        .list_routines()
        .await
        .map_err(|e| eyre::eyre!("list_routines: {e:?}"))?;
    rows.into_iter()
        .find(|r| r.path == target || r.name.eq_ignore_ascii_case(target))
        .ok_or_else(|| eyre::eyre!("not found: {target}"))
}

async fn resolve_session_target(
    client: &workouts::WorkoutsServiceClient,
    target: &str,
) -> eyre::Result<workouts::WorkoutSession> {
    if uuid::Uuid::parse_str(target).is_ok() {
        return client
            .get_session(target.to_owned())
            .await
            .map_err(|e| eyre::eyre!("get_session: {e:?}"));
    }
    let rows = client
        .list_sessions()
        .await
        .map_err(|e| eyre::eyre!("list_sessions: {e:?}"))?;
    rows.into_iter()
        .find(|s| s.path == target)
        .ok_or_else(|| eyre::eyre!("not found: {target}"))
}

async fn run_workout(cmd: WorkoutCmd) -> eyre::Result<()> {
    match cmd {
        WorkoutCmd::Routine(rc) => run_workout_routine(rc).await,
        WorkoutCmd::Session(sc) => run_workout_session(sc).await,
    }
}

async fn run_workout_routine(cmd: WorkoutRoutineCmd) -> eyre::Result<()> {
    match cmd {
        WorkoutRoutineCmd::List { org, server, json } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_workouts_client(&u).await?;
            let rows = client
                .list_routines()
                .await
                .map_err(|e| eyre::eyre!("list_routines: {e:?}"))?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&rows).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            for r in &rows {
                println!("{:<32}  {} days    {}", r.name, r.days.0.len(), r.path);
            }
        }
        WorkoutRoutineCmd::Get {
            target,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_workouts_client(&u).await?;
            let r = resolve_routine_target(&client, &target).await?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&r).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            println!("{}\n", r.name);
            println!("  id:    {}", r.id);
            println!("  path:  {}", r.path);
            for d in &r.days.0 {
                println!("  day:   {}  ({} slots)", d.name, d.slots.len());
            }
        }
        WorkoutRoutineCmd::Delete {
            target,
            yes,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_workouts_client(&u).await?;
            let r = resolve_routine_target(&client, &target).await?;
            if !yes && !confirm(&format!("delete `{}` ({})?", r.name, r.path))? {
                println!("aborted");
                return Ok(());
            }
            client
                .delete_routine(r.id.to_string())
                .await
                .map_err(|e| eyre::eyre!("delete_routine: {e:?}"))?;
            println!("deleted {}", r.path);
        }
    }
    Ok(())
}

async fn run_workout_session(cmd: WorkoutSessionCmd) -> eyre::Result<()> {
    match cmd {
        WorkoutSessionCmd::List {
            date,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_workouts_client(&u).await?;
            let parsed = match date {
                None => None,
                Some(s) => Some(
                    chrono::NaiveDate::parse_from_str(&s, "%Y-%m-%d")
                        .map_err(|e| eyre::eyre!("--date: {e}"))?,
                ),
            };
            let rows: Vec<_> = client
                .list_sessions()
                .await
                .map_err(|e| eyre::eyre!("list_sessions: {e:?}"))?
                .into_iter()
                .filter(|s| parsed.is_none_or(|d| s.date == d))
                .collect();
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&rows).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            for s in &rows {
                println!(
                    "{}  {:<24}  {} sets    {}",
                    s.date,
                    s.name,
                    s.logged_sets.0.len(),
                    s.path
                );
            }
        }
        WorkoutSessionCmd::Get {
            target,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_workouts_client(&u).await?;
            let s = resolve_session_target(&client, &target).await?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&s).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            println!("{} ({})\n", s.name, s.date);
            println!("  id:    {}", s.id);
            println!("  path:  {}", s.path);
            for set in &s.logged_sets.0 {
                let rpe = set.rpe.map(|r| format!(" @ rpe {r}")).unwrap_or_default();
                println!(
                    "    [{}] {}: {}x{}kg{rpe}",
                    set.order, set.exercise_name, set.reps, set.weight_kg
                );
            }
        }
        WorkoutSessionCmd::StartFromRoutine {
            routine,
            day,
            date,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_workouts_client(&u).await?;
            let r = resolve_routine_target(&client, &routine).await?;
            let date_str = date.unwrap_or_else(|| chrono::Local::now().date_naive().to_string());
            let session = client
                .start_from_routine(r.id.to_string(), day, date_str)
                .await
                .map_err(|e| eyre::eyre!("start_from_routine: {e:?}"))?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&session).map_err(|e| eyre::eyre!("json: {e}"))?
                );
            } else {
                println!("started {} ({})", session.name, session.path);
                println!("  id: {}", session.id);
            }
        }
        WorkoutSessionCmd::LogSet {
            session,
            exercise,
            reps,
            weight,
            rpe,
            note,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_workouts_client(&u).await?;
            let s = resolve_session_target(&client, &session).await?;
            // Resolve the exercise to its id (auto-population
            // of `exercise_name` happens server-side; we still
            // pass a best-effort cache here).
            let ec = connect_exercises_client(&u).await?;
            let ex = resolve_exercise_target(&ec, &exercise).await?;
            let order = u32::try_from(s.logged_sets.0.len()).unwrap_or(0);
            let set = workouts::LoggedSet {
                id: uuid::Uuid::new_v4(),
                exercise_id: ex.id,
                exercise_name: ex.name,
                order,
                reps,
                weight_kg: weight,
                rir: None,
                rpe,
                completed: true,
                note,
            };
            let updated = client
                .log_set(s.id.to_string(), set)
                .await
                .map_err(|e| eyre::eyre!("log_set: {e:?}"))?;
            println!(
                "logged set #{order} on {} ({} total sets)",
                updated.name,
                updated.logged_sets.0.len()
            );
        }
        WorkoutSessionCmd::Delete {
            target,
            yes,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_workouts_client(&u).await?;
            let s = resolve_session_target(&client, &target).await?;
            if !yes && !confirm(&format!("delete `{}` ({})?", s.name, s.path))? {
                println!("aborted");
                return Ok(());
            }
            client
                .delete_session(s.id.to_string())
                .await
                .map_err(|e| eyre::eyre!("delete_session: {e:?}"))?;
            println!("deleted {}", s.path);
        }
    }
    Ok(())
}

// ── Intake (intake::Store) ───────────────────────────────────────────

async fn connect_intake_client(url: &str) -> eyre::Result<intake::IntakeServiceClient> {
    Box::pin(vox::connect(url).establish())
        .await
        .map_err(|e| eyre::eyre!("connect `{url}`: {e:?}"))
}

async fn resolve_intake_target(
    client: &intake::IntakeServiceClient,
    target: &str,
) -> eyre::Result<intake::IntakeLog> {
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
        .find(|l| l.path == target || l.date.to_string() == target)
        .ok_or_else(|| eyre::eyre!("not found: {target}"))
}

async fn run_intake(cmd: IntakeCmd) -> eyre::Result<()> {
    match cmd {
        IntakeCmd::List { org, server, json } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_intake_client(&u).await?;
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
            for l in &rows {
                println!(
                    "{}  {:<24}  {} entries    {}",
                    l.date,
                    l.name,
                    l.entries.0.len(),
                    l.path
                );
            }
        }
        IntakeCmd::Get {
            target,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_intake_client(&u).await?;
            let l = resolve_intake_target(&client, &target).await?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&l).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            println!("{} ({})\n", l.name, l.date);
            println!("  id:    {}", l.id);
            println!("  path:  {}", l.path);
            for e in &l.entries.0 {
                let slot = e.slot.as_deref().unwrap_or("?");
                println!("    [{slot}] {}", e.name);
            }
        }
        IntakeCmd::ForDay {
            date,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_intake_client(&u).await?;
            let l = client
                .for_day(date.clone())
                .await
                .map_err(|e| eyre::eyre!("for_day: {e:?}"))?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&l).map_err(|e| eyre::eyre!("json: {e}"))?
                );
            } else {
                println!("{} ({})", l.name, l.date);
                println!("  {} entries", l.entries.0.len());
            }
        }
        IntakeCmd::LogRecipe {
            date,
            recipe,
            servings,
            slot,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_intake_client(&u).await?;
            let updated = client
                .log_recipe(date, recipe, servings, slot)
                .await
                .map_err(|e| eyre::eyre!("log_recipe: {e:?}"))?;
            println!(
                "logged → {} entries on {}",
                updated.entries.0.len(),
                updated.date
            );
        }
        IntakeCmd::LogPantry {
            date,
            item,
            qty,
            slot,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_intake_client(&u).await?;
            // Resolve pantry path/name → id.
            let pc = connect_pantry_client(&u).await?;
            let p = resolve_pantry_target(&pc, &item).await?;
            let updated = client
                .log_pantry(date, p.id.to_string(), qty, slot)
                .await
                .map_err(|e| eyre::eyre!("log_pantry: {e:?}"))?;
            println!(
                "logged → {} entries on {}",
                updated.entries.0.len(),
                updated.date
            );
        }
        IntakeCmd::LogFreeform {
            date,
            name,
            kcal,
            slot,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_intake_client(&u).await?;
            let nutrition = cookbook::Nutrition {
                calories: kcal,
                protein_g: None,
                carbs_g: None,
                fat_g: None,
                fiber_g: None,
                sugar_g: None,
            };
            let updated = client
                .log_freeform(date, name, nutrition, slot)
                .await
                .map_err(|e| eyre::eyre!("log_freeform: {e:?}"))?;
            println!(
                "logged → {} entries on {}",
                updated.entries.0.len(),
                updated.date
            );
        }
        IntakeCmd::Delete {
            target,
            yes,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_intake_client(&u).await?;
            let l = resolve_intake_target(&client, &target).await?;
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
