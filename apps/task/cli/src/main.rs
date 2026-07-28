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

mod admin;
mod auth;
mod brief;
mod collection;
mod cycle;
mod errors;
mod finance;
mod forge;
mod goal;
mod json_out;
mod mealprep;
mod media;
mod mount;
mod org;
mod org_ctx;
mod plan;
mod project;
mod recipe_import;
mod session_store;
mod shared;
mod task_cmd;
mod timer;
mod wiki;
mod workstream;

use clap::{Parser, Subcommand};
use crate::task_cmd::{TaskCmd, connect_task_client, run_task};
use crate::wiki::{WikiCmd, run_wiki};
use crate::timer::{TimerCmd, run_timer, timer_owner_id};
use crate::finance::{FinanceCmd, run_finance};
use crate::auth::{AuthCmd, run_auth, ws_base_to_http};
use crate::admin::{AdminCmd, run_admin};
use crate::org::{OrgCmd, run_org};
use crate::mount::{MountCmd, run_mount};
use crate::cycle::{CycleCmd, run_cycle};
use crate::goal::{GoalCmd, connect_goal_client, mutate_goal, resolve_cycle_arg, resolve_goal_target, run_goal};
use crate::project::{ProjectCmd, connect_project_client, resolve_project_target, run_project};
use crate::forge::{ForgeBackend, build_repo_id, forge_backend_for, forge_link_store, forgejo_base_url, forgejo_token, github_token, parse_repo_slug};
use shared::RemoteVoxConfig;
use shared::{confirm, git, resolve_body, short_uuid};
use std::collections::HashMap;
#[derive(Parser)]
#[command(name = "task", about = "Task management CLI", version)]
struct Cli {
    /// Vox WebSocket URL (e.g. <ws://127.0.0.1:9090/vox>). Falls back
    /// to `TASK_VOX_URL` (loaded from .env) then to the localhost
    /// default.
    #[arg(long, env = "TASK_VOX_URL", global = true)]
    server: Option<String>,

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
    /// command itself routes through `agent-::wiki::bridge`.
    #[command(subcommand)]
    Wiki(WikiCmd),
    /// Billable time tracking. Local SQLite backed (no
    /// server needed); same `::timer::Store` the server
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
    /// Server administration — server-native git snapshots of the
    /// data root (snapshot / log / branch / restore). Talks to
    /// `<server>/server/vox` (`SnapshotService`), like `task org
    /// create`.
    #[command(subcommand)]
    Admin(AdminCmd),
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
    /// Workstreams — the parent-with-swarm construct (lead +
    /// members + status + dates) that replaces the 'epic' tag.
    /// Tasks attach via `workflow.workstream`; progress is a
    /// derived rollup (`task workstream rollup`).
    #[command(subcommand)]
    Workstream(workstream::WorkstreamCmd),
    /// Ordered collections — libraries, setlists, shows, playlists.
    /// All the same primitive: an ordered list of `NodeRef` items
    /// over `CollectionService`. Create, populate, reorder, and
    /// inspect headlessly (the entry point for library/setlist
    /// seeding).
    #[command(subcommand)]
    Collection(collection::CollectionCmd),
    /// Songs — build a durable Song folder (via the `song` crate)
    /// and add it to a target collection as a `song:` node.
    #[command(subcommand)]
    Song(collection::SongCmd),
    /// Media — content-addressed blobs streamed over vox (stat /
    /// get / verify-song). The no-browser audio-streaming E2E.
    #[command(subcommand)]
    Media(media::MediaCmd),
    /// Physical places — studios, rooms, venues, storage.
    /// Pantry + inventory reference these by id.
    #[command(subcommand)]
    Location(LocationCmd),
    /// Inbox — capture fleeting notes and triage the daily queue.
    #[command(subcommand)]
    Inbox(InboxCmd),
    /// Threads — log conversations & topics on a task or project.
    #[command(subcommand)]
    Threads(ThreadsCmd),
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
    /// Shopping lists — auto-populate from recipe shortages /
    /// low stock / expiry; mark-purchased restocks the pantry.
    #[command(subcommand)]
    Shopping(mealprep::ShoppingCmd),
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
    /// Day-plan schedule surface — show / edit blocks, assign
    /// tasks, materialize from templates, plan-vs-actual diff.
    /// All logic in `plan.rs`.
    #[command(subcommand)]
    Plan(plan::PlanCmd),
    /// What should I be doing right now — current block + time
    /// remaining + next block (falls back to the next due task).
    Next(plan::NextArgs),
    /// Morning digest — today's blocks + events, due/overdue +
    /// in-progress tasks, active timer, blocked agent tasks, open
    /// inbox, meals + bookings. All logic in `brief.rs`.
    Brief(brief::BriefArgs),
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
/// Log conversations & topics against a task or project. `new` opens a
/// thread (topic); `post` adds a message; `list`/`show` read them.
/// Anchored by `(entity_type, entity_id)` so the same primitive works
/// for any entity later (forge issues, chats, ingested comms).
///
/// Org / server routing: uses the global `--org` / `--server` flags
/// (no per-variant duplicates).
enum ThreadsCmd {
    /// Open a new thread (topic) on a task or project.
    New {
        /// Host entity kind: `task` | `project`.
        #[arg(long)]
        entity_type: String,
        /// Host entity — UUID, id prefix, vault path, or title
        /// (resolved per `--entity-type`).
        #[arg(long)]
        entity_id: String,
        /// Topic / title. Quote multi-word.
        title: Vec<String>,
        /// Kind: `discussion` (default) | `question` | `decision` | `action` | `praise`.
        #[arg(long)]
        kind: Option<String>,
    },
    /// Post a message to a thread.
    Post {
        /// Target thread id.
        thread_id: uuid::Uuid,
        /// Message text. Quote multi-word.
        text: Vec<String>,
        /// Reply to another message in the thread.
        #[arg(long)]
        reply_to: Option<uuid::Uuid>,
        /// Source label: `native` (default) | `agent` | …
        #[arg(long)]
        source: Option<String>,
        /// Author display label. Defaults to `cli` (or `agent` when `--source agent`).
        #[arg(long)]
        author: Option<String>,
    },
    /// List threads on a task or project.
    List {
        #[arg(long)]
        entity_type: String,
        /// Host entity — UUID, id prefix, vault path, or title
        /// (resolved per `--entity-type`).
        #[arg(long)]
        entity_id: String,
        #[arg(long)]
        json: bool,
    },
    /// Show a thread's messages.
    Show {
        thread_id: uuid::Uuid,
        #[arg(long)]
        json: bool,
    },
    /// Mark a thread resolved (or `--unresolve` to reopen).
    Resolve {
        thread_id: uuid::Uuid,
        #[arg(long)]
        unresolve: bool,
    },
    /// Delete a thread and its messages.
    Rm { id: uuid::Uuid },
}

#[derive(Subcommand)]
/// Capture + triage the inbox — the FLAP "capture" loop. Capture a
/// fleeting note with `add`, read the queue with `list` (open items,
/// oldest first), then `mark` / `snooze` / `rm` during the daily
/// review.
enum InboxCmd {
    /// Capture a note into the inbox (default kind `fleeting`).
    Add {
        /// The note text. Quote multi-word captures.
        text: Vec<String>,
        /// Note kind: `fleeting` (default), `literature`, `lecture`.
        #[arg(long)]
        kind: Option<String>,
        /// Capture source label. Defaults to `cli`.
        #[arg(long)]
        source: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Stage an agent-proposed capture for one-tap review (status
    /// `suggested`). Producers (email ingestion, …) use this so
    /// suggestions don't flood the open queue until you accept them.
    Suggest {
        /// The summary text. Quote multi-word input.
        text: Vec<String>,
        /// Capture source label, e.g. `email`. Defaults to `agent`.
        #[arg(long)]
        source: Option<String>,
        /// Optional link back to the original (appended to the body).
        #[arg(long)]
        link: Option<String>,
        /// Note kind: `fleeting` (default), `literature`, `lecture`.
        #[arg(long)]
        kind: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// The AI daily processing pass: run ONE LLM turn over every
    /// open item and review the proposed promotions — `task`
    /// (created via TaskService), `note` (written to the org vault),
    /// or `skip` (offer archive). Drives `codex app-server` like
    /// `task wiki ingest`; `--heuristic` skips the LLM and proposes
    /// a task per item from `task::capture` parsing alone.
    Process {
        /// Model id (`gpt-5.4-mini`, `o3`, …). Default: daemon default.
        #[arg(long)]
        model: Option<String>,
        /// Print the proposals and stop — apply nothing.
        #[arg(long)]
        dry_run: bool,
        /// Accept every proposal without prompting.
        #[arg(long)]
        yes: bool,
        /// Deterministic proposals without an LLM: every item
        /// proposes a task, body first line = capture input.
        #[arg(long)]
        heuristic: bool,
        /// LLM turn timeout in seconds (the one turn covers the
        /// whole batch).
        #[arg(long, default_value_t = 300)]
        timeout_secs: u64,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// List the inbox. By default shows only `open` items, oldest
    /// first; `--all` includes processed + archived.
    List {
        #[arg(long)]
        all: bool,
        #[arg(long)]
        json: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Set an item's triage status: `open` / `processed` / `archived`.
    Mark {
        id: String,
        /// `open` | `processed` | `archived`.
        status: String,
        /// For `processed`: id of the task / note it became.
        #[arg(long)]
        into: Option<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Snooze an item until a date (`YYYY-MM-DD`); it's hidden from
    /// the daily queue until then.
    Snooze {
        id: String,
        until: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Permanently delete an item.
    Rm {
        id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
}

#[derive(clap::Subcommand)]
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
    /// Author a new `.cook` recipe (validates the cooklang by
    /// parsing before anything is written).
    Create(mealprep::RecipeCreateArgs),
    /// Import a recipe from a webpage (schema.org/Recipe → cooklang;
    /// LLM-synthesized, `--offline` for the deterministic converter,
    /// `--from-file` for bot-protected sites).
    Import(recipe_import::RecipeImportArgs),
    /// Replace an existing recipe's cooklang source (validates
    /// by parsing first).
    Update(mealprep::RecipeUpdateArgs),
    /// Rendered view — ingredients / cookware / steps /
    /// servings (`--json` for the wire shape).
    Show(mealprep::RecipeShowArgs),
    /// Fulfillment check against the pantry: have / missing /
    /// substitution suggestions.
    CanCook(mealprep::CanCookArgs),
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
    /// Put the meal on its date's day plan as a `Meal` block
    /// (`task meal schedule <meal> 17:30-18:30`). Overlapping
    /// blocks are rejected unless `--force`.
    Schedule(mealprep::MealScheduleArgs),
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

/// Linear-style issue surface over TaskInfo's WorkflowAttrs.
///
/// **Why this exists alongside `task task *`.** TaskInfo is the
/// canonical unit of work in Task — the same row underpins both
/// `task task *` (the TaskNotes-shape personal-task surface)
/// and `task issue *` (the Linear-shape work-tracking surface).
/// `issue` verbs operate through `WorkflowAttrs`: filter / show
/// / patch the workspace + cycle + project + estimate +
/// assignees + blockers triplet.
///
/// Org / server routing: this command group relies on the global
/// `--org` / `--server` flags (clap propagates them, so they can
/// still be passed after the subcommand) instead of re-declaring
/// per-variant duplicates like the older groups do.
#[derive(Subcommand)]
enum IssueCmd {
    /// List tasks filtered by their workflow attributes.
    List {
        /// Filter by cycle — UUID, `YYYY:Qn:Cm` / `YYYY-Qn-Cm`
        /// label, or `current` for today's cycle.
        #[arg(long)]
        cycle: Option<String>,
        /// Filter by project — UUID, id prefix, vault path, or
        /// name (exact / unique prefix).
        #[arg(long)]
        project: Option<String>,
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
        /// Emit JSON instead of the tabular default.
        #[arg(long)]
        json: bool,
    },

    /// Show a single issue. Accepts a UUID, an id prefix, a vault
    /// path, or a title (exact / unique prefix).
    Show {
        id: String,
        #[arg(long)]
        json: bool,
    },

    /// Render the agent prompt for a task — its PRD plus the parent
    /// issue's PRD (when it's a subtask), formatted as the directive
    /// an agent receives. The same renderer `task agent goal --task`
    /// feeds the loop, exposed standalone so you can inspect exactly
    /// what an agent will be handed.
    Prompt { id: String },

    /// Patch the issue's `WorkflowAttrs` in place. Repeatable
    /// `--add-assignee` / `--add-blocker` for set operations.
    /// Pass `--clear` to drop the workflow block entirely (the
    /// task becomes a plain TaskNotes-shape task again).
    SetWorkflow {
        id: String,
        /// UUID, `YYYY:Qn:Cm`, `current`, or `"none"` / `""` to
        /// clear.
        #[arg(long)]
        cycle: Option<String>,
        /// Project reference (UUID, name, path, prefix), or
        /// `"none"` / `""` to clear.
        #[arg(long)]
        project: Option<String>,
        /// Workstream reference (UUID, name, path, prefix), or
        /// `"none"` / `""` to clear. Sets `workflow.workstream`.
        #[arg(long)]
        workstream: Option<String>,
        /// `xs`, `s`, `m`, `l`, `xl`, or a plain integer for
        /// `Estimate::Points`.
        #[arg(long)]
        estimate: Option<String>,
        #[arg(long = "add-assignee", value_name = "AGENT")]
        add_assignee: Vec<String>,
        #[arg(long = "remove-assignee", value_name = "AGENT")]
        remove_assignee: Vec<String>,
        /// Blocking issue (UUID, id prefix, path, or title).
        #[arg(long = "add-blocker", value_name = "TASK")]
        add_blocker: Vec<String>,
        /// Blocking issue (UUID, id prefix, path, or title).
        #[arg(long = "remove-blocker", value_name = "TASK")]
        remove_blocker: Vec<String>,
        /// Drop the workflow block entirely.
        #[arg(long)]
        clear: bool,
        /// Emit the resulting issue as JSON.
        #[arg(long)]
        json: bool,
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
        /// Emit the claimed issue as JSON.
        #[arg(long)]
        json: bool,
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
    },

    /// List the subtasks of a parent task with their claim +
    /// status, so you can see who's working what at a glance.
    /// Header shows the derived rollup (done / in-progress /
    /// blocked / points), classified via state groups.
    Subtasks {
        /// Parent task id (UUID or 8-char prefix).
        id: String,
        #[arg(long)]
        json: bool,
    },

    /// Derived sub-issue rollup for one parent — done / total /
    /// in-progress / blocked / estimate points over its direct
    /// children (`workflow.parent`), classified via each child's
    /// project state registry. Same engine as the workstream
    /// rollup.
    Rollup {
        /// Parent issue id (UUID, prefix, path, or title).
        id: String,
        #[arg(long)]
        json: bool,
    },

    /// Add a typed relation between two issues:
    /// `task issue relate <a> <kind> <b>` records
    /// "`<a>` `<kind>`s `<b>`" (kind ∈ blocks | duplicate | implements
    /// | relates).
    /// Stored in `<a>`'s `workflow.relations`; the legacy
    /// blockers / relates_to lists keep working alongside.
    Relate {
        /// Source issue (UUID, prefix, path, or title).
        a: String,
        /// blocks | duplicate | implements | relates.
        kind: String,
        /// Target issue (UUID, prefix, path, or title).
        b: String,
        /// Remove the relation instead of adding it.
        #[arg(long)]
        remove: bool,
        /// Emit the resulting source issue as JSON.
        #[arg(long)]
        json: bool,
    },

    /// Show an issue's relation graph — outgoing edges (typed
    /// relations + the legacy relates_to entries + "blocks"
    /// edges implied by other tasks' blockers lists) and
    /// incoming reverse edges ("what blocks / duplicates /
    /// implements THIS"), merged across both encodings.
    Relations {
        id: String,
        #[arg(long)]
        json: bool,
    },

    /// Sugar: every issue this one BLOCKS (typed `blocks`
    /// relations + other tasks listing it in `blockers`).
    Blocking {
        id: String,
        #[arg(long)]
        json: bool,
    },

    /// List the current assignees on an issue.
    Assignees {
        id: String,
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
        /// Cycle (UUID, `YYYY:Qn:Cm`, or `current`). Sets
        /// `workflow.cycle`.
        #[arg(long)]
        cycle: Option<String>,
        /// Project (UUID, name, path, prefix). Sets `project_id`.
        #[arg(long)]
        project: Option<String>,
        /// Parent issue (UUID, id prefix, path, or title) — makes
        /// this a subtask. Sets `workflow.parent`.
        #[arg(long)]
        parent: Option<String>,
        /// Workstream (UUID, name, path, prefix). Sets
        /// `workflow.workstream`.
        #[arg(long)]
        workstream: Option<String>,
        /// Estimate (`xs` / `s` / `m` / `l` / `xl` / integer).
        #[arg(long)]
        estimate: Option<String>,
        /// Repeatable assignee. `agent:name[@ver]` or
        /// `human:user_id`. Bare names default to agent.
        #[arg(long = "assignee", value_name = "AGENT")]
        assignees: Vec<String>,
        /// Repeatable blocker (UUID, id prefix, path, or title) —
        /// `task issue ready` won't surface this issue until each
        /// blocker closes.
        #[arg(long = "blocker", value_name = "TASK")]
        blockers: Vec<String>,
        /// Repeatable tag.
        #[arg(long = "tag", value_name = "TAG")]
        tags: Vec<String>,
        /// Body (markdown). Pass `-` for stdin, or a file path.
        #[arg(long)]
        body: Option<String>,
        #[arg(long)]
        json: bool,
    },

    /// Show issues ready to work — open, not done, with no
    /// unresolved blockers. The beads-equivalent of `bd ready`.
    Ready {
        /// Cycle filter — UUID, `YYYY:Qn:Cm` label, or `current`.
        #[arg(long)]
        cycle: Option<String>,
        /// Project filter — UUID, id prefix, path, or name.
        #[arg(long)]
        project: Option<String>,
        /// Show only issues claimable by this agent (no
        /// assignee yet, OR this agent is already listed).
        #[arg(long)]
        as_agent: Option<String>,
        /// Max rows to show.
        #[arg(long, default_value = "20")]
        limit: usize,
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
        /// Emit the resulting issue as JSON.
        #[arg(long)]
        json: bool,
    },

    /// Close an issue — flips status to `done` and stamps
    /// `completedDate`. Pass `--undo` to reopen.
    Close {
        id: String,
        #[arg(long)]
        undo: bool,
        /// Emit the resulting issue as JSON.
        #[arg(long)]
        json: bool,
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
    },

    /// Project-level overview — counts grouped by status,
    /// priority, workspace, and assignee. Beads-equivalent of
    /// `bd stats`.
    Stats {
        /// Restrict to one project (UUID, id prefix, path, or
        /// name).
        #[arg(long)]
        project: Option<String>,
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
        /// Optional project (UUID, name, path, prefix) to stamp
        /// on newly-pulled tasks.
        #[arg(long)]
        project: Option<String>,
        /// Don't create local tasks for forge issues we don't
        /// track — only reconcile state of already-linked ones.
        #[arg(long)]
        no_pull: bool,
    },

    /// Sync every linked repo in the org in one pass — one
    /// cron line keeps all your tracked repos fresh without
    /// webhooks.
    SyncAll {
        /// Optional project (UUID, name, path, prefix) to stamp
        /// on newly-pulled tasks.
        #[arg(long)]
        project: Option<String>,
        /// Only reconcile existing links; don't pull new issues.
        #[arg(long)]
        no_pull: bool,
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
    },

    /// Serialize-merge a queue of open PRs (the parallel-agent
    /// landing strip). Merges in PR-number order, one at a time, so
    /// N worktree PRs from one issue land without racing on `main`.
    /// Each merged PR closes its linked task (and that task's forge
    /// issue). On a merge that the forge rejects (e.g. now-conflicting
    /// after an earlier merge) the queue stops — fix the conflict and
    /// re-run — unless `--keep-going`.
    MergeQueue {
        #[arg(long)]
        repo: String,
        #[arg(long)]
        github: bool,
        #[arg(long)]
        base_url: Option<String>,
        /// `squash` (default), `merge`, or `rebase`.
        #[arg(long, default_value = "squash")]
        method: String,
        /// Only queue PRs linked to subtasks of this issue (UUID or
        /// 8-char prefix). Omit to queue every open PR on the repo.
        #[arg(long)]
        issue: Option<String>,
        /// Print the merge plan without merging anything.
        #[arg(long)]
        dry_run: bool,
        /// Keep merging the rest of the queue after a failed merge
        /// instead of stopping at the first conflict.
        #[arg(long)]
        keep_going: bool,
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
        /// Optional project (UUID, name, path, prefix) to stamp
        /// on pulled-in tasks.
        #[arg(long)]
        project: Option<String>,
        /// Filter by issue state: `open` (default), `closed`, or `all`.
        #[arg(long, default_value = "open")]
        state: String,
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
    /// branch off the current HEAD. With `--worktree`, the
    /// branch gets its own git worktree (separate directory) so
    /// multiple agents can work different subtasks of one issue
    /// in parallel without colliding on HEAD / the index.
    Start {
        /// Task id (UUID or 8-char prefix).
        id: String,
        /// Claim as this agent (`name[@version]`).
        #[arg(long = "as-agent")]
        as_agent: Option<String>,
        /// Branch prefix. Default `task`.
        #[arg(long, default_value = "task")]
        prefix: String,
        /// Create an isolated git worktree for the branch (under
        /// `.task-worktrees/`) instead of switching the current
        /// checkout. The key to parallel agents on one issue.
        #[arg(long)]
        worktree: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// List active `task code` worktrees (parallel work dirs).
    Worktrees {
        /// Emit `{branch, path}` rows as a JSON array.
        #[arg(long)]
        json: bool,
    },
    /// Remove the worktree for a task branch once it's merged
    /// (or to abandon it). Accepts the task short-id or branch.
    Cleanup {
        /// Task short-id (8 chars) or full branch name.
        id: String,
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
        /// Emit `{branch, task, links}` as JSON.
        #[arg(long)]
        json: bool,
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
        /// Emit the open handoffs as a JSON array.
        #[arg(long)]
        json: bool,
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
        /// Scope the label to one project (UUID). Omit for an
        /// org-wide label available across every project.
        #[arg(long)]
        project: Option<uuid::Uuid>,
        #[arg(long)]
        org: Option<String>,
    },
    /// List labels in the org. By default shows every label;
    /// `--project` narrows to that project's labels plus the
    /// org-wide ones.
    List {
        /// Only labels visible to this project (UUID): the
        /// project's own labels plus org-wide (unscoped) ones.
        #[arg(long)]
        project: Option<uuid::Uuid>,
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
    /// Put an agent in an autonomous loop toward a completion
    /// condition, the way Claude Code's `/goal` does — but
    /// agent-agnostic and persisted. Each iteration runs the worker
    /// command (one "turn"), then a separate evaluator judges whether
    /// the condition holds against what the worker surfaced. "Not
    /// met" loops with the evaluator's reason fed back as guidance;
    /// "met" stops. Bounded by `--max-iters`.
    ///
    /// The run is a `WorkSession` (workflows-orchestrator), so every
    /// turn is logged and the run is resumable. Distinct from the
    /// life-Goal/OKR system (`task goal`).
    ///
    /// Example:
    ///   task agent goal run "all tests in features/git pass" \
    ///     --cmd 'claude -p' --eval-cmd 'claude -p' --as-agent claude
    ///
    /// The standing session can be inspected (`goal status`), parked
    /// (`goal pause`), continued with a fresh turn budget
    /// (`goal resume`), or dropped (`goal clear`).
    Goal {
        #[command(subcommand)]
        cmd: GoalLoopCmd,
    },
}

/// `task agent goal *` — the autonomous goal loop and its lifecycle
/// verbs. The loop persists a `GoalSession` (condition, turn budget,
/// progress) over a `WorkSession`, so it can be parked and resumed
/// without losing the directive.
#[derive(Subcommand)]
enum GoalLoopCmd {
    /// Start (or restart) the loop toward a completion condition.
    Run {
        /// The completion condition. Write it so the worker's own
        /// output can demonstrate it (e.g. "`cargo test -p x` exits
        /// 0"). Up to a few KB.
        condition: String,
        /// Worker command, run via `sh -c` once per turn. The prompt
        /// (condition + last evaluator reason) is piped to its stdin;
        /// `TASK_GOAL` / `TASK_GOAL_ITER` are set in its env. Falls
        /// back to `TASK_AGENT_CMD`.
        #[arg(long)]
        cmd: Option<String>,
        /// Evaluator command, run via `sh -c` after each turn. Reads
        /// the condition + the worker's captured output on stdin;
        /// exit `0` = met (stop), nonzero = not met (its stdout is
        /// the reason, fed into the next turn). Falls back to
        /// `TASK_GOAL_EVAL_CMD`. If unset and `--task` is given, the
        /// built-in evaluator checks whether the task is `done`.
        #[arg(long)]
        eval_cmd: Option<String>,
        /// Tie the run to an existing task (UUID or 8-char prefix):
        /// claim it, make it the session subject, and (default
        /// evaluator) treat `status == done` as the condition.
        #[arg(long)]
        task: Option<String>,
        /// Attribute the loop to this agent (`name[@version]`).
        #[arg(long = "as-agent", default_value = "claude")]
        as_agent: String,
        /// Turn ceiling before parking the session as resumable.
        #[arg(long, default_value_t = 25)]
        max_iters: u32,
        /// Render + print the first prompt and exit — no worker,
        /// no evaluator, no state change.
        #[arg(long)]
        dry_run: bool,
        /// Skip auto-triage. By default a `--task` with no subtasks
        /// is first decomposed into agent-sized subtasks (one
        /// decompose turn by the worker) before the loop executes;
        /// this works it as a single task instead.
        #[arg(long)]
        no_triage: bool,
        /// Hand the whole goal to the agent's own native goal loop in
        /// one shot, instead of our turn-by-turn loop. For agents with
        /// their own loop (Hermes/Codex/Claude). Also settable via
        /// `agent.json` `goal.mode = "delegate"`.
        #[arg(long)]
        delegate: bool,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Show the active goal session: condition, turns used/budget,
    /// the last evaluator reason, and the session status.
    Status {
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Park the active goal session (stop auto-continuation) without
    /// dropping it. Resume later with `goal resume`.
    Pause {
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Resume the parked goal session: reset the turn counter to 0
    /// and continue the loop toward the stored condition.
    Resume {
        /// Worker command override (else env / per-org config).
        #[arg(long)]
        cmd: Option<String>,
        /// Evaluator command override (else env / per-org config).
        #[arg(long)]
        eval_cmd: Option<String>,
        /// Attribute the resumed loop to this agent (`name[@version]`).
        #[arg(long = "as-agent", default_value = "claude")]
        as_agent: String,
        /// Turn ceiling for the resumed run. Defaults to the stored
        /// budget from the original `goal run`.
        #[arg(long)]
        max_iters: Option<u32>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Drop the active goal session (cancel it + delete its row).
    Clear {
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Steer a running loop: replace the active session's completion
    /// condition. A loop in another process re-reads it at the top of
    /// its next turn and re-steers — no restart needed.
    Update {
        /// The new completion condition.
        #[arg(long)]
        condition: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
    },
    /// Manage the active session's subgoals — extra acceptance
    /// criteria the worker sees and the judge must also satisfy.
    ///
    ///   goal subgoal `"<text>"`   append a criterion
    ///   goal subgoal            list them (alias: `goal subgoal list`)
    ///   goal subgoal remove `<N>` drop the Nth (1-based)
    ///   goal subgoal clear      drop all
    ///
    /// A running loop in another process folds the current set into
    /// its next worker prompt and evaluator gate — no restart needed.
    Subgoal {
        /// The subverb + text. With no args: list. A bare string:
        /// append it as a criterion. `remove <N>`: drop the Nth.
        /// `clear`: drop all. `list`: list.
        #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
        args: Vec<String>,
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
        /// Emit the resulting milestone as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Set or clear (`none`) the due date.
    SetDue {
        target: String,
        due: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Emit the resulting milestone as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Set or clear (`none`) the life-goal link.
    SetGoal {
        target: String,
        goal: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Emit the resulting milestone as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Set or clear (`none`) the forge sync ref.
    SetForgeRef {
        target: String,
        forge_ref: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Emit the resulting milestone as JSON.
        #[arg(long)]
        json: bool,
    },
    /// `closed`. Just `set-status <target> closed`.
    Close {
        target: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Emit the resulting milestone as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Reopen (status = open).
    Reopen {
        target: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Emit the resulting milestone as JSON.
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
        /// Emit the renamed milestone as JSON.
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

/// Global `--org` / `--server` flags, captured once before dispatch.
/// Subcommands that still declare local duplicates shadow these; the
/// shared resolvers ([`resolve_active_org`], [`resolve_org_vox_url`],
/// `org_ctx::resolve_active`) fall back here when a handler passes
/// `None`, so `task --org foo <any subcommand>` works even where the
/// local flag was removed (issue / threads) or never existed.
static GLOBAL_ORG: std::sync::OnceLock<Option<String>> = std::sync::OnceLock::new();
static GLOBAL_SERVER: std::sync::OnceLock<Option<String>> = std::sync::OnceLock::new();

pub(crate) fn global_org() -> Option<String> {
    GLOBAL_ORG.get().cloned().flatten()
}

fn global_server() -> Option<String> {
    GLOBAL_SERVER.get().cloned().flatten()
}

#[tokio::main]
async fn main() {
    // wss:// (vox-websocket TLS) needs a process-level rustls
    // CryptoProvider; this binary unifies both `ring` and
    // `aws-lc-rs` in its graph, so rustls cannot infer one.
    // Install ring once, before anything can open a TLS socket.
    // Err just means a provider is already installed — fine.
    let _ = rustls::crypto::ring::default_provider().install_default();
    // Best-effort .env load before clap reads env. Missing file is
    // not an error — we just fall through to the hard-coded default.
    let _ = dotenvy::dotenv();
    let cli = Cli::parse();
    GLOBAL_ORG.set(cli.org.clone()).ok();
    GLOBAL_SERVER.set(cli.server.clone()).ok();
    // Error boundary: render the taxonomy line + hint and exit with
    // the stable code (4 not-found / 5 conflict / 6 connection / 1).
    if let Err(report) = run(cli).await {
        errors::exit_with(&report);
    }
}

/// The proto/server skew guard half of `task doctor`: fetch the
/// server's `/.well-known/task-server.json`, compare its
/// `schema_stamps` (computed from the descriptors the *running*
/// binary mounts) against this CLI's own build (the CLI links
/// `task_server::schema_stamps()` directly, so both sides fold
/// the exact same descriptor list — no second list to drift).
///
/// A mismatch means the running task-server predates (or
/// postdates) a `*-proto` change relative to this CLI — the
/// state that otherwise surfaces as vox `structural mismatch` /
/// `InvalidPayload` / `Unknown method` errors with zero context.
/// Exits non-zero so dev scripts can gate on it.
async fn doctor_check_schema(ws_url: &str) -> eyre::Result<()> {
    // ws(s)://host:port[/path] → http(s)://host:port/.well-known/…
    let origin = {
        let http = ws_url
            .replacen("wss://", "https://", 1)
            .replacen("ws://", "http://", 1);
        let after_scheme = http.find("://").map_or(http.len(), |i| i + 3);
        let end = http[after_scheme..]
            .find('/')
            .map_or(http.len(), |i| after_scheme + i);
        http[..end].to_owned()
    };
    let url = format!("{origin}/.well-known/task-server.json");

    let doc: serde_json::Value = match reqwest::get(&url).await {
        Ok(resp) => resp
            .json()
            .await
            .map_err(|e| eyre::eyre!("parse {url}: {e}"))?,
        Err(e) => {
            println!("Schema check: SKIPPED — could not fetch {url} ({e})");
            return Ok(());
        }
    };
    let Some(served) = doc.get("schema_stamps").and_then(|v| v.as_object()) else {
        println!(
            "Schema check: UNVERIFIED — the server exposes no `schema_stamps` \
             (it predates the skew guard). If you see `structural mismatch` / \
             `InvalidPayload` errors, rebuild + restart task-server."
        );
        return Ok(());
    };

    let local = task_server::schema_stamps();
    let mut stale: Vec<&str> = Vec::new();
    let mut unserved: Vec<&str> = Vec::new();
    for (name, stamp) in &local {
        match served.get(*name).and_then(|v| v.as_str()) {
            Some(s) if s == stamp => {}
            Some(_) => stale.push(name),
            None => unserved.push(name),
        }
    }

    if stale.is_empty() && unserved.is_empty() {
        println!(
            "Schema check: OK — {} service stamps match the running server",
            local.len()
        );
        return Ok(());
    }
    if !unserved.is_empty() {
        println!(
            "Schema check: {} service(s) not stamped by the server (added since \
             its build?): {}",
            unserved.len(),
            unserved.join(", ")
        );
    }
    if !stale.is_empty() {
        println!(
            "Schema check: STALE — stamp mismatch on: {}",
            stale.join(", ")
        );
        println!(
            "  The running task-server was built against different `*-proto` \
             shapes than this CLI."
        );
        println!(
            "  Fix: rebuild + restart it (`cargo run -p task-server`), or rebuild \
             this CLI if the server is newer."
        );
        return Err(eyre::eyre!(
            "proto/server schema skew on {} service(s)",
            stale.len()
        ));
    }
    Ok(())
}

async fn run(cli: Cli) -> eyre::Result<()> {
    match cli.command {
        Commands::Doctor => {
            let server = cli
                .server
                .unwrap_or_else(|| "ws://127.0.0.1:9090/vox".to_owned());
            let remote =
                RemoteVoxConfig::from_args(server.clone(), cli.session_token, cli.organization_id)?;
            println!("Vox endpoint: {}", remote.display_url);
            doctor_check_schema(&server).await?;
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
        Commands::Admin(cmd) => {
            return Box::pin(run_admin(cmd)).await;
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
        Commands::Workstream(cmd) => {
            return Box::pin(workstream::run_workstream(cmd)).await;
        }
        Commands::Collection(cmd) => {
            return Box::pin(collection::run_collection(cmd)).await;
        }
        Commands::Song(cmd) => {
            return Box::pin(collection::run_song(cmd)).await;
        }
        Commands::Media(cmd) => {
            return Box::pin(media::run_media(cmd)).await;
        }
        Commands::Location(cmd) => {
            return Box::pin(run_location(cmd)).await;
        }
        Commands::Inbox(cmd) => {
            return Box::pin(run_inbox(cmd)).await;
        }
        Commands::Threads(cmd) => {
            return Box::pin(run_threads(cmd)).await;
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
        Commands::Shopping(cmd) => {
            return Box::pin(mealprep::run_shopping(cmd)).await;
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
        Commands::Plan(cmd) => {
            return Box::pin(plan::run_plan(cmd)).await;
        }
        Commands::Next(args) => {
            return Box::pin(plan::run_next(args)).await;
        }
        Commands::Brief(args) => {
            return Box::pin(brief::run_brief(args)).await;
        }
    }
    Ok(())
}

/// Resolve the per-org vox URL from CLI flags + env + session.
/// Mirror of the helper inside `run_vault_sync`, lifted out
/// because project + goal share the same routing surface.
fn resolve_org_vox_url(server: Option<String>, org_slug: &str) -> String {
    let base = resolve_server_base(server.as_deref());
    format!("{base}/org/{org_slug}/vox")
}

/// Which server should this invocation talk to? Precedence:
///
/// 1. explicit `--server` (clap; the flag beats its env binding)
/// 2. `TASK_VOX_URL` env (folded into the global flag by clap)
/// 3. the active session's stored server URL (`task auth login`
///    against a remote records where it signed in, so subsequent
///    commands need nothing but the session)
/// 4. the localhost default
///
/// Returns a normalized vox base (`ws(s)://host[:port]`, no
/// trailing `/vox`).
fn resolve_server_base(explicit: Option<&str>) -> String {
    let flag_or_env = explicit
        .map(str::to_owned)
        .or_else(global_server)
        .or_else(|| std::env::var("TASK_VOX_URL").ok())
        .filter(|u| !u.trim().is_empty());
    // Only consult the session file when nothing explicit is set —
    // keeps the hot path off the filesystem.
    let session_url = if flag_or_env.is_some() {
        None
    } else {
        session_store::load()
            .ok()
            .flatten()
            .and_then(|s| s.active_server().map(|e| e.url.clone()))
    };
    pick_server_base(flag_or_env.as_deref(), session_url.as_deref())
}

/// HTTP(S) base for the server's plain HTTP routes (`/blobs/*`),
/// derived from the resolved vox base (`ws→http`, `wss→https`).
fn resolve_server_http_base(explicit: Option<&str>) -> String {
    let base = resolve_server_base(explicit);
    if let Some(rest) = base.strip_prefix("wss://") {
        format!("https://{rest}")
    } else if let Some(rest) = base.strip_prefix("ws://") {
        format!("http://{rest}")
    } else {
        base
    }
}

/// Pure core of [`resolve_server_base`] — unit-testable precedence
/// fold. `flag_or_env` is `--server`/`TASK_VOX_URL` (already
/// flag-over-env, courtesy of clap), `session_url` the active
/// session entry's stored server.
fn pick_server_base(flag_or_env: Option<&str>, session_url: Option<&str>) -> String {
    if let Some(u) = flag_or_env.filter(|u| !u.trim().is_empty()) {
        return session_store::normalize_server_base(u);
    }
    if let Some(u) = session_url.filter(|u| !u.trim().is_empty()) {
        return session_store::normalize_server_base(u);
    }
    session_store::DEFAULT_LOCAL_VOX.to_owned()
}

#[cfg(test)]
mod server_resolution_tests {
    use super::*;

    #[test]
    fn flag_or_env_beats_session() {
        assert_eq!(
            pick_server_base(
                Some("wss://task.starcommand.live/vox"),
                Some("ws://127.0.0.1:18080")
            ),
            "wss://task.starcommand.live"
        );
        // …and the flip: env pointing local wins over a stored
        // remote session — the URL switch IS the selector.
        assert_eq!(
            pick_server_base(
                Some("ws://127.0.0.1:18080/vox"),
                Some("wss://task.starcommand.live")
            ),
            "ws://127.0.0.1:18080"
        );
    }

    #[test]
    fn session_beats_default() {
        assert_eq!(
            pick_server_base(None, Some("wss://task.starcommand.live/vox")),
            "wss://task.starcommand.live"
        );
        // Legacy "local" session entries resolve to the default.
        assert_eq!(
            pick_server_base(None, Some("local")),
            session_store::DEFAULT_LOCAL_VOX
        );
    }

    #[test]
    fn default_when_nothing_set() {
        assert_eq!(
            pick_server_base(None, None),
            session_store::DEFAULT_LOCAL_VOX
        );
        // Blank values don't shadow lower-precedence sources.
        assert_eq!(
            pick_server_base(Some(""), Some(" ")),
            session_store::DEFAULT_LOCAL_VOX
        );
    }

    #[test]
    fn org_url_appends_per_org_path() {
        // resolve_org_vox_url rides the same fold; with an
        // explicit server the env/session never enter.
        assert_eq!(
            resolve_org_vox_url(Some("wss://task.starcommand.live/vox".into()), "codywright"),
            "wss://task.starcommand.live/org/codywright/vox"
        );
    }

    #[test]
    fn ws_http_derivation() {
        assert_eq!(
            ws_base_to_http("wss://task.starcommand.live"),
            "https://task.starcommand.live"
        );
        assert_eq!(
            ws_base_to_http("ws://127.0.0.1:18080"),
            "http://127.0.0.1:18080"
        );
    }
}

/// Embedded backend, built once per process: a full `AppState` plus the
/// construction `Scope` that keeps its in-process vox acceptor tasks
/// alive. Only initialized when embedded mode is active.
struct Embedded {
    state: task_server::AppState,
    scope: std::sync::Arc<architect::Scope>,
}

static EMBEDDED: tokio::sync::OnceCell<Embedded> = tokio::sync::OnceCell::const_new();

/// True when the CLI should host the backend in-process instead of
/// talking to a running `task-server`. Opt-in via `TASK_EMBED`.
fn embed_enabled() -> bool {
    std::env::var("TASK_EMBED").is_ok_and(|v| matches!(v.as_str(), "1" | "true" | "yes"))
}

/// Lazily build (once) and return the embedded backend.
async fn embedded() -> eyre::Result<&'static Embedded> {
    EMBEDDED
        .get_or_try_init(|| async {
            let scope = architect::Scope::new();
            let state = task_server::AppState::new(None)
                .await
                .map_err(|e| eyre::eyre!("embedded backend boot: {e}"))?;
            Ok::<_, eyre::Report>(Embedded { state, scope })
        })
        .await
}

/// Establish a typed service client over the active transport: an
/// in-process `LocalServer` when embedded (`TASK_EMBED`), otherwise a
/// vox WebSocket to the resolved per-org URL. Same client type either
/// way — architect's "inject remote vs local, one client".
async fn establish_client<C>(server: Option<String>, slug: &str) -> eyre::Result<C>
where
    C: vox_core::FromVoxLane,
{
    if embed_enabled() {
        establish_embedded(slug).await
    } else {
        let url = resolve_org_vox_url(server, slug);
        Box::pin(vox::connect_lane(&url).establish())
            .await
            .map_err(|e| connect_error(&url, &e))
    }
}

/// Tag a vox connect/establish failure with the `Connection` exit
/// class (6) and a "how do I point this somewhere else" hint.
fn connect_error<E: std::fmt::Debug>(url: &str, e: &E) -> eyre::Report {
    errors::connection(format!("connect `{url}`"))
        .cause(format!("{e:?}"))
        .hint("is task-server running? point the CLI elsewhere with --server or TASK_VOX_URL")
        .report()
}

/// Establish a typed client given an already-resolved per-org vox URL
/// (`…/org/<slug>/vox`). In embedded mode the slug is recovered from the
/// URL and served in-process; otherwise it's a plain WebSocket connect.
/// The choke point for the `connect_*_client` helpers, which only carry
/// a URL.
async fn establish_for_url<C>(url: &str) -> eyre::Result<C>
where
    C: vox_core::FromVoxLane,
{
    if embed_enabled() {
        let slug = url
            .rsplit_once("/org/")
            .and_then(|(_, rest)| rest.strip_suffix("/vox"))
            .ok_or_else(|| {
                eyre::eyre!("can't recover an org slug from `{url}` for embedded mode")
            })?;
        establish_embedded(slug).await
    } else {
        Box::pin(vox::connect_lane(url).establish())
            .await
            .map_err(|e| connect_error(url, &e))
    }
}

/// Establish a typed client against the **server-management** endpoint
/// (`/server/vox` — `OrgManagementService` / `SnapshotService`). The
/// server-level counterpart of [`establish_client`]: no per-org slug.
/// Embedded (`TASK_EMBED`) serves the same router in-process via
/// [`task_server::AppState::server_local_server`]; otherwise it's a
/// WebSocket to the resolved server URL. Returns the client plus the
/// endpoint label for user-facing messages (`(embedded)` in-process).
async fn establish_server_client<C>(server: Option<&str>) -> eyre::Result<(C, String)>
where
    C: vox_core::FromVoxLane,
{
    if embed_enabled() {
        let emb = embedded().await?;
        let client = emb
            .state
            .server_local_server(&emb.scope)
            .establish()
            .await
            .map_err(|e| eyre::eyre!("embedded /server/vox establish: {e:?}"))?;
        Ok((client, "(embedded)".into()))
    } else {
        let url = resolve_server_vox_url(server)?;
        let client = Box::pin(vox::connect_lane(&url).establish())
            .await
            .map_err(|e| connect_error(&url, &e))?;
        Ok((client, url))
    }
}

/// Establish a typed client against the in-process [`LocalServer`] for
/// `slug`. Shared by [`establish_client`] and [`establish_for_url`].
async fn establish_embedded<C>(slug: &str) -> eyre::Result<C>
where
    C: vox_core::FromVoxLane,
{
    let emb = embedded().await?;
    emb.state
        .local_server(slug, &emb.scope)
        .ok_or_else(|| eyre::eyre!("org `{slug}` not hosted in embedded mode"))?
        .establish()
        .await
        .map_err(|e| eyre::eyre!("embedded establish for `{slug}`: {e:?}"))
}

/// Resolve the active org slug from `--org` flag or the
/// stored session. Returns a friendly error if neither
/// resolves.
///
/// Server-aware: when `--server` / `TASK_VOX_URL` targets a
/// specific server, the session entry FOR THAT SERVER supplies the
/// slug — switching the URL between the local dev server and a
/// remote deployment flips to the matching signed-in session
/// automatically, even though `active` still points elsewhere.
fn resolve_active_org(override_slug: Option<String>) -> eyre::Result<String> {
    if let Some(s) = override_slug.or_else(global_org) {
        return Ok(s);
    }
    let no_session = || {
        errors::usage("resolve active org")
            .cause("no org selected and no stored session")
            .hint("pass --org <slug> or run `task auth login` first")
            .report()
    };
    let sess = session_store::load()?.ok_or_else(no_session)?;
    if let Some(target) = global_server().or_else(|| std::env::var("TASK_VOX_URL").ok()) {
        if !target.trim().is_empty() {
            if let Some((_, entry)) = sess.entry_for_server(&target) {
                return Ok(entry.slug.clone());
            }
        }
    }
    let slug = sess.active_slug();
    if slug.is_empty() {
        return Err(no_session());
    }
    Ok(slug)
}

/// Per-project state registries: project id → its optional
/// `states:` config. Best-effort (an unreachable project service
/// degrades to the default registry everywhere).
async fn project_states_map(
    url: &str,
) -> std::collections::HashMap<uuid::Uuid, Option<::project::StatesConfig>> {
    match connect_project_client(url).await {
        Ok(pc) => pc
            .list()
            .await
            .map(|ps| ps.into_iter().map(|p| (p.id, p.states)).collect())
            .unwrap_or_default(),
        Err(_) => std::collections::HashMap::new(),
    }
}

/// Classify one task's status via its owning project's state
/// registry (default registry when project unknown / unset).
fn resolve_task_group(
    states: &std::collections::HashMap<uuid::Uuid, Option<::project::StatesConfig>>,
    t: &task::TaskInfo,
) -> ::project::StateGroup {
    let cfg = t
        .project_id
        .and_then(|pid| states.get(&pid))
        .and_then(Option::as_ref);
    ::project::resolve_state_group(cfg, &t.status)
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

// ── Wiki RPC handlers ────────────────────────────────────────────────

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
        AgentCmd::Goal { cmd } => match cmd {
            GoalLoopCmd::Run {
                condition,
                cmd,
                eval_cmd,
                task,
                as_agent,
                max_iters,
                dry_run,
                no_triage,
                delegate,
                org,
                server,
            } => {
                Box::pin(run_agent_goal(
                    condition, cmd, eval_cmd, task, as_agent, max_iters, dry_run, no_triage,
                    delegate, org, server,
                ))
                .await
            }
            GoalLoopCmd::Status { org, server } => Box::pin(run_goal_status(org, server)).await,
            GoalLoopCmd::Pause { org, server } => Box::pin(run_goal_pause(org, server)).await,
            GoalLoopCmd::Resume {
                cmd,
                eval_cmd,
                as_agent,
                max_iters,
                org,
                server,
            } => {
                Box::pin(run_goal_resume(
                    cmd, eval_cmd, as_agent, max_iters, org, server,
                ))
                .await
            }
            GoalLoopCmd::Clear { org, server } => Box::pin(run_goal_clear(org, server)).await,
            GoalLoopCmd::Update {
                condition,
                org,
                server,
            } => Box::pin(run_goal_update(condition, org, server)).await,
            GoalLoopCmd::Subgoal { args, org, server } => {
                Box::pin(run_goal_subgoal(args, org, server)).await
            }
        },
    }
}

/// `task agent goal` — the autonomous goal loop (worker turn +
/// evaluator gate, looped until the condition is met). See [`AgentCmd::Goal`].
#[allow(clippy::too_many_arguments)]
async fn run_agent_goal(
    condition: String,
    cmd: Option<String>,
    eval_cmd: Option<String>,
    task_ref: Option<String>,
    as_agent: String,
    max_iters: u32,
    dry_run: bool,
    no_triage: bool,
    delegate: bool,
    org: Option<String>,
    server: Option<String>,
) -> eyre::Result<()> {
    use workflows_orchestrator::{CodingWorkflow, WorkflowStore};

    let slug = resolve_active_org(org)?;
    let agent = parse_agent_ref(&format!("agent:{as_agent}"))?;
    let url = resolve_org_vox_url(server.clone(), &slug);

    // Resolve the optional linked task (claim it; it becomes the
    // session subject + the default evaluator's completion check).
    let (task_id, task_info, parent_info) = match &task_ref {
        Some(r) => {
            let client = connect_task_client(&url).await?;
            let t = resolve_issue_id(&client, r).await?;
            if let ClaimOutcome::Lost(holder) = try_claim(&client, &t.id, &agent, false).await? {
                return Err(eyre::eyre!("{} is held by {holder}", short_uuid(&t.id)));
            }
            let parent = match t.workflow.as_ref().and_then(|w| w.parent) {
                Some(pid) => client.get(pid).await.ok(),
                None => None,
            };
            (Some(t.id), Some(t), parent)
        }
        None => (None, None, None),
    };

    // Resolve the worker + evaluator commands. Precedence: explicit
    // flag → env var → per-org `agent.json` [goal] defaults. The
    // config layer (mirroring Hermes's `config.yaml: goals/auxiliary`)
    // lets an org set its agent once instead of passing --cmd every
    // run — the executor seam. The command itself is agent-agnostic:
    // `claude -p`, `codex exec`, `hermes -p`, etc. all satisfy the
    // stdin-prompt contract.
    let cfg = load_goal_config(&slug);
    let worker = cmd
        .or_else(|| std::env::var("TASK_AGENT_CMD").ok())
        .or(cfg.worker_cmd);
    let evaluator = eval_cmd
        .or_else(|| std::env::var("TASK_GOAL_EVAL_CMD").ok())
        .or(cfg.eval_cmd);

    // Auto-triage (Hermes kanban-orchestrator style: "decompose,
    // don't execute"). A linked task with no subtasks gets one
    // decompose turn — the worker breaks the PRD into agent-sized
    // titles, or judges it a one-shot and emits none (we don't
    // fragment small tasks). Skipped on --no-triage / --dry-run.
    let mut subtasks_md = String::new();
    if let Some(t) = &task_info {
        let client = connect_task_client(&url).await?;
        let mut children = subtasks_of(&client, t.id).await?;
        if children.is_empty() && !no_triage && !dry_run {
            let w = worker.as_deref().ok_or_else(|| {
                eyre::eyre!(
                    "auto-triage needs a worker: pass --cmd / set TASK_AGENT_CMD / goal.worker_cmd, or --no-triage"
                )
            })?;
            let dprompt = decompose_prompt(&render_task_prompt(t, parent_info.as_ref()));
            println!("⊙ auto-triage: decomposing {} …", short_uuid(&t.id));
            let out = run_subprocess(w, &dprompt, 0, &condition)?;
            let titles = parse_subtask_titles(&out.stdout);
            if titles.is_empty() {
                println!("  one-shot task — no subtasks created");
            } else {
                let mut made = 0usize;
                for title in &titles {
                    if create_subtask(&client, t, title).await? {
                        made += 1;
                        println!("    + {title}");
                    } else {
                        println!("    ~ {title} (already exists — skipped)");
                    }
                }
                if made > 0 {
                    // Flip the parent into the working state.
                    let mut p = t.clone();
                    p.status = "in-progress".into();
                    p.completed_date = None;
                    let _ = client.update(p).await;
                }
                println!("  created {made}/{} subtask(s)", titles.len());
                children = subtasks_of(&client, t.id).await?;
            }
        }
        subtasks_md = render_subtask_checklist(&children);
    }

    // The first prompt: the condition is the directive (matching
    // Claude Code's `/goal`, where the condition itself starts the
    // turn). When tied to a task, lead with its rendered PRD + the
    // subtask checklist. Subsequent turns append the evaluator reason.
    let preamble = task_info
        .as_ref()
        .map(|t| render_task_prompt(t, parent_info.as_ref()))
        .unwrap_or_default();
    // Static preamble (task PRD + subtask checklist, or empty) — the
    // part of the prompt that doesn't change turn to turn. The
    // condition is read live from the store each turn so `goal update`
    // can steer the loop, so it's not baked in here.
    let static_preamble = if preamble.is_empty() {
        String::new()
    } else {
        format!("{preamble}{subtasks_md}")
    };
    if dry_run {
        // No session yet at dry-run time, so no subgoals to fold in.
        println!("{}", goal_prompt(&static_preamble, &condition, &[], ""));
        return Ok(());
    }

    let worker = worker.ok_or_else(|| {
        eyre::eyre!(
            "no worker command: pass --cmd, set TASK_AGENT_CMD, or set goal.worker_cmd in ~/.task/orgs/{slug}/agent.json"
        )
    })?;
    // Native-delegate uses the worker's exit code as the verdict, so
    // it needs no separate evaluator.
    let delegate = delegate || cfg.mode.as_deref() == Some("delegate");
    if !delegate && evaluator.is_none() && task_id.is_none() {
        return Err(eyre::eyre!(
            "no evaluator: pass --eval-cmd, set TASK_GOAL_EVAL_CMD / goal.eval_cmd, or pass --task for the built-in done-check"
        ));
    }

    // Open the work session (subject = the task if linked, else a
    // custom "goal" subject keyed by a fresh id).
    let store_dir = org_workflows_dir(&slug)?;
    let wf = CodingWorkflow::new(WorkflowStore::open(store_dir));
    let subject_task = task_id.unwrap_or_else(uuid::Uuid::new_v4);
    let session = wf.start(subject_task, agent.clone())?;
    println!(
        "goal session {} — “{condition}” (max {max_iters} turns)",
        short_uuid(&session.id)
    );

    // Persist the goal-loop state (condition, budget, progress) so it
    // can be inspected (`goal status`), parked (`goal pause`), and
    // resumed (`goal resume`) independently of this process.
    wf.store().put_goal(&workflows_proto::GoalSession::new(
        session.id, &condition, max_iters,
    ))?;

    // Pick the executor: --delegate (or agent.json goal.mode) hands
    // the whole goal to the agent's native loop; default is our
    // turn-by-turn subprocess loop. (`delegate` resolved above.)
    let executor = if delegate {
        GoalExecutor::NativeDelegate { worker: &worker }
    } else {
        GoalExecutor::Subprocess {
            worker: &worker,
            evaluator: evaluator.as_deref(),
        }
    };
    let run = executor.drive(&wf, session.id, &agent, &static_preamble, max_iters)?;

    finalize_goal_run(&run, max_iters, task_id, &url, &wf, session.id).await
}

/// Drive the worker/evaluator loop for one goal session, persisting
/// per-turn progress (`turns_used`, `last_reason`) onto the
/// [`GoalSession`](workflows_proto::GoalSession) row so a concurrent
/// `goal status` reads live state. Shared by `goal run` and
/// `goal resume`.
#[allow(clippy::too_many_arguments)]
/// Assemble a turn's worker prompt from the static preamble (the task
/// PRD with its subtask checklist, or empty), the live completion
/// condition, and the evaluator's last reason. Kept in one place so
/// `goal run`, `goal resume`, the per-turn loop, and `--dry-run` agree.
fn goal_prompt(
    static_preamble: &str,
    condition: &str,
    subgoals: &[String],
    reason: &str,
) -> String {
    let mut p = if static_preamble.is_empty() {
        format!(
            "Goal: {condition}\n\nWork toward this goal. When you believe it is fully met, stop."
        )
    } else {
        format!(
            "{static_preamble}\n---\n\nGoal (stop when met): {condition}\n\nWork toward this goal using the task spec above; complete the subtasks in order."
        )
    };
    // Subgoals are extra acceptance criteria added mid-run — the
    // worker must satisfy every one in addition to the condition.
    if !subgoals.is_empty() {
        p.push_str("\n\nAdditional acceptance criteria (ALL must also be met):");
        for (i, sg) in subgoals.iter().enumerate() {
            p.push_str(&format!("\n  {}. {sg}", i + 1));
        }
    }
    if !reason.is_empty() {
        p.push_str(&format!(
            "\n\nThe goal is NOT yet met. Evaluator feedback:\n{reason}\n\nContinue."
        ));
    }
    p
}

fn drive_goal_loop(
    wf: &workflows_orchestrator::CodingWorkflow,
    session_id: uuid::Uuid,
    agent: &workflows_proto::AgentRef,
    static_preamble: &str,
    worker: &str,
    evaluator: Option<&str>,
    budget: u32,
) -> eyre::Result<workflows_orchestrator::SessionRun> {
    use workflows_orchestrator::IterationOutcome;

    // The evaluator's latest reason, carried into the next worker turn.
    let last_reason = std::cell::RefCell::new(String::new());
    // The condition seen on the previous turn — to detect live edits.
    let last_condition = std::cell::RefCell::new(String::new());

    let run = wf.run_session(session_id, agent.clone(), budget, |iter| {
        // Re-read the condition + subgoals from the store every turn
        // so an out-of-band `goal update` / `goal subgoal` steers the
        // loop in real time (#174). The store is the steering channel.
        let (condition, subgoals) = wf
            .store()
            .goal(session_id)
            .map(|g| (g.condition, g.subgoals.0))
            .unwrap_or_default();
        {
            let mut lc = last_condition.borrow_mut();
            if !lc.is_empty() && *lc != condition {
                println!("  ◎ goal updated — re-steering toward: {condition}");
            }
            *lc = condition.clone();
        }

        // 1. Worker turn — prompt assembled from the static preamble
        //    + the (possibly just-updated) condition + last reason.
        let reason = last_reason.borrow().clone();
        let prompt = goal_prompt(static_preamble, &condition, &subgoals, &reason);
        println!("▶ turn {iter} — worker running…");
        let work = run_subprocess(worker, &prompt, iter, &condition)
            .map_err(|e| workflows_proto::WorkflowError::Backend(format!("worker: {e}")))?;

        // 2. Evaluator gate — judge the worker's output against the
        //    current condition. Built-in done-check if no eval command.
        let verdict = match evaluator {
            Some(ev) => {
                println!("⧖ turn {iter} — judging…");
                // Fold any subgoals into the condition block so the
                // judge must verify them too — they're extra acceptance
                // criteria, equal in weight to the condition.
                let criteria = if subgoals.is_empty() {
                    condition.clone()
                } else {
                    let mut c = condition.clone();
                    c.push_str("\n\nALSO required (every one must be met):");
                    for (i, sg) in subgoals.iter().enumerate() {
                        c.push_str(&format!("\n  {}. {sg}", i + 1));
                    }
                    c
                };
                // Lead with the strict judge directive so a raw LLM
                // evaluator (`claude -p`, `hermes -p`) emits the
                // verdict instead of prose — otherwise parsing fails
                // and we'd fall back to "exit 0 = always met". The
                // conservative framing mirrors Hermes/Claude /goal.
                let eval_in = format!(
                    "You are a STRICT completion judge. Reply with ONLY a JSON object: \
                     {{\"done\": true|false, \"reason\": \"<one sentence>\"}}. Mark done:true \
                     ONLY if the WORKER OUTPUT shows the CONDITION is fully and verifiably met; \
                     when in doubt, done:false with the gap as the reason.\n\n\
                     CONDITION:\n{criteria}\n\nWORKER OUTPUT:\n{}",
                    work.stdout
                );
                let r = run_subprocess(ev, &eval_in, iter, &condition)
                    .map_err(|e| workflows_proto::WorkflowError::Backend(format!("eval: {e}")))?;
                // Prefer a structured verdict — the judge convention
                // Hermes / Claude / Codex `/goal` all share:
                // `{"done": bool, "reason": "..."}` on stdout. This is
                // the cross-agent interop contract. Fall back to the
                // exit code when the output isn't that JSON.
                parse_eval_verdict(&r.stdout, r.code)
            }
            None => {
                // No evaluator + a linked task: the condition is
                // "task is done". Checked synchronously below by the
                // outer loop via a flag; here we approximate using
                // the worker exit code as a hint and defer the
                // authoritative check to the post-run reconcile.
                EvalVerdict {
                    met: work.code == 0,
                    reason: "worker did not exit 0".to_owned(),
                }
            }
        };

        // Persist progress so `goal status` reflects this turn. Atomic
        // read-modify-write under the session lock so a concurrent
        // `goal update`/`subgoal` (live steering) can't lose-update
        // against this write.
        let activity = work.last_activity.clone();
        let _ = wf.store().mutate_goal(session_id, |g| {
            g.turns_used = iter + 1;
            g.last_reason = verdict.reason.clone();
            if let Some(a) = &activity {
                g.current_activity = a.clone();
            }
            g.updated_at = chrono::Utc::now();
        });

        if verdict.met {
            Ok(IterationOutcome::Done)
        } else {
            *last_reason.borrow_mut() = verdict.reason.clone();
            if verdict.reason.is_empty() {
                println!("  ◎ turn {iter}: not met yet");
            } else {
                println!("  ◎ turn {iter}: not met — {}", verdict.reason);
            }
            Ok(IterationOutcome::Continue)
        }
    })?;
    Ok(run)
}

/// The two ways to drive a goal session. Selected by `--delegate` /
/// `agent.json goal.mode`. This is the seam #161 deferred —
/// concretised now that a second real executor exists.
enum GoalExecutor<'a> {
    /// Default: our turn-by-turn loop — worker turn → judge → repeat,
    /// with live steering + the budget. Agent-agnostic.
    Subprocess {
        worker: &'a str,
        evaluator: Option<&'a str>,
    },
    /// Hand the whole goal to the agent's *own* loop in one shot (for
    /// agents that have a native goal loop — Hermes/Codex/Claude). One
    /// worker invocation with the full directive; its exit code is the
    /// verdict. No re-prompt, no separate judge.
    NativeDelegate { worker: &'a str },
}

impl GoalExecutor<'_> {
    fn drive(
        &self,
        wf: &workflows_orchestrator::CodingWorkflow,
        session_id: uuid::Uuid,
        agent: &workflows_proto::AgentRef,
        static_preamble: &str,
        budget: u32,
    ) -> eyre::Result<workflows_orchestrator::SessionRun> {
        match self {
            GoalExecutor::Subprocess { worker, evaluator } => drive_goal_loop(
                wf,
                session_id,
                agent,
                static_preamble,
                worker,
                *evaluator,
                budget,
            ),
            GoalExecutor::NativeDelegate { worker } => {
                drive_goal_delegate(wf, session_id, agent, static_preamble, worker)
            }
        }
    }
}

/// Native-delegate executor (#169): one worker invocation handed the
/// full goal (the agent runs its *own* loop internally), mapped onto a
/// single-turn `WorkSession`. Exit 0 = met → finish; nonzero → park.
fn drive_goal_delegate(
    wf: &workflows_orchestrator::CodingWorkflow,
    session_id: uuid::Uuid,
    agent: &workflows_proto::AgentRef,
    static_preamble: &str,
    worker: &str,
) -> eyre::Result<workflows_orchestrator::SessionRun> {
    use workflows_orchestrator::IterationOutcome;
    // Budget of 1: a single delegated invocation. The agent's native
    // loop does the iterating; we just record the one outcome.
    let run = wf.run_session(session_id, agent.clone(), 1, |_iter| {
        let (condition, subgoals) = wf
            .store()
            .goal(session_id)
            .map(|g| (g.condition, g.subgoals.0))
            .unwrap_or_default();
        let prompt = goal_prompt(static_preamble, &condition, &subgoals, "");
        println!("▶ delegating the whole goal to the agent's native loop…");
        let work = run_subprocess(worker, &prompt, 0, &condition)
            .map_err(|e| workflows_proto::WorkflowError::Backend(format!("worker: {e}")))?;
        let _ = wf.store().mutate_goal(session_id, |g| {
            g.turns_used = 1;
            if let Some(a) = &work.last_activity {
                g.current_activity = a.clone();
            }
            g.updated_at = chrono::Utc::now();
        });
        if work.code == 0 {
            Ok(IterationOutcome::Done)
        } else {
            Ok(IterationOutcome::Blocked {
                reason: format!("delegated agent exited {}", work.code),
                summary: "native-delegate run did not complete the goal".to_owned(),
            })
        }
    })?;
    Ok(run)
}

/// Report a finished goal run + reconcile side effects: drop the
/// goal row on completion and close the linked task. Shared by
/// `goal run` and `goal resume`.
async fn finalize_goal_run(
    run: &workflows_orchestrator::SessionRun,
    budget: u32,
    task_id: Option<uuid::Uuid>,
    url: &str,
    wf: &workflows_orchestrator::CodingWorkflow,
    session_id: uuid::Uuid,
) -> eyre::Result<()> {
    use workflows_orchestrator::RunEnd;
    match &run.end {
        RunEnd::Completed => {
            println!("✓ goal met after {} turn(s)", run.iterations);
            // The standing goal is satisfied — drop its row so it no
            // longer shows up in `goal status`.
            let _ = wf.store().remove_goal(session_id);
            if let Some(id) = task_id {
                let client = connect_task_client(url).await?;
                if let Ok(mut t) = client.get(id).await {
                    if task::Status::from_str(&t.status)
                        .is_none_or(|s| !matches!(s, task::Status::Done))
                    {
                        t.status = "done".into();
                        t.completed_date = Some(chrono::Utc::now().date_naive());
                        let _ = client.update(t).await;
                        println!("  closed linked task {}", short_uuid(&id));
                    }
                }
            }
        }
        RunEnd::Parked { reason } => {
            println!("⏸ goal parked after {} turn(s): {reason}", run.iterations);
        }
        RunEnd::MaxedOut => {
            println!(
                "⏹ hit the {budget}-turn ceiling without meeting the goal — session parked, resume to continue"
            );
        }
    }
    Ok(())
}

/// The org's standing goal session: the most recently touched
/// `Active`/`Parked` [`WorkSession`](workflows_proto::WorkSession)
/// that carries a [`GoalSession`](workflows_proto::GoalSession) row.
/// `None` when no goal loop is in flight. Backs `status` / `pause` /
/// `resume` / `clear`.
fn active_goal_session(
    wf: &workflows_orchestrator::CodingWorkflow,
) -> eyre::Result<Option<(workflows_proto::WorkSession, workflows_proto::GoalSession)>> {
    use workflows_proto::SessionStatus;
    let mut hits: Vec<(workflows_proto::WorkSession, workflows_proto::GoalSession)> = Vec::new();
    for g in wf.store().goals()? {
        if let Ok(s) = wf.store().session(g.session_id) {
            if matches!(s.status, SessionStatus::Active | SessionStatus::Parked) {
                hits.push((s, g));
            }
        }
    }
    hits.sort_by_key(|(s, _)| s.updated_at);
    Ok(hits.pop())
}

/// `task agent goal status` — report the active goal session.
async fn run_goal_status(org: Option<String>, _server: Option<String>) -> eyre::Result<()> {
    use workflows_orchestrator::{CodingWorkflow, WorkflowStore};
    use workflows_proto::{SessionStatus, SubjectRef};

    let slug = resolve_active_org(org)?;
    let wf = CodingWorkflow::new(WorkflowStore::open(org_workflows_dir(&slug)?));
    match active_goal_session(&wf)? {
        None => println!("no active goal session"),
        Some((s, g)) => {
            let status = match s.status {
                SessionStatus::Active => "active",
                SessionStatus::Parked => "parked",
                SessionStatus::Blocked => "blocked",
                SessionStatus::Finished => "finished",
                SessionStatus::Cancelled => "cancelled",
            };
            println!("goal session {}  [{status}]", short_uuid(&s.id));
            println!("  condition: {}", g.condition);
            println!("  turns:     {}/{}", g.turns_used, g.budget);
            if let SubjectRef::Task { id } = s.subject {
                println!("  task:      {}", short_uuid(&id));
            }
            if !g.subgoals.0.is_empty() {
                println!("  subgoals:");
                for (i, sg) in g.subgoals.0.iter().enumerate() {
                    println!("    {}. {sg}", i + 1);
                }
            }
            if g.last_reason.is_empty() {
                println!("  last eval: (none yet)");
            } else {
                println!("  last eval: {}", g.last_reason);
            }
            if !g.current_activity.is_empty() {
                println!("  doing:     {}", g.current_activity);
            }
            // Heartbeat + a peek at recent activity — so a running
            // loop's liveness ("how long since the last turn") and
            // what it's been doing are visible on demand.
            let recent = wf.store().activities_for(s.id).unwrap_or_default();
            if let Some(last) = recent.first() {
                println!("  heartbeat: last activity {} ago", human_ago(last.at));
            }
            if !recent.is_empty() {
                println!("  recent:");
                for a in recent.iter().take(5) {
                    let kind = serde_json::to_value(&a.kind)
                        .ok()
                        .and_then(|v| v.get("kind").and_then(|k| k.as_str()).map(str::to_owned))
                        .unwrap_or_else(|| "activity".into());
                    println!("    {} · {kind}", human_ago(a.at));
                }
            }
        }
    }
    Ok(())
}

/// Render a `DateTime` as a coarse "Ns / Nm / Nh ago" string for the
/// goal-status heartbeat.
fn human_ago(at: chrono::DateTime<chrono::Utc>) -> String {
    let secs = (chrono::Utc::now() - at).num_seconds().max(0);
    if secs < 90 {
        format!("{secs}s")
    } else if secs < 5400 {
        format!("{}m", secs / 60)
    } else {
        format!("{}h", secs / 3600)
    }
}

/// `task agent goal pause` — park the active goal session without
/// dropping it. Idempotent if already parked.
async fn run_goal_pause(org: Option<String>, _server: Option<String>) -> eyre::Result<()> {
    use workflows_orchestrator::{CodingWorkflow, WorkflowStore};
    use workflows_proto::{HandoffReason, SessionStatus};

    let slug = resolve_active_org(org)?;
    let wf = CodingWorkflow::new(WorkflowStore::open(org_workflows_dir(&slug)?));
    match active_goal_session(&wf)? {
        None => println!("no active goal session to pause"),
        Some((s, _g)) => {
            if s.status == SessionStatus::Parked {
                println!("goal session {} is already parked", short_uuid(&s.id));
                return Ok(());
            }
            wf.park(
                s.id,
                s.current_actor.clone(),
                HandoffReason::EndOfChunk,
                "goal paused via `goal pause`",
                "",
                "- resume with `task agent goal resume`",
            )?;
            println!("⏸ paused goal session {}", short_uuid(&s.id));
        }
    }
    Ok(())
}

/// `task agent goal clear` — drop the active goal session: cancel it
/// and delete its goal row.
async fn run_goal_clear(org: Option<String>, _server: Option<String>) -> eyre::Result<()> {
    use workflows_orchestrator::{CodingWorkflow, WorkflowStore};

    let slug = resolve_active_org(org)?;
    let wf = CodingWorkflow::new(WorkflowStore::open(org_workflows_dir(&slug)?));
    match active_goal_session(&wf)? {
        None => println!("no active goal session to clear"),
        Some((s, _g)) => {
            wf.cancel(s.id, s.current_actor.clone())?;
            wf.store().remove_goal(s.id)?;
            println!("⏹ cleared goal session {}", short_uuid(&s.id));
        }
    }
    Ok(())
}

/// `task agent goal update --condition` — live-steer the active loop
/// by replacing its completion condition. The running loop re-reads
/// the `GoalSession` row at the top of each turn (#174), so the next
/// turn re-steers toward the new condition without a restart.
async fn run_goal_update(
    condition: String,
    org: Option<String>,
    _server: Option<String>,
) -> eyre::Result<()> {
    use workflows_orchestrator::{CodingWorkflow, WorkflowStore};

    let slug = resolve_active_org(org)?;
    let wf = CodingWorkflow::new(WorkflowStore::open(org_workflows_dir(&slug)?));
    match active_goal_session(&wf)? {
        None => println!("no active goal session to update"),
        Some((s, _g)) => {
            // Atomic RMW so we don't clobber the loop's concurrent
            // per-turn progress write.
            wf.store().mutate_goal(s.id, |g| {
                g.condition = condition.clone();
                g.updated_at = chrono::Utc::now();
            })?;
            println!("◎ updated goal session {} condition:", short_uuid(&s.id));
            println!("  {condition}");
            println!("  (a running loop re-steers on its next turn)");
        }
    }
    Ok(())
}

/// `task agent goal subgoal *` — manage the active session's subgoals
/// (extra acceptance criteria appended mid-run). Dispatches on the
/// first token: `remove <N>` / `clear` / `list`, else the joined text
/// is appended as a new criterion (bare = list). Mutations persist on
/// the `GoalSession` row, so a running loop folds them into its next
/// worker prompt + evaluator gate at the top of its next turn.
async fn run_goal_subgoal(
    args: Vec<String>,
    org: Option<String>,
    _server: Option<String>,
) -> eyre::Result<()> {
    use workflows_orchestrator::{CodingWorkflow, WorkflowStore};

    let slug = resolve_active_org(org)?;
    let wf = CodingWorkflow::new(WorkflowStore::open(org_workflows_dir(&slug)?));
    // `g` is a read-only snapshot for validation/display; every write
    // goes through the store's atomic `mutate_goal` (locked RMW) so a
    // concurrent loop turn or another steering command can't lose it.
    let Some((s, g)) = active_goal_session(&wf)? else {
        println!("no active goal session");
        return Ok(());
    };

    // Dispatch on the first token. `remove`/`clear`/`list` are
    // subverbs; anything else is the criterion text to append.
    match args.first().map(String::as_str) {
        None => {
            print_subgoals(&s.id, &g.subgoals.0);
        }
        Some("list") if args.len() == 1 => {
            print_subgoals(&s.id, &g.subgoals.0);
        }
        Some("clear") if args.len() == 1 => {
            wf.store().mutate_goal(s.id, |g| {
                g.subgoals.0.clear();
                g.updated_at = chrono::Utc::now();
            })?;
            println!("⌫ cleared subgoal(s) on {}", short_uuid(&s.id));
        }
        Some("remove") => {
            // `remove <N>` — N is 1-based, matching the `list` display.
            let n: usize = match args.get(1).and_then(|a| a.parse().ok()) {
                Some(n) if n >= 1 && n <= g.subgoals.0.len() => n,
                _ => {
                    println!(
                        "remove takes a number 1..={} (the index shown by `goal subgoal`)",
                        g.subgoals.0.len()
                    );
                    return Ok(());
                }
            };
            let updated = wf.store().mutate_goal(s.id, |g| {
                if n - 1 < g.subgoals.0.len() {
                    g.subgoals.0.remove(n - 1);
                    g.updated_at = chrono::Utc::now();
                }
            })?;
            println!("⌫ removed subgoal {n} on {}", short_uuid(&s.id));
            print_subgoals(&s.id, &updated.subgoals.0);
        }
        Some(_) => {
            // No subverb matched: treat the whole arg list as the
            // criterion text (so unquoted multi-word input still works).
            let text = args.join(" ");
            let text = text.trim();
            if text.is_empty() {
                print_subgoals(&s.id, &g.subgoals.0);
                return Ok(());
            }
            let updated = wf.store().mutate_goal(s.id, |g| {
                g.subgoals.0.push(text.to_owned());
                g.updated_at = chrono::Utc::now();
            })?;
            println!(
                "＋ added subgoal {} on {}: {text}",
                updated.subgoals.0.len(),
                short_uuid(&s.id)
            );
        }
    }
    Ok(())
}

/// Print the numbered subgoal list (1-based, matching `remove <N>`).
fn print_subgoals(session_id: &uuid::Uuid, subgoals: &[String]) {
    if subgoals.is_empty() {
        println!("no subgoals on goal session {}", short_uuid(session_id));
        return;
    }
    println!("subgoals on goal session {}:", short_uuid(session_id));
    for (i, sg) in subgoals.iter().enumerate() {
        println!("  {}. {sg}", i + 1);
    }
}

/// `task agent goal resume` — reset the parked session's turn counter
/// to 0 and continue the loop toward its stored condition.
async fn run_goal_resume(
    cmd: Option<String>,
    eval_cmd: Option<String>,
    as_agent: String,
    max_iters: Option<u32>,
    org: Option<String>,
    server: Option<String>,
) -> eyre::Result<()> {
    use workflows_orchestrator::{CodingWorkflow, WorkflowStore};
    use workflows_proto::SubjectRef;

    let slug = resolve_active_org(org)?;
    let url = resolve_org_vox_url(server, &slug);
    let agent = parse_agent_ref(&format!("agent:{as_agent}"))?;
    let wf = CodingWorkflow::new(WorkflowStore::open(org_workflows_dir(&slug)?));

    let Some((session, mut goal)) = active_goal_session(&wf)? else {
        println!("no goal session to resume");
        return Ok(());
    };

    // Re-resolve the worker / evaluator commands (flag → env → config),
    // matching `goal run`'s precedence — the loop doesn't persist them.
    let cfg = load_goal_config(&slug);
    let worker = cmd
        .or_else(|| std::env::var("TASK_AGENT_CMD").ok())
        .or(cfg.worker_cmd)
        .ok_or_else(|| {
            eyre::eyre!(
                "no worker command: pass --cmd, set TASK_AGENT_CMD, or set goal.worker_cmd in ~/.task/orgs/{slug}/agent.json"
            )
        })?;
    let evaluator = eval_cmd
        .or_else(|| std::env::var("TASK_GOAL_EVAL_CMD").ok())
        .or(cfg.eval_cmd);

    let task_id = match session.subject {
        SubjectRef::Task { id } => Some(id),
        _ => None,
    };
    if evaluator.is_none() && task_id.is_none() {
        return Err(eyre::eyre!(
            "no evaluator: pass --eval-cmd or set TASK_GOAL_EVAL_CMD / goal.eval_cmd"
        ));
    }

    let static_preamble = build_goal_preamble(&url, task_id).await?;
    let budget = max_iters.unwrap_or(goal.budget);

    // Resume the session (→ Active) and reset the turn counter — the
    // defining behaviour of `resume` vs a fresh `run`.
    wf.resume(session.id, agent.clone())?;
    goal.turns_used = 0;
    goal.budget = budget;
    goal.last_reason = String::new();
    goal.updated_at = chrono::Utc::now();
    wf.store().put_goal(&goal)?;

    println!(
        "▶ resuming goal session {} — “{}” (max {budget} turns, counter reset)",
        short_uuid(&session.id),
        goal.condition
    );

    // Honor the configured executor mode on resume too.
    let executor = if cfg.mode.as_deref() == Some("delegate") {
        GoalExecutor::NativeDelegate { worker: &worker }
    } else {
        GoalExecutor::Subprocess {
            worker: &worker,
            evaluator: evaluator.as_deref(),
        }
    };
    let run = executor.drive(&wf, session.id, &agent, &static_preamble, budget)?;
    finalize_goal_run(&run, budget, task_id, &url, &wf, session.id).await
}

/// Rebuild the loop's static preamble on resume: the rendered task
/// PRD + subtask checklist when the session is task-linked, else
/// empty. The condition is read live from the store per turn, so it's
/// deliberately not part of the preamble. Mirrors `goal run`.
async fn build_goal_preamble(url: &str, task_id: Option<uuid::Uuid>) -> eyre::Result<String> {
    if let Some(id) = task_id {
        let client = connect_task_client(url).await?;
        if let Ok(t) = client.get(id).await {
            let parent = match t.workflow.as_ref().and_then(|w| w.parent) {
                Some(pid) => client.get(pid).await.ok(),
                None => None,
            };
            let children = subtasks_of(&client, t.id).await.unwrap_or_default();
            let preamble = render_task_prompt(&t, parent.as_ref());
            let subtasks_md = render_subtask_checklist(&children);
            return Ok(format!("{preamble}{subtasks_md}"));
        }
    }
    Ok(String::new())
}

/// Verdict from one evaluator pass.
struct EvalVerdict {
    met: bool,
    reason: String,
}

/// Per-org `agent.json` `[goal]` defaults for `task agent goal` —
/// the executor seam's config layer. JSON to match the CLI's other
/// per-org stores (`labels.json`, `handoffs.json`). All optional.
#[derive(Default, serde::Deserialize)]
struct GoalConfig {
    worker_cmd: Option<String>,
    eval_cmd: Option<String>,
    #[allow(dead_code)] // reserved: config-driven turn budget
    max_turns: Option<u32>,
    /// `"delegate"` selects the native-delegate executor by default
    /// (for agents with their own goal loop). Anything else / unset =
    /// the turn-by-turn subprocess loop.
    mode: Option<String>,
}

#[derive(Default, serde::Deserialize)]
struct AgentConfig {
    #[serde(default)]
    goal: GoalConfig,
}

/// Load `~/.task/orgs/<slug>/agent.json` — missing / unparseable
/// file yields defaults (the feature degrades to flags + env).
fn load_goal_config(org_slug: &str) -> GoalConfig {
    let Some(home) = std::env::var_os("HOME") else {
        return GoalConfig::default();
    };
    let p = std::path::Path::new(&home)
        .join(".task")
        .join("orgs")
        .join(org_slug)
        .join("agent.json");
    std::fs::read(&p)
        .ok()
        .and_then(|b| serde_json::from_slice::<AgentConfig>(&b).ok())
        .unwrap_or_default()
        .goal
}

/// Interpret an evaluator's output. The cross-agent judge convention
/// (Hermes / Claude / Codex `/goal`) is a JSON object
/// `{"done": bool, "reason": "..."}` on stdout — preferred when
/// present (anywhere in the output, so a chatty judge still works).
/// Otherwise fall back to the exit code: `0` = met, nonzero = not,
/// with the trimmed stdout as the reason.
fn parse_eval_verdict(stdout: &str, code: i32) -> EvalVerdict {
    #[derive(serde::Deserialize)]
    struct Judged {
        done: bool,
        #[serde(default)]
        reason: String,
    }
    // Scan for the first `{...}` that parses as the judge shape, so
    // surrounding prose (common with LLM judges) doesn't defeat it.
    if let (Some(start), Some(end)) = (stdout.find('{'), stdout.rfind('}')) {
        if end > start {
            if let Ok(j) = serde_json::from_str::<Judged>(&stdout[start..=end]) {
                return EvalVerdict {
                    met: j.done,
                    reason: j.reason.trim().to_owned(),
                };
            }
        }
    }
    EvalVerdict {
        met: code == 0,
        reason: stdout.trim().to_owned(),
    }
}

/// Captured result of a worker / evaluator subprocess.
struct SubprocOut {
    code: i32,
    stdout: String,
    /// The most recent normalized step parsed from a stream-json
    /// worker (e.g. `Edit src/foo.rs`), if any — what it was last
    /// doing. `None` for opaque (non-stream-json) workers.
    last_activity: Option<String>,
}

/// Turn one line of a Claude `--output-format stream-json` worker into
/// a short, human-readable "current step" — a tool call (`Edit
/// <file>`, `Bash: <cmd>`) or an assistant message. Returns `None` for
/// lines that aren't a recognized event (the caller then streams them
/// raw), so plain workers are unaffected. Mirrors how t3code
/// normalizes agent events into a content-stream.
fn parse_stream_event(line: &str) -> Option<String> {
    fn clip(s: &str, n: usize) -> String {
        let s = s.trim();
        if s.chars().count() > n {
            format!("{}…", s.chars().take(n).collect::<String>())
        } else {
            s.to_owned()
        }
    }
    let v: serde_json::Value = serde_json::from_str(line.trim()).ok()?;
    match v.get("type")?.as_str()? {
        "assistant" => {
            let content = v.get("message")?.get("content")?.as_array()?;
            // Prefer a tool call (the concrete action).
            for b in content {
                if b.get("type").and_then(serde_json::Value::as_str) == Some("tool_use") {
                    let name = b
                        .get("name")
                        .and_then(serde_json::Value::as_str)
                        .unwrap_or("tool");
                    let arg = b.get("input").and_then(|i| {
                        ["file_path", "path", "command", "pattern", "query"]
                            .iter()
                            .find_map(|k| i.get(*k).and_then(serde_json::Value::as_str))
                    });
                    return Some(match arg {
                        Some(a) => format!("{name}: {}", clip(a, 80)),
                        None => name.to_owned(),
                    });
                }
            }
            // Else the assistant's message text.
            for b in content {
                if b.get("type").and_then(serde_json::Value::as_str) == Some("text") {
                    if let Some(t) = b.get("text").and_then(serde_json::Value::as_str) {
                        let first = t.lines().find(|l| !l.trim().is_empty()).unwrap_or("");
                        if !first.is_empty() {
                            return Some(format!("\u{1f4ac} {}", clip(first, 80)));
                        }
                    }
                }
            }
            None
        }
        "result" => Some("✓ turn result".to_owned()),
        _ => None,
    }
}

/// Run `command` via `sh -c`, piping `prompt` to its stdin and
/// exposing `TASK_GOAL` / `TASK_GOAL_ITER` in its env.
///
/// Streams the child's stdout **live**, line by line, while also
/// capturing it for the caller — so a multi-minute agent turn isn't a
/// black box (the whole reason the loop looked "stuck"). A heartbeat
/// thread prints elapsed time every ~15s while the child runs quietly,
/// so even a worker that buffers its output (e.g. `claude -p`) shows
/// signs of life. stderr passes straight through.
fn run_subprocess(
    command: &str,
    prompt: &str,
    iter: u32,
    condition: &str,
) -> eyre::Result<SubprocOut> {
    use std::io::{BufRead as _, BufReader, Write as _};
    use std::process::{Command, Stdio};
    use std::sync::Arc;
    use std::sync::atomic::{AtomicBool, Ordering};

    let started = std::time::Instant::now();
    let mut child = Command::new("sh")
        .arg("-c")
        .arg(command)
        .env("TASK_GOAL", condition)
        .env("TASK_GOAL_ITER", iter.to_string())
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .spawn()
        .map_err(|e| eyre::eyre!("spawn `{command}`: {e}"))?;

    // Feed the prompt and close stdin (drop) so the child sees EOF and
    // starts working.
    if let Some(mut stdin) = child.stdin.take() {
        stdin
            .write_all(prompt.as_bytes())
            .map_err(|e| eyre::eyre!("write stdin: {e}"))?;
    }

    // Heartbeat: every 15s of quiet, remind the user it's alive.
    let alive = Arc::new(AtomicBool::new(true));
    let beat = {
        let alive = Arc::clone(&alive);
        std::thread::spawn(move || {
            let mut waited = 0u64;
            while alive.load(Ordering::Relaxed) {
                std::thread::sleep(std::time::Duration::from_secs(1));
                waited += 1;
                if waited.is_multiple_of(15) && alive.load(Ordering::Relaxed) {
                    eprintln!("    …still working ({waited}s)");
                }
            }
        })
    };

    // Stream + capture stdout line by line. Stream-json events are
    // rendered as normalized steps (`→ Edit foo.rs`); everything else
    // streams raw (`│ <line>`).
    let mut captured = String::new();
    let mut last_activity: Option<String> = None;
    if let Some(out) = child.stdout.take() {
        for line in BufReader::new(out).lines() {
            let line = line.map_err(|e| eyre::eyre!("read stdout: {e}"))?;
            if let Some(step) = parse_stream_event(&line) {
                println!("    → {step}");
                last_activity = Some(step);
            } else {
                println!("    │ {line}");
            }
            captured.push_str(&line);
            captured.push('\n');
        }
    }

    let status = child.wait().map_err(|e| eyre::eyre!("wait: {e}"))?;
    alive.store(false, Ordering::Relaxed);
    let _ = beat.join();
    eprintln!("    └ done in {}s", started.elapsed().as_secs());
    Ok(SubprocOut {
        code: status.code().unwrap_or(-1),
        stdout: captured,
        last_activity,
    })
}

/// Render the agent-facing prompt for a task: its own PRD plus the
/// parent issue's PRD when it's a subtask. The one built-in template
/// (concrete-first; a pluggable template system is deferred). Shared
/// by `task issue prompt` and `task agent goal --task` so the loop
/// and the standalone preview never drift.
fn render_task_prompt(t: &task::TaskInfo, parent: Option<&task::TaskInfo>) -> String {
    let mut s = String::new();
    if let Some(p) = parent {
        s.push_str(&format!("# Parent issue (PRD): {}\n\n", p.title));
        let body = p.details.trim();
        if body.is_empty() {
            s.push_str("(no description)\n\n");
        } else {
            s.push_str(body);
            s.push_str("\n\n");
        }
        s.push_str("---\n\n");
    }
    s.push_str(&format!("# Task: {}  [{}]\n\n", t.title, t.priority));
    let body = t.details.trim();
    if body.is_empty() {
        s.push_str("(no description)\n");
    } else {
        s.push_str(body);
        s.push('\n');
    }
    if !t.tags.0.is_empty() {
        s.push_str(&format!("\ntags: {}\n", t.tags.0.join(", ")));
    }
    s
}

/// Open subtasks of `parent_id` — tasks whose `workflow.parent`
/// points at it — oldest first by title (stable enough for a list).
async fn subtasks_of(
    client: &task::TaskServiceClient,
    parent_id: uuid::Uuid,
) -> eyre::Result<Vec<task::TaskInfo>> {
    let rows = client
        .list()
        .await
        .map_err(|e| eyre::eyre!("list: {e:?}"))?;
    let mut kids: Vec<task::TaskInfo> = rows
        .into_iter()
        .filter(|t| t.workflow.as_ref().and_then(|w| w.parent) == Some(parent_id))
        .collect();
    kids.sort_by(|a, b| a.title.cmp(&b.title));
    Ok(kids)
}

/// Create one subtask `title` under `parent`, mirroring `issue
/// triage`'s row shape (parent link, `subtask` tag, inherited
/// project). Returns `false` (rather than erroring) when a task with
/// the same title-slug already exists — generated titles can collide,
/// and a collision shouldn't abort the whole triage.
async fn create_subtask(
    client: &task::TaskServiceClient,
    parent: &task::TaskInfo,
    title: &str,
) -> eyre::Result<bool> {
    let sub = task::TaskInfo {
        id: uuid::Uuid::nil(),
        path: String::new(),
        title: title.to_owned(),
        status: "open".into(),
        priority: parent.priority.clone(),
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
    match client.create(sub).await {
        Ok(_) => Ok(true),
        // Title-slug already taken — skip rather than abort the run.
        // Matched on the message since the error is wrapped in
        // VoxError<TaskError> across the wire.
        Err(e) if format!("{e:?}").contains("AlreadyExists") => Ok(false),
        Err(e) => Err(eyre::eyre!("create subtask: {e:?}")),
    }
}

/// The decompose-turn prompt. "Decompose, don't execute" (per
/// Hermes's kanban-orchestrator): the worker breaks the PRD into
/// agent-sized titles or declares it a one-shot.
fn decompose_prompt(task_prompt: &str) -> String {
    format!(
        "{task_prompt}\n\n---\n\nYou are TRIAGING this task — do NOT implement anything. \
         Break it into 2–6 agent-sized subtasks, each a single focused PR's worth of work, \
         ideally independent. Output ONLY the subtask titles, one per line, no numbering or \
         prose. If the task is small enough to do in one shot (no fan-out needed), output \
         the single line: ONE-SHOT"
    )
}

/// Parse subtask titles from a decompose turn's output. Strips
/// bullets / numbering, drops blanks and the `ONE-SHOT` sentinel,
/// and ignores obvious prose (lines ending in `:` or very long).
fn parse_subtask_titles(out: &str) -> Vec<String> {
    out.lines()
        .map(|l| {
            l.trim()
                .trim_start_matches(['-', '*', '•'])
                .trim_start_matches(|c: char| c.is_ascii_digit() || c == '.' || c == ')')
                .trim()
                .to_owned()
        })
        .filter(|l| {
            !l.is_empty()
                && !l.eq_ignore_ascii_case("ONE-SHOT")
                && !l.ends_with(':')
                && l.len() <= 140
        })
        .collect()
}

/// Render the subtask checklist appended to the goal prompt.
fn render_subtask_checklist(children: &[task::TaskInfo]) -> String {
    if children.is_empty() {
        return String::new();
    }
    let mut s = String::from("\n\n## Subtasks\n");
    for c in children {
        let done = matches!(task::Status::from_str(&c.status), Some(task::Status::Done));
        s.push_str(&format!(
            "- [{}] {}\n",
            if done { "x" } else { " " },
            c.title
        ));
    }
    s
}

/// `~/.task/orgs/<slug>/workflows` — the orchestrator store dir.
fn org_workflows_dir(org_slug: &str) -> eyre::Result<std::path::PathBuf> {
    let home = std::env::var_os("HOME").ok_or_else(|| eyre::eyre!("HOME not set"))?;
    Ok(std::path::Path::new(&home)
        .join(".task")
        .join("orgs")
        .join(org_slug)
        .join("workflows"))
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

/// Resolve an issue reference — uuid, id prefix, vault path, or
/// title (issues and tasks are the same `TaskInfo` row, so this is
/// the shared flexible task resolver).
async fn resolve_issue_id(
    client: &task::TaskServiceClient,
    id: &str,
) -> eyre::Result<task::TaskInfo> {
    json_out::resolve_task_flexible(client, id).await
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
    if let Some(ws) = w.workstream {
        println!("    workstream:{ws}");
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

/// Apply `set-workflow` style edits to a `TaskInfo` in-place. The
/// cycle / project / blocker references arrive pre-resolved (the
/// caller ran the flexible resolvers): outer `None` = leave alone,
/// `Some(None)` = clear, `Some(Some(id))` = set.
#[allow(clippy::too_many_arguments)]
// Option<Option<_>> is exactly the tri-state these patch fields
// need (untouched / cleared / set) — a custom enum adds noise for
// one private helper.
#[allow(clippy::option_option)]
fn apply_workflow_patch(
    t: &mut task::TaskInfo,
    cycle: Option<Option<uuid::Uuid>>,
    project: Option<Option<uuid::Uuid>>,
    workstream: Option<Option<uuid::Uuid>>,
    estimate: Option<String>,
    add_assignee: Vec<workflows_proto::AgentRef>,
    remove_assignee: Vec<workflows_proto::AgentRef>,
    add_blocker: Vec<uuid::Uuid>,
    remove_blocker: Vec<uuid::Uuid>,
) -> eyre::Result<()> {
    // Project membership lives on TaskInfo.project_id (the
    // canonical Project link), not in WorkflowAttrs.
    if let Some(v) = project {
        t.project_id = v;
    }

    let w = t
        .workflow
        .get_or_insert_with(task::model::WorkflowAttrs::default);

    if let Some(v) = cycle {
        w.cycle = v;
    }
    if let Some(v) = workstream {
        w.workstream = v;
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

/// Resolve an optional `--project` filter (uuid, id prefix, path,
/// or name) into the project id, dialing the project service only
/// when the flag is present.
async fn resolve_project_filter(
    url: &str,
    project: Option<String>,
) -> eyre::Result<Option<uuid::Uuid>> {
    match project {
        None => Ok(None),
        Some(p) => {
            let pc = connect_project_client(url).await?;
            Ok(Some(json_out::resolve_project_flexible(&pc, &p).await?.id))
        }
    }
}

/// Resolve an optional `--workstream` filter (uuid, id prefix,
/// path, or name) into the workstream id, dialing the workstream
/// service only when the flag is present.
async fn resolve_workstream_filter(
    url: &str,
    workstream: Option<String>,
) -> eyre::Result<Option<uuid::Uuid>> {
    match workstream {
        None => Ok(None),
        Some(w) => {
            let wc: ::workstream::WorkstreamServiceClient = establish_for_url(url).await?;
            Ok(Some(
                json_out::resolve_workstream_flexible(&wc, &w).await?.id,
            ))
        }
    }
}

/// Org slug + per-org vox URL from the global `--org` / `--server`
/// flags (the `issue` group dropped its per-variant duplicates).
/// Called per-arm so org-free verbs (`pr-list`, dry runs) keep
/// working without a session.
fn issue_ctx() -> eyre::Result<(String, String)> {
    let slug = resolve_active_org(None)?;
    let url = resolve_org_vox_url(None, &slug);
    Ok((slug, url))
}

async fn run_issue(cmd: IssueCmd) -> eyre::Result<()> {
    match cmd {
        IssueCmd::List {
            cycle,
            project,
            assignee,
            status,
            has_workflow,
            json,
        } => {
            let (_slug, url) = issue_ctx()?;
            let cycle = resolve_cycle_arg(cycle, false)?;
            let project = resolve_project_filter(&url, project).await?;
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
        IssueCmd::Show { id, json } => {
            let (_slug, url) = issue_ctx()?;
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
        IssueCmd::Prompt { id } => {
            let (_slug, url) = issue_ctx()?;
            let client = connect_task_client(&url).await?;
            let t = resolve_issue_id(&client, &id).await?;
            let parent = match t.workflow.as_ref().and_then(|w| w.parent) {
                Some(pid) => client.get(pid).await.ok(),
                None => None,
            };
            print!("{}", render_task_prompt(&t, parent.as_ref()));
        }
        IssueCmd::SetWorkflow {
            id,
            cycle,
            project,
            workstream,
            estimate,
            add_assignee,
            remove_assignee,
            add_blocker,
            remove_blocker,
            clear,
            json,
        } => {
            let (_slug, url) = issue_ctx()?;
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
                // Resolve the entity references up-front: cycle
                // accepts uuid / label / `current` / `none`; project
                // accepts uuid / name / path / prefix / `none`;
                // blockers accept any issue reference.
                let cycle = match cycle {
                    None => None,
                    Some(c) => Some(resolve_cycle_arg(Some(c), false)?),
                };
                let project = match project.as_deref() {
                    None => None,
                    Some("" | "none" | "null") => Some(None),
                    Some(p) => Some(resolve_project_filter(&url, Some(p.to_owned())).await?),
                };
                let workstream = match workstream.as_deref() {
                    None => None,
                    Some("" | "none" | "null") => Some(None),
                    Some(w) => Some(resolve_workstream_filter(&url, Some(w.to_owned())).await?),
                };
                let mut add_b = Vec::with_capacity(add_blocker.len());
                for b in &add_blocker {
                    add_b.push(resolve_issue_id(&client, b).await?.id);
                }
                let mut rm_b = Vec::with_capacity(remove_blocker.len());
                for b in &remove_blocker {
                    rm_b.push(resolve_issue_id(&client, b).await?.id);
                }
                apply_workflow_patch(
                    &mut t, cycle, project, workstream, estimate, add, rm, add_b, rm_b,
                )?;
            }
            let updated = client
                .update(t)
                .await
                .map_err(|e| eyre::eyre!("update: {e:?}"))?;
            if json {
                json_out::print_json(&updated)?;
                return Ok(());
            }
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
            json,
        } => {
            let (_slug, url) = issue_ctx()?;
            let client = connect_task_client(&url).await?;
            let agent = parse_agent_ref(&format!("agent:{as_agent}"))?;
            let t = resolve_issue_id(&client, &id).await?;
            match try_claim(&client, &t.id, &agent, force).await? {
                ClaimOutcome::Won => {
                    if !json {
                        println!("claimed {} by {}", short_uuid(&t.id), agent.short_label());
                    }
                }
                ClaimOutcome::AlreadyMine => {
                    if !json {
                        println!(
                            "{} already claimed by {}",
                            short_uuid(&t.id),
                            agent.short_label()
                        );
                    }
                }
                ClaimOutcome::Lost(holder) => {
                    return Err(errors::conflict("claim issue", short_uuid(&t.id))
                        .cause(format!("already claimed by {holder}"))
                        .hint("pass --force to steal the claim")
                        .report());
                }
            }
            if json {
                // Re-read so the emitted entity reflects the claim.
                let after = client
                    .get(t.id)
                    .await
                    .map_err(|e| eyre::eyre!("re-read after claim: {e:?}"))?;
                json_out::print_json(&after)?;
            }
        }
        IssueCmd::Triage {
            id,
            subtasks,
            from,
            parent_status,
            priority,
        } => {
            let (_slug, url) = issue_ctx()?;
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
        IssueCmd::Subtasks { id, json } => {
            let (_slug, url) = issue_ctx()?;
            let client = connect_task_client(&url).await?;
            let parent = resolve_issue_id(&client, &id).await?;
            let all = client
                .list()
                .await
                .map_err(|e| eyre::eyre!("list: {e:?}"))?;
            // Derived rollup over the children — shared engine,
            // classified via each task's project state registry.
            let states = project_states_map(&url).await;
            let rollup =
                ::workstream::subtask_rollup(parent.id, &all, |t| resolve_task_group(&states, t));
            let mut subs: Vec<&task::TaskInfo> = all
                .iter()
                .filter(|t| t.workflow.as_ref().and_then(|w| w.parent) == Some(parent.id))
                .collect();
            subs.sort_by(|a, b| a.status.cmp(&b.status).then_with(|| a.title.cmp(&b.title)));
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&serde_json::json!({
                        "parent": parent.id,
                        "rollup": rollup,
                        "subtasks": subs,
                    }))
                    .map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            println!(
                "{} [{}]  {}",
                short_uuid(&parent.id),
                parent.status,
                parent.title
            );
            println!(
                "  {}/{} done · {} in-progress · {} blocked · {} pts\n",
                rollup.done,
                rollup.total,
                rollup.in_progress,
                rollup.blocked,
                rollup.estimate_points_sum
            );
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
        IssueCmd::Rollup { id, json } => {
            let (_slug, url) = issue_ctx()?;
            let client = connect_task_client(&url).await?;
            let parent = resolve_issue_id(&client, &id).await?;
            let all = client
                .list()
                .await
                .map_err(|e| eyre::eyre!("list: {e:?}"))?;
            let states = project_states_map(&url).await;
            let rollup =
                ::workstream::subtask_rollup(parent.id, &all, |t| resolve_task_group(&states, t));
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&serde_json::json!({
                        "parent": parent.id,
                        "rollup": rollup,
                    }))
                    .map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            println!(
                "{} [{}]  {}",
                short_uuid(&parent.id),
                parent.status,
                parent.title
            );
            println!("  done:        {}/{}", rollup.done, rollup.total);
            println!("  in-progress: {}", rollup.in_progress);
            println!("  blocked:     {}", rollup.blocked);
            println!("  points:      {}", rollup.estimate_points_sum);
            let g = &rollup.groups;
            println!(
                "  groups:      backlog {} / unstarted {} / started {} / completed {} / cancelled {}",
                g.backlog, g.unstarted, g.started, g.completed, g.cancelled
            );
            if rollup.total > 0 {
                println!(
                    "  progress:    {:.0}%",
                    f64::from(rollup.done) * 100.0 / f64::from(rollup.total)
                );
            }
        }
        IssueCmd::Relate {
            a,
            kind,
            b,
            remove,
            json,
        } => {
            let (_slug, url) = issue_ctx()?;
            let client = connect_task_client(&url).await?;
            let kind = task::RelationKind::from_str(&kind).ok_or_else(|| {
                eyre::eyre!(
                    "unknown relation kind `{kind}` — one of blocks / duplicate / \
                     implements / relates"
                )
            })?;
            let mut src = resolve_issue_id(&client, &a).await?;
            let dst = resolve_issue_id(&client, &b).await?;
            if src.id == dst.id {
                return Err(eyre::eyre!("an issue can't relate to itself"));
            }
            let rel = task::Relation {
                kind,
                target: dst.id,
            };
            let w = src
                .workflow
                .get_or_insert_with(task::model::WorkflowAttrs::default);
            let already = w.relations.0.contains(&rel);
            if remove {
                if !already {
                    return Err(eyre::eyre!(
                        "no `{}` relation from {} to {}",
                        kind.as_str(),
                        short_uuid(&src.id),
                        short_uuid(&dst.id)
                    ));
                }
                w.relations.0.retain(|r| r != &rel);
            } else if !already {
                w.relations.0.push(rel);
            }
            // Relation changes ride the normal update path, so the
            // backend publishes TaskEvent::Upserted to subscribers.
            let updated = client
                .update(src)
                .await
                .map_err(|e| eyre::eyre!("update: {e:?}"))?;
            if json {
                json_out::print_json(&updated)?;
                return Ok(());
            }
            let verb = if remove { "unrelated" } else { "related" };
            println!(
                "{verb}: {} ({}) —{}→ {} ({})",
                updated.title,
                short_uuid(&updated.id),
                kind.as_str(),
                dst.title,
                short_uuid(&dst.id)
            );
        }
        IssueCmd::Relations { id, json } => {
            let (_slug, url) = issue_ctx()?;
            let client = connect_task_client(&url).await?;
            let t = resolve_issue_id(&client, &id).await?;
            let all = client
                .list()
                .await
                .map_err(|e| eyre::eyre!("list: {e:?}"))?;
            let by_id: std::collections::HashMap<uuid::Uuid, &task::TaskInfo> =
                all.iter().map(|x| (x.id, x)).collect();
            let label = |id: &uuid::Uuid| {
                by_id
                    .get(id)
                    .map_or_else(|| "(unknown)".to_string(), |x| x.title.clone())
            };
            // Outgoing from the merged local view; incoming via
            // the server's reverse index.
            let outgoing = task::relations::outgoing(t.id, &all);
            let incoming = client
                .reverse_relations(t.id)
                .await
                .map_err(|e| eyre::eyre!("reverse_relations: {e:?}"))?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&serde_json::json!({
                        "id": t.id,
                        "outgoing": outgoing,
                        "incoming": incoming,
                    }))
                    .map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            println!("{} [{}]  {}\n", short_uuid(&t.id), t.status, t.title);
            if outgoing.is_empty() && incoming.is_empty() {
                println!("  (no relations)");
                return Ok(());
            }
            if !outgoing.is_empty() {
                println!("  outgoing (this issue → other):");
                for r in &outgoing {
                    println!(
                        "    {:<11} {}  {}",
                        r.kind.as_str(),
                        short_uuid(&r.target),
                        label(&r.target)
                    );
                }
            }
            if !incoming.is_empty() {
                println!("  incoming (other → this issue):");
                for r in &incoming {
                    println!(
                        "    {:<11} {}  {}",
                        r.kind.as_str(),
                        short_uuid(&r.source),
                        label(&r.source)
                    );
                }
            }
        }
        IssueCmd::Blocking { id, json } => {
            let (_slug, url) = issue_ctx()?;
            let client = connect_task_client(&url).await?;
            let t = resolve_issue_id(&client, &id).await?;
            let all = client
                .list()
                .await
                .map_err(|e| eyre::eyre!("list: {e:?}"))?;
            let blocked_ids = task::relations::blocking(t.id, &all);
            let by_id: std::collections::HashMap<uuid::Uuid, &task::TaskInfo> =
                all.iter().map(|x| (x.id, x)).collect();
            if json {
                let rows: Vec<&task::TaskInfo> = blocked_ids
                    .iter()
                    .filter_map(|bid| by_id.get(bid).copied())
                    .collect();
                println!(
                    "{}",
                    serde_json::to_string_pretty(&rows).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            if blocked_ids.is_empty() {
                println!("{} blocks nothing", short_uuid(&t.id));
                return Ok(());
            }
            println!("{} blocks:", short_uuid(&t.id));
            for bid in &blocked_ids {
                match by_id.get(bid) {
                    Some(b) => println!("  {}  {:<12} {}", short_uuid(bid), b.status, b.title),
                    None => println!("  {bid}  (unknown)"),
                }
            }
        }
        IssueCmd::Assignees { id, json } => {
            let (_slug, url) = issue_ctx()?;
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
            workstream,
            estimate,
            assignees,
            blockers,
            tags,
            body,
            json,
        } => {
            let (_slug, url) = issue_ctx()?;
            let client = connect_task_client(&url).await?;
            let body = resolve_body(body)?;

            // Resolve entity references: cycle label / `current`,
            // project name / path / prefix, parent + blockers by
            // any issue reference.
            let cycle = resolve_cycle_arg(cycle, false)?;
            let project = resolve_project_filter(&url, project).await?;
            let workstream = resolve_workstream_filter(&url, workstream).await?;
            let parent = match parent {
                None => None,
                Some(p) => Some(resolve_issue_id(&client, &p).await?.id),
            };
            let mut blocker_ids = Vec::with_capacity(blockers.len());
            for b in &blockers {
                blocker_ids.push(resolve_issue_id(&client, b).await?.id);
            }
            let blockers = blocker_ids;

            // Build the WorkflowAttrs from inline flags. Skip if
            // nothing was set — leaves `workflow: None`, preserving
            // the TaskNotes-shape round-trip for plain tasks.
            let assignee_refs: Vec<workflows_proto::AgentRef> = assignees
                .iter()
                .map(|s| parse_agent_ref(s))
                .collect::<eyre::Result<_>>()?;
            let any_workflow = cycle.is_some()
                || parent.is_some()
                || workstream.is_some()
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
                    workstream,
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
            json,
        } => {
            let (_slug, url) = issue_ctx()?;
            let cycle = resolve_cycle_arg(cycle, false)?;
            let project = resolve_project_filter(&url, project).await?;
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
        IssueCmd::Start { id, as_agent, json } => {
            let (_slug, url) = issue_ctx()?;
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
            if json {
                json_out::print_json(&updated)?;
                return Ok(());
            }
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
        IssueCmd::ImportBeads { from, dry_run } => {
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

            let (_slug, url) = issue_ctx()?;
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
        IssueCmd::Stats { project, json } => {
            use std::collections::BTreeMap;

            let (_slug, url) = issue_ctx()?;
            let project = resolve_project_filter(&url, project).await?;
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

            // Per-project state registries: status → group
            // classification routes through each task's owning
            // project (custom registries respected; tasks with
            // no project use the default registry).
            let states_by_project: std::collections::HashMap<
                uuid::Uuid,
                Option<::project::StatesConfig>,
            > = match connect_project_client(&url).await {
                Ok(pc) => pc
                    .list()
                    .await
                    .map(|ps| ps.into_iter().map(|p| (p.id, p.states)).collect())
                    .unwrap_or_default(),
                Err(_) => std::collections::HashMap::new(),
            };
            let group_of = |t: &task::TaskInfo| -> ::project::StateGroup {
                let cfg = t
                    .project_id
                    .and_then(|pid| states_by_project.get(&pid))
                    .and_then(Option::as_ref);
                ::project::resolve_state_group(cfg, &t.status)
            };

            let total = filtered.len();
            let mut by_status: BTreeMap<String, usize> = BTreeMap::new();
            let mut by_group: BTreeMap<String, usize> = BTreeMap::new();
            let mut by_priority: BTreeMap<String, usize> = BTreeMap::new();
            let mut by_project: BTreeMap<String, usize> = BTreeMap::new();
            let mut by_assignee: BTreeMap<String, usize> = BTreeMap::new();
            let mut blocked: usize = 0;
            let mut with_workflow: usize = 0;

            let by_id: std::collections::HashMap<uuid::Uuid, &task::TaskInfo> =
                rows.iter().map(|t| (t.id, t)).collect();

            for t in filtered.drain(..) {
                *by_status.entry(t.status.clone()).or_default() += 1;
                *by_group
                    .entry(group_of(t).as_str().to_string())
                    .or_default() += 1;
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
                    // Blocked = has at least one blocker whose
                    // state *group* isn't closed (completed /
                    // cancelled), or that we can't resolve.
                    let is_blocked = wf
                        .blockers
                        .0
                        .iter()
                        .any(|bid| by_id.get(bid).is_none_or(|b| !group_of(b).is_closed()));
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
                    "by_group": by_group,
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
            println!("by group:");
            for (k, v) in &by_group {
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
        IssueCmd::Close { id, undo, json } => {
            let (slug, url) = issue_ctx()?;
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
            if json {
                json_out::print_json(&updated)?;
            } else {
                let verb = if undo { "reopened" } else { "closed" };
                println!(
                    "{verb} {}  [{}]  {}",
                    short_uuid(&updated.id),
                    updated.status,
                    updated.title
                );
            }

            // Propagate to any linked forge issues. Best-effort:
            // a forge that's unreachable / unauthenticated logs
            // a warning but doesn't fail the local close. Under
            // --json the note goes to stderr so stdout stays a
            // single parseable entity.
            let new_state = if undo {
                git_proto::IssueState::Open
            } else {
                git_proto::IssueState::Closed
            };
            match propagate_state_to_forge(&slug, &updated.id, new_state).await {
                Ok(0) => {}
                Ok(n) if json => eprintln!("propagated to {n} linked forge issue(s)"),
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
        } => {
            let (slug, url) = issue_ctx()?;
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
        } => {
            let (slug, url) = issue_ctx()?;
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
        } => {
            let (slug, url) = issue_ctx()?;
            let project = resolve_project_filter(&url, project).await?;
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
        } => {
            let (slug, url) = issue_ctx()?;
            let project = resolve_project_filter(&url, project).await?;
            let client = connect_task_client(&url).await?;
            let repo_id = build_repo_id(&repo, github, base_url)?;
            let store = forge_link_store(&slug)?;
            let (reconciled, pulled) =
                sync_repo(&client, &store, &repo_id, project, no_pull).await?;
            println!("\nsync: {reconciled} reconciled, {pulled} pulled");
        }
        IssueCmd::SyncAll { project, no_pull } => {
            let (slug, url) = issue_ctx()?;
            let project = resolve_project_filter(&url, project).await?;
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
        } => {
            let repo_id = build_repo_id(&repo, github, base_url)?;
            let mut body = resolve_body(body)?;

            // Resolve the issue number this PR should close:
            // explicit --closes wins; else look up the linked
            // forge issue for --close-task. Capture the task id
            // so we can record a PR link afterward.
            let (slug, url) = issue_ctx()?;
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
                let (slug, url) = issue_ctx()?;
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
        IssueCmd::MergeQueue {
            repo,
            github,
            base_url,
            method,
            issue,
            dry_run,
            keep_going,
        } => {
            use git_config::BindingStore as _;
            let repo_id = build_repo_id(&repo, github, base_url)?;
            let merge_method = match method.to_ascii_lowercase().as_str() {
                "merge" => git_proto::reviews::MergeMethod::Merge,
                "squash" => git_proto::reviews::MergeMethod::Squash,
                "rebase" => git_proto::reviews::MergeMethod::Rebase,
                _ => return Err(eyre::eyre!("--method must be merge, squash, or rebase")),
            };
            let (slug, url) = issue_ctx()?;
            let client = connect_task_client(&url).await?;
            let store = forge_link_store(&slug)?;

            // Map PR number → task id for this repo, via the link
            // store (Pull-kind links). Lets us close each PR's task
            // as it lands, and scope the queue to one issue.
            let tasks = client
                .list()
                .await
                .map_err(|e| eyre::eyre!("list: {e:?}"))?;
            let mut pr_task: std::collections::HashMap<u64, uuid::Uuid> =
                std::collections::HashMap::new();
            for t in &tasks {
                let links = store
                    .issues_for_task(&t.id.to_string())
                    .map_err(|e| eyre::eyre!("link store: {e}"))?;
                for l in links {
                    if l.repo == repo_id && l.kind == git_config::LinkKind::Pull {
                        pr_task.insert(l.number, t.id);
                    }
                }
            }

            // When scoped to an issue, the eligible PRs are those
            // linked to its subtasks (tasks whose workflow.parent is
            // the issue) — plus the issue itself.
            let scope: Option<std::collections::HashSet<uuid::Uuid>> = match &issue {
                None => None,
                Some(r) => {
                    let parent = resolve_issue_id(&client, r).await?;
                    let mut set: std::collections::HashSet<uuid::Uuid> =
                        std::iter::once(parent.id).collect();
                    for t in &tasks {
                        if t.workflow.as_ref().and_then(|w| w.parent) == Some(parent.id) {
                            set.insert(t.id);
                        }
                    }
                    Some(set)
                }
            };

            // Open, non-draft PRs, oldest first (PR number order).
            let repo_c = repo_id.clone();
            let mut prs = tokio::task::spawn_blocking(move || {
                let backend = forge_backend_for(&repo_c)?;
                backend
                    .list_pull_requests(&repo_c)
                    .map_err(|e| eyre::eyre!("list_pull_requests: {e:?}"))
            })
            .await
            .map_err(|e| eyre::eyre!("join: {e}"))??;
            prs.retain(|pr| {
                matches!(pr.state, git_proto::PullRequestState::Open)
                    && !pr.draft
                    && match &scope {
                        None => true,
                        Some(set) => pr_task.get(&pr.id.0).is_some_and(|tid| set.contains(tid)),
                    }
            });
            prs.sort_by_key(|pr| pr.id.0);

            if prs.is_empty() {
                println!("(no mergeable PRs in the queue)");
                return Ok(());
            }
            println!("merge queue: {} PR(s) on {}", prs.len(), repo);
            for pr in &prs {
                let who = pr_task
                    .get(&pr.id.0)
                    .map(|t| format!("  → task {}", short_uuid(t)))
                    .unwrap_or_default();
                println!(
                    "  #{:<5} {} ({} <- {}){who}",
                    pr.id.0, pr.title, pr.base, pr.head
                );
            }
            if dry_run {
                println!("\n(dry run — nothing merged; method would be {method})");
                return Ok(());
            }

            let mut merged = 0usize;
            for pr in &prs {
                let number = pr.id.0;
                let repo_c = repo_id.clone();
                let res = tokio::task::spawn_blocking(move || {
                    let backend = forge_backend_for(&repo_c)?;
                    backend
                        .merge_pull_request(&repo_c, git_proto::PullRequestId(number), merge_method)
                        .map_err(|e| eyre::eyre!("merge #{number}: {e:?}"))
                })
                .await
                .map_err(|e| eyre::eyre!("join: {e}"))?;
                match res {
                    Ok(sha) => {
                        merged += 1;
                        match sha {
                            Some(s) => println!("✓ merged #{number} ({s})"),
                            None => println!("✓ merged #{number}"),
                        }
                        // Close the PR's linked task + propagate.
                        if let Some(tid) = pr_task.get(&number) {
                            if let Ok(mut t) = client.get(*tid).await {
                                t.status = "done".into();
                                t.completed_date = Some(chrono::Local::now().date_naive());
                                if let Some(w) = t.workflow.as_mut() {
                                    w.session = None;
                                }
                                if client.update(t).await.is_ok() {
                                    println!("    closed task {}", short_uuid(tid));
                                    let _ = propagate_state_to_forge(
                                        &slug,
                                        tid,
                                        git_proto::IssueState::Closed,
                                    )
                                    .await;
                                }
                            }
                        }
                    }
                    Err(e) => {
                        println!("✗ #{number} did not merge: {e}");
                        if keep_going {
                            println!("    (--keep-going: continuing)");
                        } else {
                            println!(
                                "    stopping — rebase #{number} onto {} and re-run the queue",
                                pr.base
                            );
                            break;
                        }
                    }
                }
            }
            println!("\nmerged {merged}/{} queued PR(s)", prs.len());
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

    // 1. Reconcile already-linked issues with the per-field resolver
    //    in `git_config::sync` over the scalar forge-owned projection
    //    (title / body / state). Provenance is a value-diff against
    //    the last-converged snapshot the store persists per link, so
    //    a forge edit lands locally while a local-only edit to a
    //    forge-owned field isn't clobbered, and Task-owned fields
    //    (priority/cycle/project/estimate/agent-attribution) are never
    //    in the projection so they always survive a forge edit.
    //
    //    Both directions are wired: forge→Task below, and Task→forge
    //    (pushing task-won forge-owned edits via `update_issue`) after
    //    the merge. FUTURE (issue #127 follow-up): extend the
    //    projection past the scalar fields to labels / assignees /
    //    milestone with their richer mapping.
    let local = client
        .list()
        .await
        .map_err(|e| eyre::eyre!("list: {e:?}"))?;
    let mut reconciled = 0usize;
    for t in &local {
        let links = store
            .issues_for_task(&t.id.to_string())
            .map_err(|e| eyre::eyre!("link store: {e}"))?;
        // Only reconcile *issue* links. A task also linked to its own
        // PR (via `task code push`) must not have its title/body/state
        // synced from that PR — the PR's title is a commit subject, not
        // the issue's.
        let Some(link) = links
            .iter()
            .find(|l| &l.repo == repo_id && l.kind == git_config::LinkKind::Issue)
        else {
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

        // Trim title/body on both sides: the task-note parser strips
        // leading/trailing whitespace from the markdown body, so a
        // freshly-pulled `t.details` is never byte-identical to the
        // forge body (which often carries a leading newline). Comparing
        // trimmed values keeps that cosmetic difference from looking
        // like a real edit and churning the sync every run.
        let task_proj = git_config::SyncedFields {
            title: t.title.trim().to_string(),
            body: t.details.trim().to_string(),
            closed: t.status == "done",
        };
        let forge_proj = git_config::SyncedFields {
            title: ext.title.trim().to_string(),
            body: ext.body.trim().to_string(),
            closed: matches!(ext.state, git_proto::IssueState::Closed),
        };
        // The forge issue's last-update time (parsed from the DTO's
        // RFC-3339 string) and the recorded snapshot.
        let forge_ts = ext
            .updated_at
            .as_deref()
            .and_then(|s| chrono::DateTime::parse_from_rfc3339(s).ok())
            .map(|d| d.with_timezone(&chrono::Utc));
        let snap = store
            .get_issue_snapshot(repo_id, number)
            .map_err(|e| eyre::eyre!("snapshot: {e}"))?;

        // Fast-path: when the forge's `updated_at` hasn't advanced past
        // the snapshot and the Task projection is unchanged, neither
        // side moved — skip the diff + writes for this issue entirely.
        if let (Some(s), Some(cur)) = (snap.as_ref(), forge_ts) {
            if s.forge_updated_at == Some(cur) && s.task == task_proj {
                continue;
            }
        }

        // Baseline: the recorded snapshot, or — on the first reconcile
        // of this link — the *task* projection on both sides. That way
        // a field the forge disagrees on registers as a forge-side
        // change (forge != baseline, task == baseline) and the
        // substrate wins for its owned fields, preserving the prior
        // "forge wins for state" behaviour; a freshly-pulled task
        // already equals the forge, so this is a no-op that just seeds
        // the snapshot.
        let (base_task, base_forge) = match &snap {
            Some(s) => (s.task.clone(), s.forge.clone()),
            None => (task_proj.clone(), task_proj.clone()),
        };
        let merged = git_config::reconcile_synced(&base_task, &base_forge, &task_proj, &forge_proj);

        if merged != task_proj {
            let mut t2 = t.clone();
            t2.title = merged.title.clone();
            t2.details = merged.body.clone();
            // State projection: only cross the done boundary, leaving
            // non-done statuses (in-progress/waiting/…) intact.
            let was_done = t2.status == "done";
            if merged.closed && !was_done {
                t2.status = "done".into();
                t2.completed_date = Some(chrono::Local::now().date_naive());
                if let Some(w) = t2.workflow.as_mut() {
                    w.session = None;
                }
            } else if !merged.closed && was_done {
                t2.status = "open".into();
                t2.completed_date = None;
            }
            client
                .update(t2)
                .await
                .map_err(|e| eyre::eyre!("update: {e:?}"))?;
            reconciled += 1;
            println!("  reconciled {} #{number}", short_uuid(&t.id));
        }

        // Push the Task→forge half: any forge-owned field the Task won
        // (a local edit the forge hadn't moved) is written back via
        // `update_issue` so both sides converge. On success the forge
        // baseline advances to *what the forge returned* (not what we
        // sent), so any server-side normalization doesn't ping-pong;
        // on failure we warn and leave the baseline at `forge_proj` so
        // the next sync retries.
        let fu = git_config::forge_update(&forge_proj, &merged);
        let (forge_base, forge_ts_after) = if fu.is_empty() {
            (forge_proj, forge_ts)
        } else {
            let repo_c = repo_id.clone();
            let update = git_proto::issues::IssueUpdate {
                title: fu.title,
                body: fu.body,
                state: fu.closed.map(|c| {
                    if c {
                        git_proto::IssueState::Closed
                    } else {
                        git_proto::IssueState::Open
                    }
                }),
                labels: None,
                assignees: None,
                milestone: None,
            };
            let pushed = tokio::task::spawn_blocking(move || {
                let backend = forge_backend_for(&repo_c)?;
                backend
                    .update_issue(&repo_c, git_proto::IssueId(number), update)
                    .map_err(|e| eyre::eyre!("update_issue #{number}: {e:?}"))
            })
            .await
            .map_err(|e| eyre::eyre!("join: {e}"))?;
            match pushed {
                Ok(issue) => {
                    println!("  pushed {} -> #{number}", short_uuid(&t.id));
                    // The forge bumped its `updated_at` on this write —
                    // record what it returned so the next sync's
                    // fast-path sees the new baseline, not a stale one.
                    let ts = issue
                        .updated_at
                        .as_deref()
                        .and_then(|s| chrono::DateTime::parse_from_rfc3339(s).ok())
                        .map(|d| d.with_timezone(&chrono::Utc));
                    let fields = git_config::SyncedFields {
                        title: issue.title,
                        body: issue.body,
                        closed: matches!(issue.state, git_proto::IssueState::Closed),
                    };
                    (fields, ts)
                }
                Err(e) => {
                    eprintln!("  warn: push to #{number} failed: {e}");
                    (forge_proj, forge_ts)
                }
            }
        };

        // Record the new baseline: Task holds `merged`, forge holds
        // `forge_base` (== `merged` after a successful push, else its
        // pre-push value pending a retry), with the forge's
        // `updated_at` so the next run's fast-path can short-circuit.
        store
            .set_issue_snapshot(
                repo_id,
                number,
                git_config::IssueSnapshot {
                    task: merged,
                    forge: forge_base,
                    forge_updated_at: forge_ts_after,
                },
            )
            .map_err(|e| eyre::eyre!("snapshot: {e}"))?;
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

// ── git helpers for `task code` ──────────────────────────────

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
            worktree,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&url).await?;
            let mut t = resolve_issue_id(&client, &id).await?;
            let short = t.id.simple().to_string()[..8].to_string();
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

            if worktree {
                // Isolated worktree → parallel agents don't collide.
                // CRITICAL: place it as a SIBLING of the main repo,
                // not nested inside it. The workspace has relative
                // path-deps that point outside the repo (`../architect`,
                // `../FastTrackStudio/...`); a sibling worktree resolves
                // `../` to the same parent, so those deps still work. A
                // nested worktree would break them.
                let repo_root = git(&["rev-parse", "--show-toplevel"])?;
                let root = std::path::Path::new(&repo_root);
                let parent = root
                    .parent()
                    .ok_or_else(|| eyre::eyre!("repo has no parent dir"))?;
                let repo_name = root
                    .file_name()
                    .map_or_else(|| "repo".to_string(), |s| s.to_string_lossy().to_string());
                let wt_path = parent.join(format!("{repo_name}-wt-{short}-{title_slug}"));
                if wt_path.exists() {
                    return Err(eyre::eyre!(
                        "worktree already exists at {} — `task code cleanup {short}` to remove it",
                        wt_path.display()
                    ));
                }
                git(&["worktree", "add", "-b", &branch, &wt_path.to_string_lossy()])?;
                println!("started {short} in worktree (branch {branch})");
                println!("  work in: {}", wt_path.display());
                println!("  then: cd into it and run `task code commit` / `task code push` there");
                // Share the main repo's build cache so cargo in the
                // worktree doesn't compile from scratch. The git
                // hooks set this automatically; print it for the
                // agent's own `cargo` invocations.
                println!("  for fast builds: export CARGO_TARGET_DIR={repo_root}/target");
            } else {
                git(&["switch", "-c", &branch])?;
                println!("started {short} on branch {branch}");
            }

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
        }
        CodeCmd::Worktrees { json } => {
            // `git worktree list --porcelain` → show the task ones.
            let out = git(&["worktree", "list", "--porcelain"])?;
            let mut path = String::new();
            let mut rows: Vec<(String, String)> = Vec::new();
            for line in out.lines() {
                if let Some(p) = line.strip_prefix("worktree ") {
                    path = p.to_string();
                } else if let Some(b) = line.strip_prefix("branch ") {
                    let b = b.trim_start_matches("refs/heads/");
                    if b.starts_with("task/") {
                        rows.push((b.to_string(), path.clone()));
                    }
                }
            }
            if json {
                let out: Vec<serde_json::Value> = rows
                    .iter()
                    .map(|(branch, path)| serde_json::json!({ "branch": branch, "path": path }))
                    .collect();
                json_out::print_json(&out)?;
                return Ok(());
            }
            if rows.is_empty() {
                println!("(no task worktrees)");
            }
            for (branch, path) in rows {
                println!("{branch}\n  {path}");
            }
        }
        CodeCmd::Cleanup { id } => {
            // Resolve the worktree dir from the short-id or branch.
            let out = git(&["worktree", "list", "--porcelain"])?;
            let mut path = String::new();
            let mut target: Option<String> = None;
            for line in out.lines() {
                if let Some(p) = line.strip_prefix("worktree ") {
                    path = p.to_string();
                } else if let Some(b) = line.strip_prefix("branch ") {
                    let b = b.trim_start_matches("refs/heads/");
                    let matches =
                        b == id || b.starts_with(&format!("task/{id}-")) || b.contains(&id);
                    if matches && b.starts_with("task/") {
                        target = Some(path.clone());
                    }
                }
            }
            let Some(dir) = target else {
                return Err(eyre::eyre!("no task worktree matching `{id}`"));
            };
            git(&["worktree", "remove", "--force", &dir])?;
            println!("removed worktree {dir}");
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
        CodeCmd::Status { org, server, json } => {
            let branch = current_branch()?;
            if !json {
                println!("branch:  {branch}");
            }
            let Some(short) = task_short_from_branch(&branch) else {
                if json {
                    json_out::print_json(&serde_json::json!({
                        "branch": branch,
                        "task": null,
                        "links": [],
                    }))?;
                } else {
                    println!("task:    (branch isn't a task/<id>-… branch)");
                }
                return Ok(());
            };
            let slug = resolve_active_org(org)?;
            let vox = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&vox).await?;
            let t = resolve_issue_id(&client, &short).await?;
            let store = forge_link_store(&slug)?;
            let links = store
                .issues_for_task(&t.id.to_string())
                .map_err(|e| eyre::eyre!("link store: {e}"))?;
            if json {
                let link_rows: Vec<serde_json::Value> = links
                    .iter()
                    .map(|l| {
                        let kind = match l.kind {
                            git_config::LinkKind::Issue => "issue",
                            git_config::LinkKind::Pull => "pr",
                        };
                        serde_json::json!({
                            "kind": kind,
                            "owner": l.repo.owner,
                            "repo": l.repo.repo,
                            "number": l.number,
                        })
                    })
                    .collect();
                json_out::print_json(&serde_json::json!({
                    "branch": branch,
                    "task": {
                        "id": t.id,
                        "short": short,
                        "status": t.status,
                        "title": t.title,
                        "path": t.path,
                    },
                    "links": link_rows,
                }))?;
                return Ok(());
            }
            println!("task:    {} [{}]  {}", short, t.status, t.title);
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
            json,
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
            if open.is_empty() && !json {
                println!("(no parked tasks)");
                return Ok(());
            }
            if json {
                // Handoff entities + the joined task title.
                let mut rows: Vec<serde_json::Value> = Vec::with_capacity(open.len());
                for h in open {
                    let title = client.get(h.session_id).await.ok().map(|t| t.title);
                    let mut v = serde_json::to_value(h).unwrap_or(serde_json::Value::Null);
                    if let serde_json::Value::Object(map) = &mut v {
                        map.insert(
                            "task_short".into(),
                            h.session_id.simple().to_string()[..8].into(),
                        );
                        if let Some(t) = title {
                            map.insert("task_title".into(), t.into());
                        }
                    }
                    rows.push(v);
                }
                json_out::print_json(&rows)?;
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
            project,
            org,
        } => {
            let slug = resolve_active_org(org)?;
            let mut labels = load_labels(&slug)?;
            if let Some(existing) = labels
                .iter_mut()
                .find(|l| l.name.eq_ignore_ascii_case(&name))
            {
                // Idempotent: update color/group/description/scope on re-create.
                existing.color = color.or(existing.color.take());
                existing.group = group.or(existing.group.take());
                existing.description = description.or(existing.description.take());
                existing.project_id = project.or(existing.project_id.take());
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
            l.project_id = project;
            labels.push(l);
            save_labels(&slug, &labels)?;
            println!("created label `{name}`");
        }
        LabelCmd::List { project, org, json } => {
            let slug = resolve_active_org(org)?;
            let mut labels = load_labels(&slug)?;
            // `--project` narrows to labels visible to that project:
            // its own labels plus the org-wide (unscoped) ones.
            if let Some(pid) = project {
                labels.retain(|l| l.project_id.is_none() || l.project_id == Some(pid));
            }
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
                // Mark project-scoped labels so they're distinguishable
                // from org-wide ones in the plain listing.
                let scope = l
                    .project_id
                    .map_or(String::new(), |p| format!(" (project {p})"));
                println!("{}{group}{color}{scope}", l.name);
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
            json,
        } => mutate_milestone(target, org, server, json, |m| m.status = status).await?,
        MilestoneCmd::SetDue {
            target,
            due,
            org,
            server,
            json,
        } => {
            let v = if matches!(due.as_str(), "none" | "null" | "") {
                None
            } else {
                Some(
                    chrono::NaiveDate::parse_from_str(&due, "%Y-%m-%d")
                        .map_err(|e| eyre::eyre!("--due: {e}"))?,
                )
            };
            mutate_milestone(target, org, server, json, |m| m.due_date = v).await?;
        }
        MilestoneCmd::SetGoal {
            target,
            goal,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org.clone())?;
            let url = resolve_org_vox_url(server.clone(), &slug);
            let new_goal = if matches!(goal.as_str(), "none" | "null" | "") {
                None
            } else {
                let gc = connect_goal_client(&url).await?;
                Some(resolve_goal_target(&gc, &goal).await?.id)
            };
            mutate_milestone(target, org, server, json, |m| m.goal_id = new_goal).await?;
        }
        MilestoneCmd::SetForgeRef {
            target,
            forge_ref,
            org,
            server,
            json,
        } => {
            let v = if matches!(forge_ref.as_str(), "none" | "null" | "") {
                None
            } else {
                Some(forge_ref)
            };
            mutate_milestone(target, org, server, json, |m| m.forge_ref = v).await?;
        }
        MilestoneCmd::Close {
            target,
            org,
            server,
            json,
        } => mutate_milestone(target, org, server, json, |m| m.status = "closed".into()).await?,
        MilestoneCmd::Reopen {
            target,
            org,
            server,
            json,
        } => mutate_milestone(target, org, server, json, |m| m.status = "open".into()).await?,
        MilestoneCmd::Rename {
            target,
            new_path,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_milestone_client(&url).await?;
            let m = resolve_milestone_target(&client, &target).await?;
            let renamed = client
                .rename(m.id, new_path)
                .await
                .map_err(|e| eyre::eyre!("rename: {e:?}"))?;
            if json {
                json_out::print_json(&renamed)?;
            } else {
                println!("renamed → {}", renamed.path);
            }
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
    establish_for_url(url).await
}

/// Resolve a milestone reference — uuid, vault path, title, or a
/// unique prefix of either (shared flexible resolver).
async fn resolve_milestone_target(
    client: &milestone::MilestoneServiceClient,
    target: &str,
) -> eyre::Result<milestone::Milestone> {
    json_out::resolve_milestone_flexible(client, target).await
}

async fn mutate_milestone<F>(
    target: String,
    org: Option<String>,
    server: Option<String>,
    json: bool,
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
    if json {
        json_out::print_json(&updated)?;
    } else {
        println!("{}  [{}]  {}", updated.title, updated.status, updated.path);
    }
    Ok(())
}

// ── Location (locations::Store) ──────────────────────────────────────

async fn connect_threads_client(url: &str) -> eyre::Result<threads::ThreadsServiceClient> {
    establish_for_url(url).await
}

/// Resolve `(org_id, local_user_id)` for CLI-authored threads, matching
/// the timer CLI's identity derivation so UI + CLI share a keyspace.
fn threads_local_ids(org_override: Option<&str>) -> (uuid::Uuid, uuid::Uuid) {
    let org_id = org_ctx::resolve_active(org_override)
        .ok()
        .and_then(|ctx| ctx.root.manifest().ok().map(|m| m.id))
        .unwrap_or_else(uuid::Uuid::nil);
    (org_id, timer_owner_id(org_id))
}

/// Resolve a threads `--entity-id` reference (uuid, id prefix, path,
/// or title) against the service named by `--entity-type`. Unknown
/// entity types still take a literal UUID.
async fn resolve_thread_entity(
    url: &str,
    entity_type: &str,
    target: &str,
) -> eyre::Result<uuid::Uuid> {
    if let Ok(id) = uuid::Uuid::parse_str(target) {
        return Ok(id);
    }
    match entity_type {
        "task" => {
            let tc = connect_task_client(url).await?;
            Ok(json_out::resolve_task_flexible(&tc, target).await?.id)
        }
        "project" => {
            let pc = connect_project_client(url).await?;
            Ok(json_out::resolve_project_flexible(&pc, target).await?.id)
        }
        other => Err(errors::usage("resolve --entity-id")
            .cause(format!(
                "`{target}` is not a UUID and entity type `{other}` has no name resolver"
            ))
            .hint("pass a literal UUID, or use --entity-type task|project")
            .report()),
    }
}

#[allow(clippy::too_many_lines)]
async fn run_threads(cmd: ThreadsCmd) -> eyre::Result<()> {
    // Global --org / --server routing, shared by every arm.
    let slug = resolve_active_org(None)?;
    let url = resolve_org_vox_url(None, &slug);
    match cmd {
        ThreadsCmd::New {
            entity_type,
            entity_id,
            title,
            kind,
        } => {
            let title = title.join(" ");
            if title.trim().is_empty() {
                eyre::bail!("a thread needs a title — pass some text");
            }
            let entity_id = resolve_thread_entity(&url, &entity_type, &entity_id).await?;
            let (org_id, user_id) = threads_local_ids(None);
            let client = connect_threads_client(&url).await?;
            let t = client
                .create_thread(threads::CreateThreadRequest {
                    org_id,
                    entity_type,
                    entity_id,
                    title,
                    kind: kind.unwrap_or_default(),
                    created_by: user_id,
                    source_kind: "native".into(),
                    source_ref: None,
                    source_url: None,
                })
                .await
                .map_err(|e| eyre::eyre!("create_thread: {e:?}"))?;
            println!("created thread {}  {}", t.id, t.title);
        }
        ThreadsCmd::Post {
            thread_id,
            text,
            reply_to,
            source,
            author,
        } => {
            let body = text.join(" ");
            if body.trim().is_empty() {
                eyre::bail!("nothing to post — pass some message text");
            }
            let (org_id, user_id) = threads_local_ids(None);
            let client = connect_threads_client(&url).await?;
            let source_kind = source.unwrap_or_else(|| "native".into());
            let author_label = author.unwrap_or_else(|| {
                if source_kind == "agent" {
                    "agent".into()
                } else {
                    "cli".into()
                }
            });
            let m = client
                .post_message(threads::PostMessageRequest {
                    thread_id,
                    org_id,
                    author_id: Some(user_id),
                    author_label,
                    body,
                    reply_to,
                    source_kind,
                    external_id: None,
                    original_text: None,
                    source_url: None,
                    posted_at: None,
                })
                .await
                .map_err(|e| eyre::eyre!("post_message: {e:?}"))?;
            println!("posted {}", m.id);
        }
        ThreadsCmd::List {
            entity_type,
            entity_id,
            json,
        } => {
            let entity_id = resolve_thread_entity(&url, &entity_type, &entity_id).await?;
            let client = connect_threads_client(&url).await?;
            let rows = client
                .list_threads(entity_type, entity_id)
                .await
                .map_err(|e| eyre::eyre!("list_threads: {e:?}"))?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&rows).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            println!("{} threads", rows.len());
            for t in rows {
                let r = if t.resolved { " (resolved)" } else { "" };
                println!("  {}  [{}]{}  {}", t.id, t.kind, r, t.title);
            }
        }
        ThreadsCmd::Show { thread_id, json } => {
            let client = connect_threads_client(&url).await?;
            let msgs = client
                .list_messages(thread_id)
                .await
                .map_err(|e| eyre::eyre!("list_messages: {e:?}"))?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&msgs).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            println!("{} messages", msgs.len());
            for m in msgs {
                println!(
                    "  [{}] {}: {}",
                    m.posted_at.format("%Y-%m-%d %H:%M"),
                    m.author_label,
                    m.body
                );
            }
        }
        ThreadsCmd::Resolve {
            thread_id,
            unresolve,
        } => {
            let (_org_id, user_id) = threads_local_ids(None);
            let client = connect_threads_client(&url).await?;
            let t = client
                .set_resolved(thread_id, !unresolve, Some(user_id))
                .await
                .map_err(|e| eyre::eyre!("set_resolved: {e:?}"))?;
            println!("thread {} resolved={}", t.id, t.resolved);
        }
        ThreadsCmd::Rm { id } => {
            let client = connect_threads_client(&url).await?;
            client
                .delete_thread(id)
                .await
                .map_err(|e| eyre::eyre!("delete_thread: {e:?}"))?;
            println!("deleted thread {id}");
        }
    }
    Ok(())
}

async fn connect_inbox_client(url: &str) -> eyre::Result<inbox_proto::InboxClient> {
    establish_for_url(url).await
}

#[allow(clippy::too_many_lines)]
async fn run_inbox(cmd: InboxCmd) -> eyre::Result<()> {
    match cmd {
        InboxCmd::Add {
            text,
            kind,
            source,
            org,
            server,
        } => {
            let body = text.join(" ");
            if body.trim().is_empty() {
                eyre::bail!("nothing to capture — pass some note text");
            }
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_inbox_client(&u).await?;
            let id = uuid::Uuid::new_v4().to_string();
            let created = chrono::Utc::now().to_rfc3339();
            let mut item = inbox_proto::InboxItem::capture(
                id.clone(),
                body,
                source.unwrap_or_else(|| "cli".into()),
                created,
            );
            if let Some(k) = kind {
                item.kind = k;
            }
            client
                .upsert_inbox_item(item)
                .await
                .map_err(|e| eyre::eyre!("capture: {e:?}"))?;
            println!("captured {id}");
        }
        InboxCmd::Suggest {
            text,
            source,
            link,
            kind,
            org,
            server,
        } => {
            let mut body = text.join(" ");
            if body.trim().is_empty() {
                eyre::bail!("nothing to suggest — pass some text");
            }
            if let Some(l) = link {
                body.push_str(&format!("\n\n[open original]({l})"));
            }
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_inbox_client(&u).await?;
            let id = uuid::Uuid::new_v4().to_string();
            let created = chrono::Utc::now().to_rfc3339();
            let mut item = inbox_proto::InboxItem::capture(
                id.clone(),
                body,
                source.unwrap_or_else(|| "agent".into()),
                created,
            );
            item.status = inbox_proto::InboxItem::STATUS_SUGGESTED.to_string();
            if let Some(k) = kind {
                item.kind = k;
            }
            client
                .upsert_inbox_item(item)
                .await
                .map_err(|e| eyre::eyre!("suggest: {e:?}"))?;
            println!("suggested {id}");
        }
        InboxCmd::List {
            all,
            json,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_inbox_client(&u).await?;
            // The daily-review queue: open items whose snooze (if any)
            // has elapsed. `--all` bypasses both filters.
            let today = chrono::Utc::now().date_naive().to_string();
            let rows: Vec<_> = client
                .list_inbox()
                .await
                .map_err(|e| eyre::eyre!("list: {e:?}"))?
                .into_iter()
                .filter(|it| {
                    all || (it.is_open()
                        && it
                            .resurface_on
                            .as_deref()
                            .is_none_or(|d| d <= today.as_str()))
                })
                .collect();
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&rows).map_err(|e| eyre::eyre!("json: {e}"))?
                );
                return Ok(());
            }
            if rows.is_empty() {
                println!("inbox empty — nothing to review 🎉");
                return Ok(());
            }
            for it in &rows {
                let first_line = it.body.lines().next().unwrap_or("").trim();
                let date = it.created.get(..10).unwrap_or(&it.created);
                let snooze = it
                    .resurface_on
                    .as_deref()
                    .map(|d| format!("  💤 {d}"))
                    .unwrap_or_default();
                println!(
                    "{:<8}  {date}  {:<10}  {:<9}  {first_line}{snooze}",
                    it.id.get(..8).unwrap_or(&it.id),
                    it.kind,
                    it.status,
                );
            }
        }
        InboxCmd::Mark {
            id,
            status,
            into,
            org,
            server,
        } => {
            let allowed = [
                inbox_proto::InboxItem::STATUS_OPEN,
                inbox_proto::InboxItem::STATUS_PROCESSED,
                inbox_proto::InboxItem::STATUS_ARCHIVED,
            ];
            if !allowed.contains(&status.as_str()) {
                eyre::bail!("status must be one of: open, processed, archived");
            }
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_inbox_client(&u).await?;
            let mut item = client
                .get_inbox_item(id.clone())
                .await
                .map_err(|e| eyre::eyre!("get `{id}`: {e:?}"))?;
            item.status = status.clone();
            if into.is_some() {
                item.processed_into = into;
            }
            client
                .upsert_inbox_item(item)
                .await
                .map_err(|e| eyre::eyre!("mark: {e:?}"))?;
            println!("{id} → {status}");
        }
        InboxCmd::Snooze {
            id,
            until,
            org,
            server,
        } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_inbox_client(&u).await?;
            let mut item = client
                .get_inbox_item(id.clone())
                .await
                .map_err(|e| eyre::eyre!("get `{id}`: {e:?}"))?;
            item.resurface_on = Some(until.clone());
            client
                .upsert_inbox_item(item)
                .await
                .map_err(|e| eyre::eyre!("snooze: {e:?}"))?;
            println!("{id} snoozed until {until}");
        }
        InboxCmd::Rm { id, org, server } => {
            let slug = resolve_active_org(org)?;
            let u = resolve_org_vox_url(server, &slug);
            let client = connect_inbox_client(&u).await?;
            client
                .delete_inbox_item(id.clone())
                .await
                .map_err(|e| eyre::eyre!("delete: {e:?}"))?;
            println!("deleted {id}");
        }
        InboxCmd::Process {
            model,
            dry_run,
            yes,
            heuristic,
            timeout_secs,
            org,
            server,
        } => {
            run_inbox_process(model, dry_run, yes, heuristic, timeout_secs, org, server).await?;
        }
    }
    Ok(())
}

// ── Inbox AI processing pass ─────────────────────────────────────────
//
// `task inbox process` — the "daily processing pass" from
// plans/relevancy-and-inbox.md: one LLM turn over every open
// fleeting item proposes a `task` / `note` / `skip` promotion per
// item; the user reviews each proposal (y / n / e-dit title, or
// `--yes` for all) and accepted ones are applied through the
// existing service surfaces:
//
//   task → `task::capture(title)` (tags/contexts/dates), project id
//          from the proposed title via direct match or
//          `task::infer_project_id`, `TaskService::create`, then the
//          inbox item is marked processed with `processed_into` =
//          the created task's vault path.
//   note → written into the active org's local vault via
//          `vault_obsidian::create_page` when `<org>/vault/` exists
//          locally; otherwise printed for manual creation and marked
//          processed only on explicit confirm.
//   skip → offer `archived`.

/// What the user chose for one proposal.
enum ProcessDecision {
    Accept,
    /// Accept a task proposal with a replacement title.
    EditTitle(String),
    Decline,
    Quit,
}

fn prompt_process_decision(is_task: bool) -> eyre::Result<ProcessDecision> {
    use std::io::{BufRead as _, Write as _};
    let opts = if is_task {
        "[y]es / [e]dit title / [n]o / [q]uit"
    } else {
        "[y]es / [n]o / [q]uit"
    };
    loop {
        print!("  apply? {opts} > ");
        std::io::stdout().flush()?;
        let mut line = String::new();
        std::io::stdin().lock().read_line(&mut line)?;
        match line.trim().to_ascii_lowercase().as_str() {
            "y" | "yes" => return Ok(ProcessDecision::Accept),
            "n" | "no" | "" => return Ok(ProcessDecision::Decline),
            "q" | "quit" => return Ok(ProcessDecision::Quit),
            "e" | "edit" if is_task => {
                print!("  new title > ");
                std::io::stdout().flush()?;
                let mut t = String::new();
                std::io::stdin().lock().read_line(&mut t)?;
                let t = t.trim().to_string();
                if t.is_empty() {
                    println!("  (empty title — keeping the proposal)");
                    return Ok(ProcessDecision::Accept);
                }
                return Ok(ProcessDecision::EditTitle(t));
            }
            other => println!("  unrecognized `{other}`"),
        }
    }
}

/// One-line human rendering of a proposal action.
fn describe_proposal(action: &agent_inbox::ProposalAction) -> String {
    match action {
        agent_inbox::ProposalAction::Task {
            title,
            project_title,
            contexts,
            due,
        } => {
            let mut extras = Vec::new();
            if let Some(p) = project_title {
                extras.push(format!("project {p}"));
            }
            if !contexts.is_empty() {
                extras.push(format!("contexts {}", contexts.join(" ")));
            }
            if let Some(d) = due {
                extras.push(format!("due {d}"));
            }
            let suffix = if extras.is_empty() {
                String::new()
            } else {
                format!("  ({})", extras.join(", "))
            };
            format!("task: \"{title}\"{suffix}")
        }
        agent_inbox::ProposalAction::Note { path, body } => {
            let first = body.lines().next().unwrap_or("").trim();
            format!("note: {path}  \"{first}\"")
        }
        agent_inbox::ProposalAction::Skip { reason } => format!("skip: {reason}"),
    }
}

/// Deterministic no-LLM proposal: everything becomes a task whose
/// capture input is the item body's first line (`task::capture`
/// extracts tags / contexts / dates from it at apply time).
fn heuristic_proposal(item: &inbox_proto::InboxItem) -> agent_inbox::Proposal {
    let first_line = item.body.lines().next().unwrap_or("").trim();
    agent_inbox::Proposal {
        item_id: item.id.clone(),
        action: agent_inbox::ProposalAction::Task {
            title: if first_line.is_empty() {
                "Untitled task".to_string()
            } else {
                first_line.to_string()
            },
            project_title: None,
            contexts: Vec::new(),
            due: None,
        },
    }
}

/// Set an inbox item's status (and optional provenance) via the
/// standard get-mutate-upsert cycle the `mark` verb uses.
async fn mark_inbox_item(
    client: &inbox_proto::InboxClient,
    item: &inbox_proto::InboxItem,
    status: &str,
    processed_into: Option<String>,
) -> eyre::Result<()> {
    let mut updated = item.clone();
    updated.status = status.to_string();
    if processed_into.is_some() {
        updated.processed_into = processed_into;
    }
    client
        .upsert_inbox_item(updated)
        .await
        .map_err(|e| eyre::eyre!("mark {}: {e:?}", item.id))?;
    Ok(())
}

#[allow(clippy::too_many_lines)]
async fn run_inbox_process(
    model: Option<String>,
    dry_run: bool,
    yes: bool,
    heuristic: bool,
    timeout_secs: u64,
    org: Option<String>,
    server: Option<String>,
) -> eyre::Result<()> {
    use std::io::IsTerminal as _;

    let slug = resolve_active_org(org)?;
    let url = resolve_org_vox_url(server, &slug);
    let inbox = connect_inbox_client(&url).await?;

    // The daily queue: open items whose snooze (if any) elapsed,
    // oldest first — same filter as `task inbox list`.
    let today = chrono::Utc::now().date_naive().to_string();
    let mut items: Vec<inbox_proto::InboxItem> = inbox
        .list_inbox()
        .await
        .map_err(|e| eyre::eyre!("list inbox: {e:?}"))?
        .into_iter()
        .filter(|it| {
            it.is_open()
                && it
                    .resurface_on
                    .as_deref()
                    .is_none_or(|d| d <= today.as_str())
        })
        .collect();
    items.sort_by(|a, b| a.created.cmp(&b.created));
    if items.is_empty() {
        println!("inbox empty — nothing to process 🎉");
        return Ok(());
    }

    if !dry_run && !yes && !std::io::stdin().is_terminal() {
        eyre::bail!(
            "stdin is not a terminal — the review loop is interactive; \
             rerun with --yes to accept all proposals or --dry-run to only print them"
        );
    }

    // Project list — both the prompt vocabulary and the apply-time
    // title → id resolver.
    let pc = connect_project_client(&url).await?;
    let known_projects: Vec<(uuid::Uuid, String)> = pc
        .list()
        .await
        .map_err(|e| eyre::eyre!("list projects: {e:?}"))?
        .into_iter()
        .map(|p| (p.id, p.title))
        .collect();

    // Local org checkout (when there is one): the LLM turn's
    // workspace root + the vault dir notes get written into.
    let local_org_root = org_ctx::resolve_active(Some(slug.as_str()))
        .ok()
        .map(|ctx| ctx.root.path().to_path_buf());
    let vault_dir = local_org_root
        .as_ref()
        .map(|r| r.join("vault"))
        .filter(|p| p.is_dir());

    // ── Propose ────────────────────────────────────────────────
    let proposals: Vec<agent_inbox::Proposal> = if heuristic {
        items.iter().map(heuristic_proposal).collect()
    } else {
        let req = agent_inbox::bridge::ProcessRequest {
            items: items
                .iter()
                .map(|it| agent_inbox::bridge::ProcessItem {
                    id: it.id.clone(),
                    body: it.body.clone(),
                    source: it.source.clone(),
                    created: it.created.clone(),
                })
                .collect(),
            project_titles: known_projects.iter().map(|(_, t)| t.clone()).collect(),
            today: today.clone(),
            model: model.clone(),
            timeout: std::time::Duration::from_secs(timeout_secs),
        };
        let workspace = match &local_org_root {
            Some(r) => r.clone(),
            None => std::env::current_dir().map_err(|e| eyre::eyre!("cwd: {e}"))?,
        };
        eprintln!(
            "› inbox process@{}  {} item(s), {} project(s)",
            model.as_deref().unwrap_or("default"),
            items.len(),
            known_projects.len()
        );
        let backend = agent_codex::CodexBackend::new();
        match agent_inbox::bridge::run_process(&backend, &workspace, req).await {
            Ok(p) => p,
            Err(e) => {
                return Err(eyre::eyre!(
                    "inbox process: {e}\n\nThis verb drives `codex app-server` — the same \
                     backend as `task wiki ingest`.\n  - check the `codex` CLI is installed and \
                     on $PATH (and signed in)\n  - or rerun with --heuristic for deterministic \
                     no-LLM proposals"
                ));
            }
        }
    };

    // ── Review + apply ─────────────────────────────────────────
    let total = items.len();
    let mut created_tasks = 0usize;
    let mut notes_written = 0usize;
    let mut archived = 0usize;
    let mut left_open = 0usize;

    let mut task_client: Option<task::TaskServiceClient> = None;
    'items: for (idx, item) in items.iter().enumerate() {
        let first_line = item.body.lines().next().unwrap_or("").trim();
        println!(
            "\n[{}/{total}] {}  {}  \"{first_line}\"",
            idx + 1,
            item.id.get(..8).unwrap_or(&item.id),
            item.created.get(..10).unwrap_or(&item.created),
        );
        let Some(proposal) = proposals.iter().find(|p| p.item_id == item.id) else {
            println!("  → (no proposal returned for this item — left open)");
            left_open += 1;
            continue;
        };
        println!("  → {}", describe_proposal(&proposal.action));
        if dry_run {
            continue;
        }

        let is_task = matches!(proposal.action, agent_inbox::ProposalAction::Task { .. });
        let decision = if yes {
            ProcessDecision::Accept
        } else {
            prompt_process_decision(is_task)?
        };
        let edited_title = match decision {
            ProcessDecision::Quit => {
                println!("stopping — remaining items left open");
                left_open += total - idx;
                break 'items;
            }
            ProcessDecision::Decline => {
                left_open += 1;
                continue;
            }
            ProcessDecision::EditTitle(t) => Some(t),
            ProcessDecision::Accept => None,
        };

        match &proposal.action {
            agent_inbox::ProposalAction::Task {
                title,
                project_title,
                contexts,
                due,
            } => {
                let capture_input = edited_title.as_deref().unwrap_or(title);
                let mut info = task::capture(capture_input);
                info.path = task::write::default_task_path(&info.title, None);
                // Merge proposal contexts into whatever `capture`
                // extracted from inline `@…` tokens.
                for c in contexts {
                    if !info.contexts.0.iter().any(|x| x.eq_ignore_ascii_case(c)) {
                        info.contexts.0.push(c.clone());
                    }
                }
                if info.due.is_none() {
                    info.due.clone_from(due);
                }
                // Project: proposed title (direct, case-insensitive
                // match against the provided list only), else any
                // `[[wikilink]]` the capture parser extracted.
                info.project_id = project_title
                    .as_deref()
                    .and_then(|t| {
                        known_projects
                            .iter()
                            .find(|(_, kt)| kt.eq_ignore_ascii_case(t))
                            .map(|(id, _)| *id)
                    })
                    .or_else(|| task::infer_project_id(&info.projects.0, &known_projects));
                if task_client.is_none() {
                    task_client = Some(connect_task_client(&url).await?);
                }
                let created = task_client
                    .as_ref()
                    .expect("task client connected above")
                    .create(info)
                    .await
                    .map_err(|e| eyre::eyre!("create task: {e:?}"))?;
                println!("  created task {} ({})", created.title, created.path);
                mark_inbox_item(
                    &inbox,
                    item,
                    inbox_proto::InboxItem::STATUS_PROCESSED,
                    Some(created.path),
                )
                .await?;
                created_tasks += 1;
            }
            agent_inbox::ProposalAction::Note { path, body } => {
                // Fall back to the raw capture when the LLM sent an
                // empty BODY — never write an empty note.
                let content = if body.trim().is_empty() {
                    item.body.as_str()
                } else {
                    body.as_str()
                };
                if let Some(dir) = &vault_dir {
                    let mut v = vault_obsidian::Vault::open(dir)
                        .map_err(|e| eyre::eyre!("open vault {}: {e}", dir.display()))?;
                    let guard = vault_obsidian::SelfWriteGuard::new();
                    match vault_obsidian::create_page(&mut v, path, &[], content, &guard) {
                        Ok(()) => {
                            println!("  wrote note {path}");
                            mark_inbox_item(
                                &inbox,
                                item,
                                inbox_proto::InboxItem::STATUS_PROCESSED,
                                Some(path.clone()),
                            )
                            .await?;
                            notes_written += 1;
                        }
                        Err(e) => {
                            println!("  could not write {path}: {e} — left open");
                            left_open += 1;
                        }
                    }
                } else {
                    // No local vault checkout for this org — print the
                    // note for manual creation; only an explicit
                    // confirm marks the item processed.
                    println!("  no local vault at <org>/vault — create it yourself:");
                    println!("  ── {path} ──");
                    for line in content.lines() {
                        println!("  {line}");
                    }
                    println!("  ──");
                    if yes {
                        println!("  (--yes: not marking processed without the note written)");
                        left_open += 1;
                    } else {
                        println!("  mark processed once you've created it?");
                        match prompt_process_decision(false)? {
                            ProcessDecision::Accept | ProcessDecision::EditTitle(_) => {
                                mark_inbox_item(
                                    &inbox,
                                    item,
                                    inbox_proto::InboxItem::STATUS_PROCESSED,
                                    Some(path.clone()),
                                )
                                .await?;
                                notes_written += 1;
                            }
                            ProcessDecision::Quit => {
                                println!("stopping — remaining items left open");
                                left_open += total - idx;
                                break 'items;
                            }
                            ProcessDecision::Decline => left_open += 1,
                        }
                    }
                }
            }
            agent_inbox::ProposalAction::Skip { .. } => {
                mark_inbox_item(&inbox, item, inbox_proto::InboxItem::STATUS_ARCHIVED, None)
                    .await?;
                println!("  archived");
                archived += 1;
            }
        }
    }

    if dry_run {
        println!("\n(dry run — nothing applied)");
    } else {
        println!(
            "\ndone: {created_tasks} task(s) created, {notes_written} note(s), \
             {archived} archived, {left_open} left open"
        );
    }
    Ok(())
}

async fn connect_locations_client(url: &str) -> eyre::Result<locations::LocationsServiceClient> {
    establish_for_url(url).await
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
        .ok_or_else(|| {
            errors::not_found("resolve target", target)
                .cause("no path or name match")
                .report()
        })
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
    establish_for_url(url).await
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
        RecipeCmd::Create(a) => return mealprep::recipe_create(a).await,
        RecipeCmd::Import(a) => return recipe_import::recipe_import(a).await,
        RecipeCmd::Update(a) => return mealprep::recipe_update(a).await,
        RecipeCmd::Show(a) => return mealprep::recipe_show(a).await,
        RecipeCmd::CanCook(a) => return mealprep::recipe_can_cook(a).await,
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
    establish_for_url(url).await
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
    rows.into_iter().find(|m| m.path == target).ok_or_else(|| {
        errors::not_found("resolve target", target)
            .cause("no path or name match")
            .report()
    })
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
            // Accept recipe display names as well as `.cook` paths.
            let recipe = mealprep::resolve_recipe_refs(&u, recipe).await?;
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
        MealCmd::Schedule(a) => return mealprep::meal_schedule(a).await,
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
    establish_for_url(url).await
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
        .ok_or_else(|| {
            errors::not_found("resolve target", target)
                .cause("no path or name match")
                .report()
        })
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
        establish_for_url(&url).await
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
    establish_for_url(url).await
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
        .ok_or_else(|| {
            errors::not_found("resolve target", target)
                .cause("no path or name match")
                .report()
        })
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
    establish_for_url(url).await
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
        .ok_or_else(|| {
            errors::not_found("resolve target", target)
                .cause("no path or name match")
                .report()
        })
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
    establish_for_url(url).await
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
        .ok_or_else(|| {
            errors::not_found("resolve target", target)
                .cause("no path or name match")
                .report()
        })
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
    rows.into_iter().find(|s| s.path == target).ok_or_else(|| {
        errors::not_found("resolve target", target)
            .cause("no path or name match")
            .report()
    })
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
    establish_for_url(url).await
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
        .ok_or_else(|| {
            errors::not_found("resolve target", target)
                .cause("no path or name match")
                .report()
        })
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
            .map(|s| s.active_slug())
            .filter(|s| !s.is_empty())
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

    let client: VaultSyncClient = establish_for_url(&url).await?;

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
