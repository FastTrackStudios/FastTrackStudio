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

mod brief;
mod collection;
mod cycle;
mod errors;
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
mod workstream;

use clap::{Parser, Subcommand};
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
enum AdminCmd {
    /// Run one server-native snapshot cycle: quiesce writes,
    /// WAL-checkpoint every open sqlite, commit the per-org +
    /// full-state git repos under `<data_root>/.gitstate/`, push
    /// when the server has a backup remote configured.
    Snapshot {
        /// Server URL (defaults like `task org create`).
        #[arg(long)]
        server: Option<String>,
    },
    /// Recent snapshot commits on the full-state repo.
    Log {
        /// Max commits to show.
        #[arg(long, default_value_t = 20)]
        limit: u32,
        #[arg(long)]
        server: Option<String>,
    },
    /// Create a branch at the full-state repo's HEAD (and push it)
    /// — "branch the data".
    Branch {
        /// Branch name (a valid git ref name).
        name: String,
        #[arg(long)]
        server: Option<String>,
    },
    /// Restore the server's data root to a snapshot commit, then
    /// the server EXITS so its supervisor restarts it on the
    /// restored data (local dev: restart task-server manually).
    /// By default a rescue snapshot runs first; requires --yes.
    Restore {
        /// Full-repo commit (sha or ref) to restore to.
        commit: String,
        /// Skip the rescue snapshot and proceed even if the
        /// server's work tree has uncommitted changes.
        #[arg(long)]
        force: bool,
        /// Confirm the restore (sends the confirmation token).
        /// Without it the command only prints what would happen.
        #[arg(long)]
        yes: bool,
        #[arg(long)]
        server: Option<String>,
    },
}

#[derive(Subcommand)]
enum AuthCmd {
    /// Sign in over the server's per-org `AuthService`
    /// (`<server>/org/<slug>/vox`) — works against a remote
    /// server with NO local org dir. Persists the session
    /// (token + user + org + server URL) to
    /// `$XDG_DATA_HOME/task/session.json`, so subsequent
    /// commands need nothing else: the stored server URL is
    /// used whenever `--server` / `TASK_VOX_URL` is absent.
    Login {
        #[arg(long)]
        email: String,
        #[arg(long)]
        password: String,
    },
    /// Create a new email/password user over the org's
    /// `AuthService` and persist the resulting session — like
    /// `login`, purely remote. The first user signed up in a
    /// fresh org is its de-facto owner — architect-auth has no
    /// separate ownership concept yet. Use `--org <slug>` /
    /// `--server <url>` to target a specific org.
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
    /// Switch the active session entry (server profile) without
    /// re-authenticating. `task auth whoami` lists the stored
    /// entries; reference one by key (`slug@host`), bare slug, or
    /// any unique prefix. Subsequent commands talk to that
    /// entry's server unless `--server` / `TASK_VOX_URL` says
    /// otherwise.
    Use {
        /// Session reference — key, slug, or unique prefix.
        session: String,
    },
    /// Invalidate the active session server-side AND remove
    /// the local session file.
    Logout,
    /// Org membership + selection.
    #[command(subcommand)]
    Org(AuthOrgCmd),
    /// List every user in the active org's `auth.sqlite`.
    /// Useful when you need a user_id to pass to
    /// `timer reassign-user --to`.
    Users,
}

#[derive(Subcommand)]
enum AuthOrgCmd {
    /// List orgs the signed-in user is a member of.
    List,
    /// Set the active org for subsequent commands. Updates
    /// both the local session file and the server-side
    /// `auth_session.active_organization_id`.
    Use {
        /// Org reference — UUID, slug, or name (exact / unique
        /// prefix), matched against your memberships.
        org_id: String,
    },
}

// A clap command enum: constructed once per invocation, so the
// inter-variant size gap is irrelevant, and boxing a variant's
// args fights the `Subcommand` derive / flattening.
#[allow(clippy::large_enum_variant)]
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
        /// Also merge prose notes from this tree (typically
        /// the org vault) into the graph as `note:/<path>`
        /// nodes — notes and wiki pages cross-link through
        /// the usual title/stem wikilink resolution.
        #[arg(long)]
        notes: Option<std::path::PathBuf>,
        /// Overlay a typed-link store (`links.jsonl`) as
        /// direct-link edges — picks up user-asserted
        /// note↔note links that aren't wikilinks in any
        /// body. Pair with `--notes` (the endpoints must be
        /// nodes to land).
        #[arg(long)]
        links: Option<std::path::PathBuf>,
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
    /// Archive a URL (or local file) into `Wiki/raw/sources/`
    /// with provenance frontmatter, then enqueue an ingest
    /// task. The front door of the wiki archive feature:
    /// routes by content type — articles → readability
    /// extraction, Google Docs → markdown export, YouTube /
    /// video → yt-dlp transcript with `^t<sec>` block
    /// anchors. Canonical-URL dedup: re-archiving the same
    /// resource (even via a differently-tracked link) is a
    /// no-op unless `--force`.
    Archive(WikiArchiveArgs),
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

#[derive(clap::Args)]
#[command(args_conflicts_with_subcommands = true, subcommand_negates_reqs = true)]
struct WikiArchiveArgs {
    /// Bulk importers (`task wiki archive import <kind>`).
    #[command(subcommand)]
    cmd: Option<WikiArchiveSub>,
    /// URL (http/https) or local file path to archive.
    #[arg(required = true)]
    target: Option<String>,
    /// Override the recorded title (default: extracted from
    /// the content).
    #[arg(long)]
    title: Option<String>,
    /// Re-archive even when the canonical URL already exists
    /// under `raw/sources/`.
    #[arg(long)]
    force: bool,
    /// Import + record only — don't enqueue an ingest task.
    #[arg(long)]
    no_enqueue: bool,
    /// yt-dlp binary used for video routes. Also settable
    /// via `TASK_YTDLP`.
    #[arg(long, env = "TASK_YTDLP", default_value = "yt-dlp")]
    yt_dlp: String,
    /// pdftotext binary (poppler) — the PDF-extraction
    /// fallback when pdfium isn't available. The pdfium path
    /// itself loads `libpdfium` from the `TASK_PDFIUM`
    /// directory or the system library path at runtime.
    #[arg(long, env = "TASK_PDFTOTEXT", default_value = "pdftotext")]
    pdftotext: String,
    /// Podcast episode picker for show-level URLs: a title
    /// (or substring) matched against the feed. Episode
    /// links (`?i=` on Apple) resolve without this; without
    /// either, the latest episode is archived.
    #[arg(long)]
    episode: Option<String>,
    /// Podcast transcript strategy: `auto` (feed transcript
    /// tag, then local whisper when compiled in), `tag`
    /// (feed tag only), `groq` (Groq API backfill,
    /// needs GROQ_API_KEY, ~$0.04/audio-hour), `whisper`
    /// (local model — requires a `--features whisper` build),
    /// `none` (metadata + show notes only).
    #[arg(long, default_value = "auto")]
    transcribe: String,
    /// Whisper model: a name (`small` dev default;
    /// `large-v3-turbo` for production quality) cached under
    /// ~/.cache/task/whisper/ and downloaded on first use, or
    /// a path to a ggml .bin file.
    #[arg(long, env = "TASK_WHISPER_MODEL", default_value = "small")]
    whisper_model: String,
    /// ffmpeg binary for enclosure → PCM decode (whisper
    /// path only).
    #[arg(long, env = "TASK_FFMPEG", default_value = "ffmpeg")]
    ffmpeg: String,
    #[arg(long, default_value = "default")]
    wiki_id: String,
    #[arg(long)]
    org: Option<String>,
    #[arg(long)]
    server: Option<String>,
    #[arg(long)]
    json: bool,
}

#[derive(Subcommand)]
enum WikiArchiveSub {
    /// Bookmark-service importers — batch front-ends to the
    /// archive router. Canonical-URL dedup makes re-runs (and
    /// cross-service overlap) idempotent.
    #[command(subcommand)]
    Import(WikiArchiveImportCmd),
    /// Extractor health: which archive routes currently work,
    /// which are broken, and the last error seen — from the
    /// per-org ledger every archive attempt records into.
    /// The phase-3 social routes are accept-fragility by
    /// design; this surface is how their breakage stays
    /// honest instead of silent.
    Health {
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Re-attempt every unarchived stub (`unarchived-*.md`
    /// under raw/sources/ — written when an accept-fragility
    /// route was blocked). Cron-friendly: throttled per
    /// route, always exits 0, prints a summary. A success
    /// imports the real source and deletes the stub.
    Retry {
        /// Max stubs to attempt this run.
        #[arg(long)]
        limit: Option<usize>,
        #[arg(long, default_value = "default")]
        wiki_id: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Import retried sources but don't enqueue ingest.
        #[arg(long)]
        no_enqueue: bool,
    },
}

#[derive(Subcommand)]
enum WikiArchiveImportCmd {
    /// Readwise classic highlights (v2 /export/ API; Token
    /// auth, 20 req/min — pages are throttled automatically).
    Readwise {
        /// Readwise access token (readwise.io/access_token).
        #[arg(long, env = "READWISE_TOKEN")]
        token: String,
        /// Incremental cursor: only books with highlights
        /// updated after this RFC3339 instant. The previous
        /// run prints the value to pass here.
        #[arg(long)]
        updated_after: Option<String>,
        #[command(flatten)]
        common: WikiArchiveImportCommon,
    },
    /// Readwise Reader documents (v3 /list/ API with
    /// withHtmlContent=true — full stored article HTML).
    Reader {
        #[arg(long, env = "READWISE_TOKEN")]
        token: String,
        #[arg(long)]
        updated_after: Option<String>,
        #[command(flatten)]
        common: WikiArchiveImportCommon,
    },
    /// Karakeep (self-hosted) via its REST API with
    /// includeContent=true. The JSON export is lossy (drops
    /// crawled htmlContent) — the API is the real source.
    Karakeep {
        /// Instance base URL, e.g. <https://keep.example.com>
        #[arg(long, env = "KARAKEEP_ENDPOINT")]
        endpoint: String,
        /// API key (ak2_… — Settings → API Keys).
        #[arg(long, env = "KARAKEEP_TOKEN")]
        token: String,
        #[command(flatten)]
        common: WikiArchiveImportCommon,
    },
    /// Pocket export zip (the service is dead — exports
    /// only). Reads part_*.csv saves + annotations/*.json
    /// highlights.
    Pocket {
        /// Path to the Pocket export zip.
        zip: std::path::PathBuf,
        #[command(flatten)]
        common: WikiArchiveImportCommon,
    },
    /// Netscape bookmarks HTML (what every browser exports).
    Bookmarks {
        /// Path to bookmarks.html.
        html: std::path::PathBuf,
        #[command(flatten)]
        common: WikiArchiveImportCommon,
    },
}

#[derive(clap::Args, Clone)]
struct WikiArchiveImportCommon {
    /// Max items to archive this run (after dedup).
    #[arg(long)]
    limit: Option<usize>,
    /// Parse + report only; nothing is written to the wiki.
    #[arg(long)]
    dry_run: bool,
    /// Import sources but don't enqueue ingest tasks (use
    /// `task wiki raw rescan` later to enqueue in bulk).
    #[arg(long)]
    no_enqueue: bool,
    #[arg(long, default_value = "default")]
    wiki_id: String,
    #[arg(long)]
    org: Option<String>,
    #[arg(long)]
    server: Option<String>,
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
        /// Only tasks relevant *right now* (see task::relevance):
        /// time-window contexts (`@morning` / `@mealprep` /
        /// `@evening`) gate to their windows, `@<location>` /
        /// `@<device>` gate to `--location` / `--device`,
        /// due/scheduled-today always shows. Implies `--open`;
        /// active-timer-project rows sort first.
        #[arg(long)]
        relevant: bool,
        /// Override the clock for `--relevant` (`HH:MM`, local).
        #[arg(long)]
        at: Option<String>,
        /// Where you are, for `--relevant` (`home`, `studio`, …).
        #[arg(long)]
        location: Option<String>,
        /// What you're on, for `--relevant` (`phone`, `computer`).
        #[arg(long)]
        device: Option<String>,
        /// Page size — at most this many rows (applied
        /// server-side, after `--status`/`--project`, over a
        /// stable path ordering; other filters then apply
        /// client-side within the page).
        #[arg(long)]
        limit: Option<u32>,
        /// Rows to skip before `--limit` (server-side).
        #[arg(long)]
        offset: Option<u32>,
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
    /// Start working: sets `status: in-progress`, which begins
    /// automatic time tracking (an inline TimeEntry on the task —
    /// edit it afterwards if the tracked time needs correcting).
    /// `done` stops the clock; `set-status open` pauses it.
    Start {
        target: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Emit the resulting task as JSON.
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
        /// Emit the resulting task as JSON.
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
        /// Emit the resulting task as JSON.
        #[arg(long)]
        json: bool,
    },
    SetPriority {
        target: String,
        priority: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Emit the resulting task as JSON.
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
        /// Emit the resulting task as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Set or clear (`none`) the scheduled date.
    SetScheduled {
        target: String,
        scheduled: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Emit the resulting task as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Set or clear (`none`) the owning project.
    SetProject {
        target: String,
        project: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Emit the resulting task as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Set or clear (`none`) the milestone link.
    SetMilestone {
        target: String,
        milestone: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Emit the resulting task as JSON.
        #[arg(long)]
        json: bool,
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
        /// Emit the resulting task as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Set or clear (`none`) the parent task — this task becomes a
    /// subtask (`workflow.parent`), rolled up in the parent's
    /// subtask list. Parent accepts an id or vault path.
    SetParent {
        target: String,
        parent: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Emit the resulting task as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Replace the GTD context list (`@`-prefix optional; it's
    /// added when missing). Relevancy gates ride on these — see
    /// `list --relevant`.
    SetContexts {
        target: String,
        #[arg(value_delimiter = ',')]
        contexts: Vec<String>,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Emit the resulting task as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Move backing markdown file. `id` preserved.
    Rename {
        target: String,
        new_path: String,
        #[arg(long)]
        org: Option<String>,
        #[arg(long)]
        server: Option<String>,
        /// Emit the renamed task as JSON.
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

#[derive(Subcommand)]
enum TimerCmd {
    /// Start the timer for the configured user. Fails if a
    /// session is already open.
    Start {
        /// Free-text description. Quoted to allow spaces.
        /// Optional when `--task` is given (defaults to the
        /// task's title).
        #[arg(required_unless_present = "task")]
        description: Option<String>,
        /// Task to track against — full UUID, unique id
        /// prefix, or vault-relative path. Validates the
        /// task exists and fills description (title),
        /// project (the task's project), and task-note
        /// (the task's path); explicit flags still win.
        #[arg(long)]
        task: Option<String>,
        /// Project the session is logged against — uuid,
        /// title, vault path, or a unique prefix of either.
        /// Empty = uncategorized.
        #[arg(long)]
        project: Option<String>,
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
        /// Emit the started session as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Stop the current session. Snapshots `rate_cents` +
    /// `currency` via the rate cascade and writes the closed
    /// row.
    Stop {
        /// Emit the closed session as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Show the active session, if any.
    Active {
        /// Emit the session as JSON (plus derived
        /// `seconds_elapsed` and joined task / project
        /// titles where resolvable). `null` when idle.
        #[arg(long)]
        json: bool,
    },
    /// Atomic stop-then-start. Same args as `start`.
    Switch {
        #[arg(required_unless_present = "task")]
        description: Option<String>,
        /// Task to track against (id / prefix / path) —
        /// same semantics as `start --task`.
        #[arg(long)]
        task: Option<String>,
        /// Project — uuid, title, path, or unique prefix.
        #[arg(long)]
        project: Option<String>,
        #[arg(long, default_value = "")]
        task_note: String,
        #[arg(long = "tag")]
        tags: Vec<String>,
        /// Emit `{stopped, started}` sessions as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Retro-log a past session: `--from` / `--to` ISO 8601
    /// timestamps + description. Skips the active-timer
    /// invariant.
    Log {
        #[arg(required_unless_present = "task")]
        description: Option<String>,
        #[arg(long)]
        from: chrono::DateTime<chrono::Utc>,
        #[arg(long)]
        to: chrono::DateTime<chrono::Utc>,
        /// Task to log against (id / prefix / path) — same
        /// semantics as `start --task`.
        #[arg(long)]
        task: Option<String>,
        /// Project — uuid, title, path, or unique prefix.
        #[arg(long)]
        project: Option<String>,
        #[arg(long, default_value = "")]
        task_note: String,
        /// `true` / `false` to override the project default.
        /// Omit to inherit.
        #[arg(long)]
        billable: Option<bool>,
        #[arg(long = "tag")]
        tags: Vec<String>,
        /// Emit the logged session as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Set an org-level member hourly rate (cascade level 3) for a
    /// user. New sessions logged for that user snapshot this rate at
    /// close. Upserts. Use `--org` to target the org's timer DB.
    SetRate {
        /// The member's user id (uuid).
        #[arg(long)]
        user_id: uuid::Uuid,
        /// Hourly rate in cents (e.g. 3000 = $30/hr).
        #[arg(long)]
        cents: i64,
        #[arg(long, default_value = "USD")]
        currency: String,
        /// Emit the stored rate as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Edit an existing session. Only the flags you pass change; the
    /// billable rate is re-snapshotted from the cascade afterward
    /// (so reassigning `--user-id` or `--project` re-rates it).
    Edit {
        /// Session id (uuid).
        id: uuid::Uuid,
        #[arg(long)]
        description: Option<String>,
        #[arg(long)]
        from: Option<chrono::DateTime<chrono::Utc>>,
        #[arg(long)]
        to: Option<chrono::DateTime<chrono::Utc>>,
        /// Reassign to a project — uuid, title, path, or a
        /// unique prefix of either.
        #[arg(long)]
        project: Option<String>,
        /// Reassign to a different member.
        #[arg(long)]
        user_id: Option<uuid::Uuid>,
        #[arg(long)]
        billable: Option<bool>,
        #[arg(long)]
        task_note: Option<String>,
        /// Emit the updated session as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Delete a session by id. Permanent.
    Delete {
        /// Session id (uuid).
        id: uuid::Uuid,
        /// Emit `{"deleted": <id>}` as JSON.
        #[arg(long)]
        json: bool,
    },
    /// List sessions. Defaults to the last 7 days, all
    /// users (matching the `finance project` rollup —
    /// the per-org DB is already the scope).
    List {
        /// Only sessions on this project — uuid, title,
        /// path, or a unique prefix of either.
        #[arg(long)]
        project: Option<String>,
        /// Only sessions logged by this user id. Omit for
        /// all users in the org.
        #[arg(long)]
        user: Option<uuid::Uuid>,
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
        /// Emit the sessions as a JSON array.
        #[arg(long)]
        json: bool,
    },
    /// Resolve the rate cascade for the configured user +
    /// project. Useful to preview "what will this session
    /// bill at" before stopping.
    Resolve {
        /// Project — uuid, title, path, or unique prefix.
        #[arg(long)]
        project: Option<String>,
        /// Emit the resolution as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Audit which user_ids appear on sessions, with name
    /// resolution from the org's `auth.sqlite`. Useful for
    /// spotting detached / mis-attributed ids before
    /// invoicing.
    Users {
        /// Emit the per-user aggregates as a JSON array.
        #[arg(long)]
        json: bool,
    },
    /// Bulk-swap every matching session's `user_id`.
    /// Optional filters narrow the swap to a project /
    /// date window — without them, ALL sessions for `from`
    /// in the org are moved.
    ReassignUser {
        /// Source user_id (current owner of the sessions).
        #[arg(long)]
        from: uuid::Uuid,
        /// Destination user_id (new owner).
        #[arg(long)]
        to: uuid::Uuid,
        /// Limit to one project — uuid, title, path, or a
        /// unique prefix of either.
        #[arg(long)]
        project: Option<String>,
        /// Inclusive lower bound on `start_time`.
        #[arg(long)]
        since: Option<chrono::DateTime<chrono::Utc>>,
        /// Exclusive upper bound on `start_time`.
        #[arg(long)]
        until: Option<chrono::DateTime<chrono::Utc>>,
        /// Limit to sessions whose description matches this
        /// substring (case-insensitive). Useful for
        /// untangling "video editing" vs "PNG tracking"
        /// rows that share a user_id.
        #[arg(long)]
        description_contains: Option<String>,
        /// Re-snapshot `rate_cents` + `currency` from the
        /// rate cascade for the *new* user. Off by default
        /// so already-billed amounts don't shift; pass when
        /// you're correcting a fresh mistake.
        #[arg(long, default_value_t = false)]
        rerate: bool,
        /// Show what would change without writing.
        #[arg(long, default_value_t = false)]
        dry_run: bool,
        /// Emit the match/update summary as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Tag CRUD + attach to existing sessions.
    #[command(subcommand)]
    Tag(TimerTagCmd),
}

#[derive(Subcommand)]
enum TimerTagCmd {
    /// List tags in the calling user's org.
    List {
        /// Emit the tags as a JSON array.
        #[arg(long)]
        json: bool,
    },
    /// Create a tag. Idempotent — no-op if a tag with that
    /// name already exists.
    Create {
        name: String,
        /// Hex `#RRGGBB` (UI hint). Empty = auto-pick.
        #[arg(long, default_value = "")]
        color: String,
        /// Emit the tag as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Delete a tag by name. Removes the join rows on every
    /// session via FK cascade.
    Rm {
        name: String,
        /// Emit the deleted tag as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Attach tags to an existing session.
    Attach {
        session_id: uuid::Uuid,
        #[arg(long = "tag", required = true)]
        tags: Vec<String>,
        /// Emit `{session_id, attached}` as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Detach tags from a session. `--tag <name>` removes
    /// that tag; `--all` removes every tag.
    Detach {
        session_id: uuid::Uuid,
        #[arg(long = "tag")]
        tags: Vec<String>,
        #[arg(long)]
        all: bool,
        /// Emit `{session_id, detached}` as JSON.
        #[arg(long)]
        json: bool,
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
        /// Emit the summary as JSON instead of markdown.
        #[arg(long)]
        json: bool,
    },
    /// Per-project hours rollup for a range. Defaults to
    /// the last 7 days.
    Project {
        #[arg(long)]
        since: Option<chrono::DateTime<chrono::Utc>>,
        #[arg(long)]
        until: Option<chrono::DateTime<chrono::Utc>>,
        /// Emit the rollup rows as a JSON array.
        #[arg(long)]
        json: bool,
    },
    /// Build + render an invoice from billable sessions on
    /// one project. By default writes both a PDF and a
    /// markdown stub into the vault's `Reports/Invoices/`
    /// directory (PDF under `Reports/Invoices/pdfs/`, MD at
    /// `Reports/Invoices/<num>.md` wikilinking the PDF).
    /// Use `--out` to override the PDF location and skip the
    /// vault export.
    Invoice {
        /// Project frontmatter uuid. Omit to bill every
        /// billable session in the range regardless of
        /// project (including unscoped time).
        #[arg(long)]
        project: Option<uuid::Uuid>,
        /// Inclusive lower bound on `start_time`.
        #[arg(long)]
        since: chrono::DateTime<chrono::Utc>,
        /// Exclusive upper bound on `start_time`.
        #[arg(long)]
        until: chrono::DateTime<chrono::Utc>,
        /// Explicit invoice number, e.g. `INV-2026-0042`.
        /// Mutually exclusive with `--prefix`.
        #[arg(long, conflicts_with = "prefix")]
        number: Option<String>,
        /// Auto-increment from the highest existing
        /// `<prefix>NNN` (zero-padded `--pad` digits, default
        /// 3). Example: `--prefix TBM-2026-` → finds the
        /// next free `TBM-2026-001`, `TBM-2026-002`…
        #[arg(long)]
        prefix: Option<String>,
        /// Width of the numeric suffix when using `--prefix`.
        #[arg(long, default_value_t = 3)]
        pad: usize,
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
        /// Render the PDF without persisting the invoice to
        /// `finance.sqlite` or stamping
        /// `work_sessions.invoice_id`. Use for previews.
        /// Without this flag (the default), the same
        /// `--since/--until` window won't re-bill the same
        /// hours on a later run.
        #[arg(long, default_value_t = false)]
        no_commit: bool,
        /// Emit the build/persist outcome as JSON.
        #[arg(long)]
        json: bool,
    },
    /// List persisted invoices in `finance.sqlite`.
    Invoices {
        /// Filter by status slug (draft / sent / paid /
        /// void / etc). Case-insensitive.
        #[arg(long)]
        status: Option<String>,
        /// Filter by party id.
        #[arg(long)]
        party: Option<uuid::Uuid>,
        /// Cap the output at this many rows (newest issued
        /// first).
        #[arg(long, default_value_t = 50)]
        limit: u64,
        /// Emit the invoices as a JSON array.
        #[arg(long)]
        json: bool,
    },
    /// Show one persisted invoice in detail — header,
    /// totals, line items, and the contributing session
    /// ids stamped to it.
    InvoiceShow {
        /// Invoice number.
        number: String,
        /// Emit the invoice (+ stamped sessions) as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Record a payment and update the invoice's balance.
    /// `--amount` is in minor units (cents). Sets the
    /// invoice to Paid if balance reaches zero,
    /// PartiallyPaid otherwise.
    InvoiceMarkPaid {
        /// Invoice number.
        number: String,
        /// Payment amount in minor units (cents). Omit to
        /// pay the full outstanding balance.
        #[arg(long)]
        amount: Option<i64>,
        /// ISO 8601 date (YYYY-MM-DD) the payment landed.
        /// Defaults to today.
        #[arg(long)]
        on: Option<chrono::NaiveDate>,
        /// Free-text note (cheque #, wire ref, …).
        #[arg(long, default_value = "")]
        memo: String,
        /// Emit the payment outcome as JSON.
        #[arg(long)]
        json: bool,
    },
    /// Cancel an invoice + un-stamp the contributing
    /// sessions so they can be re-billed. Idempotent on a
    /// missing invoice; refuses if the invoice already has
    /// payments against it (use a credit note instead).
    InvoiceVoid {
        /// Invoice number.
        number: String,
        /// Emit the void outcome as JSON.
        #[arg(long)]
        json: bool,
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

async fn run_admin(cmd: AdminCmd) -> eyre::Result<()> {
    // Same connection style as `task org create`: the
    // server-management endpoint at `<server>/server/vox`, the
    // active session's token for auth. An explicitly-set
    // `TASK_BACKUP_GIT_TOKEN` wins over the stored session so
    // headless automation (and admins targeting a server their
    // session wasn't minted on) can drive the verbs directly.
    async fn connect_snapshot(
        server: Option<&str>,
    ) -> eyre::Result<(org_proto::SnapshotServiceClient, String)> {
        let token = std::env::var("TASK_BACKUP_GIT_TOKEN")
            .ok()
            .filter(|t| !t.is_empty())
            .or_else(|| {
                session_store::load()
                    .ok()
                    .flatten()
                    .and_then(|s| s.servers.get(&s.active).map(|e| e.token.clone()))
            })
            .unwrap_or_default();
        let (client, _url): (org_proto::SnapshotServiceClient, _) =
            establish_server_client(server).await?;
        Ok((client, token))
    }

    fn print_repos(repos: &[org_proto::RepoResult]) {
        for r in repos {
            if !r.error.is_empty() {
                println!("  {}  ERROR: {}", r.repo, r.error);
            } else if r.clean {
                println!("  {}  clean — skip{}", r.repo, push_badge(r.pushed));
            } else {
                println!(
                    "  {}  committed {}{}",
                    r.repo,
                    &r.committed[..r.committed.len().min(12)],
                    push_badge(r.pushed)
                );
            }
        }
    }
    fn push_badge(pushed: bool) -> &'static str {
        if pushed { "  [pushed]" } else { "" }
    }

    match cmd {
        AdminCmd::Snapshot { server } => {
            let (client, token) = connect_snapshot(server.as_deref()).await?;
            let report = client
                .snapshot(token)
                .await
                .map_err(|e| eyre::eyre!("snapshot: {e:?}"))?;
            println!("snapshot {}", report.stamp);
            print_repos(&report.repos);
            if report.repos.iter().any(|r| !r.error.is_empty()) {
                return Err(eyre::eyre!("snapshot cycle reported per-repo errors"));
            }
        }
        AdminCmd::Log { limit, server } => {
            let (client, token) = connect_snapshot(server.as_deref()).await?;
            let entries = client
                .log(token, limit)
                .await
                .map_err(|e| eyre::eyre!("log: {e:?}"))?;
            if entries.is_empty() {
                println!("(no snapshots yet — run `task admin snapshot`)");
                return Ok(());
            }
            for e in entries {
                println!(
                    "{}  {}  {}",
                    &e.commit[..e.commit.len().min(12)],
                    e.timestamp,
                    e.message
                );
            }
        }
        AdminCmd::Branch { name, server } => {
            let (client, token) = connect_snapshot(server.as_deref()).await?;
            let res = client
                .branch(token, name)
                .await
                .map_err(|e| eyre::eyre!("branch: {e:?}"))?;
            println!(
                "branched `{}` at {}{}",
                res.name,
                &res.commit[..res.commit.len().min(12)],
                push_badge(res.pushed)
            );
        }
        AdminCmd::Restore {
            commit,
            force,
            yes,
            server,
        } => {
            if !yes {
                println!("Would restore the server's data root to `{commit}`.");
                println!("This rewrites EVERY org's files + sqlites on the server, then the");
                println!("server process exits so its supervisor restarts it (local dev: restart");
                println!("task-server manually).");
                if force {
                    println!(
                        "--force: skips the rescue snapshot — pre-restore state is NOT saved."
                    );
                } else {
                    println!("A rescue snapshot of the current state runs first.");
                }
                println!("\nRe-run with --yes to proceed.");
                return Ok(());
            }
            let (client, token) = connect_snapshot(server.as_deref()).await?;
            let report = client
                .restore(token, commit.clone(), commit, force)
                .await
                .map_err(|e| eyre::eyre!("restore: {e:?}"))?;
            if !report.pre_restore.is_empty() {
                println!("rescue snapshot:");
                print_repos(&report.pre_restore);
            }
            println!("restored data root to {}", report.commit);
            if report.restarting {
                println!("server is exiting for restart — give it a few seconds to come back");
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

/// One hosted org row from a server's discovery document.
#[derive(Debug, serde::Deserialize)]
struct HostedOrg {
    slug: String,
    #[serde(default)]
    is_home: bool,
}

/// `ws(s)://` vox base → `http(s)://` origin+path for the same
/// server (the well-known + health endpoints live on plain HTTP).
fn ws_base_to_http(base: &str) -> String {
    base.replacen("wss://", "https://", 1)
        .replacen("ws://", "http://", 1)
}

/// Fetch the server's `/.well-known/task-server.json` org list —
/// the remote replacement for scanning `<data_root>/orgs/`.
async fn fetch_hosted_orgs(base: &str) -> eyre::Result<Vec<HostedOrg>> {
    let url = format!(
        "{}/.well-known/task-server.json",
        ws_base_to_http(&session_store::normalize_server_base(base))
    );
    let doc: serde_json::Value = reqwest::get(&url)
        .await
        .map_err(|e| eyre::eyre!("fetch {url}: {e}"))?
        .json()
        .await
        .map_err(|e| eyre::eyre!("parse {url}: {e}"))?;
    let orgs = doc
        .get("orgs")
        .cloned()
        .ok_or_else(|| eyre::eyre!("{url}: no `orgs` field"))?;
    serde_json::from_value(orgs).map_err(|e| eyre::eyre!("{url}: bad `orgs` shape: {e}"))
}

/// Resolve which (org slug, server base) the remote auth verbs
/// operate on. Purely remote — requires NO local org dir:
///
/// - server: [`resolve_server_base`] precedence (flag > env >
///   session > localhost).
/// - slug: `--org` > the session entry for that server > the
///   server's well-known org list (its home org, or the single
///   hosted org).
///
/// When discovery works, an unknown slug fails here with the
/// hosted list — clearer than a raw vox connect error. When the
/// well-known endpoint is unreachable the vox connect downstream
/// reports the connection failure with its own taxonomy.
async fn resolve_auth_target(org_override: Option<&str>) -> eyre::Result<(String, String)> {
    let base = resolve_server_base(None);
    let hosted = fetch_hosted_orgs(&base).await.ok();
    let chosen = if let Some(s) = org_override.map(str::to_owned).or_else(global_org) {
        Some(s)
    } else if let Some((_, entry)) = session_store::load()?
        .as_ref()
        .and_then(|s| s.entry_for_server(&base))
    {
        Some(entry.slug.clone())
    } else {
        // No flag, no session: let the server disambiguate — its
        // home org, or the only org it hosts.
        hosted.as_ref().and_then(|orgs| {
            orgs.iter()
                .find(|o| o.is_home)
                .or_else(|| (orgs.len() == 1).then(|| &orgs[0]))
                .map(|o| o.slug.clone())
        })
    };
    let Some(slug) = chosen else {
        return Err(errors::usage("resolve org for auth")
            .cause(format!(
                "no `--org` given and nothing to infer it from ({base})"
            ))
            .hint("pass --org <slug> (see `task org list --server …` for what the server hosts)")
            .report());
    };
    if let Some(orgs) = &hosted {
        if !orgs.iter().any(|o| o.slug == slug) {
            let names: Vec<&str> = orgs.iter().map(|o| o.slug.as_str()).collect();
            return Err(errors::not_found("resolve org on server", &slug)
                .cause(format!("{base} hosts: {}", names.join(", ")))
                .hint("pass --org <slug> from that list, or `task org create` it first")
                .report());
        }
    }
    Ok((slug, base))
}

async fn run_auth(cmd: AuthCmd, org_override: Option<&str>) -> eyre::Result<()> {
    use architect_auth::commands::CurrentSession;
    use architect_auth::proto::{AuthServiceClient, SignInEmailPassword, SignUpEmailPassword};
    match cmd {
        AuthCmd::Signup {
            email,
            password,
            username,
            name,
        } => {
            // Remote-first: sign up over the org's AuthService —
            // the same per-org vox endpoint every other service
            // rides. No local org dir required.
            let (slug, base) = resolve_auth_target(org_override).await?;
            let url = resolve_org_vox_url(Some(base.clone()), &slug);
            let client: AuthServiceClient = establish_for_url(&url).await?;
            let bundle = client
                .sign_up_email_password(SignUpEmailPassword {
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
                .map_err(|e| eyre::eyre!("sign up: {e}"))?;
            let resolved_email = bundle.user.email.clone().unwrap_or_else(|| email.clone());
            // Persist the session keyed by (server, org) — same
            // shape as `Login` so subsequent commands work
            // without a follow-up `task auth login`.
            let mut sess = session_store::load()?.unwrap_or_else(session_store::CliSession::empty);
            let key = sess.record_login(
                &slug,
                &base,
                bundle.user.id,
                resolved_email.clone(),
                bundle.token.clone(),
            );
            session_store::save(&sess)?;
            println!(
                "Created user {} ({}) in org `{slug}`",
                resolved_email, bundle.user.id,
            );
            if let Some(u) = username {
                println!("  username: {u}");
            }
            if let Some(n) = name {
                println!("  name:     {n}");
            }
            println!("  server:   {base}");
            println!("  session:  {key}");
        }
        AuthCmd::Login { email, password } => {
            // Remote-first sign-in over the org's AuthService.
            // The org is resolved via the server's well-known
            // document — no `task org init` needed on this box.
            let (slug, base) = resolve_auth_target(org_override).await?;
            let url = resolve_org_vox_url(Some(base.clone()), &slug);
            let client: AuthServiceClient = establish_for_url(&url).await?;
            let bundle = client
                .sign_in_email_password(SignInEmailPassword {
                    email: email.clone(),
                    password,
                    ip_address: None,
                    user_agent: Some("task-cli".into()),
                })
                .await
                .map_err(|e| eyre::eyre!("sign in: {e}"))?;
            let resolved_email = bundle.user.email.clone().unwrap_or_else(|| email.clone());
            // Multi-server session: insert/update the entry keyed
            // by (server, org) and make it active. The stored
            // server URL is what later invocations resolve when
            // neither `--server` nor `TASK_VOX_URL` is set.
            let mut sess = session_store::load()?.unwrap_or_else(session_store::CliSession::empty);
            let key = sess.record_login(
                &slug,
                &base,
                bundle.user.id,
                resolved_email.clone(),
                bundle.token.clone(),
            );
            session_store::save(&sess)?;
            println!(
                "Signed in as {} ({}) on org `{slug}`",
                resolved_email, bundle.user.id,
            );
            println!("  server:   {base}");
            println!("  session:  {key}");
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
                for (key, entry) in &s.servers {
                    let marker = if *key == s.active { "*" } else { " " };
                    println!(
                        "{marker} {key:<28}  org={}  {}  {}  server={}",
                        entry.slug, entry.email, entry.user_id, entry.url
                    );
                }
                // Where the NEXT command will go, after the full
                // precedence fold (flag > env > session > default).
                println!("server: {} (this invocation)", resolve_server_base(None));
                println!("session: {}", session_store::session_path()?.display());
            }
            None => {
                println!("Not signed in. Run `task auth login --email … --password …`.");
            }
        },
        AuthCmd::Use { session } => {
            let Some(mut sess) = session_store::load()? else {
                return Err(errors::usage("auth use")
                    .cause("no stored session")
                    .hint("run `task auth login` first")
                    .report());
            };
            let key = match_session_entry(&sess, &session)?;
            sess.active = key.clone();
            session_store::save(&sess)?;
            let entry = &sess.servers[&key];
            println!(
                "Active session: {key} — org `{}` on {} ({})",
                entry.slug, entry.url, entry.email
            );
        }
        AuthCmd::Logout => {
            let Some(mut sess) = session_store::load()? else {
                println!("Not signed in — nothing to do.");
                return Ok(());
            };
            // Which entry? `--org` picks by slug (preferring the
            // entry on the currently-resolved server); default is
            // the active entry. Other servers stay linked.
            let key = match org_override.map(str::to_owned).or_else(global_org) {
                Some(slug) => {
                    let base = resolve_server_base(None);
                    sess.servers
                        .iter()
                        .find(|(_, e)| e.slug == slug && session_store::same_server(&e.url, &base))
                        .or_else(|| sess.servers.iter().find(|(_, e)| e.slug == slug))
                        .map(|(k, _)| k.clone())
                        .ok_or_else(|| {
                            errors::not_found("logout", &slug)
                                .cause("no stored session for that org")
                                .hint("`task auth whoami` lists the signed-in sessions")
                                .report()
                        })?
                }
                None => sess.active.clone(),
            };
            if let Some(entry) = sess.servers.remove(&key) {
                // Server-side revoke, best effort — over the
                // entry's OWN server (remote logout). Legacy
                // `"local"` entries go straight at the org's
                // on-disk auth.sqlite.
                let revoked: eyre::Result<()> = if entry.url == session_store::LOCAL_URL {
                    revoke_local_session(&entry).await
                } else {
                    let url = resolve_org_vox_url(Some(entry.url.clone()), &entry.slug);
                    match Box::pin(establish_for_url::<AuthServiceClient>(&url)).await {
                        Ok(client) => client
                            .sign_out(entry.token.clone())
                            .await
                            .map_err(|e| eyre::eyre!("{e}")),
                        Err(e) => Err(e),
                    }
                };
                if let Err(e) = revoked {
                    eprintln!("warning: server-side sign out failed: {e:#}");
                }
                println!("Signed out of `{}` ({}).", entry.slug, entry.url);
            } else {
                println!("No stored session under `{key}`.");
            }
            // If no servers left, clear the file entirely; else
            // write the shrunken session back.
            if sess.servers.is_empty() {
                session_store::clear()?;
            } else {
                // Active falls back to home if home is still
                // present, otherwise pick the first remaining
                // server.
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
        AuthCmd::Org(AuthOrgCmd::List) => {
            let ctx = local_org_ctx(org_override, "auth org list")?;
            let auth_db_path = ctx.root.auth_db();
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
            let ctx = local_org_ctx(org_override, "auth org use")?;
            let auth_db_path = ctx.root.auth_db();
            let Some(sess) = session_store::load()? else {
                return Err(eyre::eyre!("not signed in — run `task auth login` first"));
            };
            let Some(active_entry) = sess.active_server() else {
                return Err(eyre::eyre!("no active server in session"));
            };
            // Resolve the reference against the user's memberships:
            // uuid / id prefix, slug, or name (exact / unique prefix)
            // — same matcher as every other entity flag. Doubles as
            // the membership check (non-members never match).
            let memberships = list_user_memberships(active_entry.user_id, &auth_db_path).await?;
            let cands: Vec<json_out::Candidate> = memberships
                .iter()
                .map(|(m, o)| (m.organization_id, o.name.clone(), o.slug.clone()))
                .collect();
            let resolved = match json_out::match_entity(&cands, &org_id, "organization") {
                Ok(i) => cands[i].0,
                Err(fail) => {
                    return Err(fail.into_report("organization", &org_id));
                }
            };
            update_session_active_org(&active_entry.token, Some(resolved), &auth_db_path).await?;
            println!("Architect-auth active membership set to {resolved}");
        }
        AuthCmd::Users => {
            use architect_auth::db::AuthUserEntity;
            use sea_orm::{Database, EntityTrait};
            let ctx = local_org_ctx(org_override, "auth users")?;
            let auth_db_path = ctx.root.auth_db();
            if !auth_db_path.exists() {
                return Err(eyre::eyre!("no auth.sqlite at {}", auth_db_path.display()));
            }
            let url = format!("sqlite://{}?mode=ro", auth_db_path.display());
            let db = Database::connect(&url)
                .await
                .map_err(|e| eyre::eyre!("open {url}: {e}"))?;
            let users = AuthUserEntity::find()
                .all(&db)
                .await
                .map_err(|e| eyre::eyre!("query auth_users: {e}"))?;
            if users.is_empty() {
                println!("(no users)");
            }
            println!("{:<38}  {:<24}  email", "user_id", "name");
            for u in users {
                println!(
                    "{:<38}  {:<24}  {}",
                    u.id,
                    u.name.unwrap_or_default(),
                    u.email.unwrap_or_default()
                );
            }
        }
    }
    Ok(())
}

/// Resolve a `task auth use` reference against the stored session
/// entries: exact key, exact slug (unique), then unique prefix of
/// either. Ambiguity and misses list what IS stored.
fn match_session_entry(sess: &session_store::CliSession, reference: &str) -> eyre::Result<String> {
    if sess.servers.contains_key(reference) {
        return Ok(reference.to_owned());
    }
    let slug_hits: Vec<&String> = sess
        .servers
        .iter()
        .filter(|(_, e)| e.slug == reference)
        .map(|(k, _)| k)
        .collect();
    if let [one] = slug_hits.as_slice() {
        return Ok((*one).clone());
    }
    let prefix_hits: Vec<&String> = if slug_hits.is_empty() {
        sess.servers
            .iter()
            .filter(|(k, e)| k.starts_with(reference) || e.slug.starts_with(reference))
            .map(|(k, _)| k)
            .collect()
    } else {
        slug_hits
    };
    let stored = || {
        sess.servers
            .keys()
            .map(String::as_str)
            .collect::<Vec<_>>()
            .join(", ")
    };
    match prefix_hits.as_slice() {
        [one] => Ok((*one).clone()),
        [] => Err(errors::not_found("auth use", reference)
            .cause(format!("stored sessions: {}", stored()))
            .hint("`task auth whoami` lists the stored entries")
            .report()),
        many => Err(errors::conflict("auth use", reference)
            .cause(format!(
                "matches {} entries: {}",
                many.len(),
                many.iter()
                    .map(|k| k.as_str())
                    .collect::<Vec<_>>()
                    .join(", ")
            ))
            .hint("disambiguate with the full key (`slug@host`)")
            .report()),
    }
}

/// Resolve a LOCAL on-disk org for the auth verbs that read the
/// org's `auth.sqlite` directly (`auth users`, `auth org …`).
/// Distinguishes "this command is local-only" from "you're not
/// signed in": a remote session can never serve these — they need
/// an org dir under the data root.
fn local_org_ctx(org_override: Option<&str>, what: &str) -> eyre::Result<org_ctx::ActiveOrg> {
    org_ctx::resolve_active(org_override).map_err(|e| {
        errors::usage(format!(
            "{what} is a local-only command (it reads the org's on-disk auth.sqlite)"
        ))
        .cause(format!("{e:#}"))
        .hint(
            "run `task org init <slug>` to create a local org dir; a remote session \
             (`task auth login --server …`) cannot serve this command",
        )
        .report()
    })
}

/// Best-effort server-side revocation for a legacy `"local"`
/// session entry: open the org's `auth.sqlite` directly, like the
/// old local-first `auth logout` did.
async fn revoke_local_session(entry: &session_store::ServerEntry) -> eyre::Result<()> {
    use architect_auth::commands::SignOut;
    let root =
        org_proto::DataRoot::from_env().map_err(|e| eyre::eyre!("resolve data root: {e}"))?;
    let (org, _) = root
        .load_org(&entry.slug)
        .map_err(|e| eyre::eyre!("no local org dir for `{}`: {e}", entry.slug))?;
    let auth = open_local_auth(&org.auth_db()).await?;
    auth.sign_out(SignOut {
        token: entry.token.clone(),
    })
    .await
    .map_err(|e| eyre::eyre!("{e}"))?;
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
    // `TASK_VAULT_ROOT` is a fixture override; the real
    // default is the active org's vault. (Was a cwd-relative
    // `examples/vault` fallback in the invoice arm, which
    // silently exported invoices into whatever repo you
    // happened to run from.)
    let vault_root = std::env::var("TASK_VAULT_ROOT")
        .map_or_else(|_| ctx.root.vault_dir(), std::path::PathBuf::from);

    match cmd {
        FinanceCmd::Weekly { week_of, json } => {
            let day = week_of.unwrap_or_else(|| chrono::Utc::now().date_naive());
            let summary = finance::reports::weekly_summary(&timer_conn, None, day)
                .await
                .map_err(|e| eyre::eyre!("weekly: {e}"))?;
            if json {
                json_out::print_json(&summary)?;
            } else {
                print!("{}", summary.to_markdown());
            }
        }
        FinanceCmd::Project { since, until, json } => {
            use finance::reports::DateRange;
            // Each bound defaults independently: missing
            // `--until` means "now", missing `--since` means
            // 7 days before until. Previously `--since`
            // alone was silently ignored (full fallback to
            // last-7-days).
            let range = {
                let u = until.unwrap_or_else(chrono::Utc::now);
                let s = since.unwrap_or(u - chrono::Duration::days(7));
                DateRange { since: s, until: u }
            };
            let rows = finance::reports::hours_by_project(&timer_conn, None, range)
                .await
                .map_err(|e| eyre::eyre!("project: {e}"))?;
            if json {
                // Rollup rows + the same resolved display label
                // the human rendering computes.
                let out: Vec<serde_json::Value> = rows
                    .iter()
                    .map(|r| {
                        let mut v = serde_json::to_value(r).unwrap_or(serde_json::Value::Null);
                        if let serde_json::Value::Object(map) = &mut v {
                            let label = if !r.project_path.is_empty() {
                                r.project_path.clone()
                            } else if let Some(pid) = r.project_id {
                                let resolved = project_path_for(&vault_root, Some(pid));
                                if resolved.is_empty() {
                                    format!("(project {pid})")
                                } else {
                                    resolved
                                }
                            } else {
                                "(unscoped)".to_string()
                            };
                            map.insert("project".into(), label.into());
                        }
                        v
                    })
                    .collect();
                json_out::print_json(&out)?;
                return Ok(());
            }
            if rows.is_empty() {
                println!("(no closed sessions in range)");
            }
            for r in rows {
                // Older sessions may carry a project_id but
                // an empty project_path (the path resolver
                // used to miss nested project folders), so
                // fall back to a vault lookup before
                // declaring the bucket unscoped.
                let project = if !r.project_path.is_empty() {
                    r.project_path.clone()
                } else if let Some(pid) = r.project_id {
                    let resolved = project_path_for(&vault_root, Some(pid));
                    if resolved.is_empty() {
                        format!("(project {pid})")
                    } else {
                        resolved
                    }
                } else {
                    "(unscoped)".to_string()
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
            prefix,
            pad,
            net_days,
            client_name,
            out,
            no_commit,
            json,
        } => {
            if number.is_none() && prefix.is_none() {
                return Err(eyre::eyre!(
                    "pass either --number <explicit> or --prefix <auto>"
                ));
            }
            // Stable per-org / per-client UUIDv5 ids so
            // repeated invoices share a single Book and
            // Party row in finance.sqlite. Avoids the
            // FK-constraint failure that hits when book_id /
            // party_id are nil, and keeps the schema sane
            // until a real CLI surface for Books + Parties
            // lands.
            let book_id = uuid::Uuid::new_v5(
                &uuid::Uuid::NAMESPACE_DNS,
                format!("task-finance-book/{}", ctx.root.slug()).as_bytes(),
            );
            let party_id = uuid::Uuid::new_v5(
                &uuid::Uuid::NAMESPACE_DNS,
                format!("task-finance-party/{}/{}", ctx.root.slug(), client_name).as_bytes(),
            );
            let book = finance_proto::book::Book {
                id: book_id,
                name: format!("{} Book", ctx.root.slug()),
                kind: finance_proto::book::BookKind::Personal,
                base_currency: "USD".into(),
                settings_json: "{}".into(),
                created_at: chrono::Utc::now(),
                updated_at: chrono::Utc::now(),
            };
            let party = finance_proto::party::Party {
                id: party_id,
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
            // Open the org's finance.sqlite up-front (even
            // for --no-commit) so we can pre-check the
            // invoice number against the unique index and
            // fail before spending render time on a dupe.
            let finance_conn = {
                use sea_orm_migration::MigratorTrait;
                let url = format!("sqlite://{}?mode=rwc", ctx.root.finance_db().display());
                let conn = Database::connect(&url)
                    .await
                    .map_err(|e| eyre::eyre!("connect finance db `{url}`: {e}"))?;
                finance_db::Migrator::up(&conn, None)
                    .await
                    .map_err(|e| eyre::eyre!("finance migrations: {e}"))?;
                conn
            };
            // Resolve the final invoice number: explicit
            // --number, or auto-incremented from --prefix.
            let final_number: String = if let Some(n) = number.clone() {
                use finance_db::entity::{InvoiceColumn, InvoiceEntity};
                use sea_orm::{ColumnTrait, EntityTrait, QueryFilter};
                let existing = InvoiceEntity::find()
                    .filter(InvoiceColumn::Number.eq(n.clone()))
                    .one(&finance_conn)
                    .await
                    .map_err(|e| eyre::eyre!("check invoice number: {e}"))?;
                if existing.is_some() {
                    return Err(eyre::eyre!(
                        "invoice number `{n}` is already in finance.sqlite. Pick a new --number, or pass --no-commit to render-only."
                    ));
                }
                n
            } else {
                let p = prefix.clone().expect("validated above");
                next_invoice_number(&finance_conn, &p, pad).await?
            };

            // When `--project` is set we delegate to the
            // pipeline's per-engagement query. Without it,
            // load every billable + uninvoiced session in
            // the window and hand the list to
            // `build_from_models`.
            let build = if let Some(pid) = project {
                finance::invoice_from_sessions::build_invoice_from_sessions(
                    &timer_conn,
                    finance::invoice_from_sessions::BuildInvoiceArgs {
                        book: book.clone(),
                        party: party.clone(),
                        project_id: pid,
                        since,
                        until,
                        net_days,
                        number: final_number.clone(),
                        notes_public: String::new(),
                        notes_private: String::new(),
                        terms: String::new(),
                    },
                )
                .await
                .map_err(|e| eyre::eyre!("build invoice: {e}"))?
            } else {
                use sea_orm::{ColumnTrait, EntityTrait, QueryFilter};
                use timer::entity::{WorkSessionColumn, WorkSessionEntity};
                let sessions = WorkSessionEntity::find()
                    .filter(WorkSessionColumn::Billable.eq(true))
                    .filter(WorkSessionColumn::EndTime.is_not_null())
                    .filter(WorkSessionColumn::InvoiceId.is_null())
                    .filter(WorkSessionColumn::StartTime.gte(since))
                    .filter(WorkSessionColumn::StartTime.lt(until))
                    .all(&timer_conn)
                    .await
                    .map_err(|e| eyre::eyre!("query sessions: {e}"))?;
                finance::invoice_from_sessions::build_from_models(
                    book.clone(),
                    party.clone(),
                    sessions,
                    net_days,
                    final_number.clone(),
                    String::new(),
                    String::new(),
                    String::new(),
                )
                .map_err(|e| eyre::eyre!("build invoice: {e}"))?
            };

            // Issuer ("From" block): `<org>/issuer.toml` is
            // the durable source; `TASK_ISSUER_*` env vars
            // override per-field for fixtures. "Your Name"
            // placeholder only when neither is set.
            let stored = org_proto::IssuerProfile::load(&ctx.root.issuer_path())
                .map_err(|e| eyre::eyre!("issuer.toml: {e}"))?
                .unwrap_or_default();
            let field = |env: &str, file: String, default: &str| {
                std::env::var(env).unwrap_or(if file.is_empty() {
                    default.to_string()
                } else {
                    file
                })
            };
            let issuer = finance::pdf_adapter::IssuerProfile {
                name: field("TASK_ISSUER_NAME", stored.name, "Your Name"),
                address: field("TASK_ISSUER_ADDRESS", stored.address, ""),
                email: field("TASK_ISSUER_EMAIL", stored.email, ""),
                phone: field("TASK_ISSUER_PHONE", stored.phone, ""),
                tax_id: field("TASK_ISSUER_TAX_ID", stored.tax_id, ""),
            };
            let mut ifp = finance::pdf_adapter::invoice_for_pdf(&build.invoice, &issuer, &party);
            // Resolve user_id → display name from the org's
            // auth.sqlite. Missing rows fall back to a
            // short-id label so a stranded id still reads.
            let mut names_by_id = {
                use architect_auth::db::{AuthUserColumn, AuthUserEntity};
                use sea_orm::{ColumnTrait, Database, EntityTrait, QueryFilter};
                let auth_path = ctx.root.auth_db();
                let mut map: std::collections::HashMap<uuid::Uuid, String> =
                    std::collections::HashMap::new();
                let ids: Vec<uuid::Uuid> = build.line_meta.iter().map(|m| m.user_id).collect();
                if !ids.is_empty() && auth_path.exists() {
                    let url = format!("sqlite://{}?mode=ro", auth_path.display());
                    if let Ok(db) = Database::connect(&url).await {
                        if let Ok(rows) = AuthUserEntity::find()
                            .filter(AuthUserColumn::Id.is_in(ids.clone()))
                            .all(&db)
                            .await
                        {
                            for r in rows {
                                let label = r
                                    .name
                                    .filter(|s| !s.is_empty())
                                    .or(r.email)
                                    .unwrap_or_else(|| r.id.simple().to_string());
                                map.insert(r.id, label);
                            }
                        }
                    }
                }
                map
            };
            // Manual override: `TASK_MEMBER_NAMES="<uuid>=Name;<uuid>=Name"`
            // wins over the auth.sqlite lookup — and seeds a display name
            // when there's no auth row at all (CLI-only invoices where the
            // member id is a local stand-in, not a signed-up account).
            if let Ok(raw) = std::env::var("TASK_MEMBER_NAMES") {
                for pair in raw.split([';', ',']) {
                    if let Some((id, name)) = pair.split_once('=') {
                        if let (Ok(uid), name) = (id.trim().parse::<uuid::Uuid>(), name.trim()) {
                            if !name.is_empty() {
                                names_by_id.insert(uid, name.to_string());
                            }
                        }
                    }
                }
            }
            enrich_invoice_with_assignees(&mut ifp, &build.line_meta, &names_by_id);
            // User asked to drop the due-date row; keep
            // `Invoice.due_date` in the proto for accounting
            // semantics, just hide it on the PDF.
            ifp.due_date.clear();
            // Same idea for the status pill — the proto
            // still says "Draft" until we mount a real
            // posting flow, but the PDF doesn't need to
            // shout that at the recipient.
            ifp.status.clear();
            // Period the invoice spans — drives the
            // "Period:" row in the header so a reader
            // doesn't have to scan line dates.
            ifp.period_start = since.format("%Y-%m-%d").to_string();
            ifp.period_end = until.format("%Y-%m-%d").to_string();
            // Decide PDF path: explicit --out wins; else vault-export under
            // `<vault>/Reports/Invoices/pdfs/<num>.pdf`.
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
            let mut md_out: Option<std::path::PathBuf> = None;
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
                    since,
                    until,
                    &ifp.people,
                    &ifp.assignees,
                );
                std::fs::write(&md_path, md)
                    .map_err(|e| eyre::eyre!("write {}: {e}", md_path.display()))?;
                if !json {
                    println!("Wrote {}", md_path.display());
                }
                md_out = Some(md_path);
            }
            // Persist to finance.sqlite + stamp the
            // contributing sessions so the same range can't
            // re-bill the same hours. SQLite-per-DB means
            // we can't span a tx across the two; finance
            // first (atomic insert), then timer stamp. If
            // the stamp fails mid-way the worst case is a
            // partial set of sessions linked to a real
            // invoice — re-running `--no-commit=false` will
            // pick up the leftovers next time because the
            // invoice number now collides.
            let mut stamped_sessions: u64 = 0;
            if no_commit {
                if !json {
                    println!("Skipped commit (--no-commit). Sessions remain unbilled.");
                }
            } else {
                use finance_db::entity::{
                    BookColumn, BookEntity, InvoiceEntity, PartyColumn, PartyEntity,
                };
                use sea_orm::sea_query::OnConflict;
                use sea_orm::{ColumnTrait, EntityTrait, QueryFilter};
                use timer::entity::{WorkSessionColumn, WorkSessionEntity};
                // Insert-if-missing book + party (do-nothing
                // on conflict). The first invoice in a fresh
                // finance.sqlite is what creates these.
                BookEntity::insert(finance::billing::book_to_active(&book))
                    .on_conflict(OnConflict::column(BookColumn::Id).do_nothing().to_owned())
                    .do_nothing()
                    .exec(&finance_conn)
                    .await
                    .map_err(|e| eyre::eyre!("upsert book: {e}"))?;
                PartyEntity::insert(finance::billing::party_to_active(&party))
                    .on_conflict(OnConflict::column(PartyColumn::Id).do_nothing().to_owned())
                    .do_nothing()
                    .exec(&finance_conn)
                    .await
                    .map_err(|e| eyre::eyre!("upsert party: {e}"))?;
                let active = finance::billing::invoice_to_active(&build.invoice);
                InvoiceEntity::insert(active)
                    .exec(&finance_conn)
                    .await
                    .map_err(|e| eyre::eyre!("insert invoice: {e}"))?;
                let stamped = WorkSessionEntity::update_many()
                    .col_expr(
                        WorkSessionColumn::InvoiceId,
                        sea_orm::sea_query::Expr::value(build.invoice.id),
                    )
                    .col_expr(
                        WorkSessionColumn::UpdatedAt,
                        sea_orm::sea_query::Expr::value(chrono::Utc::now()),
                    )
                    .filter(WorkSessionColumn::Id.is_in(build.source_session_ids.clone()))
                    .exec(&timer_conn)
                    .await
                    .map_err(|e| eyre::eyre!("stamp sessions: {e}"))?;
                stamped_sessions = stamped.rows_affected;
                if !json {
                    println!(
                        "Persisted invoice {} + stamped {} session(s).",
                        build.invoice.id, stamped.rows_affected
                    );
                }
            }
            if json {
                json_out::print_json(&serde_json::json!({
                    "id": build.invoice.id,
                    "number": build.invoice.number,
                    "currency": build.invoice.currency,
                    "subtotal_minor": build.invoice.subtotal_minor,
                    "total_minor": build.invoice.total_minor,
                    "sessions": build.source_session_ids,
                    "pdf_path": pdf_path,
                    "pdf_bytes": bytes_len,
                    "markdown_path": md_out,
                    "committed": !no_commit,
                    "stamped_sessions": stamped_sessions,
                }))?;
            } else {
                println!(
                    "Wrote {} ({bytes_len} bytes, {} sessions, {} {})",
                    pdf_path.display(),
                    build.source_session_ids.len(),
                    fmt_minor(build.invoice.total_minor),
                    build.invoice.currency,
                );
            }
        }
        FinanceCmd::Invoices {
            status,
            party,
            limit,
            json,
        } => {
            use finance_db::entity::{InvoiceColumn, InvoiceEntity};
            use sea_orm::{ColumnTrait, EntityTrait, QueryFilter, QueryOrder, QuerySelect};
            use sea_orm_migration::MigratorTrait;
            let url = format!("sqlite://{}?mode=rwc", ctx.root.finance_db().display());
            let conn = Database::connect(&url)
                .await
                .map_err(|e| eyre::eyre!("connect finance db: {e}"))?;
            finance_db::Migrator::up(&conn, None).await.ok();
            let mut q = InvoiceEntity::find()
                .order_by_desc(InvoiceColumn::IssueDate)
                .order_by_desc(InvoiceColumn::CreatedAt)
                .limit(limit);
            if let Some(p) = party {
                q = q.filter(InvoiceColumn::PartyId.eq(p));
            }
            let rows = q
                .all(&conn)
                .await
                .map_err(|e| eyre::eyre!("list invoices: {e}"))?;
            let status_needle = status.map(|s| s.to_lowercase());
            let filtered: Vec<_> = rows
                .into_iter()
                .filter(|r| {
                    status_needle
                        .as_ref()
                        .is_none_or(|n| format!("{:?}", r.status).to_lowercase() == *n)
                })
                .collect();
            if json {
                let out: Vec<serde_json::Value> =
                    filtered.iter().map(json_out::invoice_json).collect();
                json_out::print_json(&out)?;
                return Ok(());
            }
            if filtered.is_empty() {
                println!("(no invoices)");
            }
            println!(
                "{:<24}  {:<11}  {:>12}  {:>12}  {:<10}",
                "number", "issued", "total", "balance", "status"
            );
            for r in filtered {
                println!(
                    "{:<24}  {:<11}  {:>12}  {:>12}  {:<10}",
                    r.number,
                    r.issue_date,
                    fmt_minor(r.total_minor),
                    fmt_minor(r.balance_minor),
                    format!("{:?}", r.status).to_lowercase(),
                );
            }
        }
        FinanceCmd::InvoiceShow { number, json } => {
            use finance_db::entity::{InvoiceColumn, InvoiceEntity};
            use sea_orm::{ColumnTrait, EntityTrait, QueryFilter};
            use sea_orm_migration::MigratorTrait;
            let url = format!("sqlite://{}?mode=rwc", ctx.root.finance_db().display());
            let conn = Database::connect(&url)
                .await
                .map_err(|e| eyre::eyre!("connect finance db: {e}"))?;
            finance_db::Migrator::up(&conn, None).await.ok();
            let row = InvoiceEntity::find()
                .filter(InvoiceColumn::Number.eq(number.clone()))
                .one(&conn)
                .await
                .map_err(|e| eyre::eyre!("query: {e}"))?
                .ok_or_else(|| eyre::eyre!("invoice `{number}` not found"))?;
            // Sessions stamped to this invoice (best-effort).
            let sessions = {
                use timer::entity::{WorkSessionColumn, WorkSessionEntity};
                WorkSessionEntity::find()
                    .filter(WorkSessionColumn::InvoiceId.eq(row.id))
                    .all(&timer_conn)
                    .await
                    .unwrap_or_default()
            };
            if json {
                let mut v = json_out::invoice_json(&row);
                if let serde_json::Value::Object(map) = &mut v {
                    let rows: Vec<serde_json::Value> = sessions
                        .into_iter()
                        .map(|m| json_out::session_json(&timer_proto::WorkSession::from(m)))
                        .collect();
                    map.insert("sessions".into(), serde_json::Value::Array(rows));
                }
                json_out::print_json(&v)?;
                return Ok(());
            }
            println!("Invoice {}", row.number);
            println!("  id:          {}", row.id);
            println!("  status:      {:?}", row.status);
            println!("  issued:      {}", row.issue_date);
            println!("  due:         {}", row.due_date);
            println!("  currency:    {}", row.currency);
            println!("  subtotal:    {}", fmt_minor(row.subtotal_minor));
            println!("  total:       {}", fmt_minor(row.total_minor));
            println!("  paid:        {}", fmt_minor(row.amount_paid_minor));
            println!("  balance:     {}", fmt_minor(row.balance_minor));
            println!("  party_id:    {}", row.party_id);
            println!("  book_id:     {}", row.book_id);
            println!("  line items:  {}", row.line_items.0.len());
            for li in &row.line_items.0 {
                println!(
                    "    - {}  qty={:.2}h  amount={}",
                    li.description,
                    (li.quantity_milli as f64) / 1000.0,
                    fmt_minor(li.line_total_minor),
                );
            }
            println!("  sessions:    {}", sessions.len());
            for s in sessions {
                println!(
                    "    - {}  {}",
                    s.start_time.format("%Y-%m-%d %H:%M"),
                    s.description
                );
            }
        }
        FinanceCmd::InvoiceMarkPaid {
            number,
            amount,
            on,
            memo,
            json,
        } => {
            use finance_db::entity::{InvoiceActive, InvoiceColumn, InvoiceEntity};
            use sea_orm::{
                ActiveModelTrait, ActiveValue::Set, ColumnTrait, EntityTrait, QueryFilter,
            };
            use sea_orm_migration::MigratorTrait;
            let url = format!("sqlite://{}?mode=rwc", ctx.root.finance_db().display());
            let conn = Database::connect(&url)
                .await
                .map_err(|e| eyre::eyre!("connect finance db: {e}"))?;
            finance_db::Migrator::up(&conn, None).await.ok();
            let row = InvoiceEntity::find()
                .filter(InvoiceColumn::Number.eq(number.clone()))
                .one(&conn)
                .await
                .map_err(|e| eyre::eyre!("query: {e}"))?
                .ok_or_else(|| eyre::eyre!("invoice `{number}` not found"))?;
            let outstanding = row.balance_minor;
            if outstanding <= 0 {
                return Err(eyre::eyre!(
                    "invoice `{number}` already has zero balance ({})",
                    fmt_minor(row.amount_paid_minor)
                ));
            }
            let pay = amount.unwrap_or(outstanding);
            if pay <= 0 {
                return Err(eyre::eyre!("--amount must be positive"));
            }
            if pay > outstanding {
                return Err(eyre::eyre!(
                    "--amount {} exceeds outstanding balance {}",
                    fmt_minor(pay),
                    fmt_minor(outstanding)
                ));
            }
            let new_paid = row.amount_paid_minor + pay;
            let new_balance = outstanding - pay;
            let new_status = if new_balance == 0 {
                finance_proto::invoice::InvoiceStatus::Paid
            } else {
                finance_proto::invoice::InvoiceStatus::PartiallyPaid
            };
            let on_date = on.unwrap_or_else(|| chrono::Utc::now().date_naive());
            let id = row.id;
            let mut active: InvoiceActive = row.into();
            active.amount_paid_minor = Set(new_paid);
            active.balance_minor = Set(new_balance);
            active.status = Set(new_status);
            active.updated_at = Set(chrono::Utc::now());
            active
                .update(&conn)
                .await
                .map_err(|e| eyre::eyre!("update invoice: {e}"))?;
            if json {
                json_out::print_json(&serde_json::json!({
                    "id": id,
                    "number": number,
                    "payment_minor": pay,
                    "on": on_date,
                    "memo": memo,
                    "status": format!("{new_status:?}").to_lowercase(),
                    "amount_paid_minor": new_paid,
                    "balance_minor": new_balance,
                }))?;
            } else {
                println!(
                    "Recorded payment of {} on {} ({}). status={:?}, paid={}, balance={}",
                    fmt_minor(pay),
                    on_date,
                    if memo.is_empty() { "no memo" } else { &memo },
                    new_status,
                    fmt_minor(new_paid),
                    fmt_minor(new_balance),
                );
            }
        }
        FinanceCmd::InvoiceVoid { number, json } => {
            use finance_db::entity::{InvoiceActive, InvoiceColumn, InvoiceEntity};
            use sea_orm::{
                ActiveModelTrait, ActiveValue::Set, ColumnTrait, EntityTrait, QueryFilter,
            };
            use sea_orm_migration::MigratorTrait;
            let url = format!("sqlite://{}?mode=rwc", ctx.root.finance_db().display());
            let conn = Database::connect(&url)
                .await
                .map_err(|e| eyre::eyre!("connect finance db: {e}"))?;
            finance_db::Migrator::up(&conn, None).await.ok();
            let row = InvoiceEntity::find()
                .filter(InvoiceColumn::Number.eq(number.clone()))
                .one(&conn)
                .await
                .map_err(|e| eyre::eyre!("query: {e}"))?
                .ok_or_else(|| eyre::eyre!("invoice `{number}` not found"))?;
            if row.amount_paid_minor > 0 {
                return Err(eyre::eyre!(
                    "invoice `{number}` has payments against it ({}). Issue a credit note instead.",
                    fmt_minor(row.amount_paid_minor)
                ));
            }
            let invoice_id = row.id;
            let mut active: InvoiceActive = row.into();
            active.status = Set(finance_proto::invoice::InvoiceStatus::Cancelled);
            active.updated_at = Set(chrono::Utc::now());
            active
                .update(&conn)
                .await
                .map_err(|e| eyre::eyre!("update invoice: {e}"))?;
            // Un-stamp the contributing sessions so they
            // become re-billable.
            use sea_orm::Database;
            let timer_url = std::env::var("TASK_TIMER_DB")
                .unwrap_or_else(|_| format!("sqlite://{}?mode=rwc", ctx.root.timer_db().display()));
            let tc = Database::connect(&timer_url)
                .await
                .map_err(|e| eyre::eyre!("connect timer db: {e}"))?;
            use timer::entity::{WorkSessionColumn, WorkSessionEntity};
            let cleared = WorkSessionEntity::update_many()
                .col_expr(
                    WorkSessionColumn::InvoiceId,
                    sea_orm::sea_query::Expr::value(Option::<uuid::Uuid>::None),
                )
                .col_expr(
                    WorkSessionColumn::UpdatedAt,
                    sea_orm::sea_query::Expr::value(chrono::Utc::now()),
                )
                .filter(WorkSessionColumn::InvoiceId.eq(invoice_id))
                .exec(&tc)
                .await
                .map_err(|e| eyre::eyre!("un-stamp sessions: {e}"))?;
            if json {
                json_out::print_json(&serde_json::json!({
                    "id": invoice_id,
                    "number": number,
                    "status": "cancelled",
                    "sessions_unstamped": cleared.rows_affected,
                }))?;
            } else {
                println!(
                    "Voided `{number}` and un-stamped {} session(s).",
                    cleared.rows_affected
                );
            }
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

/// Stitch assignee labels onto every line, sort by
/// (assignee → date), and synthesize the per-assignee
/// summary block + the two chart SVGs that the template
/// embeds verbatim.
///
/// Single-assignee invoices are left untouched (no column,
/// no summary, no charts) — the breakdown is only useful
/// when the work is split across people.
/// Scan `finance_invoices.number` for rows whose number
/// starts with `prefix` and whose suffix parses as an
/// integer; return `<prefix><next>` zero-padded to `pad`.
/// Starts at 1 if no match exists.
async fn next_invoice_number(
    conn: &sea_orm::DatabaseConnection,
    prefix: &str,
    pad: usize,
) -> eyre::Result<String> {
    use finance_db::entity::{InvoiceColumn, InvoiceEntity};
    use sea_orm::{ColumnTrait, EntityTrait, QueryFilter};
    let rows = InvoiceEntity::find()
        .filter(InvoiceColumn::Number.starts_with(prefix))
        .all(conn)
        .await
        .map_err(|e| eyre::eyre!("scan invoice numbers: {e}"))?;
    let highest = rows
        .iter()
        .filter_map(|r| {
            r.number
                .strip_prefix(prefix)
                .and_then(|s| s.parse::<u64>().ok())
        })
        .max()
        .unwrap_or(0);
    let next = highest + 1;
    Ok(format!("{prefix}{next:0>pad$}"))
}

fn enrich_invoice_with_assignees(
    ifp: &mut finance::pdf_adapter::InvoiceForPdf,
    line_meta: &[finance::invoice_from_sessions::LineMeta],
    names_by_id: &std::collections::HashMap<uuid::Uuid, String>,
) {
    if ifp.lines.len() != line_meta.len() || line_meta.is_empty() {
        return;
    }
    // Distinct chart-friendly palette. Reused mod-N for
    // unusually large teams.
    const PALETTE: &[&str] = &[
        "#3b82f6", "#f97316", "#10b981", "#a855f7", "#ef4444", "#eab308", "#0ea5e9", "#ec4899",
    ];
    let label_for = |uid: uuid::Uuid| -> String {
        names_by_id
            .get(&uid)
            .cloned()
            .unwrap_or_else(|| format!("user {}", &uid.simple().to_string()[..8]))
    };

    // Tag each line with its assignee name + carry the
    // matching meta through the sort so downstream
    // aggregations stay aligned with the rendered lines.
    let mut tagged: Vec<(
        usize,
        String,
        finance::pdf_adapter::InvoiceLineForPdf,
        finance::invoice_from_sessions::LineMeta,
    )> = ifp
        .lines
        .drain(..)
        .zip(line_meta.iter().copied())
        .enumerate()
        .map(|(i, (mut line, meta))| {
            let name = label_for(meta.user_id);
            line.assignee = name.clone();
            (i, name, line, meta)
        })
        .collect();
    tagged.sort_by(|a, b| a.1.cmp(&b.1).then(a.0.cmp(&b.0)));

    // Hide the per-line assignee column on
    // single-assignee invoices (no useful signal), but
    // still produce the per-task breakdown below.
    let distinct: std::collections::BTreeSet<&str> =
        tagged.iter().map(|(_, n, _, _)| n.as_str()).collect();
    let single_assignee = distinct.len() <= 1;
    if single_assignee {
        for (_, _, line, _) in &mut tagged {
            line.assignee.clear();
        }
    }

    // Aggregate by task (case-folded description) AND by
    // person from the sorted tuples — meta is paired with
    // its line so totals can't drift if sort order changes.
    let mut totals: std::collections::BTreeMap<String, (i64, i64)> =
        std::collections::BTreeMap::new();
    let mut by_person_raw: std::collections::BTreeMap<String, (i64, i64)> =
        std::collections::BTreeMap::new();
    for (_, _name, line, meta) in &tagged {
        let key = canonical_task_label(&line.description);
        let t = totals.entry(key).or_insert((0, 0));
        t.0 += meta.secs;
        t.1 += meta.cents;
        // Use the user-id directly so the per-person split
        // is correct even when we've hidden the column.
        let person = label_for(meta.user_id);
        if !person.is_empty() {
            let p = by_person_raw.entry(person).or_insert((0, 0));
            p.0 += meta.secs;
            p.1 += meta.cents;
        }
    }
    ifp.lines = tagged.into_iter().map(|(_, _, l, _)| l).collect();
    if totals.len() <= 1 {
        return;
    }
    let total_secs: i64 = totals.values().map(|(s, _)| *s).sum();
    let total_secs_f = total_secs.max(1) as f64;

    let tasks: Vec<finance::pdf_adapter::AssigneeSummary> = totals
        .iter()
        .enumerate()
        .map(|(i, (name, (secs, cents)))| {
            let hours = *secs as f64 / 3600.0;
            let pct = (*secs as f64) * 100.0 / total_secs_f;
            finance::pdf_adapter::AssigneeSummary {
                name: name.clone(),
                hours: format!("{hours:.2}"),
                amount: fmt_minor(*cents),
                pct: format!("{pct:.1}"),
                color: PALETTE[i % PALETTE.len()].to_string(),
            }
        })
        .collect();

    ifp.donut_svg = build_donut_svg(&tasks, &totals, total_secs);
    ifp.bars_svg = build_bars_svg(&tasks, &totals);
    ifp.assignees = tasks;

    // Per-person concise roll-up, computed above from the
    // sorted (line, meta) tuples — guaranteed aligned.
    let total_p_secs = by_person_raw.values().map(|(s, _)| *s).sum::<i64>().max(1) as f64;
    ifp.people = by_person_raw
        .into_iter()
        .enumerate()
        .map(
            |(i, (name, (secs, cents)))| finance::pdf_adapter::AssigneeSummary {
                name,
                hours: format!("{:.2}", secs as f64 / 3600.0),
                amount: fmt_minor(cents),
                pct: format!("{:.1}", (secs as f64) * 100.0 / total_p_secs),
                color: PALETTE[i % PALETTE.len()].to_string(),
            },
        )
        .collect();
}

/// Pull a stable task label out of a line description.
/// Lines are formatted as `"{date_prefix}  {description}"`
/// — the date prefix is either `YYYY-MM-DD` or
/// `YYYY-MM-DD – MM-DD`. Strip it, normalise the
/// remainder, and case-fold for grouping.
fn canonical_task_label(line_desc: &str) -> String {
    let trimmed = line_desc.trim_start();
    // Date prefix always starts with 10 chars of date —
    // skip until the first run of two spaces, which is
    // how the prefix is separated from the description.
    let body = trimmed.split_once("  ").map_or(trimmed, |(_, rest)| rest);
    let body = body.trim().trim_end_matches(" (mixed rates)");
    let mut out = String::with_capacity(body.len());
    let mut prev_was_space = false;
    for ch in body.chars() {
        if ch.is_whitespace() {
            if !prev_was_space {
                out.push(' ');
            }
            prev_was_space = true;
        } else {
            for c in ch.to_lowercase() {
                out.push(c);
            }
            prev_was_space = false;
        }
    }
    // Title-case the first letter so the legend reads
    // naturally ("Video editing" vs "video editing").
    let mut chars = out.chars();
    match chars.next() {
        Some(first) => first.to_uppercase().chain(chars).collect(),
        None => "Untitled".to_string(),
    }
}

/// SVG donut showing each assignee's share of total hours.
/// Inline + self-contained — fulgur fetches no externals.
fn build_donut_svg(
    summaries: &[finance::pdf_adapter::AssigneeSummary],
    totals: &std::collections::BTreeMap<String, (i64, i64)>,
    total_secs: i64,
) -> String {
    const SIZE: f64 = 110.0;
    const CX: f64 = SIZE / 2.0;
    const CY: f64 = SIZE / 2.0;
    const R_OUTER: f64 = 48.0;
    const R_INNER: f64 = 28.0;
    let total = total_secs.max(1) as f64;
    let mut start = -std::f64::consts::FRAC_PI_2; // 12 o'clock
    let mut paths = String::new();
    for s in summaries {
        let secs = totals.get(&s.name).map_or(0, |(sec, _)| *sec) as f64;
        let frac = secs / total;
        let sweep = frac * std::f64::consts::TAU;
        let end = start + sweep;
        // Single-slice (100%) needs a full-circle path
        // rather than two arcs that share both endpoints.
        let path = if (frac - 1.0).abs() < 1e-6 {
            format!(
                "M {x1:.3} {y1:.3} A {ro} {ro} 0 1 1 {x2:.3} {y2:.3} \
                 M {x3:.3} {y3:.3} A {ri} {ri} 0 1 0 {x4:.3} {y4:.3} Z",
                x1 = CX + R_OUTER,
                y1 = CY,
                x2 = CX + R_OUTER - 0.001,
                y2 = CY,
                x3 = CX + R_INNER,
                y3 = CY,
                x4 = CX + R_INNER - 0.001,
                y4 = CY,
                ro = R_OUTER,
                ri = R_INNER,
            )
        } else {
            let large = i32::from(sweep > std::f64::consts::PI);
            let (sx, sy) = (CX + R_OUTER * start.cos(), CY + R_OUTER * start.sin());
            let (ex, ey) = (CX + R_OUTER * end.cos(), CY + R_OUTER * end.sin());
            let (isx, isy) = (CX + R_INNER * end.cos(), CY + R_INNER * end.sin());
            let (iex, iey) = (CX + R_INNER * start.cos(), CY + R_INNER * start.sin());
            format!(
                "M {sx:.3} {sy:.3} A {R_OUTER} {R_OUTER} 0 {large} 1 {ex:.3} {ey:.3} \
                 L {isx:.3} {isy:.3} A {R_INNER} {R_INNER} 0 {large} 0 {iex:.3} {iey:.3} Z",
            )
        };
        paths.push_str(&format!("<path d=\"{path}\" fill=\"{}\" />", s.color));
        start = end;
    }
    format!(
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"{SIZE}\" height=\"{SIZE}\" viewBox=\"0 0 {SIZE} {SIZE}\">{paths}</svg>"
    )
}

/// Horizontal bars — amount billed per assignee, ranked
/// high-to-low. Pairs with the donut (hours share) so the
/// reader sees rate-weighted contribution too.
fn build_bars_svg(
    summaries: &[finance::pdf_adapter::AssigneeSummary],
    totals: &std::collections::BTreeMap<String, (i64, i64)>,
) -> String {
    const ROW_H: f64 = 16.0;
    const PAD_X: f64 = 4.0;
    const BAR_AREA_W: f64 = 110.0;
    let mut ranked: Vec<_> = summaries.iter().collect();
    ranked.sort_by(|a, b| {
        let av = totals.get(&a.name).map_or(0, |(_, c)| *c);
        let bv = totals.get(&b.name).map_or(0, |(_, c)| *c);
        bv.cmp(&av)
    });
    let max_cents = ranked
        .iter()
        .map(|a| totals.get(&a.name).map_or(0, |(_, c)| *c))
        .max()
        .unwrap_or(0)
        .max(1) as f64;
    let h = ROW_H * ranked.len() as f64 + 4.0;
    let w = BAR_AREA_W + PAD_X * 2.0;
    let mut bars = String::new();
    for (i, s) in ranked.iter().enumerate() {
        let cents = totals.get(&s.name).map_or(0, |(_, c)| *c) as f64;
        let bar_w = (cents / max_cents) * BAR_AREA_W;
        let y = i as f64 * ROW_H + 4.0;
        bars.push_str(&format!(
            "<rect x=\"{PAD_X}\" y=\"{y:.2}\" width=\"{bar_w:.2}\" height=\"8\" rx=\"2\" fill=\"{}\" />\
             <text x=\"{tx:.2}\" y=\"{ty:.2}\" font-size=\"6.5\" font-family=\"Helvetica,Arial,sans-serif\" fill=\"#222\">${}</text>",
            s.color,
            s.amount,
            tx = PAD_X + bar_w + 3.0,
            ty = y + 7.0,
        ));
    }
    format!(
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"{w}\" height=\"{h:.2}\" viewBox=\"0 0 {w} {h:.2}\">{bars}</svg>"
    )
}

/// Companion markdown stub for an invoice. Wikilinks the
/// PDF (Obsidian-style `![[pdfs/INV-...pdf]]` embed) so a
/// vault viewer can open the file inline. Frontmatter makes
/// the page queryable in `Reports/Invoices/*.base`.
#[allow(clippy::too_many_arguments)]
fn render_invoice_markdown(
    invoice: &finance_proto::invoice::Invoice,
    party: &finance_proto::party::Party,
    rel_pdf_path: &str,
    session_count: usize,
    period_start: chrono::DateTime<chrono::Utc>,
    period_end: chrono::DateTime<chrono::Utc>,
    people: &[finance::pdf_adapter::AssigneeSummary],
    tasks: &[finance::pdf_adapter::AssigneeSummary],
) -> String {
    let mut out = String::new();
    out.push_str("---\n");
    out.push_str("type: invoice\n");
    out.push_str(&format!("number: {}\n", invoice.number));
    out.push_str(&format!("status: {:?}\n", invoice.status).to_lowercase());
    out.push_str(&format!("issueDate: {}\n", invoice.issue_date));
    out.push_str(&format!("dueDate: {}\n", invoice.due_date));
    out.push_str(&format!(
        "periodStart: {}\n",
        period_start.format("%Y-%m-%d")
    ));
    out.push_str(&format!("periodEnd: {}\n", period_end.format("%Y-%m-%d")));
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
        "**To:** {}  \n**Issued:** {}  \n**Period:** {} → {}  \n**Total:** {} {}\n\n",
        party.display_name,
        invoice.issue_date,
        period_start.format("%Y-%m-%d"),
        period_end.format("%Y-%m-%d"),
        fmt_minor(invoice.total_minor),
        invoice.currency,
    ));
    out.push_str("## PDF\n\n");
    out.push_str(&format!("![[{rel_pdf_path}]]\n\n"));
    if !people.is_empty() {
        out.push_str("## Per person\n\n");
        out.push_str("| Member | Hours | Share | Amount |\n");
        out.push_str("|---|---:|---:|---:|\n");
        for p in people {
            out.push_str(&format!(
                "| {} | {} | {}% | {} |\n",
                p.name, p.hours, p.pct, p.amount
            ));
        }
        out.push('\n');
    }
    if !tasks.is_empty() {
        out.push_str("## Time by task\n\n");
        out.push_str("| Task | Hours | Share | Amount |\n");
        out.push_str("|---|---:|---:|---:|\n");
        for t in tasks {
            out.push_str(&format!(
                "| {} | {} | {}% | {} |\n",
                t.name, t.hours, t.pct, t.amount
            ));
        }
        out.push('\n');
    }
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

/// Deterministic local-owner user id for an org. MUST stay identical to
/// the web UI's `task_ui::chrome::owner_id` (`v5(org_id,
/// "task-local-owner")`) so the CLI and the `/timer` page resolve the
/// same user and therefore see the same sessions.
fn timer_owner_id(org_id: uuid::Uuid) -> uuid::Uuid {
    uuid::Uuid::new_v5(&org_id, b"task-local-owner")
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
    // Unified identity. The org id is the org's *manifest* id (the
    // same value the web UI gets from `.well-known` → `OrgMeta.id`),
    // and the default user is the deterministic "local owner" derived
    // from it — matching `task_ui::chrome::owner_id`. This is what makes
    // CLI- and UI-logged sessions land in the same `(org_id, user_id)`
    // keyspace so both surfaces see the same data. `TASK_ORG_ID` /
    // `TASK_USER_ID` still override (e.g. logging a contractor's time
    // under a distinct user id).
    let org_id = std::env::var("TASK_ORG_ID")
        .ok()
        .and_then(|s| s.parse::<uuid::Uuid>().ok())
        .or_else(|| ctx.root.manifest().ok().map(|m| m.id))
        .unwrap_or_else(|| uuid::Uuid::parse_str("00000000-0000-0000-0000-00000000000a").unwrap());
    let user_id = std::env::var("TASK_USER_ID")
        .ok()
        .and_then(|s| s.parse::<uuid::Uuid>().ok())
        .unwrap_or_else(|| timer_owner_id(org_id));

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

    // Per-org vox URL for `--task` / `--project` reference
    // resolution. `establish_for_url` honors `TASK_EMBED` (in-process
    // backend) vs a running server; the URL is only dialed when a
    // flag actually needs resolving, so plain local timer use (raw
    // uuids / no flags) keeps working fully offline.
    let vox_url = resolve_org_vox_url(None, ctx.root.slug());
    // `--task <id|prefix|path>` → TaskInfo, used to default the
    // description / project / task-note on start | switch | log.
    let resolve_task_flag = |flag: Option<String>| {
        let vox_url = vox_url.clone();
        async move {
            match flag {
                None => Ok::<_, eyre::Report>(None),
                Some(t) => {
                    let tc: task::TaskServiceClient = establish_for_url(&vox_url).await?;
                    Ok(Some(json_out::resolve_task_flexible(&tc, &t).await?))
                }
            }
        }
    };
    // `--project <uuid|title|path|prefix>` → (id, known-path).
    let resolve_project_flag = |flag: Option<String>| {
        let vox_url = vox_url.clone();
        async move {
            json_out::resolve_project_arg(flag.as_deref(), || async {
                establish_for_url::<::project::ProjectServiceClient>(&vox_url).await
            })
            .await
        }
    };

    match cmd {
        TimerCmd::Start {
            description,
            task,
            project,
            task_note,
            tags,
            json,
        } => {
            let task_info = resolve_task_flag(task).await?;
            let (mut project_id, resolved_path) = resolve_project_flag(project).await?;
            // --task fills the gaps; explicit flags win.
            if project_id.is_none() {
                project_id = task_info.as_ref().and_then(|t| t.project_id);
            }
            let description = description
                .or_else(|| task_info.as_ref().map(|t| t.title.clone()))
                .unwrap_or_default();
            let task_note = if task_note.is_empty() {
                task_info
                    .as_ref()
                    .map(|t| t.path.clone())
                    .unwrap_or_default()
            } else {
                task_note
            };
            let project_path =
                resolved_path.unwrap_or_else(|| project_path_for(&vault_root, project_id));
            let session = store
                .start_timer(StartTimerRequest {
                    user_id,
                    org_id,
                    project_id,
                    project_path,
                    task_note_path: task_note,
                    description,
                })
                .await
                .map_err(|e| eyre::eyre!("start: {e}"))?;
            attach_tags_by_name(store.conn(), org_id, session.id, &tags).await?;
            if json {
                json_out::print_json(&json_out::session_json(&session))?;
            } else {
                println!("Started {} at {}", session.id, session.start_time);
                println!("  description: {}", session.description);
                if !session.project_path.is_empty() {
                    println!("  project:     {}", session.project_path);
                }
                if !session.task_note_path.is_empty() {
                    println!("  task:        {}", session.task_note_path);
                }
                println!("  billable:    {}", session.billable);
                if !tags.is_empty() {
                    println!("  tags:        {}", tags.join(", "));
                }
            }
        }
        TimerCmd::Stop { json } => {
            let session = store
                .stop_timer(user_id)
                .await
                .map_err(|e| eyre::eyre!("stop: {e}"))?;
            if json {
                json_out::print_json(&json_out::session_json(&session))?;
            } else {
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
        }
        TimerCmd::Active { json } => {
            match store
                .active_timer(user_id)
                .await
                .map_err(|e| eyre::eyre!("{e}"))?
            {
                Some(s) => {
                    if json {
                        // Joined titles are best-effort: vox being
                        // down shouldn't break `active --json` —
                        // the entity + derived seconds still print.
                        let task_title = if s.task_note_path.is_empty() {
                            None
                        } else {
                            match establish_for_url::<task::TaskServiceClient>(&vox_url).await {
                                Ok(tc) => tc
                                    .get_by_path(s.task_note_path.clone())
                                    .await
                                    .ok()
                                    .map(|t| t.title),
                                Err(_) => None,
                            }
                        };
                        let project_title = match s.project_id {
                            None => None,
                            Some(pid) => {
                                match establish_for_url::<::project::ProjectServiceClient>(&vox_url)
                                    .await
                                {
                                    Ok(pc) => pc.get(pid).await.ok().map(|p| p.title),
                                    Err(_) => None,
                                }
                            }
                        };
                        json_out::print_json(&json_out::session_json_joined(
                            &s,
                            task_title,
                            project_title,
                        ))?;
                    } else {
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
                }
                None => {
                    if json {
                        println!("null");
                    } else {
                        println!("No active timer.");
                    }
                }
            }
        }
        TimerCmd::Switch {
            description,
            task,
            project,
            task_note,
            tags,
            json,
        } => {
            let task_info = resolve_task_flag(task).await?;
            let (mut project_id, resolved_path) = resolve_project_flag(project).await?;
            if project_id.is_none() {
                project_id = task_info.as_ref().and_then(|t| t.project_id);
            }
            let description = description
                .or_else(|| task_info.as_ref().map(|t| t.title.clone()))
                .unwrap_or_default();
            let task_note = if task_note.is_empty() {
                task_info
                    .as_ref()
                    .map(|t| t.path.clone())
                    .unwrap_or_default()
            } else {
                task_note
            };
            let project_path =
                resolved_path.unwrap_or_else(|| project_path_for(&vault_root, project_id));
            let (closed, started) = store
                .switch_timer(StartTimerRequest {
                    user_id,
                    org_id,
                    project_id,
                    project_path,
                    task_note_path: task_note,
                    description,
                })
                .await
                .map_err(|e| eyre::eyre!("switch: {e}"))?;
            attach_tags_by_name(store.conn(), org_id, started.id, &tags).await?;
            if json {
                json_out::print_json(&serde_json::json!({
                    "stopped": closed.as_ref().map(json_out::session_json),
                    "started": json_out::session_json(&started),
                }))?;
            } else {
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
        }
        TimerCmd::Log {
            description,
            from,
            to,
            task,
            project,
            task_note,
            billable,
            tags,
            json,
        } => {
            let task_info = resolve_task_flag(task).await?;
            let (mut project_id, resolved_path) = resolve_project_flag(project).await?;
            if project_id.is_none() {
                project_id = task_info.as_ref().and_then(|t| t.project_id);
            }
            let description = description
                .or_else(|| task_info.as_ref().map(|t| t.title.clone()))
                .unwrap_or_default();
            let task_note = if task_note.is_empty() {
                task_info
                    .as_ref()
                    .map(|t| t.path.clone())
                    .unwrap_or_default()
            } else {
                task_note
            };
            let project_path =
                resolved_path.unwrap_or_else(|| project_path_for(&vault_root, project_id));
            let session = store
                .log_session(LogSessionRequest {
                    user_id,
                    org_id,
                    project_id,
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
            if json {
                json_out::print_json(&json_out::session_json(&session))?;
            } else {
                println!("Logged {} ({})", session.id, fmt_duration(to - from));
            }
        }
        TimerCmd::SetRate {
            user_id,
            cents,
            currency,
            json,
        } => {
            store
                .set_org_member_rate(org_id, user_id, cents, &currency)
                .await
                .map_err(|e| eyre::eyre!("set rate: {e}"))?;
            if json {
                json_out::print_json(&serde_json::json!({
                    "org_id": org_id,
                    "user_id": user_id,
                    "hourly_cents": cents,
                    "currency": currency,
                }))?;
            } else {
                println!(
                    "Set org rate for {user_id}: {} {currency}/hr",
                    fmt_money(cents)
                );
            }
        }
        TimerCmd::Edit {
            id,
            description,
            from,
            to,
            project,
            user_id: edit_user,
            billable,
            task_note,
            json,
        } => {
            let (project_id, resolved_path) = resolve_project_flag(project).await?;
            // Reassigning the project also refreshes the cached
            // path (resolver-known path first, vault scan second).
            let project_path = project_id.map(|pid| {
                resolved_path.unwrap_or_else(|| project_path_for(&vault_root, Some(pid)))
            });
            let session = store
                .update_session(timer_proto::service::UpdateSessionRequest {
                    id,
                    user_id: edit_user,
                    project_id,
                    project_path,
                    task_note_path: task_note,
                    description,
                    start_time: from,
                    end_time: to,
                    billable,
                })
                .await
                .map_err(|e| eyre::eyre!("edit: {e}"))?;
            if json {
                json_out::print_json(&json_out::session_json(&session))?;
            } else {
                println!(
                    "Updated {} — \"{}\" [{}] {}/hr",
                    session.id,
                    session.description,
                    if session.billable {
                        "billable"
                    } else {
                        "non-billable"
                    },
                    fmt_money(session.rate_cents),
                );
            }
        }
        TimerCmd::Delete { id, json } => {
            store
                .delete_session(id)
                .await
                .map_err(|e| eyre::eyre!("delete: {e}"))?;
            if json {
                json_out::print_json(&serde_json::json!({ "deleted": id }))?;
            } else {
                println!("Deleted {id}");
            }
        }
        TimerCmd::List {
            project,
            user,
            since,
            until,
            open,
            billable,
            json,
        } => {
            let (project_id, _) = resolve_project_flag(project).await?;
            // No default user filter: sessions land in this
            // DB from several surfaces (CLI, web UI) whose
            // identity derivations have drifted, and a
            // silent owner filter made `list` undercount vs
            // the finance rollup (which has always been
            // org-wide).
            let filter = timer_proto::WorkSessionFilter {
                user_id: user,
                project_id,
                since: Some(
                    since.unwrap_or_else(|| chrono::Utc::now() - chrono::Duration::days(7)),
                ),
                until,
                billable,
                open,
            };
            let rows = store
                .query_sessions(&filter)
                .await
                .map_err(|e| eyre::eyre!("list: {e}"))?;
            if json {
                let out: Vec<serde_json::Value> = rows.iter().map(json_out::session_json).collect();
                json_out::print_json(&out)?;
                return Ok(());
            }
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
        TimerCmd::Users { json } => {
            // All sessions in scope; aggregate per user_id.
            let rows = store
                .query_sessions(&timer_proto::WorkSessionFilter::default())
                .await
                .map_err(|e| eyre::eyre!("list: {e}"))?;
            let mut agg: std::collections::BTreeMap<uuid::Uuid, (usize, i64, i64)> =
                std::collections::BTreeMap::new();
            for s in &rows {
                let e = agg.entry(s.user_id).or_default();
                e.0 += 1;
                let secs = s
                    .end_time
                    .unwrap_or(s.start_time)
                    .signed_duration_since(s.start_time)
                    .num_seconds()
                    .max(0);
                e.1 += secs;
                e.2 +=
                    i64::try_from(i128::from(secs) * i128::from(s.rate_cents) / 3600).unwrap_or(0);
            }
            // Resolve names from auth.sqlite — same lookup
            // the invoice path uses.
            let names = {
                use architect_auth::db::{AuthUserColumn, AuthUserEntity};
                use sea_orm::{ColumnTrait, Database, EntityTrait, QueryFilter};
                let mut map: std::collections::HashMap<uuid::Uuid, String> =
                    std::collections::HashMap::new();
                let auth_path = ctx.root.auth_db();
                if auth_path.exists() {
                    let url = format!("sqlite://{}?mode=ro", auth_path.display());
                    if let Ok(db) = Database::connect(&url).await {
                        let ids: Vec<uuid::Uuid> = agg.keys().copied().collect();
                        if let Ok(users) = AuthUserEntity::find()
                            .filter(AuthUserColumn::Id.is_in(ids))
                            .all(&db)
                            .await
                        {
                            for u in users {
                                let lbl = u
                                    .name
                                    .filter(|s| !s.is_empty())
                                    .or(u.email)
                                    .unwrap_or_default();
                                map.insert(u.id, lbl);
                            }
                        }
                    }
                }
                map
            };
            if json {
                let out: Vec<serde_json::Value> = agg
                    .iter()
                    .map(|(uid, (count, secs, cents))| {
                        serde_json::json!({
                            "user_id": uid,
                            "sessions": count,
                            "seconds": secs,
                            "cents": cents,
                            "name": names.get(uid),
                        })
                    })
                    .collect();
                json_out::print_json(&out)?;
                return Ok(());
            }
            if agg.is_empty() {
                println!("(no sessions)");
            }
            println!(
                "{:<38}  {:>6}  {:>9}  {:>10}  name",
                "user_id", "count", "hours", "cents"
            );
            for (uid, (count, secs, cents)) in agg {
                let hours = secs as f64 / 3600.0;
                let name = names
                    .get(&uid)
                    .cloned()
                    .unwrap_or_else(|| "(not in auth_users)".into());
                println!("{uid:<38}  {count:>6}  {hours:>9.2}  {cents:>10}  {name}");
            }
        }
        TimerCmd::ReassignUser {
            from,
            to,
            project,
            since,
            until,
            description_contains,
            rerate,
            dry_run,
            json,
        } => {
            let (project_id, _) = resolve_project_flag(project).await?;
            let filter = timer_proto::WorkSessionFilter {
                user_id: Some(from),
                project_id,
                since,
                until,
                billable: None,
                open: None,
            };
            let rows = store
                .query_sessions(&filter)
                .await
                .map_err(|e| eyre::eyre!("list: {e}"))?;
            let needle = description_contains.map(|s| s.to_lowercase());
            let matched: Vec<_> = rows
                .into_iter()
                .filter(|s| {
                    needle
                        .as_ref()
                        .is_none_or(|n| s.description.to_lowercase().contains(n.as_str()))
                })
                .collect();
            if !json {
                println!(
                    "{} session(s) match (from={from}, to={to}, rerate={rerate}, dry_run={dry_run})",
                    matched.len()
                );
                for s in &matched {
                    println!(
                        "  {}  {}  {}",
                        s.start_time.format("%Y-%m-%d %H:%M"),
                        s.id,
                        s.description
                    );
                }
            }
            if dry_run || matched.is_empty() {
                if json {
                    json_out::print_json(&serde_json::json!({
                        "from": from,
                        "to": to,
                        "rerate": rerate,
                        "dry_run": dry_run,
                        "matched": matched.len(),
                        "updated": 0,
                        "session_ids": matched.iter().map(|s| s.id).collect::<Vec<_>>(),
                    }))?;
                }
                return Ok(());
            }
            let mut updated = 0_usize;
            for s in &matched {
                if rerate {
                    // Goes through `update_session`, which
                    // re-snapshots `rate_cents` + `currency`
                    // from the cascade for the new user.
                    store
                        .update_session(timer_proto::service::UpdateSessionRequest {
                            id: s.id,
                            user_id: Some(to),
                            ..Default::default()
                        })
                        .await
                        .map_err(|e| eyre::eyre!("reassign {}: {e}", s.id))?;
                } else {
                    // Preserve the historical rate snapshot
                    // — only swap user_id. Direct SeaORM
                    // update bypasses cascade re-resolution.
                    use sea_orm::{ActiveModelTrait, EntityTrait, Set};
                    use timer::entity::{WorkSessionActive, WorkSessionEntity};
                    let row = WorkSessionEntity::find_by_id(s.id)
                        .one(store.conn())
                        .await?
                        .ok_or_else(|| eyre::eyre!("session {} disappeared", s.id))?;
                    let mut active: WorkSessionActive = row.into();
                    active.user_id = Set(to);
                    active.updated_at = Set(chrono::Utc::now());
                    active.update(store.conn()).await?;
                }
                updated += 1;
            }
            if json {
                json_out::print_json(&serde_json::json!({
                    "from": from,
                    "to": to,
                    "rerate": rerate,
                    "dry_run": false,
                    "matched": matched.len(),
                    "updated": updated,
                    "session_ids": matched.iter().map(|s| s.id).collect::<Vec<_>>(),
                }))?;
            } else {
                println!("Updated {updated} session(s).");
            }
        }
        TimerCmd::Resolve { project, json } => {
            let (project_id, _) = resolve_project_flag(project).await?;
            let resolved = store
                .resolve_rate(user_id, project_id)
                .await
                .map_err(|e| eyre::eyre!("resolve: {e}"))?;
            if json {
                json_out::print_json(&resolved)?;
            } else {
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
        }
        TimerCmd::Tag(sub) => match sub {
            TimerTagCmd::List { json } => {
                let rows = TagEntity::find()
                    .filter(TagColumn::OrgId.eq(org_id))
                    .all(store.conn())
                    .await
                    .map_err(|e| eyre::eyre!("list tags: {e}"))?;
                if json {
                    let out: Vec<serde_json::Value> = rows
                        .into_iter()
                        .map(|t| json_out::tag_json(&timer_proto::Tag::from(t)))
                        .collect();
                    json_out::print_json(&out)?;
                    return Ok(());
                }
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
            TimerTagCmd::Create { name, color, json } => {
                let tag = ensure_tag(store.conn(), org_id, &name, &color).await?;
                if json {
                    json_out::print_json(&json_out::tag_json(&timer_proto::Tag::from(tag)))?;
                } else {
                    println!("{}  {}", tag.id, tag.name);
                }
            }
            TimerTagCmd::Rm { name, json } => {
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
                if json {
                    json_out::print_json(&serde_json::json!({
                        "deleted": json_out::tag_json(&timer_proto::Tag::from(tag)),
                    }))?;
                } else {
                    println!("Deleted tag {} ({})", tag.name, tag.id);
                }
            }
            TimerTagCmd::Attach {
                session_id,
                tags,
                json,
            } => {
                attach_tags_by_name(store.conn(), org_id, session_id, &tags).await?;
                if json {
                    json_out::print_json(&serde_json::json!({
                        "session_id": session_id,
                        "attached": tags,
                    }))?;
                } else {
                    println!("Attached {} to {session_id}", tags.join(", "));
                }
            }
            TimerTagCmd::Detach {
                session_id,
                tags,
                all,
                json,
            } => {
                if all {
                    WorkSessionTagEntity::delete_many()
                        .filter(WorkSessionTagColumn::WorkSessionId.eq(session_id))
                        .exec(store.conn())
                        .await
                        .map_err(|e| eyre::eyre!("detach all: {e}"))?;
                    if json {
                        json_out::print_json(&serde_json::json!({
                            "session_id": session_id,
                            "detached": "all",
                        }))?;
                    } else {
                        println!("Detached all tags from {session_id}");
                    }
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
                    if json {
                        json_out::print_json(&serde_json::json!({
                            "session_id": session_id,
                            "detached": tags,
                        }))?;
                    } else {
                        println!("Detached {} from {session_id}", tags.join(", "));
                    }
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
/// id by scanning `Projects/**/*.md` recursively (projects
/// conventionally live in their own folder, e.g.
/// `Projects/<Name>/<Name>.md` — a flat scan misses them and
/// every session then stores an empty `project_path`).
/// `None` project_id → empty.
fn project_path_for(vault_root: &std::path::Path, project_id: Option<uuid::Uuid>) -> String {
    let Some(pid) = project_id else {
        return String::new();
    };
    let mut dirs = vec![vault_root.join("Projects")];
    while let Some(dir) = dirs.pop() {
        let Ok(entries) = std::fs::read_dir(&dir) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                dirs.push(path);
                continue;
            }
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
            let Ok(p) = ::project::parse_str(&rel, basename, &raw) else {
                continue;
            };
            if p.id == pid {
                return rel;
            }
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
            notes,
            links,
        } => {
            let vault = vault
                .canonicalize()
                .map_err(|e| eyre::eyre!("vault {}: {e}", vault.display()))?;
            // Typed-link overlay: note↔note links become
            // direct-link edges (verses aren't graph nodes;
            // endpoints missing from the graph are dropped
            // inside `build_context`).
            let extra_edges = match links {
                Some(path) => {
                    use links::LinksService as _;
                    links::Store::open(path)
                        .graph(links::Confidence::Speculative, true)
                        .map_err(|e| eyre::eyre!("links store: {e}"))?
                        .into_iter()
                        .filter(|l| {
                            l.source.kind == links::NodeKind::Note
                                && l.target.kind == links::NodeKind::Note
                        })
                        .map(|l| {
                            (
                                format!("{}{}", wiki_graph::NOTE_ID_PREFIX, l.source.id),
                                format!("{}{}", wiki_graph::NOTE_ID_PREFIX, l.target.id),
                            )
                        })
                        .collect()
                }
                None => Vec::new(),
            };
            let result = wiki_graph::build_context(
                &vault,
                wiki_graph::ContextOpts {
                    query,
                    node_type,
                    budget_tokens,
                    max_nodes,
                    summary_chars,
                    notes_root: notes,
                    extra_edges,
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
        WikiCmd::Archive(args) => run_wiki_archive(args).await,
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
        establish_for_url(url).await
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
        establish_for_url(url).await
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
        establish_for_url(url).await
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

/// `task wiki archive <url|file>` — extract locally, then
/// feed the UNCHANGED raw→ingest pipeline over RPC:
/// `import_raw_source` (sha-dedup server-side) +
/// `enqueue_ingest`. Canonical-URL dedup happens client-side
/// by filename scan — archived sources are named
/// `<slug>-<canon8>.md` so the same resource reached via
/// different links collapses to one file.
async fn run_wiki_archive(mut args: WikiArchiveArgs) -> eyre::Result<()> {
    use wiki_proto::raw::ImportRawSource;
    use wiki_proto::service::ingest::IngestClient;
    use wiki_proto::service::raw_layer::RawLayerClient;

    match args.cmd.take() {
        Some(WikiArchiveSub::Import(cmd)) => return run_wiki_archive_import(cmd).await,
        Some(WikiArchiveSub::Health { org, json }) => {
            return run_wiki_archive_health(org, json);
        }
        Some(WikiArchiveSub::Retry {
            limit,
            wiki_id,
            org,
            server,
            no_enqueue,
        }) => {
            return run_wiki_archive_retry(limit, wiki_id, org, server, no_enqueue).await;
        }
        None => {}
    }
    let target = args
        .target
        .clone()
        .ok_or_else(|| eyre::eyre!("a URL or file path is required"))?;

    let slug = resolve_active_org(args.org.clone())?;
    let vox_url = resolve_org_vox_url(args.server.clone(), &slug);

    // ── Resolve the target into import-ready parts ──────
    let local = std::path::Path::new(&target);
    let (filename, mime, title, bytes, canon8) = if local.exists() {
        // Local file: bytes go in as-is (binary originals
        // included) — the ingest pipeline + wiki-extract
        // already handle md/txt/pdf/docx. No provenance
        // frontmatter is injected into foreign bytes.
        let name = local
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or("source")
            .to_string();
        let mime = archive_mime_for_filename(&name);
        let title = args.title.clone().unwrap_or_else(|| name.clone());
        (name, mime, title, std::fs::read(local)?, None)
    } else {
        // Health bookkeeping — every attempt lands in the
        // per-org extractor ledger (`task wiki archive health`).
        let route_label = wiki_archive::classify(&target)
            .ok()
            .map(|(_, r)| wiki_archive::health::route_label(&r));
        let health_path = archive_health_path(&slug);
        match archive_extract_url(&target, &args).await {
            Ok((prov, body)) => {
                if let (Some(p), Some(label)) = (&health_path, route_label) {
                    wiki_archive::health::record(p, label, Ok(()));
                }
                let md = wiki_archive::compose_source_markdown(&prov, &body);
                (
                    prov.filename(),
                    "text/markdown".to_string(),
                    prov.title.clone(),
                    md.into_bytes(),
                    Some(prov.canon8()),
                )
            }
            Err(e) => {
                if let (Some(p), Some(label)) = (&health_path, route_label) {
                    wiki_archive::health::record(p, label, Err(&e.to_string()));
                }
                // Accept-fragility routes (Reddit/X) store an
                // honest unarchived stub instead of failing —
                // `task wiki archive retry` sweeps them later.
                let Some((prov, body)) = unarchived_stub(&target, &e.to_string()) else {
                    return Err(eyre::eyre!("archive {target}: {e}"));
                };
                println!("could not archive {target}: {e}");
                println!(
                    "storing an unarchived stub — `task wiki archive retry` re-attempts later"
                );
                let md = wiki_archive::compose_source_markdown(&prov, &body);
                (
                    format!("unarchived-{}", prov.filename()),
                    "text/markdown".to_string(),
                    prov.title.clone(),
                    md.into_bytes(),
                    Some(prov.canon8()),
                )
            }
        }
    };

    let raw: RawLayerClient = establish_for_url(&vox_url).await?;

    // ── Canonical-URL dedup ─────────────────────────────
    let mut stale_stub: Option<String> = None;
    if let Some(c8) = &canon8 {
        let existing = raw
            .list_raw_sources(args.wiki_id.clone())
            .await
            .map_err(|e| eyre::eyre!("list_raw_sources: {e:?}"))?;
        let hit =
            wiki_archive::find_canonical_match(existing.iter().map(|r| r.filename.as_str()), c8)
                .map(ToString::to_string);
        if let Some(hit) = hit {
            let importing_stub = filename.starts_with("unarchived-");
            if importing_stub {
                // Something (stub or real) already tracks this
                // canonical URL — never stack stubs.
                println!("already tracked as raw/sources/{hit} — not writing another stub");
                return Ok(());
            }
            if hit.starts_with("unarchived-") {
                // A failed earlier attempt left a stub and this
                // run extracted successfully — replace it.
                println!("replacing unarchived stub {hit}");
                stale_stub = Some(hit);
            } else if args.force {
                println!("note: canonical URL already archived as {hit} (continuing: --force)");
            } else {
                if args.json {
                    println!(
                        "{}",
                        serde_json::json!({ "deduped": true, "existing": hit, "canon8": c8 })
                    );
                } else {
                    println!("already archived (canonical-url dedup): raw/sources/{hit}");
                    println!("pass --force to archive a fresh copy");
                }
                return Ok(());
            }
        }
    }

    // ── Import (server sha-dedups byte-identical content) ──
    let r = raw
        .import_raw_source(
            args.wiki_id.clone(),
            ImportRawSource {
                filename,
                mime,
                title,
                bytes,
                auto_enqueue: false, // explicit enqueue below
            },
        )
        .await
        .map_err(|e| eyre::eyre!("import_raw_source: {e:?}"))?;

    // The successful archive supersedes any unarchived stub.
    if let Some(stub) = stale_stub {
        if let Err(e) = raw
            .delete_raw_source(args.wiki_id.clone(), format!("raw/sources/{stub}"))
            .await
        {
            println!("note: could not delete stale stub {stub}: {e:?}");
        }
    }

    // ── Enqueue ingest ──────────────────────────────────
    let task_id = if args.no_enqueue {
        None
    } else {
        let ing: IngestClient = establish_for_url(&vox_url).await?;
        let task = ing
            .enqueue_ingest(
                args.wiki_id.clone(),
                r.path.clone(),
                wiki_proto::ingest::SourceChange::Created,
            )
            .await
            .map_err(|e| eyre::eyre!("enqueue_ingest: {e:?}"))?;
        Some(task.id)
    };

    if args.json {
        println!(
            "{}",
            serde_json::json!({
                "path": r.path,
                "sha256": r.sha256,
                "size": r.size,
                "title": r.title,
                "ingest_task": task_id,
            })
        );
    } else {
        println!(
            "archived: {} ({} bytes, sha256 {})",
            r.path,
            r.size,
            &r.sha256[..12]
        );
        match task_id {
            Some(id) => println!("ingest task enqueued: {id}"),
            None => println!("ingest not enqueued (--no-enqueue)"),
        }
    }
    Ok(())
}

/// Per-org extractor-health ledger location (best-effort —
/// health is advisory and never blocks an archive).
fn archive_health_path(slug: &str) -> Option<std::path::PathBuf> {
    org_proto::DataRoot::from_env()
        .ok()
        .map(|root| root.org(slug).path().join("wiki-archive-health.json"))
}

/// Build the unarchived-stub `(Provenance, body)` for a
/// failed extraction — but ONLY for accept-fragility routes
/// (Reddit/X), where blocks are expected and a retry sweep
/// exists. Everything else keeps fail-loud semantics.
fn unarchived_stub(target: &str, error: &str) -> Option<(wiki_archive::Provenance, String)> {
    let (url, route) = wiki_archive::classify(target).ok()?;
    if !matches!(
        route,
        wiki_archive::Route::Reddit { .. } | wiki_archive::Route::Tweet { .. }
    ) {
        return None;
    }
    let canonical = wiki_archive::canonicalize(&url, &route);
    let label = wiki_archive::health::route_label(&route);
    let mut prov = wiki_archive::Provenance::new(
        target,
        target,
        canonical,
        wiki_archive::content_type_for(&route),
        "unarchived",
    );
    prov.archive_status = Some("unarchived".into());
    prov.archive_error = Some(error.to_string());
    let body = format!(
        "## Archive status\n\nNot archived yet — {error}\n\nThe `{label}` route is \
         accept-fragility tier: blocks and shape drift are expected, and the content \
         is still only a retry away. Re-run `task wiki archive retry` later \
         (cron-friendly), or re-archive this URL directly once the block clears."
    );
    Some((prov, body))
}

/// `task wiki archive health` — render the per-org ledger.
fn run_wiki_archive_health(org: Option<String>, json: bool) -> eyre::Result<()> {
    let slug = resolve_active_org(org)?;
    let path = archive_health_path(&slug)
        .ok_or_else(|| eyre::eyre!("no data root — set TASK_DATA_ROOT or run `task org init`"))?;
    let ledger = wiki_archive::health::load(&path);
    if json {
        println!("{}", serde_json::to_string_pretty(&ledger)?);
    } else {
        println!("{}", wiki_archive::health::render(&ledger));
    }
    Ok(())
}

/// `task wiki archive retry` — sweep `unarchived-*` stubs and
/// re-attempt them, throttled per route. Always exits 0 so a
/// cron line stays quiet; the summary tells the story.
async fn run_wiki_archive_retry(
    limit: Option<usize>,
    wiki_id: String,
    org: Option<String>,
    server: Option<String>,
    no_enqueue: bool,
) -> eyre::Result<()> {
    use wiki_proto::raw::ImportRawSource;
    use wiki_proto::service::ingest::IngestClient;
    use wiki_proto::service::raw_layer::RawLayerClient;

    let slug = resolve_active_org(org.clone())?;
    let vox_url = resolve_org_vox_url(server.clone(), &slug);
    let raw: RawLayerClient = establish_for_url(&vox_url).await?;
    let health_path = archive_health_path(&slug);

    let stubs: Vec<String> = raw
        .list_raw_sources(wiki_id.clone())
        .await
        .map_err(|e| eyre::eyre!("list_raw_sources: {e:?}"))?
        .into_iter()
        .map(|r| r.filename)
        .filter(|f| f.starts_with("unarchived-"))
        .collect();
    if stubs.is_empty() {
        println!("no unarchived stubs — nothing to retry");
        return Ok(());
    }
    let limit = limit.unwrap_or(usize::MAX);
    println!("{} unarchived stub(s); retrying up to {limit}", stubs.len());

    // Default single-shot args for re-extraction.
    let extract_args = WikiArchiveArgs {
        cmd: None,
        target: None,
        title: None,
        force: false,
        no_enqueue,
        yt_dlp: std::env::var("TASK_YTDLP").unwrap_or_else(|_| "yt-dlp".into()),
        pdftotext: std::env::var("TASK_PDFTOTEXT").unwrap_or_else(|_| "pdftotext".into()),
        episode: None,
        transcribe: "auto".into(),
        whisper_model: std::env::var("TASK_WHISPER_MODEL").unwrap_or_else(|_| "small".into()),
        ffmpeg: std::env::var("TASK_FFMPEG").unwrap_or_else(|_| "ffmpeg".into()),
        wiki_id: wiki_id.clone(),
        org,
        server,
        json: false,
    };

    let (mut recovered, mut still_blocked, mut skipped) = (0usize, 0usize, 0usize);
    let mut first = true;
    for stub in stubs.iter().take(limit) {
        // Pull the original URL out of the stub frontmatter.
        let bytes = match raw
            .read_raw_source(wiki_id.clone(), format!("raw/sources/{stub}"))
            .await
        {
            Ok(b) => b,
            Err(e) => {
                println!("  skip {stub}: read failed: {e:?}");
                skipped += 1;
                continue;
            }
        };
        let text = String::from_utf8_lossy(&bytes);
        let Some(source_url) = text
            .lines()
            .find_map(|l| l.strip_prefix("source_url:"))
            .map(|v| v.trim().trim_matches('"').to_string())
            .filter(|v| !v.is_empty())
        else {
            println!("  skip {stub}: no source_url in frontmatter");
            skipped += 1;
            continue;
        };

        // Throttle BETWEEN requests, per route — Reddit
        // tolerates ~10/min anonymously.
        let route = wiki_archive::classify(&source_url).ok().map(|(_, r)| r);
        if !first {
            let pause = match route {
                Some(wiki_archive::Route::Reddit { .. }) => {
                    wiki_archive::reddit::MIN_REQUEST_INTERVAL
                }
                _ => std::time::Duration::from_secs(1),
            };
            tokio::time::sleep(pause).await;
        }
        first = false;

        let label = route.as_ref().map(wiki_archive::health::route_label);
        match archive_extract_url(&source_url, &extract_args).await {
            Ok((prov, body)) => {
                if let (Some(p), Some(label)) = (&health_path, label) {
                    wiki_archive::health::record(p, label, Ok(()));
                }
                let md = wiki_archive::compose_source_markdown(&prov, &body);
                let r = raw
                    .import_raw_source(
                        wiki_id.clone(),
                        ImportRawSource {
                            filename: prov.filename(),
                            mime: "text/markdown".into(),
                            title: prov.title.clone(),
                            bytes: md.into_bytes(),
                            auto_enqueue: false,
                        },
                    )
                    .await
                    .map_err(|e| eyre::eyre!("import_raw_source {source_url}: {e:?}"))?;
                if !no_enqueue {
                    let ing: IngestClient = establish_for_url(&vox_url).await?;
                    ing.enqueue_ingest(
                        wiki_id.clone(),
                        r.path.clone(),
                        wiki_proto::ingest::SourceChange::Created,
                    )
                    .await
                    .map_err(|e| eyre::eyre!("enqueue_ingest {}: {e:?}", r.path))?;
                }
                if let Err(e) = raw
                    .delete_raw_source(wiki_id.clone(), format!("raw/sources/{stub}"))
                    .await
                {
                    println!("  note: stub {stub} not deleted: {e:?}");
                }
                println!("  ✓ {source_url} → {}", r.path);
                recovered += 1;
            }
            Err(e) => {
                if let (Some(p), Some(label)) = (&health_path, label) {
                    wiki_archive::health::record(p, label, Err(&e.to_string()));
                }
                println!("  ✗ {source_url}: still blocked ({e})");
                still_blocked += 1;
            }
        }
    }
    println!(
        "retry sweep: {recovered} recovered, {still_blocked} still blocked, {skipped} skipped \
         (of {})",
        stubs.len()
    );
    Ok(())
}

/// Route a URL to its extractor and run it. Returns the
/// provenance + extracted markdown body.
async fn archive_extract_url(
    target: &str,
    args: &WikiArchiveArgs,
) -> eyre::Result<(wiki_archive::Provenance, String)> {
    let title_override = args.title.clone();
    let yt_dlp = args.yt_dlp.as_str();
    let (url, route) = wiki_archive::classify(target).map_err(|e| eyre::eyre!("{e}"))?;
    let canonical = wiki_archive::canonicalize(&url, &route);
    let content_type = wiki_archive::content_type_for(&route);
    match route {
        wiki_archive::Route::Article => {
            let client = wiki_archive::article::http_client().map_err(|e| eyre::eyre!("{e}"))?;
            // Fetch once as bytes: extensionless PDF links
            // (arxiv `/pdf/…`, DOI redirects) divert to the
            // PDF extractor on content-type or magic bytes.
            let (ct, bytes) = wiki_archive::article::fetch_bytes(
                &client,
                url.as_str(),
                "text/html,application/xhtml+xml,application/pdf;q=0.9,*/*;q=0.8",
            )
            .await
            .map_err(|e| eyre::eyre!("{e}"))?;
            if ct == "application/pdf" || bytes.starts_with(b"%PDF-") {
                return archive_pdf_from_bytes(target, canonical, args, &bytes).await;
            }
            let html = String::from_utf8_lossy(&bytes);
            let a = wiki_archive::article::extract_article(&html, url.as_str())
                .map_err(|e| eyre::eyre!("{e}"))?;
            let title = title_override
                .or_else(|| (!a.title.is_empty()).then(|| a.title.clone()))
                .unwrap_or_else(|| target.to_string());
            let prov = wiki_archive::Provenance::new(
                title,
                target,
                canonical,
                content_type,
                "dom_smoothie",
            );
            let body = match &a.byline {
                Some(byline) => format!("_{byline}_\n\n{}", a.markdown),
                None => a.markdown,
            };
            Ok((prov, body))
        }
        wiki_archive::Route::Pdf => {
            let client = wiki_archive::article::http_client().map_err(|e| eyre::eyre!("{e}"))?;
            let (_ct, bytes) = wiki_archive::article::fetch_bytes(
                &client,
                url.as_str(),
                "application/pdf,*/*;q=0.8",
            )
            .await
            .map_err(|e| eyre::eyre!("{e}"))?;
            archive_pdf_from_bytes(target, canonical, args, &bytes).await
        }
        wiki_archive::Route::GoogleDoc { doc_id } => {
            let client = wiki_archive::article::http_client().map_err(|e| eyre::eyre!("{e}"))?;
            let (doc_title, md) = wiki_archive::article::fetch_google_doc(&client, &doc_id)
                .await
                .map_err(|e| eyre::eyre!("{e}"))?;
            let title = title_override.unwrap_or(doc_title);
            let prov = wiki_archive::Provenance::new(
                title,
                target,
                canonical,
                content_type,
                "gdocs-export",
            );
            Ok((prov, md))
        }
        wiki_archive::Route::ApplePodcast {
            podcast_id,
            episode_id,
        } => {
            let client = wiki_archive::article::http_client().map_err(|e| eyre::eyre!("{e}"))?;
            let show = wiki_archive::podcast::apple_lookup_feed(&client, &podcast_id)
                .await
                .map_err(|e| eyre::eyre!("{e}"))?;
            // Episode links need the entity=podcastEpisode
            // listing — a direct lookup of the episode id
            // returns 0 results.
            let episode_hint = match (&episode_id, &args.episode) {
                (Some(ep), _) => {
                    let hint =
                        wiki_archive::podcast::apple_lookup_episode_title(&client, &podcast_id, ep)
                            .await
                            .map_err(|e| eyre::eyre!("{e}"))?;
                    if hint.is_none() {
                        println!(
                            "note: episode id {ep} not in the show's latest 200 — archiving the latest episode instead (pass --episode <title> to pick)"
                        );
                    }
                    hint
                }
                (None, Some(title)) => Some(title.clone()),
                (None, None) => None,
            };
            archive_podcast_from_feed(
                target,
                canonical,
                args,
                &client,
                &show.feed_url,
                show.show_title.as_deref(),
                episode_hint.as_deref(),
            )
            .await
        }
        wiki_archive::Route::SpotifyPodcast { .. } => {
            let client = wiki_archive::article::http_client().map_err(|e| eyre::eyre!("{e}"))?;
            let oembed_title = wiki_archive::podcast::spotify_oembed_title(&client, target)
                .await
                .map_err(|e| eyre::eyre!("{e}"))?;
            // Podcast Index title-search: the only route from a
            // Spotify URL back to public RSS. Best-effort.
            let pi_key = std::env::var("PODCASTINDEX_API_KEY")
                .ok()
                .filter(|s| !s.is_empty());
            let pi_secret = std::env::var("PODCASTINDEX_API_SECRET")
                .ok()
                .filter(|s| !s.is_empty());
            if let (Some(key), Some(secret)) = (pi_key, pi_secret) {
                match wiki_archive::podcast::podcastindex_feed_by_title(
                    &client,
                    &key,
                    &secret,
                    &oembed_title,
                )
                .await
                {
                    Ok(Some(feed_url)) => {
                        println!("podcastindex resolved a public feed: {feed_url}");
                        return archive_podcast_from_feed(
                            target,
                            canonical,
                            args,
                            &client,
                            &feed_url,
                            None,
                            Some(&oembed_title),
                        )
                        .await;
                    }
                    Ok(None) => println!(
                        "podcastindex: no feed matched `{oembed_title}` — falling back to metadata-only"
                    ),
                    Err(e) => {
                        println!(
                            "podcastindex lookup failed ({e}) — falling back to metadata-only"
                        );
                    }
                }
            }
            // Honest metadata-only archive: Spotify exposes no
            // public audio stream or transcript.
            let item = wiki_archive::feed::FeedItem {
                title: oembed_title.clone(),
                ..Default::default()
            };
            let body = wiki_archive::podcast::render_podcast_markdown(
                None,
                &item,
                &[],
                Some(
                    "Spotify-exclusive: Spotify exposes no public audio stream or \
                     transcript for this episode, so only metadata could be archived. \
                     If the show also publishes a public RSS feed, archive its Apple \
                     Podcasts page or feed URL instead — or set PODCASTINDEX_API_KEY / \
                     PODCASTINDEX_API_SECRET so Task can resolve it by title.",
                ),
            );
            let title = args.title.clone().unwrap_or(oembed_title);
            let mut prov = wiki_archive::Provenance::new(
                title,
                target,
                canonical,
                content_type,
                "spotify-oembed",
            );
            prov.media = Some(target.to_string());
            Ok((prov, body))
        }
        wiki_archive::Route::Reddit { permalink } => {
            // Dedicated client: Reddit 403s self-identifying
            // UAs (no loid cookie) — see reddit.rs.
            let client = wiki_archive::reddit::client().map_err(|e| eyre::eyre!("{e}"))?;
            let thread = wiki_archive::reddit::fetch_thread(&client, &permalink)
                .await
                .map_err(|e| eyre::eyre!("{e}"))?;
            let body = wiki_archive::reddit::render_reddit_markdown(&thread);
            let title = title_override.unwrap_or_else(|| thread.title.clone());
            let prov = wiki_archive::Provenance::new(
                title,
                target,
                canonical,
                content_type,
                "reddit-json",
            );
            Ok((prov, body))
        }
        wiki_archive::Route::Tweet { status_id } => {
            let client = wiki_archive::article::http_client().map_err(|e| eyre::eyre!("{e}"))?;
            let result = wiki_archive::x::fetch_tweet(&client, &status_id)
                .await
                .map_err(|e| eyre::eyre!("{e}"))?;
            let body = wiki_archive::x::render_tweet_markdown(&result.tweet, target);
            let title = title_override.unwrap_or_else(|| {
                let head: String = result.tweet.text.chars().take(60).collect();
                match &result.tweet.author_handle {
                    Some(h) => format!("@{h}: {head}"),
                    None => head,
                }
            });
            // `extractor:` records which ladder rung answered
            // — fragility honesty for later debugging.
            let prov =
                wiki_archive::Provenance::new(title, target, canonical, content_type, result.rung);
            Ok((prov, body))
        }
        wiki_archive::Route::YouTube { .. } | wiki_archive::Route::Video => {
            let yt = wiki_archive::youtube::YtDlp::new(yt_dlp);
            let meta = yt.probe(target).await.map_err(|e| eyre::eyre!("{e}"))?;
            let blocks = match &meta.json3_track {
                Some((lang, track_url)) => {
                    let client =
                        wiki_archive::article::http_client().map_err(|e| eyre::eyre!("{e}"))?;
                    let json3 =
                        wiki_archive::article::fetch_text(&client, track_url, "application/json")
                            .await
                            .map_err(|e| eyre::eyre!("subtitle track ({lang}): {e}"))?;
                    let cues = wiki_archive::youtube::parse_json3_cues(&json3)
                        .map_err(|e| eyre::eyre!("{e}"))?;
                    wiki_archive::youtube::coalesce_cues(
                        &cues,
                        wiki_archive::youtube::DEFAULT_BLOCK_SECS,
                    )
                }
                None => Vec::new(),
            };
            let body = wiki_archive::youtube::render_video_markdown(&meta, &blocks);
            let title = title_override.unwrap_or_else(|| meta.title.clone());
            let mut prov = wiki_archive::Provenance::new(
                title,
                target,
                canonical.clone(),
                content_type,
                "yt-dlp",
            );
            // `media:` = what the SourceViewer should embed.
            prov.media = Some(canonical);
            prov.duration_secs = meta.duration_secs;
            Ok((prov, body))
        }
    }
}

/// PDF bytes → per-page text with `^p<page>` anchors.
/// Engine order: pdfium (feature `pdf`, dynamic libpdfium
/// binding) then `pdftotext -layout`. Title precedence:
/// `--title` → PDF metadata title → URL filename stem.
async fn archive_pdf_from_bytes(
    target: &str,
    canonical: String,
    args: &WikiArchiveArgs,
    bytes: &[u8],
) -> eyre::Result<(wiki_archive::Provenance, String)> {
    let (text, engine) = wiki_archive::pdf::extract_pdf(bytes, &args.pdftotext)
        .await
        .map_err(|e| eyre::eyre!("{e}"))?;
    let title = args
        .title
        .clone()
        .or_else(|| text.title.clone())
        .unwrap_or_else(|| {
            // Last path segment without the extension.
            target
                .rsplit('/')
                .next()
                .unwrap_or(target)
                .split('?')
                .next()
                .unwrap_or(target)
                .trim_end_matches(".pdf")
                .trim_end_matches(".PDF")
                .to_string()
        });
    let page_count = text.pages.len();
    let body = wiki_archive::pdf::render_pdf_markdown(&text.pages);
    let body = format!("_{page_count} page(s)_\n\n{body}");
    let prov = wiki_archive::Provenance::new(title, target, canonical, "pdf", engine);
    Ok((prov, body))
}

/// Archive one podcast episode from its RSS feed: pick the
/// episode, resolve a transcript (feed `<podcast:transcript>`
/// tag fast path → Groq backfill → local whisper, governed by
/// `--transcribe`), render `^t<sec>`-anchored markdown.
async fn archive_podcast_from_feed(
    target: &str,
    canonical: String,
    args: &WikiArchiveArgs,
    client: &reqwest::Client,
    feed_url: &str,
    show_title: Option<&str>,
    episode_hint: Option<&str>,
) -> eyre::Result<(wiki_archive::Provenance, String)> {
    let xml = wiki_archive::article::fetch_text(client, feed_url, "application/rss+xml,*/*")
        .await
        .map_err(|e| eyre::eyre!("feed {feed_url}: {e}"))?;
    let feed = wiki_archive::feed::parse_feed(&xml).map_err(|e| eyre::eyre!("{e}"))?;
    let show_title = show_title.or(if feed.title.is_empty() {
        None
    } else {
        Some(&feed.title)
    });
    let item = wiki_archive::feed::pick_episode(&feed, episode_hint).ok_or_else(|| {
        let sample: Vec<&str> = feed
            .items
            .iter()
            .take(5)
            .map(|i| i.title.as_str())
            .collect();
        eyre::eyre!(
            "no episode matched `{}` — recent episodes: {}",
            episode_hint.unwrap_or("<latest>"),
            sample.join(" | ")
        )
    })?;

    let (cues, extractor, note) = podcast_transcript_cues(args, client, item).await?;
    let blocks =
        wiki_archive::youtube::coalesce_cues(&cues, wiki_archive::youtube::DEFAULT_BLOCK_SECS);
    let body =
        wiki_archive::podcast::render_podcast_markdown(show_title, item, &blocks, note.as_deref());
    let title = args.title.clone().unwrap_or_else(|| item.title.clone());
    let mut prov = wiki_archive::Provenance::new(title, target, canonical, "podcast", extractor);
    // `media:` = the playable enclosure — the SourceViewer's
    // audio player + seek-on-anchor reads this.
    prov.media = item
        .enclosure_url
        .clone()
        .or_else(|| Some(target.to_string()));
    prov.duration_secs = item.duration_secs;
    Ok((prov, body))
}

/// Transcript ladder for one episode, honest at every rung.
/// Returns `(cues, extractor-label, note-when-empty)`.
async fn podcast_transcript_cues(
    args: &WikiArchiveArgs,
    client: &reqwest::Client,
    item: &wiki_archive::feed::FeedItem,
) -> eyre::Result<(Vec<wiki_archive::youtube::Cue>, String, Option<String>)> {
    let mode = args.transcribe.as_str();
    if !matches!(mode, "auto" | "tag" | "groq" | "whisper" | "none") {
        return Err(eyre::eyre!(
            "--transcribe must be auto|tag|groq|whisper|none (got `{mode}`)"
        ));
    }
    if mode == "none" {
        return Ok((
            Vec::new(),
            "podcast-feed".into(),
            Some("Transcript skipped (--transcribe none).".into()),
        ));
    }

    // ── Fast path: the feed's own transcript tag ────────
    if matches!(mode, "auto" | "tag") {
        for tref in wiki_archive::transcript::pick_transcripts(&item.transcripts) {
            let accept = if tref.mime.is_empty() {
                "*/*"
            } else {
                &tref.mime
            };
            match wiki_archive::article::fetch_text(client, &tref.url, accept).await {
                Ok(content) => {
                    match wiki_archive::transcript::parse_transcript(&content, &tref.mime) {
                        Ok(cues) => {
                            return Ok((cues, "podcast-transcript-tag".into(), None));
                        }
                        Err(e) => println!("note: transcript {} unusable: {e}", tref.url),
                    }
                }
                Err(e) => println!("note: transcript fetch {} failed: {e}", tref.url),
            }
        }
        if mode == "tag" {
            return Ok((
                Vec::new(),
                "podcast-feed".into(),
                Some(
                    "No usable transcript tag in the feed (--transcribe tag stops here; \
                     try groq or whisper)."
                        .into(),
                ),
            ));
        }
    }

    // ── Backfills need the audio itself ─────────────────
    let fetch_audio = || async {
        let url = item
            .enclosure_url
            .as_deref()
            .ok_or_else(|| eyre::eyre!("episode has no enclosure URL — nothing to transcribe"))?;
        let enc = wiki_archive::podcast::enclosure_client().map_err(|e| eyre::eyre!("{e}"))?;
        // Stacked redirect trackers are normal here; the
        // enclosure client follows a deeper chain.
        let (_ct, bytes) = wiki_archive::article::fetch_bytes(&enc, url, "audio/*,*/*")
            .await
            .map_err(|e| eyre::eyre!("enclosure {url}: {e}"))?;
        eyre::Ok(bytes)
    };

    if mode == "groq" {
        let key = std::env::var("GROQ_API_KEY")
            .ok()
            .filter(|s| !s.is_empty())
            .ok_or_else(|| eyre::eyre!("--transcribe groq needs GROQ_API_KEY"))?;
        let audio = fetch_audio().await?;
        println!(
            "transcribing via Groq ({} MB upload)…",
            audio.len() / 1_048_576
        );
        let cues = wiki_archive::podcast::groq_transcribe(client, &key, audio, "episode.mp3")
            .await
            .map_err(|e| eyre::eyre!("{e}"))?;
        return Ok((cues, "groq-whisper-large-v3-turbo".into(), None));
    }

    // mode is now `whisper` or `auto`-falling-through.
    #[cfg(feature = "whisper")]
    {
        let audio = fetch_audio().await?;
        println!(
            "transcribing locally (whisper, model `{}`)…",
            args.whisper_model
        );
        let cues = wiki_archive::whisper::transcribe_enclosure(
            client,
            &args.whisper_model,
            &args.ffmpeg,
            audio,
        )
        .await
        .map_err(|e| eyre::eyre!("{e}"))?;
        let label = format!("whisper-rs-{}", args.whisper_model);
        Ok((cues, label, None))
    }
    #[cfg(not(feature = "whisper"))]
    {
        let _ = &args.whisper_model;
        let _ = &args.ffmpeg;
        let _ = fetch_audio; // audio only fetched when a backfill can use it
        if mode == "whisper" {
            return Err(eyre::eyre!(
                "this build has no local whisper — rebuild the CLI with `--features whisper`, \
                 or use --transcribe groq (GROQ_API_KEY)"
            ));
        }
        Ok((
            Vec::new(),
            "podcast-feed".to_string(),
            Some(
                "No transcript tag in the feed. Re-run with --transcribe groq \
                 (GROQ_API_KEY, ~$0.04/audio-hour) or a `--features whisper` build \
                 for local transcription."
                    .to_string(),
            ),
        ))
    }
}

/// `task wiki archive import <kind>` — run one importer and
/// feed every item through the same provenance + dedup +
/// import + enqueue path as a single-URL archive.
async fn run_wiki_archive_import(cmd: WikiArchiveImportCmd) -> eyre::Result<()> {
    use wiki_archive::import as imp;

    let client = wiki_archive::article::http_client().map_err(|e| eyre::eyre!("{e}"))?;
    let started_at = chrono::Utc::now();
    let (items, common, next_cursor_hint): (Vec<imp::ImportedItem>, _, Option<String>) = match cmd {
        WikiArchiveImportCmd::Readwise {
            token,
            updated_after,
            common,
        } => {
            let items = imp::readwise::fetch_export(&client, &token, updated_after.as_deref())
                .await
                .map_err(|e| eyre::eyre!("readwise: {e}"))?;
            (items, common, Some(started_at.to_rfc3339()))
        }
        WikiArchiveImportCmd::Reader {
            token,
            updated_after,
            common,
        } => {
            let items = imp::readwise::fetch_reader(&client, &token, updated_after.as_deref())
                .await
                .map_err(|e| eyre::eyre!("readwise reader: {e}"))?;
            (items, common, Some(started_at.to_rfc3339()))
        }
        WikiArchiveImportCmd::Karakeep {
            endpoint,
            token,
            common,
        } => {
            let (items, skipped) = imp::karakeep::fetch_bookmarks(&client, &endpoint, &token)
                .await
                .map_err(|e| eyre::eyre!("karakeep: {e}"))?;
            if skipped > 0 {
                println!("note: skipped {skipped} non-link bookmark(s) (text/asset)");
            }
            (items, common, None)
        }
        WikiArchiveImportCmd::Pocket { zip, common } => {
            let items = imp::pocket::import_zip(&zip).map_err(|e| eyre::eyre!("pocket: {e}"))?;
            (items, common, None)
        }
        WikiArchiveImportCmd::Bookmarks { html, common } => {
            let body = std::fs::read_to_string(&html)?;
            let items =
                imp::netscape::parse_bookmarks_html(&body).map_err(|e| eyre::eyre!("{e}"))?;
            (items, common, None)
        }
    };

    println!("{} item(s) from the importer", items.len());
    if common.dry_run {
        for item in items.iter().take(25) {
            println!("  [{}] {}  {}", item.origin, item.title, item.url);
        }
        if items.len() > 25 {
            println!("  … {} more", items.len() - 25);
        }
        println!("dry run — nothing written");
        return Ok(());
    }

    archive_imported_items(items, &common).await?;
    if let Some(cursor) = next_cursor_hint {
        println!("incremental: next run pass --updated-after {cursor}");
    }
    Ok(())
}

/// Shared importer back-half: dedup against the wiki's
/// existing sources (canonical-URL filename scan, plus
/// in-batch), import over RPC, enqueue ingest.
async fn archive_imported_items(
    items: Vec<wiki_archive::import::ImportedItem>,
    common: &WikiArchiveImportCommon,
) -> eyre::Result<()> {
    use wiki_proto::raw::ImportRawSource;
    use wiki_proto::service::ingest::IngestClient;
    use wiki_proto::service::raw_layer::RawLayerClient;

    let slug = resolve_active_org(common.org.clone())?;
    let vox_url = resolve_org_vox_url(common.server.clone(), &slug);
    let raw: RawLayerClient = establish_for_url(&vox_url).await?;
    let ing: IngestClient = establish_for_url(&vox_url).await?;

    let mut existing: Vec<String> = raw
        .list_raw_sources(common.wiki_id.clone())
        .await
        .map_err(|e| eyre::eyre!("list_raw_sources: {e:?}"))?
        .into_iter()
        .map(|r| r.filename)
        .collect();

    let limit = common.limit.unwrap_or(usize::MAX);
    let (mut imported, mut deduped, mut skipped) = (0usize, 0usize, 0usize);
    for item in &items {
        if imported >= limit {
            break;
        }
        let (prov, body) = match wiki_archive::import::item_to_source(item) {
            Ok(v) => v,
            Err(e) => {
                println!("  skip {}: {e}", item.url);
                skipped += 1;
                continue;
            }
        };
        let canon8 = prov.canon8();
        if wiki_archive::find_canonical_match(existing.iter().map(String::as_str), &canon8)
            .is_some()
        {
            deduped += 1;
            continue;
        }
        let md = wiki_archive::compose_source_markdown(&prov, &body);
        let filename = prov.filename();
        let r = raw
            .import_raw_source(
                common.wiki_id.clone(),
                ImportRawSource {
                    filename: filename.clone(),
                    mime: "text/markdown".to_string(),
                    title: prov.title.clone(),
                    bytes: md.into_bytes(),
                    auto_enqueue: false,
                },
            )
            .await
            .map_err(|e| eyre::eyre!("import_raw_source {}: {e:?}", item.url))?;
        existing.push(filename);
        if !common.no_enqueue {
            ing.enqueue_ingest(
                common.wiki_id.clone(),
                r.path.clone(),
                wiki_proto::ingest::SourceChange::Created,
            )
            .await
            .map_err(|e| eyre::eyre!("enqueue_ingest {}: {e:?}", r.path))?;
        }
        println!("  + {}", r.path);
        imported += 1;
    }
    println!(
        "imported {imported}, deduped {deduped}, skipped {skipped} (of {})",
        items.len()
    );
    if common.no_enqueue && imported > 0 {
        println!("ingest not enqueued (--no-enqueue) — `task wiki raw rescan` enqueues later");
    }
    Ok(())
}

/// Extension → MIME for local-file archives. Mirrors
/// `wiki_extract::extract_path`'s table.
fn archive_mime_for_filename(name: &str) -> String {
    let ext = name
        .rsplit_once('.')
        .map(|(_, e)| e.to_ascii_lowercase())
        .unwrap_or_default();
    match ext.as_str() {
        "md" | "markdown" => "text/markdown",
        "txt" => "text/plain",
        "html" | "htm" => "text/html",
        "pdf" => "application/pdf",
        "png" => "image/png",
        "jpg" | "jpeg" => "image/jpeg",
        "gif" => "image/gif",
        "webp" => "image/webp",
        "docx" => "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
        "pptx" => "application/vnd.openxmlformats-officedocument.presentationml.presentation",
        "zip" => "application/zip",
        "json" => "application/json",
        "csv" => "text/csv",
        _ => "application/octet-stream",
    }
    .to_string()
}

async fn run_wiki_ingest(cmd: WikiIngestCmd) -> eyre::Result<()> {
    use wiki_proto::service::ingest::IngestClient;
    async fn connect(url: &str) -> eyre::Result<IngestClient> {
        establish_for_url(url).await
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
        establish_for_url(url).await
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
        establish_for_url(url).await
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
        establish_for_url(url).await
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
        establish_for_url(url).await
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
            relevant,
            at,
            location,
            device,
            limit,
            offset,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&url).await?;
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

            // Push --status/--project/--limit/--offset to the
            // server (`TaskService::query`) so big orgs don't
            // ship the whole list over the wire. A server that
            // predates the verb (schema skew — see `task
            // doctor`) falls back to the unfiltered `list()` +
            // the client-side filters below. Skip the server
            // path when a page window combines with
            // client-only filters: slicing before --tag /
            // --context / --milestone / --open would drop rows.
            let has_client_only_filters = tag.is_some()
                || ctx_filter.is_some()
                || milestone_filter.is_some()
                || open
                || relevant;
            let want_server_query =
                (status.is_some() || project_id.is_some() || limit.is_some() || offset.is_some())
                    && !((limit.is_some() || offset.is_some()) && has_client_only_filters);
            let mut window_applied = false;
            let rows = if want_server_query {
                let filter = task::TaskListFilter {
                    project: project_id,
                    workstream: None,
                    status: status.clone(),
                    limit,
                    offset,
                    ..Default::default()
                };
                match client.query(filter).await {
                    Ok(rows) => {
                        window_applied = true;
                        rows
                    }
                    Err(e) => {
                        eprintln!(
                            "warning: server-side query failed ({e:?}); falling back to full \
                             list() + client-side filters (is task-server stale? run `task \
                             doctor`)"
                        );
                        client
                            .list()
                            .await
                            .map_err(|e| eyre::eyre!("list: {e:?}"))?
                    }
                }
            } else {
                client
                    .list()
                    .await
                    .map_err(|e| eyre::eyre!("list: {e:?}"))?
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
            // Same business logic the web store applies — one
            // relevance implementation (task::relevance), two
            // renderers. FUTURE: read the running timer session
            // for the active-project boost.
            let relevance_ctx = relevant.then(|| {
                let now = chrono::Local::now();
                task::RelevanceContext {
                    local_hhmm: Some(at.unwrap_or_else(|| now.format("%H:%M").to_string())),
                    local_date: Some(now.format("%Y-%m-%d").to_string()),
                    location,
                    device,
                    active_project: None,
                }
            });
            if let Some(ctx) = &relevance_ctx {
                rows.retain(|t| task::status_is_open(&t.status) && task::is_relevant(t, ctx));
                // One next action per project — task-dumping into a
                // project can't inflate the "right now" list.
                task::condense_next_per_project(&mut rows);
            }
            rows.sort_by(|a, b| {
                let a_done = task::Status::from_str(&a.status).is_some_and(task::Status::is_done);
                let b_done = task::Status::from_str(&b.status).is_some_and(task::Status::is_done);
                a_done
                    .cmp(&b_done)
                    .then_with(|| a.due.is_none().cmp(&b.due.is_none()))
                    .then_with(|| a.due.cmp(&b.due))
                    .then_with(|| a.title.cmp(&b.title))
            });
            // Stable rank pass on top of the general order:
            // active-project / due-today rows lead.
            if let Some(ctx) = &relevance_ctx {
                rows.sort_by_key(|t| task::relevance_rank(t, ctx));
            }
            // Page window that couldn't go server-side (combined
            // with client-only filters, or the query fallback):
            // slice after filtering + sorting.
            if !window_applied && (limit.is_some() || offset.is_some()) {
                let off = offset.unwrap_or(0) as usize;
                rows = rows
                    .into_iter()
                    .skip(off)
                    .take(limit.map_or(usize::MAX, |n| n as usize))
                    .collect();
            }

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
            // Subtasks render indented under their parent when both
            // made the cut — same arrangement the web list uses.
            let arranged = task::arrange_families(
                rows,
                |t| t.id,
                |t| t.workflow.as_ref().and_then(|w| w.parent),
            );
            for (depth, t) in &arranged {
                let marker = match task::Status::from_str(&t.status) {
                    Some(s) if s.is_done() => "[x]",
                    Some(task::Status::InProgress) => "[~]",
                    _ => "[ ]",
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
                let indent = if *depth > 0 { "  ↳ " } else { "" };
                println!("{marker} {indent}{}{prio}{due}{ms}    {}", t.title, t.path);
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
        TaskCmd::Start {
            target,
            org,
            server,
            json,
        } => {
            mutate_task(target, org, server, json, |t| {
                t.status = "in-progress".into();
            })
            .await?;
        }
        TaskCmd::Done {
            target,
            undo,
            org,
            server,
            json,
        } => {
            mutate_task(target, org, server, json, |t| {
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
            json,
        } => mutate_task(target, org, server, json, |t| t.status = status).await?,
        TaskCmd::SetPriority {
            target,
            priority,
            org,
            server,
            json,
        } => mutate_task(target, org, server, json, |t| t.priority = priority).await?,
        TaskCmd::SetDue {
            target,
            due,
            org,
            server,
            json,
        } => {
            let v = if matches!(due.as_str(), "none" | "null" | "") {
                None
            } else {
                Some(due)
            };
            mutate_task(target, org, server, json, |t| t.due = v).await?;
        }
        TaskCmd::SetScheduled {
            target,
            scheduled,
            org,
            server,
            json,
        } => {
            let v = if matches!(scheduled.as_str(), "none" | "null" | "") {
                None
            } else {
                Some(scheduled)
            };
            mutate_task(target, org, server, json, |t| t.scheduled = v).await?;
        }
        TaskCmd::SetProject {
            target,
            project,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org.clone())?;
            let url = resolve_org_vox_url(server.clone(), &slug);
            let new_proj = if matches!(project.as_str(), "none" | "null" | "") {
                None
            } else {
                let pc = connect_project_client(&url).await?;
                Some(resolve_project_target(&pc, &project).await?.id)
            };
            mutate_task(target, org, server, json, |t| t.project_id = new_proj).await?;
        }
        TaskCmd::SetMilestone {
            target,
            milestone,
            org,
            server,
            json,
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
            mutate_task(target, org, server, json, |t| {
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
            json,
        } => {
            mutate_task(target, org, server, json, |t| {
                t.tags = task::model::StringList(tags);
            })
            .await?;
        }
        TaskCmd::SetParent {
            target,
            parent,
            org,
            server,
            json,
        } => {
            let parent_id = match parent.as_str() {
                "none" | "null" => None,
                p => {
                    let slug = resolve_active_org(org.clone())?;
                    let url = resolve_org_vox_url(server.clone(), &slug);
                    let client = connect_task_client(&url).await?;
                    Some(json_out::resolve_task_flexible(&client, p).await?.id)
                }
            };
            mutate_task(target, org, server, json, |t| {
                let mut wf = t.workflow.clone().unwrap_or_default();
                wf.parent = parent_id;
                t.workflow = Some(wf);
            })
            .await?;
        }
        TaskCmd::SetContexts {
            target,
            contexts,
            org,
            server,
            json,
        } => {
            let contexts: Vec<String> = contexts
                .into_iter()
                .map(|c| {
                    if c.starts_with('@') {
                        c
                    } else {
                        format!("@{c}")
                    }
                })
                .collect();
            mutate_task(target, org, server, json, |t| {
                t.contexts = task::model::StringList(contexts);
            })
            .await?;
        }
        TaskCmd::Rename {
            target,
            new_path,
            org,
            server,
            json,
        } => {
            let slug = resolve_active_org(org)?;
            let url = resolve_org_vox_url(server, &slug);
            let client = connect_task_client(&url).await?;
            let t = resolve_task_target(&client, &target).await?;
            let renamed = client
                .rename(t.id, new_path)
                .await
                .map_err(|e| eyre::eyre!("rename: {e:?}"))?;
            if json {
                json_out::print_json(&renamed)?;
            } else {
                println!("renamed → {}", renamed.path);
            }
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
    establish_for_url(url).await
}

/// Resolve a task reference — uuid, vault path, title, or a unique
/// prefix of either (shared flexible resolver).
async fn resolve_task_target(
    client: &task::TaskServiceClient,
    target: &str,
) -> eyre::Result<task::TaskInfo> {
    json_out::resolve_task_flexible(client, target).await
}

async fn mutate_task<F>(
    target: String,
    org: Option<String>,
    server: Option<String>,
    json: bool,
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
    if json {
        json_out::print_json(&updated)?;
    } else {
        println!("{}  [{}]  {}", updated.title, updated.status, updated.path);
    }
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
