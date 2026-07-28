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
mod agent;
mod auth;
mod brief;
mod code;
mod collection;
mod cycle;
mod errors;
mod finance;
mod forge;
mod goal;
mod issue;
mod json_out;
mod label;
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
use crate::label::{LabelCmd, run_label};
use crate::code::{CodeCmd, run_code};
use crate::issue::{ClaimOutcome, IssueCmd, parse_agent_ref, resolve_issue_id, run_issue, try_claim};
use crate::agent::{AgentCmd, render_task_prompt, run_agent};
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

// ── git helpers for `task code` ──────────────────────────────

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
