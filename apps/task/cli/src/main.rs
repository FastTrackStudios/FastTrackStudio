//! `task` CLI — argument parsing, dispatch, and client plumbing.
//!
//! Every command lives in its own module (`project`, `wiki`,
//! `timer`, …); this file owns [`Cli`] / [`Commands`], the dispatch
//! match, and the vox client helpers the modules share. The module
//! for `task task …` is `task_cmd` — `mod task` at the crate root
//! would shadow the `task` crate itself.
//!
//! Commands reach their data one of two ways, and which one is not
//! yet a settled design. Most go through vox against
//! `/org/<slug>/vox`. A handful — `wiki`, `vault`, `agent`, `label`,
//! `code`, `cycle`, `setup` — open the org tree on disk directly,
//! and `timer` / `finance` / `auth` / `inbox` do both. The
//! direct-to-disk paths resolve through
//! `org_proto::DataRoot::from_env()` and therefore only work against
//! an org hosted on this machine; `auth` makes that explicit with a
//! guard that refuses to run against a remote session. Unifying this
//! needs a decision about whether the CLI is a client or a
//! co-resident tool.
//!
//! Server endpoint resolution (first match wins):
//! 1. `--server <url>` flag.
//! 2. `TASK_VOX_URL` env var (loaded from `.env` if present).
//! 3. `ws://127.0.0.1:9090/vox` default.

mod admin;
mod agent;
mod api;
mod auth;
mod body;
mod brief;
mod code;
mod collection;
mod cycle;
mod errors;
mod exercise;
mod finance;
mod forge;
mod goal;
mod inbox;
mod intake;
mod issue;
mod json_out;
mod label;
mod location;
mod meal;
mod mealprep;
mod media;
mod milestone;
mod mount;
mod org;
mod org_ctx;
mod pantry;
mod plan;
mod project;
mod recipe;
mod recipe_import;
mod session_store;
mod setup;
mod shared;
mod task_cmd;
mod threads;
mod timer;
mod vault;
mod wiki;
mod workout;
mod workstream;

use crate::admin::{AdminCmd, run_admin};
use crate::agent::{AgentCmd, run_agent};
use crate::api::{ApiArgs, run_api};
use crate::auth::{AuthCmd, run_auth, ws_base_to_http};
use crate::body::{BodyCmd, run_body};
use crate::code::{CodeCmd, run_code};
use crate::cycle::{CycleCmd, run_cycle};
use crate::exercise::{ExerciseCmd, run_exercise};
use crate::finance::{FinanceCmd, run_finance};
use crate::goal::{GoalCmd, run_goal};
use crate::inbox::{InboxCmd, run_inbox};
use crate::intake::{IntakeCmd, run_intake};
use crate::issue::{IssueCmd, run_issue};
use crate::label::{LabelCmd, run_label};
use crate::location::{LocationCmd, run_location};
use crate::meal::{MealCmd, run_meal};
use crate::milestone::{MilestoneCmd, run_milestone};
use crate::mount::{MountCmd, run_mount};
use crate::org::{OrgCmd, run_org};
use crate::pantry::{PantryCmd, run_pantry};
use crate::project::{ProjectCmd, run_project};
use crate::recipe::{RecipeCmd, run_recipe};
use crate::setup::{SetupCmd, run_setup};
use crate::task_cmd::{TaskCmd, run_task};
use crate::threads::{ThreadsCmd, run_threads};
use crate::timer::{TimerCmd, run_timer};
use crate::vault::{VaultCmd, run_vault, run_vault_sync};
use crate::wiki::{WikiCmd, run_wiki};
use crate::workout::{WorkoutCmd, run_workout};
use clap::{Parser, Subcommand};
use shared::RemoteVoxConfig;

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
    /// The API reference, rendered from the live registry
    /// (`task_server::permits::mounts()`): every mounted service,
    /// its methods + arg names, the permit action/resource per
    /// method, stream vs rpc, and the schema stamp. `task api
    /// <service>` for one service; `--markdown` regenerates
    /// `apps/task/docs/api-reference.md`; `--json` mirrors
    /// `GET /org/{slug}/api`.
    Api(ApiArgs),
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
    let origin = http_origin(ws_url);
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

/// ws(s)://host:port[/path] → http(s)://host:port — the HTTP origin of
/// the vox endpoint, where the server's plain-HTTP surfaces live.
fn http_origin(ws_url: &str) -> String {
    let http = ws_url
        .replacen("wss://", "https://", 1)
        .replacen("ws://", "http://", 1);
    let after_scheme = http.find("://").map_or(http.len(), |i| i + 3);
    let end = http[after_scheme..]
        .find('/')
        .map_or(http.len(), |i| after_scheme + i);
    http[..end].to_owned()
}

/// The API-surface half of `task doctor`: fetch `GET /org/{slug}/api`
/// (the registry the running server serializes — see `task api`),
/// report what is mounted (services vs `#[subscribe]` streams), and
/// warn per service whose schema stamp differs from this CLI's build —
/// the same stamp logic as the schema check, surfaced per-service
/// instead of pass/fail. Warn-only: `doctor_check_schema` already
/// gates hard on skew.
async fn doctor_check_api(ws_url: &str) -> eyre::Result<()> {
    let origin = http_origin(ws_url);

    // Which org? Ask the server itself — the well-known document lists
    // every hosted slug; prefer the home org.
    let wk_url = format!("{origin}/.well-known/task-server.json");
    let slug = match reqwest::get(&wk_url).await {
        Ok(resp) => resp
            .json::<serde_json::Value>()
            .await
            .ok()
            .and_then(|doc| {
                let orgs = doc.get("orgs")?.as_array()?.clone();
                let pick = orgs
                    .iter()
                    .find(|o| o.get("is_home").and_then(|v| v.as_bool()).unwrap_or(false))
                    .or_else(|| orgs.first())?;
                Some(pick.get("slug")?.as_str()?.to_owned())
            }),
        Err(e) => {
            println!("API check: SKIPPED — could not fetch {wk_url} ({e})");
            return Ok(());
        }
    };
    let Some(slug) = slug else {
        println!("API check: SKIPPED — the server hosts no orgs (nothing to describe)");
        return Ok(());
    };

    let api_url = format!("{origin}/org/{slug}/api");
    let doc: serde_json::Value = match reqwest::get(&api_url).await {
        Ok(resp) if resp.status().is_success() => resp
            .json()
            .await
            .map_err(|e| eyre::eyre!("parse {api_url}: {e}"))?,
        Ok(resp) => {
            println!(
                "API check: UNVERIFIED — {api_url} returned {} (the server \
                 predates the API reference endpoint)",
                resp.status()
            );
            return Ok(());
        }
        Err(e) => {
            println!("API check: SKIPPED — could not fetch {api_url} ({e})");
            return Ok(());
        }
    };

    let services = doc
        .get("services")
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();
    let streams = services
        .iter()
        .filter(|s| s.get("stream").and_then(|v| v.as_bool()).unwrap_or(false))
        .count();
    println!(
        "API check: org `{slug}` mounts {} service(s) — {} rpc, {} stream(s) \
         ({api_url})",
        services.len(),
        services.len() - streams,
        streams,
    );

    // Per-service stamp diff against this build's registry.
    let local = task_server::schema_stamps();
    let mut stale: Vec<String> = Vec::new();
    let mut unserved: Vec<&str> = Vec::new();
    for (name, stamp) in &local {
        let served = services.iter().find_map(|s| {
            (s.get("name")?.as_str()? == *name).then(|| s.get("stamp"))?
        });
        match served.and_then(|v| v.as_str()) {
            Some(s) if s == stamp => {}
            Some(s) => stale.push(format!("{name} (server {s}, this build {stamp})")),
            None => unserved.push(name),
        }
    }
    if stale.is_empty() && unserved.is_empty() {
        println!("  all {} service stamps match this build", local.len());
    }
    if !unserved.is_empty() {
        println!(
            "  WARNING: {} service(s) in this build are not mounted by the \
             server: {}",
            unserved.len(),
            unserved.join(", ")
        );
    }
    for line in &stale {
        println!("  WARNING: stamp skew on {line}");
    }
    if !stale.is_empty() {
        println!(
            "  The running task-server was built against different `*-proto` \
             shapes than this CLI for the service(s) above — rebuild whichever \
             is older."
        );
    }
    Ok(())
}

/// The authorization half of `task doctor`: how much of the org lane the
/// permission gate actually covers, and what would break if enforcement
/// were switched on.
///
/// Static and offline — it folds the very same `permits` tables the server
/// installs (the CLI links `task_server`), so it answers for the build in
/// front of you without needing a running server or a token. For the
/// *runtime* half — what real clients have actually been refused — ask a
/// running server: `GET /server/permissions` with the
/// `TASK_BACKUP_GIT_TOKEN` bearer.
fn doctor_check_permissions() {
    println!("{}", task_server::permits::coverage_summary());
    if task_server::enforce_permissions() {
        println!("  TASK_ENFORCE_PERMISSIONS=1 in this environment — the gate ENFORCES.");
    } else {
        println!(
            "  Enforcement is OFF here (TASK_ENFORCE_PERMISSIONS != 1); the gate audits only."
        );
    }
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
            doctor_check_permissions();
            doctor_check_api(&server).await?;
        }
        Commands::Api(args) => {
            return run_api(args);
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
