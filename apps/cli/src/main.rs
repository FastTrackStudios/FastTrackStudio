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
}

#[derive(Subcommand)]
enum OrgCmd {
    /// Scaffold a new org under `<data-root>/orgs/<slug>/`.
    /// Writes `org.toml` + creates `vault/` and `attachments/`
    /// subdirs. Idempotency: refuses to overwrite an existing
    /// org dir (federation-breaking change a human should
    /// confirm). Use `--home` to mark this as your identity
    /// anchor (only one home per data root in practice).
    Init {
        /// `[a-z0-9-]`, 1–64 chars, no leading/trailing `-`.
        slug: String,
        /// Human-facing display name. Free-form UTF-8.
        #[arg(long)]
        name: String,
        /// Mark this org as the identity anchor (home).
        #[arg(long)]
        home: bool,
    },
    /// List every org dir under `<data-root>/orgs/` that has
    /// a loadable `org.toml`. Skips partial scaffolds.
    List,
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
    /// Create a new task from a natural-language line. Extracts
    /// `#tag`s, `@context`s, `[[Project]]`s, `!priority`, and
    /// date keywords (`today`, `tomorrow`, `next monday`, `mon`,
    /// `YYYY-MM-DD`). Title = the remaining text.
    ///
    /// Examples:
    ///   task task capture "Buy milk tomorrow #errands @shopping"
    ///   task task capture "Ship vault-graph !high next friday"
    Capture {
        /// The task line. Quote the whole thing.
        text: String,
        /// Vault root. Defaults to `examples/vault`.
        #[arg(long, default_value = "examples/vault")]
        vault: std::path::PathBuf,
        /// Override folder. Default: `tasks/`.
        #[arg(long)]
        folder: Option<String>,
    },
    /// List tasks in the vault. Filters compose (AND).
    List {
        #[arg(long, default_value = "examples/vault")]
        vault: std::path::PathBuf,
        /// Restrict to one status (e.g. `open`, `done`).
        #[arg(long)]
        status: Option<String>,
        /// Restrict to tasks with this tag (without `#`).
        #[arg(long)]
        tag: Option<String>,
        /// Restrict to tasks with this context (with or without `@`).
        #[arg(long)]
        context: Option<String>,
    },
    /// Mark a task done. Sets `status: done` and `completedDate`
    /// to today. `task_id` matches a unique basename prefix.
    Done {
        /// Task identifier — basename, prefix, or full
        /// `tasks/foo.md` path.
        task_id: String,
        #[arg(long, default_value = "examples/vault")]
        vault: std::path::PathBuf,
        /// Re-open the task (clear `completedDate`, set status
        /// to `open`).
        #[arg(long)]
        undo: bool,
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
        Commands::Vault { cmd } => {
            return run_vault(cmd);
        }
        Commands::Task(cmd) => {
            return run_task(cmd);
        }
        Commands::Agent(cmd) => {
            return run_agent(cmd).await;
        }
        Commands::Wiki(cmd) => {
            return run_wiki(cmd).await;
        }
        Commands::Timer(cmd) => {
            return run_timer(cmd).await;
        }
        Commands::Finance(cmd) => {
            return run_finance(cmd).await;
        }
        Commands::Auth(cmd) => {
            return run_auth(cmd).await;
        }
        Commands::Org(cmd) => {
            return run_org(cmd);
        }
    }
    Ok(())
}

fn run_org(cmd: OrgCmd) -> eyre::Result<()> {
    let root =
        org_proto::DataRoot::from_env().map_err(|e| eyre::eyre!("resolve data root: {e}"))?;
    root.ensure()
        .map_err(|e| eyre::eyre!("ensure data root: {e}"))?;
    match cmd {
        OrgCmd::Init { slug, name, home } => {
            let org = root
                .init_org(&slug, &name, home)
                .map_err(|e| eyre::eyre!("init org: {e}"))?;
            let manifest = org
                .manifest()
                .map_err(|e| eyre::eyre!("load fresh manifest: {e}"))?;
            println!("Initialized org `{}` at {}", slug, org.path().display());
            println!("  id:         {}", manifest.id);
            println!("  name:       {}", manifest.display_name);
            println!("  is_home:    {}", manifest.is_home);
            println!("  vault:      {}", org.vault_dir().display());
            println!("  auth.db:    {}", org.auth_db().display());
            println!("  timer.db:   {}", org.timer_db().display());
            println!("  finance.db: {}", org.finance_db().display());
        }
        OrgCmd::List => {
            let orgs = root
                .scan_orgs()
                .map_err(|e| eyre::eyre!("scan orgs: {e}"))?;
            if orgs.is_empty() {
                println!("(no orgs under {})", root.orgs_dir().display());
                return Ok(());
            }
            for (org, m) in orgs {
                let badge = if m.is_home { " [home]" } else { "" };
                println!("{}{}  {}  ({})", org.slug(), badge, m.display_name, m.id);
                if !m.federation_url.is_empty() {
                    println!("    federation: {}", m.federation_url);
                }
            }
        }
    }
    Ok(())
}

/// Open `ArchitectAuth` against the local `auth.sqlite` —
/// same DB the server uses. CLI ↔ server interop hinges on
/// matching `default_auth_db_path()` + `DEFAULT_AUTH_SECRET`.
async fn open_local_auth()
-> eyre::Result<architect_auth::ArchitectAuth<architect_auth::db::AuthSeaOrmStorage>> {
    use architect_auth::db::{AuthSeaOrmStorage, Migrator as AuthMigrator};
    use sea_orm::Database;
    use sea_orm_migration::MigratorTrait;
    let path = session_store::default_auth_db_path()?;
    let db_url = format!("sqlite://{}?mode=rwc", path.display());
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

async fn run_auth(cmd: AuthCmd) -> eyre::Result<()> {
    use architect_auth::commands::{CurrentSession, SignOut};
    use architect_auth::proto::SignInEmailPassword;
    match cmd {
        AuthCmd::Login { email, password } => {
            let auth = open_local_auth().await?;
            let bundle = auth
                .sign_in_email_password(SignInEmailPassword {
                    email: email.clone(),
                    password,
                    ip_address: None,
                    user_agent: Some("task-cli".into()),
                })
                .await
                .map_err(|e| eyre::eyre!("sign in: {e}"))?;
            let sess = session_store::CliSession {
                token: bundle.token.clone(),
                user_id: bundle.user.id,
                email: bundle.user.email.clone().unwrap_or_else(|| email.clone()),
                org_id: bundle.session.active_organization_id,
            };
            session_store::save(&sess)?;
            println!("Signed in as {} ({})", sess.email, sess.user_id);
            match sess.org_id {
                Some(org) => println!("Active org: {org}"),
                None => println!("No active org — pick one with `task auth org use <id>`."),
            }
        }
        AuthCmd::Whoami => match session_store::load()? {
            Some(s) => {
                println!("email:   {}", s.email);
                println!("user_id: {}", s.user_id);
                match s.org_id {
                    Some(org) => println!("org_id:  {org}"),
                    None => println!("org_id:  (none — `task auth org use <id>`)"),
                }
                println!(
                    "token:   <stored in {}>",
                    session_store::session_path()?.display()
                );
            }
            None => {
                println!("Not signed in. Run `task auth login --email … --password …`.");
            }
        },
        AuthCmd::Logout => {
            if let Some(s) = session_store::load()? {
                let auth = open_local_auth().await?;
                if let Err(e) = auth.sign_out(SignOut { token: s.token }).await {
                    eprintln!("warning: server-side sign out failed: {e}");
                }
            }
            session_store::clear()?;
            println!("Signed out.");
        }
        AuthCmd::Org(AuthOrgCmd::List) => {
            let Some(sess) = session_store::load()? else {
                return Err(eyre::eyre!("not signed in — run `task auth login` first"));
            };
            let auth = open_local_auth().await?;
            // Verify session still valid + refresh `user_id`.
            let bundle = auth
                .current_session(CurrentSession { token: sess.token })
                .await
                .map_err(|e| eyre::eyre!("session: {e}"))?;
            let memberships = list_user_memberships(bundle.user.id).await?;
            if memberships.is_empty() {
                println!("(no org memberships)");
            }
            for (member, org) in memberships {
                let marker = if Some(member.organization_id) == sess.org_id {
                    " *"
                } else {
                    "  "
                };
                println!(
                    "{marker} {}  {}  ({})",
                    member.organization_id, org.name, member.role
                );
            }
        }
        AuthCmd::Org(AuthOrgCmd::Use { org_id }) => {
            let Some(mut sess) = session_store::load()? else {
                return Err(eyre::eyre!("not signed in — run `task auth login` first"));
            };
            // Membership check.
            let memberships = list_user_memberships(sess.user_id).await?;
            if !memberships.iter().any(|(m, _)| m.organization_id == org_id) {
                return Err(eyre::eyre!("user is not a member of org {org_id}"));
            }
            update_session_active_org(&sess.token, Some(org_id)).await?;
            sess.org_id = Some(org_id);
            session_store::save(&sess)?;
            println!("Active org set to {org_id}");
        }
    }
    Ok(())
}

async fn open_auth_db() -> eyre::Result<sea_orm::DatabaseConnection> {
    use sea_orm::Database;
    use sea_orm_migration::MigratorTrait;
    let path = session_store::default_auth_db_path()?;
    let db = Database::connect(format!("sqlite://{}?mode=rwc", path.display()))
        .await
        .map_err(|e| eyre::eyre!("connect auth db: {e}"))?;
    architect_auth::db::Migrator::up(&db, None)
        .await
        .map_err(|e| eyre::eyre!("auth migrations: {e}"))?;
    Ok(db)
}

async fn list_user_memberships(
    user_id: uuid::Uuid,
) -> eyre::Result<
    Vec<(
        architect_auth::db::AuthMemberModel,
        architect_auth::db::AuthOrganizationModel,
    )>,
> {
    use architect_auth::db::{AuthMemberColumn, AuthMemberEntity, AuthOrganizationEntity};
    use sea_orm::{ColumnTrait, EntityTrait, QueryFilter};
    let db = open_auth_db().await?;
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

async fn update_session_active_org(token: &str, org_id: Option<uuid::Uuid>) -> eyre::Result<()> {
    use architect_auth::db::{AuthSessionActiveModel, AuthSessionColumn, AuthSessionEntity};
    use sea_orm::{ActiveModelTrait, ColumnTrait, EntityTrait, IntoActiveModel, QueryFilter, Set};
    let token_hash = hash_session_token(session_store::DEFAULT_AUTH_SECRET, token);
    let db = open_auth_db().await?;
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

async fn run_finance(cmd: FinanceCmd) -> eyre::Result<()> {
    use sea_orm::Database;
    use sea_orm_migration::MigratorTrait;

    let db_url = std::env::var("TASK_TIMER_DB").unwrap_or_else(|_| {
        let base = std::env::var("XDG_DATA_HOME")
            .ok()
            .filter(|s| !s.is_empty())
            .map(std::path::PathBuf::from)
            .or_else(|| {
                std::env::var("HOME")
                    .ok()
                    .map(std::path::PathBuf::from)
                    .map(|p| p.join(".local/share"))
            })
            .unwrap_or_else(|| std::path::PathBuf::from("."));
        let dir = base.join("task");
        let _ = std::fs::create_dir_all(&dir);
        format!("sqlite://{}?mode=rwc", dir.join("timer.sqlite").display())
    });
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

async fn run_timer(cmd: TimerCmd) -> eyre::Result<()> {
    use sea_orm::Database;
    use sea_orm::{ColumnTrait, EntityTrait, QueryFilter};
    use sea_orm_migration::MigratorTrait;
    use std::sync::Arc;
    use timer::entity::{TagColumn, TagEntity, WorkSessionTagColumn, WorkSessionTagEntity};
    use timer::store::{Store, VaultProjectDefaults};
    use timer_proto::service::{LogSessionRequest, StartTimerRequest, TimerService};

    // Layout — single-user CLI mode:
    // - DB at $XDG_DATA_HOME/task/timer.sqlite (override via
    //   `TASK_TIMER_DB`).
    // - Vault root at `TASK_VAULT_ROOT` (defaults to
    //   `./examples/vault` so the rate-cascade lookup
    //   works against the demo vault out of the box).
    // - User + org ids from `TASK_USER_ID` / `TASK_ORG_ID`
    //   env, falling back to nil-uuid so a fresh setup
    //   "just works" before auth is wired in.
    let db_url = std::env::var("TASK_TIMER_DB").unwrap_or_else(|_| {
        let base = std::env::var("XDG_DATA_HOME")
            .ok()
            .filter(|s| !s.is_empty())
            .map(std::path::PathBuf::from)
            .or_else(|| {
                std::env::var("HOME")
                    .ok()
                    .map(std::path::PathBuf::from)
                    .map(|p| p.join(".local/share"))
            })
            .unwrap_or_else(|| std::path::PathBuf::from("."));
        let dir = base.join("task");
        let _ = std::fs::create_dir_all(&dir);
        format!("sqlite://{}?mode=rwc", dir.join("timer.sqlite").display())
    });
    let vault_root = std::env::var("TASK_VAULT_ROOT").map_or_else(
        |_| std::path::PathBuf::from("examples/vault"),
        std::path::PathBuf::from,
    );
    // ID resolution order (first match wins):
    //   1. `task auth login`-issued session.json
    //   2. `TASK_USER_ID` / `TASK_ORG_ID` env vars
    //   3. fixed dev nil-uuids — only useful for fresh setups
    //      before architect-auth is wired
    let stored_session = session_store::load().ok().flatten();
    let session_user_id = stored_session.as_ref().map(|s| s.user_id);
    let session_org_id = stored_session.as_ref().and_then(|s| s.org_id);
    let user_id = session_user_id
        .or_else(|| {
            std::env::var("TASK_USER_ID")
                .ok()
                .and_then(|s| s.parse::<uuid::Uuid>().ok())
        })
        .unwrap_or_else(|| uuid::Uuid::parse_str("00000000-0000-0000-0000-000000000001").unwrap());
    let org_id = session_org_id
        .or_else(|| {
            std::env::var("TASK_ORG_ID")
                .ok()
                .and_then(|s| s.parse::<uuid::Uuid>().ok())
        })
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

fn run_task(cmd: TaskCmd) -> eyre::Result<()> {
    match cmd {
        TaskCmd::Capture {
            text,
            vault,
            folder,
        } => {
            let mut info = task::capture(&text);
            info.path = task::write::default_task_path(&info.title, folder.as_deref());
            let abs = task::write_task(&vault, &mut info, false)
                .map_err(|e| eyre::eyre!("write task: {e}"))?;
            println!("Created {}", abs.display());
            println!("  title:    {}", info.title);
            println!("  status:   {}", info.status);
            println!("  priority: {}", info.priority);
            if let Some(d) = &info.due {
                println!("  due:      {d}");
            }
            if !info.tags.is_empty() {
                println!("  tags:     {}", info.tags.join(", "));
            }
            if !info.contexts.is_empty() {
                println!("  contexts: {}", info.contexts.join(", "));
            }
            if !info.projects.is_empty() {
                println!("  projects: {}", info.projects.join(", "));
            }
        }
        TaskCmd::List {
            vault,
            status,
            tag,
            context,
        } => {
            let v = vault::Vault::open(&vault).map_err(|e| eyre::eyre!("open: {e}"))?;
            let ctx_filter = context.map(|c| {
                if c.starts_with('@') {
                    c
                } else {
                    format!("@{c}")
                }
            });
            let mut tasks: Vec<_> = task::scan_vault(&v)
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
                .collect();
            tasks.sort_by(|a, b| {
                // Open before done; then by due date ascending
                // (None last); then by title.
                let a_done = task::Status::from_str(&a.status).is_some_and(task::Status::is_done);
                let b_done = task::Status::from_str(&b.status).is_some_and(task::Status::is_done);
                a_done
                    .cmp(&b_done)
                    .then_with(|| a.due.is_none().cmp(&b.due.is_none()))
                    .then_with(|| a.due.cmp(&b.due))
                    .then_with(|| a.title.cmp(&b.title))
            });
            if tasks.is_empty() {
                println!("(no tasks)");
                return Ok(());
            }
            for t in &tasks {
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
                println!("{marker} {}{prio}{due}    {}", t.title, t.path);
            }
        }
        TaskCmd::Done {
            task_id,
            vault,
            undo,
        } => {
            let v = vault::Vault::open(&vault).map_err(|e| eyre::eyre!("open: {e}"))?;
            let tasks = task::scan_vault(&v);
            let needle = task_id.trim_end_matches(".md").to_ascii_lowercase();
            let matches: Vec<_> = tasks
                .iter()
                .filter(|t| {
                    t.path.eq_ignore_ascii_case(&task_id)
                        || std::path::Path::new(&t.path)
                            .file_stem()
                            .and_then(|s| s.to_str())
                            .is_some_and(|s| s.to_ascii_lowercase().starts_with(&needle))
                })
                .collect();
            let matched = match matches.as_slice() {
                [] => return Err(eyre::eyre!("no task matched {task_id:?}")),
                [t] => *t,
                multi => {
                    return Err(eyre::eyre!(
                        "{} tasks matched {task_id:?} (be more specific)",
                        multi.len()
                    ));
                }
            };
            let mut info = matched.clone();
            if undo {
                info.status = "open".into();
                info.completed_date = None;
            } else {
                info.status = "done".into();
                info.completed_date = Some(chrono::Local::now().date_naive());
            }
            task::write_task(&vault, &mut info, true)
                .map_err(|e| eyre::eyre!("write task: {e}"))?;
            let verb = if undo { "Reopened" } else { "Done" };
            println!("{verb} {}    {}", info.title, info.path);
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
    }
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
