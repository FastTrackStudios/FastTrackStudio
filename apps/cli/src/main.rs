//! `task` CLI — vertical-slice scaffold.
//!
//! Talks to a `task-server` over vox WebSocket. `task list` calls the
//! auto-generated `TaskRepoClient::list`; `task doctor` just prints
//! the resolved endpoint URL.
//!
//! Endpoint resolution (first match wins):
//! 1. `--server <url>` flag.
//! 2. `TASK_VOX_URL` env var (loaded from `.env` if present in CWD
//!    or any parent dir — see `dotenvy::dotenv()`). Named to avoid
//!    collision with the broader `TASK_SERVER` you may already
//!    have set for the prod box.
//! 3. `ws://127.0.0.1:9090/vox` default.

mod shared;

use std::collections::HashMap;

use clap::{Parser, Subcommand};
use project_crdt::{ProjectRepoLoro, TaskRepoLoro};
use project_proto::architect::Page;
use project_proto::{ProjectCreate, ProjectRepo, TaskCreate, TaskRepo, TaskUpdate};
use shared::{LiveSession, RemoteVoxConfig};
use uuid::Uuid;

#[derive(Parser)]
#[command(name = "task", about = "Task management CLI", version)]
struct Cli {
    /// Vox WebSocket URL (e.g. ws://127.0.0.1:9090/vox). Falls back
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
    /// List tasks grouped by project.
    List,
    /// Mark a task done (or undone). Triggers a live push to every
    /// subscribed peer through `WorkspaceSync`.
    SetDone {
        /// Task UUID (any unambiguous prefix accepted).
        task_id: String,
        /// Mark the task open again instead of done.
        #[arg(long)]
        undo: bool,
    },
    /// Create a new task inside a project (matched by name prefix).
    NewTask {
        /// Project name (or unique prefix).
        project: String,
        /// Task title.
        title: String,
    },
    /// Create a new project.
    NewProject {
        /// Project name.
        name: String,
    },
    /// Probe the configured vox endpoint.
    Doctor,
}

#[tokio::main]
async fn main() -> eyre::Result<()> {
    // Best-effort .env load before clap reads env. Missing file is
    // not an error — we just fall through to the hard-coded default.
    let _ = dotenvy::dotenv();
    let cli = Cli::parse();
    match cli.command {
        Commands::List => {
            let remote =
                RemoteVoxConfig::from_args(cli.server, cli.session_token, cli.organization_id)?;
            let session = LiveSession::open(&remote).await?;
            let project_repo = ProjectRepoLoro::new(&session.doc);
            let task_repo = TaskRepoLoro::new(&session.doc);
            let big_page = Page {
                index: 0,
                size: 1000,
            };
            let project_page = project_repo
                .list(big_page.clone(), None, None)
                .await
                .map_err(|e| eyre::eyre!("project list: {e}"))?;
            let task_page = task_repo
                .list(big_page, None, None)
                .await
                .map_err(|e| eyre::eyre!("task list: {e}"))?;

            let mut grouped: HashMap<Uuid, Vec<_>> = HashMap::new();
            for task in task_page.items {
                grouped.entry(task.project_id).or_default().push(task);
            }

            if project_page.items.is_empty() {
                println!("(no projects)");
            } else {
                let mut projects = project_page.items;
                projects.sort_by(|a, b| a.name.cmp(&b.name));
                for project in projects {
                    let tasks = grouped.remove(&project.id).unwrap_or_default();
                    println!("\n## {}  ({} tasks)", project.name, tasks.len());
                    for task in tasks {
                        let short_id = &task.id.to_string()[..8];
                        let mark = if task.done { "[x]" } else { "[ ]" };
                        println!("  [{short_id}] {mark} {}", task.title);
                    }
                }
                // Orphans — tasks pointing at projects we don't have
                // in the snapshot (would be a data bug, but print
                // rather than silently drop).
                for (project_id, tasks) in grouped {
                    println!(
                        "\n## (unknown project {project_id})  ({} tasks)",
                        tasks.len()
                    );
                    for task in tasks {
                        let short_id = &task.id.to_string()[..8];
                        let mark = if task.done { "[x]" } else { "[ ]" };
                        println!("  [{short_id}] {mark} {}", task.title);
                    }
                }
            }
        }
        Commands::SetDone { task_id, undo } => {
            let done = !undo;
            let remote =
                RemoteVoxConfig::from_args(cli.server, cli.session_token, cli.organization_id)?;
            let session = LiveSession::open(&remote).await?;
            let task_repo = TaskRepoLoro::new(&session.doc);
            let big_page = Page {
                index: 0,
                size: 1000,
            };
            // Resolve a UUID prefix → full id by scanning the local
            // snapshot. No round-trip needed; the doc is already
            // loaded.
            let id = if let Ok(id) = Uuid::parse_str(&task_id) {
                id
            } else {
                let page = task_repo
                    .list(big_page, None, None)
                    .await
                    .map_err(|e| eyre::eyre!("task list (for id resolve): {e}"))?;
                let prefix = task_id.to_lowercase();
                let matches: Vec<_> = page
                    .items
                    .iter()
                    .filter(|t| t.id.to_string().starts_with(&prefix))
                    .collect();
                match matches.as_slice() {
                    [] => return Err(eyre::eyre!("no task matched prefix {task_id:?}")),
                    [t] => t.id,
                    multi => {
                        return Err(eyre::eyre!(
                            "{} tasks matched prefix {task_id:?}; use a longer prefix",
                            multi.len()
                        ));
                    }
                }
            };
            let updated = task_repo
                .update(
                    id,
                    TaskUpdate {
                        done: Some(done),
                        ..Default::default()
                    },
                )
                .await
                .map_err(|e| eyre::eyre!("task update: {e}"))?;
            let mark = if updated.done { "done" } else { "open" };
            println!("{} {} → {mark}", updated.id, updated.title);
            session.flush().await?;
        }
        Commands::NewTask { project, title } => {
            let remote =
                RemoteVoxConfig::from_args(cli.server, cli.session_token, cli.organization_id)?;
            let session = LiveSession::open(&remote).await?;
            let project_repo = ProjectRepoLoro::new(&session.doc);
            let task_repo = TaskRepoLoro::new(&session.doc);
            let big_page = Page {
                index: 0,
                size: 1000,
            };
            let projects = project_repo
                .list(big_page, None, None)
                .await
                .map_err(|e| eyre::eyre!("project list: {e}"))?;
            // Exact match first; then case-insensitive prefix.
            let project_id = if let Some(p) = projects.items.iter().find(|p| p.name == project) {
                p.id
            } else {
                let needle = project.to_lowercase();
                let matches: Vec<_> = projects
                    .items
                    .iter()
                    .filter(|p| p.name.to_lowercase().starts_with(&needle))
                    .collect();
                match matches.as_slice() {
                    [] => return Err(eyre::eyre!("no project matched {project:?}")),
                    [p] => p.id,
                    multi => {
                        return Err(eyre::eyre!(
                            "{} projects matched {project:?}; be more specific",
                            multi.len()
                        ));
                    }
                }
            };
            let created = task_repo
                .create(TaskCreate {
                    project_id,
                    title: title.clone(),
                    done: false,
                })
                .await
                .map_err(|e| eyre::eyre!("task create: {e}"))?;
            println!("{} {}", created.id, created.title);
            session.flush().await?;
        }
        Commands::NewProject { name } => {
            let remote =
                RemoteVoxConfig::from_args(cli.server, cli.session_token, cli.organization_id)?;
            let session = LiveSession::open(&remote).await?;
            let project_repo = ProjectRepoLoro::new(&session.doc);
            let created = project_repo
                .create(ProjectCreate { name: name.clone() })
                .await
                .map_err(|e| eyre::eyre!("project create: {e}"))?;
            println!("{} {}", created.id, created.name);
            session.flush().await?;
        }
        Commands::Doctor => {
            let remote =
                RemoteVoxConfig::from_args(cli.server, cli.session_token, cli.organization_id)?;
            println!("Vox endpoint: {}", remote.display_url);
        }
    }
    Ok(())
}
