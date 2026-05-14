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
use project_proto::architect::Page;
use project_proto::{ProjectRepoClient, TaskRepoClient, TaskUpdate};
use shared::RemoteVoxConfig;
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
        /// `true` (or absent) to mark done; `false` to clear.
        #[arg(default_value_t = true)]
        done: bool,
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
            // Two parallel sessions — `vox::connect` is cheap and
            // each `*RepoClient` owns its own session driver.
            let projects: ProjectRepoClient = remote.connect().await?;
            let tasks: TaskRepoClient = remote.connect().await?;
            let project_page = projects
                .list(
                    Page {
                        index: 0,
                        size: 1000,
                    },
                    None,
                    None,
                )
                .await
                .map_err(|e| eyre::eyre!("project list: {e}"))?;
            let task_page = tasks
                .list(
                    Page {
                        index: 0,
                        size: 1000,
                    },
                    None,
                    None,
                )
                .await
                .map_err(|e| eyre::eyre!("task list: {e}"))?;

            let project_names: HashMap<Uuid, String> = project_page
                .items
                .into_iter()
                .map(|p| (p.id, p.name))
                .collect();
            let mut grouped: HashMap<Uuid, Vec<_>> = HashMap::new();
            for task in task_page.items {
                grouped.entry(task.project_id).or_default().push(task);
            }

            if grouped.is_empty() {
                println!("(no tasks)");
            } else {
                let mut project_ids: Vec<_> = grouped.keys().copied().collect();
                project_ids.sort_by_key(|id| {
                    project_names
                        .get(id)
                        .cloned()
                        .unwrap_or_else(|| "(unknown)".into())
                });
                for project_id in project_ids {
                    let name = project_names
                        .get(&project_id)
                        .cloned()
                        .unwrap_or_else(|| format!("(unknown project {project_id})"));
                    let tasks = grouped.get(&project_id).expect("inserted above");
                    println!("\n## {name}  ({} tasks)", tasks.len());
                    for task in tasks {
                        let short_id = &task.id.to_string()[..8];
                        let mark = if task.done { "[x]" } else { "[ ]" };
                        println!("  [{short_id}] {mark} {}", task.title);
                    }
                }
            }
        }
        Commands::SetDone { task_id, done } => {
            let remote =
                RemoteVoxConfig::from_args(cli.server, cli.session_token, cli.organization_id)?;
            let tasks: TaskRepoClient = remote.connect().await?;
            // Resolve a UUID prefix → full id by listing all tasks.
            // Cheap at this scale; revisit if/when the corpus grows
            // past the page size.
            let id = if let Ok(id) = Uuid::parse_str(&task_id) {
                id
            } else {
                let page = tasks
                    .list(
                        Page {
                            index: 0,
                            size: 1000,
                        },
                        None,
                        None,
                    )
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
            let updated = tasks
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
        }
        Commands::Doctor => {
            let remote =
                RemoteVoxConfig::from_args(cli.server, cli.session_token, cli.organization_id)?;
            println!("Vox endpoint: {}", remote.display_url);
        }
    }
    Ok(())
}
