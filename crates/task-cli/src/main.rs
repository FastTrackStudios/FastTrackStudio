//! `task` CLI — vertical-slice scaffold.
//!
//! Talks to a `task-server` over vox WebSocket. `task list` calls the
//! auto-generated `TaskRepoClient::list`; `task doctor` just prints
//! the resolved endpoint URL.

mod shared;

use std::collections::HashMap;

use clap::{Parser, Subcommand};
use project_proto::architect::Page;
use project_proto::{ProjectRepoClient, TaskRepoClient};
use shared::RemoteVoxConfig;
use uuid::Uuid;

#[derive(Parser)]
#[command(name = "task", about = "Task management CLI", version)]
struct Cli {
    /// Vox WebSocket URL (e.g. ws://127.0.0.1:9090/vox).
    #[arg(long, env = "TASK_SERVER", global = true)]
    server: Option<String>,

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
    /// List tasks via the project-proto Task vox client (TODO).
    List,
    /// Probe the configured vox endpoint.
    Doctor,
}

#[tokio::main]
async fn main() -> eyre::Result<()> {
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
                        println!("  [{}] {}", task.status, task.title);
                    }
                }
            }
        }
        Commands::Doctor => {
            let remote =
                RemoteVoxConfig::from_args(cli.server, cli.session_token, cli.organization_id)?;
            println!("Vox endpoint: {}", remote.display_url);
        }
    }
    Ok(())
}
