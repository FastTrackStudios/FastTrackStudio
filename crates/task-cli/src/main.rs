//! `task` CLI — vertical-slice scaffold.
//!
//! Talks to a `task-server` over vox WebSocket. `task list` calls the
//! auto-generated `TaskRepoClient::list`; `task doctor` just prints
//! the resolved endpoint URL.

mod shared;

use clap::{Parser, Subcommand};
use project_proto::TaskRepoClient;
use project_proto::architect::Page;
use shared::RemoteVoxConfig;

#[derive(Parser)]
#[command(name = "task", about = "Task management CLI", version)]
struct Cli {
    /// Vox WebSocket URL (e.g. ws://127.0.0.1:9090/vox).
    #[arg(long, env = "TASK_SERVER", global = true)]
    server: Option<String>,

    /// Better Auth session token for remote vox.
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
            let client: TaskRepoClient = remote.connect().await?;
            let page = client
                .list(Page::default(), None, None)
                .await
                .map_err(|e| eyre::eyre!("task list: {e}"))?;
            if page.items.is_empty() {
                println!("(no tasks)");
            } else {
                for task in page.items {
                    println!("{}\t{}\t{}", task.id, task.status, task.title);
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
