//! `task` CLI — vertical-slice scaffold.
//!
//! Right now the CLI is intentionally tiny while the per-feature
//! commands get rebuilt on top of vox-RPC against the new
//! features/* trios. The `server` subcommand opens a vox session to
//! verify connectivity; `task list` is a placeholder until the
//! Project + Task vox services land.

mod shared;

use clap::{Parser, Subcommand};
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
            let _remote =
                RemoteVoxConfig::from_args(cli.server, cli.session_token, cli.organization_id)?;
            println!(
                "task list: vox transport not wired yet — stand by for Phase D Project/Task service clients."
            );
        }
        Commands::Doctor => {
            let remote =
                RemoteVoxConfig::from_args(cli.server, cli.session_token, cli.organization_id)?;
            println!("Vox endpoint: {}", remote.display_url);
        }
    }
    Ok(())
}
