//! fts — Central CLI for the FastTrackStudio stack.
//!
//! Today this is a thin shim over the domain CLIs:
//! - `fts session …`  — live session control (mode, setlist, …) via
//!   the Vox socket published by the `fts-extensions` REAPER plugin.
//!
//! Daw + Signal subcommands are temporarily disabled while their
//! crates catch up to the current `vox` / `facet` / `daw` API. See the
//! commented-out blocks in this file and `Cargo.toml` for re-enable
//! checklists.

mod reaper;
mod tui;

use std::path::PathBuf;

use clap::{Parser, Subcommand};
use eyre::Result;

#[derive(Parser)]
#[command(
    name = "fts",
    about = "FastTrackStudio CLI — control the running REAPER stack"
)]
struct Cli {
    /// Unix socket path for the DAW connection. Defaults to
    /// auto-discovery of the most-recently-modified `/tmp/fts-daw-*.sock`.
    #[arg(long, global = true)]
    socket: Option<PathBuf>,

    /// Output as JSON. Default is human-readable.
    #[arg(long, global = true)]
    json: bool,

    /// Launch the interactive ratatui dashboard. When set, any
    /// subcommand is ignored.
    #[arg(short = 'i', long)]
    interactive: bool,

    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Subcommand)]
enum Command {
    /// Launch / inspect the FTS REAPER dev configuration.
    #[command(subcommand)]
    Reaper(reaper::ReaperCommand),
    /// Live session control — mode switching, setlist, playback.
    #[command(subcommand)]
    Session(session_cli::SessionCommand),
    //
    // === Disabled subcommands ===
    //
    // `fts daw …` — needs the fts-cli daw.rs / introspect.rs modules
    // ported to the current daw API. Stale references to FxParamHandle,
    // private Daw struct, etc.
    //
    // `fts signal …` — signal crate still pins Codys-Wright/vox@2a2f793b
    // (facet 0.44); workspace is on bearcove/vox@27eef573 (facet 0.46).
    // Re-enable both `signal-cli` in Cargo.toml and the subcommand here
    // once signal bumps vox/facet.
}

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "warn".into()),
        )
        .with_writer(std::io::stderr)
        .init();

    let cli = Cli::parse();
    if cli.interactive {
        return tui::run(cli.socket).await;
    }
    match cli.command {
        Some(Command::Reaper(cmd)) => reaper::run(cmd)?,
        Some(Command::Session(cmd)) => session_cli::run(cli.socket, cmd, cli.json).await?,
        None => {
            // No subcommand and no -i flag — print help and exit
            // non-zero (clap's behaviour when subcommand is required).
            use clap::CommandFactory;
            Cli::command().print_help().ok();
            println!();
            std::process::exit(2);
        }
    }
    Ok(())
}
