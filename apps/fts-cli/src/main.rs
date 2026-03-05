//! fts — Central CLI for the FastTrack Studio stack.
//!
//! Provides full read/write programmatic access to DAW control,
//! Signal (rig/patch/macro system), and Session (setlist/navigation).

mod daw;
mod introspect;

use std::path::PathBuf;

use clap::{Parser, Subcommand};
use eyre::Result;

#[derive(Parser)]
#[command(
    name = "fts",
    about = "FastTrack Studio CLI — full read/write access to the FTS stack"
)]
struct Cli {
    /// Unix socket path for DAW connection (auto-discovers from /tmp if omitted)
    #[arg(long, global = true)]
    socket: Option<PathBuf>,

    /// Signal DB path (defaults to ~/Music/FastTrackStudio/Library/signal.db)
    #[arg(long, global = true)]
    db: Option<PathBuf>,

    /// Output as JSON
    #[arg(long, global = true)]
    json: bool,

    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// DAW control (tracks, FX, transport, markers, regions)
    #[command(subcommand)]
    Daw(daw::DawCommand),
    /// Signal library (presets, rigs, profiles, macros, songs, setlists)
    #[command(subcommand)]
    Signal(signal_cli::SignalCommand),
    /// Live session control (setlist navigation, playback)
    #[command(subcommand)]
    Session(session_cli::SessionCommand),
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

    match cli.command {
        Command::Daw(cmd) => daw::run(cli.socket, cmd, cli.json).await?,
        Command::Signal(cmd) => signal_cli::run(cli.db, cli.socket.clone(), cmd, cli.json).await?,
        Command::Session(cmd) => session_cli::run(cli.socket, cmd, cli.json).await?,
    }

    Ok(())
}
