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

mod daemon;
mod reaper;
mod tui;

use std::path::PathBuf;

use clap::{Parser, Subcommand};
use eyre::Result;

#[derive(Subcommand)]
enum DaemonCommand {
    /// Run the daemon in the foreground until killed. Pair with
    /// `fts reaper dev` (which detaches by default) so REAPER and
    /// the daemon share an environment.
    Serve,
    /// Probe an already-running daemon. Prints `pong\n` + roundtrip
    /// timing on success, or a connection error.
    Ping,
    /// Ask the running daemon to exit. No-op if no daemon is up.
    Stop,
    /// Print the socket path the daemon will / does listen on.
    Path,
}

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
    /// Run / control the `ftsd` daemon. The daemon holds the Vox
    /// connection so one-shot commands skip the 870ms cranelift JIT
    /// cost on every invocation.
    #[command(subcommand)]
    Daemon(DaemonCommand),
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
        Some(Command::Daemon(cmd)) => run_daemon_cmd(cmd, cli.socket).await?,
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

/// Try to satisfy a session command via the daemon. Returns:
/// - `Ok(Some(true))`  — daemon handled it; main should not call
///   `session_cli::run`.
/// - `Ok(Some(false))` — daemon was reached but doesn't know this
///   command yet; main should fall through to the direct path.
/// - `Ok(None)`        — no daemon up; main should fall through.
async fn daemon_fast_path(cmd: &session_cli::SessionCommand) -> Result<Option<bool>> {
    use session_cli::{ModeCommand, SessionCommand};
    let request = match cmd {
        SessionCommand::Mode(ModeCommand::Get) => daemon::Request::ModeGet,
        SessionCommand::Mode(ModeCommand::Set { slug }) => {
            daemon::Request::ModeSet(slug.clone())
        }
        SessionCommand::Mode(ModeCommand::List) => daemon::Request::ModeList,
        SessionCommand::Play => daemon::Request::PlayPause,
        SessionCommand::Pause => daemon::Request::Pause,
        SessionCommand::Stop => daemon::Request::Stop,
        _ => return Ok(Some(false)),
    };
    let Some(resp) = daemon::try_call(request).await? else {
        return Ok(None);
    };
    match resp {
        daemon::Response::Ok => {}
        daemon::Response::Value(v) => println!("{v}"),
        daemon::Response::List(items) => {
            for item in items {
                println!("{item}");
            }
        }
        daemon::Response::Pong => println!("pong"),
        daemon::Response::Err(e) => eyre::bail!("daemon: {e}"),
    }
    Ok(Some(true))
}

async fn run_daemon_cmd(cmd: DaemonCommand, vox_socket: Option<PathBuf>) -> Result<()> {
    match cmd {
        DaemonCommand::Path => {
            println!("{}", daemon::default_socket_path().display());
        }
        DaemonCommand::Ping => {
            let t0 = std::time::Instant::now();
            match daemon::try_call(daemon::Request::Ping).await? {
                Some(daemon::Response::Pong) => {
                    println!("pong ({:?})", t0.elapsed());
                }
                Some(other) => eyre::bail!("unexpected daemon response: {other:?}"),
                None => eyre::bail!("no daemon listening at {}", daemon::default_socket_path().display()),
            }
        }
        DaemonCommand::Stop => match daemon::try_call(daemon::Request::Shutdown).await? {
            Some(daemon::Response::Ok) => println!("daemon stopping"),
            Some(other) => eyre::bail!("unexpected daemon response: {other:?}"),
            None => println!("no daemon running"),
        },
        DaemonCommand::Serve => {
            let socket = daemon::default_socket_path();
            // Foreground until killed — caller is expected to manage
            // backgrounding (systemd, `&`, `disown`, …).
            daemon::serve(socket, vox_socket).await?;
        }
    }
    Ok(())
}
