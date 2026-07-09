//! fts — the one FastTrackStudio CLI.
//!
//! Mounts the domain CLIs as subcommands (`fts daw <...>` → `daw::cli`,
//! `fts keyflow <...>` → `keyflow-cli`) and manages the headless engines:
//! `fts signal engine` spawns the `signal-engine` binary (the rig core,
//! ws://:4040/vox), `fts session engine` runs the standalone session
//! engine in-process (ws://:3030/ws), `fts status` probes both ports.

use std::ffi::OsString;

use clap::{Parser, Subcommand};
use engine_launcher::{LaunchSource, SESSION_ENGINE, SIGNAL_ENGINE, probe, spawn};
use eyre::Result;

#[cfg(feature = "session")]
mod session_engine;

#[derive(Parser)]
#[command(name = "fts", about = "FastTrackStudio — one CLI over the whole stack")]
struct Fts {
    #[command(subcommand)]
    command: Cmd,
}

#[derive(Subcommand)]
enum Cmd {
    /// The daw command tree (live-query REAPER / standalone) — `fts daw tracks`, ...
    #[cfg(feature = "daw")]
    #[command(disable_help_flag = true)]
    Daw {
        /// Arguments forwarded verbatim to the daw CLI.
        #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
        args: Vec<OsString>,
    },
    /// The keyflow chart CLI — `fts keyflow parse chart.kf`, ... (alias: kf)
    #[cfg(feature = "keyflow")]
    #[command(alias = "kf", disable_help_flag = true)]
    Keyflow {
        /// Arguments forwarded verbatim to the kf CLI.
        #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
        args: Vec<OsString>,
    },
    /// Signal domain — the headless rig core.
    Signal {
        #[command(subcommand)]
        command: SignalCmd,
    },
    /// Session domain — the standalone setlist engine.
    Session {
        #[command(subcommand)]
        command: SessionCmd,
    },
    /// Probe the known engine ports and report up/down.
    Status,
}

#[derive(Subcommand)]
enum SignalCmd {
    /// Spawn the signal-engine binary (guitar in → NAM chain → out,
    /// vox router on ws://:4040/vox). Detaches by default.
    Engine {
        /// Bind address for the engine (sets SIGNAL_ENGINE_ADDR).
        #[arg(long)]
        addr: Option<String>,
        /// Run attached to this terminal instead of detaching.
        #[arg(long)]
        foreground: bool,
    },
}

#[derive(Subcommand)]
enum SessionCmd {
    /// Run the standalone session engine in-process: daw-standalone +
    /// SetlistService + stream pumps, served on ws://:3030/ws.
    Engine {
        /// Bind address (default 0.0.0.0:3030; SESSION_ENGINE_ADDR also honored).
        #[arg(long)]
        addr: Option<String>,
    },
}

fn main() -> Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env().unwrap_or_else(|_| "info".into()),
        )
        .with_writer(std::io::stderr)
        .init();

    match Fts::parse().command {
        #[cfg(feature = "daw")]
        Cmd::Daw { args } => {
            let argv = std::iter::once(OsString::from("daw")).chain(args);
            tokio::runtime::Builder::new_multi_thread()
                .enable_all()
                .build()?
                .block_on(daw::cli::cli_main(argv))
        }
        #[cfg(feature = "keyflow")]
        Cmd::Keyflow { args } => {
            let argv = std::iter::once(OsString::from("kf")).chain(args);
            keyflow_cli::cli_main(argv);
            Ok(())
        }
        Cmd::Signal {
            command: SignalCmd::Engine { addr, foreground },
        } => signal_engine(addr, foreground),
        Cmd::Session {
            command: SessionCmd::Engine { addr },
        } => session_engine_cmd(addr),
        Cmd::Status => {
            status();
            Ok(())
        }
    }
}

/// `fts signal engine` — spawn the signal-engine binary as a child process.
fn signal_engine(addr: Option<String>, foreground: bool) -> Result<()> {
    // Only meaningful on the default port — a custom --addr is the
    // caller saying "bind here", e.g. a second engine next to a live one.
    if addr.is_none() && probe(&SIGNAL_ENGINE) {
        println!(
            "signal engine already running — {}",
            SIGNAL_ENGINE.ws_url()
        );
        return Ok(());
    }

    // The engine reads SIGNAL_ENGINE_ADDR (legacy RIGD_ADDR still honored);
    // the rest of the environment propagates as-is.
    let mut env = Vec::new();
    if let Some(addr) = &addr {
        env.push(("SIGNAL_ENGINE_ADDR".to_string(), addr.clone()));
    }

    let spawned = spawn(&SIGNAL_ENGINE, &[], &env, !foreground)?;
    let url = match &addr {
        Some(a) => format!("ws://{a}/vox"),
        None => SIGNAL_ENGINE.ws_url(),
    };
    match &spawned.source {
        LaunchSource::Binary(path) => eprintln!("spawned {}", path.display()),
        LaunchSource::Cargo => eprintln!("no signal-engine binary found — `cargo run -p signal-engine` (dev fallback)"),
    }

    if foreground {
        // Attached: the child shares our process group, so Ctrl-C reaches
        // it directly; we just wait and mirror its exit status.
        println!("signal engine (foreground) — {url}");
        let mut child = spawned.child;
        let status = child.wait()?;
        if !status.success() {
            eyre::bail!("signal-engine exited: {status}");
        }
        Ok(())
    } else {
        println!("signal engine started — pid {} — {url}", spawned.pid());
        Ok(())
    }
}

/// `fts session engine` — the in-process standalone session engine.
#[cfg(feature = "session")]
fn session_engine_cmd(addr: Option<String>) -> Result<()> {
    session_engine::run_blocking(addr)
}

#[cfg(not(feature = "session"))]
fn session_engine_cmd(_addr: Option<String>) -> Result<()> {
    eyre::bail!("fts was built without the `session` feature")
}

/// `fts status` — up/down per known engine port.
fn status() {
    for engine in [&SIGNAL_ENGINE, &SESSION_ENGINE] {
        let up = probe(engine);
        println!(
            "{:<8} {}  {}",
            engine.name.to_lowercase(),
            if up { "up  " } else { "down" },
            engine.ws_url(),
        );
    }
}
