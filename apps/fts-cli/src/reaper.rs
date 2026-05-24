//! `fts reaper …` — launch / inspect the FTS REAPER dev configuration.
//!
//! Today this is a thin shim over the existing `fts-dev` shell script
//! (installed by the nix flake at `/run/current-system/sw/bin/fts-dev`)
//! which knows how to boot REAPER inside the right FHS wrapper, with
//! the right cfgfile, and tag the window for later discovery.
//!
//! Centralising the entry point here means scripts and humans only
//! have to remember `fts reaper dev` (or `fts reaper kill`) instead of
//! "which nix-store path was the launcher again?".

use std::process::{Command, Stdio};

use clap::Subcommand;
use eyre::{Result, WrapErr, bail};

#[derive(Subcommand)]
pub enum ReaperCommand {
    /// Launch the FTS dev REAPER instance (`fts-dev` script).
    ///
    /// Reads `~/.fts-dev/launch.json` for executable / cfgfile / args.
    /// Streams REAPER's stderr to this terminal until REAPER exits.
    Dev {
        /// Additional args forwarded to REAPER (after `--`). Example:
        /// `fts reaper dev -- path/to/project.RPP`.
        #[arg(last = true)]
        extra: Vec<String>,
    },
    /// Re-run the `fts-dev --setup` flow that regenerates the launch
    /// config + symlinks.
    Setup,
    /// Print the resolved launch config (`~/.fts-dev/launch.json`).
    Config,
}

pub fn run(cmd: ReaperCommand) -> Result<()> {
    match cmd {
        ReaperCommand::Dev { extra } => run_dev(extra),
        ReaperCommand::Setup => run_setup(),
        ReaperCommand::Config => print_config(),
    }
}

fn fts_dev_bin() -> Result<String> {
    // The fts-dev script lives in the system path on machines built
    // with the FTS nix flake. PATH-resolve at call time so a user
    // overriding with their own dev script still gets honored.
    let path = which::which("fts-dev")
        .or_else(|_| which::which("fts_dev"))
        .map_err(|_| {
            eyre::eyre!(
                "`fts-dev` not found on PATH. Install the FTS dev nix flake or run \
                 the REAPER launcher directly per ~/.fts-dev/launch.json."
            )
        })?;
    Ok(path.to_string_lossy().to_string())
}

fn run_dev(extra: Vec<String>) -> Result<()> {
    let bin = fts_dev_bin()?;
    let status = Command::new(&bin)
        .args(&extra)
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .status()
        .wrap_err_with(|| format!("spawning {bin}"))?;
    if !status.success() {
        bail!("fts-dev exited with status {status}");
    }
    Ok(())
}

fn run_setup() -> Result<()> {
    let bin = fts_dev_bin()?;
    let status = Command::new(&bin)
        .arg("--setup")
        .stdin(Stdio::inherit())
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .status()
        .wrap_err_with(|| format!("spawning {bin} --setup"))?;
    if !status.success() {
        bail!("fts-dev --setup exited with status {status}");
    }
    Ok(())
}

fn print_config() -> Result<()> {
    let home = std::env::var_os("HOME").ok_or_else(|| eyre::eyre!("HOME not set"))?;
    let path = std::path::PathBuf::from(home).join(".fts-dev/launch.json");
    if !path.exists() {
        bail!(
            "no launch config at {} — run `fts reaper setup` to generate one",
            path.display()
        );
    }
    let body = std::fs::read_to_string(&path).wrap_err_with(|| format!("read {}", path.display()))?;
    println!("{body}");
    Ok(())
}
