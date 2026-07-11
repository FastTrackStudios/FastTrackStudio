//! fts-installer — thin downloader/installer for FastTrackStudio releases.
//!
//! Resolves a release on codeberg (Gitea API), downloads the platform
//! tarball with progress + retry, verifies it against the release's
//! SHA256SUMS when present, extracts it, and applies the installed layout
//! (the same one `just install` and the tarball's `install.sh` produce):
//!
//! ```text
//! <prefix>/.local/lib/fts/{fasttrackstudio,fts,VERSION}
//! <prefix>/.local/bin/{fasttrackstudio,fts}      (symlinks)
//! <prefix>/.config/systemd/user/signal-engine.service   (installed, NOT enabled)
//! <prefix>/.local/share/applications/fasttrackstudio.desktop
//! <prefix>/.local/share/icons/hicolor/scalable/apps/fasttrackstudio.svg
//! ```
//!
//! `--prefix` defaults to `$HOME`; with a non-home prefix the systemd /
//! desktop-database refreshes are skipped (test installs).

mod codeberg;
mod fetch;
mod layout;
mod reaper_env;

use std::path::PathBuf;

use clap::{Args, Parser, Subcommand};
use eyre::{Context, eyre};

use crate::layout::Layout;

/// Platform suffix used in release asset names, e.g.
/// `fasttrackstudio-v0.1.0-x86_64-linux.tar.gz`.
pub fn platform_suffix() -> eyre::Result<&'static str> {
    match (std::env::consts::OS, std::env::consts::ARCH) {
        ("linux", "x86_64") => Ok("x86_64-linux"),
        (os, arch) => Err(eyre!(
            "no FastTrackStudio release builds for {os}/{arch} yet (currently only linux/x86_64)"
        )),
    }
}

// No auto --version flag: it would collide with the flattened default-
// command `install --version <TAG>` (clap debug-asserts on this).
#[derive(Parser)]
#[command(
    name = "fts-installer",
    disable_version_flag = true,
    about = "Download and install FastTrackStudio"
)]
struct Cli {
    #[command(subcommand)]
    command: Option<Cmd>,

    /// (default command) install options
    #[command(flatten)]
    install: InstallArgs,
}

#[derive(Subcommand)]
enum Cmd {
    /// Download and install FastTrackStudio (the default).
    Install(InstallArgs),
    /// Install the latest release unless it is already installed.
    Update {
        /// Install under this directory instead of $HOME.
        #[arg(long, value_name = "DIR")]
        prefix: Option<PathBuf>,
    },
    /// Remove an installed FastTrackStudio (keeps user data in
    /// ~/.config/fts and ~/.config/signal).
    Uninstall {
        /// Uninstall from this directory instead of $HOME.
        #[arg(long, value_name = "DIR")]
        prefix: Option<PathBuf>,
    },
    /// Set up the complete REAPER environment: download REAPER + SWS +
    /// ReaPack (official sources) and create the three FTS rigs
    /// (fts-reaper, fts-tracks, fts-dev) with launch.json + desktop
    /// entries. Safe to re-run; never clobbers an existing reaper.ini.
    Reaper {
        /// Install under this directory instead of $HOME (test installs;
        /// skips desktop entries).
        #[arg(long, value_name = "DIR")]
        prefix: Option<PathBuf>,
    },
}

#[derive(Args, Default)]
struct InstallArgs {
    /// Install a specific release tag (e.g. v0.1.0) instead of the latest.
    #[arg(long, value_name = "TAG")]
    version: Option<String>,

    /// Install under this directory instead of $HOME.
    #[arg(long, value_name = "DIR")]
    prefix: Option<PathBuf>,

    /// Install from a direct tarball URL, skipping the codeberg API.
    #[arg(long, value_name = "URL")]
    url: Option<String>,
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let cli = Cli::parse();
    let result = match cli.command {
        None => install(cli.install).await,
        Some(Cmd::Install(args)) => install(args).await,
        Some(Cmd::Update { prefix }) => update(prefix).await,
        Some(Cmd::Uninstall { prefix }) => Layout::new(prefix).and_then(|l| l.uninstall()),
        Some(Cmd::Reaper { prefix }) => reaper_env::setup(prefix).await,
    };
    if let Err(e) = result {
        eprintln!("error: {e:#}");
        std::process::exit(1);
    }
}

async fn install(args: InstallArgs) -> eyre::Result<()> {
    let layout = Layout::new(args.prefix)?;
    let client = fetch::http_client()?;

    // Resolve the tarball URL (+ optional SHA256SUMS location).
    let (tarball_url, tarball_name, sums_url, tag) = match &args.url {
        Some(url) => {
            let name = url
                .rsplit('/')
                .next()
                .filter(|n| !n.is_empty())
                .ok_or_else(|| eyre!("--url has no filename component: {url}"))?
                .to_string();
            // Best-effort: look for SHA256SUMS next to the tarball.
            let sums = url.rsplit_once('/').map(|(dir, _)| format!("{dir}/SHA256SUMS"));
            (url.clone(), name, sums, None)
        }
        None => {
            let release = codeberg::resolve(&client, args.version.as_deref()).await?;
            println!("release: {} ({})", release.tag, release.tarball.name);
            (
                release.tarball.url,
                release.tarball.name,
                release.sums.map(|a| a.url),
                Some(release.tag),
            )
        }
    };

    install_tarball(&client, &layout, &tarball_url, &tarball_name, sums_url.as_deref()).await?;

    if let Some(tag) = tag {
        println!("installed FastTrackStudio {tag}");
    } else {
        println!("installed FastTrackStudio from {tarball_url}");
    }
    Ok(())
}

async fn update(prefix: Option<PathBuf>) -> eyre::Result<()> {
    let layout = Layout::new(prefix)?;
    let client = fetch::http_client()?;
    let release = codeberg::resolve(&client, None).await?;
    let latest = release.tag.trim_start_matches('v').to_string();

    match layout.installed_version() {
        Some(installed) if installed.trim_start_matches('v') == latest => {
            println!("FastTrackStudio {installed} is already the latest release — nothing to do");
            return Ok(());
        }
        Some(installed) => println!("updating {installed} -> {} ...", release.tag),
        None => println!("no installed version found — installing {} ...", release.tag),
    }

    install_tarball(
        &client,
        &layout,
        &release.tarball.url,
        &release.tarball.name,
        release.sums.as_ref().map(|a| a.url.as_str()),
    )
    .await?;
    println!("installed FastTrackStudio {}", release.tag);
    Ok(())
}

/// Download → verify → extract → apply layout.
async fn install_tarball(
    client: &reqwest::Client,
    layout: &Layout,
    tarball_url: &str,
    tarball_name: &str,
    sums_url: Option<&str>,
) -> eyre::Result<()> {
    let work = tempfile_dir()?;
    let tarball_path = work.join(tarball_name);

    fetch::download(client, tarball_url, &tarball_path, tarball_name).await?;

    // Verify against SHA256SUMS when we can get it (release asset, or a
    // sibling of a --url tarball); a missing sums file is only a warning.
    match sums_url {
        Some(url) => match fetch::fetch_text(client, url).await {
            Ok(sums) => {
                fetch::verify_sha256(&tarball_path, tarball_name, &sums)?;
                println!("  sha256 verified");
            }
            Err(e) => println!("  warning: SHA256SUMS not fetched ({e}); skipping verification"),
        },
        None => println!("  warning: no SHA256SUMS available; skipping verification"),
    }

    let stage = work.join("extracted");
    fetch::extract_tarball(&tarball_path, &stage)
        .wrap_err_with(|| format!("extracting {tarball_name}"))?;

    layout.install(&stage)?;

    let _ = std::fs::remove_dir_all(&work);
    Ok(())
}

/// A fresh temp working directory for this run.
fn tempfile_dir() -> eyre::Result<PathBuf> {
    let dir = std::env::temp_dir().join(format!("fts-installer-{}", std::process::id()));
    std::fs::create_dir_all(&dir).wrap_err_with(|| format!("creating {}", dir.display()))?;
    Ok(dir)
}
