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
mod macos_app;
mod plugins;
mod reaper_env;

use std::path::PathBuf;

use clap::{Args, Parser, Subcommand};
use eyre::{Context, eyre};

use crate::layout::Layout;

/// Platform suffix used in Linux release asset names, e.g.
/// `fasttrackstudio-v0.1.0-x86_64-linux.tar.gz`. The macOS app/plugin
/// assets are universal (both arches, `lipo`'d together) so they don't
/// need one — see `codeberg::resolve_macos_dmg` /
/// `resolve_macos_plugins_zip`.
pub fn platform_suffix() -> eyre::Result<&'static str> {
    match (std::env::consts::OS, std::env::consts::ARCH) {
        ("linux", "x86_64") => Ok("x86_64-linux"),
        (os, arch) => Err(eyre!(
            "no FastTrackStudio release builds for {os}/{arch} yet (currently linux/x86_64 only — macOS goes through resolve_macos_dmg/resolve_macos_plugins_zip instead)"
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
    /// Manage the FTS plugin bundle (.clap/.vst3 → ~/.clap, ~/.vst3).
    Plugins {
        #[command(subcommand)]
        cmd: PluginsCmd,
    },
    /// The full installer package: REAPER + SWS + ReaPack + the three FTS
    /// rigs, the FastTrackStudio app (with its REAPER extension dropped
    /// into each rig), and every FTS plugin. One command, nothing else to
    /// download by hand.
    Bundle {
        /// Install under this directory instead of $HOME.
        #[arg(long, value_name = "DIR")]
        prefix: Option<PathBuf>,
        /// Install a specific FastTrackStudio release tag instead of the
        /// latest (applies to the app and plugin bundle both).
        #[arg(long, value_name = "TAG")]
        version: Option<String>,
    },
}

#[derive(Subcommand)]
enum PluginsCmd {
    /// Download and install the plugin bundle (default: latest release).
    Install {
        /// Install a specific release tag instead of the latest.
        #[arg(long, value_name = "TAG")]
        version: Option<String>,
        /// Install from a local tarball or extracted directory instead of
        /// the release feed (e.g. target/bundled after `just plugins-bundle`).
        #[arg(long, value_name = "PATH")]
        from: Option<PathBuf>,
        /// Install under this directory instead of $HOME.
        #[arg(long, value_name = "DIR")]
        prefix: Option<PathBuf>,
    },
    /// Remove the installed plugin bundle (manifest-driven).
    Uninstall {
        #[arg(long, value_name = "DIR")]
        prefix: Option<PathBuf>,
    },
    /// Show the installed plugin bundle version and contents.
    List {
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
        Some(Cmd::Uninstall { prefix }) => uninstall(prefix),
        Some(Cmd::Reaper { prefix }) => reaper_env::setup(prefix).await,
        Some(Cmd::Bundle { prefix, version }) => bundle(prefix, version).await,
        Some(Cmd::Plugins { cmd }) => match cmd {
            PluginsCmd::Install {
                version,
                from,
                prefix,
            } => plugins::install(version, from, prefix).await,
            PluginsCmd::Uninstall { prefix } => plugins::uninstall(prefix),
            PluginsCmd::List { prefix } => plugins::list(prefix),
        },
    };
    if let Err(e) = result {
        eprintln!("error: {e:#}");
        std::process::exit(1);
    }
}

async fn install(args: InstallArgs) -> eyre::Result<()> {
    if cfg!(target_os = "macos") {
        return macos_app::install(args.prefix, args.version, args.url).await;
    }

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
            let sums = url
                .rsplit_once('/')
                .map(|(dir, _)| format!("{dir}/SHA256SUMS"));
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

    install_tarball(
        &client,
        &layout,
        &tarball_url,
        &tarball_name,
        sums_url.as_deref(),
    )
    .await?;

    if let Some(tag) = tag {
        println!("installed FastTrackStudio {tag}");
    } else {
        println!("installed FastTrackStudio from {tarball_url}");
    }
    Ok(())
}

/// The full installer package: REAPER env first (so the app install below
/// has rig dirs to drop `reaper_fts_extensions.so` into), then the
/// FastTrackStudio app, then every FTS plugin.
async fn bundle(prefix: Option<PathBuf>, version: Option<String>) -> eyre::Result<()> {
    println!("== 1/3: REAPER + SWS + ReaPack + the three FTS rigs ==");
    reaper_env::setup(prefix.clone()).await?;

    println!("\n== 2/3: FastTrackStudio app ==");
    install(InstallArgs {
        version: version.clone(),
        prefix: prefix.clone(),
        url: None,
    })
    .await?;

    println!("\n== 3/3: FTS plugins ==");
    plugins::install(version, None, prefix).await?;

    println!("\nbundle install complete.");
    Ok(())
}

fn uninstall(prefix: Option<PathBuf>) -> eyre::Result<()> {
    if cfg!(target_os = "macos") {
        return macos_app::uninstall(prefix);
    }
    Layout::new(prefix)?.uninstall()
}

async fn update(prefix: Option<PathBuf>) -> eyre::Result<()> {
    if cfg!(target_os = "macos") {
        return macos_app::update(prefix).await;
    }

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
        None => println!(
            "no installed version found — installing {} ...",
            release.tag
        ),
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
