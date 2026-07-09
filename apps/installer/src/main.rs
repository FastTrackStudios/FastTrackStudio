//! FastTrackStudio Installer — downloads REAPER, installs extensions and presets.
//!
//! ## CLI flags
//!
//! ```text
//! fts-installer [OPTIONS]
//!
//!   --silent              Run without GUI (headless install)
//!   --install-dir <PATH>  Override the default install directory
//! ```

mod app;
mod wizard;

use std::path::PathBuf;

use clap::Parser;
use dioxus::desktop::tao::dpi::LogicalSize;
use dioxus::desktop::{Config, WindowBuilder};
use dioxus::prelude::*;
use installer_core::{InstallContext, InstallEvent, InstallPlan};
use tracing_subscriber::prelude::*;

const MAIN_CSS: Asset = asset!("/assets/main.css");
const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

#[derive(Parser)]
#[command(name = "fts-installer", about = "FastTrackStudio Installer")]
struct Cli {
    /// Run without GUI (headless install).
    #[arg(long)]
    silent: bool,

    /// Override the default install directory.
    #[arg(long, value_name = "PATH")]
    install_dir: Option<PathBuf>,
}

/// Log file location: ~/Library/Logs/FastTrackStudio/installer.log (macOS)
/// or ~/.local/share/FastTrackStudio/installer.log (Linux).
fn log_path() -> Option<PathBuf> {
    let home = std::env::var_os("HOME").map(PathBuf::from)?;
    if cfg!(target_os = "macos") {
        Some(home.join("Library/Logs/FastTrackStudio/installer.log"))
    } else {
        Some(home.join(".local/share/FastTrackStudio/installer.log"))
    }
}

fn main() {
    let cli = Cli::parse();

    // Log to stderr + file (if writable).
    let stderr_layer = tracing_subscriber::fmt::layer()
        .with_writer(std::io::stderr)
        .with_filter(tracing_subscriber::EnvFilter::new("info,installer_core=debug"));

    let registry = tracing_subscriber::registry().with(stderr_layer);

    if let Some(path) = log_path() {
        if let Some(parent) = path.parent() {
            let _ = std::fs::create_dir_all(parent);
        }
        if let Ok(file) = std::fs::File::create(&path) {
            let file_layer = tracing_subscriber::fmt::layer()
                .with_ansi(false)
                .with_writer(file)
                .with_filter(tracing_subscriber::EnvFilter::new("debug"));
            registry.with(file_layer).init();
            eprintln!("Logging to {}", path.display());
        } else {
            registry.init();
        }
    } else {
        registry.init();
    };

    let plan = match &cli.install_dir {
        Some(dir) => InstallPlan::with_install_dir(dir.clone()),
        None => InstallPlan::default_for_machine(),
    };

    if cli.silent {
        run_silent(plan);
    } else {
        // Store the plan override for the GUI to pick up.
        INITIAL_PLAN.with(|p| *p.borrow_mut() = Some(plan));

        let config = Config::new().with_window(
            WindowBuilder::new()
                .with_title("FastTrackStudio Installer")
                .with_inner_size(LogicalSize::new(640.0_f64, 500.0_f64))
                .with_resizable(false),
        );

        dioxus::LaunchBuilder::desktop()
            .with_cfg(config)
            .launch(app::App);
    }
}

thread_local! {
    /// Allows main() to pass CLI overrides to the Dioxus app component.
    pub static INITIAL_PLAN: std::cell::RefCell<Option<InstallPlan>> = const { std::cell::RefCell::new(None) };
}

/// Headless installer — runs all steps, prints progress to stdout, exits with
/// appropriate status code.
fn run_silent(plan: InstallPlan) {
    let rt = tokio::runtime::Runtime::new().expect("failed to create tokio runtime");
    rt.block_on(async move {
        if let Err(errors) = plan.validate() {
            for e in &errors {
                eprintln!("error: {e}");
            }
            std::process::exit(1);
        }

        println!("Installing to {}", plan.install_root.display());

        let (tx, mut rx) = tokio::sync::mpsc::channel(64);
        let ctx = InstallContext {
            plan,
            extension_bytes: vec![],
            fts_extensions: crate::bundled_extensions(),
            launcher_bin: crate::find_launcher_bin(),
            selected_profiles: installer_core::profiles::ALL_PROFILES
                .iter()
                .map(|p| p.id.to_string())
                .collect(),
        };

        let handle = tokio::spawn(async move {
            installer_core::run_all_steps(ctx, tx).await
        });

        // Print progress events to stdout.
        while let Some(event) = rx.recv().await {
            match &event {
                InstallEvent::StepStarted { label, .. } => {
                    println!("  [{label}] started");
                }
                InstallEvent::StepProgress { step: _, fraction, message } => {
                    if !message.is_empty() {
                        println!("  ... {message} ({:.0}%)", fraction * 100.0);
                    }
                }
                InstallEvent::StepCompleted(step) => {
                    println!("  [{}] done", step.label());
                }
                InstallEvent::StepFailed { step, error } => {
                    eprintln!("  [{}] FAILED: {error}", step.label());
                }
                InstallEvent::AllCompleted => {
                    println!("Installation complete.");
                }
            }
        }

        match handle.await.expect("install task panicked") {
            Ok(()) => std::process::exit(0),
            Err(e) => {
                eprintln!("Installation failed: {e:#}");
                std::process::exit(1);
            }
        }
    });
}

/// Find the fts-bundle directory inside the installer .app bundle.
///
/// Layout: FtsInstaller.app/Contents/Resources/fts-bundle/
///   ├── reaper-launcher
///   ├── extensions/
///   │   ├── sync-extension
///   │   └── signal-extension
///   ├── fx/
///   │   └── FTS Signal Controller.clap
///   └── daw-bridge/
///       └── libreaper_daw_bridge.dylib
fn fts_bundle_dir() -> Option<PathBuf> {
    let exe = std::env::current_exe().ok()?;
    // Inside .app/Contents/MacOS/fts-installer
    let resources = exe.parent()?.parent()?.join("Resources/fts-bundle");
    if resources.exists() {
        return Some(resources);
    }
    // Dev builds: check cargo workspace
    for candidate in ["target/release", "../target/release", "../../target/release"] {
        let p = PathBuf::from(candidate);
        if p.join("reaper-launcher").exists() {
            return Some(p);
        }
    }
    None
}

/// Find the reaper-launcher binary.
pub fn find_launcher_bin() -> Option<PathBuf> {
    let bundle = fts_bundle_dir()?;
    let launcher = bundle.join("reaper-launcher");
    if launcher.exists() {
        Some(launcher)
    } else {
        None
    }
}

/// Discover FTS extensions bundled in the installer .app.
pub fn bundled_extensions() -> Vec<installer_core::steps::install_fts_extensions::BundledExtension> {
    // We can't use include_bytes! for runtime-discovered files, so we read them
    // from disk and leak the memory (installer is short-lived, this is fine).
    let bundle = match fts_bundle_dir() {
        Some(b) => b,
        None => return vec![],
    };

    let mut exts = Vec::new();

    // FTS extensions → UserPlugins/fts-extensions/
    let ext_dir = bundle.join("extensions");
    if ext_dir.exists() {
        if let Ok(entries) = std::fs::read_dir(&ext_dir) {
            for entry in entries.flatten() {
                if entry.file_type().map(|t| t.is_file()).unwrap_or(false) {
                    let name = entry.file_name().to_string_lossy().to_string();
                    if let Ok(data) = std::fs::read(entry.path()) {
                        let rel_path = format!("UserPlugins/fts-extensions/{name}");
                        // Leak the strings/data so they have 'static lifetime
                        let rel: &'static str = Box::leak(rel_path.into_boxed_str());
                        let bytes: &'static [u8] = Box::leak(data.into_boxed_slice());
                        exts.push(installer_core::BundledExtension {
                            rel_path: rel,
                            data: bytes,
                        });
                    }
                }
            }
        }
    }

    // CLAP plugins → UserPlugins/FX/FTS/
    let fx_dir = bundle.join("fx");
    if fx_dir.exists() {
        if let Ok(entries) = std::fs::read_dir(&fx_dir) {
            for entry in entries.flatten() {
                let name = entry.file_name().to_string_lossy().to_string();
                if name.ends_with(".clap") {
                    // CLAP plugins are directories — copy as a single file marker for now
                    // TODO: handle .clap bundle directories properly
                }
                if entry.file_type().map(|t| t.is_file()).unwrap_or(false) {
                    if let Ok(data) = std::fs::read(entry.path()) {
                        let rel_path = format!("UserPlugins/FX/FTS/{name}");
                        let rel: &'static str = Box::leak(rel_path.into_boxed_str());
                        let bytes: &'static [u8] = Box::leak(data.into_boxed_slice());
                        exts.push(installer_core::BundledExtension {
                            rel_path: rel,
                            data: bytes,
                        });
                    }
                }
            }
        }
    }

    // daw-bridge → UserPlugins/ (directly, rename lib prefix to reaper_ for REAPER to load it)
    let bridge_dir = bundle.join("daw-bridge");
    if bridge_dir.exists() {
        if let Ok(entries) = std::fs::read_dir(&bridge_dir) {
            for entry in entries.flatten() {
                if entry.file_type().map(|t| t.is_file()).unwrap_or(false) {
                    let name = entry.file_name().to_string_lossy().to_string();
                    // Rename libreaper_* → reaper_* (REAPER expects reaper_ prefix)
                    let name = if let Some(rest) = name.strip_prefix("lib") {
                        rest.to_string()
                    } else {
                        name
                    };
                    if let Ok(data) = std::fs::read(entry.path()) {
                        let rel_path = format!("UserPlugins/{name}");
                        let rel: &'static str = Box::leak(rel_path.into_boxed_str());
                        let bytes: &'static [u8] = Box::leak(data.into_boxed_slice());
                        exts.push(installer_core::BundledExtension {
                            rel_path: rel,
                            data: bytes,
                        });
                    }
                }
            }
        }
    }

    exts
}
