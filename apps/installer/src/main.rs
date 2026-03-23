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

fn main() {
    let cli = Cli::parse();

    tracing_subscriber::fmt()
        .with_env_filter("info,installer_core=debug")
        .init();

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
