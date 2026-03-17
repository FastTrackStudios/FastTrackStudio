//! Top-level orchestrator — runs all install steps in sequence.

use tracing::info;

use crate::plan::InstallPlan;
use crate::progress::{EventSender, InstallEvent, InstallStep};
use crate::steps;

/// Context for a running installation. The installer app populates this
/// with the extension binary before calling `run_all_steps`.
pub struct InstallContext {
    pub plan: InstallPlan,
    /// FTS REAPER extension bytes (e.g. from `include_bytes!`).
    /// Empty if not bundled (extension step will be skipped).
    pub extension_bytes: Vec<u8>,
}

/// Run every installation step in order. Sends progress events via `tx`.
/// Stops on the first error.
pub async fn run_all_steps(ctx: InstallContext, tx: EventSender) -> eyre::Result<()> {
    let plan = &ctx.plan;

    info!("Starting installation to {}", plan.install_root.display());
    tokio::fs::create_dir_all(&plan.install_root).await?;

    // 1. Download REAPER
    send_started(&tx, InstallStep::DownloadReaper).await;
    let dmg_path = match steps::download_reaper::download_reaper(plan, &tx).await {
        Ok(p) => { send_completed(&tx, InstallStep::DownloadReaper).await; p }
        Err(e) => { send_failed(&tx, InstallStep::DownloadReaper, &e).await; return Err(e); }
    };

    // 2. Extract DMG
    send_started(&tx, InstallStep::ExtractDmg).await;
    match steps::extract_dmg::extract_dmg(&dmg_path, &plan.reaper_dir(), &tx).await {
        Ok(()) => send_completed(&tx, InstallStep::ExtractDmg).await,
        Err(e) => { send_failed(&tx, InstallStep::ExtractDmg, &e).await; return Err(e); }
    }

    // 3. Copy extension
    send_started(&tx, InstallStep::CopyExtension).await;
    if ctx.extension_bytes.is_empty() {
        let _ = tx.send(InstallEvent::StepProgress {
            step: InstallStep::CopyExtension,
            fraction: 1.0,
            message: "No extension bundled, skipping".into(),
        }).await;
        send_completed(&tx, InstallStep::CopyExtension).await;
    } else {
        match steps::copy_extension::copy_extension(&ctx.extension_bytes, &plan.reaper_dir(), &tx).await {
            Ok(()) => send_completed(&tx, InstallStep::CopyExtension).await,
            Err(e) => { send_failed(&tx, InstallStep::CopyExtension, &e).await; return Err(e); }
        }
    }

    // 4. Download & extract library (presets, FXChains, TrackTemplates)
    send_started(&tx, InstallStep::DownloadLibrary).await;
    match steps::download_library::download_library(&plan.library_url, &plan.install_root, &tx).await {
        Ok(()) => send_completed(&tx, InstallStep::DownloadLibrary).await,
        Err(e) => { send_failed(&tx, InstallStep::DownloadLibrary, &e).await; return Err(e); }
    }

    // 5. Write reaper.ini
    send_started(&tx, InstallStep::WriteReaperIni).await;
    match steps::write_reaper_ini::write_reaper_ini(&plan.reaper_dir(), &tx).await {
        Ok(()) => send_completed(&tx, InstallStep::WriteReaperIni).await,
        Err(e) => { send_failed(&tx, InstallStep::WriteReaperIni, &e).await; return Err(e); }
    }

    // 6. Install FTS-Control.app
    send_started(&tx, InstallStep::InstallFtsControl).await;
    if let Some(ref source) = plan.fts_control_source {
        match steps::install_fts_control::install_fts_control(source, &plan.install_root, &tx).await {
            Ok(()) => send_completed(&tx, InstallStep::InstallFtsControl).await,
            Err(e) => { send_failed(&tx, InstallStep::InstallFtsControl, &e).await; return Err(e); }
        }
    } else {
        let _ = tx.send(InstallEvent::StepProgress {
            step: InstallStep::InstallFtsControl,
            fraction: 1.0,
            message: "FTS-Control.app not found alongside installer, skipping".into(),
        }).await;
        send_completed(&tx, InstallStep::InstallFtsControl).await;
    }

    // 7. Setup shell PATH
    send_started(&tx, InstallStep::SetupShell).await;
    match steps::setup_shell::setup_shell(&plan.install_root, &tx).await {
        Ok(()) => send_completed(&tx, InstallStep::SetupShell).await,
        Err(e) => { send_failed(&tx, InstallStep::SetupShell, &e).await; return Err(e); }
    }

    let _ = tx.send(InstallEvent::AllCompleted).await;
    info!("Installation complete");
    Ok(())
}

async fn send_started(tx: &EventSender, step: InstallStep) {
    let _ = tx.send(InstallEvent::StepStarted {
        step,
        label: step.label().to_string(),
    }).await;
}

async fn send_completed(tx: &EventSender, step: InstallStep) {
    let _ = tx.send(InstallEvent::StepCompleted(step)).await;
}

async fn send_failed(tx: &EventSender, step: InstallStep, error: &eyre::Error) {
    let _ = tx.send(InstallEvent::StepFailed {
        step,
        error: format!("{error:#}"),
    }).await;
}
