//! Extract REAPER.app from a downloaded DMG using hdiutil.

use std::path::{Path, PathBuf};

use eyre::Context;
use tracing::info;

use crate::progress::{EventSender, InstallEvent, InstallStep};

/// Mount the DMG, copy REAPER.app to the install dir, then detach.
pub async fn extract_dmg(
    dmg_path: &Path,
    reaper_dir: &Path,
    tx: &EventSender,
) -> eyre::Result<()> {
    let mount_point = tempfile::tempdir()
        .wrap_err("Failed to create temp mount point")?;
    let mount_path = mount_point.path();

    let _ = tx.send(InstallEvent::StepProgress {
        step: InstallStep::ExtractDmg,
        fraction: 0.1,
        message: "Mounting disk image...".into(),
    }).await;

    // Mount DMG
    let status = tokio::process::Command::new("hdiutil")
        .args([
            "attach",
            "-nobrowse",
            "-noverify",
            "-quiet",
            "-mountpoint",
        ])
        .arg(mount_path)
        .arg(dmg_path)
        .status()
        .await
        .wrap_err("Failed to run hdiutil")?;

    if !status.success() {
        eyre::bail!("hdiutil attach failed with status {status}");
    }

    // Find REAPER.app in the mounted volume
    let _ = tx.send(InstallEvent::StepProgress {
        step: InstallStep::ExtractDmg,
        fraction: 0.4,
        message: "Copying REAPER.app...".into(),
    }).await;

    let reaper_app_src = find_reaper_app(mount_path).await?;
    let reaper_app_dst = reaper_dir.join("REAPER.app");

    // Remove existing if present
    if reaper_app_dst.exists() {
        tokio::fs::remove_dir_all(&reaper_app_dst).await
            .wrap_err("Failed to remove existing REAPER.app")?;
    }

    tokio::fs::create_dir_all(reaper_dir).await?;

    // cp -R for .app bundles
    let cp_status = tokio::process::Command::new("cp")
        .args(["-R"])
        .arg(&reaper_app_src)
        .arg(&reaper_app_dst)
        .status()
        .await
        .wrap_err("Failed to copy REAPER.app")?;

    if !cp_status.success() {
        eyre::bail!("Failed to copy REAPER.app");
    }

    info!("Copied REAPER.app to {}", reaper_app_dst.display());

    // Detach DMG
    let _ = tx.send(InstallEvent::StepProgress {
        step: InstallStep::ExtractDmg,
        fraction: 0.9,
        message: "Unmounting disk image...".into(),
    }).await;

    let _ = tokio::process::Command::new("hdiutil")
        .args(["detach", "-quiet"])
        .arg(mount_path)
        .status()
        .await;

    Ok(())
}

/// Find REAPER.app inside the mounted DMG volume.
async fn find_reaper_app(mount_path: &Path) -> eyre::Result<PathBuf> {
    let mut entries = tokio::fs::read_dir(mount_path).await?;
    while let Some(entry) = entries.next_entry().await? {
        let name = entry.file_name();
        let name_str = name.to_string_lossy();
        if name_str.ends_with(".app") && name_str.contains("REAPER") {
            return Ok(entry.path());
        }
    }
    eyre::bail!("REAPER.app not found in mounted DMG at {}", mount_path.display())
}
