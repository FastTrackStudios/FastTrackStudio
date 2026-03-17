//! Download REAPER from reaper.fm with streaming progress.

use std::path::{Path, PathBuf};

use eyre::Context;
use futures::StreamExt;
use tracing::info;

use crate::progress::{EventSender, InstallEvent, InstallStep};
use crate::plan::InstallPlan;

/// Download REAPER to a temp directory. Returns the path to the downloaded DMG.
///
/// Skips download if a file of the correct size already exists.
pub async fn download_reaper(plan: &InstallPlan, tx: &EventSender) -> eyre::Result<PathBuf> {
    let url = plan.reaper_download_url();
    let download_dir = std::env::temp_dir().join("fts-installer");
    tokio::fs::create_dir_all(&download_dir).await?;

    let filename = url.rsplit('/').next().unwrap_or("reaper.dmg");
    let dest = download_dir.join(filename);

    info!("Downloading REAPER from {url}");
    let _ = tx.send(InstallEvent::StepProgress {
        step: InstallStep::DownloadReaper,
        fraction: 0.0,
        message: format!("Downloading {filename}..."),
    }).await;

    let client = reqwest::Client::new();
    let response = client.get(&url).send().await
        .wrap_err("Failed to connect to reaper.fm")?;

    if !response.status().is_success() {
        eyre::bail!("Download failed: HTTP {}", response.status());
    }

    let total_size = response.content_length().unwrap_or(0);

    // Skip if already downloaded
    if dest.exists() {
        if let Ok(meta) = tokio::fs::metadata(&dest).await {
            if total_size > 0 && meta.len() == total_size {
                info!("REAPER already downloaded at {}", dest.display());
                let _ = tx.send(InstallEvent::StepProgress {
                    step: InstallStep::DownloadReaper,
                    fraction: 1.0,
                    message: "Already downloaded".into(),
                }).await;
                return Ok(dest);
            }
        }
    }

    let mut file = tokio::fs::File::create(&dest).await
        .wrap_err_with(|| format!("Failed to create {}", dest.display()))?;

    let mut stream = response.bytes_stream();
    let mut downloaded: u64 = 0;

    use tokio::io::AsyncWriteExt;
    while let Some(chunk) = stream.next().await {
        let chunk = chunk.wrap_err("Download interrupted")?;
        file.write_all(&chunk).await?;
        downloaded += chunk.len() as u64;

        if total_size > 0 {
            let fraction = downloaded as f32 / total_size as f32;
            let mb = downloaded as f32 / 1_048_576.0;
            let total_mb = total_size as f32 / 1_048_576.0;
            let _ = tx.send(InstallEvent::StepProgress {
                step: InstallStep::DownloadReaper,
                fraction,
                message: format!("{mb:.1} / {total_mb:.1} MB"),
            }).await;
        }
    }

    file.flush().await?;
    info!("Downloaded {} bytes to {}", downloaded, dest.display());

    Ok(dest)
}
