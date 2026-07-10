//! Download / verify / extract primitives, built on installer-core's
//! retry helper and the same reqwest (rustls, streaming) stack.

use std::path::Path;
use std::time::Duration;

use eyre::{Context, eyre};
use futures::StreamExt;
use installer_core::retry::with_retry;
use sha2::{Digest, Sha256};

pub fn http_client() -> eyre::Result<reqwest::Client> {
    reqwest::Client::builder()
        .user_agent(concat!("fts-installer/", env!("CARGO_PKG_VERSION")))
        .timeout(Duration::from_secs(600))
        .connect_timeout(Duration::from_secs(15))
        .build()
        .wrap_err("failed to create HTTP client")
}

/// Download `url` to `dest`, streaming with a progress line on stderr.
/// Retries up to 3 times with exponential backoff.
pub async fn download(
    client: &reqwest::Client,
    url: &str,
    dest: &Path,
    label: &str,
) -> eyre::Result<()> {
    with_retry(label, 3, Duration::from_secs(1), || async {
        let resp = client
            .get(url)
            .send()
            .await
            .wrap_err_with(|| format!("connecting to {url}"))?;
        if !resp.status().is_success() {
            eyre::bail!("{url} -> HTTP {}", resp.status());
        }
        let total = resp.content_length().unwrap_or(0);

        let mut file = tokio::fs::File::create(dest)
            .await
            .wrap_err_with(|| format!("creating {}", dest.display()))?;

        use tokio::io::AsyncWriteExt;
        let mut stream = resp.bytes_stream();
        let mut downloaded: u64 = 0;
        while let Some(chunk) = stream.next().await {
            let chunk = chunk.wrap_err("download interrupted")?;
            file.write_all(&chunk).await?;
            downloaded += chunk.len() as u64;
            let mb = downloaded as f64 / 1_048_576.0;
            if total > 0 {
                let total_mb = total as f64 / 1_048_576.0;
                let pct = 100.0 * downloaded as f64 / total as f64;
                eprint!("\r  {label}: {mb:.1} / {total_mb:.1} MB ({pct:.0}%)   ");
            } else {
                eprint!("\r  {label}: {mb:.1} MB   ");
            }
        }
        file.flush().await?;
        eprintln!();
        Ok(())
    })
    .await
}

/// Fetch a small text resource (e.g. SHA256SUMS).
pub async fn fetch_text(client: &reqwest::Client, url: &str) -> eyre::Result<String> {
    let resp = client.get(url).send().await.wrap_err_with(|| format!("requesting {url}"))?;
    if !resp.status().is_success() {
        eyre::bail!("{url} -> HTTP {}", resp.status());
    }
    resp.text().await.wrap_err("reading response body")
}

/// Verify `path` against the `sha256sum`-format `sums` text (lines of
/// `<hex>  <filename>`), matching on `filename`.
pub fn verify_sha256(path: &Path, filename: &str, sums: &str) -> eyre::Result<()> {
    let expected = sums
        .lines()
        .filter_map(|line| {
            let mut parts = line.split_whitespace();
            let hash = parts.next()?;
            let name = parts.next()?;
            Some((hash, name.trim_start_matches('*')))
        })
        .find(|(_, name)| *name == filename)
        .map(|(hash, _)| hash.to_ascii_lowercase())
        .ok_or_else(|| eyre!("SHA256SUMS has no entry for {filename}"))?;

    let mut hasher = Sha256::new();
    let mut file = std::fs::File::open(path).wrap_err_with(|| format!("opening {}", path.display()))?;
    std::io::copy(&mut file, &mut hasher)?;
    let actual = format!("{:x}", hasher.finalize());

    if actual != expected {
        eyre::bail!(
            "sha256 mismatch for {filename}:\n  expected {expected}\n  actual   {actual}"
        );
    }
    Ok(())
}

/// Extract a .tar.gz into `dest` (created fresh).
pub fn extract_tarball(tarball: &Path, dest: &Path) -> eyre::Result<()> {
    if dest.exists() {
        std::fs::remove_dir_all(dest)?;
    }
    std::fs::create_dir_all(dest)?;
    let file = std::fs::File::open(tarball).wrap_err_with(|| format!("opening {}", tarball.display()))?;
    let gz = flate2::read::GzDecoder::new(std::io::BufReader::new(file));
    tar::Archive::new(gz)
        .unpack(dest)
        .wrap_err_with(|| format!("unpacking into {}", dest.display()))?;
    Ok(())
}
