//! Getting one audio rendition per charting song.
//!
//! Three steps per song, each of which can fail independently and is
//! recorded so the run is resumable and auditable:
//!
//! 1. **resolve** — ask YouTube Music for candidates and pick the one
//!    that is actually the charting master ([`score`]).
//! 2. **download** — fetch the best audio stream, without re-encoding.
//! 3. **probe** — read back what actually landed on disk with ffprobe,
//!    and check it against what was promised.
//!
//! ## Why YouTube Music rather than YouTube
//!
//! Plain search ranks by engagement, so it surfaces reactions, karaoke,
//! covers and lyric videos ahead of the record — measured on a real
//! chart-topper, twelve plain results contained no original master at
//! all. YouTube Music returns the label's own "art tracks", and yt-dlp
//! reports their `artist` / `track` / `release_year` metadata, which is
//! exactly what [`score`] needs to tell an original from a
//! re-recording.
//!
//! ## Why nothing is re-encoded
//!
//! The whole study is a measurement of level and spectrum. Every
//! transcode is a generation of loss applied unevenly across the
//! corpus, so the downloaded stream is stored exactly as served and
//! decoded only at analysis time.

pub mod score;

use std::path::{Path, PathBuf};
use std::process::Stdio;

use anyhow::{Context, Result, bail};
use tokio::process::Command;

pub use score::{Candidate, Score, Target};

/// External binaries this stage shells out to.
///
/// Resolved once so a long run does not pay PATH lookup per song, and
/// so a missing tool fails at startup rather than 4,000 songs in.
#[derive(Debug, Clone)]
pub struct Tools {
    pub yt_dlp: PathBuf,
    pub ffprobe: PathBuf,
}

impl Tools {
    /// Find the tools on `PATH`, honouring `YT_DLP` / `FFPROBE`
    /// overrides.
    pub fn discover() -> Result<Tools> {
        let find = |env: &str, exe: &str| -> Result<PathBuf> {
            if let Ok(p) = std::env::var(env) {
                return Ok(PathBuf::from(p));
            }
            which(exe).with_context(|| {
                format!("{exe} not found on PATH — enter the dev shell (`nix develop`) or set ${env}")
            })
        };
        Ok(Tools {
            yt_dlp: find("YT_DLP", "yt-dlp")?,
            ffprobe: find("FFPROBE", "ffprobe")?,
        })
    }
}

fn which(exe: &str) -> Result<PathBuf> {
    let path = std::env::var_os("PATH").context("PATH is unset")?;
    std::env::split_paths(&path)
        .map(|d| d.join(exe))
        .find(|p| p.is_file())
        .with_context(|| format!("{exe} not on PATH"))
}

/// Ask YouTube Music for candidate uploads of a song.
pub async fn resolve(tools: &Tools, target: &Target, limit: usize) -> Result<Vec<Candidate>> {
    let query = format!("{} {}", target.artist, target.title);
    let url = format!(
        "https://music.youtube.com/search?q={}",
        urlencode(&query)
    );

    let out = Command::new(&tools.yt_dlp)
        .args([
            "--dump-json",
            "--skip-download",
            "--no-warnings",
            "--ignore-errors",
            "--playlist-end",
            &limit.to_string(),
        ])
        .arg(&url)
        .stdin(Stdio::null())
        .stderr(Stdio::null())
        .output()
        .await
        .context("running yt-dlp to resolve candidates")?;

    // yt-dlp exits non-zero when *some* entries fail, which is normal
    // here; the JSON lines it did produce are still good.
    let mut cands = Vec::new();
    for line in String::from_utf8_lossy(&out.stdout).lines() {
        let line = line.trim();
        if !line.starts_with('{') {
            continue;
        }
        match serde_json::from_str::<Candidate>(line) {
            Ok(c) => cands.push(c),
            Err(e) => tracing::debug!(error = %e, "skipping unparseable yt-dlp record"),
        }
    }
    Ok(cands)
}

/// What landed on disk.
#[derive(Debug, Clone)]
pub struct Rendition {
    pub path: PathBuf,
    pub bytes: u64,
    pub duration_s: f64,
    pub codec: String,
    pub sample_rate: u32,
    pub channels: u32,
}

/// YouTube player clients to try, in order.
///
/// Resolving metadata is not the same as being allowed to fetch the
/// media: the default client resolves fine and then answers **HTTP 403**
/// on the audio stream itself. Measured against a real track, only
/// `web_embedded` served the audio — `tv` demanded a reload, and
/// `web`, `web_safari`, `ios`, `mweb` and `android` all reported the
/// format as unavailable.
///
/// It is a list rather than a constant because which clients work is a
/// property of YouTube's bot defences on any given week, not of this
/// code. When the corpus starts failing wholesale at the download step,
/// re-test the clients and reorder this.
pub const PLAYER_CLIENTS: &[&str] = &["web_embedded", "tv", "web_safari", "android_vr"];

/// Download a candidate's best audio stream into `dir`.
pub async fn download(tools: &Tools, video_id: &str, dir: &Path) -> Result<PathBuf> {
    tokio::fs::create_dir_all(dir)
        .await
        .with_context(|| format!("creating {}", dir.display()))?;

    let template = dir.join("%(id)s.%(ext)s");
    let mut last_err = String::new();

    for client in PLAYER_CLIENTS {
        let out = Command::new(&tools.yt_dlp)
            .args([
                "--format",
                // Best audio-only stream. No `--extract-audio`, no
                // `--recode`: re-encoding would add a generation of loss
                // to a corpus whose whole purpose is measuring level and
                // spectrum.
                "bestaudio",
                "--no-playlist",
                "--no-warnings",
                "--no-progress",
                "--retries",
                "3",
                "--extractor-args",
            ])
            .arg(format!("youtube:player_client={client}"))
            .arg("-o")
            .arg(&template)
            .arg(format!("https://music.youtube.com/watch?v={video_id}"))
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            // Captured, not discarded: swallowing this turned a plain
            // "HTTP 403" into an opaque "exited with status 1" and cost
            // an hour of guessing.
            .stderr(Stdio::piped())
            .output()
            .await
            .context("running yt-dlp to download")?;

        if out.status.success() {
            last_err.clear();
            break;
        }

        last_err = String::from_utf8_lossy(&out.stderr)
            .lines()
            .find(|l| l.contains("ERROR"))
            .unwrap_or("no error line")
            .trim()
            .to_string();
        tracing::debug!(video_id, client, error = %last_err, "player client failed, trying next");
    }

    if !last_err.is_empty() {
        bail!("every player client failed; last: {last_err}");
    }

    // The extension depends on which stream was served, so find what
    // actually appeared rather than assuming.
    let mut entries = tokio::fs::read_dir(dir).await?;
    while let Some(e) = entries.next_entry().await? {
        let p = e.path();
        if p.file_stem().and_then(|s| s.to_str()) == Some(video_id) {
            return Ok(p);
        }
    }
    bail!("yt-dlp reported success but produced no file for {video_id}")
}

/// Shortest audio we will accept as a whole song.
///
/// Guards against the failure that would be invisible in the numbers: a
/// preview clip or a truncated download measured as if it were the
/// record. A 30-second excerpt has a different crest factor from the
/// track it came from — it is usually a chorus, the loudest and most
/// compressed part — so it would bias the whole study downward while
/// looking like perfectly ordinary data.
pub const MIN_FULL_SONG_S: f64 = 60.0;

/// How far the delivered audio may drift from the length promised in
/// the search metadata before it is treated as truncated.
pub const DURATION_TOLERANCE: f64 = 0.05;

/// Check that what landed is the whole song.
pub fn check_complete(got: &Rendition, expected_s: Option<f64>) -> Result<()> {
    if got.duration_s < MIN_FULL_SONG_S {
        bail!(
            "only {:.0}s of audio — a clip or a truncated download, not the full song",
            got.duration_s
        );
    }
    if let Some(expected) = expected_s {
        let drift = (got.duration_s - expected).abs() / expected.max(1.0);
        if drift > DURATION_TOLERANCE {
            bail!(
                "got {:.0}s but the listing promised {:.0}s ({:.0}% off) — likely truncated",
                got.duration_s,
                expected,
                drift * 100.0
            );
        }
    }
    Ok(())
}

/// Read back what is actually in the file.
///
/// The download is never trusted on its own: a truncated or
/// wrong-length file would otherwise be measured as if it were the
/// record.
pub async fn probe(tools: &Tools, path: &Path) -> Result<Rendition> {
    let out = Command::new(&tools.ffprobe)
        .args([
            "-v", "quiet",
            "-print_format", "json",
            "-show_format",
            "-show_streams",
            "-select_streams", "a:0",
        ])
        .arg(path)
        .stdin(Stdio::null())
        .output()
        .await
        .context("running ffprobe")?;

    let v: serde_json::Value =
        serde_json::from_slice(&out.stdout).context("parsing ffprobe JSON")?;

    let stream = v["streams"].get(0).context("no audio stream in file")?;
    let format = &v["format"];

    let duration_s = format["duration"]
        .as_str()
        .and_then(|s| s.parse::<f64>().ok())
        .or_else(|| stream["duration"].as_str().and_then(|s| s.parse().ok()))
        .context("ffprobe reported no duration")?;

    Ok(Rendition {
        path: path.to_path_buf(),
        bytes: tokio::fs::metadata(path).await?.len(),
        duration_s,
        codec: stream["codec_name"].as_str().unwrap_or("unknown").to_string(),
        sample_rate: stream["sample_rate"]
            .as_str()
            .and_then(|s| s.parse().ok())
            .unwrap_or(0),
        channels: stream["channels"].as_u64().unwrap_or(0) as u32,
    })
}

/// Percent-encode a search query.
fn urlencode(s: &str) -> String {
    let mut out = String::with_capacity(s.len() * 3);
    for b in s.bytes() {
        match b {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' => {
                out.push(b as char)
            }
            b' ' => out.push('+'),
            _ => out.push_str(&format!("%{b:02X}")),
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rendition(duration_s: f64) -> Rendition {
        Rendition {
            path: PathBuf::from("x.opus"),
            bytes: 1,
            duration_s,
            codec: "opus".into(),
            sample_rate: 48_000,
            channels: 2,
        }
    }

    #[test]
    fn a_full_song_passes() {
        assert!(check_complete(&rendition(289.0), Some(289.0)).is_ok());
        // Container rounding is not truncation.
        assert!(check_complete(&rendition(288.0), Some(289.0)).is_ok());
    }

    #[test]
    fn a_thirty_second_preview_is_rejected() {
        let err = check_complete(&rendition(30.0), Some(289.0)).unwrap_err();
        assert!(err.to_string().contains("not the full song"), "{err}");
    }

    #[test]
    fn a_download_that_stopped_early_is_rejected() {
        // Long enough to pass the floor, but nowhere near the listing.
        let err = check_complete(&rendition(120.0), Some(289.0)).unwrap_err();
        assert!(err.to_string().contains("truncated"), "{err}");
    }

    #[test]
    fn without_an_expected_length_only_the_floor_applies() {
        assert!(check_complete(&rendition(289.0), None).is_ok());
        assert!(check_complete(&rendition(30.0), None).is_err());
    }

    #[test]
    fn queries_are_encoded_safely() {
        assert_eq!(urlencode("Bryan Adams"), "Bryan+Adams");
        // The characters that actually appear in chart titles.
        assert_eq!(urlencode("Woman?"), "Woman%3F");
        assert_eq!(urlencode("A&B"), "A%26B");
        assert_eq!(urlencode("Despacito"), "Despacito");
        assert_eq!(urlencode("Décimas"), "D%C3%A9cimas");
        assert_eq!(urlencode("hi #1"), "hi+%231");
    }
}
