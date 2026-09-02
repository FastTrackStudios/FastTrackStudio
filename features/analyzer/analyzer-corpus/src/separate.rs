//! Splitting each song into a vocal and an instrumental stem.
//!
//! Demucs does the separation; ffmpeg encodes the result. Both stems are
//! kept, because the questions this corpus is for are not only about the
//! vocal in isolation — "where does the vocal stand clear of the track
//! and where is it buried" needs the instrumental as audio, not as a
//! summary number.
//!
//! ## Two stems, not four
//!
//! `--two-stems=vocals` yields `vocals` and `no_vocals`. Drums, bass and
//! other are not wanted, and asking for them would triple the storage
//! for material nothing downstream reads.
//!
//! ## Why Opus 128k
//!
//! Lossless FLAC for both stems across the corpus is ~443 GB, which does
//! not fit comfortably. Opus 128k is ~99 GB. Measured over twelve stems,
//! 128k shifts crest factor by 0.12 dB on average and the averaged
//! spectrum by 0.08 dB — both far below the effects being studied,
//! though its worst case (0.81 dB on one stem) is why anything needing
//! exactness should be measured before encoding rather than after.
//!
//! ## Demucs exits 0 when it fails
//!
//! Handed audio it cannot decode, demucs prints a `LoadAudioError` and
//! **still exits successfully**. A batch run trusting the exit code
//! would silently produce nothing for thousands of songs and report
//! success. Every song is therefore confirmed by the presence of its
//! output files, never by the exit status. (It needs ffmpeg on PATH to
//! read the corpus's Opus/WebM downloads at all.)

use std::path::{Path, PathBuf};
use std::process::Stdio;

use anyhow::{Context, Result, bail};
use tokio::process::Command;

/// The separation model.
///
/// `htdemucs` rather than `htdemucs_ft`: the fine-tuned variant is a bag
/// of four models and roughly four times slower, which is the
/// difference between a one-day run and a four-day one over this
/// corpus. Quality is close enough that the difference is well inside
/// the spread being measured — but a subset should be cross-checked
/// against `htdemucs_ft` before conclusions are drawn, the way the
/// reference study cross-checked two separators against each other.
pub const DEFAULT_MODEL: &str = "htdemucs";

/// Bitrate for the archived stems, in kbit/s.
pub const DEFAULT_BITRATE_K: u32 = 128;

/// External tools this stage drives.
#[derive(Debug, Clone)]
pub struct Tools {
    /// `uv`, used to run demucs in its own environment.
    pub uv: PathBuf,
    pub ffmpeg: PathBuf,
    /// Prepended to `LD_LIBRARY_PATH` for the demucs child.
    ///
    /// On NixOS torch cannot see the GPU without the driver's library
    /// directory on the path: `torch.cuda.is_available()` returns False
    /// and everything silently runs on CPU, which turns a one-day run
    /// into a multi-week one. Nothing errors — it is just slow.
    pub driver_lib: Option<PathBuf>,
}

impl Tools {
    pub fn discover() -> Result<Tools> {
        let find = |env: &str, exe: &str| -> Result<PathBuf> {
            if let Ok(p) = std::env::var(env) {
                return Ok(PathBuf::from(p));
            }
            which(exe)
                .with_context(|| format!("{exe} not found on PATH — run inside `nix develop`, or set ${env}"))
        };
        let driver = PathBuf::from("/run/opengl-driver/lib");
        Ok(Tools {
            uv: find("UV", "uv")?,
            ffmpeg: find("FFMPEG", "ffmpeg")?,
            driver_lib: driver.is_dir().then_some(driver),
        })
    }

    fn ld_library_path(&self) -> String {
        let existing = std::env::var("LD_LIBRARY_PATH").unwrap_or_default();
        match &self.driver_lib {
            Some(d) if existing.is_empty() => d.display().to_string(),
            Some(d) => format!("{}:{existing}", d.display()),
            None => existing,
        }
    }

    /// `PATH` for the demucs child, with ffmpeg's directory prepended.
    ///
    /// demucs shells out to ffmpeg to decode anything its built-in
    /// loader cannot read — which includes every file in this corpus,
    /// since they are Opus in WebM. Knowing where ffmpeg lives is not
    /// enough: the child looks it up on its own PATH, so it has to be
    /// put there explicitly.
    fn child_path(&self) -> String {
        let existing = std::env::var("PATH").unwrap_or_default();
        match self.ffmpeg.parent() {
            Some(d) if !existing.is_empty() => format!("{}:{existing}", d.display()),
            Some(d) => d.display().to_string(),
            None => existing,
        }
    }
}

fn which(exe: &str) -> Result<PathBuf> {
    let path = std::env::var_os("PATH").context("PATH is unset")?;
    std::env::split_paths(&path)
        .map(|d| d.join(exe))
        .find(|p| p.is_file())
        .with_context(|| format!("{exe} not on PATH"))
}

/// One song's separated stems, on disk.
#[derive(Debug, Clone)]
pub struct Stems {
    pub vocal: PathBuf,
    pub instrumental: PathBuf,
    pub vocal_bytes: u64,
    pub instrumental_bytes: u64,
}

/// Run demucs over a batch of source files into `out_dir`.
///
/// Batched because loading the model costs a couple of seconds; one
/// invocation per song would spend more time loading weights than
/// separating. Returns the directory demucs wrote into.
pub async fn separate_batch(
    tools: &Tools,
    model: &str,
    sources: &[PathBuf],
    out_dir: &Path,
    device: &str,
) -> Result<PathBuf> {
    if sources.is_empty() {
        bail!("no sources given");
    }
    tokio::fs::create_dir_all(out_dir).await?;

    let mut cmd = Command::new(&tools.uv);
    cmd.args([
        "run", "--quiet", "--with", "demucs", "--with", "numpy", "python", "-m", "demucs",
    ])
    .args(["-n", model])
    .args(["--two-stems", "vocals"])
    // FLAC out of demucs, transcoded after. WAV would be ~5x the
    // temporary disk for no benefit; both are lossless.
    .arg("--flac")
    .args(["-d", device])
    .arg("-o")
    .arg(out_dir);
    for s in sources {
        cmd.arg(s);
    }

    let out = cmd
        .env("LD_LIBRARY_PATH", tools.ld_library_path())
        .env("PATH", tools.child_path())
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .output()
        .await
        .context("running demucs")?;

    // The exit status is NOT evidence of success here — see the module
    // docs. It is only consulted to produce a better error message.
    if !out.status.success() {
        let tail: String = String::from_utf8_lossy(&out.stderr)
            .lines()
            .rev()
            .take(3)
            .collect::<Vec<_>>()
            .join(" | ");
        bail!("demucs exited {}: {tail}", out.status);
    }

    Ok(out_dir.join(model))
}

/// Encode one song's two stems to Opus and place them under `dest`.
///
/// Fails if demucs did not actually write the stems, which is how a
/// silent decode failure is caught.
pub async fn encode_stems(
    tools: &Tools,
    separated: &Path,
    stem_name: &str,
    dest: &Path,
    bitrate_k: u32,
) -> Result<Stems> {
    let src_dir = separated.join(stem_name);
    let vocal_src = src_dir.join("vocals.flac");
    let instr_src = src_dir.join("no_vocals.flac");

    for (p, what) in [(&vocal_src, "vocals"), (&instr_src, "no_vocals")] {
        if !p.is_file() {
            bail!(
                "demucs produced no {what} for {stem_name} — it reports success even when it \
                 cannot decode the input, so this is checked rather than assumed"
            );
        }
    }

    tokio::fs::create_dir_all(dest).await?;
    let vocal_out = dest.join("vocals.opus");
    let instr_out = dest.join("instrumental.opus");

    for (src, out) in [(&vocal_src, &vocal_out), (&instr_src, &instr_out)] {
        let status = Command::new(&tools.ffmpeg)
            .args(["-v", "error", "-y", "-i"])
            .arg(src)
            .args(["-c:a", "libopus", "-b:a", &format!("{bitrate_k}k")])
            .arg(out)
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .await
            .context("running ffmpeg to encode a stem")?;
        if !status.success() {
            bail!("ffmpeg failed encoding {}", src.display());
        }
    }

    Ok(Stems {
        vocal_bytes: tokio::fs::metadata(&vocal_out).await?.len(),
        instrumental_bytes: tokio::fs::metadata(&instr_out).await?.len(),
        vocal: vocal_out,
        instrumental: instr_out,
    })
}

/// The name demucs derives its output subdirectory from.
pub fn stem_name(source: &Path) -> Option<String> {
    source
        .file_stem()
        .and_then(|s| s.to_str())
        .map(|s| s.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn output_dir_is_named_after_the_source_file() {
        assert_eq!(
            stem_name(Path::new("/x/audio/4/GIBk1JRhbJs.webm")).as_deref(),
            Some("GIBk1JRhbJs")
        );
    }

    #[test]
    fn ffmpeg_is_put_on_the_child_path() {
        // demucs cannot decode this corpus at all without it, and it
        // looks ffmpeg up on its own PATH.
        let t = Tools {
            uv: "uv".into(),
            ffmpeg: PathBuf::from("/nix/store/xyz-ffmpeg/bin/ffmpeg"),
            driver_lib: None,
        };
        assert!(
            t.child_path().starts_with("/nix/store/xyz-ffmpeg/bin"),
            "got {}",
            t.child_path()
        );
    }

    #[test]
    fn driver_lib_is_prepended_not_replaced() {
        // Dropping an existing LD_LIBRARY_PATH would break the PyPI
        // wheels' own bundled libraries.
        let t = Tools {
            uv: "uv".into(),
            ffmpeg: "ffmpeg".into(),
            driver_lib: Some(PathBuf::from("/run/opengl-driver/lib")),
        };
        let got = t.ld_library_path();
        assert!(got.starts_with("/run/opengl-driver/lib"), "got {got}");
    }
}
