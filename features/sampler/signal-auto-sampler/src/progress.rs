//! Per-sample resume: remember which cells are already recorded.
//!
//! A patch takes hours, and a batch is interrupted for ordinary reasons —
//! a crash, a laptop being carried away. Resume was per *patch*, keyed on the
//! finished pack, so stopping mid-patch threw away every sample it had
//! recorded. On these long pads that is up to 3.6 h of playing the instrument
//! at itself.
//!
//! The WAVs are already on disk and already final: each was trimmed with its
//! own run's measured latency before being written, so a file recorded in an
//! earlier run is exactly what this run would produce. The only per-sample fact
//! *not* recoverable from the audio is how long the note was held — the sampler
//! releases as soon as a note decays, so the hold varies per cell, and it is
//! what becomes `release_start` and bounds the loop search.
//!
//! So persist that one number beside the audio and a resumed run can reconstruct
//! everything else.
//!
//! # Crash safety
//!
//! The entry is appended *after* the WAV is finalised. A run killed mid-write
//! leaves a truncated WAV with no entry, so it is re-recorded rather than
//! trusted. The ordering is the guarantee — there is no separate validation
//! step to get wrong.

use std::collections::HashMap;
use std::fmt::Write as _;
use std::io::Write as _;
use std::path::{Path, PathBuf};

use eyre::{Result, WrapErr};

/// Sidecar filename, kept inside the samples directory so it travels with them.
const FILE: &str = ".progress.tsv";

pub fn path(out_dir: &Path) -> PathBuf {
    out_dir.join(FILE)
}

/// Every cell already recorded: sample filename → hold in milliseconds.
pub fn load(out_dir: &Path) -> HashMap<String, u32> {
    let mut out = HashMap::new();
    let Ok(text) = std::fs::read_to_string(path(out_dir)) else {
        return out;
    };
    for line in text.lines() {
        // A line torn by a crash simply fails to parse and is ignored, which
        // re-records that one cell.
        if let Some((file, held)) = line.split_once('\t')
            && let Ok(held_ms) = held.trim().parse::<u32>()
            && !file.is_empty()
        {
            out.insert(file.to_string(), held_ms);
        }
    }
    out
}

/// Append one finished cell. Call only after the WAV is on disk.
pub fn append(out_dir: &Path, file: &str, held_ms: u32) -> Result<()> {
    let p = path(out_dir);
    let mut f = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&p)
        .wrap_err_with(|| format!("open {}", p.display()))?;
    let mut line = String::new();
    let _ = writeln!(line, "{file}\t{held_ms}");
    f.write_all(line.as_bytes())
        .wrap_err_with(|| format!("append to {}", p.display()))?;
    Ok(())
}

/// Forget all recorded progress, so the next run re-records everything.
pub fn clear(out_dir: &Path) -> Result<()> {
    let p = path(out_dir);
    match std::fs::remove_file(&p) {
        Ok(()) => Ok(()),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
        Err(e) => Err(e).wrap_err_with(|| format!("remove {}", p.display())),
    }
}

/// Whether `file` is already recorded: it has an entry *and* its audio exists.
///
/// Both are required. An entry without a file means the samples were cleaned up
/// (they are recoverable from a pack, so this is a normal state); a file without
/// an entry means the run died mid-write.
pub fn is_recorded(out_dir: &Path, recorded: &HashMap<String, u32>, file: &str) -> Option<u32> {
    let held = recorded.get(file).copied()?;
    out_dir.join(file).is_file().then_some(held)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tmp(tag: &str) -> PathBuf {
        static SEQ: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        let d = std::env::temp_dir().join(format!(
            "fts-progress-{tag}-{}-{}",
            std::process::id(),
            SEQ.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
        ));
        std::fs::create_dir_all(&d).unwrap();
        d
    }

    #[test]
    fn round_trips_entries() {
        let d = tmp("rt");
        append(&d, "a.wav", 3000).unwrap();
        append(&d, "b.wav", 12000).unwrap();
        let got = load(&d);
        assert_eq!(got.get("a.wav"), Some(&3000));
        assert_eq!(got.get("b.wav"), Some(&12000));
        std::fs::remove_dir_all(&d).ok();
    }

    #[test]
    fn missing_file_means_no_progress() {
        let d = tmp("empty");
        assert!(load(&d).is_empty());
        std::fs::remove_dir_all(&d).ok();
    }

    #[test]
    fn an_entry_without_its_audio_is_not_recorded() {
        // The samples may have been deleted to reclaim disk; they are
        // recoverable from the pack, so this is normal rather than corrupt.
        let d = tmp("noaudio");
        append(&d, "a.wav", 3000).unwrap();
        let rec = load(&d);
        assert_eq!(is_recorded(&d, &rec, "a.wav"), None);
        std::fs::remove_dir_all(&d).ok();
    }

    #[test]
    fn audio_without_an_entry_is_not_recorded() {
        // The crash-safety case: killed mid-write, so the WAV may be truncated.
        let d = tmp("noentry");
        std::fs::write(d.join("a.wav"), b"partial").unwrap();
        let rec = load(&d);
        assert_eq!(is_recorded(&d, &rec, "a.wav"), None);
        std::fs::remove_dir_all(&d).ok();
    }

    #[test]
    fn entry_plus_audio_is_recorded_and_returns_the_hold() {
        let d = tmp("ok");
        std::fs::write(d.join("a.wav"), b"x").unwrap();
        append(&d, "a.wav", 12000).unwrap();
        let rec = load(&d);
        assert_eq!(is_recorded(&d, &rec, "a.wav"), Some(12000));
        std::fs::remove_dir_all(&d).ok();
    }

    #[test]
    fn a_torn_line_is_ignored_rather_than_poisoning_the_rest() {
        let d = tmp("torn");
        append(&d, "a.wav", 3000).unwrap();
        // Simulate a line cut mid-write by a kill.
        let mut f = std::fs::OpenOptions::new()
            .append(true)
            .open(path(&d))
            .unwrap();
        f.write_all(b"b.wav\tnot-a-number\nc.wav").unwrap();
        drop(f);
        let got = load(&d);
        assert_eq!(got.get("a.wav"), Some(&3000));
        assert!(!got.contains_key("b.wav"));
        assert!(!got.contains_key("c.wav"));
        std::fs::remove_dir_all(&d).ok();
    }

    #[test]
    fn clear_forgets_everything() {
        let d = tmp("clear");
        append(&d, "a.wav", 3000).unwrap();
        clear(&d).unwrap();
        assert!(load(&d).is_empty());
        clear(&d).unwrap(); // idempotent
        std::fs::remove_dir_all(&d).ok();
    }
}
