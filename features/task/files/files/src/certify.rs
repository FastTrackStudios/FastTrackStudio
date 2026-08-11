//! Certification: reading a live-tree file into the CAS *and proving
//! the read was of one coherent state*.
//!
//! Spec #255 / issue #260: "Checkpoint certification runs a full
//! stat-scan; a file changing mid-hash is requeued, not corrupted."
//! Files here are DAW sessions and multi-GB media being written by
//! another process at the same moment we hash them — a 40 GB render is
//! minutes of streaming, and a torn read of it would be committed as a
//! perfectly valid-looking version of a file that never existed.
//!
//! The guard is a stat sandwich: `stat` the file, stream it into the
//! chunk store, `stat` it again. If size or mtime moved, the bytes we
//! hashed were a moving target — retry, and after
//! [`CadenceConfig::certify_attempts`](crate::cadence::CadenceConfig::certify_attempts)
//! attempts give up on *this* file only. Giving up means the file keeps
//! whatever state it already had in the store and rides into the next
//! capture; the capture in progress still succeeds for everything else.
//! A writer that never pauses would otherwise be able to block a whole
//! root's history indefinitely.
//!
//! An abandoned attempt does leave its chunks (and a manifest) behind in
//! the CAS. That is exactly what `ChunkStore::gc`'s manifest sweep is
//! for: nothing in any commit tree references the abandoned `FileId`, so
//! the next GC pass reclaims it (issue #258).

use std::path::Path;
use std::sync::Arc;
use std::time::SystemTime;

use task_files_chunk_store::{ChunkStore, FileId};

use crate::error::{Error, Result};

/// Test seam: a callback invoked after the pre-read `stat` and before
/// the streaming read, so a test can make a file change *during* its
/// own hash deterministically instead of racing a background writer.
/// Production never sets one.
pub type MidHashHook = Arc<dyn Fn(&Path) + Send + Sync>;

/// The identity a stat sandwich compares. Deliberately not a content
/// hash — the point is to detect that the file moved under us without
/// reading it a second time.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct FileStat {
    len: u64,
    modified: Option<SystemTime>,
}

fn stat(path: &Path) -> Result<FileStat> {
    let metadata = std::fs::metadata(path)?;
    Ok(FileStat {
        len: metadata.len(),
        modified: metadata.modified().ok(),
    })
}

/// Stream `path` into `chunks`, certified stable.
///
/// `Ok(Some(id))` — the file was identical before and after the read;
/// `id` is its content address. `Ok(None)` — the file was still being
/// written after `attempts` tries: requeue it.
pub async fn stream_certified(
    chunks: &ChunkStore,
    path: &Path,
    attempts: u32,
    hook: Option<&MidHashHook>,
) -> Result<Option<FileId>> {
    for _ in 0..attempts.max(1) {
        let before = stat(path)?;
        if let Some(hook) = hook {
            hook(path);
        }
        let file = tokio::fs::File::open(path).await?;
        let file_id = chunks
            .write_stream(file)
            .await
            .map_err(|e| Error::Repo(format!("chunk store: {e}")))?;
        let after = stat(path)?;
        if before == after {
            return Ok(Some(file_id));
        }
    }
    Ok(None)
}
