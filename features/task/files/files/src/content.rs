//! Getting one live-tree file's bytes into a root's object store, per
//! flavor — the seam [`crate::checkpoint`] writes through.
//!
//! Both flavors must satisfy the same two properties, and neither gets
//! them from jj's `Backend::write_file` alone:
//!
//! - **Bounded memory.** A checkpoint may cross files far larger than
//!   RAM. The CAS backend's `write_file` already streams (PR #280); jj's
//!   `GitBackend::write_file` does not — it `read_to_end`s the whole file
//!   into a `Vec` and `unwrap()`s the read (jj-lib 0.44
//!   `git_backend.rs:1068`), which is the same whole-root-OOM class PR
//!   #280 fixed for media, plus a panic path on a mid-read I/O error (PR
//!   #282 review). So [`ContentStore::Git`] writes the blob itself
//!   through gix's streaming object store instead.
//! - **Don't write what's already there.** A git blob id is a pure
//!   function of the bytes, so [`ContentStore::probe`] can compute a
//!   file's identity *without writing anything*; the checkpoint compares
//!   that to the head tree first and skips untouched files entirely. On
//!   a tracking day where two files out of ten thousand changed, the
//!   other 9998 cost one streaming hash each and no object writes.
//!   (The CAS backend has no cheap probe — its id is a chunk-manifest
//!   hash — so media roots still write-then-compare, where the chunk
//!   store dedups. Cheap stat-based skipping for both flavors belongs
//!   with the cadence engine's watcher state, #260.)

use std::path::Path;
use std::sync::Arc;
use std::sync::atomic::AtomicBool;

use jj_lib::backend::{Backend, FileId};
use jj_lib::object_id::ObjectId as _;
use jj_lib::repo::{ReadonlyRepo, Repo as _};
use jj_lib::repo_path::RepoPath;
use tokio_util::compat::TokioAsyncReadCompatExt as _;

use crate::error::{Error, Result};

pub enum ContentStore<'a> {
    /// Media roots: jj's own streaming `Backend::write_file` over the CAS
    /// chunk store.
    Cas(&'a dyn Backend),
    /// Software roots: the colocated git object database, written
    /// directly (see the module doc for why not through jj).
    Git(gix::Repository),
}

impl<'a> ContentStore<'a> {
    /// Pick the writer matching `repo`'s backend. A git-backed repo is
    /// recognized through jj-lib's own accessor, so a repo that merely
    /// claims the name can't slip through.
    pub fn for_repo(repo: &'a Arc<ReadonlyRepo>, backend: &'a dyn Backend) -> Result<Self> {
        match jj_lib::git::get_git_repo(repo.store()) {
            Ok(git_repo) => Ok(Self::Git(git_repo)),
            Err(_) => Ok(Self::Cas(backend)),
        }
    }

    /// This file's content id, computed without writing anything —
    /// `None` when the backend has no cheap way to know it in advance.
    pub fn probe(&self, disk_path: &Path) -> Result<Option<FileId>> {
        let Self::Git(git_repo) = self else {
            return Ok(None);
        };
        let len = std::fs::metadata(disk_path)?.len();
        let mut file = std::fs::File::open(disk_path)?;
        let oid = gix::objs::compute_stream_hash(
            git_repo.object_hash(),
            gix::objs::Kind::Blob,
            &mut file,
            len,
            &mut gix::progress::Discard,
            &AtomicBool::new(false),
        )
        .map_err(|e| Error::Repo(format!("{}: hashing: {e}", disk_path.display())))?;
        Ok(Some(FileId::new(oid.as_bytes().to_vec())))
    }

    /// Store the file's bytes and return its content id. `probed` is
    /// [`ContentStore::probe`]'s answer for the same file, threaded back
    /// in so the id isn't computed twice — and so an object already in
    /// the store is never rewritten.
    pub async fn write(
        &self,
        path: &RepoPath,
        disk_path: &Path,
        probed: Option<FileId>,
    ) -> Result<FileId> {
        match self {
            Self::Cas(backend) => {
                // `Backend::write_file` reads through futures-io; tokio's
                // file handle wears the `compat()` adapter (the same seam
                // the version-store backend uses internally).
                let mut disk = tokio::fs::File::open(disk_path).await?.compat();
                Ok(backend.write_file(path, &mut disk).await?)
            }
            Self::Git(git_repo) => Self::write_git_blob(git_repo, disk_path, probed),
        }
    }

    fn write_git_blob(
        git_repo: &gix::Repository,
        disk_path: &Path,
        probed: Option<FileId>,
    ) -> Result<FileId> {
        use gix::objs::{Exists as _, Write as _};

        let known = probed
            .as_ref()
            .map(|id| gix::ObjectId::from_bytes_or_panic(id.as_bytes()));
        if let (Some(id), Some(oid)) = (&probed, &known)
            && git_repo.objects.exists(oid)
        {
            // Identical bytes are already an object here — nothing to
            // write, and the blob is immutable, so this is complete.
            return Ok(id.clone());
        }

        let len = std::fs::metadata(disk_path)?.len();
        let mut file = std::fs::File::open(disk_path)?;
        let written = match known {
            Some(oid) => git_repo.objects.write_stream_with_known_id(
                gix::objs::Kind::Blob,
                len,
                &mut file,
                oid,
            ),
            None => git_repo
                .objects
                .write_stream(gix::objs::Kind::Blob, len, &mut file),
        }
        .map_err(|e| Error::Repo(format!("{}: writing blob: {e}", disk_path.display())))?;
        Ok(FileId::new(written.as_bytes().to_vec()))
    }
}
