//! [`ChunkStore`]: the on-disk pairing of an iroh-blobs `FsStore` (chunk
//! bytes, content-addressed by blake3) with a manifests directory (Files'
//! own `FileId -> chunk list` records, kept outside iroh-blobs per
//! ADR 0001).

use std::collections::{BTreeSet, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::SystemTime;

use fastcdc::v2020::AsyncStreamCDC;
use futures::StreamExt;
use iroh_blobs::store::fs::options::Options;
use iroh_blobs::store::{GcConfig as IrohGcConfig, ProtectCb, ProtectOutcome};
use tokio::io::{AsyncRead, AsyncWrite, AsyncWriteExt};
use tokio::sync::RwLock;

use crate::chunker::ChunkerConfig;
use crate::error::{Error, Result};
use crate::gc::{GcConfig, GcStats};
use crate::manifest::{ChunkRef, FileId, Manifest};

/// The chunk-store substrate: streaming write/read of files as
/// content-defined, deduplicated, blake3-addressed chunks.
///
/// A store lives at a directory with two subdirectories: `blobs/` (the
/// iroh-blobs `FsStore`) and `manifests/` (Files-owned, plain files named
/// `<file id hex>.manifest`). Both are required to resolve a `FileId`;
/// deleting `blobs/` and reconstructing it from a copy plus the manifests
/// (or vice versa, once GC is in place) is the "rebuildable" property ADR
/// 0001 asks for.
///
/// **GC pinning model (deliberate choice, not the iroh-blobs default):**
/// chunks are stored via [`iroh_blobs::api::blobs::Batch::temp_tag`] rather
/// than the default `.await` (which mints a *permanent* named `Tag` per
/// call — with ~1 chunk per MiB, a multi-GB file would leave thousands of
/// permanent rows in iroh-blobs' own tags table that nothing ever cleans
/// up). Per ADR 0001, liveness here is Files' own manifests, not iroh-blobs
/// tags/temp-tags — manifests are the roots (see the `gc` module doc for
/// how a store opened with GC enabled derives that liveness). Every
/// `TempTag` [`ChunkStore::write_stream`] mints for one call — new adds
/// *and* already-present chunks alike — is held until that call's manifest
/// is durable, then dropped together: each chunk needs protection for the
/// whole call, not just up to its own `add`, since it has no manifest
/// referencing it — and therefore nothing else keeping it alive — until
/// the call finishes.
pub struct ChunkStore {
    blobs: iroh_blobs::store::fs::FsStore,
    manifests_dir: PathBuf,
    chunker_config: ChunkerConfig,
    /// Set only for a store opened via [`ChunkStore::open_with_gc`] — the
    /// gate [`ChunkStore::gc`] checks before doing anything, since a store
    /// opened without a GC interval has nothing that will ever reclaim the
    /// chunks a manifest removal would orphan. See the `gc` module doc:
    /// iroh-blobs' background task (wired up at open time when this is
    /// `true`) derives its own liveness live from the manifests directory
    /// on every sweep, so there's no protect-set *state* to hold here.
    chunk_gc_enabled: bool,
    /// Quiesce lock between [`ChunkStore::write_stream`] and the GC protect
    /// callback (only actually contended when GC is enabled — held
    /// uncontended otherwise). iroh-blobs' own mark-then-sweep pass takes a
    /// *snapshot* of live tags/temp-tags once, before sweeping — a chunk
    /// added and tagged strictly after that snapshot but before the
    /// subsequent sweep's blob listing is invisible to that pass' `live`
    /// set even though its temp tag exists at sweep time (confirmed the
    /// hard way: holding `TempTag`s for a whole `write_stream` call alone
    /// was not sufficient under sustained concurrent write/GC pressure in
    /// testing — a handful of chunks vanished despite their manifests
    /// staying intact). Rather than chase that timing inside iroh-blobs'
    /// internals, `write_stream` holds a read guard on this lock for its
    /// whole call, and the protect callback holds a write guard for the
    /// duration of its scan — so no `write_stream` call can ever be
    /// partway through (some chunks added, blob visible; others not) while
    /// a scan is in flight. Every write the callback's scan can observe is
    /// either fully durable (its manifest is on disk, findable) or not yet
    /// started (blocked on the lock); there is no partial state to miss.
    write_lock: Arc<RwLock<()>>,
}

impl ChunkStore {
    /// Open (creating if absent) a chunk store rooted at `root`, with no
    /// chunk-level GC — [`ChunkStore::gc`] on a store opened this way
    /// removes swept manifests but returns [`Error::GcDisabled`] rather
    /// than reclaiming chunks, since nothing would ever sweep them.
    pub async fn open(root: impl AsRef<Path>) -> Result<Self> {
        Self::open_with_config(root, ChunkerConfig::default()).await
    }

    /// Open a chunk store with a non-default [`ChunkerConfig`] (e.g. a
    /// smaller average chunk size for a root of many small text files) and
    /// no chunk-level GC — see [`ChunkStore::open`].
    pub async fn open_with_config(
        root: impl AsRef<Path>,
        chunker_config: ChunkerConfig,
    ) -> Result<Self> {
        Self::open_inner(root, chunker_config, None).await
    }

    /// Open a chunk store with chunk-level GC enabled: [`ChunkStore::gc`]
    /// will actually reclaim unreferenced chunks (on iroh-blobs' own
    /// background schedule, per the `gc` module doc — not synchronously
    /// within the `gc` call itself).
    pub async fn open_with_gc(
        root: impl AsRef<Path>,
        chunker_config: ChunkerConfig,
        gc: GcConfig,
    ) -> Result<Self> {
        Self::open_inner(root, chunker_config, Some(gc)).await
    }

    async fn open_inner(
        root: impl AsRef<Path>,
        chunker_config: ChunkerConfig,
        gc: Option<GcConfig>,
    ) -> Result<Self> {
        chunker_config.validate()?;
        let root = root.as_ref();
        let blobs_dir = root.join("blobs");
        let manifests_dir = root.join("manifests");
        tokio::fs::create_dir_all(&blobs_dir).await?;
        tokio::fs::create_dir_all(&manifests_dir).await?;

        let chunk_gc_enabled = gc.is_some();
        let write_lock: Arc<RwLock<()>> = Arc::new(RwLock::new(()));
        let mut options = Options::new(&blobs_dir);
        if let Some(gc) = gc {
            // Derives liveness live, from what's actually on disk, on every
            // invocation — see the `gc` module doc for why this (not a
            // snapshot `ChunkStore::gc` publishes) is what closes the
            // stale-protect-set data-loss window.
            //
            // Any read failure here — listing the directory, or reading
            // one manifest — aborts the *whole pass* rather than skipping
            // just the one file that failed. This has to be conservative:
            // a permanently corrupt manifest and a manifest that's fine but
            // hit a transient I/O hiccup on this particular scan are
            // indistinguishable from here, and treating the latter as "no
            // chunks to protect for it" is exactly how a perfectly healthy
            // manifest's chunk gets swept out from under it — irreversibly,
            // since a chunk deletion can't be undone by the *next* pass
            // reading it correctly. `gc` itself never decodes a *kept*
            // manifest's contents (only enumerates the directory for
            // `FileId`/mtime), so a corrupt manifest still can't wedge
            // anything durable — it just means this pass reclaims nothing
            // and tries again next interval.
            let manifests_dir_for_gc = manifests_dir.clone();
            let write_lock_for_gc = write_lock.clone();
            let add_protected: ProtectCb = Arc::new(move |live: &mut HashSet<iroh_blobs::Hash>| {
                let manifests_dir = manifests_dir_for_gc.clone();
                let write_lock = write_lock_for_gc.clone();
                Box::pin(async move {
                    // Quiesce every `write_stream` call for the duration of
                    // this scan (see `write_lock`'s doc on `ChunkStore` for
                    // why this, not iroh-blobs' own temp-tag bookkeeping,
                    // is what actually closes the concurrent-write race).
                    let _quiesce = write_lock.write().await;
                    let Ok(manifests) = manifests_with_mtime_in(&manifests_dir).await else {
                        return ProtectOutcome::Abort;
                    };
                    for (file_id, _mtime) in &manifests {
                        let Ok(manifest) = read_manifest_in(&manifests_dir, *file_id).await else {
                            return ProtectOutcome::Abort;
                        };
                        live.extend(
                            manifest
                                .chunks
                                .iter()
                                .map(|chunk| iroh_blobs::Hash::from(chunk.hash)),
                        );
                    }
                    ProtectOutcome::Continue
                })
            });
            options.gc = Some(IrohGcConfig {
                interval: gc.interval,
                add_protected: Some(add_protected),
            });
        }

        let db_path = blobs_dir.join("blobs.db");
        let blobs = iroh_blobs::store::fs::FsStore::load_with_opts(db_path, options)
            .await
            .map_err(|e| {
                Error::Store(format!(
                    "opening blob store at {}: {e}",
                    blobs_dir.display()
                ))
            })?;
        Ok(Self {
            blobs,
            manifests_dir,
            chunker_config,
            chunk_gc_enabled,
            write_lock,
        })
    }

    /// Removes every manifest that isn't in `protected` (the caller's
    /// externally-referenced set — Vault-referenced versions, from the
    /// version-store layer above) and is older than `keep_newer` (guards a
    /// manifest written concurrently with this call, mirroring
    /// `Backend::gc`'s own `keep_newer` contract) — the *only* thing this
    /// method does. Requires a store opened with [`ChunkStore::open_with_gc`]
    /// (otherwise [`Error::GcDisabled`]): its removals are only meaningful
    /// because that store's background task is what actually reclaims the
    /// chunks a removed manifest orphans (see the `gc` module doc for why
    /// that reclamation is asynchronous and driven independently, not
    /// published by this call).
    pub async fn gc(
        &self,
        protected: &BTreeSet<FileId>,
        keep_newer: SystemTime,
    ) -> Result<GcStats> {
        if !self.chunk_gc_enabled {
            return Err(Error::GcDisabled);
        }

        let mut manifests_swept = 0usize;
        for (file_id, mtime) in self.manifests_with_mtime().await? {
            let keep = protected.contains(&file_id) || mtime >= keep_newer;
            if !keep {
                self.remove_manifest(file_id).await?;
                manifests_swept += 1;
            }
        }

        Ok(GcStats { manifests_swept })
    }

    /// Every manifest currently on disk, with its last-modified time (the
    /// `keep_newer` protection signal, mirroring
    /// `ObjectStore::list_with_mtime` in the version-store crate).
    async fn manifests_with_mtime(&self) -> Result<Vec<(FileId, SystemTime)>> {
        manifests_with_mtime_in(&self.manifests_dir).await
    }

    async fn remove_manifest(&self, file_id: FileId) -> Result<()> {
        let path = self.manifest_path(file_id);
        match tokio::fs::remove_file(&path).await {
            Ok(()) => Ok(()),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
            Err(e) => Err(Error::Io(e)),
        }
    }

    /// The number of distinct chunk blobs currently in the store — a cheap
    /// observability hook, and how `gc`'s test suite observes iroh-blobs'
    /// background sweep actually reclaiming a chunk (see the `gc` module
    /// doc: reclamation happens on iroh-blobs' own schedule, not
    /// synchronously within `ChunkStore::gc`).
    pub async fn chunk_count(&self) -> Result<usize> {
        let hashes = self
            .blobs
            .blobs()
            .list()
            .hashes()
            .await
            .map_err(|e| Error::Store(format!("listing chunks: {e}")))?;
        Ok(hashes.len())
    }

    /// Stream `source` into the store: chunk it with FastCDC, blake3-hash
    /// each chunk, and put chunks the blob store doesn't already have
    /// (existing chunks are skipped — this is where cross-save dedup
    /// happens). Returns the resulting file's content address.
    ///
    /// Bounded memory: `source` is never read into a single buffer. At
    /// most one chunk (at most `chunker_config.max_size` bytes) is held at
    /// a time, so this is safe to call on a multi-GB source.
    pub async fn write_stream<R>(&self, source: R) -> Result<FileId>
    where
        R: AsyncRead + Unpin + Send,
    {
        let mut chunker = AsyncStreamCDC::new(
            source,
            self.chunker_config.min_size,
            self.chunker_config.avg_size,
            self.chunker_config.max_size,
        );
        let mut stream = std::pin::pin!(chunker.as_stream());

        // The real protection against a concurrent GC pass (see
        // `write_lock`'s doc on `ChunkStore`): held for this whole call, so
        // no GC scan can observe this write partway through. Acquired
        // before any chunk touches the blob store.
        let _write_guard = self.write_lock.read().await;

        // Belt-and-suspenders on top of `write_lock`, and iroh-blobs' own
        // recommended pattern regardless: protects every chunk this call
        // touches — newly added *and* already-present — via its tagging
        // mechanism too. Each `TempTag` protects its blob only as long as
        // the `TempTag` value itself is held (a `Batch`'s own lifetime does
        // *not* keep them alive on its own — `#[must_use]`, confirmed the
        // hard way), so every one returned below is collected here and
        // dropped together only after `write_manifest` durably references
        // every chunk in it.
        let batch = self
            .blobs
            .batch()
            .await
            .map_err(|e| Error::Store(format!("opening a gc-protection batch: {e}")))?;
        let mut pending_tags = Vec::new();

        let mut chunks: Vec<ChunkRef> = Vec::new();
        while let Some(item) = stream.next().await {
            let data = item.map_err(|e| Error::Io(e.into()))?.data;
            let hash = blake3::hash(&data);
            let len = data.len() as u64;
            let already_present = self
                .blobs
                .has(*hash.as_bytes())
                .await
                .map_err(|e| Error::Store(format!("checking chunk {hash}: {e}")))?;
            let temp_tag = if already_present {
                batch
                    .temp_tag(iroh_blobs::Hash::from(hash))
                    .await
                    .map_err(|e| Error::Store(format!("protecting existing chunk {hash}: {e}")))?
            } else {
                batch
                    .add_bytes(data)
                    .await
                    .map_err(|e| Error::Store(format!("storing chunk {hash}: {e}")))?
            };
            pending_tags.push(temp_tag);
            chunks.push(ChunkRef { hash, len });
        }

        let manifest = Manifest::new(chunks);
        let file_id = manifest.file_id();
        self.write_manifest(file_id, &manifest).await?;
        drop(pending_tags);
        drop(batch);
        Ok(file_id)
    }

    /// Derive the [`FileId`] `source` *would* have in this store,
    /// writing nothing — no chunks, no manifest, no locks. The pure
    /// half of [`ChunkStore::write_stream`]: same chunker config, same
    /// per-chunk blake3, same manifest encoding, so the answer is
    /// exactly the id a real write of the same bytes returns. This is
    /// what lets an is-this-content-already-versioned comparison run
    /// without persisting never-versioned bytes as orphaned store data
    /// (PR #289 review).
    pub async fn probe_stream<R>(&self, source: R) -> Result<FileId>
    where
        R: AsyncRead + Unpin + Send,
    {
        let mut chunker = AsyncStreamCDC::new(
            source,
            self.chunker_config.min_size,
            self.chunker_config.avg_size,
            self.chunker_config.max_size,
        );
        let mut stream = std::pin::pin!(chunker.as_stream());
        let mut chunks: Vec<ChunkRef> = Vec::new();
        while let Some(item) = stream.next().await {
            let data = item.map_err(|e| Error::Io(e.into()))?.data;
            chunks.push(ChunkRef {
                hash: blake3::hash(&data),
                len: data.len() as u64,
            });
        }
        Ok(Manifest::new(chunks).file_id())
    }

    /// Stream the file named by `file_id` to `dest`, one chunk at a time.
    /// Bounded memory: chunks are copied to `dest` and dropped as they are
    /// read, never assembled into a whole-file buffer.
    pub async fn read_to<W>(&self, file_id: FileId, dest: &mut W) -> Result<()>
    where
        W: AsyncWrite + Unpin,
    {
        let manifest = self.read_manifest(file_id).await?;
        for chunk in &manifest.chunks {
            let hash_bytes = *chunk.hash.as_bytes();
            let mut reader = self.blobs.reader(hash_bytes);
            let copied = match tokio::io::copy(&mut reader, dest).await {
                Ok(copied) => copied,
                Err(io_err) => {
                    // Distinguish "chunk genuinely absent from the blob
                    // store" (repairable — re-fetch/re-derive it) from a
                    // real I/O fault, so #257's version-store layer can
                    // tell the two apart instead of treating everything
                    // as fatal.
                    let present = self.blobs.has(hash_bytes).await.unwrap_or(true);
                    if present {
                        return Err(Error::Io(io_err));
                    }
                    return Err(Error::MissingChunk(chunk.hash.to_hex().to_string()));
                }
            };
            if copied != chunk.len {
                return Err(Error::MissingChunk(chunk.hash.to_hex().to_string()));
            }
        }
        Ok(())
    }

    /// Convenience wrapper over [`ChunkStore::read_to`] that collects the
    /// whole file into memory. For tests and small files — large files
    /// should use `read_to` against a sink (a file, a network stream)
    /// directly.
    pub async fn read_to_vec(&self, file_id: FileId) -> Result<Vec<u8>> {
        let mut buf = Vec::new();
        self.read_to(file_id, &mut buf).await?;
        Ok(buf)
    }

    /// Fetch the manifest for `file_id`, if this store has it.
    pub async fn manifest(&self, file_id: FileId) -> Result<Manifest> {
        self.read_manifest(file_id).await
    }

    /// Whether a manifest for `file_id` is on disk. Does not verify that
    /// every chunk it references is still present in the blob store.
    pub async fn has(&self, file_id: FileId) -> bool {
        self.read_manifest(file_id).await.is_ok()
    }

    /// Flush the blob store to disk. iroh-blobs' `FsStore` may not
    /// durably persist the last few seconds of writes without this
    /// (see the crate's own `fs` module docs) — call it before a process
    /// exit or before relying on the store surviving a crash.
    pub async fn shutdown(&self) -> Result<()> {
        self.blobs
            .shutdown()
            .await
            .map_err(|e| Error::Store(format!("shutdown: {e}")))
    }

    fn manifest_path(&self, file_id: FileId) -> PathBuf {
        manifest_path_in(&self.manifests_dir, file_id)
    }

    /// Durably write `manifest` at `file_id`'s path. If a file already
    /// exists there *and decodes*, it is necessarily byte-identical (the
    /// path is derived from the content hash of the manifest bytes), so
    /// there is nothing to do beyond refreshing its mtime (see below). If
    /// it exists but fails to decode — e.g. a prior write crashed between
    /// `rename` and this process' next start, on a filesystem where rename
    /// can be observed before the data it pointed at is durable — that is
    /// treated as damage to repair, not a reason to skip the write:
    /// without this, `read_to` for that `FileId` would fail forever.
    async fn write_manifest(&self, file_id: FileId, manifest: &Manifest) -> Result<()> {
        let path = self.manifest_path(file_id);
        if let Ok(existing) = tokio::fs::read(&path).await {
            if Manifest::decode(&existing).is_ok() {
                // `ChunkStore::gc`'s `keep_newer` protection relies on
                // mtime reflecting the most recent write, not just the
                // first one — a caller re-`write_stream`ing already-stored
                // content is exactly the "written concurrently with a gc
                // pass" case that contract exists to protect (mirrors
                // `ObjectStore::write`'s identical fix in the version-store
                // crate).
                match self.touch_manifest(&path).await {
                    Ok(()) => return Ok(()),
                    Err(Error::Io(e)) if e.kind() == std::io::ErrorKind::NotFound => {
                        // Raced a concurrent `gc()` sweep of this exact
                        // manifest between the read above and this touch —
                        // the file we just confirmed exists (and decodes)
                        // is gone. The bytes are still known-good (we
                        // decoded them a moment ago), so this is a
                        // legitimate rewrite, not corruption: fall through
                        // to the normal write-then-rename path below rather
                        // than surfacing an io::Error for what the caller
                        // sees as a successful write.
                    }
                    Err(e) => return Err(e),
                }
            }
        }

        // Write-then-fsync-then-rename, plus an fsync of the containing
        // directory: on ext4/btrfs a rename can be observed durable before
        // the data blocks it points at are, so skipping the file fsync
        // (or the directory fsync after the rename) can leave a durable
        // but corrupt manifest behind a power loss. The temp name must be
        // unique per *call*, not just per file id: two concurrent
        // write_stream calls for identical content otherwise share one
        // tmp path, and the loser's rename fails with ENOENT because the
        // winner's rename already consumed it out from under it.
        static TMP_COUNTER: AtomicU64 = AtomicU64::new(0);
        let unique = TMP_COUNTER.fetch_add(1, Ordering::Relaxed);
        let tmp_path = self.manifests_dir.join(format!(
            "{}.manifest.tmp.{}.{unique}",
            file_id.to_hex(),
            std::process::id()
        ));
        {
            let mut file = tokio::fs::File::create(&tmp_path).await?;
            file.write_all(&manifest.encode()).await?;
            file.sync_all().await?;
        }
        tokio::fs::rename(&tmp_path, &path).await?;
        Self::fsync_dir(&self.manifests_dir).await?;
        Ok(())
    }

    async fn fsync_dir(dir: &Path) -> Result<()> {
        let dir = tokio::fs::File::open(dir).await?;
        dir.sync_all().await?;
        Ok(())
    }

    /// Set `path`'s mtime to now (see `ObjectStore::touch` for the same
    /// pattern: `std::fs::File::set_modified` has no tokio-native
    /// equivalent, so this hands the already-open, already I/O-completed
    /// file handle to a blocking thread).
    async fn touch_manifest(&self, path: &Path) -> Result<()> {
        let file = tokio::fs::OpenOptions::new().write(true).open(path).await?;
        let std_file = file.into_std().await;
        tokio::task::spawn_blocking(move || std_file.set_modified(SystemTime::now()))
            .await
            .map_err(|e| Error::Io(std::io::Error::other(e)))??;
        Ok(())
    }

    async fn read_manifest(&self, file_id: FileId) -> Result<Manifest> {
        read_manifest_in(&self.manifests_dir, file_id).await
    }
}

fn manifest_path_in(dir: &Path, file_id: FileId) -> PathBuf {
    dir.join(format!("{}.manifest", file_id.to_hex()))
}

/// Free-function twin of `ChunkStore::manifests_with_mtime`, taking the
/// manifests directory directly rather than `&self` — the GC protect
/// callback built in `open_inner` needs this (and [`read_manifest_in`])
/// without holding a `ChunkStore` reference, since the callback is
/// constructed *during* `open_inner`, before `Self` exists.
async fn manifests_with_mtime_in(dir: &Path) -> Result<Vec<(FileId, SystemTime)>> {
    let mut out = Vec::new();
    let mut entries = tokio::fs::read_dir(dir).await?;
    while let Some(entry) = entries.next_entry().await? {
        let name = entry.file_name();
        let Some(name) = name.to_str() else {
            continue;
        };
        let Some(hex) = name.strip_suffix(".manifest") else {
            continue; // skip temp files (`.manifest.tmp.<pid>.<n>`)
        };
        let Ok(file_id) = FileId::from_hex(hex) else {
            continue;
        };
        let metadata = entry.metadata().await?;
        out.push((file_id, metadata.modified()?));
    }
    Ok(out)
}

/// Free-function twin of `ChunkStore::read_manifest` — see
/// [`manifests_with_mtime_in`]'s doc for why the GC protect callback needs
/// this shape.
async fn read_manifest_in(dir: &Path, file_id: FileId) -> Result<Manifest> {
    let path = manifest_path_in(dir, file_id);
    let bytes = match tokio::fs::read(&path).await {
        Ok(bytes) => bytes,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            return Err(Error::UnknownFileId(file_id.to_hex()));
        }
        Err(e) => return Err(Error::Io(e)),
    };
    Manifest::decode(&bytes)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Regression test (data-loss finding): `write_manifest`'s dedup fast
    /// path reads + decodes an existing manifest, then calls
    /// `touch_manifest` to refresh its mtime — but nothing prevented a
    /// concurrent `remove_manifest` (what `ChunkStore::gc` calls on a swept
    /// manifest) from deleting that exact file in between, which used to
    /// surface as `Error::Io(NotFound)` from a logically-successful,
    /// content-already-durable write. `write_manifest` now falls through to
    /// a full rewrite on that specific NotFound instead of propagating it.
    /// This hammers the race probabilistically (no single interleaving is
    /// forceable through the public API) rather than asserting one exact
    /// ordering.
    #[tokio::test]
    async fn write_manifest_survives_concurrent_manifest_removal_pressure() {
        let dir = tempfile::tempdir().unwrap();
        let store = Arc::new(ChunkStore::open(dir.path()).await.unwrap());
        let content = b"racing a concurrent manifest removal".to_vec();
        let file_id = store.write_stream(&content[..]).await.unwrap();

        let remover = {
            let store = store.clone();
            tokio::spawn(async move {
                for _ in 0..200 {
                    store.remove_manifest(file_id).await.unwrap();
                    tokio::task::yield_now().await;
                }
            })
        };
        let writer = {
            let store = store.clone();
            let content = content.clone();
            tokio::spawn(async move {
                for _ in 0..200 {
                    store.write_stream(&content[..]).await.unwrap();
                    tokio::task::yield_now().await;
                }
            })
        };
        let (remover, writer) = tokio::join!(remover, writer);
        remover.unwrap();
        writer.unwrap();

        // One final write must leave the store in a consistent, readable
        // state regardless of how the race above landed.
        let final_id = store.write_stream(&content[..]).await.unwrap();
        assert_eq!(final_id, file_id);
        assert!(
            store.has(file_id).await,
            "the manifest must survive a final write after the race"
        );
        assert_eq!(store.read_to_vec(file_id).await.unwrap(), content);
    }

    #[tokio::test]
    async fn read_to_reports_missing_chunk_not_io_error() {
        let dir = tempfile::tempdir().unwrap();
        let store = ChunkStore::open(dir.path()).await.unwrap();

        // A manifest naming a chunk that was never written to the blob
        // store — simulates a chunk that's absent (evicted, not yet
        // hydrated, or a corrupt write) without needing a delete API
        // iroh-blobs doesn't expose to us.
        let fake_chunk = ChunkRef {
            hash: blake3::hash(b"this chunk was never stored"),
            len: 4,
        };
        let manifest = Manifest::new(vec![fake_chunk]);
        let file_id = manifest.file_id();
        store.write_manifest(file_id, &manifest).await.unwrap();

        let mut sink = tokio::io::sink();
        let err = store.read_to(file_id, &mut sink).await.unwrap_err();
        assert!(
            matches!(err, Error::MissingChunk(_)),
            "expected Error::MissingChunk for an absent chunk, got {err:?}"
        );
    }

    #[tokio::test]
    async fn write_stream_does_not_mint_persistent_tags() {
        let dir = tempfile::tempdir().unwrap();
        let store = ChunkStore::open(dir.path()).await.unwrap();
        // Several MiB so this writes multiple chunks, not just one.
        let content: Vec<u8> = (0..3 * 1024 * 1024).map(|i| (i % 251) as u8).collect();
        store.write_stream(&content[..]).await.unwrap();

        let tag_count = store.blobs.tags().list().await.unwrap().count().await;
        assert_eq!(
            tag_count, 0,
            "write_stream must not leave persistent iroh-blobs tags behind — \
             Files' manifests are the liveness authority, not the tags table"
        );
    }

    #[tokio::test]
    async fn write_manifest_repairs_a_corrupt_existing_file() {
        let dir = tempfile::tempdir().unwrap();
        let store = ChunkStore::open(dir.path()).await.unwrap();
        let content = b"repair me if I show up corrupt on disk";

        let file_id = store.write_stream(&content[..]).await.unwrap();
        // Simulate the crash scenario: a manifest file exists at the
        // right path but its bytes are garbage (e.g. rename observed
        // before the data was durable).
        let path = store.manifest_path(file_id);
        tokio::fs::write(&path, b"not a valid manifest")
            .await
            .unwrap();
        assert!(Manifest::decode(&tokio::fs::read(&path).await.unwrap()).is_err());

        // Re-writing the same content must repair it rather than
        // early-returning past the corruption.
        let repaired_id = store.write_stream(&content[..]).await.unwrap();
        assert_eq!(repaired_id, file_id);
        assert_eq!(store.read_to_vec(file_id).await.unwrap(), content);
    }
}
