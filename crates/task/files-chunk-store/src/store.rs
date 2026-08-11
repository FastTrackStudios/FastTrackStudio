//! [`ChunkStore`]: the on-disk pairing of an iroh-blobs `FsStore` (chunk
//! bytes, content-addressed by blake3) with a manifests directory (Files'
//! own `FileId -> chunk list` records, kept outside iroh-blobs per
//! ADR 0001).

use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

use fastcdc::v2020::AsyncStreamCDC;
use futures::StreamExt;
use tokio::io::{AsyncRead, AsyncWrite, AsyncWriteExt};

use crate::chunker::ChunkerConfig;
use crate::error::{Error, Result};
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
/// chunks are stored via a [`iroh_blobs::api::blobs::AddProgress::temp_tag`]
/// rather than the default `.await` (which mints a *permanent* named `Tag`
/// per call — with ~1 chunk per MiB, a multi-GB file would leave thousands
/// of permanent rows in iroh-blobs' own tags table that nothing ever
/// cleans up). Per ADR 0001, liveness here is Files' own manifests, not
/// iroh-blobs tags — manifests are the roots. This crate never calls
/// iroh-blobs' own `gc()`, so an untagged chunk is safe until Files' own
/// GC (future work, `#257`, driven by manifest reachability) decides
/// otherwise; the temp tag only needs to outlive the single `add` call it
/// guards against iroh-blobs' internal concurrent-write races.
pub struct ChunkStore {
    blobs: iroh_blobs::store::fs::FsStore,
    manifests_dir: PathBuf,
    chunker_config: ChunkerConfig,
}

impl ChunkStore {
    /// Open (creating if absent) a chunk store rooted at `root`.
    pub async fn open(root: impl AsRef<Path>) -> Result<Self> {
        Self::open_with_config(root, ChunkerConfig::default()).await
    }

    /// Open a chunk store with a non-default [`ChunkerConfig`] (e.g. a
    /// smaller average chunk size for a root of many small text files).
    pub async fn open_with_config(
        root: impl AsRef<Path>,
        chunker_config: ChunkerConfig,
    ) -> Result<Self> {
        chunker_config.validate()?;
        let root = root.as_ref();
        let blobs_dir = root.join("blobs");
        let manifests_dir = root.join("manifests");
        tokio::fs::create_dir_all(&blobs_dir).await?;
        tokio::fs::create_dir_all(&manifests_dir).await?;
        let blobs = iroh_blobs::store::fs::FsStore::load(&blobs_dir)
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
        })
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
            if !already_present {
                // See the "GC pinning model" doc on ChunkStore: temp_tag,
                // not the default persistent-tag `.await`, and the tag is
                // dropped immediately — Files' manifests are the roots.
                let _temp_tag = self
                    .blobs
                    .add_bytes(data)
                    .temp_tag()
                    .await
                    .map_err(|e| Error::Store(format!("storing chunk {hash}: {e}")))?;
            }
            chunks.push(ChunkRef { hash, len });
        }

        let manifest = Manifest::new(chunks);
        let file_id = manifest.file_id();
        self.write_manifest(file_id, &manifest).await?;
        Ok(file_id)
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
        self.manifests_dir
            .join(format!("{}.manifest", file_id.to_hex()))
    }

    /// Durably write `manifest` at `file_id`'s path. If a file already
    /// exists there *and decodes*, it is necessarily byte-identical (the
    /// path is derived from the content hash of the manifest bytes), so
    /// there is nothing to do. If it exists but fails to decode — e.g. a
    /// prior write crashed between `rename` and this process' next start,
    /// on a filesystem where rename can be observed before the data it
    /// pointed at is durable — that is treated as damage to repair, not a
    /// reason to skip the write: without this, `read_to` for that
    /// `FileId` would fail forever.
    async fn write_manifest(&self, file_id: FileId, manifest: &Manifest) -> Result<()> {
        let path = self.manifest_path(file_id);
        if let Ok(existing) = tokio::fs::read(&path).await {
            if Manifest::decode(&existing).is_ok() {
                return Ok(());
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

    async fn read_manifest(&self, file_id: FileId) -> Result<Manifest> {
        let path = self.manifest_path(file_id);
        let bytes = match tokio::fs::read(&path).await {
            Ok(bytes) => bytes,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                return Err(Error::UnknownFileId(file_id.to_hex()));
            }
            Err(e) => return Err(Error::Io(e)),
        };
        Manifest::decode(&bytes)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
