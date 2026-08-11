//! [`ChunkStore`]: the on-disk pairing of an iroh-blobs `FsStore` (chunk
//! bytes, content-addressed by blake3) with a manifests directory (Files'
//! own `FileId -> chunk list` records, kept outside iroh-blobs per
//! ADR 0001).

use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

use fastcdc::v2020::AsyncStreamCDC;
use futures::StreamExt;
use tokio::io::{AsyncRead, AsyncWrite};

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
                self.blobs
                    .add_bytes(data)
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
            let mut reader = self.blobs.reader(*chunk.hash.as_bytes());
            let copied = tokio::io::copy(&mut reader, dest).await?;
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

    async fn write_manifest(&self, file_id: FileId, manifest: &Manifest) -> Result<()> {
        let path = self.manifest_path(file_id);
        // FileId is a content hash of the manifest itself, so if a
        // manifest already exists at this path its bytes are necessarily
        // identical — nothing to do.
        if tokio::fs::try_exists(&path).await.unwrap_or(false) {
            return Ok(());
        }
        // Write-then-rename so a crash mid-write never leaves a partial
        // manifest visible at the final path. The temp name must be
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
        tokio::fs::write(&tmp_path, manifest.encode()).await?;
        tokio::fs::rename(&tmp_path, &path).await?;
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
