//! `media-proto` — first-class media streaming over vox.
//!
//! The attachments flow (Phase 7) moves blob *bytes* out-of-band over
//! HTTP against signed URLs; that leaves large-media consumers (the
//! session player's stems, part bundles, sample previews) hand-rolling
//! HTTP Range plumbing next to the vox surface. [`MediaService`] is the
//! architect-native alternative: content-addressed reads streamed as
//! [`MediaChunk`]s through a `Tx` lane, so browser, native, and CLI
//! clients all reach media through the same per-org vox router — same
//! origin, same auth path, no second protocol.
//!
//! Ranged reads are explicit (`start`/`len`) so a seeking player pulls
//! exactly the window it needs; `len = u64::MAX` means "to the end".
//! Chunk size is a server concern — consumers must tolerate any
//! chunking and reassemble by `offset`.

use facet::Facet;
use thiserror::Error;

/// Metadata for one content-addressed media blob.
#[derive(Debug, Clone, Facet)]
pub struct MediaInfo {
    /// SHA-256 hex — the blob's identity (same namespace as
    /// attachments; a stem uploaded as an attachment is readable
    /// here by the same hash).
    pub content_hash: String,
    pub size_bytes: u64,
    /// Best-effort mime type from the attachment catalog;
    /// `application/octet-stream` when unknown.
    pub mime_type: String,
    /// Original filename, empty when unknown.
    pub filename: String,
}

/// One streamed slice of a blob. Chunks arrive in order with
/// contiguous, monotonically increasing `offset`s; the stream closing
/// (without error) marks the end of the requested window.
#[derive(Debug, Clone, Facet)]
pub struct MediaChunk {
    /// Absolute byte offset of this chunk within the blob.
    pub offset: u64,
    pub bytes: Vec<u8>,
}

#[derive(Debug, Clone, PartialEq, Eq, Facet, Error)]
#[repr(u8)]
pub enum MediaError {
    #[error("not found")]
    NotFound,
    #[error("invalid range: {0}")]
    InvalidRange(String),
    #[error("internal: {0}")]
    Internal(String),
}

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for MediaInfo {
    type Ref<'a> = MediaInfo;
}
#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for MediaChunk {
    type Ref<'a> = MediaChunk;
}
#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for MediaError {
    type Ref<'a> = MediaError;
}

/// Content-addressed media reads over vox. Mounted per org next to
/// `AttachmentService`; hashes are the same blob-store namespace.
#[cfg(feature = "vox")]
#[vox::service]
pub trait MediaService {
    /// Size + mime for a blob — what a player needs before it
    /// requests windows.
    async fn stat(&self, content_hash: String) -> Result<MediaInfo, MediaError>;

    /// Stream `len` bytes starting at `start` (clamped to the blob's
    /// end; `len = u64::MAX` reads to the end) as ordered
    /// [`MediaChunk`]s on `tx`. Returns after the last chunk is
    /// queued; the closed stream is the end-of-window signal.
    async fn read(
        &self,
        content_hash: String,
        start: u64,
        len: u64,
        tx: vox::Tx<MediaChunk>,
    ) -> Result<(), MediaError>;
}
