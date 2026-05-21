//! Manifest types — one [`ManifestEntry`] per file in a vault,
//! aggregated into a [`Manifest`] returned by
//! [`crate::VaultSync::manifest`]. Used by clients to compute
//! diffs against their local state before pulling individual
//! files.

use facet::Facet;

/// One file's metadata: relative path, sha256 (lowercase hex),
/// last-modified time, byte length.
#[derive(Debug, Clone, PartialEq, Eq, Facet)]
pub struct ManifestEntry {
    pub path: String,
    pub sha256: String,
    pub mtime_ms: i64,
    pub size: u64,
}

/// Full file listing for one vault. `vault_id` echoes the
/// request so a client juggling several vaults can double-check
/// what came back. `files` is unordered — sort client-side if a
/// stable order matters.
#[derive(Debug, Clone, Facet)]
pub struct Manifest {
    pub vault_id: String,
    pub files: Vec<ManifestEntry>,
}

#[cfg(feature = "vox")]
#[allow(unsafe_code)]
mod reborrow_impls {
    use super::{Manifest, ManifestEntry};
    unsafe impl vox_types::Reborrow for ManifestEntry {
        type Ref<'a> = ManifestEntry;
    }
    unsafe impl vox_types::Reborrow for Manifest {
        type Ref<'a> = Manifest;
    }
}
