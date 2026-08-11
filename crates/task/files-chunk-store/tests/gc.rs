//! Chunk-level GC (issue #258): `ChunkStore::gc`'s manifest mark/sweep is
//! synchronous, but actual chunk-blob reclamation happens on iroh-blobs'
//! own background schedule (see `src/gc.rs`'s module doc for why that's the
//! only deletion path 0.103 exposes publicly) — so tests that need to
//! observe reclamation poll with a bounded timeout against a short GC
//! interval, rather than asserting immediately after `gc` returns.

use std::collections::BTreeSet;
use std::time::{Duration, SystemTime};

use task_files_chunk_store::{ChunkStore, ChunkerConfig, Error, GcConfig};

const POLL_INTERVAL: Duration = Duration::from_millis(20);
const POLL_TIMEOUT: Duration = Duration::from_secs(5);

async fn open_gc_store(dir: &std::path::Path) -> ChunkStore {
    ChunkStore::open_with_gc(
        dir,
        ChunkerConfig::default(),
        GcConfig {
            interval: Duration::from_millis(30),
        },
    )
    .await
    .unwrap()
}

/// Poll `chunk_count` until it drops below `before`, or time out. Returns
/// the final count either way so callers can assert on it.
async fn wait_for_chunk_count_below(store: &ChunkStore, before: usize) -> usize {
    let deadline = tokio::time::Instant::now() + POLL_TIMEOUT;
    loop {
        let count = store.chunk_count().await.unwrap();
        if count < before || tokio::time::Instant::now() >= deadline {
            return count;
        }
        tokio::time::sleep(POLL_INTERVAL).await;
    }
}

/// "Unreferenced expired snapshots are swept; their unshared chunks are
/// reclaimed."
#[tokio::test]
async fn unreferenced_expired_manifest_is_swept_and_its_chunk_reclaimed() {
    let dir = tempfile::tempdir().unwrap();
    let store = open_gc_store(dir.path()).await;

    let file_id = store
        .write_stream(&b"a file nobody keeps"[..])
        .await
        .unwrap();
    let before = store.chunk_count().await.unwrap();
    assert!(
        before > 0,
        "the write should have stored at least one chunk"
    );

    // Let the manifest age past `keep_newer`.
    tokio::time::sleep(Duration::from_millis(10)).await;
    let keep_newer = SystemTime::now();

    let stats = store.gc(&BTreeSet::new(), keep_newer).await.unwrap();
    assert_eq!(stats.manifests_swept, 1);
    assert!(stats.chunks_marked_for_reclamation > 0);

    // The manifest is gone immediately (gc's mark phase is synchronous).
    assert!(!store.has(file_id).await);

    // The chunk itself is reclaimed on iroh-blobs' own schedule.
    let after = wait_for_chunk_count_below(&store, before).await;
    assert!(
        after < before,
        "expected the unshared chunk to be reclaimed: before={before} after={after}"
    );
}

/// "Protect-callback-listed versions survive GC regardless of age."
#[tokio::test]
async fn protected_manifest_survives_gc_regardless_of_age() {
    let dir = tempfile::tempdir().unwrap();
    let store = open_gc_store(dir.path()).await;

    let file_id = store
        .write_stream(&b"a version the Vault still points at"[..])
        .await
        .unwrap();
    tokio::time::sleep(Duration::from_millis(10)).await;
    let keep_newer = SystemTime::now();

    let mut protected = BTreeSet::new();
    protected.insert(file_id);
    let stats = store.gc(&protected, keep_newer).await.unwrap();
    assert_eq!(stats.manifests_swept, 0);

    assert!(store.has(file_id).await);
    assert_eq!(
        store.read_to_vec(file_id).await.unwrap(),
        b"a version the Vault still points at"
    );
}

/// "keep_newer guards concurrent writers (nothing newer is swept)."
#[tokio::test]
async fn keep_newer_protects_a_manifest_written_after_the_cutoff() {
    let dir = tempfile::tempdir().unwrap();
    let store = open_gc_store(dir.path()).await;

    let keep_newer = SystemTime::now();
    tokio::time::sleep(Duration::from_millis(10)).await;
    // Written *after* keep_newer, and never protected: only its mtime saves it.
    let file_id = store
        .write_stream(&b"written after the gc cutoff"[..])
        .await
        .unwrap();

    let stats = store.gc(&BTreeSet::new(), keep_newer).await.unwrap();
    assert_eq!(
        stats.manifests_swept, 0,
        "a manifest newer than keep_newer must never be swept"
    );
    assert!(store.has(file_id).await);
}

/// `ChunkStore::gc` on a store opened without GC enabled has nothing that
/// will ever reclaim the chunks a manifest removal would orphan, so it
/// refuses rather than silently leaking or lying about what it did.
#[tokio::test]
async fn gc_is_disabled_without_open_with_gc() {
    let dir = tempfile::tempdir().unwrap();
    let store = ChunkStore::open(dir.path()).await.unwrap();

    let err = store
        .gc(&BTreeSet::new(), SystemTime::now())
        .await
        .unwrap_err();
    assert!(
        matches!(err, Error::GcDisabled),
        "expected GcDisabled, got {err:?}"
    );
}
