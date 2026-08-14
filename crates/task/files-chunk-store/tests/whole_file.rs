//! Large files are stored WHOLE — one blob, imported by path so the
//! filesystem can bring it in with a reflink instead of a copy — while
//! small ones keep chunking. See `ChunkStore::write_path`.
//!
//! The reflink itself is opportunistic (XFS `reflink=1`, btrfs; a plain
//! copy elsewhere), so these tests assert the properties that must hold
//! *regardless* of whether the clone happened: the same content address
//! either way, honest round-trips, and — the one that silently ruins
//! everything if it breaks — probe and write agreeing about which side
//! of the threshold a file is on.

use std::path::Path;

use task_files_chunk_store::{ChunkStore, ChunkerConfig};

const SMALL_THRESHOLD: u64 = 1024 * 1024;

async fn store_at(root: &Path) -> ChunkStore {
    ChunkStore::open_with_config(
        root,
        ChunkerConfig::default().with_whole_file_threshold(SMALL_THRESHOLD),
    )
    .await
    .unwrap()
}

/// Varied enough to produce many chunk boundaries when chunked, so the
/// whole-file path is genuinely doing something different.
fn content(len: usize) -> Vec<u8> {
    (0..len)
        .map(|i| ((i as u32).wrapping_mul(2_654_435_761) >> 13) as u8)
        .collect()
}

/// Genuinely incompressible bytes. The free-space measurement below is
/// meaningless on a transparently-compressing filesystem — this repo's
/// own `/tmp` is btrfs with `compress=zstd` — unless the content resists
/// compression the way real media does.
fn incompressible(len: usize) -> Vec<u8> {
    use std::io::Read as _;
    let mut buf = vec![0u8; len];
    std::fs::File::open("/dev/urandom")
        .expect("/dev/urandom")
        .read_exact(&mut buf)
        .expect("reading random bytes");
    buf
}

#[tokio::test]
async fn a_large_file_is_stored_as_one_blob_and_reads_back_whole() {
    let dir = tempfile::tempdir().unwrap();
    let store = store_at(dir.path()).await;
    let bytes = content(4 * 1024 * 1024);
    let path = dir.path().join("big.wav");
    tokio::fs::write(&path, &bytes).await.unwrap();

    let file_id = store.write_path(&path).await.unwrap();

    let manifest = store.manifest(file_id).await.unwrap();
    assert_eq!(
        manifest.chunks.len(),
        1,
        "a file above the threshold must be one whole blob, not chunked"
    );
    assert_eq!(manifest.chunks[0].len, bytes.len() as u64);
    assert_eq!(
        manifest.chunks[0].hash,
        blake3::hash(&bytes),
        "the single entry's hash must be the blake3 of the whole file"
    );
    assert_eq!(store.read_to_vec(file_id).await.unwrap(), bytes);
    assert_eq!(
        store.content_len(file_id).await.unwrap(),
        bytes.len() as u64
    );
}

#[tokio::test]
async fn a_small_file_still_chunks_and_matches_the_streaming_write() {
    let dir = tempfile::tempdir().unwrap();
    let store = store_at(dir.path()).await;
    let bytes = content(200 * 1024); // under SMALL_THRESHOLD
    let path = dir.path().join("small.txt");
    tokio::fs::write(&path, &bytes).await.unwrap();

    let by_path = store.write_path(&path).await.unwrap();
    let by_stream = store.write_stream(&bytes[..]).await.unwrap();
    assert_eq!(
        by_path, by_stream,
        "below the threshold, write_path is write_stream — same id"
    );
    assert_eq!(store.read_to_vec(by_path).await.unwrap(), bytes);
}

/// The invariant that quietly destroys everything if it breaks: a
/// whole-stored file and a chunked one have *different* ids for the same
/// bytes, so if `probe_path` made a different size decision than
/// `write_path`, every capture would see every large file as changed and
/// re-import the whole tree — forever, on a tree that never changed.
#[tokio::test]
async fn probe_agrees_with_write_on_both_sides_of_the_threshold() {
    let dir = tempfile::tempdir().unwrap();
    let store = store_at(dir.path()).await;

    for (name, len) in [
        ("under.bin", (SMALL_THRESHOLD - 1) as usize),
        ("exactly.bin", SMALL_THRESHOLD as usize),
        ("over.bin", (SMALL_THRESHOLD * 3) as usize),
    ] {
        let path = dir.path().join(name);
        tokio::fs::write(&path, content(len)).await.unwrap();

        let probed = store.probe_path(&path).await.unwrap();
        let written = store.write_path(&path).await.unwrap();
        assert_eq!(
            probed, written,
            "{name}: probe_path must predict write_path"
        );
    }
}

/// A whole-file blob can be the entire multi-hundred-GB file, so serving
/// a `<video>` seek must read only the window — and, being one "chunk",
/// it exercises a code path the chunked case never reaches.
#[tokio::test]
async fn range_reads_work_over_a_whole_file_blob() {
    let dir = tempfile::tempdir().unwrap();
    let store = store_at(dir.path()).await;
    let bytes = content(3 * 1024 * 1024);
    let path = dir.path().join("seekable.mov");
    tokio::fs::write(&path, &bytes).await.unwrap();
    let file_id = store.write_path(&path).await.unwrap();

    let mut full = Vec::new();
    store
        .read_range(file_id, 0, bytes.len() as u64, &mut full)
        .await
        .unwrap();
    assert_eq!(full, bytes, "full range == whole file");

    for (start, len) in [
        (0u64, 10u64),
        (1_000_000, 65_536),
        (bytes.len() as u64 - 5, 50), // straddles the end: clamped
    ] {
        let mut got = Vec::new();
        store
            .read_range(file_id, start, len, &mut got)
            .await
            .unwrap();
        let end = (start + len).min(bytes.len() as u64) as usize;
        assert_eq!(got, bytes[start as usize..end], "window {start}+{len}");
    }
}

/// Re-storing an unchanged file must be a no-op, not a second clone —
/// this is the property the whole change exists to buy (a nightly
/// snapshot of a tree nobody touched should cost nothing).
#[tokio::test]
async fn rewriting_an_unchanged_large_file_is_idempotent() {
    let dir = tempfile::tempdir().unwrap();
    let store = store_at(dir.path()).await;
    let path = dir.path().join("stable.braw");
    tokio::fs::write(&path, content(2 * 1024 * 1024))
        .await
        .unwrap();

    let first = store.write_path(&path).await.unwrap();
    let before = store.chunk_count().await.unwrap();
    let second = store.write_path(&path).await.unwrap();
    let after = store.chunk_count().await.unwrap();

    assert_eq!(first, second);
    assert_eq!(
        before, after,
        "re-storing identical content must not add a blob"
    );
}

/// Free blocks on the filesystem holding `path`, in bytes — taken after
/// a `sync`, because a filesystem with delayed allocation (btrfs, ext4)
/// does not charge a write against free space until it commits. Without
/// it, an unmeasured full 256 MiB copy reads back as costing nothing and
/// the assertion below passes for entirely the wrong reason (confirmed:
/// it did, until this line existed).
fn free_bytes(path: &Path) -> u64 {
    let _ = std::process::Command::new("sync").status();
    let out = std::process::Command::new("stat")
        .args(["-f", "-c", "%f %S"])
        .arg(path)
        .output()
        .expect("stat -f");
    let text = String::from_utf8_lossy(&out.stdout);
    let mut parts = text.split_whitespace();
    let blocks: u64 = parts.next().unwrap().parse().unwrap();
    let size: u64 = parts.next().unwrap().parse().unwrap();
    blocks * size
}

/// Can this filesystem clone extents at all? Asked by trying it, not by
/// naming filesystems — a test that hard-codes "xfs means yes" is wrong
/// the moment someone builds on an XFS made without `reflink=1`.
fn supports_reflink(dir: &Path) -> bool {
    let src = dir.join(".reflink-probe-src");
    let dst = dir.join(".reflink-probe-dst");
    if std::fs::write(&src, vec![7u8; 1024 * 1024]).is_err() {
        return false;
    }
    let ok = std::process::Command::new("cp")
        .arg("--reflink=always")
        .args([&src, &dst])
        .status()
        .map(|s| s.success())
        .unwrap_or(false);
    let _ = std::fs::remove_file(&src);
    let _ = std::fs::remove_file(&dst);
    ok
}

/// The point of the whole exercise, measured rather than asserted from
/// the API surface: storing a large file on a reflink-capable filesystem
/// must not consume another copy of it.
///
/// Skipped where the filesystem cannot clone — there the fallback is a
/// real copy and that is correct, just slow. Deliberately generous
/// (under a quarter of the file) because this reads *filesystem* free
/// space, which other processes also move; the failure it is meant to
/// catch is a full second copy, which is 4x outside that band.
#[tokio::test]
async fn storing_a_large_file_does_not_consume_a_second_copy() {
    let dir = tempfile::tempdir().unwrap();
    if !supports_reflink(dir.path()) {
        eprintln!(
            "skipping: {} is not on a reflink-capable filesystem",
            dir.path().display()
        );
        return;
    }
    let store = store_at(dir.path()).await;

    // Big enough that a full copy is unmistakable against filesystem
    // noise, small enough to stay quick.
    let bytes = incompressible(256 * 1024 * 1024);
    let path = dir.path().join("camera-original.braw");
    tokio::fs::write(&path, &bytes).await.unwrap();
    store.shutdown().await.unwrap();
    let store = store_at(dir.path()).await;

    let before = free_bytes(dir.path());
    let file_id = store.write_path(&path).await.unwrap();
    store.shutdown().await.unwrap();
    let after = free_bytes(dir.path());

    let consumed = before.saturating_sub(after);
    let budget = bytes.len() as u64 / 4;
    assert!(
        consumed < budget,
        "storing a {} MiB file consumed {} MiB — that is a copy, not a clone",
        bytes.len() / 1024 / 1024,
        consumed / 1024 / 1024,
    );

    // And it is a real, readable version — a clone that couldn't be read
    // back would be a very cheap way to lose data.
    let store = store_at(dir.path()).await;
    assert_eq!(store.read_to_vec(file_id).await.unwrap(), bytes);
}

/// Editing the live file must not disturb the version already stored —
/// with a reflink that means copy-on-write does its job, and without one
/// it is trivially true. Either way, history stays real.
#[tokio::test]
async fn editing_the_live_file_leaves_the_stored_version_intact() {
    let dir = tempfile::tempdir().unwrap();
    let store = store_at(dir.path()).await;
    let original = content(2 * 1024 * 1024);
    let path = dir.path().join("edited.wav");
    tokio::fs::write(&path, &original).await.unwrap();
    let v1 = store.write_path(&path).await.unwrap();

    let mut edited = original.clone();
    edited[..4096].fill(0xAB);
    edited.extend_from_slice(&content(1024));
    tokio::fs::write(&path, &edited).await.unwrap();
    let v2 = store.write_path(&path).await.unwrap();

    assert_ne!(v1, v2);
    assert_eq!(
        store.read_to_vec(v1).await.unwrap(),
        original,
        "the first version must still read back byte-for-byte"
    );
    assert_eq!(store.read_to_vec(v2).await.unwrap(), edited);
}
