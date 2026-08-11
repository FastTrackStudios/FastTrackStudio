//! The cadence engine (issue #260) end to end over an in-process
//! `architect::LocalServer` — the spec's Testing Decisions primary seam
//! ("assert external behavior at the RPC seam ... advance a virtual
//! clock through debounce/quiescence, then assert what the service
//! reports"), with `TestClock` standing in for the wall clock so a
//! 30-minute quiescence window costs no wall-clock time at all.
//!
//! One acceptance criterion per test, plus the watcher (which is the
//! one part that has to touch a real filesystem to mean anything).

use std::sync::{Arc, Mutex};
use std::time::Duration;

use architect::{LayerRouter, LocalServer, Scope};
use chrono::TimeDelta;
use files::service::FilesServiceStreamSource as _;
use files::{
    CadenceConfig, FileRootInfo, FilesBackend, FilesEvent, FilesServiceClient,
    FilesServiceStreamClient, RootFlavor, TestClock, files_service_layer,
    files_service_stream_layer,
};
use uuid::Uuid;

fn router(backend: FilesBackend) -> LayerRouter {
    LayerRouter::new()
        .merge(files_service_layer(backend.clone()))
        .merge(files_service_stream_layer(backend))
}

/// A backend on a `TestClock`, its RPC client, and a live collector of
/// everything `#[subscribe] fn events` emits — the whole harness every
/// test below runs on.
struct Harness {
    _data_dir: tempfile::TempDir,
    root_dir: std::path::PathBuf,
    backend: FilesBackend,
    client: FilesServiceClient,
    clock: Arc<TestClock>,
    events: Arc<Mutex<Vec<FilesEvent>>>,
    scope: std::sync::Arc<Scope>,
    _subscription: tokio::task::JoinHandle<()>,
    _collector: tokio::task::JoinHandle<()>,
}

impl Harness {
    async fn start(config: CadenceConfig) -> Self {
        let data_dir = tempfile::tempdir().expect("data tempdir");
        // Roots are confined to the org's own files area (PR #280
        // review), so the "existing folder" is staged under it.
        let root_dir = data_dir.path().join("El Artisa");
        std::fs::create_dir(&root_dir).unwrap();

        let clock = Arc::new(TestClock::default());
        let backend =
            FilesBackend::with_cadence(data_dir.path(), config, clock.clone()).expect("backend");
        let scope = Scope::new();
        let local = LocalServer::serve(router(backend.clone()), scope.clone());
        let client: FilesServiceClient = local.establish().await.expect("establish client");
        let stream: FilesServiceStreamClient =
            local.establish().await.expect("establish stream client");

        let (tx, mut rx) = vox::channel::<FilesEvent>();
        let subscription = tokio::spawn(async move {
            let _ = stream.events(tx).await;
        });
        let events = Arc::new(Mutex::new(Vec::new()));
        let sink = events.clone();
        let collector = tokio::spawn(async move {
            while let Ok(Some(frame)) = rx.recv().await {
                let mut copied = None;
                let _ = frame.map(|ev| copied = Some(ev));
                if let Some(ev) = copied {
                    sink.lock().expect("event sink poisoned").push(ev);
                }
            }
        });
        // Subscribe before mutating: the sink has to have reached the
        // hub or the first events are simply missed.
        tokio::time::timeout(Duration::from_secs(10), async {
            while backend.events_hub().subscriber_count() == 0 {
                tokio::time::sleep(Duration::from_millis(5)).await;
            }
        })
        .await
        .expect("subscriber sink never reached the backend hub");

        // `local` is kept alive by the client handles; dropping it here
        // is fine (the served router lives on the scope).
        drop(local);

        Self {
            _data_dir: data_dir,
            root_dir,
            backend,
            client,
            clock,
            events,
            scope,
            _subscription: subscription,
            _collector: collector,
        }
    }

    async fn create_root(&self) -> FileRootInfo {
        self.client
            .create_root(
                self.root_dir.to_str().unwrap().to_string(),
                "El Artisa".to_string(),
                RootFlavor::Media,
            )
            .await
            .expect("create_root rpc")
    }

    fn write(&self, rel: &str, content: &[u8]) {
        let path = self.root_dir.join(rel);
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).unwrap();
        }
        std::fs::write(path, content).unwrap();
    }

    /// Write a file and tell the service about it, exactly as a watcher
    /// (or a sync daemon) would.
    async fn write_and_hint(&self, root_id: Uuid, rel: &str, content: &[u8]) -> u32 {
        self.write(rel, content);
        self.client
            .hint_activity(root_id, vec![rel.to_string()])
            .await
            .expect("hint_activity rpc")
    }

    /// Let the event collector catch up, then count events of a kind.
    async fn counts(&self) -> (usize, usize) {
        tokio::task::yield_now().await;
        tokio::time::sleep(Duration::from_millis(20)).await;
        let events = self.events.lock().expect("event sink poisoned");
        let snapshots = events
            .iter()
            .filter(|e| matches!(e, FilesEvent::Snapshotted(_)))
            .count();
        let checkpoints = events
            .iter()
            .filter(|e| matches!(e, FilesEvent::Checkpointed(_)))
            .count();
        (snapshots, checkpoints)
    }

    async fn shutdown(self) {
        self.backend.shutdown().await;
        self.scope.close().await;
    }
}

/// Acceptance criterion 1: "A simulated recording storm (files every
/// few minutes) yields auto-snapshots but exactly one Session
/// checkpoint at quiescence."
#[tokio::test(flavor = "multi_thread")]
async fn a_recording_storm_yields_snapshots_and_exactly_one_checkpoint() {
    let harness = Harness::start(CadenceConfig::default()).await;
    let root = harness.create_root().await;

    // A tracking day: a take lands every 3 minutes for 45 minutes, then
    // everyone goes home. The driver ticks every minute throughout.
    for minute in 0..150u32 {
        if minute <= 42 && minute % 3 == 0 {
            harness
                .write_and_hint(
                    root.id,
                    "Audio Files/gtr.wav",
                    format!("take at minute {minute}").as_bytes(),
                )
                .await;
        }
        harness.backend.tick().await;
        harness.clock.advance(TimeDelta::minutes(1));
    }

    let (snapshots, checkpoints) = harness.counts().await;
    assert_eq!(
        checkpoints, 1,
        "exactly one Session checkpoint, minted at quiescence — not one per tick"
    );
    assert!(
        snapshots >= 4,
        "the storm should have been captured by auto-snapshots along the way, got {snapshots}"
    );

    // The snapshots are real, listed, and recoverable...
    let listed = harness
        .client
        .snapshots(root.id)
        .await
        .expect("snapshots rpc");
    assert_eq!(listed.len(), snapshots, "every snapshot is listed");

    // ...and none of them is a version: the file's chain holds exactly
    // the one checkpoint (glossary: an auto-snapshot is "never a chain
    // entry").
    let chain = harness
        .client
        .chain(root.id, "Audio Files/gtr.wav".to_string())
        .await
        .expect("chain rpc");
    assert_eq!(
        chain.len(),
        1,
        "snapshots must not surface as version-chain entries: {chain:?}"
    );

    // A quiescent root stays quiescent: more ticks, no more captures.
    for _ in 0..10 {
        harness.backend.tick().await;
        harness.clock.advance(TimeDelta::minutes(10));
    }
    let (_, checkpoints_after) = harness.counts().await;
    assert_eq!(checkpoints_after, 1, "a silent root mints no checkpoints");

    harness.shutdown().await;
}

/// Acceptance criterion 2: "A project-file save marks a save point
/// visible in the chain metadata."
#[tokio::test(flavor = "multi_thread")]
async fn a_project_file_save_marks_a_save_point_visible_in_the_chain() {
    let harness = Harness::start(CadenceConfig::default()).await;
    let root = harness.create_root().await;

    // An ordinary media write is not a save point; saving the REAPER
    // project is.
    harness
        .write_and_hint(root.id, "Audio Files/gtr.wav", b"take 1")
        .await;
    harness.clock.advance(TimeDelta::minutes(2));
    harness
        .write_and_hint(root.id, "El Artisa.rpp", b"<REAPER_PROJECT> take 1")
        .await;

    // Mid-session: the save point rides the nearest auto-snapshot.
    harness.clock.advance(TimeDelta::minutes(11));
    harness.backend.tick().await;
    let snapshots = harness
        .client
        .snapshots(root.id)
        .await
        .expect("snapshots rpc");
    assert_eq!(snapshots.len(), 1, "one auto-snapshot so far");
    assert_eq!(
        snapshots[0]
            .save_points
            .iter()
            .map(|s| s.path.as_str())
            .collect::<Vec<_>>(),
        ["El Artisa.rpp"],
        "only the project-file save marks a save point"
    );

    // At quiescence the session's checkpoint carries the same save
    // point, and it is visible on the chain entry that checkpoint
    // produced — the criterion's "chain metadata".
    harness.clock.advance(TimeDelta::minutes(31));
    harness.backend.tick().await;

    let chain = harness
        .client
        .chain(root.id, "El Artisa.rpp".to_string())
        .await
        .expect("chain rpc");
    assert_eq!(chain.len(), 1, "one checkpointed state: {chain:?}");
    assert_eq!(
        chain[0]
            .save_points
            .iter()
            .map(|s| s.path.as_str())
            .collect::<Vec<_>>(),
        ["El Artisa.rpp"],
        "the save point is chain metadata: {:?}",
        chain[0]
    );

    // The same metadata rides the audio file's chain entry too — a save
    // point belongs to the session, not to one path.
    let audio_chain = harness
        .client
        .chain(root.id, "Audio Files/gtr.wav".to_string())
        .await
        .expect("chain rpc");
    assert_eq!(audio_chain[0].save_points.len(), 1);

    harness.shutdown().await;
}

/// Acceptance criterion 3: "Ignore-set patterns (e.g. rpp-bak) never
/// enter the store (El Artisa fixture shape as the test case)." The
/// fixture's shape is a REAPER session directory whose every save left
/// another `.rpp-bak` behind, next to the peak caches REAPER rebuilds
/// from the audio itself.
#[tokio::test(flavor = "multi_thread")]
async fn ignored_patterns_never_enter_the_store() {
    let harness = Harness::start(CadenceConfig::default()).await;
    let root = harness.create_root().await;

    harness.write("El Artisa.rpp", b"<REAPER_PROJECT>");
    harness.write("El Artisa.rpp-bak", b"<REAPER_PROJECT> older");
    harness.write("El Artisa-2026-08-01.rpp-bak", b"<REAPER_PROJECT> oldest");
    harness.write("Audio Files/gtr.wav", b"guitar");
    harness.write("Audio Files/gtr.reapeaks", b"peak cache");
    harness.write("Audio Files/.DS_Store", b"finder junk");

    // The set is seeded from the root's flavor at creation.
    let ignore = harness
        .client
        .ignore_set(root.id)
        .await
        .expect("ignore_set rpc");
    assert!(ignore.contains(&"*.rpp-bak".to_string()), "{ignore:?}");
    assert!(ignore.contains(&"*.reapeaks".to_string()), "{ignore:?}");

    // Backup churn is not activity: hinting only ignored paths opens no
    // session at all.
    let accepted = harness
        .client
        .hint_activity(
            root.id,
            vec![
                "El Artisa.rpp-bak".to_string(),
                "Audio Files/gtr.reapeaks".to_string(),
            ],
        )
        .await
        .expect("hint_activity rpc");
    assert_eq!(accepted, 0, "ignored hints are dropped");
    assert!(!harness.backend.cadence().session_open(root.id));

    let checkpoint = harness
        .client
        .checkpoint_now(root.id, None)
        .await
        .expect("checkpoint_now rpc");
    assert_eq!(
        checkpoint.changed_paths,
        vec![
            "Audio Files/gtr.wav".to_string(),
            "El Artisa.rpp".to_string(),
        ],
        "only the work is versioned, never the scaffolding"
    );

    for junk in [
        "El Artisa.rpp-bak",
        "El Artisa-2026-08-01.rpp-bak",
        "Audio Files/gtr.reapeaks",
        "Audio Files/.DS_Store",
    ] {
        let chain = harness
            .client
            .chain(root.id, junk.to_string())
            .await
            .expect("chain rpc");
        assert!(chain.is_empty(), "{junk} must never enter the store");
    }

    // The set is editable per root, and the edit takes effect on the
    // next capture.
    let stored = harness
        .client
        .set_ignore_set(root.id, vec!["*.wav".to_string(), " *.wav ".to_string()])
        .await
        .expect("set_ignore_set rpc");
    assert_eq!(
        stored,
        vec!["*.wav".to_string()],
        "normalized on the way in"
    );

    harness.write("Audio Files/bass.wav", b"bass");
    harness.write("El Artisa.rpp", b"<REAPER_PROJECT> v2");
    let after = harness
        .client
        .checkpoint_now(root.id, None)
        .await
        .expect("checkpoint_now rpc");
    assert!(
        !after.changed_paths.iter().any(|p| p.ends_with("bass.wav")),
        "a newly ignored pattern keeps its files out: {:?}",
        after.changed_paths
    );

    // A bad glob is rejected rather than silently ignoring nothing.
    assert!(
        harness
            .client
            .set_ignore_set(root.id, vec!["[".to_string()])
            .await
            .is_err(),
        "an invalid glob must be rejected"
    );

    harness.shutdown().await;
}

/// Acceptance criteria 4 and 5: "Checkpoint certification runs a full
/// stat-scan; a file changing mid-hash is requeued, not corrupted" and
/// "In-flight writes ride into the next snapshot without failing the
/// current one."
#[tokio::test(flavor = "multi_thread")]
async fn a_file_changing_mid_hash_is_requeued_and_rides_into_the_next_capture() {
    let harness = Harness::start(CadenceConfig::default()).await;
    let root = harness.create_root().await;

    harness.write("Renders/mix.wav", b"render v1");
    let first = harness
        .client
        .checkpoint_now(root.id, Some("v1".to_string()))
        .await
        .expect("checkpoint_now rpc");
    assert!(first.requeued_paths.is_empty(), "nothing was moving");
    let settled = harness
        .client
        .chain(root.id, "Renders/mix.wav".to_string())
        .await
        .expect("chain rpc");
    assert_eq!(settled.len(), 1);
    let settled_file_id = settled[0].file_id.clone();

    // Now a bounce is in progress: every time the scan reads the file,
    // the "renderer" has already appended more to it. The hook fires
    // between the pre-read stat and the read itself, which is exactly
    // what a mid-hash write looks like to the certifying scan.
    let render_path = harness.root_dir.join("Renders/mix.wav");
    let writes = Arc::new(std::sync::atomic::AtomicUsize::new(0));
    let writes_seen = writes.clone();
    harness
        .backend
        .set_mid_hash_hook(Some(Arc::new(move |path: &std::path::Path| {
            if path == render_path {
                let n = writes_seen.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                std::fs::write(path, format!("render v2, {} more blocks", n + 1)).unwrap();
            }
        })));

    // A quiet file lands in the same capture — the in-flight one must
    // not fail it.
    harness.write("Session/notes.txt", b"bounce running");
    let during = harness
        .client
        .checkpoint_now(root.id, Some("mid-bounce".to_string()))
        .await
        .expect("a capture with an in-flight write must still succeed");
    assert_eq!(
        during.requeued_paths,
        vec!["Renders/mix.wav".to_string()],
        "the moving file is requeued, by name"
    );
    assert_eq!(
        during.changed_paths,
        vec!["Session/notes.txt".to_string()],
        "everything that was settled still got captured"
    );
    assert!(
        writes.load(std::sync::atomic::Ordering::SeqCst) > 1,
        "certification retried the moving file before giving up"
    );

    // Not corrupted: the file's newest version is still the last state
    // that was actually coherent, not a torn read of the bounce.
    let chain = harness
        .client
        .chain(root.id, "Renders/mix.wav".to_string())
        .await
        .expect("chain rpc");
    assert_eq!(chain.len(), 1, "no torn version was committed: {chain:?}");
    assert_eq!(chain[0].file_id, settled_file_id);

    // The bounce finishes. The next capture picks the file up — it rode
    // into it rather than being lost.
    harness.backend.set_mid_hash_hook(None);
    let after = harness
        .client
        .checkpoint_now(root.id, Some("bounce done".to_string()))
        .await
        .expect("checkpoint_now rpc");
    assert!(after.requeued_paths.is_empty());
    assert!(
        after.changed_paths.contains(&"Renders/mix.wav".to_string()),
        "the requeued file rides into the next capture: {:?}",
        after.changed_paths
    );
    let chain = harness
        .client
        .chain(root.id, "Renders/mix.wav".to_string())
        .await
        .expect("chain rpc");
    assert_eq!(chain.len(), 2, "and becomes a version of its own");
    assert_ne!(chain[0].file_id, settled_file_id);

    harness.shutdown().await;
}

/// The watcher half: hints reach the cadence engine from a real
/// filesystem event, with no explicit `hint_activity` call. Runs on the
/// real clock's *events* (the cadence itself is still `TestClock`) —
/// the assertion is only that a write opens a session.
#[tokio::test(flavor = "multi_thread")]
async fn the_watcher_hints_activity_into_the_cadence() {
    let harness = Harness::start(CadenceConfig::default()).await;
    let root = harness.create_root().await;
    harness.backend.watch_root(root.id).expect("watch_root");

    // Ignored churn first: a `.rpp-bak` storm must not open a session
    // even when the watcher faithfully reports it.
    harness.write("El Artisa.rpp-bak", b"backup");
    tokio::time::sleep(Duration::from_millis(300)).await;
    assert!(
        !harness.backend.cadence().session_open(root.id),
        "the watcher's hints still go through the Ignore set"
    );

    harness.write("Audio Files/gtr.wav", b"real work");
    let opened = tokio::time::timeout(Duration::from_secs(10), async {
        while !harness.backend.cadence().session_open(root.id) {
            tokio::time::sleep(Duration::from_millis(25)).await;
        }
    })
    .await;
    assert!(
        opened.is_ok(),
        "a real write should have reached the cadence engine as a hint"
    );

    harness.backend.unwatch_root(root.id);
    harness.shutdown().await;
}
