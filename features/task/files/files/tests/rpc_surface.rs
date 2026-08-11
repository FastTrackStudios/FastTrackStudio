//! The Files RPC surface v1 (issue #259) end to end over an in-process
//! `architect::LocalServer` — the spec's Testing Decisions primary
//! seam ("the established idiom ... the session facade's memory-link
//! bootstrap tests are the prior art"), mirroring `task`'s own
//! `tests/events_stream.rs`.
//!
//! Covers every acceptance criterion, plus the PR #280 review's
//! findings: org confinement (`create_root`/`drive_browse` can't reach
//! outside the org's own files area), root-browse escape (absolute
//! subpath, `..`, symlink), nested-root rejection, `changed_paths`
//! accuracy, the concurrent-checkpoint race, and root identity
//! surviving a genuine `FilesBackend` restart.

use std::time::Duration;

use architect::{LayerRouter, LocalServer, Scope};
use files::service::FilesServiceStreamSource as _;
use files::{
    FileRootInfo, FilesBackend, FilesEvent, FilesServiceClient, FilesServiceStreamClient,
    RootFlavor, files_service_layer, files_service_stream_layer,
};

fn router(backend: FilesBackend) -> LayerRouter {
    LayerRouter::new()
        .merge(files_service_layer(backend.clone()))
        .merge(files_service_stream_layer(backend))
}

async fn next_event(rx: &mut vox::Rx<FilesEvent>) -> FilesEvent {
    let frame = tokio::time::timeout(Duration::from_secs(10), rx.recv())
        .await
        .expect("timed out waiting for a FilesEvent")
        .expect("event channel errored")
        .expect("event stream closed early");
    let mut copied = None;
    let _ = frame.map(|ev| copied = Some(ev));
    copied.expect("SelfRef::map ran")
}

#[tokio::test(flavor = "multi_thread")]
async fn create_browse_chain_checkpoint_over_rpc() {
    let data_dir = tempfile::tempdir().expect("data tempdir");
    // Roots are confined to the org's own files area (PR #280 review) —
    // `data_dir` doubles as both `FilesBackend`'s data root and its
    // confinement boundary, so a producer's "existing folder" must
    // already be staged somewhere under it (e.g. an inbox the sync
    // daemon populated) rather than anywhere on the server.
    let root_dir = data_dir.path().join("mix-session");
    std::fs::create_dir(&root_dir).unwrap();
    std::fs::write(root_dir.join("mix.wav"), b"take one").unwrap();
    std::fs::create_dir(root_dir.join("stems")).unwrap();
    std::fs::write(root_dir.join("stems").join("kick.wav"), b"boom").unwrap();

    let backend = FilesBackend::new(data_dir.path()).expect("backend");
    let scope = Scope::new();
    let local = LocalServer::serve(router(backend.clone()), scope.clone());

    let client: FilesServiceClient = local
        .establish()
        .await
        .expect("establish FilesServiceClient");
    let stream: FilesServiceStreamClient = local
        .establish()
        .await
        .expect("establish FilesServiceStreamClient");

    // Subscribe before mutating (the call stays in flight for the life
    // of the subscription — see `task`'s `events_stream.rs` for why).
    let (tx, mut rx) = vox::channel::<FilesEvent>();
    let subscription = tokio::spawn(async move {
        stream.events(tx).await.expect("subscribe to files events");
    });
    tokio::time::timeout(Duration::from_secs(10), async {
        while backend.events_hub().subscriber_count() == 0 {
            tokio::time::sleep(Duration::from_millis(5)).await;
        }
    })
    .await
    .expect("subscriber sink never reached the backend hub");

    // create_root: marker file + entity.
    let root = client
        .create_root(
            root_dir.to_str().unwrap().to_string(),
            "Mix Session".to_string(),
            RootFlavor::Media,
        )
        .await
        .expect("create_root rpc");
    assert_eq!(root.name, "Mix Session");
    assert!(
        root_dir.join(".fts-root.json").exists(),
        "marker file written into the root's own tree"
    );
    match next_event(&mut rx).await {
        FilesEvent::RootCreated(r) => assert_eq!(r.id, root.id),
        other => panic!("expected RootCreated, got {other:?}"),
    }

    // A second create_root on the same folder is rejected — root
    // identity is unique per tree.
    let dup = client
        .create_root(
            root_dir.to_str().unwrap().to_string(),
            "Dup".to_string(),
            RootFlavor::Media,
        )
        .await;
    assert!(
        dup.is_err(),
        "creating a root over an existing root must fail"
    );

    // list_roots / get_root.
    let listed = client.list_roots().await.expect("list_roots rpc");
    assert_eq!(listed.len(), 1);
    let got = client.get_root(root.id).await.expect("get_root rpc");
    assert_eq!(got.path, root.path);

    // browse (root-scoped) hides the marker file / store dir; a nested
    // subpath is a distinct call.
    let top = client
        .browse(root.id, String::new())
        .await
        .expect("browse rpc");
    let names: Vec<_> = top.iter().map(|e| e.name.as_str()).collect();
    assert!(names.contains(&"mix.wav"));
    assert!(names.contains(&"stems"));
    assert!(
        !names.contains(&".fts-root.json") && !names.contains(&".fts-files"),
        "root browsing hides internals: {names:?}"
    );
    let stems = client
        .browse(root.id, "stems".to_string())
        .await
        .expect("browse rpc");
    assert_eq!(stems.len(), 1);
    assert_eq!(stems[0].name, "kick.wav");

    // drive_browse (rootless, still org-confined) is a genuinely
    // different view — it shows the raw tree, internals included.
    let drive = client
        .drive_browse(root_dir.to_str().unwrap().to_string())
        .await
        .expect("drive_browse rpc");
    let drive_names: Vec<_> = drive.iter().map(|e| e.name.as_str()).collect();
    assert!(
        drive_names.contains(&".fts-root.json"),
        "drive_browse shows the raw tree: {drive_names:?}"
    );

    // checkpoint_now — the live tree checkpoints; chain sees it.
    let cp1 = client
        .checkpoint_now(root.id, Some("first save".to_string()))
        .await
        .expect("checkpoint_now rpc");
    assert!(cp1.changed_paths.contains(&"mix.wav".to_string()));
    assert!(cp1.changed_paths.contains(&"stems/kick.wav".to_string()));
    match next_event(&mut rx).await {
        FilesEvent::Checkpointed(info) => assert_eq!(info.commit_id, cp1.commit_id),
        other => panic!("expected Checkpointed, got {other:?}"),
    }

    let chain = client
        .chain(root.id, "mix.wav".to_string())
        .await
        .expect("chain rpc");
    assert_eq!(chain.len(), 1, "one saved state so far: {chain:?}");
    assert_eq!(chain[0].commit_id, cp1.commit_id);

    // Edit the file and checkpoint again — a second chain entry.
    std::fs::write(root_dir.join("mix.wav"), b"take two, final").unwrap();
    let cp2 = client
        .checkpoint_now(root.id, None)
        .await
        .expect("checkpoint_now rpc");
    assert_eq!(cp2.description, "checkpoint now", "default description");
    assert_ne!(cp2.commit_id, cp1.commit_id);
    assert_eq!(
        cp2.changed_paths,
        vec!["mix.wav".to_string()],
        "only the edited file is reported changed"
    );

    let chain = client
        .chain(root.id, "mix.wav".to_string())
        .await
        .expect("chain rpc");
    assert_eq!(chain.len(), 2, "two saved states now: {chain:?}");
    assert_eq!(chain[0].commit_id, cp2.commit_id, "newest first");
    assert_eq!(chain[1].commit_id, cp1.commit_id);

    // A checkpoint with no live-tree changes still succeeds (a new
    // commit — the certifying scan itself is the event), but writes
    // nothing: `changed_paths` is empty (its own documented contract),
    // and no new chain entry appears.
    let cp3 = client
        .checkpoint_now(root.id, None)
        .await
        .expect("checkpoint_now rpc");
    assert_ne!(cp3.commit_id, cp2.commit_id, "still a new commit");
    assert!(
        cp3.changed_paths.is_empty(),
        "a no-op checkpoint changes nothing: {:?}",
        cp3.changed_paths
    );
    let chain_after_noop = client
        .chain(root.id, "mix.wav".to_string())
        .await
        .expect("chain rpc");
    assert_eq!(
        chain_after_noop.len(),
        2,
        "an unchanged file gains no new chain entry: {chain_after_noop:?}"
    );

    subscription.abort();
    scope.close().await;
}

/// PR #280 review findings 1+2: `create_root` and `drive_browse` must
/// not reach outside the org's own files area, and root-scoped
/// `browse` must not escape the root via an absolute subpath, `..`, or
/// a symlink.
#[tokio::test(flavor = "multi_thread")]
async fn filesystem_access_is_confined() {
    let data_dir = tempfile::tempdir().expect("data tempdir");
    let outside = tempfile::tempdir().expect("outside tempdir");
    std::fs::write(outside.path().join("secret.txt"), b"org B's data").unwrap();

    let backend = FilesBackend::new(data_dir.path()).expect("backend");
    let scope = Scope::new();
    let local = LocalServer::serve(router(backend), scope.clone());
    let client: FilesServiceClient = local.establish().await.expect("establish client");

    // create_root outside the org's files area is rejected outright.
    let outside_create = client
        .create_root(
            outside.path().to_str().unwrap().to_string(),
            "Escape".to_string(),
            RootFlavor::Media,
        )
        .await;
    assert!(
        outside_create.is_err(),
        "create_root must reject a path outside the org's files area"
    );
    assert!(
        !outside.path().join(".fts-root.json").exists(),
        "a rejected create_root must not write a marker file outside confinement"
    );

    // drive_browse outside the org's files area is rejected outright —
    // no server-filesystem enumeration at plain member tier.
    let outside_browse = client
        .drive_browse(outside.path().to_str().unwrap().to_string())
        .await;
    assert!(
        outside_browse.is_err(),
        "drive_browse must reject a path outside the org's files area"
    );

    // A real root, to probe browse's escape guard against.
    let root_dir = data_dir.path().join("root-a");
    std::fs::create_dir(&root_dir).unwrap();
    std::fs::write(root_dir.join("inside.txt"), b"ok").unwrap();
    let root = client
        .create_root(
            root_dir.to_str().unwrap().to_string(),
            "Root A".to_string(),
            RootFlavor::Media,
        )
        .await
        .expect("create_root rpc");

    // Absolute subpath: `root_path.join("/etc")` replaces the base
    // entirely under plain `PathBuf::join` semantics — must still be
    // rejected, not silently resolve to `/etc`.
    let abs_escape = client.browse(root.id, "/etc".to_string()).await;
    assert!(
        abs_escape.is_err(),
        "an absolute subpath must not escape the root"
    );

    // `..`-relative escape.
    let dotdot_escape = client.browse(root.id, "../".to_string()).await;
    assert!(
        dotdot_escape.is_err(),
        "a `..` subpath must not escape the root"
    );

    // Symlink escape: a symlink *inside* the root pointing at
    // `outside` — canonicalizing the resolved target must catch this
    // even though the textual path never leaves the root.
    #[cfg(unix)]
    {
        std::os::unix::fs::symlink(outside.path(), root_dir.join("escape-link")).unwrap();
        let symlink_escape = client.browse(root.id, "escape-link".to_string()).await;
        assert!(
            symlink_escape.is_err(),
            "a symlink inside the root pointing outside it must not be followed out"
        );
    }

    scope.close().await;
}

/// PR #280 review finding 5: an outer root must not be creatable
/// around (or inside) an existing root — either direction.
#[tokio::test(flavor = "multi_thread")]
async fn nested_roots_are_rejected() {
    let data_dir = tempfile::tempdir().expect("data tempdir");
    let backend = FilesBackend::new(data_dir.path()).expect("backend");
    let scope = Scope::new();
    let local = LocalServer::serve(router(backend), scope.clone());
    let client: FilesServiceClient = local.establish().await.expect("establish client");

    // Outer root first; a descendant root creation is rejected.
    let outer = data_dir.path().join("outer");
    std::fs::create_dir(&outer).unwrap();
    client
        .create_root(
            outer.to_str().unwrap().to_string(),
            "Outer".to_string(),
            RootFlavor::Media,
        )
        .await
        .expect("create outer root");

    let inner = outer.join("inner");
    std::fs::create_dir(&inner).unwrap();
    let nested = client
        .create_root(
            inner.to_str().unwrap().to_string(),
            "Inner".to_string(),
            RootFlavor::Media,
        )
        .await;
    assert!(
        nested.is_err(),
        "a root nested inside an existing root must be rejected"
    );

    // The other direction: an existing (deeper) root blocks a new
    // ancestor root from being created around it.
    let parent = data_dir.path().join("parent");
    let child = parent.join("child");
    std::fs::create_dir_all(&child).unwrap();
    client
        .create_root(
            child.to_str().unwrap().to_string(),
            "Child".to_string(),
            RootFlavor::Media,
        )
        .await
        .expect("create child root");
    let ancestor = client
        .create_root(
            parent.to_str().unwrap().to_string(),
            "Parent".to_string(),
            RootFlavor::Media,
        )
        .await;
    assert!(
        ancestor.is_err(),
        "a root that would ancestor an existing root must be rejected"
    );

    scope.close().await;
}

/// PR #280 review finding 4: two concurrent `checkpoint_now` calls on
/// the same root must not race — every writer's change must land in
/// the chain, none silently orphaned by a lost `set_head`.
#[tokio::test(flavor = "multi_thread")]
async fn concurrent_checkpoints_on_same_root_do_not_race() {
    let data_dir = tempfile::tempdir().expect("data tempdir");
    let root_dir = data_dir.path().join("concurrent");
    std::fs::create_dir(&root_dir).unwrap();

    let backend = FilesBackend::new(data_dir.path()).expect("backend");
    let scope = Scope::new();
    let local = LocalServer::serve(router(backend), scope.clone());
    let client: FilesServiceClient = local.establish().await.expect("establish client");
    let root = client
        .create_root(
            root_dir.to_str().unwrap().to_string(),
            "Concurrent".to_string(),
            RootFlavor::Media,
        )
        .await
        .expect("create_root rpc");

    const N: usize = 8;
    let mut tasks = Vec::new();
    for i in 0..N {
        std::fs::write(root_dir.join(format!("f{i}.txt")), format!("content {i}")).unwrap();
        let client = client.clone();
        let root_id = root.id;
        tasks.push(tokio::spawn(async move {
            client
                .checkpoint_now(root_id, Some(format!("writer {i}")))
                .await
                .expect("checkpoint_now rpc")
        }));
    }
    let mut commit_ids = std::collections::HashSet::new();
    for t in tasks {
        let info = t.await.expect("checkpoint task panicked");
        commit_ids.insert(info.commit_id);
    }
    assert_eq!(
        commit_ids.len(),
        N,
        "every concurrent checkpoint produced a distinct commit"
    );

    // The real proof: every writer's file is reachable through the
    // (single, linear) head — a lost commit would leave its file
    // either absent from `chain` or the file simply never having been
    // committed onto the line of history the tracked head walks.
    for i in 0..N {
        let chain = client
            .chain(root.id, format!("f{i}.txt"))
            .await
            .unwrap_or_else(|_| panic!("chain rpc for f{i}.txt"));
        assert_eq!(
            chain.len(),
            1,
            "f{i}.txt's checkpoint must not have been orphaned by a racing writer: {chain:?}"
        );
    }

    scope.close().await;
}

// A genuine two-process restart reopens in well under a second (per
// PR #280 review, refuting this test's earlier "reopen hangs" theory —
// the hang was two `FilesBackend`s alive in *one* process at once,
// this test's own earlier setup, most likely iroh-blobs' `FsStore`
// holding file-backed resources open until `ChunkStore::shutdown`).
// This version tears the first backend all the way down —
// `FilesBackend::shutdown` flushes its chunk stores, then every RPC
// handle/router/scope is dropped — before constructing the second.
#[tokio::test(flavor = "multi_thread")]
async fn root_identity_survives_backend_restart() {
    let data_dir = tempfile::tempdir().expect("data tempdir");
    let root_dir = data_dir.path().join("session");
    std::fs::create_dir(&root_dir).unwrap();
    std::fs::write(root_dir.join("session.rpp"), b"reaper project").unwrap();

    let created: FileRootInfo = {
        let backend = FilesBackend::new(data_dir.path()).expect("backend");
        let scope = Scope::new();
        let local = LocalServer::serve(router(backend.clone()), scope.clone());
        let client: FilesServiceClient = local.establish().await.expect("establish client");
        let root = client
            .create_root(
                root_dir.to_str().unwrap().to_string(),
                "Session".to_string(),
                RootFlavor::Media,
            )
            .await
            .expect("create_root rpc");
        client
            .checkpoint_now(root.id, Some("initial".to_string()))
            .await
            .expect("checkpoint_now rpc");

        // Tear this backend all the way down before the second one
        // touches the same repo: flush its chunk stores, drop the RPC
        // client and the server/scope, then drop the backend itself.
        backend.shutdown().await;
        drop(client);
        scope.close().await;
        drop(local);
        drop(backend);
        // Give any detached background task (the chunk store's own
        // actor, if `shutdown` didn't fully join it) a beat to
        // actually exit before the second backend opens the same
        // on-disk store.
        for _ in 0..10 {
            tokio::task::yield_now().await;
        }

        root
    };

    // A fresh `FilesBackend` pointed at the same data dir (simulating a
    // server restart) still knows the root and can still derive its
    // chain — both the marker file and the registry entity, plus the
    // reopened jj repo, survive.
    let restart = tokio::time::timeout(Duration::from_secs(15), async {
        let backend = FilesBackend::new(data_dir.path()).expect("backend");
        let scope = Scope::new();
        let local = LocalServer::serve(router(backend), scope.clone());
        let client: FilesServiceClient = local.establish().await.expect("establish client");

        let roots = client.list_roots().await.expect("list_roots rpc");
        assert_eq!(roots.len(), 1);
        assert_eq!(roots[0].id, created.id);

        let chain = client
            .chain(created.id, "session.rpp".to_string())
            .await
            .expect("chain rpc");
        assert_eq!(
            chain.len(),
            1,
            "the checkpoint from before the restart is still there"
        );

        scope.close().await;
    })
    .await;
    restart.expect("reopening the root after a full backend teardown must not hang");
}
