//! End-to-end check for the `VaultSync` vox service against a
//! live `task-server`. Boots `AppState` on an ephemeral TCP port
//! (with `TASK_SERVER_VAULT_ROOT` pointed at a temp dir),
//! connects a `VaultSyncClient`, and exercises PUT → manifest →
//! GET, the subscribe stream observing a PUT + DELETE, and a
//! conflict round-trip.

use std::time::Duration;

use crdt_seaorm::SeaOrmPersistence;
use task_db::{default_database_url, open_and_migrate};
use task_server::{AppState, router};
use vault_sync_proto::{
    DeleteFileArg, GetFileArg, IfMatch, PutFileArg, VaultEvent, VaultIdArg, VaultSyncClient,
    VaultSyncError,
};
use vox::VoxError;

/// Spawn task-server on `127.0.0.1:0` with its vault root pointed
/// at a fresh temp dir. Returns the `ws://…/vox` URL.
///
/// `TASK_SERVER_VAULT_ROOT` is process-wide. Tests in this binary
/// run sequentially per the default `cargo test` runner, so each
/// `boot_server` call wins the env race.
/// Serializes env-var twiddling. `cargo test` runs tests on a
/// shared thread pool; without this, two `boot_server` calls
/// would race on `TASK_SERVER_VAULT_ROOT` and one could read the
/// other test's path. The mutex is held only across
/// `AppState::new` — once the value is captured into
/// `state.vault_sync`, subsequent env mutations are irrelevant.
static ENV_LOCK: tokio::sync::Mutex<()> = tokio::sync::Mutex::const_new(());

async fn boot_server() -> eyre::Result<(String, tempfile::TempDir)> {
    let tmp = tempfile::tempdir()?;
    let _guard = ENV_LOCK.lock().await;
    // SAFETY: held under `ENV_LOCK` for the duration of
    // `AppState::new`, which reads the var exactly once.
    unsafe {
        std::env::set_var("TASK_SERVER_VAULT_ROOT", tmp.path());
    }
    let persistence: SeaOrmPersistence = open_and_migrate(&default_database_url()).await?;
    let state = AppState::new(persistence).await?;
    drop(_guard);
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
    let port = listener.local_addr()?.port();
    let app = router(state);
    tokio::spawn(async move {
        let _ = axum::serve(listener, app).await;
    });
    let url = format!("ws://127.0.0.1:{port}/vox");
    Ok((url, tmp))
}

async fn connect(url: &str) -> eyre::Result<VaultSyncClient> {
    vox::connect(url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("vault-sync connect: {e:?}"))
}

#[tokio::test]
async fn put_manifest_get_round_trip() {
    let (url, _tmp) = boot_server().await.unwrap();
    let client = connect(&url).await.unwrap();

    let ack = client
        .put_file(PutFileArg {
            vault_id: "v1".into(),
            path: "notes/a.md".into(),
            bytes: b"hello".to_vec(),
            if_match: IfMatch::CreateOnly,
        })
        .await
        .unwrap();
    assert!(!ack.sha256.is_empty(), "PUT should return a sha");

    let manifest = client
        .manifest(VaultIdArg {
            vault_id: "v1".into(),
        })
        .await
        .unwrap();
    assert_eq!(manifest.vault_id, "v1");
    assert_eq!(manifest.files.len(), 1);
    assert_eq!(manifest.files[0].path, "notes/a.md");
    assert_eq!(manifest.files[0].size, 5);

    let bytes = client
        .get_file(GetFileArg {
            vault_id: "v1".into(),
            path: "notes/a.md".into(),
        })
        .await
        .unwrap();
    assert_eq!(&bytes.0[..], b"hello");
}

#[tokio::test]
async fn subscribe_receives_put_and_delete() {
    let (url, _tmp) = boot_server().await.unwrap();
    let client = connect(&url).await.unwrap();
    let writer = connect(&url).await.unwrap();

    let (tx, mut rx) = vox::channel::<VaultEvent>();
    let _sub = tokio::spawn(async move {
        let _ = client
            .subscribe(
                VaultIdArg {
                    vault_id: "v1".into(),
                },
                tx,
            )
            .await;
    });

    // Tiny delay so the subscribe handler is fully attached
    // before we emit the event.
    tokio::time::sleep(Duration::from_millis(50)).await;

    writer
        .put_file(PutFileArg {
            vault_id: "v1".into(),
            path: "a.md".into(),
            bytes: b"x".to_vec(),
            if_match: IfMatch::CreateOnly,
        })
        .await
        .unwrap();

    let msg = tokio::time::timeout(Duration::from_secs(2), rx.recv())
        .await
        .expect("event timeout")
        .expect("rx error")
        .expect("rx closed");
    match msg.get() {
        VaultEvent::Put { path, size, .. } => {
            assert_eq!(path, "a.md");
            assert_eq!(*size, 1);
        }
        other => panic!("expected Put, got {other:?}"),
    }

    writer
        .delete_file(DeleteFileArg {
            vault_id: "v1".into(),
            path: "a.md".into(),
            if_match: IfMatch::Force,
        })
        .await
        .unwrap();
    let msg = tokio::time::timeout(Duration::from_secs(2), rx.recv())
        .await
        .expect("event timeout")
        .expect("rx error")
        .expect("rx closed");
    match msg.get() {
        VaultEvent::Delete { path } => assert_eq!(path, "a.md"),
        other => panic!("expected Delete, got {other:?}"),
    }
}

#[tokio::test]
async fn put_conflict_returns_server_bytes() {
    let (url, _tmp) = boot_server().await.unwrap();
    let client = connect(&url).await.unwrap();
    client
        .put_file(PutFileArg {
            vault_id: "v1".into(),
            path: "x.md".into(),
            bytes: b"first".to_vec(),
            if_match: IfMatch::CreateOnly,
        })
        .await
        .unwrap();
    let err = client
        .put_file(PutFileArg {
            vault_id: "v1".into(),
            path: "x.md".into(),
            bytes: b"second".to_vec(),
            if_match: IfMatch::CreateOnly,
        })
        .await
        .unwrap_err();
    match err {
        VoxError::User(VaultSyncError::Conflict {
            server_sha,
            server_bytes,
        }) => {
            assert!(!server_sha.is_empty());
            assert_eq!(&server_bytes[..], b"first");
        }
        other => panic!("expected User(Conflict), got {other:?}"),
    }
}
