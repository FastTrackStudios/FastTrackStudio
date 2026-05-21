//! End-to-end sanity check for `vault-sync` (client) wired
//! against `task_server::vault_sync::router()`. Spawns the
//! router on an ephemeral TCP port; covers PUT → manifest →
//! GET, plus the live WebSocket subscriber observing a PUT
//! and a DELETE.

use std::time::Duration;

use task_server::vault_sync::{VaultSyncState, router};
use vault_sync::{Bytes, IfMatch, VaultClient, VaultEvent};

async fn spawn_server() -> (String, tempfile::TempDir) {
    let tmp = tempfile::tempdir().unwrap();
    let state = VaultSyncState::new(tmp.path().to_path_buf()).unwrap();
    let app = router().with_state(state);
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let port = listener.local_addr().unwrap().port();
    tokio::spawn(async move {
        let _ = axum::serve(listener, app).await;
    });
    (format!("http://127.0.0.1:{port}"), tmp)
}

#[tokio::test]
async fn put_manifest_get_round_trip() {
    let (base, _tmp) = spawn_server().await;
    let client = VaultClient::new(&base, "v1").unwrap();

    let sha = client
        .put_file(
            "notes/a.md",
            Bytes::from_static(b"hello"),
            IfMatch::CreateOnly,
        )
        .await
        .unwrap();
    assert!(!sha.is_empty(), "PUT should return a sha");

    let manifest = client.manifest().await.unwrap();
    assert_eq!(manifest.vault_id, "v1");
    assert_eq!(manifest.files.len(), 1);
    assert_eq!(manifest.files[0].path, "notes/a.md");
    assert_eq!(manifest.files[0].size, 5);

    let bytes = client.get_file("notes/a.md").await.unwrap();
    assert_eq!(&bytes[..], b"hello");
}

#[tokio::test]
async fn subscribe_receives_put_and_delete() {
    let (base, _tmp) = spawn_server().await;
    let client = VaultClient::new(&base, "v1").unwrap();
    let mut sub = client.subscribe().await.unwrap();

    // Tiny delay so the WS handler is fully attached before
    // we emit the event.
    tokio::time::sleep(Duration::from_millis(50)).await;

    let _ = client
        .put_file("a.md", Bytes::from_static(b"x"), IfMatch::CreateOnly)
        .await
        .unwrap();

    let evt = tokio::time::timeout(Duration::from_secs(2), sub.next_event())
        .await
        .expect("event timeout")
        .unwrap()
        .expect("server closed");
    match evt {
        VaultEvent::Put { path, size, .. } => {
            assert_eq!(path, "a.md");
            assert_eq!(size, 1);
        }
        other => panic!("expected Put, got {other:?}"),
    }

    client.delete_file("a.md", IfMatch::Force).await.unwrap();
    let evt = tokio::time::timeout(Duration::from_secs(2), sub.next_event())
        .await
        .expect("event timeout")
        .unwrap()
        .expect("server closed");
    match evt {
        VaultEvent::Delete { path } => assert_eq!(path, "a.md"),
        other => panic!("expected Delete, got {other:?}"),
    }
}

#[tokio::test]
async fn put_conflict_returns_server_bytes() {
    let (base, _tmp) = spawn_server().await;
    let client = VaultClient::new(&base, "v1").unwrap();
    let _ = client
        .put_file("x.md", Bytes::from_static(b"first"), IfMatch::CreateOnly)
        .await
        .unwrap();
    let err = client
        .put_file("x.md", Bytes::from_static(b"second"), IfMatch::CreateOnly)
        .await
        .unwrap_err();
    match err {
        vault_sync::Error::Conflict {
            server_sha,
            server_bytes,
        } => {
            assert!(!server_sha.is_empty());
            assert_eq!(&server_bytes[..], b"first");
        }
        other => panic!("expected Conflict, got {other:?}"),
    }
}
