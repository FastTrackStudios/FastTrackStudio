//! App-level end-to-end tests.
//!
//! Mounts the *real* server router (`app_server::vox_router`, in-memory
//! backend) on an ephemeral TCP port, then drives it with real vox
//! clients over a WebSocket — the same `ExampleRepoClient` /
//! `ExampleServiceClient` a browser or the CLI establishes. Every byte
//! goes through facet encoding on the wire, so this is the test that
//! catches transport + schema regressions (it's what surfaced the vox
//! Uuid-encoding bug).
//!
//! Run with: `cargo test -p app-tests-e2e`

#![cfg(test)]

use app_server::vox_router;
use example::architect::{Page, Sort, SortOrder};
use example::backend_memory::ExampleRepoMemory;
use example::{ExampleCreate, ExampleRepoClient, ExampleServiceClient, ExampleUpdate};
use tokio::sync::oneshot;
use vox_core::{TransportMode, initiator_on};
use vox_websocket::WsLink;

/// Spawn the real router on an OS-assigned port. Returns the `/vox` URL
/// and a shutdown sender — send `()` (or drop it) to stop the server.
async fn spawn() -> (String, oneshot::Sender<()>) {
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap();
    let (shutdown_tx, shutdown_rx) = oneshot::channel::<()>();
    let app = vox_router(ExampleRepoMemory::new());
    tokio::spawn(async move {
        let _ = axum::serve(listener, app)
            .with_graceful_shutdown(async {
                let _ = shutdown_rx.await;
            })
            .await;
    });
    (format!("ws://{addr}/vox"), shutdown_tx)
}

async fn repo_client(ws_url: &str) -> ExampleRepoClient {
    let link = WsLink::connect(ws_url).await.expect("WsLink::connect");
    initiator_on(link, TransportMode::Bare)
        .establish::<ExampleRepoClient>()
        .await
        .expect("ExampleRepo handshake")
}

async fn service_client(ws_url: &str) -> ExampleServiceClient {
    let link = WsLink::connect(ws_url).await.expect("WsLink::connect");
    initiator_on(link, TransportMode::Bare)
        .establish::<ExampleServiceClient>()
        .await
        .expect("ExampleService handshake")
}

fn page(size: u32) -> Page {
    Page { index: 0, size }
}

#[tokio::test]
async fn health_endpoint_serves_ok() {
    let (ws_url, shutdown) = spawn().await;
    let http = ws_url
        .replace("ws://", "http://")
        .replace("/vox", "/api/health");
    assert_eq!(reqwest_health(&http).await, "ok");
    let _ = shutdown.send(());
}

#[tokio::test]
async fn repo_full_crud_round_trip() {
    let (ws_url, shutdown) = spawn().await;
    let client = repo_client(&ws_url).await;

    // create
    let created = client
        .create(ExampleCreate {
            name: "alpha".into(),
            description: "first".into(),
        })
        .await
        .expect("create");
    assert_eq!(created.name, "alpha");

    // get
    let fetched = client.get(created.id).await.expect("get");
    assert_eq!(fetched.id, created.id);
    assert_eq!(fetched.description, "first");

    // update (partial: name only)
    let updated = client
        .update(
            created.id,
            ExampleUpdate {
                name: Some("alpha-renamed".into()),
                description: None,
            },
        )
        .await
        .expect("update");
    assert_eq!(updated.name, "alpha-renamed");
    assert_eq!(updated.description, "first", "untouched field preserved");

    // list shows it
    let list = client
        .list(
            page(100),
            Some(Sort {
                field: "name".into(),
                order: SortOrder::Asc,
            }),
            None,
        )
        .await
        .expect("list");
    assert!(list.items.iter().any(|e| e.id == created.id));

    // delete, then get -> NotFound
    client.delete(created.id).await.expect("delete");
    let err = client.get(created.id).await.unwrap_err();
    assert!(
        format!("{err:?}").contains("NotFound"),
        "expected NotFound after delete, got {err:?}"
    );

    let _ = shutdown.send(());
}

#[tokio::test]
async fn service_search_matches_name_and_description() {
    let (ws_url, shutdown) = spawn().await;
    let repo = repo_client(&ws_url).await;
    let service = service_client(&ws_url).await;

    for (name, desc) in [
        ("apple pie", "a dessert"),
        ("banana bread", "also a dessert"),
        ("car engine", "not food"),
    ] {
        repo.create(ExampleCreate {
            name: name.into(),
            description: desc.into(),
        })
        .await
        .expect("seed create");
    }

    // matches a name
    let by_name = service.search("banana".into(), 10).await.expect("search");
    assert_eq!(by_name.len(), 1);
    assert_eq!(by_name[0].name, "banana bread");

    // matches a description across rows (case-insensitive)
    let by_desc = service.search("DESSERT".into(), 10).await.expect("search");
    assert_eq!(by_desc.len(), 2, "both desserts match on description");

    // empty query is rejected
    let err = service.search("   ".into(), 10).await.unwrap_err();
    assert!(format!("{err:?}").contains("InvalidInput"));

    let _ = shutdown.send(());
}

#[tokio::test]
async fn service_duplicate_copies_row() {
    let (ws_url, shutdown) = spawn().await;
    let repo = repo_client(&ws_url).await;
    let service = service_client(&ws_url).await;

    let original = repo
        .create(ExampleCreate {
            name: "template".into(),
            description: "reusable".into(),
        })
        .await
        .expect("create");

    // default name = "<name> (copy)"
    let copy = service
        .duplicate(original.id, None)
        .await
        .expect("duplicate");
    assert_ne!(copy.id, original.id);
    assert_eq!(copy.name, "template (copy)");
    assert_eq!(copy.description, "reusable");

    // explicit new name
    let named = service
        .duplicate(original.id, Some("renamed copy".into()))
        .await
        .expect("duplicate named");
    assert_eq!(named.name, "renamed copy");

    // both copies + original are present
    let list = repo.list(page(100), None, None).await.expect("list");
    assert_eq!(list.total, 3);

    // duplicating a missing row -> NotFound
    let err = service
        .duplicate(uuid::Uuid::new_v4(), None)
        .await
        .unwrap_err();
    assert!(format!("{err:?}").contains("NotFound"));

    let _ = shutdown.send(());
}

// Same contract, no server: serve the backend in-process over a vox
// in-memory link and drive the *same* generated clients. This is the
// "inject the transport" payoff — the desktop app runs exactly this way.
#[tokio::test]
async fn local_transport_round_trip() {
    use app_server::service_router;
    use architect::{LocalServer, Scope};

    let scope = Scope::new();
    // Serve the full surface (repo + ExampleService) in-process — the same
    // `service_router` the axum server mounts, just over an in-memory link.
    let local = LocalServer::serve(service_router(ExampleRepoMemory::new()), scope.clone());
    let repo: ExampleRepoClient = local.establish().await.expect("local repo establish");
    let service: ExampleServiceClient = local.establish().await.expect("local service establish");

    // create → get (no socket, no server)
    let created = repo
        .create(ExampleCreate {
            name: "local".into(),
            description: "in-process".into(),
        })
        .await
        .expect("create");
    let got = repo.get(created.id).await.expect("get");
    assert_eq!(got.name, "local");

    // the ExampleService surface works identically in-process
    let hits = service.search("local".into(), 10).await.expect("search");
    assert_eq!(hits.len(), 1);
    let copy = service
        .duplicate(created.id, None)
        .await
        .expect("duplicate");
    assert_eq!(copy.name, "local (copy)");

    scope.close().await;
}

/// Live events over the real WebSocket: subscribe with a vox channel,
/// mutate through a *different* client connection, and assert the
/// broadcast arrives — the current row set as `Snapshot` **first**
/// (effect-`SubscriptionRef` semantics), then create/update as
/// `Upserted` and delete as `Deleted`. This is the wire proof for
/// `architect::PubSub` + the streaming sibling-trait pattern (and what
/// the Dioxus `use_store_stream` hook rides in the browser).
#[tokio::test]
async fn events_stream_broadcasts_writes() {
    use example::architect::vox;
    use example::{ExampleEvent, ExampleEventsClient};
    use tokio::time::{Duration, timeout};

    let (ws_url, shutdown) = spawn().await;

    // A row that exists *before* anyone subscribes — it must arrive in
    // the snapshot, not as a change event.
    let repo = repo_client(&ws_url).await;
    let pre_existing = repo
        .create(ExampleCreate {
            name: "pre-existing".into(),
            description: "before subscribe".into(),
        })
        .await
        .expect("create pre-existing");

    // The subscriber is a *view over the writer's own socket* — every
    // typed client is `Client::new(caller)` over one shared connection
    // (the single-socket model the app shell uses). A second socket works
    // identically; this proves multiplexing.
    let events = ExampleEventsClient::new(repo.caller.clone());
    let (tx, mut rx) = vox::channel::<ExampleEvent>();
    events.subscribe(tx).await.expect("subscribe");

    // Writes on the same connection still broadcast back to it.
    let created = repo
        .create(ExampleCreate {
            name: "streamed".into(),
            description: "live".into(),
        })
        .await
        .expect("create");

    async fn recv_owned(rx: &mut vox::Rx<ExampleEvent>) -> ExampleEvent {
        let item = timeout(Duration::from_secs(5), rx.recv())
            .await
            .expect("event within 5s")
            .expect("stream healthy")
            .expect("stream open");
        let mut owned: Option<ExampleEvent> = None;
        let _ = item.map(|e| owned = Some(e.clone()));
        owned.expect("owned event")
    }

    // First event: the snapshot, containing the pre-existing row (and
    // possibly the just-created one — the create raced the snapshot read;
    // either way the *next* delivery of `created` is the Upserted).
    match recv_owned(&mut rx).await {
        ExampleEvent::Snapshot(rows) => {
            assert!(
                rows.iter().any(|r| r.id == pre_existing.id),
                "snapshot must contain the pre-existing row: {rows:?}"
            );
        }
        other => panic!("expected Snapshot first, got {other:?}"),
    }

    match recv_owned(&mut rx).await {
        ExampleEvent::Upserted(row) => {
            assert_eq!(row.id, created.id);
            assert_eq!(row.name, "streamed");
        }
        other => panic!("expected Upserted after create, got {other:?}"),
    }

    repo.update(
        created.id,
        ExampleUpdate {
            name: Some("streamed-2".into()),
            description: None,
        },
    )
    .await
    .expect("update");
    match recv_owned(&mut rx).await {
        ExampleEvent::Upserted(row) => assert_eq!(row.name, "streamed-2"),
        other => panic!("expected Upserted after update, got {other:?}"),
    }

    repo.delete(created.id).await.expect("delete");
    match recv_owned(&mut rx).await {
        ExampleEvent::Deleted(id) => assert_eq!(id, created.id),
        other => panic!("expected Deleted after delete, got {other:?}"),
    }

    let _ = shutdown.send(());
}

// Tiny stdlib HTTP client to avoid pulling reqwest in for one GET.
async fn reqwest_health(url: &str) -> String {
    let (host_port, path) = url.trim_start_matches("http://").split_once('/').unwrap();
    let mut stream = tokio::net::TcpStream::connect(host_port).await.unwrap();
    use tokio::io::{AsyncReadExt, AsyncWriteExt};
    let req = format!("GET /{path} HTTP/1.1\r\nHost: {host_port}\r\nConnection: close\r\n\r\n");
    stream.write_all(req.as_bytes()).await.unwrap();
    let mut buf = Vec::new();
    stream.read_to_end(&mut buf).await.unwrap();
    let s = String::from_utf8_lossy(&buf);
    s.rsplit("\r\n\r\n").next().unwrap_or("").trim().to_string()
}
