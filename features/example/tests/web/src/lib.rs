//! Browser integration tests for the architect-generated vox client.
//!
//! Drives `ExampleRepoClient` end-to-end against a real `example-server`
//! over a WebSocket: create a row, fetch it back, list, delete, verify
//! it's gone. Every byte goes through facet encoding on the wire.
//!
//! Prerequisite: `cargo run -p app-server` in another terminal.
//! Then run from the architect repo root:
//!
//! ```sh
//! wasm-pack test --headless --chrome crates/example-test-wasm
//! ```

#![cfg(target_arch = "wasm32")]

use example::{
    architect::{Page, SortOrder, Sort},
    ExampleCreate, ExampleRepoClient,
};
use uuid::Uuid;
use vox_core::{initiator_on, TransportMode};
use vox_websocket::WsLink;
use wasm_bindgen_test::*;

wasm_bindgen_test_configure!(run_in_browser);

const SERVER_URL: &str = "ws://127.0.0.1:4040/vox";

async fn connect() -> ExampleRepoClient {
    let link = WsLink::connect(SERVER_URL)
        .await
        .expect("WsLink::connect — is example-server running on :4040?");
    initiator_on(link, TransportMode::Bare)
        .establish::<ExampleRepoClient>()
        .await
        .expect("vox handshake failed")
}

#[wasm_bindgen_test]
async fn create_then_get_round_trip() {
    let client = connect().await;

    let name = format!("wasm-test-{}", Uuid::new_v4());
    let created = client
        .create(ExampleCreate {
            name: name.clone(),
            description: "round-trip from a browser".into(),
        })
        .await
        .expect("create RPC failed");

    assert_eq!(created.name, name);

    let fetched = client.get(created.id).await.expect("get RPC failed");
    assert_eq!(fetched.id, created.id);
    assert_eq!(fetched.name, name);
}

#[wasm_bindgen_test]
async fn list_includes_inserted_row() {
    let client = connect().await;

    let marker = format!("list-marker-{}", Uuid::new_v4());
    let created = client
        .create(ExampleCreate {
            name: marker.clone(),
            description: "list test row".into(),
        })
        .await
        .expect("create failed");

    let page = client
        .list(
            Page { index: 0, size: 100 },
            Some(Sort {
                field: "name".into(),
                order: SortOrder::Asc,
            }),
            None,
        )
        .await
        .expect("list failed");

    assert!(page.total >= 1);
    let found = page.items.iter().find(|e| e.id == created.id);
    assert!(found.is_some(), "freshly-created row not in list");
}

#[wasm_bindgen_test]
async fn delete_removes_row() {
    let client = connect().await;

    let created = client
        .create(ExampleCreate {
            name: format!("delete-{}", Uuid::new_v4()),
            description: "to be deleted".into(),
        })
        .await
        .expect("create failed");

    client.delete(created.id).await.expect("delete failed");

    let err = client.get(created.id).await.unwrap_err();
    // RepoError::NotFound is what the server returns when the row's gone.
    let msg = format!("{err:?}");
    assert!(
        msg.contains("NotFound"),
        "expected NotFound, got: {msg}"
    );
}
