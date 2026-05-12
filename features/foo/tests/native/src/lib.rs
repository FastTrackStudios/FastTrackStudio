//! Native integration tests for the `foo` feature.

#![cfg(test)]

use foo_memory::FooRepoMemory;
use foo_proto::{FooCreate, FooRepo};

#[tokio::test]
async fn create_then_get_round_trip() {
    let r = FooRepoMemory::new();
    let created = r
        .create(FooCreate { name: "alpha".into() })
        .await
        .unwrap();
    let got = r.get(created.id).await.unwrap();
    assert_eq!(got.id, created.id);
    assert_eq!(got.name, "alpha");
}
