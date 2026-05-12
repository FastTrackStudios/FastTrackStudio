//! Native integration tests for the `bar` feature.

#![cfg(test)]

use bar_memory::BarRepoMemory;
use bar_proto::{BarCreate, BarRepo};

#[tokio::test]
async fn create_then_get_round_trip() {
    let r = BarRepoMemory::new();
    let created = r
        .create(BarCreate { name: "alpha".into() })
        .await
        .unwrap();
    let got = r.get(created.id).await.unwrap();
    assert_eq!(got.id, created.id);
    assert_eq!(got.name, "alpha");
}
