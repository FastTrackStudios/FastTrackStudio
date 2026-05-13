//! Native integration tests for the `foo` feature.
//!
//! Each test is annotated `r[verify <rule>]` so `tracey query status`
//! shows full coverage for `foo/live`. Drive the trait, not the
//! concrete `FooRepoMemory` — the same tests apply to any future
//! `FooRepo` impl.

#![cfg(test)]

use architect::RepoError;
use foo_memory::FooRepoMemory;
use foo_proto::{FooCreate, FooRepo, FooUpdate};

fn repo() -> FooRepoMemory {
    FooRepoMemory::new()
}

// r[verify foo.create.id-generated]
// r[verify foo.create.timestamps]
#[tokio::test]
async fn create_assigns_uuid_and_timestamps() {
    let r = repo();
    let a = r
        .create(FooCreate { name: "alpha".into() })
        .await
        .unwrap();
    let b = r
        .create(FooCreate { name: "beta".into() })
        .await
        .unwrap();

    // Distinct, non-nil IDs.
    assert_ne!(a.id, b.id);
    assert_ne!(a.id, uuid::Uuid::nil());
    assert_ne!(b.id, uuid::Uuid::nil());

    // Timestamps populated; created_at == updated_at on insert.
    assert_eq!(a.created_at, a.updated_at);
    assert!(a.created_at <= chrono::Utc::now());
}

// r[verify foo.get.missing]
#[tokio::test]
async fn get_unknown_id_returns_not_found() {
    let r = repo();
    let bogus = uuid::Uuid::new_v4();
    assert!(matches!(r.get(bogus).await, Err(RepoError::NotFound)));
}

// r[verify foo.update.partial]
#[tokio::test]
async fn update_with_none_preserves_field() {
    let r = repo();
    let row = r
        .create(FooCreate { name: "original".into() })
        .await
        .unwrap();
    let after = r
        .update(row.id, FooUpdate { name: None })
        .await
        .unwrap();
    assert_eq!(after.name, "original");
}

// r[verify foo.update.touches-updated-at]
#[tokio::test]
async fn update_bumps_updated_at() {
    let r = repo();
    let row = r
        .create(FooCreate { name: "foo".into() })
        .await
        .unwrap();
    let before = row.updated_at;
    // Small sleep so the clock advances measurably even on fast hosts.
    tokio::time::sleep(std::time::Duration::from_millis(2)).await;
    let after = r
        .update(row.id, FooUpdate { name: Some("bar".into()) })
        .await
        .unwrap();
    assert!(after.updated_at > before);
}

// r[verify foo.delete.missing]
#[tokio::test]
async fn delete_unknown_id_returns_not_found() {
    let r = repo();
    let bogus = uuid::Uuid::new_v4();
    assert!(matches!(r.delete(bogus).await, Err(RepoError::NotFound)));
}
