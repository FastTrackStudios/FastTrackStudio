//! Native integration tests for the example feature. See `tests/web`
//! for the matching browser/wasm coverage that exercises the same
//! contract over a real vox WebSocket against the server.

#![cfg(test)]

use architect::{Page, RepoError, Sort, SortOrder};
use example_memory::ExampleRepoMemory;
use example_proto::{ExampleCreate, ExampleRepo, ExampleUpdate};

fn repo() -> ExampleRepoMemory {
    ExampleRepoMemory::new()
}

// r[verify repo.create.id]
#[tokio::test]
async fn create_then_get_round_trip() {
    let r = repo();
    let created = r
        .create(ExampleCreate {
            name: "alpha".into(),
            description: "first".into(),
        })
        .await
        .unwrap();
    assert_eq!(created.name, "alpha");

    let got = r.get(created.id).await.unwrap();
    assert_eq!(got.id, created.id);
    assert_eq!(got.name, "alpha");
}

// r[verify repo.list.sort.name]
#[tokio::test]
async fn list_sorted_by_name_ascending() {
    let r = repo();
    for n in ["charlie", "alpha", "bravo"] {
        r.create(ExampleCreate {
            name: n.into(),
            description: String::new(),
        })
        .await
        .unwrap();
    }
    let page = r
        .list(
            Page {
                index: 0,
                size: 100,
            },
            Some(Sort {
                field: "name".into(),
                order: SortOrder::Asc,
            }),
            None,
        )
        .await
        .unwrap();
    let names: Vec<_> = page.items.iter().map(|e| e.name.as_str()).collect();
    assert_eq!(names, vec!["alpha", "bravo", "charlie"]);
}

// r[verify repo.update.partial]
#[tokio::test]
async fn update_changes_fields() {
    let r = repo();
    let created = r
        .create(ExampleCreate {
            name: "before".into(),
            description: "old".into(),
        })
        .await
        .unwrap();
    let updated = r
        .update(
            created.id,
            ExampleUpdate {
                name: Some("after".into()),
                description: None,
            },
        )
        .await
        .unwrap();
    assert_eq!(updated.name, "after");
    assert_eq!(updated.description, "old");
}

// r[verify repo.delete.missing]
#[tokio::test]
async fn delete_removes_row() {
    let r = repo();
    let created = r
        .create(ExampleCreate {
            name: "tmp".into(),
            description: String::new(),
        })
        .await
        .unwrap();
    r.delete(created.id).await.unwrap();
    assert!(matches!(r.get(created.id).await, Err(RepoError::NotFound)));
}

// ── Fake-rs seeding ───────────────────────────────────────────────────
//
// `example/fake` adds `#[derive(Dummy)]` to every wire + emitted
// struct, and (because `repo` is on too) emits a `seed_fake_example`
// helper that loops + creates in one call. Pass it a count or a range.

#[tokio::test]
async fn seed_fake_with_fixed_count() {
    let r = repo();
    let rows = example_proto::seed_fake_example(&r, 5usize).await.unwrap();
    assert_eq!(rows.len(), 5);
    let page = r
        .list(Page { index: 0, size: 100 }, None, None)
        .await
        .unwrap();
    assert_eq!(page.total, 5);
}

#[tokio::test]
async fn seed_fake_with_range_picks_within_bounds() {
    let r = repo();
    let rows = example_proto::seed_fake_example(&r, 3..8usize).await.unwrap();
    assert!(rows.len() >= 3 && rows.len() < 8);
}

#[tokio::test]
async fn seed_with_seeded_rng_is_deterministic() {
    use fake::Fake;
    use fake::rand::SeedableRng;
    let mut rng_a = fake::rand::rngs::StdRng::seed_from_u64(42);
    let mut rng_b = fake::rand::rngs::StdRng::seed_from_u64(42);
    let a: ExampleCreate = fake::Faker.fake_with_rng(&mut rng_a);
    let b: ExampleCreate = fake::Faker.fake_with_rng(&mut rng_b);
    assert_eq!(a.name, b.name);
    assert_eq!(a.description, b.description);
}

// r[verify repo.list.sort.unknown]
#[tokio::test]
async fn unsortable_field_errors() {
    let r = repo();
    let err = r
        .list(
            Page { index: 0, size: 10 },
            Some(Sort {
                field: "description".into(),
                order: SortOrder::Asc,
            }),
            None,
        )
        .await
        .unwrap_err();
    assert!(matches!(err, RepoError::InvalidInput(_)));
}

// ── External backend conformance ──────────────────────────────────────
//
// Same contract, different impl. Proves the third-party extension
// pattern: a backend that depends only on `example-proto` (no
// `architect/server`, no SeaORM, no other in-tree backend) satisfies
// the same `ExampleRepo` trait the in-tree memory backend does.
//
// Both tests use the trait surface only — there's no way for the test
// body to tell it's running against `StubBackend` vs `ExampleRepoMemory`.

// r[verify repo.create.id]
#[tokio::test]
async fn external_backend_round_trip() {
    let r = example_stub_backend::StubBackend::new();
    let created = r
        .create(ExampleCreate {
            name: "external".into(),
            description: "from a third-party-shaped crate".into(),
        })
        .await
        .unwrap();
    let got = r.get(created.id).await.unwrap();
    assert_eq!(got.id, created.id);
    assert_eq!(got.name, "external");
}

// r[verify repo.list.sort.name]
#[tokio::test]
async fn external_backend_seed_data_sorts() {
    let r = example_stub_backend::StubBackend::with_seed_data();
    let page = r
        .list(
            Page { index: 0, size: 100 },
            Some(Sort {
                field: "name".into(),
                order: SortOrder::Asc,
            }),
            None,
        )
        .await
        .unwrap();
    let names: Vec<_> = page.items.iter().map(|e| e.name.as_str()).collect();
    assert_eq!(
        names,
        vec!["stub.alpha", "stub.bravo", "stub.charlie"]
    );
}
