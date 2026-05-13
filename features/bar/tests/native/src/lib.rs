//! Native integration tests for the `bar` feature.
//!
//! Each test is annotated `r[verify <rule>]` so `tracey query status`
//! shows full coverage for `bar/live`. Same trait surface any future
//! `BarRepo` impl would satisfy.

#![cfg(test)]

use architect::{Page, RepoError, Sort, SortOrder};
use bar_memory::BarRepoMemory;
use bar_proto::{BarCreate, BarRepo};

fn repo() -> BarRepoMemory {
    BarRepoMemory::new()
}

async fn seed_names(r: &BarRepoMemory, names: &[&str]) {
    for n in names {
        r.create(BarCreate { name: (*n).into() }).await.unwrap();
    }
}

// r[verify bar.list.empty]
#[tokio::test]
async fn empty_repo_lists_zero() {
    let r = repo();
    let page = r
        .list(Page { index: 0, size: 100 }, None, None)
        .await
        .unwrap();
    assert_eq!(page.total, 0);
    assert!(page.items.is_empty());
}

// r[verify bar.list.pagination.size]
#[tokio::test]
async fn pagination_respects_size() {
    let r = repo();
    seed_names(&r, &["a", "b", "c", "d", "e"]).await;
    let page = r
        .list(Page { index: 0, size: 2 }, None, None)
        .await
        .unwrap();
    assert_eq!(page.items.len(), 2);
    assert_eq!(page.total, 5);
}

// r[verify bar.list.pagination.size-clamped]
#[tokio::test]
async fn pagination_zero_size_clamped_to_one() {
    let r = repo();
    seed_names(&r, &["a", "b", "c"]).await;
    let page = r
        .list(Page { index: 0, size: 0 }, None, None)
        .await
        .unwrap();
    // size=0 → clamped to 1, so we get 1 row back, not panic / empty.
    assert_eq!(page.items.len(), 1);
    assert_eq!(page.total, 3);
}

// r[verify bar.list.pagination.offset]
#[tokio::test]
async fn pagination_offset_skips_correctly() {
    let r = repo();
    seed_names(&r, &["a", "b", "c", "d", "e"]).await;
    let p0 = r
        .list(
            Page { index: 0, size: 2 },
            Some(Sort { field: "name".into(), order: SortOrder::Asc }),
            None,
        )
        .await
        .unwrap();
    let p1 = r
        .list(
            Page { index: 1, size: 2 },
            Some(Sort { field: "name".into(), order: SortOrder::Asc }),
            None,
        )
        .await
        .unwrap();
    let p_beyond = r
        .list(Page { index: 99, size: 2 }, None, None)
        .await
        .unwrap();

    let p0_names: Vec<_> = p0.items.iter().map(|e| e.name.as_str()).collect();
    let p1_names: Vec<_> = p1.items.iter().map(|e| e.name.as_str()).collect();
    assert_eq!(p0_names, vec!["a", "b"]);
    assert_eq!(p1_names, vec!["c", "d"]);
    assert!(p_beyond.items.is_empty());
    assert_eq!(p_beyond.total, 5);
}

// r[verify bar.list.sort.name-asc]
#[tokio::test]
async fn sort_name_ascending() {
    let r = repo();
    seed_names(&r, &["charlie", "alpha", "bravo"]).await;
    let page = r
        .list(
            Page { index: 0, size: 100 },
            Some(Sort { field: "name".into(), order: SortOrder::Asc }),
            None,
        )
        .await
        .unwrap();
    let names: Vec<_> = page.items.iter().map(|e| e.name.as_str()).collect();
    assert_eq!(names, vec!["alpha", "bravo", "charlie"]);
}

// r[verify bar.list.sort.name-desc]
#[tokio::test]
async fn sort_name_descending() {
    let r = repo();
    seed_names(&r, &["charlie", "alpha", "bravo"]).await;
    let page = r
        .list(
            Page { index: 0, size: 100 },
            Some(Sort { field: "name".into(), order: SortOrder::Desc }),
            None,
        )
        .await
        .unwrap();
    let names: Vec<_> = page.items.iter().map(|e| e.name.as_str()).collect();
    assert_eq!(names, vec!["charlie", "bravo", "alpha"]);
}

// r[verify bar.list.sort.unknown]
#[tokio::test]
async fn sort_unknown_field_errors() {
    let r = repo();
    let err = r
        .list(
            Page { index: 0, size: 10 },
            Some(Sort { field: "uuid".into(), order: SortOrder::Asc }),
            None,
        )
        .await
        .unwrap_err();
    assert!(matches!(err, RepoError::InvalidInput(_)));
}
