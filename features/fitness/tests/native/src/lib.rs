//! Native integration tests for the `fitness` feature. Drives
//! the wire contract against the Loro-backed implementation through
//! an ephemeral `CrdtDoc`.

#![cfg(test)]

use fitness_crdt::{CrdtDoc, FitnessRepoLoro};
use fitness_proto::{FitnessCreate, FitnessRepo};

fn repo() -> FitnessRepoLoro {
    FitnessRepoLoro::new(&CrdtDoc::ephemeral())
}

#[tokio::test]
async fn create_then_get_round_trip() {
    let r = repo();
    let created = r
        .create(FitnessCreate {
            name: "alpha".into(),
        })
        .await
        .unwrap();
    let got = r.get(created.id).await.unwrap();
    assert_eq!(got.id, created.id);
    assert_eq!(got.name, "alpha");
}

// Concurrent-replica merge — two repos backed by independent docs
// exchange their bytes and converge. This is the test that says
// "yes, this is actually a CRDT."
#[tokio::test]
async fn two_replicas_converge_after_sync() {
    use loro::ExportMode;
    let a = repo();
    let b = repo();
    a.create(FitnessCreate {
        name: "from-a".into(),
    })
    .await
    .unwrap();
    b.create(FitnessCreate {
        name: "from-b".into(),
    })
    .await
    .unwrap();
    let ab = a.doc().export(ExportMode::all_updates()).unwrap();
    let bb = b.doc().export(ExportMode::all_updates()).unwrap();
    b.doc().import(&ab).unwrap();
    a.doc().import(&bb).unwrap();
    let a_list = a
        .list(
            architect::Page {
                index: 0,
                size: 100,
            },
            None,
            None,
        )
        .await
        .unwrap();
    let b_list = b
        .list(
            architect::Page {
                index: 0,
                size: 100,
            },
            None,
            None,
        )
        .await
        .unwrap();
    assert_eq!(a_list.total, 2);
    assert_eq!(b_list.total, 2);
}
