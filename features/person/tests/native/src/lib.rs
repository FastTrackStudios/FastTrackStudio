//! Native integration tests for the `person` feature. One ephemeral
//! `CrdtDoc` hosts every entity type — proves they coexist under one
//! root and survive cross-entity sync round-trips.

#![cfg(test)]

use architect::Page;
use loro::ExportMode;
use person_crdt::{ClientRepoLoro, CrdtDoc, PersonRepoLoro, TeamRepoLoro};
use person_proto::{ClientCreate, ClientRepo, PersonCreate, PersonRepo, TeamCreate, TeamRepo};

#[tokio::test]
async fn person_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let repo = PersonRepoLoro::new(&doc);

    let p = repo
        .create(PersonCreate {
            name: "Cody Wright".into(),
            email: Some("cody@example.com".into()),
            phone: Some("+15555550100".into()),
            role: Some("engineer".into()),
            client_id: None,
            team_id: None,
            notes: Some("primary contact".into()),
            tags: vec!["mixing".into(), "mastering".into()],
        })
        .await
        .unwrap();

    let got = repo.get(p.id).await.unwrap();
    assert_eq!(got.name, "Cody Wright");
    assert_eq!(got.email.as_deref(), Some("cody@example.com"));
    assert_eq!(got.role.as_deref(), Some("engineer"));
    assert_eq!(
        got.tags,
        vec!["mixing".to_string(), "mastering".to_string()]
    );
}

#[tokio::test]
async fn client_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let repo = ClientRepoLoro::new(&doc);

    let c = repo
        .create(ClientCreate {
            name: "Acme Records".into(),
            kind: "label".into(),
            website: Some("https://acme.example".into()),
            contact_email: Some("hello@acme.example".into()),
            address: Some("123 Main St".into()),
            country_code: Some("US".into()),
            default_billable_rate_cents: Some(15000),
            notes: None,
            tags: vec!["label".into()],
        })
        .await
        .unwrap();

    let got = repo.get(c.id).await.unwrap();
    assert_eq!(got.name, "Acme Records");
    assert_eq!(got.kind, "label");
    assert_eq!(got.default_billable_rate_cents, Some(15000));
    assert_eq!(got.country_code.as_deref(), Some("US"));
}

#[tokio::test]
async fn team_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let repo = TeamRepoLoro::new(&doc);

    let t = repo
        .create(TeamCreate {
            name: "Mix Team".into(),
            description: Some("Internal mix engineers".into()),
            owner: Some("cody".into()),
            archived: false,
            tags: vec!["mix".into()],
        })
        .await
        .unwrap();

    let got = repo.get(t.id).await.unwrap();
    assert_eq!(got.name, "Mix Team");
    assert!(!got.archived);
    assert_eq!(got.owner.as_deref(), Some("cody"));
}

#[tokio::test]
async fn all_three_coexist_in_one_doc() {
    let doc = CrdtDoc::ephemeral();
    let people = PersonRepoLoro::new(&doc);
    let clients = ClientRepoLoro::new(&doc);
    let teams = TeamRepoLoro::new(&doc);

    clients
        .create(ClientCreate {
            name: "C1".into(),
            kind: "company".into(),
            website: None,
            contact_email: None,
            address: None,
            country_code: None,
            default_billable_rate_cents: None,
            notes: None,
            tags: vec![],
        })
        .await
        .unwrap();

    teams
        .create(TeamCreate {
            name: "T1".into(),
            description: None,
            owner: None,
            archived: false,
            tags: vec![],
        })
        .await
        .unwrap();

    people
        .create(PersonCreate {
            name: "P1".into(),
            email: None,
            phone: None,
            role: None,
            client_id: None,
            team_id: None,
            notes: None,
            tags: vec![],
        })
        .await
        .unwrap();

    assert_eq!(
        people
            .list(
                Page {
                    index: 0,
                    size: 100
                },
                None,
                None
            )
            .await
            .unwrap()
            .total,
        1
    );
    assert_eq!(
        clients
            .list(
                Page {
                    index: 0,
                    size: 100
                },
                None,
                None
            )
            .await
            .unwrap()
            .total,
        1
    );
    assert_eq!(
        teams
            .list(
                Page {
                    index: 0,
                    size: 100
                },
                None,
                None
            )
            .await
            .unwrap()
            .total,
        1
    );
}

#[tokio::test]
async fn replicas_converge_across_all_entities() {
    let a = CrdtDoc::ephemeral();
    let b = CrdtDoc::ephemeral();
    let pa = PersonRepoLoro::new(&a);
    let pb = PersonRepoLoro::new(&b);
    let ca = ClientRepoLoro::new(&a);
    let cb = ClientRepoLoro::new(&b);
    let ta = TeamRepoLoro::new(&a);
    let tb = TeamRepoLoro::new(&b);

    pa.create(PersonCreate {
        name: "from-a".into(),
        email: None,
        phone: None,
        role: None,
        client_id: None,
        team_id: None,
        notes: None,
        tags: vec![],
    })
    .await
    .unwrap();
    pb.create(PersonCreate {
        name: "from-b".into(),
        email: None,
        phone: None,
        role: None,
        client_id: None,
        team_id: None,
        notes: None,
        tags: vec![],
    })
    .await
    .unwrap();

    ca.create(ClientCreate {
        name: "ca-client".into(),
        kind: "company".into(),
        website: None,
        contact_email: None,
        address: None,
        country_code: None,
        default_billable_rate_cents: None,
        notes: None,
        tags: vec![],
    })
    .await
    .unwrap();
    cb.create(ClientCreate {
        name: "cb-client".into(),
        kind: "individual".into(),
        website: None,
        contact_email: None,
        address: None,
        country_code: None,
        default_billable_rate_cents: None,
        notes: None,
        tags: vec![],
    })
    .await
    .unwrap();

    ta.create(TeamCreate {
        name: "ta-team".into(),
        description: None,
        owner: None,
        archived: false,
        tags: vec![],
    })
    .await
    .unwrap();
    tb.create(TeamCreate {
        name: "tb-team".into(),
        description: None,
        owner: None,
        archived: true,
        tags: vec![],
    })
    .await
    .unwrap();

    let ab = pa.doc().export(ExportMode::all_updates()).unwrap();
    let bb = pb.doc().export(ExportMode::all_updates()).unwrap();
    pb.doc().import(&ab).unwrap();
    pa.doc().import(&bb).unwrap();

    assert_eq!(
        pa.list(
            Page {
                index: 0,
                size: 100
            },
            None,
            None
        )
        .await
        .unwrap()
        .total,
        2
    );
    assert_eq!(
        pb.list(
            Page {
                index: 0,
                size: 100
            },
            None,
            None
        )
        .await
        .unwrap()
        .total,
        2
    );
    assert_eq!(
        ca.list(
            Page {
                index: 0,
                size: 100
            },
            None,
            None
        )
        .await
        .unwrap()
        .total,
        2
    );
    assert_eq!(
        cb.list(
            Page {
                index: 0,
                size: 100
            },
            None,
            None
        )
        .await
        .unwrap()
        .total,
        2
    );
    assert_eq!(
        ta.list(
            Page {
                index: 0,
                size: 100
            },
            None,
            None
        )
        .await
        .unwrap()
        .total,
        2
    );
    assert_eq!(
        tb.list(
            Page {
                index: 0,
                size: 100
            },
            None,
            None
        )
        .await
        .unwrap()
        .total,
        2
    );
}
