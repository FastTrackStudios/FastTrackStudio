//! Native integration tests for the `finance` feature.

#![cfg(test)]

use architect::Page;
use chrono::Utc;
use finance_crdt::{CrdtDoc, ExpenseRepoLoro, RevenueRepoLoro};
use finance_proto::{ExpenseCreate, ExpenseRepo, RevenueCreate, RevenueRepo};
use loro::ExportMode;
use uuid::Uuid;

#[tokio::test]
async fn revenue_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let repo = RevenueRepoLoro::new(&doc);

    let client_id = Uuid::new_v4();
    let invoice_id = Uuid::new_v4();
    let received = Utc::now();

    let r = repo
        .create(RevenueCreate {
            source: "client-work".into(),
            client_id: Some(client_id),
            invoice_id: Some(invoice_id),
            amount_cents: 250_000,
            currency: "USD".into(),
            received_at: received,
            notes: Some("wire transfer".into()),
            tags: vec!["q1".into()],
        })
        .await
        .unwrap();

    let got = repo.get(r.id).await.unwrap();
    assert_eq!(got.source, "client-work");
    assert_eq!(got.client_id, Some(client_id));
    assert_eq!(got.invoice_id, Some(invoice_id));
    assert_eq!(got.amount_cents, 250_000);
    assert_eq!(got.currency, "USD");
    assert_eq!(got.notes.as_deref(), Some("wire transfer"));
    assert_eq!(got.tags, vec!["q1".to_string()]);
}

#[tokio::test]
async fn expense_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let repo = ExpenseRepoLoro::new(&doc);

    let project_id = Uuid::new_v4();
    let spent = Utc::now();

    let e = repo
        .create(ExpenseCreate {
            category: "gear".into(),
            vendor: Some("Sweetwater".into()),
            amount_cents: 199_900,
            currency: "USD".into(),
            spent_at: spent,
            project_id: Some(project_id),
            tax_deductible: true,
            receipt_url: Some("https://example.com/r.pdf".into()),
            notes: Some("new monitors".into()),
            tags: vec!["studio".into()],
        })
        .await
        .unwrap();

    let got = repo.get(e.id).await.unwrap();
    assert_eq!(got.category, "gear");
    assert_eq!(got.vendor.as_deref(), Some("Sweetwater"));
    assert_eq!(got.amount_cents, 199_900);
    assert_eq!(got.currency, "USD");
    assert_eq!(got.project_id, Some(project_id));
    assert!(got.tax_deductible);
    assert_eq!(
        got.receipt_url.as_deref(),
        Some("https://example.com/r.pdf")
    );
    assert_eq!(got.notes.as_deref(), Some("new monitors"));
    assert_eq!(got.tags, vec!["studio".to_string()]);
}

#[tokio::test]
async fn both_in_one_doc() {
    let doc = CrdtDoc::ephemeral();
    let revenues = RevenueRepoLoro::new(&doc);
    let expenses = ExpenseRepoLoro::new(&doc);

    revenues
        .create(RevenueCreate {
            source: "royalties".into(),
            client_id: None,
            invoice_id: None,
            amount_cents: 10_000,
            currency: "USD".into(),
            received_at: Utc::now(),
            notes: None,
            tags: vec![],
        })
        .await
        .unwrap();

    expenses
        .create(ExpenseCreate {
            category: "subscription".into(),
            vendor: Some("Splice".into()),
            amount_cents: 1_299,
            currency: "USD".into(),
            spent_at: Utc::now(),
            project_id: None,
            tax_deductible: true,
            receipt_url: None,
            notes: None,
            tags: vec![],
        })
        .await
        .unwrap();

    assert_eq!(
        revenues
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
        expenses
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
async fn replicas_converge() {
    let a = CrdtDoc::ephemeral();
    let b = CrdtDoc::ephemeral();
    let ra = RevenueRepoLoro::new(&a);
    let rb = RevenueRepoLoro::new(&b);

    ra.create(RevenueCreate {
        source: "client-work".into(),
        client_id: None,
        invoice_id: None,
        amount_cents: 100,
        currency: "USD".into(),
        received_at: Utc::now(),
        notes: None,
        tags: vec![],
    })
    .await
    .unwrap();
    rb.create(RevenueCreate {
        source: "licensing".into(),
        client_id: None,
        invoice_id: None,
        amount_cents: 200,
        currency: "USD".into(),
        received_at: Utc::now(),
        notes: None,
        tags: vec![],
    })
    .await
    .unwrap();

    let ab = ra.doc().export(ExportMode::all_updates()).unwrap();
    let bb = rb.doc().export(ExportMode::all_updates()).unwrap();
    rb.doc().import(&ab).unwrap();
    ra.doc().import(&bb).unwrap();

    assert_eq!(
        ra.list(
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
        rb.list(
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
