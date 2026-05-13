//! Native integration tests for the `invoice` feature.

#![cfg(test)]

use architect::Page;
use chrono::{Duration, Utc};
use invoice_crdt::{CrdtDoc, InvoiceLineRepoLoro, InvoiceRepoLoro};
use invoice_proto::{InvoiceCreate, InvoiceLineCreate, InvoiceLineRepo, InvoiceRepo};
use loro::ExportMode;
use uuid::Uuid;

#[tokio::test]
async fn invoice_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let repo = InvoiceRepoLoro::new(&doc);

    let client_id = Uuid::new_v4();
    let issue = Utc::now();
    let due = issue + Duration::days(30);

    let inv = repo
        .create(InvoiceCreate {
            number: "INV-2026-0001".into(),
            client_id,
            status: "draft".into(),
            issue_date: issue,
            due_date: Some(due),
            paid_at: None,
            currency: "USD".into(),
            subtotal_cents: 100_000,
            tax_cents: 8_250,
            total_cents: 108_250,
            notes: Some("net 30".into()),
            tags: vec!["q1".into(), "audio".into()],
        })
        .await
        .unwrap();

    let got = repo.get(inv.id).await.unwrap();
    assert_eq!(got.number, "INV-2026-0001");
    assert_eq!(got.client_id, client_id);
    assert_eq!(got.status, "draft");
    assert_eq!(got.currency, "USD");
    assert_eq!(got.subtotal_cents, 100_000);
    assert_eq!(got.tax_cents, 8_250);
    assert_eq!(got.total_cents, 108_250);
    assert_eq!(got.notes.as_deref(), Some("net 30"));
    assert_eq!(got.tags, vec!["q1".to_string(), "audio".to_string()]);
    assert!(got.due_date.is_some());
    assert!(got.paid_at.is_none());
}

#[tokio::test]
async fn line_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let lines = InvoiceLineRepoLoro::new(&doc);

    let invoice_id = Uuid::new_v4();
    let line = lines
        .create(InvoiceLineCreate {
            invoice_id,
            description: "Mixing session".into(),
            quantity_thousandths: 2_000,
            unit_price_cents: 15_000,
            amount_cents: 30_000,
            sort_index: 0,
        })
        .await
        .unwrap();

    let got = lines.get(line.id).await.unwrap();
    assert_eq!(got.invoice_id, invoice_id);
    assert_eq!(got.description, "Mixing session");
    assert_eq!(got.quantity_thousandths, 2_000);
    assert_eq!(got.unit_price_cents, 15_000);
    assert_eq!(got.amount_cents, 30_000);
    assert_eq!(got.sort_index, 0);
}

#[tokio::test]
async fn invoice_and_lines_in_one_doc() {
    let doc = CrdtDoc::ephemeral();
    let invoices = InvoiceRepoLoro::new(&doc);
    let lines = InvoiceLineRepoLoro::new(&doc);

    let inv = invoices
        .create(InvoiceCreate {
            number: "INV-001".into(),
            client_id: Uuid::new_v4(),
            status: "draft".into(),
            issue_date: Utc::now(),
            due_date: None,
            paid_at: None,
            currency: "USD".into(),
            subtotal_cents: 0,
            tax_cents: 0,
            total_cents: 0,
            notes: None,
            tags: vec![],
        })
        .await
        .unwrap();

    for i in 0..2 {
        lines
            .create(InvoiceLineCreate {
                invoice_id: inv.id,
                description: format!("line-{i}"),
                quantity_thousandths: 1_000,
                unit_price_cents: 10_000,
                amount_cents: 10_000,
                sort_index: i,
            })
            .await
            .unwrap();
    }

    let inv_list = invoices
        .list(
            Page {
                index: 0,
                size: 100,
            },
            None,
            None,
        )
        .await
        .unwrap();
    let line_list = lines
        .list(
            Page {
                index: 0,
                size: 100,
            },
            None,
            None,
        )
        .await
        .unwrap();

    assert_eq!(inv_list.total, 1);
    assert_eq!(line_list.total, 2);
}

#[tokio::test]
async fn replicas_converge() {
    let a = CrdtDoc::ephemeral();
    let b = CrdtDoc::ephemeral();
    let ia = InvoiceRepoLoro::new(&a);
    let ib = InvoiceRepoLoro::new(&b);

    ia.create(InvoiceCreate {
        number: "A-1".into(),
        client_id: Uuid::new_v4(),
        status: "draft".into(),
        issue_date: Utc::now(),
        due_date: None,
        paid_at: None,
        currency: "USD".into(),
        subtotal_cents: 0,
        tax_cents: 0,
        total_cents: 0,
        notes: None,
        tags: vec![],
    })
    .await
    .unwrap();
    ib.create(InvoiceCreate {
        number: "B-1".into(),
        client_id: Uuid::new_v4(),
        status: "draft".into(),
        issue_date: Utc::now(),
        due_date: None,
        paid_at: None,
        currency: "USD".into(),
        subtotal_cents: 0,
        tax_cents: 0,
        total_cents: 0,
        notes: None,
        tags: vec![],
    })
    .await
    .unwrap();

    let ab = ia.doc().export(ExportMode::all_updates()).unwrap();
    let bb = ib.doc().export(ExportMode::all_updates()).unwrap();
    ib.doc().import(&ab).unwrap();
    ia.doc().import(&bb).unwrap();

    assert_eq!(
        ia.list(
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
        ib.list(
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
