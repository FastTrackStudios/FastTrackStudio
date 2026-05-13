//! Native integration tests for the `invoice` feature.

#![cfg(test)]

use architect::Page;
use chrono::{Duration, Utc};
use invoice_crdt::{
    ClientRepoLoro, CrdtDoc, InvoiceLineRepoLoro, InvoiceRepoLoro, PaymentRepoLoro,
};
use invoice_proto::{
    ClientCreate, ClientRepo, InvoiceCreate, InvoiceLineCreate, InvoiceLineRepo, InvoiceRepo,
    InvoiceUpdate, PaymentCreate, PaymentRepo,
};
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
            discount_cents: 0,
            tax_rate_bps: 825,
            tax_inclusive: false,
            tax_cents: 8_250,
            total_cents: 108_250,
            balance_cents: 108_250,
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
            project_id: None,
            time_entry_id: None,
            description: "Mixing session".into(),
            quantity_thousandths: 2_000,
            unit_price_cents: 15_000,
            amount_cents: 30_000,
            tax_rate_bps: None,
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
            discount_cents: 0,
            tax_rate_bps: 0,
            tax_inclusive: false,
            tax_cents: 0,
            total_cents: 0,
            balance_cents: 0,
            notes: None,
            tags: vec![],
        })
        .await
        .unwrap();

    for i in 0..2 {
        lines
            .create(InvoiceLineCreate {
                invoice_id: inv.id,
                project_id: None,
                time_entry_id: None,
                description: format!("line-{i}"),
                quantity_thousandths: 1_000,
                unit_price_cents: 10_000,
                amount_cents: 10_000,
                tax_rate_bps: None,
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
        discount_cents: 0,
        tax_rate_bps: 0,
        tax_inclusive: false,
        tax_cents: 0,
        total_cents: 0,
        balance_cents: 0,
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
        discount_cents: 0,
        tax_rate_bps: 0,
        tax_inclusive: false,
        tax_cents: 0,
        total_cents: 0,
        balance_cents: 0,
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

#[tokio::test]
async fn client_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let clients = ClientRepoLoro::new(&doc);

    let c = clients
        .create(ClientCreate {
            name: "Acme Studios".into(),
            email: Some("billing@acme.test".into()),
            phone: Some("+1-555-0100".into()),
            billing_address_line1: Some("123 Market St".into()),
            billing_address_line2: None,
            billing_city: Some("Portland".into()),
            billing_region: Some("OR".into()),
            billing_postal_code: Some("97204".into()),
            billing_country: Some("US".into()),
            currency: "USD".into(),
            default_rate_cents: Some(15_000),
            notes: Some("preferred customer".into()),
            tags: vec!["recurring".into()],
        })
        .await
        .unwrap();

    let got = clients.get(c.id).await.unwrap();
    assert_eq!(got.name, "Acme Studios");
    assert_eq!(got.email.as_deref(), Some("billing@acme.test"));
    assert_eq!(got.billing_country.as_deref(), Some("US"));
    assert_eq!(got.currency, "USD");
    assert_eq!(got.default_rate_cents, Some(15_000));
    assert_eq!(got.tags, vec!["recurring".to_string()]);
}

#[tokio::test]
async fn payment_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let payments = PaymentRepoLoro::new(&doc);

    let invoice_id = Uuid::new_v4();
    let p = payments
        .create(PaymentCreate {
            invoice_id,
            amount_cents: 50_000,
            paid_at: Utc::now(),
            method: "stripe".into(),
            reference: Some("ch_abc123".into()),
            notes: None,
        })
        .await
        .unwrap();

    let got = payments.get(p.id).await.unwrap();
    assert_eq!(got.invoice_id, invoice_id);
    assert_eq!(got.amount_cents, 50_000);
    assert_eq!(got.method, "stripe");
    assert_eq!(got.reference.as_deref(), Some("ch_abc123"));
}

#[tokio::test]
async fn balance_recomputation_after_payments() {
    let doc = CrdtDoc::ephemeral();
    let invoices = InvoiceRepoLoro::new(&doc);
    let payments = PaymentRepoLoro::new(&doc);

    let inv = invoices
        .create(InvoiceCreate {
            number: "INV-BAL-001".into(),
            client_id: Uuid::new_v4(),
            status: "sent".into(),
            issue_date: Utc::now(),
            due_date: Some(Utc::now() + Duration::days(14)),
            paid_at: None,
            currency: "USD".into(),
            subtotal_cents: 100_000,
            discount_cents: 0,
            tax_rate_bps: 0,
            tax_inclusive: false,
            tax_cents: 0,
            total_cents: 100_000,
            balance_cents: 100_000,
            notes: None,
            tags: vec![],
        })
        .await
        .unwrap();

    // Two partial payments: 30k + 25k = 55k.
    for amt in [30_000i64, 25_000] {
        payments
            .create(PaymentCreate {
                invoice_id: inv.id,
                amount_cents: amt,
                paid_at: Utc::now(),
                method: "bank-transfer".into(),
                reference: None,
                notes: None,
            })
            .await
            .unwrap();
    }

    // Recompute balance like InvoiceServiceImpl::record_payment does.
    let all = payments
        .list(
            Page {
                index: 0,
                size: 1000,
            },
            None,
            None,
        )
        .await
        .unwrap();
    let sum: i64 = all
        .items
        .iter()
        .filter(|p| p.invoice_id == inv.id)
        .map(|p| p.amount_cents)
        .sum();
    let new_balance = inv.total_cents - sum;
    invoices
        .update(
            inv.id,
            InvoiceUpdate {
                balance_cents: Some(new_balance),
                ..Default::default()
            },
        )
        .await
        .unwrap();

    let refreshed = invoices.get(inv.id).await.unwrap();
    assert_eq!(refreshed.balance_cents, 45_000);
}

#[tokio::test]
async fn line_records_project_and_time_entry_links() {
    let doc = CrdtDoc::ephemeral();
    let lines = InvoiceLineRepoLoro::new(&doc);

    let invoice_id = Uuid::new_v4();
    let project_id = Uuid::new_v4();
    let time_entry_id = Uuid::new_v4();

    let line = lines
        .create(InvoiceLineCreate {
            invoice_id,
            project_id: Some(project_id),
            time_entry_id: Some(time_entry_id),
            description: "Roll-up: 4h engineering".into(),
            quantity_thousandths: 4_000,
            unit_price_cents: 15_000,
            amount_cents: 60_000,
            tax_rate_bps: Some(700),
            sort_index: 0,
        })
        .await
        .unwrap();

    let got = lines.get(line.id).await.unwrap();
    assert_eq!(got.project_id, Some(project_id));
    assert_eq!(got.time_entry_id, Some(time_entry_id));
    assert_eq!(got.tax_rate_bps, Some(700));
}
