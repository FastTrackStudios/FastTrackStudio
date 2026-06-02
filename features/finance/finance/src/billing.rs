//! `FinanceBackend` — the mounted [`Invoicing`] service.
//!
//! Holds the org's finance + timer SQLite connections. Persists
//! invoices in `finance-db`, links billed sessions back via
//! `timer_work_sessions.invoice_id`, and answers the UI's
//! generate / list / pay / uninvoiced queries. The `Invoicing` trait
//! is sync, so (like `LedgerService`) each method `block_on`s a
//! captured runtime handle around the async DB work.
//!
//! MVP fidelity: persisted invoices with Draft → Sent → Paid /
//! PartiallyPaid status + simple numbering. Double-entry ledger
//! postings, taxes, and credit notes are deferred (the proto supports
//! them; the four unimplemented `Invoicing` methods return a backend
//! error for now).

use architect::HasDispatcher;
use architect::dispatch::TokioBlockingDispatcher;
use chrono::{Datelike, Duration, NaiveDate, TimeZone, Utc};
use sea_orm::sea_query::Expr;
use sea_orm::{
    ActiveModelTrait, ColumnTrait, DatabaseConnection, EntityTrait, QueryFilter, QueryOrder, Set,
};
use uuid::Uuid;

use finance_db::entity::{
    BookActive, BookEntity, BookModel, InvoiceActive, InvoiceColumn, InvoiceEntity, InvoiceModel,
    PartyActive, PartyColumn, PartyEntity, PartyModel,
};
use finance_proto::book::{Book, BookKind};
use finance_proto::error::FinanceError;
use finance_proto::invoice::{Invoice, InvoiceStatus};
use finance_proto::party::{Party, PartyKind};
use finance_proto::service::invoicing::{
    GenerateInvoice, Invoicing, RecordPayment, UninvoicedGroup,
};
use timer::entity::{WorkSessionColumn, WorkSessionEntity};

use crate::invoice_from_sessions::{BuildInvoiceArgs, build_invoice_from_sessions};

/// Disk-backed invoicing service for one org.
#[derive(Clone)]
pub struct FinanceBackend {
    finance: DatabaseConnection,
    timer: DatabaseConnection,
    runtime: tokio::runtime::Handle,
    org_name: String,
}

impl FinanceBackend {
    /// Build over the org's finance + timer connections. Must be called
    /// from inside a tokio runtime.
    pub fn new(
        finance: DatabaseConnection,
        timer: DatabaseConnection,
        org_name: impl Into<String>,
    ) -> Result<Self, &'static str> {
        let runtime = tokio::runtime::Handle::try_current()
            .map_err(|_| "FinanceBackend::new must be called from a tokio runtime")?;
        Ok(Self {
            finance,
            timer,
            runtime,
            org_name: org_name.into(),
        })
    }

    fn err(e: sea_orm::DbErr) -> FinanceError {
        FinanceError::Backend {
            message: e.to_string(),
        }
    }

    fn backend(msg: impl Into<String>) -> FinanceError {
        FinanceError::Backend {
            message: msg.into(),
        }
    }

    // ── book / party (find-or-create) ──────────────────────────────

    async fn ensure_book(&self) -> Result<Book, FinanceError> {
        if let Some(m) = BookEntity::find()
            .one(&self.finance)
            .await
            .map_err(Self::err)?
        {
            return Ok(model_to_book(m));
        }
        let now = Utc::now();
        let book = Book {
            id: Uuid::new_v4(),
            name: self.org_name.clone(),
            kind: BookKind::Business,
            base_currency: "USD".into(),
            settings_json: "{}".into(),
            created_at: now,
            updated_at: now,
        };
        BookEntity::insert(book_to_active(&book))
            .exec(&self.finance)
            .await
            .map_err(Self::err)?;
        Ok(book)
    }

    async fn ensure_party(&self, book_id: Uuid, name: &str) -> Result<Party, FinanceError> {
        if let Some(m) = PartyEntity::find()
            .filter(PartyColumn::BookId.eq(book_id))
            .filter(PartyColumn::DisplayName.eq(name))
            .one(&self.finance)
            .await
            .map_err(Self::err)?
        {
            return Ok(model_to_party(m));
        }
        let now = Utc::now();
        let party = Party {
            id: Uuid::new_v4(),
            book_id,
            kind: PartyKind::Client,
            display_name: name.to_string(),
            legal_name: name.to_string(),
            email: String::new(),
            phone: String::new(),
            address: String::new(),
            tax_id: String::new(),
            default_currency: "USD".into(),
            default_net_days: 30,
            default_rate_minor_per_hour: 0,
            notes: String::new(),
            is_archived: false,
            created_at: now,
            updated_at: now,
        };
        PartyEntity::insert(party_to_active(&party))
            .exec(&self.finance)
            .await
            .map_err(Self::err)?;
        Ok(party)
    }

    // ── operations ─────────────────────────────────────────────────

    async fn generate_inner(&self, req: GenerateInvoice) -> Result<Invoice, FinanceError> {
        let book = self.ensure_book().await?;
        let party = self.ensure_party(book.id, req.client_name.trim()).await?;

        let since = parse_day(&req.since).unwrap_or_else(|| Utc.timestamp_opt(0, 0).unwrap());
        let until = parse_day(&req.until)
            .map(|d| d + Duration::days(1))
            .unwrap_or_else(|| Utc::now() + Duration::days(1));

        let build = build_invoice_from_sessions(
            &self.timer,
            BuildInvoiceArgs {
                book,
                party,
                project_id: req.project_id,
                since,
                until,
                net_days: req.net_days,
                number: String::new(),
                notes_public: String::new(),
                notes_private: String::new(),
                terms: format!("Net {} days from issue date.", req.net_days),
            },
        )
        .await
        .map_err(|e| Self::backend(e.to_string()))?;

        let invoice = build.invoice;
        InvoiceEntity::insert(invoice_to_active(&invoice))
            .exec(&self.finance)
            .await
            .map_err(Self::err)?;

        // Mark the billed sessions so they don't get re-invoiced.
        if !build.source_session_ids.is_empty() {
            WorkSessionEntity::update_many()
                .col_expr(WorkSessionColumn::InvoiceId, Expr::value(invoice.id))
                .filter(WorkSessionColumn::Id.is_in(build.source_session_ids.clone()))
                .exec(&self.timer)
                .await
                .map_err(Self::err)?;
        }
        Ok(invoice)
    }

    async fn list_inner(&self) -> Result<Vec<Invoice>, FinanceError> {
        let rows = InvoiceEntity::find()
            .order_by_desc(InvoiceColumn::CreatedAt)
            .all(&self.finance)
            .await
            .map_err(Self::err)?;
        Ok(rows.into_iter().map(model_to_invoice).collect())
    }

    async fn get_inner(&self, id: Uuid) -> Result<Invoice, FinanceError> {
        InvoiceEntity::find_by_id(id)
            .one(&self.finance)
            .await
            .map_err(Self::err)?
            .map(model_to_invoice)
            .ok_or_else(|| FinanceError::NotFound { id: id.to_string() })
    }

    async fn delete_inner(&self, id: Uuid) -> Result<(), FinanceError> {
        let inv = self.get_inner(id).await?;
        if inv.status != InvoiceStatus::Draft {
            return Err(Self::backend(
                "only draft invoices can be deleted; void instead",
            ));
        }
        InvoiceEntity::delete_by_id(id)
            .exec(&self.finance)
            .await
            .map_err(Self::err)?;
        // Un-bill its sessions.
        WorkSessionEntity::update_many()
            .col_expr(
                WorkSessionColumn::InvoiceId,
                Expr::value(Option::<Uuid>::None),
            )
            .filter(WorkSessionColumn::InvoiceId.eq(id))
            .exec(&self.timer)
            .await
            .map_err(Self::err)?;
        Ok(())
    }

    async fn pay_inner(
        &self,
        id: Uuid,
        amount_minor: i64,
        _date: String,
    ) -> Result<Invoice, FinanceError> {
        let mut inv = self.get_inner(id).await?;
        inv.amount_paid_minor += amount_minor;
        inv.balance_minor = inv.total_minor - inv.amount_paid_minor;
        inv.status = if inv.balance_minor <= 0 {
            InvoiceStatus::Paid
        } else {
            InvoiceStatus::PartiallyPaid
        };
        inv.updated_at = Utc::now();
        let mut active = invoice_to_active(&inv);
        active.id = sea_orm::ActiveValue::Unchanged(inv.id);
        active.update(&self.finance).await.map_err(Self::err)?;
        Ok(inv)
    }

    async fn mark_sent_inner(&self, id: Uuid) -> Result<Invoice, FinanceError> {
        let mut inv = self.get_inner(id).await?;
        if inv.status == InvoiceStatus::Draft {
            // Assign the next sequential number in the book.
            let issued = InvoiceEntity::find()
                .filter(InvoiceColumn::Number.ne(""))
                .all(&self.finance)
                .await
                .map_err(Self::err)?
                .len();
            inv.number = format!("INV-{}-{:04}", Utc::now().year(), issued + 1);
            inv.status = InvoiceStatus::Sent;
            inv.posted_at = Utc::now();
            inv.locked = true;
            inv.updated_at = Utc::now();
            let mut active = invoice_to_active(&inv);
            active.id = sea_orm::ActiveValue::Unchanged(inv.id);
            active.update(&self.finance).await.map_err(Self::err)?;
        }
        Ok(inv)
    }

    async fn uninvoiced_inner(&self) -> Result<Vec<UninvoicedGroup>, FinanceError> {
        let rows = WorkSessionEntity::find()
            .filter(WorkSessionColumn::Billable.eq(true))
            .filter(WorkSessionColumn::EndTime.is_not_null())
            .filter(WorkSessionColumn::InvoiceId.is_null())
            .all(&self.timer)
            .await
            .map_err(Self::err)?;
        let mut groups: std::collections::BTreeMap<Uuid, UninvoicedGroup> =
            std::collections::BTreeMap::new();
        for s in rows {
            let Some(pid) = s.project_id else { continue };
            let Some(end) = s.end_time else { continue };
            let secs = (end - s.start_time).num_seconds().max(0);
            let g = groups.entry(pid).or_insert_with(|| UninvoicedGroup {
                project_id: pid,
                session_count: 0,
                seconds: 0,
                amount_minor: 0,
                currency: s.currency.clone(),
            });
            g.session_count += 1;
            g.seconds += secs;
            g.amount_minor += secs * s.rate_cents / 3600;
        }
        Ok(groups.into_values().collect())
    }
}

// Sync rpc trait → needs a `spawn_blocking` dispatcher.
impl HasDispatcher for FinanceBackend {
    type Dispatcher = TokioBlockingDispatcher;
    fn dispatcher(&self) -> Self::Dispatcher {
        TokioBlockingDispatcher
    }
}

impl Invoicing for FinanceBackend {
    fn generate_invoice(&self, req: GenerateInvoice) -> Result<Invoice, FinanceError> {
        self.runtime.block_on(self.generate_inner(req))
    }
    fn list_invoices(&self) -> Result<Vec<Invoice>, FinanceError> {
        self.runtime.block_on(self.list_inner())
    }
    fn get_invoice(&self, id: Uuid) -> Result<Invoice, FinanceError> {
        self.runtime.block_on(self.get_inner(id))
    }
    fn delete_invoice(&self, id: Uuid) -> Result<(), FinanceError> {
        self.runtime.block_on(self.delete_inner(id))
    }
    fn record_invoice_payment(
        &self,
        id: Uuid,
        amount_minor: i64,
        date: String,
    ) -> Result<Invoice, FinanceError> {
        self.runtime
            .block_on(self.pay_inner(id, amount_minor, date))
    }
    fn uninvoiced(&self) -> Result<Vec<UninvoicedGroup>, FinanceError> {
        self.runtime.block_on(self.uninvoiced_inner())
    }
    fn mark_sent(&self, id: Uuid) -> Result<Invoice, FinanceError> {
        self.runtime.block_on(self.mark_sent_inner(id))
    }

    // Deferred (ledger / credits / recurring) — not in the MVP.
    fn void_with_credit(&self, _id: Uuid, _reason: String) -> Result<Uuid, FinanceError> {
        Err(Self::backend("void_with_credit not implemented yet"))
    }
    fn record_payment(&self, _payload: RecordPayment) -> Result<Uuid, FinanceError> {
        Err(Self::backend(
            "record_payment (allocations) not implemented yet",
        ))
    }
    fn refund_payment(
        &self,
        _id: Uuid,
        _amount_minor: i64,
        _reason: String,
    ) -> Result<(), FinanceError> {
        Err(Self::backend("refund_payment not implemented yet"))
    }
    fn run_schedule_once(&self, _id: Uuid) -> Result<Option<Uuid>, FinanceError> {
        Err(Self::backend("run_schedule_once not implemented yet"))
    }
}

// ── ISO date helper ────────────────────────────────────────────────

fn parse_day(s: &str) -> Option<chrono::DateTime<Utc>> {
    let d = NaiveDate::parse_from_str(s.trim(), "%Y-%m-%d").ok()?;
    Some(Utc.from_utc_datetime(&d.and_hms_opt(0, 0, 0)?))
}

// ── Model ⇄ proto converters (architect Model has identical fields) ─

fn model_to_book(m: BookModel) -> Book {
    Book {
        id: m.id,
        name: m.name,
        kind: m.kind,
        base_currency: m.base_currency,
        settings_json: m.settings_json,
        created_at: m.created_at,
        updated_at: m.updated_at,
    }
}

fn book_to_active(b: &Book) -> BookActive {
    BookActive {
        id: Set(b.id),
        name: Set(b.name.clone()),
        kind: Set(b.kind.clone()),
        base_currency: Set(b.base_currency.clone()),
        settings_json: Set(b.settings_json.clone()),
        created_at: Set(b.created_at),
        updated_at: Set(b.updated_at),
    }
}

fn model_to_party(m: PartyModel) -> Party {
    Party {
        id: m.id,
        book_id: m.book_id,
        kind: m.kind,
        display_name: m.display_name,
        legal_name: m.legal_name,
        email: m.email,
        phone: m.phone,
        address: m.address,
        tax_id: m.tax_id,
        default_currency: m.default_currency,
        default_net_days: m.default_net_days,
        default_rate_minor_per_hour: m.default_rate_minor_per_hour,
        notes: m.notes,
        is_archived: m.is_archived,
        created_at: m.created_at,
        updated_at: m.updated_at,
    }
}

fn party_to_active(p: &Party) -> PartyActive {
    PartyActive {
        id: Set(p.id),
        book_id: Set(p.book_id),
        kind: Set(p.kind.clone()),
        display_name: Set(p.display_name.clone()),
        legal_name: Set(p.legal_name.clone()),
        email: Set(p.email.clone()),
        phone: Set(p.phone.clone()),
        address: Set(p.address.clone()),
        tax_id: Set(p.tax_id.clone()),
        default_currency: Set(p.default_currency.clone()),
        default_net_days: Set(p.default_net_days),
        default_rate_minor_per_hour: Set(p.default_rate_minor_per_hour),
        notes: Set(p.notes.clone()),
        is_archived: Set(p.is_archived),
        created_at: Set(p.created_at),
        updated_at: Set(p.updated_at),
    }
}

fn model_to_invoice(m: InvoiceModel) -> Invoice {
    Invoice {
        id: m.id,
        book_id: m.book_id,
        party_id: m.party_id,
        kind: m.kind,
        number: m.number,
        status: m.status,
        issue_date: m.issue_date,
        due_date: m.due_date,
        currency: m.currency,
        exchange_rate_micro: m.exchange_rate_micro,
        line_items: m.line_items,
        invoice_taxes: m.invoice_taxes,
        uses_inclusive_taxes: m.uses_inclusive_taxes,
        subtotal_minor: m.subtotal_minor,
        tax_total_minor: m.tax_total_minor,
        total_minor: m.total_minor,
        amount_paid_minor: m.amount_paid_minor,
        balance_minor: m.balance_minor,
        notes_public: m.notes_public,
        notes_private: m.notes_private,
        terms: m.terms,
        footer: m.footer,
        locked: m.locked,
        posted_at: m.posted_at,
        created_at: m.created_at,
        updated_at: m.updated_at,
    }
}

fn invoice_to_active(i: &Invoice) -> InvoiceActive {
    InvoiceActive {
        id: Set(i.id),
        book_id: Set(i.book_id),
        party_id: Set(i.party_id),
        kind: Set(i.kind.clone()),
        number: Set(i.number.clone()),
        status: Set(i.status.clone()),
        issue_date: Set(i.issue_date.clone()),
        due_date: Set(i.due_date.clone()),
        currency: Set(i.currency.clone()),
        exchange_rate_micro: Set(i.exchange_rate_micro),
        line_items: Set(i.line_items.clone()),
        invoice_taxes: Set(i.invoice_taxes.clone()),
        uses_inclusive_taxes: Set(i.uses_inclusive_taxes),
        subtotal_minor: Set(i.subtotal_minor),
        tax_total_minor: Set(i.tax_total_minor),
        total_minor: Set(i.total_minor),
        amount_paid_minor: Set(i.amount_paid_minor),
        balance_minor: Set(i.balance_minor),
        notes_public: Set(i.notes_public.clone()),
        notes_private: Set(i.notes_private.clone()),
        terms: Set(i.terms.clone()),
        footer: Set(i.footer.clone()),
        locked: Set(i.locked),
        posted_at: Set(i.posted_at),
        created_at: Set(i.created_at),
        updated_at: Set(i.updated_at),
    }
}
