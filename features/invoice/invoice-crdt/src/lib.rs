//! Loro-backed source-of-truth for the invoice feature. Two
//! entities, two `EntityCrdt` impls, two `*RepoLoro` newtypes.

use architect::{Page, RepoError, SortOrder};
use chrono::Utc;
use crdt::EntityCrdt;
use crdt::codec::{
    read_dt, read_i64, read_opt_dt, read_opt_str, read_str, read_string_list, read_uuid, write_dt,
    write_i64, write_opt_dt, write_opt_str, write_opt_string_list, write_str, write_string_list,
    write_uuid,
};
use invoice_proto::{
    Invoice, InvoiceCreate, InvoiceLine, InvoiceLineCreate, InvoiceLineList, InvoiceLineRepo,
    InvoiceLineUpdate, InvoiceList, InvoiceRepo, InvoiceUpdate,
};
use loro::LoroMap;
use uuid::Uuid;

pub use crdt::{CrdtDoc, LoroRepo};

// ── Invoice ───────────────────────────────────────────────────────────

pub struct InvoiceEntity;

#[derive(Clone)]
pub struct InvoiceRepoLoro {
    inner: LoroRepo<InvoiceEntity>,
}

impl InvoiceRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<InvoiceEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for InvoiceEntity {
    type Wire = Invoice;
    type Create = InvoiceCreate;
    type Update = InvoiceUpdate;
    type List = InvoiceList;

    const ROOT: &'static str = "invoices";

    fn id(w: &Invoice) -> Uuid {
        w.id
    }

    fn from_create(input: InvoiceCreate) -> Invoice {
        let now = Utc::now();
        Invoice {
            id: Uuid::new_v4(),
            number: input.number,
            client_id: input.client_id,
            status: input.status,
            issue_date: input.issue_date,
            due_date: input.due_date,
            paid_at: input.paid_at,
            currency: input.currency,
            subtotal_cents: input.subtotal_cents,
            tax_cents: input.tax_cents,
            total_cents: input.total_cents,
            notes: input.notes,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Invoice) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "number", &e.number)?;
        write_uuid(m, "client_id", e.client_id)?;
        write_str(m, "status", &e.status)?;
        write_dt(m, "issue_date", e.issue_date)?;
        write_opt_dt(m, "due_date", e.due_date)?;
        write_opt_dt(m, "paid_at", e.paid_at)?;
        write_str(m, "currency", &e.currency)?;
        write_i64(m, "subtotal_cents", e.subtotal_cents)?;
        write_i64(m, "tax_cents", e.tax_cents)?;
        write_i64(m, "total_cents", e.total_cents)?;
        write_opt_str(m, "notes", e.notes.as_deref())?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Invoice, RepoError> {
        Ok(Invoice {
            id: read_uuid(m, "id")?,
            number: read_str(m, "number")?,
            client_id: read_uuid(m, "client_id")?,
            status: read_str(m, "status")?,
            issue_date: read_dt(m, "issue_date")?,
            due_date: read_opt_dt(m, "due_date")?,
            paid_at: read_opt_dt(m, "paid_at")?,
            currency: read_str(m, "currency")?,
            subtotal_cents: read_i64(m, "subtotal_cents")?,
            tax_cents: read_i64(m, "tax_cents")?,
            total_cents: read_i64(m, "total_cents")?,
            notes: read_opt_str(m, "notes")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: InvoiceUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.number {
            write_str(m, "number", &v)?;
        }
        if let Some(v) = u.client_id {
            write_uuid(m, "client_id", v)?;
        }
        if let Some(v) = u.status {
            write_str(m, "status", &v)?;
        }
        if let Some(v) = u.issue_date {
            write_dt(m, "issue_date", v)?;
        }
        if let Some(v) = u.due_date {
            write_opt_dt(m, "due_date", v)?;
        }
        if let Some(v) = u.paid_at {
            write_opt_dt(m, "paid_at", v)?;
        }
        if let Some(v) = u.currency {
            write_str(m, "currency", &v)?;
        }
        if let Some(v) = u.subtotal_cents {
            write_i64(m, "subtotal_cents", v)?;
        }
        if let Some(v) = u.tax_cents {
            write_i64(m, "tax_cents", v)?;
        }
        if let Some(v) = u.total_cents {
            write_i64(m, "total_cents", v)?;
        }
        if let Some(v) = u.notes {
            write_opt_str(m, "notes", v.as_deref())?;
        }
        if let Some(v) = u.tags {
            write_opt_string_list(m, "tags", Some(&v))?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [Invoice], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "number" => items.sort_by(|a, b| a.number.cmp(&b.number)),
            "status" => items.sort_by(|a, b| a.status.cmp(&b.status)),
            "issue_date" => items.sort_by(|a, b| a.issue_date.cmp(&b.issue_date)),
            "due_date" => items.sort_by(|a, b| a.due_date.cmp(&b.due_date)),
            "paid_at" => items.sort_by(|a, b| a.paid_at.cmp(&b.paid_at)),
            "subtotal_cents" => items.sort_by(|a, b| a.subtotal_cents.cmp(&b.subtotal_cents)),
            "total_cents" => items.sort_by(|a, b| a.total_cents.cmp(&b.total_cents)),
            other => {
                return Err(RepoError::InvalidInput(format!(
                    "unsortable field: {other}"
                )));
            }
        }
        if matches!(order, SortOrder::Desc) {
            items.reverse();
        }
        Ok(())
    }

    fn build_list(items: Vec<Invoice>, total: u32, page: Page) -> InvoiceList {
        InvoiceList { items, total, page }
    }
}

impl InvoiceRepo for InvoiceRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Invoice, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<InvoiceList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: InvoiceCreate) -> Result<Invoice, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: InvoiceUpdate) -> Result<Invoice, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── InvoiceLine ───────────────────────────────────────────────────────

pub struct InvoiceLineEntity;

#[derive(Clone)]
pub struct InvoiceLineRepoLoro {
    inner: LoroRepo<InvoiceLineEntity>,
}

impl InvoiceLineRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<InvoiceLineEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for InvoiceLineEntity {
    type Wire = InvoiceLine;
    type Create = InvoiceLineCreate;
    type Update = InvoiceLineUpdate;
    type List = InvoiceLineList;

    const ROOT: &'static str = "invoice_lines";

    fn id(w: &InvoiceLine) -> Uuid {
        w.id
    }

    fn from_create(input: InvoiceLineCreate) -> InvoiceLine {
        let now = Utc::now();
        InvoiceLine {
            id: Uuid::new_v4(),
            invoice_id: input.invoice_id,
            description: input.description,
            quantity_thousandths: input.quantity_thousandths,
            unit_price_cents: input.unit_price_cents,
            amount_cents: input.amount_cents,
            sort_index: input.sort_index,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &InvoiceLine) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "invoice_id", e.invoice_id)?;
        write_str(m, "description", &e.description)?;
        write_i64(m, "quantity_thousandths", e.quantity_thousandths)?;
        write_i64(m, "unit_price_cents", e.unit_price_cents)?;
        write_i64(m, "amount_cents", e.amount_cents)?;
        write_i64(m, "sort_index", e.sort_index)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<InvoiceLine, RepoError> {
        Ok(InvoiceLine {
            id: read_uuid(m, "id")?,
            invoice_id: read_uuid(m, "invoice_id")?,
            description: read_str(m, "description")?,
            quantity_thousandths: read_i64(m, "quantity_thousandths")?,
            unit_price_cents: read_i64(m, "unit_price_cents")?,
            amount_cents: read_i64(m, "amount_cents")?,
            sort_index: read_i64(m, "sort_index")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: InvoiceLineUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.invoice_id {
            write_uuid(m, "invoice_id", v)?;
        }
        if let Some(v) = u.description {
            write_str(m, "description", &v)?;
        }
        if let Some(v) = u.quantity_thousandths {
            write_i64(m, "quantity_thousandths", v)?;
        }
        if let Some(v) = u.unit_price_cents {
            write_i64(m, "unit_price_cents", v)?;
        }
        if let Some(v) = u.amount_cents {
            write_i64(m, "amount_cents", v)?;
        }
        if let Some(v) = u.sort_index {
            write_i64(m, "sort_index", v)?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(
        items: &mut [InvoiceLine],
        field: &str,
        order: SortOrder,
    ) -> Result<(), RepoError> {
        match field {
            "invoice_id" => items.sort_by(|a, b| a.invoice_id.cmp(&b.invoice_id)),
            "amount_cents" => items.sort_by(|a, b| a.amount_cents.cmp(&b.amount_cents)),
            "sort_index" => items.sort_by(|a, b| a.sort_index.cmp(&b.sort_index)),
            other => {
                return Err(RepoError::InvalidInput(format!(
                    "unsortable field: {other}"
                )));
            }
        }
        if matches!(order, SortOrder::Desc) {
            items.reverse();
        }
        Ok(())
    }

    fn build_list(items: Vec<InvoiceLine>, total: u32, page: Page) -> InvoiceLineList {
        InvoiceLineList { items, total, page }
    }
}

impl InvoiceLineRepo for InvoiceLineRepoLoro {
    async fn get(&self, id: Uuid) -> Result<InvoiceLine, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<InvoiceLineList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: InvoiceLineCreate) -> Result<InvoiceLine, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: InvoiceLineUpdate) -> Result<InvoiceLine, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}
