//! Loro-backed source-of-truth for the invoice feature. Four
//! entities, four `EntityCrdt` impls, four `*RepoLoro` newtypes:
//! `Invoice`, `InvoiceLine`, `Client`, `Payment`.

use architect::{Page, RepoError, SortOrder};
use chrono::Utc;
use crdt::EntityCrdt;
use crdt::codec::{
    read_bool, read_dt, read_i64, read_opt_dt, read_opt_i64, read_opt_str, read_opt_uuid, read_str,
    read_string_list, read_uuid, write_bool, write_dt, write_i64, write_opt_dt, write_opt_i64,
    write_opt_str, write_opt_string_list, write_opt_uuid, write_str, write_string_list, write_uuid,
};

// `crdt::codec` has no native i32 helpers — store i32 fields
// (e.g. tax_rate_bps) widened to i64 in Loro, narrow on read.
// Tax rates fit comfortably in i32; we don't need wider precision,
// just a stable on-the-wire representation.
fn write_i32(m: &loro::LoroMap, k: &str, v: i32) -> Result<(), architect::RepoError> {
    write_i64(m, k, v as i64)
}
fn read_i32(m: &loro::LoroMap, k: &str) -> Result<i32, architect::RepoError> {
    Ok(read_i64(m, k)? as i32)
}
fn write_opt_i32(m: &loro::LoroMap, k: &str, v: Option<i32>) -> Result<(), architect::RepoError> {
    write_opt_i64(m, k, v.map(|x| x as i64))
}
fn read_opt_i32(m: &loro::LoroMap, k: &str) -> Result<Option<i32>, architect::RepoError> {
    Ok(read_opt_i64(m, k)?.map(|x| x as i32))
}
use invoice_proto::{
    Client, ClientCreate, ClientList, ClientRepo, ClientUpdate, Invoice, InvoiceCreate,
    InvoiceLine, InvoiceLineCreate, InvoiceLineList, InvoiceLineRepo, InvoiceLineUpdate,
    InvoiceList, InvoiceRepo, InvoiceUpdate, Payment, PaymentCreate, PaymentList, PaymentRepo,
    PaymentUpdate, RecurringInvoice, RecurringInvoiceCreate, RecurringInvoiceLine,
    RecurringInvoiceLineCreate, RecurringInvoiceLineList, RecurringInvoiceLineRepo,
    RecurringInvoiceLineUpdate, RecurringInvoiceList, RecurringInvoiceRepo, RecurringInvoiceUpdate,
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
            discount_cents: input.discount_cents,
            tax_rate_bps: input.tax_rate_bps,
            tax_inclusive: input.tax_inclusive,
            tax_cents: input.tax_cents,
            total_cents: input.total_cents,
            balance_cents: input.balance_cents,
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
        write_i64(m, "discount_cents", e.discount_cents)?;
        write_i32(m, "tax_rate_bps", e.tax_rate_bps)?;
        write_bool(m, "tax_inclusive", e.tax_inclusive)?;
        write_i64(m, "tax_cents", e.tax_cents)?;
        write_i64(m, "total_cents", e.total_cents)?;
        write_i64(m, "balance_cents", e.balance_cents)?;
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
            discount_cents: read_i64(m, "discount_cents")?,
            tax_rate_bps: read_i32(m, "tax_rate_bps")?,
            tax_inclusive: read_bool(m, "tax_inclusive")?,
            tax_cents: read_i64(m, "tax_cents")?,
            total_cents: read_i64(m, "total_cents")?,
            balance_cents: read_i64(m, "balance_cents")?,
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
        if let Some(v) = u.discount_cents {
            write_i64(m, "discount_cents", v)?;
        }
        if let Some(v) = u.tax_rate_bps {
            write_i32(m, "tax_rate_bps", v)?;
        }
        if let Some(v) = u.tax_inclusive {
            write_bool(m, "tax_inclusive", v)?;
        }
        if let Some(v) = u.tax_cents {
            write_i64(m, "tax_cents", v)?;
        }
        if let Some(v) = u.total_cents {
            write_i64(m, "total_cents", v)?;
        }
        if let Some(v) = u.balance_cents {
            write_i64(m, "balance_cents", v)?;
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
            "balance_cents" => items.sort_by(|a, b| a.balance_cents.cmp(&b.balance_cents)),
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
            project_id: input.project_id,
            time_entry_id: input.time_entry_id,
            description: input.description,
            quantity_thousandths: input.quantity_thousandths,
            unit_price_cents: input.unit_price_cents,
            amount_cents: input.amount_cents,
            tax_rate_bps: input.tax_rate_bps,
            sort_index: input.sort_index,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &InvoiceLine) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "invoice_id", e.invoice_id)?;
        write_opt_uuid(m, "project_id", e.project_id)?;
        write_opt_uuid(m, "time_entry_id", e.time_entry_id)?;
        write_str(m, "description", &e.description)?;
        write_i64(m, "quantity_thousandths", e.quantity_thousandths)?;
        write_i64(m, "unit_price_cents", e.unit_price_cents)?;
        write_i64(m, "amount_cents", e.amount_cents)?;
        write_opt_i32(m, "tax_rate_bps", e.tax_rate_bps)?;
        write_i64(m, "sort_index", e.sort_index)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<InvoiceLine, RepoError> {
        Ok(InvoiceLine {
            id: read_uuid(m, "id")?,
            invoice_id: read_uuid(m, "invoice_id")?,
            project_id: read_opt_uuid(m, "project_id")?,
            time_entry_id: read_opt_uuid(m, "time_entry_id")?,
            description: read_str(m, "description")?,
            quantity_thousandths: read_i64(m, "quantity_thousandths")?,
            unit_price_cents: read_i64(m, "unit_price_cents")?,
            amount_cents: read_i64(m, "amount_cents")?,
            tax_rate_bps: read_opt_i32(m, "tax_rate_bps")?,
            sort_index: read_i64(m, "sort_index")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: InvoiceLineUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.invoice_id {
            write_uuid(m, "invoice_id", v)?;
        }
        if let Some(v) = u.project_id {
            write_opt_uuid(m, "project_id", v)?;
        }
        if let Some(v) = u.time_entry_id {
            write_opt_uuid(m, "time_entry_id", v)?;
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
        if let Some(v) = u.tax_rate_bps {
            write_opt_i32(m, "tax_rate_bps", v)?;
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

// ── Client ────────────────────────────────────────────────────────────

pub struct ClientEntity;

#[derive(Clone)]
pub struct ClientRepoLoro {
    inner: LoroRepo<ClientEntity>,
}

impl ClientRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<ClientEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for ClientEntity {
    type Wire = Client;
    type Create = ClientCreate;
    type Update = ClientUpdate;
    type List = ClientList;

    const ROOT: &'static str = "clients";

    fn id(w: &Client) -> Uuid {
        w.id
    }

    fn from_create(input: ClientCreate) -> Client {
        let now = Utc::now();
        Client {
            id: Uuid::new_v4(),
            name: input.name,
            email: input.email,
            phone: input.phone,
            billing_address_line1: input.billing_address_line1,
            billing_address_line2: input.billing_address_line2,
            billing_city: input.billing_city,
            billing_region: input.billing_region,
            billing_postal_code: input.billing_postal_code,
            billing_country: input.billing_country,
            currency: input.currency,
            default_rate_cents: input.default_rate_cents,
            notes: input.notes,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Client) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "name", &e.name)?;
        write_opt_str(m, "email", e.email.as_deref())?;
        write_opt_str(m, "phone", e.phone.as_deref())?;
        write_opt_str(
            m,
            "billing_address_line1",
            e.billing_address_line1.as_deref(),
        )?;
        write_opt_str(
            m,
            "billing_address_line2",
            e.billing_address_line2.as_deref(),
        )?;
        write_opt_str(m, "billing_city", e.billing_city.as_deref())?;
        write_opt_str(m, "billing_region", e.billing_region.as_deref())?;
        write_opt_str(m, "billing_postal_code", e.billing_postal_code.as_deref())?;
        write_opt_str(m, "billing_country", e.billing_country.as_deref())?;
        write_str(m, "currency", &e.currency)?;
        write_opt_i64(m, "default_rate_cents", e.default_rate_cents)?;
        write_opt_str(m, "notes", e.notes.as_deref())?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Client, RepoError> {
        Ok(Client {
            id: read_uuid(m, "id")?,
            name: read_str(m, "name")?,
            email: read_opt_str(m, "email")?,
            phone: read_opt_str(m, "phone")?,
            billing_address_line1: read_opt_str(m, "billing_address_line1")?,
            billing_address_line2: read_opt_str(m, "billing_address_line2")?,
            billing_city: read_opt_str(m, "billing_city")?,
            billing_region: read_opt_str(m, "billing_region")?,
            billing_postal_code: read_opt_str(m, "billing_postal_code")?,
            billing_country: read_opt_str(m, "billing_country")?,
            currency: read_str(m, "currency")?,
            default_rate_cents: read_opt_i64(m, "default_rate_cents")?,
            notes: read_opt_str(m, "notes")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: ClientUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.email {
            write_opt_str(m, "email", v.as_deref())?;
        }
        if let Some(v) = u.phone {
            write_opt_str(m, "phone", v.as_deref())?;
        }
        if let Some(v) = u.billing_address_line1 {
            write_opt_str(m, "billing_address_line1", v.as_deref())?;
        }
        if let Some(v) = u.billing_address_line2 {
            write_opt_str(m, "billing_address_line2", v.as_deref())?;
        }
        if let Some(v) = u.billing_city {
            write_opt_str(m, "billing_city", v.as_deref())?;
        }
        if let Some(v) = u.billing_region {
            write_opt_str(m, "billing_region", v.as_deref())?;
        }
        if let Some(v) = u.billing_postal_code {
            write_opt_str(m, "billing_postal_code", v.as_deref())?;
        }
        if let Some(v) = u.billing_country {
            write_opt_str(m, "billing_country", v.as_deref())?;
        }
        if let Some(v) = u.currency {
            write_str(m, "currency", &v)?;
        }
        if let Some(v) = u.default_rate_cents {
            write_opt_i64(m, "default_rate_cents", v)?;
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

    fn sort_items(items: &mut [Client], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "name" => items.sort_by(|a, b| a.name.cmp(&b.name)),
            "default_rate_cents" => {
                items.sort_by(|a, b| a.default_rate_cents.cmp(&b.default_rate_cents))
            }
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

    fn build_list(items: Vec<Client>, total: u32, page: Page) -> ClientList {
        ClientList { items, total, page }
    }
}

impl ClientRepo for ClientRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Client, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<ClientList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: ClientCreate) -> Result<Client, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: ClientUpdate) -> Result<Client, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── Payment ───────────────────────────────────────────────────────────

pub struct PaymentEntity;

#[derive(Clone)]
pub struct PaymentRepoLoro {
    inner: LoroRepo<PaymentEntity>,
}

impl PaymentRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<PaymentEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for PaymentEntity {
    type Wire = Payment;
    type Create = PaymentCreate;
    type Update = PaymentUpdate;
    type List = PaymentList;

    const ROOT: &'static str = "payments";

    fn id(w: &Payment) -> Uuid {
        w.id
    }

    fn from_create(input: PaymentCreate) -> Payment {
        let now = Utc::now();
        Payment {
            id: Uuid::new_v4(),
            invoice_id: input.invoice_id,
            amount_cents: input.amount_cents,
            paid_at: input.paid_at,
            method: input.method,
            reference: input.reference,
            notes: input.notes,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Payment) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "invoice_id", e.invoice_id)?;
        write_i64(m, "amount_cents", e.amount_cents)?;
        write_dt(m, "paid_at", e.paid_at)?;
        write_str(m, "method", &e.method)?;
        write_opt_str(m, "reference", e.reference.as_deref())?;
        write_opt_str(m, "notes", e.notes.as_deref())?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Payment, RepoError> {
        Ok(Payment {
            id: read_uuid(m, "id")?,
            invoice_id: read_uuid(m, "invoice_id")?,
            amount_cents: read_i64(m, "amount_cents")?,
            paid_at: read_dt(m, "paid_at")?,
            method: read_str(m, "method")?,
            reference: read_opt_str(m, "reference")?,
            notes: read_opt_str(m, "notes")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: PaymentUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.invoice_id {
            write_uuid(m, "invoice_id", v)?;
        }
        if let Some(v) = u.amount_cents {
            write_i64(m, "amount_cents", v)?;
        }
        if let Some(v) = u.paid_at {
            write_dt(m, "paid_at", v)?;
        }
        if let Some(v) = u.method {
            write_str(m, "method", &v)?;
        }
        if let Some(v) = u.reference {
            write_opt_str(m, "reference", v.as_deref())?;
        }
        if let Some(v) = u.notes {
            write_opt_str(m, "notes", v.as_deref())?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [Payment], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "amount_cents" => items.sort_by(|a, b| a.amount_cents.cmp(&b.amount_cents)),
            "paid_at" => items.sort_by(|a, b| a.paid_at.cmp(&b.paid_at)),
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

    fn build_list(items: Vec<Payment>, total: u32, page: Page) -> PaymentList {
        PaymentList { items, total, page }
    }
}

impl PaymentRepo for PaymentRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Payment, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<PaymentList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: PaymentCreate) -> Result<Payment, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: PaymentUpdate) -> Result<Payment, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── RecurringInvoice ──────────────────────────────────────────────────

pub struct RecurringInvoiceEntity;

#[derive(Clone)]
pub struct RecurringInvoiceRepoLoro {
    inner: LoroRepo<RecurringInvoiceEntity>,
}

impl RecurringInvoiceRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<RecurringInvoiceEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for RecurringInvoiceEntity {
    type Wire = RecurringInvoice;
    type Create = RecurringInvoiceCreate;
    type Update = RecurringInvoiceUpdate;
    type List = RecurringInvoiceList;

    const ROOT: &'static str = "recurring_invoices";

    fn id(w: &RecurringInvoice) -> Uuid {
        w.id
    }

    fn from_create(input: RecurringInvoiceCreate) -> RecurringInvoice {
        let now = Utc::now();
        RecurringInvoice {
            id: Uuid::new_v4(),
            client_id: input.client_id,
            status: input.status,
            frequency: input.frequency,
            next_issue_date: input.next_issue_date,
            end_date: input.end_date,
            last_generated_at: input.last_generated_at,
            generated_count: input.generated_count,
            currency: input.currency,
            subtotal_cents: input.subtotal_cents,
            tax_rate_bps: input.tax_rate_bps,
            tax_inclusive: input.tax_inclusive,
            discount_cents: input.discount_cents,
            notes: input.notes,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &RecurringInvoice) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "client_id", e.client_id)?;
        write_str(m, "status", &e.status)?;
        write_str(m, "frequency", &e.frequency)?;
        write_dt(m, "next_issue_date", e.next_issue_date)?;
        write_opt_dt(m, "end_date", e.end_date)?;
        write_opt_dt(m, "last_generated_at", e.last_generated_at)?;
        write_i64(m, "generated_count", e.generated_count)?;
        write_str(m, "currency", &e.currency)?;
        write_i64(m, "subtotal_cents", e.subtotal_cents)?;
        write_i32(m, "tax_rate_bps", e.tax_rate_bps)?;
        write_bool(m, "tax_inclusive", e.tax_inclusive)?;
        write_i64(m, "discount_cents", e.discount_cents)?;
        write_opt_str(m, "notes", e.notes.as_deref())?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<RecurringInvoice, RepoError> {
        Ok(RecurringInvoice {
            id: read_uuid(m, "id")?,
            client_id: read_uuid(m, "client_id")?,
            status: read_str(m, "status")?,
            frequency: read_str(m, "frequency")?,
            next_issue_date: read_dt(m, "next_issue_date")?,
            end_date: read_opt_dt(m, "end_date")?,
            last_generated_at: read_opt_dt(m, "last_generated_at")?,
            generated_count: read_i64(m, "generated_count")?,
            currency: read_str(m, "currency")?,
            subtotal_cents: read_i64(m, "subtotal_cents")?,
            tax_rate_bps: read_i32(m, "tax_rate_bps")?,
            tax_inclusive: read_bool(m, "tax_inclusive")?,
            discount_cents: read_i64(m, "discount_cents")?,
            notes: read_opt_str(m, "notes")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: RecurringInvoiceUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.client_id {
            write_uuid(m, "client_id", v)?;
        }
        if let Some(v) = u.status {
            write_str(m, "status", &v)?;
        }
        if let Some(v) = u.frequency {
            write_str(m, "frequency", &v)?;
        }
        if let Some(v) = u.next_issue_date {
            write_dt(m, "next_issue_date", v)?;
        }
        if let Some(v) = u.end_date {
            write_opt_dt(m, "end_date", v)?;
        }
        if let Some(v) = u.last_generated_at {
            write_opt_dt(m, "last_generated_at", v)?;
        }
        if let Some(v) = u.generated_count {
            write_i64(m, "generated_count", v)?;
        }
        if let Some(v) = u.currency {
            write_str(m, "currency", &v)?;
        }
        if let Some(v) = u.subtotal_cents {
            write_i64(m, "subtotal_cents", v)?;
        }
        if let Some(v) = u.tax_rate_bps {
            write_i32(m, "tax_rate_bps", v)?;
        }
        if let Some(v) = u.tax_inclusive {
            write_bool(m, "tax_inclusive", v)?;
        }
        if let Some(v) = u.discount_cents {
            write_i64(m, "discount_cents", v)?;
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

    fn sort_items(
        items: &mut [RecurringInvoice],
        field: &str,
        order: SortOrder,
    ) -> Result<(), RepoError> {
        match field {
            "next_issue_date" => items.sort_by(|a, b| a.next_issue_date.cmp(&b.next_issue_date)),
            "status" => items.sort_by(|a, b| a.status.cmp(&b.status)),
            "frequency" => items.sort_by(|a, b| a.frequency.cmp(&b.frequency)),
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

    fn build_list(items: Vec<RecurringInvoice>, total: u32, page: Page) -> RecurringInvoiceList {
        RecurringInvoiceList { items, total, page }
    }
}

impl RecurringInvoiceRepo for RecurringInvoiceRepoLoro {
    async fn get(&self, id: Uuid) -> Result<RecurringInvoice, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<RecurringInvoiceList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: RecurringInvoiceCreate) -> Result<RecurringInvoice, RepoError> {
        self.inner.create(input).await
    }
    async fn update(
        &self,
        id: Uuid,
        input: RecurringInvoiceUpdate,
    ) -> Result<RecurringInvoice, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── RecurringInvoiceLine ──────────────────────────────────────────────

pub struct RecurringInvoiceLineEntity;

#[derive(Clone)]
pub struct RecurringInvoiceLineRepoLoro {
    inner: LoroRepo<RecurringInvoiceLineEntity>,
}

impl RecurringInvoiceLineRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<RecurringInvoiceLineEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for RecurringInvoiceLineEntity {
    type Wire = RecurringInvoiceLine;
    type Create = RecurringInvoiceLineCreate;
    type Update = RecurringInvoiceLineUpdate;
    type List = RecurringInvoiceLineList;

    const ROOT: &'static str = "recurring_invoice_lines";

    fn id(w: &RecurringInvoiceLine) -> Uuid {
        w.id
    }

    fn from_create(input: RecurringInvoiceLineCreate) -> RecurringInvoiceLine {
        let now = Utc::now();
        RecurringInvoiceLine {
            id: Uuid::new_v4(),
            recurring_invoice_id: input.recurring_invoice_id,
            project_id: input.project_id,
            description: input.description,
            quantity_thousandths: input.quantity_thousandths,
            unit_price_cents: input.unit_price_cents,
            amount_cents: input.amount_cents,
            sort_index: input.sort_index,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &RecurringInvoiceLine) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "recurring_invoice_id", e.recurring_invoice_id)?;
        write_opt_uuid(m, "project_id", e.project_id)?;
        write_str(m, "description", &e.description)?;
        write_i64(m, "quantity_thousandths", e.quantity_thousandths)?;
        write_i64(m, "unit_price_cents", e.unit_price_cents)?;
        write_i64(m, "amount_cents", e.amount_cents)?;
        write_i64(m, "sort_index", e.sort_index)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<RecurringInvoiceLine, RepoError> {
        Ok(RecurringInvoiceLine {
            id: read_uuid(m, "id")?,
            recurring_invoice_id: read_uuid(m, "recurring_invoice_id")?,
            project_id: read_opt_uuid(m, "project_id")?,
            description: read_str(m, "description")?,
            quantity_thousandths: read_i64(m, "quantity_thousandths")?,
            unit_price_cents: read_i64(m, "unit_price_cents")?,
            amount_cents: read_i64(m, "amount_cents")?,
            sort_index: read_i64(m, "sort_index")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: RecurringInvoiceLineUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.recurring_invoice_id {
            write_uuid(m, "recurring_invoice_id", v)?;
        }
        if let Some(v) = u.project_id {
            write_opt_uuid(m, "project_id", v)?;
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
        items: &mut [RecurringInvoiceLine],
        field: &str,
        order: SortOrder,
    ) -> Result<(), RepoError> {
        match field {
            "recurring_invoice_id" => {
                items.sort_by(|a, b| a.recurring_invoice_id.cmp(&b.recurring_invoice_id))
            }
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

    fn build_list(
        items: Vec<RecurringInvoiceLine>,
        total: u32,
        page: Page,
    ) -> RecurringInvoiceLineList {
        RecurringInvoiceLineList { items, total, page }
    }
}

impl RecurringInvoiceLineRepo for RecurringInvoiceLineRepoLoro {
    async fn get(&self, id: Uuid) -> Result<RecurringInvoiceLine, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<RecurringInvoiceLineList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(
        &self,
        input: RecurringInvoiceLineCreate,
    ) -> Result<RecurringInvoiceLine, RepoError> {
        self.inner.create(input).await
    }
    async fn update(
        &self,
        id: Uuid,
        input: RecurringInvoiceLineUpdate,
    ) -> Result<RecurringInvoiceLine, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}
