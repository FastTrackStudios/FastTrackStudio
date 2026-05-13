//! Loro-backed source-of-truth for the finance feature. Two
//! entities, two `EntityCrdt` impls, two `*RepoLoro` newtypes.

use architect::{Page, RepoError, SortOrder};
use chrono::Utc;
use crdt::EntityCrdt;
use crdt::codec::{
    read_bool, read_dt, read_i64, read_opt_str, read_opt_uuid, read_str, read_string_list,
    read_uuid, write_bool, write_dt, write_i64, write_opt_str, write_opt_string_list,
    write_opt_uuid, write_str, write_string_list, write_uuid,
};
use finance_proto::{
    Expense, ExpenseCreate, ExpenseList, ExpenseRepo, ExpenseUpdate, Revenue, RevenueCreate,
    RevenueList, RevenueRepo, RevenueUpdate,
};
use loro::LoroMap;
use uuid::Uuid;

pub use crdt::{CrdtDoc, LoroRepo};

// ── Revenue ───────────────────────────────────────────────────────────

pub struct RevenueEntity;

#[derive(Clone)]
pub struct RevenueRepoLoro {
    inner: LoroRepo<RevenueEntity>,
}

impl RevenueRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<RevenueEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for RevenueEntity {
    type Wire = Revenue;
    type Create = RevenueCreate;
    type Update = RevenueUpdate;
    type List = RevenueList;

    const ROOT: &'static str = "revenues";

    fn id(w: &Revenue) -> Uuid {
        w.id
    }

    fn from_create(input: RevenueCreate) -> Revenue {
        let now = Utc::now();
        Revenue {
            id: Uuid::new_v4(),
            source: input.source,
            client_id: input.client_id,
            invoice_id: input.invoice_id,
            amount_cents: input.amount_cents,
            currency: input.currency,
            received_at: input.received_at,
            notes: input.notes,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Revenue) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "source", &e.source)?;
        write_opt_uuid(m, "client_id", e.client_id)?;
        write_opt_uuid(m, "invoice_id", e.invoice_id)?;
        write_i64(m, "amount_cents", e.amount_cents)?;
        write_str(m, "currency", &e.currency)?;
        write_dt(m, "received_at", e.received_at)?;
        write_opt_str(m, "notes", e.notes.as_deref())?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Revenue, RepoError> {
        Ok(Revenue {
            id: read_uuid(m, "id")?,
            source: read_str(m, "source")?,
            client_id: read_opt_uuid(m, "client_id")?,
            invoice_id: read_opt_uuid(m, "invoice_id")?,
            amount_cents: read_i64(m, "amount_cents")?,
            currency: read_str(m, "currency")?,
            received_at: read_dt(m, "received_at")?,
            notes: read_opt_str(m, "notes")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: RevenueUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.source {
            write_str(m, "source", &v)?;
        }
        if let Some(v) = u.client_id {
            write_opt_uuid(m, "client_id", v)?;
        }
        if let Some(v) = u.invoice_id {
            write_opt_uuid(m, "invoice_id", v)?;
        }
        if let Some(v) = u.amount_cents {
            write_i64(m, "amount_cents", v)?;
        }
        if let Some(v) = u.currency {
            write_str(m, "currency", &v)?;
        }
        if let Some(v) = u.received_at {
            write_dt(m, "received_at", v)?;
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

    fn sort_items(items: &mut [Revenue], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "amount_cents" => items.sort_by(|a, b| a.amount_cents.cmp(&b.amount_cents)),
            "received_at" => items.sort_by(|a, b| a.received_at.cmp(&b.received_at)),
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

    fn build_list(items: Vec<Revenue>, total: u32, page: Page) -> RevenueList {
        RevenueList { items, total, page }
    }
}

impl RevenueRepo for RevenueRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Revenue, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<RevenueList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: RevenueCreate) -> Result<Revenue, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: RevenueUpdate) -> Result<Revenue, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── Expense ───────────────────────────────────────────────────────────

pub struct ExpenseEntity;

#[derive(Clone)]
pub struct ExpenseRepoLoro {
    inner: LoroRepo<ExpenseEntity>,
}

impl ExpenseRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<ExpenseEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for ExpenseEntity {
    type Wire = Expense;
    type Create = ExpenseCreate;
    type Update = ExpenseUpdate;
    type List = ExpenseList;

    const ROOT: &'static str = "expenses";

    fn id(w: &Expense) -> Uuid {
        w.id
    }

    fn from_create(input: ExpenseCreate) -> Expense {
        let now = Utc::now();
        Expense {
            id: Uuid::new_v4(),
            category: input.category,
            vendor: input.vendor,
            amount_cents: input.amount_cents,
            currency: input.currency,
            spent_at: input.spent_at,
            project_id: input.project_id,
            tax_deductible: input.tax_deductible,
            receipt_url: input.receipt_url,
            notes: input.notes,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Expense) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "category", &e.category)?;
        write_opt_str(m, "vendor", e.vendor.as_deref())?;
        write_i64(m, "amount_cents", e.amount_cents)?;
        write_str(m, "currency", &e.currency)?;
        write_dt(m, "spent_at", e.spent_at)?;
        write_opt_uuid(m, "project_id", e.project_id)?;
        write_bool(m, "tax_deductible", e.tax_deductible)?;
        write_opt_str(m, "receipt_url", e.receipt_url.as_deref())?;
        write_opt_str(m, "notes", e.notes.as_deref())?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Expense, RepoError> {
        Ok(Expense {
            id: read_uuid(m, "id")?,
            category: read_str(m, "category")?,
            vendor: read_opt_str(m, "vendor")?,
            amount_cents: read_i64(m, "amount_cents")?,
            currency: read_str(m, "currency")?,
            spent_at: read_dt(m, "spent_at")?,
            project_id: read_opt_uuid(m, "project_id")?,
            tax_deductible: read_bool(m, "tax_deductible")?,
            receipt_url: read_opt_str(m, "receipt_url")?,
            notes: read_opt_str(m, "notes")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: ExpenseUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.category {
            write_str(m, "category", &v)?;
        }
        if let Some(v) = u.vendor {
            write_opt_str(m, "vendor", v.as_deref())?;
        }
        if let Some(v) = u.amount_cents {
            write_i64(m, "amount_cents", v)?;
        }
        if let Some(v) = u.currency {
            write_str(m, "currency", &v)?;
        }
        if let Some(v) = u.spent_at {
            write_dt(m, "spent_at", v)?;
        }
        if let Some(v) = u.project_id {
            write_opt_uuid(m, "project_id", v)?;
        }
        if let Some(v) = u.tax_deductible {
            write_bool(m, "tax_deductible", v)?;
        }
        if let Some(v) = u.receipt_url {
            write_opt_str(m, "receipt_url", v.as_deref())?;
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

    fn sort_items(items: &mut [Expense], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "category" => items.sort_by(|a, b| a.category.cmp(&b.category)),
            "amount_cents" => items.sort_by(|a, b| a.amount_cents.cmp(&b.amount_cents)),
            "spent_at" => items.sort_by(|a, b| a.spent_at.cmp(&b.spent_at)),
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

    fn build_list(items: Vec<Expense>, total: u32, page: Page) -> ExpenseList {
        ExpenseList { items, total, page }
    }
}

impl ExpenseRepo for ExpenseRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Expense, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<ExpenseList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: ExpenseCreate) -> Result<Expense, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: ExpenseUpdate) -> Result<Expense, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}
