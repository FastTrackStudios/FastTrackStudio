//! Loro-backed source-of-truth for the finance feature. Three
//! entities (Revenue, Expense, FinancialAsset), three `EntityCrdt`
//! impls, three `*RepoLoro` newtypes.

use architect::{Page, RepoError, SortOrder};
use chrono::Utc;
use crdt::EntityCrdt;
use crdt::codec::{
    read_bool, read_dt, read_i64, read_opt_dt, read_opt_i64, read_opt_str, read_opt_uuid, read_str,
    read_string_list, read_uuid, write_bool, write_dt, write_i64, write_opt_dt, write_opt_i64,
    write_opt_str, write_opt_string_list, write_opt_uuid, write_str, write_string_list, write_uuid,
};
use finance_proto::{
    Expense, ExpenseCreate, ExpenseList, ExpenseRepo, ExpenseUpdate, FinancialAsset,
    FinancialAssetCreate, FinancialAssetList, FinancialAssetRepo, FinancialAssetUpdate, Revenue,
    RevenueCreate, RevenueList, RevenueRepo, RevenueUpdate,
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

// ── FinancialAsset ────────────────────────────────────────────────────

pub struct FinancialAssetEntity;

#[derive(Clone)]
pub struct FinancialAssetRepoLoro {
    inner: LoroRepo<FinancialAssetEntity>,
}

impl FinancialAssetRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<FinancialAssetEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for FinancialAssetEntity {
    type Wire = FinancialAsset;
    type Create = FinancialAssetCreate;
    type Update = FinancialAssetUpdate;
    type List = FinancialAssetList;

    const ROOT: &'static str = "financial_assets";

    fn id(w: &FinancialAsset) -> Uuid {
        w.id
    }

    fn from_create(input: FinancialAssetCreate) -> FinancialAsset {
        let now = Utc::now();
        FinancialAsset {
            id: Uuid::new_v4(),
            name: input.name,
            kind: input.kind,
            symbol: input.symbol,
            purchase_price_cents: input.purchase_price_cents,
            current_value_cents: input.current_value_cents,
            quantity_thousandths: input.quantity_thousandths,
            currency: input.currency,
            purchase_date: input.purchase_date,
            sold_date: input.sold_date,
            monthly_income_cents: input.monthly_income_cents,
            account: input.account,
            owner: input.owner,
            notes: input.notes,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &FinancialAsset) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "name", &e.name)?;
        write_str(m, "kind", &e.kind)?;
        write_opt_str(m, "symbol", e.symbol.as_deref())?;
        write_opt_i64(m, "purchase_price_cents", e.purchase_price_cents)?;
        write_opt_i64(m, "current_value_cents", e.current_value_cents)?;
        write_opt_i64(m, "quantity_thousandths", e.quantity_thousandths)?;
        write_str(m, "currency", &e.currency)?;
        write_opt_dt(m, "purchase_date", e.purchase_date)?;
        write_opt_dt(m, "sold_date", e.sold_date)?;
        write_opt_i64(m, "monthly_income_cents", e.monthly_income_cents)?;
        write_opt_str(m, "account", e.account.as_deref())?;
        write_opt_str(m, "owner", e.owner.as_deref())?;
        write_opt_str(m, "notes", e.notes.as_deref())?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<FinancialAsset, RepoError> {
        Ok(FinancialAsset {
            id: read_uuid(m, "id")?,
            name: read_str(m, "name")?,
            kind: read_str(m, "kind")?,
            symbol: read_opt_str(m, "symbol")?,
            purchase_price_cents: read_opt_i64(m, "purchase_price_cents")?,
            current_value_cents: read_opt_i64(m, "current_value_cents")?,
            quantity_thousandths: read_opt_i64(m, "quantity_thousandths")?,
            currency: read_str(m, "currency")?,
            purchase_date: read_opt_dt(m, "purchase_date")?,
            sold_date: read_opt_dt(m, "sold_date")?,
            monthly_income_cents: read_opt_i64(m, "monthly_income_cents")?,
            account: read_opt_str(m, "account")?,
            owner: read_opt_str(m, "owner")?,
            notes: read_opt_str(m, "notes")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: FinancialAssetUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.kind {
            write_str(m, "kind", &v)?;
        }
        if let Some(v) = u.symbol {
            write_opt_str(m, "symbol", v.as_deref())?;
        }
        if let Some(v) = u.purchase_price_cents {
            write_opt_i64(m, "purchase_price_cents", v)?;
        }
        if let Some(v) = u.current_value_cents {
            write_opt_i64(m, "current_value_cents", v)?;
        }
        if let Some(v) = u.quantity_thousandths {
            write_opt_i64(m, "quantity_thousandths", v)?;
        }
        if let Some(v) = u.currency {
            write_str(m, "currency", &v)?;
        }
        if let Some(v) = u.purchase_date {
            write_opt_dt(m, "purchase_date", v)?;
        }
        if let Some(v) = u.sold_date {
            write_opt_dt(m, "sold_date", v)?;
        }
        if let Some(v) = u.monthly_income_cents {
            write_opt_i64(m, "monthly_income_cents", v)?;
        }
        if let Some(v) = u.account {
            write_opt_str(m, "account", v.as_deref())?;
        }
        if let Some(v) = u.owner {
            write_opt_str(m, "owner", v.as_deref())?;
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
        items: &mut [FinancialAsset],
        field: &str,
        order: SortOrder,
    ) -> Result<(), RepoError> {
        match field {
            "name" => items.sort_by(|a, b| a.name.cmp(&b.name)),
            "current_value_cents" => items.sort_by(|a, b| {
                a.current_value_cents
                    .unwrap_or(0)
                    .cmp(&b.current_value_cents.unwrap_or(0))
            }),
            "purchase_price_cents" => items.sort_by(|a, b| {
                a.purchase_price_cents
                    .unwrap_or(0)
                    .cmp(&b.purchase_price_cents.unwrap_or(0))
            }),
            "purchase_date" => items.sort_by(|a, b| a.purchase_date.cmp(&b.purchase_date)),
            "monthly_income_cents" => items.sort_by(|a, b| {
                a.monthly_income_cents
                    .unwrap_or(0)
                    .cmp(&b.monthly_income_cents.unwrap_or(0))
            }),
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

    fn build_list(items: Vec<FinancialAsset>, total: u32, page: Page) -> FinancialAssetList {
        FinancialAssetList { items, total, page }
    }
}

impl FinancialAssetRepo for FinancialAssetRepoLoro {
    async fn get(&self, id: Uuid) -> Result<FinancialAsset, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<FinancialAssetList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: FinancialAssetCreate) -> Result<FinancialAsset, RepoError> {
        self.inner.create(input).await
    }
    async fn update(
        &self,
        id: Uuid,
        input: FinancialAssetUpdate,
    ) -> Result<FinancialAsset, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}
