//! Loro-backed source-of-truth for the inventory feature. Three
//! entities, three `EntityCrdt` impls, three `*RepoLoro` newtypes.

use architect::{Page, RepoError, SortOrder};
use chrono::Utc;
use crdt::EntityCrdt;
use crdt::codec::{
    read_bool, read_dt, read_i64, read_opt_dt, read_opt_i64, read_opt_str, read_opt_uuid, read_str,
    read_string_list, read_uuid, write_bool, write_dt, write_i64, write_opt_dt, write_opt_i64,
    write_opt_str, write_opt_string_list, write_opt_uuid, write_str, write_string_list, write_uuid,
};
use inventory_proto::{
    FoodProduct, FoodProductCreate, FoodProductList, FoodProductRepo, FoodProductUpdate,
    PantryItem, PantryItemCreate, PantryItemList, PantryItemRepo, PantryItemUpdate,
    ShoppingListItem, ShoppingListItemCreate, ShoppingListItemList, ShoppingListItemRepo,
    ShoppingListItemUpdate,
};
use loro::LoroMap;
use uuid::Uuid;

pub use crdt::{CrdtDoc, LoroRepo};

// ── FoodProduct ───────────────────────────────────────────────────────

pub struct FoodProductEntity;

#[derive(Clone)]
pub struct FoodProductRepoLoro {
    inner: LoroRepo<FoodProductEntity>,
}

impl FoodProductRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<FoodProductEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for FoodProductEntity {
    type Wire = FoodProduct;
    type Create = FoodProductCreate;
    type Update = FoodProductUpdate;
    type List = FoodProductList;

    const ROOT: &'static str = "food_products";

    fn id(w: &FoodProduct) -> Uuid {
        w.id
    }

    fn from_create(input: FoodProductCreate) -> FoodProduct {
        let now = Utc::now();
        FoodProduct {
            id: Uuid::new_v4(),
            name: input.name,
            brand: input.brand,
            category: input.category,
            barcode: input.barcode,
            default_unit: input.default_unit,
            default_qty_thousandths: input.default_qty_thousandths,
            notes: input.notes,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &FoodProduct) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "name", &e.name)?;
        write_opt_str(m, "brand", e.brand.as_deref())?;
        write_opt_str(m, "category", e.category.as_deref())?;
        write_opt_str(m, "barcode", e.barcode.as_deref())?;
        write_opt_str(m, "default_unit", e.default_unit.as_deref())?;
        write_opt_i64(m, "default_qty_thousandths", e.default_qty_thousandths)?;
        write_opt_str(m, "notes", e.notes.as_deref())?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<FoodProduct, RepoError> {
        Ok(FoodProduct {
            id: read_uuid(m, "id")?,
            name: read_str(m, "name")?,
            brand: read_opt_str(m, "brand")?,
            category: read_opt_str(m, "category")?,
            barcode: read_opt_str(m, "barcode")?,
            default_unit: read_opt_str(m, "default_unit")?,
            default_qty_thousandths: read_opt_i64(m, "default_qty_thousandths")?,
            notes: read_opt_str(m, "notes")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: FoodProductUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.brand {
            write_opt_str(m, "brand", v.as_deref())?;
        }
        if let Some(v) = u.category {
            write_opt_str(m, "category", v.as_deref())?;
        }
        if let Some(v) = u.barcode {
            write_opt_str(m, "barcode", v.as_deref())?;
        }
        if let Some(v) = u.default_unit {
            write_opt_str(m, "default_unit", v.as_deref())?;
        }
        if let Some(v) = u.default_qty_thousandths {
            write_opt_i64(m, "default_qty_thousandths", v)?;
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
        items: &mut [FoodProduct],
        field: &str,
        order: SortOrder,
    ) -> Result<(), RepoError> {
        match field {
            "name" => items.sort_by(|a, b| a.name.cmp(&b.name)),
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

    fn build_list(items: Vec<FoodProduct>, total: u32, page: Page) -> FoodProductList {
        FoodProductList { items, total, page }
    }
}

impl FoodProductRepo for FoodProductRepoLoro {
    async fn get(&self, id: Uuid) -> Result<FoodProduct, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<FoodProductList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: FoodProductCreate) -> Result<FoodProduct, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: FoodProductUpdate) -> Result<FoodProduct, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── PantryItem ────────────────────────────────────────────────────────

pub struct PantryItemEntity;

#[derive(Clone)]
pub struct PantryItemRepoLoro {
    inner: LoroRepo<PantryItemEntity>,
}

impl PantryItemRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<PantryItemEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for PantryItemEntity {
    type Wire = PantryItem;
    type Create = PantryItemCreate;
    type Update = PantryItemUpdate;
    type List = PantryItemList;

    const ROOT: &'static str = "pantry_items";

    fn id(w: &PantryItem) -> Uuid {
        w.id
    }

    fn from_create(input: PantryItemCreate) -> PantryItem {
        let now = Utc::now();
        PantryItem {
            id: Uuid::new_v4(),
            product_id: input.product_id,
            name: input.name,
            qty_thousandths: input.qty_thousandths,
            unit: input.unit,
            location: input.location,
            expires_at: input.expires_at,
            opened_at: input.opened_at,
            notes: input.notes,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &PantryItem) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_opt_uuid(m, "product_id", e.product_id)?;
        write_str(m, "name", &e.name)?;
        write_i64(m, "qty_thousandths", e.qty_thousandths)?;
        write_str(m, "unit", &e.unit)?;
        write_opt_str(m, "location", e.location.as_deref())?;
        write_opt_dt(m, "expires_at", e.expires_at)?;
        write_opt_dt(m, "opened_at", e.opened_at)?;
        write_opt_str(m, "notes", e.notes.as_deref())?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<PantryItem, RepoError> {
        Ok(PantryItem {
            id: read_uuid(m, "id")?,
            product_id: read_opt_uuid(m, "product_id")?,
            name: read_str(m, "name")?,
            qty_thousandths: read_i64(m, "qty_thousandths")?,
            unit: read_str(m, "unit")?,
            location: read_opt_str(m, "location")?,
            expires_at: read_opt_dt(m, "expires_at")?,
            opened_at: read_opt_dt(m, "opened_at")?,
            notes: read_opt_str(m, "notes")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: PantryItemUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.product_id {
            write_opt_uuid(m, "product_id", v)?;
        }
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.qty_thousandths {
            write_i64(m, "qty_thousandths", v)?;
        }
        if let Some(v) = u.unit {
            write_str(m, "unit", &v)?;
        }
        if let Some(v) = u.location {
            write_opt_str(m, "location", v.as_deref())?;
        }
        if let Some(v) = u.expires_at {
            write_opt_dt(m, "expires_at", v)?;
        }
        if let Some(v) = u.opened_at {
            write_opt_dt(m, "opened_at", v)?;
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
        items: &mut [PantryItem],
        field: &str,
        order: SortOrder,
    ) -> Result<(), RepoError> {
        match field {
            "qty_thousandths" => items.sort_by(|a, b| a.qty_thousandths.cmp(&b.qty_thousandths)),
            "expires_at" => items.sort_by(|a, b| a.expires_at.cmp(&b.expires_at)),
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

    fn build_list(items: Vec<PantryItem>, total: u32, page: Page) -> PantryItemList {
        PantryItemList { items, total, page }
    }
}

impl PantryItemRepo for PantryItemRepoLoro {
    async fn get(&self, id: Uuid) -> Result<PantryItem, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<PantryItemList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: PantryItemCreate) -> Result<PantryItem, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: PantryItemUpdate) -> Result<PantryItem, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── ShoppingListItem ──────────────────────────────────────────────────

pub struct ShoppingListItemEntity;

#[derive(Clone)]
pub struct ShoppingListItemRepoLoro {
    inner: LoroRepo<ShoppingListItemEntity>,
}

impl ShoppingListItemRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<ShoppingListItemEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for ShoppingListItemEntity {
    type Wire = ShoppingListItem;
    type Create = ShoppingListItemCreate;
    type Update = ShoppingListItemUpdate;
    type List = ShoppingListItemList;

    const ROOT: &'static str = "shopping_list_items";

    fn id(w: &ShoppingListItem) -> Uuid {
        w.id
    }

    fn from_create(input: ShoppingListItemCreate) -> ShoppingListItem {
        let now = Utc::now();
        ShoppingListItem {
            id: Uuid::new_v4(),
            product_id: input.product_id,
            name: input.name,
            qty_thousandths: input.qty_thousandths,
            unit: input.unit,
            purchased: input.purchased,
            purchased_at: input.purchased_at,
            sort_index: input.sort_index,
            notes: input.notes,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &ShoppingListItem) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_opt_uuid(m, "product_id", e.product_id)?;
        write_str(m, "name", &e.name)?;
        write_i64(m, "qty_thousandths", e.qty_thousandths)?;
        write_str(m, "unit", &e.unit)?;
        write_bool(m, "purchased", e.purchased)?;
        write_opt_dt(m, "purchased_at", e.purchased_at)?;
        write_i64(m, "sort_index", e.sort_index)?;
        write_opt_str(m, "notes", e.notes.as_deref())?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<ShoppingListItem, RepoError> {
        Ok(ShoppingListItem {
            id: read_uuid(m, "id")?,
            product_id: read_opt_uuid(m, "product_id")?,
            name: read_str(m, "name")?,
            qty_thousandths: read_i64(m, "qty_thousandths")?,
            unit: read_str(m, "unit")?,
            purchased: read_bool(m, "purchased")?,
            purchased_at: read_opt_dt(m, "purchased_at")?,
            sort_index: read_i64(m, "sort_index")?,
            notes: read_opt_str(m, "notes")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: ShoppingListItemUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.product_id {
            write_opt_uuid(m, "product_id", v)?;
        }
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.qty_thousandths {
            write_i64(m, "qty_thousandths", v)?;
        }
        if let Some(v) = u.unit {
            write_str(m, "unit", &v)?;
        }
        if let Some(v) = u.purchased {
            write_bool(m, "purchased", v)?;
        }
        if let Some(v) = u.purchased_at {
            write_opt_dt(m, "purchased_at", v)?;
        }
        if let Some(v) = u.sort_index {
            write_i64(m, "sort_index", v)?;
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
        items: &mut [ShoppingListItem],
        field: &str,
        order: SortOrder,
    ) -> Result<(), RepoError> {
        match field {
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

    fn build_list(items: Vec<ShoppingListItem>, total: u32, page: Page) -> ShoppingListItemList {
        ShoppingListItemList { items, total, page }
    }
}

impl ShoppingListItemRepo for ShoppingListItemRepoLoro {
    async fn get(&self, id: Uuid) -> Result<ShoppingListItem, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<ShoppingListItemList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: ShoppingListItemCreate) -> Result<ShoppingListItem, RepoError> {
        self.inner.create(input).await
    }
    async fn update(
        &self,
        id: Uuid,
        input: ShoppingListItemUpdate,
    ) -> Result<ShoppingListItem, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}
