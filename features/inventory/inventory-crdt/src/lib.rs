//! Loro-backed source-of-truth for the inventory feature. Two
//! entities: `InventoryItem` + `CheckoutEvent`.

use architect::{Page, RepoError, SortOrder};
use chrono::Utc;
use crdt::EntityCrdt;
use crdt::codec::{
    read_dt, read_opt_dt, read_opt_i64, read_opt_str, read_opt_uuid, read_str, read_string_list,
    read_uuid, write_dt, write_opt_dt, write_opt_i64, write_opt_str, write_opt_string_list,
    write_opt_uuid, write_str, write_string_list, write_uuid,
};
use inventory_proto::{
    CheckoutEvent, CheckoutEventCreate, CheckoutEventList, CheckoutEventRepo, CheckoutEventUpdate,
    InventoryItem, InventoryItemCreate, InventoryItemList, InventoryItemRepo, InventoryItemUpdate,
};
use loro::LoroMap;
use uuid::Uuid;

pub use crdt::{CrdtDoc, LoroRepo};

// ── InventoryItem ────────────────────────────────────────────────────

pub struct InventoryItemEntity;

#[derive(Clone)]
pub struct InventoryItemRepoLoro {
    inner: LoroRepo<InventoryItemEntity>,
}

impl InventoryItemRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<InventoryItemEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for InventoryItemEntity {
    type Wire = InventoryItem;
    type Create = InventoryItemCreate;
    type Update = InventoryItemUpdate;
    type List = InventoryItemList;

    const ROOT: &'static str = "inventory_items";

    fn id(w: &InventoryItem) -> Uuid {
        w.id
    }

    fn from_create(input: InventoryItemCreate) -> InventoryItem {
        let now = Utc::now();
        InventoryItem {
            id: Uuid::new_v4(),
            name: input.name,
            category: input.category,
            status: input.status,
            manufacturer: input.manufacturer,
            model: input.model,
            serial_number: input.serial_number,
            qr_code: input.qr_code,
            owner_id: input.owner_id,
            location_id: input.location_id,
            sub_location: input.sub_location,
            value_cents: input.value_cents,
            warranty_until: input.warranty_until,
            vendor: input.vendor,
            purchase_order: input.purchase_order,
            acquired_at: input.acquired_at,
            photo_url: input.photo_url,
            notes: input.notes,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &InventoryItem) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "name", &e.name)?;
        write_opt_str(m, "category", e.category.as_deref())?;
        write_str(m, "status", &e.status)?;
        write_opt_str(m, "manufacturer", e.manufacturer.as_deref())?;
        write_opt_str(m, "model", e.model.as_deref())?;
        write_opt_str(m, "serial_number", e.serial_number.as_deref())?;
        write_opt_str(m, "qr_code", e.qr_code.as_deref())?;
        write_opt_uuid(m, "owner_id", e.owner_id)?;
        write_opt_uuid(m, "location_id", e.location_id)?;
        write_opt_str(m, "sub_location", e.sub_location.as_deref())?;
        write_opt_i64(m, "value_cents", e.value_cents)?;
        write_opt_dt(m, "warranty_until", e.warranty_until)?;
        write_opt_str(m, "vendor", e.vendor.as_deref())?;
        write_opt_str(m, "purchase_order", e.purchase_order.as_deref())?;
        write_opt_dt(m, "acquired_at", e.acquired_at)?;
        write_opt_str(m, "photo_url", e.photo_url.as_deref())?;
        write_opt_str(m, "notes", e.notes.as_deref())?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<InventoryItem, RepoError> {
        Ok(InventoryItem {
            id: read_uuid(m, "id")?,
            name: read_str(m, "name")?,
            category: read_opt_str(m, "category")?,
            status: read_str(m, "status")?,
            manufacturer: read_opt_str(m, "manufacturer")?,
            model: read_opt_str(m, "model")?,
            serial_number: read_opt_str(m, "serial_number")?,
            qr_code: read_opt_str(m, "qr_code")?,
            owner_id: read_opt_uuid(m, "owner_id")?,
            location_id: read_opt_uuid(m, "location_id")?,
            sub_location: read_opt_str(m, "sub_location")?,
            value_cents: read_opt_i64(m, "value_cents")?,
            warranty_until: read_opt_dt(m, "warranty_until")?,
            vendor: read_opt_str(m, "vendor")?,
            purchase_order: read_opt_str(m, "purchase_order")?,
            acquired_at: read_opt_dt(m, "acquired_at")?,
            photo_url: read_opt_str(m, "photo_url")?,
            notes: read_opt_str(m, "notes")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: InventoryItemUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.category {
            write_opt_str(m, "category", v.as_deref())?;
        }
        if let Some(v) = u.status {
            write_str(m, "status", &v)?;
        }
        if let Some(v) = u.manufacturer {
            write_opt_str(m, "manufacturer", v.as_deref())?;
        }
        if let Some(v) = u.model {
            write_opt_str(m, "model", v.as_deref())?;
        }
        if let Some(v) = u.serial_number {
            write_opt_str(m, "serial_number", v.as_deref())?;
        }
        if let Some(v) = u.qr_code {
            write_opt_str(m, "qr_code", v.as_deref())?;
        }
        if let Some(v) = u.owner_id {
            write_opt_uuid(m, "owner_id", v)?;
        }
        if let Some(v) = u.location_id {
            write_opt_uuid(m, "location_id", v)?;
        }
        if let Some(v) = u.sub_location {
            write_opt_str(m, "sub_location", v.as_deref())?;
        }
        if let Some(v) = u.value_cents {
            write_opt_i64(m, "value_cents", v)?;
        }
        if let Some(v) = u.warranty_until {
            write_opt_dt(m, "warranty_until", v)?;
        }
        if let Some(v) = u.vendor {
            write_opt_str(m, "vendor", v.as_deref())?;
        }
        if let Some(v) = u.purchase_order {
            write_opt_str(m, "purchase_order", v.as_deref())?;
        }
        if let Some(v) = u.acquired_at {
            write_opt_dt(m, "acquired_at", v)?;
        }
        if let Some(v) = u.photo_url {
            write_opt_str(m, "photo_url", v.as_deref())?;
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
        items: &mut [InventoryItem],
        field: &str,
        order: SortOrder,
    ) -> Result<(), RepoError> {
        match field {
            "name" => items.sort_by(|a, b| a.name.cmp(&b.name)),
            "status" => items.sort_by(|a, b| a.status.cmp(&b.status)),
            "value_cents" => items.sort_by(|a, b| a.value_cents.cmp(&b.value_cents)),
            "warranty_until" => items.sort_by(|a, b| a.warranty_until.cmp(&b.warranty_until)),
            "acquired_at" => items.sort_by(|a, b| a.acquired_at.cmp(&b.acquired_at)),
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

    fn build_list(items: Vec<InventoryItem>, total: u32, page: Page) -> InventoryItemList {
        InventoryItemList { items, total, page }
    }
}

impl InventoryItemRepo for InventoryItemRepoLoro {
    async fn get(&self, id: Uuid) -> Result<InventoryItem, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<InventoryItemList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: InventoryItemCreate) -> Result<InventoryItem, RepoError> {
        self.inner.create(input).await
    }
    async fn update(
        &self,
        id: Uuid,
        input: InventoryItemUpdate,
    ) -> Result<InventoryItem, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── CheckoutEvent ────────────────────────────────────────────────────

pub struct CheckoutEventEntity;

#[derive(Clone)]
pub struct CheckoutEventRepoLoro {
    inner: LoroRepo<CheckoutEventEntity>,
}

impl CheckoutEventRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<CheckoutEventEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for CheckoutEventEntity {
    type Wire = CheckoutEvent;
    type Create = CheckoutEventCreate;
    type Update = CheckoutEventUpdate;
    type List = CheckoutEventList;

    const ROOT: &'static str = "checkout_events";

    fn id(w: &CheckoutEvent) -> Uuid {
        w.id
    }

    fn from_create(input: CheckoutEventCreate) -> CheckoutEvent {
        let now = Utc::now();
        CheckoutEvent {
            id: Uuid::new_v4(),
            item_id: input.item_id,
            person_id: input.person_id,
            checked_out_at: input.checked_out_at,
            due_at: input.due_at,
            returned_at: input.returned_at,
            note: input.note,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &CheckoutEvent) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "item_id", e.item_id)?;
        write_opt_uuid(m, "person_id", e.person_id)?;
        write_dt(m, "checked_out_at", e.checked_out_at)?;
        write_opt_dt(m, "due_at", e.due_at)?;
        write_opt_dt(m, "returned_at", e.returned_at)?;
        write_opt_str(m, "note", e.note.as_deref())?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<CheckoutEvent, RepoError> {
        Ok(CheckoutEvent {
            id: read_uuid(m, "id")?,
            item_id: read_uuid(m, "item_id")?,
            person_id: read_opt_uuid(m, "person_id")?,
            checked_out_at: read_dt(m, "checked_out_at")?,
            due_at: read_opt_dt(m, "due_at")?,
            returned_at: read_opt_dt(m, "returned_at")?,
            note: read_opt_str(m, "note")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: CheckoutEventUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.item_id {
            write_uuid(m, "item_id", v)?;
        }
        if let Some(v) = u.person_id {
            write_opt_uuid(m, "person_id", v)?;
        }
        if let Some(v) = u.checked_out_at {
            write_dt(m, "checked_out_at", v)?;
        }
        if let Some(v) = u.due_at {
            write_opt_dt(m, "due_at", v)?;
        }
        if let Some(v) = u.returned_at {
            write_opt_dt(m, "returned_at", v)?;
        }
        if let Some(v) = u.note {
            write_opt_str(m, "note", v.as_deref())?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(
        items: &mut [CheckoutEvent],
        field: &str,
        order: SortOrder,
    ) -> Result<(), RepoError> {
        match field {
            "checked_out_at" => items.sort_by(|a, b| a.checked_out_at.cmp(&b.checked_out_at)),
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

    fn build_list(items: Vec<CheckoutEvent>, total: u32, page: Page) -> CheckoutEventList {
        CheckoutEventList { items, total, page }
    }
}

impl CheckoutEventRepo for CheckoutEventRepoLoro {
    async fn get(&self, id: Uuid) -> Result<CheckoutEvent, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<CheckoutEventList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: CheckoutEventCreate) -> Result<CheckoutEvent, RepoError> {
        self.inner.create(input).await
    }
    async fn update(
        &self,
        id: Uuid,
        input: CheckoutEventUpdate,
    ) -> Result<CheckoutEvent, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}
