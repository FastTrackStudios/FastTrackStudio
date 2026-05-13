//! Loro-backed `AssetRepo`. Source of truth in a LoroDoc; persistence
//! handled by `asset-db`.

use architect::{Page, RepoError, SortOrder};
use asset_proto::{Asset, AssetCreate, AssetList, AssetRepo, AssetUpdate};
use chrono::Utc;
use crdt::EntityCrdt;
use crdt::codec::{
    read_dt, read_opt_dt, read_opt_str, read_opt_uuid, read_str, read_string_list, read_uuid,
    write_dt, write_opt_dt, write_opt_str, write_opt_string_list, write_opt_uuid, write_str,
    write_string_list, write_uuid,
};
use loro::LoroMap;
use uuid::Uuid;

pub use crdt::{CrdtDoc, LoroRepo};

pub struct AssetEntity;

#[derive(Clone)]
pub struct AssetRepoLoro {
    inner: LoroRepo<AssetEntity>,
}

impl AssetRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<AssetEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for AssetEntity {
    type Wire = Asset;
    type Create = AssetCreate;
    type Update = AssetUpdate;
    type List = AssetList;

    const ROOT: &'static str = "assets";

    fn id(w: &Asset) -> Uuid {
        w.id
    }

    fn from_create(input: AssetCreate) -> Asset {
        let now = Utc::now();
        Asset {
            id: Uuid::new_v4(),
            name: input.name,
            status: input.status,
            manufacturer: input.manufacturer,
            model: input.model,
            serial_number: input.serial_number,
            owner_id: input.owner_id,
            location_id: input.location_id,
            notes: input.notes,
            tags: input.tags,
            acquired_at: input.acquired_at,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Asset) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "name", &e.name)?;
        write_str(m, "status", &e.status)?;
        write_opt_str(m, "manufacturer", e.manufacturer.as_deref())?;
        write_opt_str(m, "model", e.model.as_deref())?;
        write_opt_str(m, "serial_number", e.serial_number.as_deref())?;
        write_opt_uuid(m, "owner_id", e.owner_id)?;
        write_opt_uuid(m, "location_id", e.location_id)?;
        write_opt_str(m, "notes", e.notes.as_deref())?;
        write_string_list(m, "tags", &e.tags)?;
        write_opt_dt(m, "acquired_at", e.acquired_at)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Asset, RepoError> {
        Ok(Asset {
            id: read_uuid(m, "id")?,
            name: read_str(m, "name")?,
            status: read_str(m, "status")?,
            manufacturer: read_opt_str(m, "manufacturer")?,
            model: read_opt_str(m, "model")?,
            serial_number: read_opt_str(m, "serial_number")?,
            owner_id: read_opt_uuid(m, "owner_id")?,
            location_id: read_opt_uuid(m, "location_id")?,
            notes: read_opt_str(m, "notes")?,
            tags: read_string_list(m, "tags")?,
            acquired_at: read_opt_dt(m, "acquired_at")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: AssetUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
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
        if let Some(v) = u.owner_id {
            write_opt_uuid(m, "owner_id", v)?;
        }
        if let Some(v) = u.location_id {
            write_opt_uuid(m, "location_id", v)?;
        }
        if let Some(v) = u.notes {
            write_opt_str(m, "notes", v.as_deref())?;
        }
        if let Some(v) = u.tags {
            write_opt_string_list(m, "tags", Some(&v))?;
        }
        if let Some(v) = u.acquired_at {
            write_opt_dt(m, "acquired_at", v)?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [Asset], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "name" => items.sort_by(|a, b| a.name.cmp(&b.name)),
            "status" => items.sort_by(|a, b| a.status.cmp(&b.status)),
            "acquired_at" => items.sort_by(|a, b| a.acquired_at.cmp(&b.acquired_at)),
            "created_at" => items.sort_by(|a, b| a.created_at.cmp(&b.created_at)),
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

    fn build_list(items: Vec<Asset>, total: u32, page: Page) -> AssetList {
        AssetList { items, total, page }
    }
}

impl AssetRepo for AssetRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Asset, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<AssetList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: AssetCreate) -> Result<Asset, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: AssetUpdate) -> Result<Asset, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}
