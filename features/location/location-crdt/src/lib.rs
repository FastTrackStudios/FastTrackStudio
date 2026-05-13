//! Loro-backed `LocationRepo`.

use architect::{Page, RepoError, SortOrder};
use chrono::Utc;
use crdt::EntityCrdt;
use crdt::codec::{
    read_dt, read_opt_str, read_opt_uuid, read_str, read_string_list, read_uuid, write_dt,
    write_opt_str, write_opt_string_list, write_opt_uuid, write_str, write_string_list, write_uuid,
};
use location_proto::{Location, LocationCreate, LocationList, LocationRepo, LocationUpdate};
use loro::LoroMap;
use uuid::Uuid;

pub use crdt::{CrdtDoc, LoroRepo};

pub struct LocationEntity;

#[derive(Clone)]
pub struct LocationRepoLoro {
    inner: LoroRepo<LocationEntity>,
}

impl LocationRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<LocationEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for LocationEntity {
    type Wire = Location;
    type Create = LocationCreate;
    type Update = LocationUpdate;
    type List = LocationList;

    const ROOT: &'static str = "locations";

    fn id(w: &Location) -> Uuid {
        w.id
    }

    fn from_create(input: LocationCreate) -> Location {
        let now = Utc::now();
        Location {
            id: Uuid::new_v4(),
            name: input.name,
            kind: input.kind,
            address1: input.address1,
            address2: input.address2,
            city: input.city,
            state: input.state,
            postal_code: input.postal_code,
            country_code: input.country_code,
            contact_name: input.contact_name,
            contact_email: input.contact_email,
            parent_id: input.parent_id,
            notes: input.notes,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Location) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "name", &e.name)?;
        write_opt_str(m, "kind", e.kind.as_deref())?;
        write_opt_str(m, "address1", e.address1.as_deref())?;
        write_opt_str(m, "address2", e.address2.as_deref())?;
        write_opt_str(m, "city", e.city.as_deref())?;
        write_opt_str(m, "state", e.state.as_deref())?;
        write_opt_str(m, "postal_code", e.postal_code.as_deref())?;
        write_opt_str(m, "country_code", e.country_code.as_deref())?;
        write_opt_str(m, "contact_name", e.contact_name.as_deref())?;
        write_opt_str(m, "contact_email", e.contact_email.as_deref())?;
        write_opt_uuid(m, "parent_id", e.parent_id)?;
        write_opt_str(m, "notes", e.notes.as_deref())?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Location, RepoError> {
        Ok(Location {
            id: read_uuid(m, "id")?,
            name: read_str(m, "name")?,
            kind: read_opt_str(m, "kind")?,
            address1: read_opt_str(m, "address1")?,
            address2: read_opt_str(m, "address2")?,
            city: read_opt_str(m, "city")?,
            state: read_opt_str(m, "state")?,
            postal_code: read_opt_str(m, "postal_code")?,
            country_code: read_opt_str(m, "country_code")?,
            contact_name: read_opt_str(m, "contact_name")?,
            contact_email: read_opt_str(m, "contact_email")?,
            parent_id: read_opt_uuid(m, "parent_id")?,
            notes: read_opt_str(m, "notes")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: LocationUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.kind {
            write_opt_str(m, "kind", v.as_deref())?;
        }
        if let Some(v) = u.address1 {
            write_opt_str(m, "address1", v.as_deref())?;
        }
        if let Some(v) = u.address2 {
            write_opt_str(m, "address2", v.as_deref())?;
        }
        if let Some(v) = u.city {
            write_opt_str(m, "city", v.as_deref())?;
        }
        if let Some(v) = u.state {
            write_opt_str(m, "state", v.as_deref())?;
        }
        if let Some(v) = u.postal_code {
            write_opt_str(m, "postal_code", v.as_deref())?;
        }
        if let Some(v) = u.country_code {
            write_opt_str(m, "country_code", v.as_deref())?;
        }
        if let Some(v) = u.contact_name {
            write_opt_str(m, "contact_name", v.as_deref())?;
        }
        if let Some(v) = u.contact_email {
            write_opt_str(m, "contact_email", v.as_deref())?;
        }
        if let Some(v) = u.parent_id {
            write_opt_uuid(m, "parent_id", v)?;
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

    fn sort_items(items: &mut [Location], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "name" => items.sort_by(|a, b| a.name.cmp(&b.name)),
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

    fn build_list(items: Vec<Location>, total: u32, page: Page) -> LocationList {
        LocationList { items, total, page }
    }
}

impl LocationRepo for LocationRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Location, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<LocationList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: LocationCreate) -> Result<Location, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: LocationUpdate) -> Result<Location, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}
