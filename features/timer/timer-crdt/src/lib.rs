//! Loro-backed `TimeEntryRepo`. Source of truth lives in a LoroDoc;
//! persistence is the concern of `timer-db`.
//!
//! Container layout: top-level `"time_entries"` LoroMap, keyed by
//! uuid; each entry is a sub-LoroMap with one key per field. All
//! field codecs come from `crdt::codec`.

use architect::{Page, RepoError, SortOrder};
use chrono::Utc;
use crdt::EntityCrdt;
use crdt::codec::{
    read_bool, read_dt, read_opt_dt, read_opt_str, read_opt_u32, read_opt_uuid, read_string_list,
    read_uuid, write_bool, write_dt, write_opt_dt, write_opt_str, write_opt_string_list,
    write_opt_u32, write_opt_uuid, write_string_list, write_uuid,
};
use loro::LoroMap;
use timer_proto::{TimeEntry, TimeEntryCreate, TimeEntryList, TimeEntryRepo, TimeEntryUpdate};
use uuid::Uuid;

pub use crdt::{CrdtDoc, LoroRepo};

pub struct TimeEntryEntity;

#[derive(Clone)]
pub struct TimeEntryRepoLoro {
    inner: LoroRepo<TimeEntryEntity>,
}

impl TimeEntryRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }

    pub fn inner(&self) -> &LoroRepo<TimeEntryEntity> {
        &self.inner
    }

    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for TimeEntryEntity {
    type Wire = TimeEntry;
    type Create = TimeEntryCreate;
    type Update = TimeEntryUpdate;
    type List = TimeEntryList;

    const ROOT: &'static str = "time_entries";

    fn id(wire: &TimeEntry) -> Uuid {
        wire.id
    }

    fn from_create(input: TimeEntryCreate) -> TimeEntry {
        let now = Utc::now();
        TimeEntry {
            id: Uuid::new_v4(),
            task_id: input.task_id,
            user: input.user,
            start_time: input.start_time,
            end_time: input.end_time,
            description: input.description,
            billable: input.billable,
            billable_rate_cents: input.billable_rate_cents,
            tags: input.tags,
            invoiced_at: input.invoiced_at,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &TimeEntry) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_opt_uuid(m, "task_id", e.task_id)?;
        write_opt_str(m, "user", e.user.as_deref())?;
        write_dt(m, "start_time", e.start_time)?;
        write_opt_dt(m, "end_time", e.end_time)?;
        write_opt_str(m, "description", e.description.as_deref())?;
        write_bool(m, "billable", e.billable)?;
        write_opt_u32(m, "billable_rate_cents", e.billable_rate_cents)?;
        write_string_list(m, "tags", &e.tags)?;
        write_opt_dt(m, "invoiced_at", e.invoiced_at)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<TimeEntry, RepoError> {
        Ok(TimeEntry {
            id: read_uuid(m, "id")?,
            task_id: read_opt_uuid(m, "task_id")?,
            user: read_opt_str(m, "user")?,
            start_time: read_dt(m, "start_time")?,
            end_time: read_opt_dt(m, "end_time")?,
            description: read_opt_str(m, "description")?,
            billable: read_bool(m, "billable")?,
            billable_rate_cents: read_opt_u32(m, "billable_rate_cents")?,
            tags: read_string_list(m, "tags")?,
            invoiced_at: read_opt_dt(m, "invoiced_at")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: TimeEntryUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.task_id {
            write_opt_uuid(m, "task_id", v)?;
        }
        if let Some(v) = u.user {
            write_opt_str(m, "user", v.as_deref())?;
        }
        if let Some(v) = u.start_time {
            write_dt(m, "start_time", v)?;
        }
        if let Some(v) = u.end_time {
            write_opt_dt(m, "end_time", v)?;
        }
        if let Some(v) = u.description {
            write_opt_str(m, "description", v.as_deref())?;
        }
        if let Some(v) = u.billable {
            write_bool(m, "billable", v)?;
        }
        if let Some(v) = u.billable_rate_cents {
            write_opt_u32(m, "billable_rate_cents", v)?;
        }
        if let Some(v) = u.tags {
            write_opt_string_list(m, "tags", Some(&v))?;
        }
        if let Some(v) = u.invoiced_at {
            write_opt_dt(m, "invoiced_at", v)?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [TimeEntry], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "start_time" => items.sort_by(|a, b| a.start_time.cmp(&b.start_time)),
            "end_time" => items.sort_by(|a, b| a.end_time.cmp(&b.end_time)),
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

    fn build_list(items: Vec<TimeEntry>, total: u32, page: Page) -> TimeEntryList {
        TimeEntryList { items, total, page }
    }
}

impl TimeEntryRepo for TimeEntryRepoLoro {
    async fn get(&self, id: Uuid) -> Result<TimeEntry, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<TimeEntryList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: TimeEntryCreate) -> Result<TimeEntry, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: TimeEntryUpdate) -> Result<TimeEntry, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}
