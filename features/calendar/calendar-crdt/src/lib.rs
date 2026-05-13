//! Loro-backed `CalendarEventRepo`. Source of truth in a LoroDoc;
//! persistence is the concern of `calendar-db`.

use architect::{Page, RepoError, SortOrder};
use calendar_proto::{
    CalendarEvent, CalendarEventCreate, CalendarEventList, CalendarEventRepo, CalendarEventUpdate,
};
use chrono::Utc;
use crdt::EntityCrdt;
use crdt::codec::{
    read_bool, read_dt, read_opt_str, read_opt_uuid, read_str, read_string_list, read_uuid,
    write_bool, write_dt, write_opt_str, write_opt_string_list, write_opt_uuid, write_str,
    write_string_list, write_uuid,
};
use loro::LoroMap;
use uuid::Uuid;

pub use crdt::{CrdtDoc, LoroRepo};

pub struct CalendarEventEntity;

#[derive(Clone)]
pub struct CalendarEventRepoLoro {
    inner: LoroRepo<CalendarEventEntity>,
}

impl CalendarEventRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<CalendarEventEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for CalendarEventEntity {
    type Wire = CalendarEvent;
    type Create = CalendarEventCreate;
    type Update = CalendarEventUpdate;
    type List = CalendarEventList;

    const ROOT: &'static str = "calendar_events";

    fn id(w: &CalendarEvent) -> Uuid {
        w.id
    }

    fn from_create(input: CalendarEventCreate) -> CalendarEvent {
        let now = Utc::now();
        CalendarEvent {
            id: Uuid::new_v4(),
            title: input.title,
            description: input.description,
            start_at: input.start_at,
            end_at: input.end_at,
            all_day: input.all_day,
            location_id: input.location_id,
            location_text: input.location_text,
            rrule: input.rrule,
            organizer: input.organizer,
            attendees: input.attendees,
            calendar_id: input.calendar_id,
            status: input.status,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &CalendarEvent) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "title", &e.title)?;
        write_opt_str(m, "description", e.description.as_deref())?;
        write_dt(m, "start_at", e.start_at)?;
        write_dt(m, "end_at", e.end_at)?;
        write_bool(m, "all_day", e.all_day)?;
        write_opt_uuid(m, "location_id", e.location_id)?;
        write_opt_str(m, "location_text", e.location_text.as_deref())?;
        write_opt_str(m, "rrule", e.rrule.as_deref())?;
        write_opt_str(m, "organizer", e.organizer.as_deref())?;
        write_string_list(m, "attendees", &e.attendees)?;
        write_opt_str(m, "calendar_id", e.calendar_id.as_deref())?;
        write_str(m, "status", &e.status)?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<CalendarEvent, RepoError> {
        Ok(CalendarEvent {
            id: read_uuid(m, "id")?,
            title: read_str(m, "title")?,
            description: read_opt_str(m, "description")?,
            start_at: read_dt(m, "start_at")?,
            end_at: read_dt(m, "end_at")?,
            all_day: read_bool(m, "all_day")?,
            location_id: read_opt_uuid(m, "location_id")?,
            location_text: read_opt_str(m, "location_text")?,
            rrule: read_opt_str(m, "rrule")?,
            organizer: read_opt_str(m, "organizer")?,
            attendees: read_string_list(m, "attendees")?,
            calendar_id: read_opt_str(m, "calendar_id")?,
            status: read_str(m, "status")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: CalendarEventUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.title {
            write_str(m, "title", &v)?;
        }
        if let Some(v) = u.description {
            write_opt_str(m, "description", v.as_deref())?;
        }
        if let Some(v) = u.start_at {
            write_dt(m, "start_at", v)?;
        }
        if let Some(v) = u.end_at {
            write_dt(m, "end_at", v)?;
        }
        if let Some(v) = u.all_day {
            write_bool(m, "all_day", v)?;
        }
        if let Some(v) = u.location_id {
            write_opt_uuid(m, "location_id", v)?;
        }
        if let Some(v) = u.location_text {
            write_opt_str(m, "location_text", v.as_deref())?;
        }
        if let Some(v) = u.rrule {
            write_opt_str(m, "rrule", v.as_deref())?;
        }
        if let Some(v) = u.organizer {
            write_opt_str(m, "organizer", v.as_deref())?;
        }
        if let Some(v) = u.attendees {
            write_opt_string_list(m, "attendees", Some(&v))?;
        }
        if let Some(v) = u.calendar_id {
            write_opt_str(m, "calendar_id", v.as_deref())?;
        }
        if let Some(v) = u.status {
            write_str(m, "status", &v)?;
        }
        if let Some(v) = u.tags {
            write_opt_string_list(m, "tags", Some(&v))?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(
        items: &mut [CalendarEvent],
        field: &str,
        order: SortOrder,
    ) -> Result<(), RepoError> {
        match field {
            "title" => items.sort_by(|a, b| a.title.cmp(&b.title)),
            "start_at" => items.sort_by(|a, b| a.start_at.cmp(&b.start_at)),
            "end_at" => items.sort_by(|a, b| a.end_at.cmp(&b.end_at)),
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

    fn build_list(items: Vec<CalendarEvent>, total: u32, page: Page) -> CalendarEventList {
        CalendarEventList { items, total, page }
    }
}

impl CalendarEventRepo for CalendarEventRepoLoro {
    async fn get(&self, id: Uuid) -> Result<CalendarEvent, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<CalendarEventList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: CalendarEventCreate) -> Result<CalendarEvent, RepoError> {
        self.inner.create(input).await
    }
    async fn update(
        &self,
        id: Uuid,
        input: CalendarEventUpdate,
    ) -> Result<CalendarEvent, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}
