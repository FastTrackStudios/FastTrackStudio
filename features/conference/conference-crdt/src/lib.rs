use architect::{Page, RepoError, SortOrder};
use chrono::Utc;
use conference_proto::{Meeting, MeetingCreate, MeetingList, MeetingRepo, MeetingUpdate};
use crdt::EntityCrdt;
use crdt::codec::{
    read_dt, read_opt_dt, read_opt_str, read_str, read_string_list, read_uuid, write_dt,
    write_opt_dt, write_opt_str, write_opt_string_list, write_str, write_string_list, write_uuid,
};
use loro::LoroMap;
use uuid::Uuid;

pub use crdt::{CrdtDoc, LoroRepo};

pub struct MeetingEntity;

#[derive(Clone)]
pub struct MeetingRepoLoro {
    inner: LoroRepo<MeetingEntity>,
}

impl MeetingRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<MeetingEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for MeetingEntity {
    type Wire = Meeting;
    type Create = MeetingCreate;
    type Update = MeetingUpdate;
    type List = MeetingList;

    const ROOT: &'static str = "meetings";

    fn id(w: &Meeting) -> Uuid {
        w.id
    }

    fn from_create(input: MeetingCreate) -> Meeting {
        let now = Utc::now();
        Meeting {
            id: Uuid::new_v4(),
            name: input.name,
            host_user: input.host_user,
            scheduled_at: input.scheduled_at,
            started_at: input.started_at,
            ended_at: input.ended_at,
            status: input.status,
            recording_url: input.recording_url,
            notes: input.notes,
            participants: input.participants,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Meeting) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "name", &e.name)?;
        write_opt_str(m, "host_user", e.host_user.as_deref())?;
        write_dt(m, "scheduled_at", e.scheduled_at)?;
        write_opt_dt(m, "started_at", e.started_at)?;
        write_opt_dt(m, "ended_at", e.ended_at)?;
        write_str(m, "status", &e.status)?;
        write_opt_str(m, "recording_url", e.recording_url.as_deref())?;
        write_opt_str(m, "notes", e.notes.as_deref())?;
        write_string_list(m, "participants", &e.participants)?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Meeting, RepoError> {
        Ok(Meeting {
            id: read_uuid(m, "id")?,
            name: read_str(m, "name")?,
            host_user: read_opt_str(m, "host_user")?,
            scheduled_at: read_dt(m, "scheduled_at")?,
            started_at: read_opt_dt(m, "started_at")?,
            ended_at: read_opt_dt(m, "ended_at")?,
            status: read_str(m, "status")?,
            recording_url: read_opt_str(m, "recording_url")?,
            notes: read_opt_str(m, "notes")?,
            participants: read_string_list(m, "participants")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: MeetingUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.host_user {
            write_opt_str(m, "host_user", v.as_deref())?;
        }
        if let Some(v) = u.scheduled_at {
            write_dt(m, "scheduled_at", v)?;
        }
        if let Some(v) = u.started_at {
            write_opt_dt(m, "started_at", v)?;
        }
        if let Some(v) = u.ended_at {
            write_opt_dt(m, "ended_at", v)?;
        }
        if let Some(v) = u.status {
            write_str(m, "status", &v)?;
        }
        if let Some(v) = u.recording_url {
            write_opt_str(m, "recording_url", v.as_deref())?;
        }
        if let Some(v) = u.notes {
            write_opt_str(m, "notes", v.as_deref())?;
        }
        if let Some(v) = u.participants {
            write_opt_string_list(m, "participants", Some(&v))?;
        }
        if let Some(v) = u.tags {
            write_opt_string_list(m, "tags", Some(&v))?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [Meeting], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "name" => items.sort_by(|a, b| a.name.cmp(&b.name)),
            "scheduled_at" => items.sort_by(|a, b| a.scheduled_at.cmp(&b.scheduled_at)),
            "started_at" => items.sort_by(|a, b| a.started_at.cmp(&b.started_at)),
            "ended_at" => items.sort_by(|a, b| a.ended_at.cmp(&b.ended_at)),
            "status" => items.sort_by(|a, b| a.status.cmp(&b.status)),
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

    fn build_list(items: Vec<Meeting>, total: u32, page: Page) -> MeetingList {
        MeetingList { items, total, page }
    }
}

impl MeetingRepo for MeetingRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Meeting, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<MeetingList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: MeetingCreate) -> Result<Meeting, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: MeetingUpdate) -> Result<Meeting, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}
