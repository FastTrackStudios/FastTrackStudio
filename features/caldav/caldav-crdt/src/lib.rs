//! Loro-backed source-of-truth for the CalDAV feature. Two
//! entities, two `EntityCrdt` impls, two `*RepoLoro` newtypes.
//! Credentials live elsewhere (`auth-db`); this crate only models
//! the wire metadata.

use architect::{Page, RepoError, SortOrder};
use caldav_proto::{
    CalDavAccount, CalDavAccountCreate, CalDavAccountList, CalDavAccountRepo, CalDavAccountUpdate,
    CalDavCalendar, CalDavCalendarCreate, CalDavCalendarList, CalDavCalendarRepo,
    CalDavCalendarUpdate,
};
use chrono::Utc;
use crdt::EntityCrdt;
use crdt::codec::{
    read_bool, read_dt, read_opt_dt, read_opt_str, read_str, read_string_list, read_u32, read_uuid,
    write_bool, write_dt, write_opt_dt, write_opt_str, write_str, write_string_list, write_u32,
    write_uuid,
};
use loro::LoroMap;
use uuid::Uuid;

pub use crdt::{CrdtDoc, LoroRepo};

// ── CalDavAccount ─────────────────────────────────────────────────────

pub struct CalDavAccountEntity;

#[derive(Clone)]
pub struct CalDavAccountRepoLoro {
    inner: LoroRepo<CalDavAccountEntity>,
}

impl CalDavAccountRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<CalDavAccountEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for CalDavAccountEntity {
    type Wire = CalDavAccount;
    type Create = CalDavAccountCreate;
    type Update = CalDavAccountUpdate;
    type List = CalDavAccountList;

    const ROOT: &'static str = "caldav_accounts";

    fn id(w: &CalDavAccount) -> Uuid {
        w.id
    }

    fn from_create(input: CalDavAccountCreate) -> CalDavAccount {
        let now = Utc::now();
        CalDavAccount {
            id: Uuid::new_v4(),
            display_name: input.display_name,
            base_url: input.base_url,
            username: input.username,
            kind: input.kind,
            last_sync_at: input.last_sync_at,
            last_error: input.last_error,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &CalDavAccount) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "display_name", &e.display_name)?;
        write_str(m, "base_url", &e.base_url)?;
        write_str(m, "username", &e.username)?;
        write_opt_str(m, "kind", e.kind.as_deref())?;
        write_opt_dt(m, "last_sync_at", e.last_sync_at)?;
        write_opt_str(m, "last_error", e.last_error.as_deref())?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<CalDavAccount, RepoError> {
        Ok(CalDavAccount {
            id: read_uuid(m, "id")?,
            display_name: read_str(m, "display_name")?,
            base_url: read_str(m, "base_url")?,
            username: read_str(m, "username")?,
            kind: read_opt_str(m, "kind")?,
            last_sync_at: read_opt_dt(m, "last_sync_at")?,
            last_error: read_opt_str(m, "last_error")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: CalDavAccountUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.display_name {
            write_str(m, "display_name", &v)?;
        }
        if let Some(v) = u.base_url {
            write_str(m, "base_url", &v)?;
        }
        if let Some(v) = u.username {
            write_str(m, "username", &v)?;
        }
        if let Some(v) = u.kind {
            write_opt_str(m, "kind", v.as_deref())?;
        }
        if let Some(v) = u.last_sync_at {
            write_opt_dt(m, "last_sync_at", v)?;
        }
        if let Some(v) = u.last_error {
            write_opt_str(m, "last_error", v.as_deref())?;
        }
        if let Some(v) = u.tags {
            write_string_list(m, "tags", &v)?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(
        items: &mut [CalDavAccount],
        field: &str,
        order: SortOrder,
    ) -> Result<(), RepoError> {
        match field {
            "display_name" => items.sort_by(|a, b| a.display_name.cmp(&b.display_name)),
            "last_sync_at" => items.sort_by(|a, b| a.last_sync_at.cmp(&b.last_sync_at)),
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

    fn build_list(items: Vec<CalDavAccount>, total: u32, page: Page) -> CalDavAccountList {
        CalDavAccountList { items, total, page }
    }
}

impl CalDavAccountRepo for CalDavAccountRepoLoro {
    async fn get(&self, id: Uuid) -> Result<CalDavAccount, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<CalDavAccountList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: CalDavAccountCreate) -> Result<CalDavAccount, RepoError> {
        self.inner.create(input).await
    }
    async fn update(
        &self,
        id: Uuid,
        input: CalDavAccountUpdate,
    ) -> Result<CalDavAccount, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── CalDavCalendar ────────────────────────────────────────────────────

pub struct CalDavCalendarEntity;

#[derive(Clone)]
pub struct CalDavCalendarRepoLoro {
    inner: LoroRepo<CalDavCalendarEntity>,
}

impl CalDavCalendarRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<CalDavCalendarEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for CalDavCalendarEntity {
    type Wire = CalDavCalendar;
    type Create = CalDavCalendarCreate;
    type Update = CalDavCalendarUpdate;
    type List = CalDavCalendarList;

    const ROOT: &'static str = "caldav_calendars";

    fn id(w: &CalDavCalendar) -> Uuid {
        w.id
    }

    fn from_create(input: CalDavCalendarCreate) -> CalDavCalendar {
        let now = Utc::now();
        CalDavCalendar {
            id: Uuid::new_v4(),
            account_id: input.account_id,
            name: input.name,
            url: input.url,
            color: input.color,
            enabled: input.enabled,
            server_ctag: input.server_ctag,
            last_sync_at: input.last_sync_at,
            event_count: input.event_count,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &CalDavCalendar) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "account_id", e.account_id)?;
        write_str(m, "name", &e.name)?;
        write_str(m, "url", &e.url)?;
        write_opt_str(m, "color", e.color.as_deref())?;
        write_bool(m, "enabled", e.enabled)?;
        write_opt_str(m, "server_ctag", e.server_ctag.as_deref())?;
        write_opt_dt(m, "last_sync_at", e.last_sync_at)?;
        write_u32(m, "event_count", e.event_count)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<CalDavCalendar, RepoError> {
        Ok(CalDavCalendar {
            id: read_uuid(m, "id")?,
            account_id: read_uuid(m, "account_id")?,
            name: read_str(m, "name")?,
            url: read_str(m, "url")?,
            color: read_opt_str(m, "color")?,
            enabled: read_bool(m, "enabled")?,
            server_ctag: read_opt_str(m, "server_ctag")?,
            last_sync_at: read_opt_dt(m, "last_sync_at")?,
            event_count: read_u32(m, "event_count")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: CalDavCalendarUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.account_id {
            write_uuid(m, "account_id", v)?;
        }
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.url {
            write_str(m, "url", &v)?;
        }
        if let Some(v) = u.color {
            write_opt_str(m, "color", v.as_deref())?;
        }
        if let Some(v) = u.enabled {
            write_bool(m, "enabled", v)?;
        }
        if let Some(v) = u.server_ctag {
            write_opt_str(m, "server_ctag", v.as_deref())?;
        }
        if let Some(v) = u.last_sync_at {
            write_opt_dt(m, "last_sync_at", v)?;
        }
        if let Some(v) = u.event_count {
            write_u32(m, "event_count", v)?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(
        items: &mut [CalDavCalendar],
        field: &str,
        order: SortOrder,
    ) -> Result<(), RepoError> {
        match field {
            "name" => items.sort_by(|a, b| a.name.cmp(&b.name)),
            "last_sync_at" => items.sort_by(|a, b| a.last_sync_at.cmp(&b.last_sync_at)),
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

    fn build_list(items: Vec<CalDavCalendar>, total: u32, page: Page) -> CalDavCalendarList {
        CalDavCalendarList { items, total, page }
    }
}

impl CalDavCalendarRepo for CalDavCalendarRepoLoro {
    async fn get(&self, id: Uuid) -> Result<CalDavCalendar, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<CalDavCalendarList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: CalDavCalendarCreate) -> Result<CalDavCalendar, RepoError> {
        self.inner.create(input).await
    }
    async fn update(
        &self,
        id: Uuid,
        input: CalDavCalendarUpdate,
    ) -> Result<CalDavCalendar, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}
