use architect::{Page, RepoError, SortOrder};
use chrono::Utc;
use crdt::EntityCrdt;
use crdt::codec::{
    read_bool, read_dt, read_opt_str, read_str, read_string_list, read_uuid, write_bool, write_dt,
    write_opt_str, write_opt_string_list, write_str, write_string_list, write_uuid,
};
use email_proto::{Email, EmailCreate, EmailList, EmailRepo, EmailUpdate};
use loro::LoroMap;
use uuid::Uuid;

pub use crdt::{CrdtDoc, LoroRepo};

pub struct EmailEntity;

#[derive(Clone)]
pub struct EmailRepoLoro {
    inner: LoroRepo<EmailEntity>,
}

impl EmailRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<EmailEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for EmailEntity {
    type Wire = Email;
    type Create = EmailCreate;
    type Update = EmailUpdate;
    type List = EmailList;

    const ROOT: &'static str = "emails";

    fn id(w: &Email) -> Uuid {
        w.id
    }

    fn from_create(input: EmailCreate) -> Email {
        let now = Utc::now();
        Email {
            id: Uuid::new_v4(),
            message_id: input.message_id,
            subject: input.subject,
            from_addr: input.from_addr,
            to_addrs: input.to_addrs,
            cc_addrs: input.cc_addrs,
            bcc_addrs: input.bcc_addrs,
            body: input.body,
            received_at: input.received_at,
            read: input.read,
            starred: input.starred,
            folder: input.folder,
            thread_id: input.thread_id,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Email) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "message_id", &e.message_id)?;
        write_str(m, "subject", &e.subject)?;
        write_str(m, "from_addr", &e.from_addr)?;
        write_string_list(m, "to_addrs", &e.to_addrs)?;
        write_string_list(m, "cc_addrs", &e.cc_addrs)?;
        write_string_list(m, "bcc_addrs", &e.bcc_addrs)?;
        write_opt_str(m, "body", e.body.as_deref())?;
        write_dt(m, "received_at", e.received_at)?;
        write_bool(m, "read", e.read)?;
        write_bool(m, "starred", e.starred)?;
        write_opt_str(m, "folder", e.folder.as_deref())?;
        write_opt_str(m, "thread_id", e.thread_id.as_deref())?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Email, RepoError> {
        Ok(Email {
            id: read_uuid(m, "id")?,
            message_id: read_str(m, "message_id")?,
            subject: read_str(m, "subject")?,
            from_addr: read_str(m, "from_addr")?,
            to_addrs: read_string_list(m, "to_addrs")?,
            cc_addrs: read_string_list(m, "cc_addrs")?,
            bcc_addrs: read_string_list(m, "bcc_addrs")?,
            body: read_opt_str(m, "body")?,
            received_at: read_dt(m, "received_at")?,
            read: read_bool(m, "read")?,
            starred: read_bool(m, "starred")?,
            folder: read_opt_str(m, "folder")?,
            thread_id: read_opt_str(m, "thread_id")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: EmailUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.message_id {
            write_str(m, "message_id", &v)?;
        }
        if let Some(v) = u.subject {
            write_str(m, "subject", &v)?;
        }
        if let Some(v) = u.from_addr {
            write_str(m, "from_addr", &v)?;
        }
        if let Some(v) = u.to_addrs {
            write_string_list(m, "to_addrs", &v)?;
        }
        if let Some(v) = u.cc_addrs {
            write_string_list(m, "cc_addrs", &v)?;
        }
        if let Some(v) = u.bcc_addrs {
            write_string_list(m, "bcc_addrs", &v)?;
        }
        if let Some(v) = u.body {
            write_opt_str(m, "body", v.as_deref())?;
        }
        if let Some(v) = u.received_at {
            write_dt(m, "received_at", v)?;
        }
        if let Some(v) = u.read {
            write_bool(m, "read", v)?;
        }
        if let Some(v) = u.starred {
            write_bool(m, "starred", v)?;
        }
        if let Some(v) = u.folder {
            write_opt_str(m, "folder", v.as_deref())?;
        }
        if let Some(v) = u.thread_id {
            write_opt_str(m, "thread_id", v.as_deref())?;
        }
        if let Some(v) = u.tags {
            write_opt_string_list(m, "tags", Some(&v))?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [Email], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "message_id" => items.sort_by(|a, b| a.message_id.cmp(&b.message_id)),
            "subject" => items.sort_by(|a, b| a.subject.cmp(&b.subject)),
            "received_at" => items.sort_by(|a, b| a.received_at.cmp(&b.received_at)),
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

    fn build_list(items: Vec<Email>, total: u32, page: Page) -> EmailList {
        EmailList { items, total, page }
    }
}

impl EmailRepo for EmailRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Email, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<EmailList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: EmailCreate) -> Result<Email, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: EmailUpdate) -> Result<Email, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}
