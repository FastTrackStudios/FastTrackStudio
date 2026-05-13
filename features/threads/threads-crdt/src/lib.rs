use architect::{Page, RepoError, SortOrder};
use chrono::Utc;
use crdt::EntityCrdt;
use crdt::codec::{
    read_bool, read_dt, read_opt_i64, read_opt_str, read_opt_uuid, read_str, read_string_list,
    read_uuid, write_bool, write_dt, write_opt_i64, write_opt_str, write_opt_string_list,
    write_opt_uuid, write_str, write_string_list, write_uuid,
};
use loro::LoroMap;
use threads_proto::{
    Attachment, AttachmentCreate, AttachmentList, AttachmentRepo, AttachmentUpdate, Comment,
    CommentCreate, CommentList, CommentRepo, CommentUpdate, Reaction, ReactionCreate, ReactionList,
    ReactionRepo, ReactionUpdate,
};
use uuid::Uuid;

pub use crdt::{CrdtDoc, LoroRepo};

pub struct CommentEntity;

#[derive(Clone)]
pub struct CommentRepoLoro {
    inner: LoroRepo<CommentEntity>,
}

impl CommentRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<CommentEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for CommentEntity {
    type Wire = Comment;
    type Create = CommentCreate;
    type Update = CommentUpdate;
    type List = CommentList;

    const ROOT: &'static str = "comments";

    fn id(w: &Comment) -> Uuid {
        w.id
    }

    fn from_create(input: CommentCreate) -> Comment {
        let now = Utc::now();
        Comment {
            id: Uuid::new_v4(),
            entity_id: input.entity_id,
            entity_type: input.entity_type,
            author: input.author,
            body: input.body,
            time_start_ms: input.time_start_ms,
            time_end_ms: input.time_end_ms,
            reply_to: input.reply_to,
            resolved: input.resolved,
            resolved_by: input.resolved_by,
            mentions: input.mentions,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Comment) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "entity_id", e.entity_id)?;
        write_str(m, "entity_type", &e.entity_type)?;
        write_str(m, "author", &e.author)?;
        write_str(m, "body", &e.body)?;
        write_opt_i64(m, "time_start_ms", e.time_start_ms)?;
        write_opt_i64(m, "time_end_ms", e.time_end_ms)?;
        write_opt_uuid(m, "reply_to", e.reply_to)?;
        write_bool(m, "resolved", e.resolved)?;
        write_opt_str(m, "resolved_by", e.resolved_by.as_deref())?;
        write_string_list(m, "mentions", &e.mentions)?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Comment, RepoError> {
        Ok(Comment {
            id: read_uuid(m, "id")?,
            entity_id: read_uuid(m, "entity_id")?,
            entity_type: read_str(m, "entity_type")?,
            author: read_str(m, "author")?,
            body: read_str(m, "body")?,
            time_start_ms: read_opt_i64(m, "time_start_ms")?,
            time_end_ms: read_opt_i64(m, "time_end_ms")?,
            reply_to: read_opt_uuid(m, "reply_to")?,
            resolved: read_bool(m, "resolved")?,
            resolved_by: read_opt_str(m, "resolved_by")?,
            mentions: read_string_list(m, "mentions")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: CommentUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.entity_id {
            write_uuid(m, "entity_id", v)?;
        }
        if let Some(v) = u.entity_type {
            write_str(m, "entity_type", &v)?;
        }
        if let Some(v) = u.author {
            write_str(m, "author", &v)?;
        }
        if let Some(v) = u.body {
            write_str(m, "body", &v)?;
        }
        if let Some(v) = u.time_start_ms {
            write_opt_i64(m, "time_start_ms", v)?;
        }
        if let Some(v) = u.time_end_ms {
            write_opt_i64(m, "time_end_ms", v)?;
        }
        if let Some(v) = u.reply_to {
            write_opt_uuid(m, "reply_to", v)?;
        }
        if let Some(v) = u.resolved {
            write_bool(m, "resolved", v)?;
        }
        if let Some(v) = u.resolved_by {
            write_opt_str(m, "resolved_by", v.as_deref())?;
        }
        if let Some(v) = u.mentions {
            write_opt_string_list(m, "mentions", Some(&v))?;
        }
        if let Some(v) = u.tags {
            write_opt_string_list(m, "tags", Some(&v))?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [Comment], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "author" => items.sort_by(|a, b| a.author.cmp(&b.author)),
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

    fn build_list(items: Vec<Comment>, total: u32, page: Page) -> CommentList {
        CommentList { items, total, page }
    }
}

impl CommentRepo for CommentRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Comment, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<CommentList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: CommentCreate) -> Result<Comment, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: CommentUpdate) -> Result<Comment, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

pub struct ReactionEntity;

#[derive(Clone)]
pub struct ReactionRepoLoro {
    inner: LoroRepo<ReactionEntity>,
}

impl ReactionRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<ReactionEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for ReactionEntity {
    type Wire = Reaction;
    type Create = ReactionCreate;
    type Update = ReactionUpdate;
    type List = ReactionList;

    const ROOT: &'static str = "reactions";

    fn id(w: &Reaction) -> Uuid {
        w.id
    }

    fn from_create(input: ReactionCreate) -> Reaction {
        let now = Utc::now();
        Reaction {
            id: Uuid::new_v4(),
            entity_id: input.entity_id,
            entity_type: input.entity_type,
            emoji: input.emoji,
            user: input.user,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Reaction) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "entity_id", e.entity_id)?;
        write_str(m, "entity_type", &e.entity_type)?;
        write_str(m, "emoji", &e.emoji)?;
        write_str(m, "user", &e.user)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Reaction, RepoError> {
        Ok(Reaction {
            id: read_uuid(m, "id")?,
            entity_id: read_uuid(m, "entity_id")?,
            entity_type: read_str(m, "entity_type")?,
            emoji: read_str(m, "emoji")?,
            user: read_str(m, "user")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: ReactionUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.entity_id {
            write_uuid(m, "entity_id", v)?;
        }
        if let Some(v) = u.entity_type {
            write_str(m, "entity_type", &v)?;
        }
        if let Some(v) = u.emoji {
            write_str(m, "emoji", &v)?;
        }
        if let Some(v) = u.user {
            write_str(m, "user", &v)?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [Reaction], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "emoji" => items.sort_by(|a, b| a.emoji.cmp(&b.emoji)),
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

    fn build_list(items: Vec<Reaction>, total: u32, page: Page) -> ReactionList {
        ReactionList { items, total, page }
    }
}

impl ReactionRepo for ReactionRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Reaction, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<ReactionList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: ReactionCreate) -> Result<Reaction, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: ReactionUpdate) -> Result<Reaction, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

pub struct AttachmentEntity;

#[derive(Clone)]
pub struct AttachmentRepoLoro {
    inner: LoroRepo<AttachmentEntity>,
}

impl AttachmentRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<AttachmentEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for AttachmentEntity {
    type Wire = Attachment;
    type Create = AttachmentCreate;
    type Update = AttachmentUpdate;
    type List = AttachmentList;

    const ROOT: &'static str = "attachments";

    fn id(w: &Attachment) -> Uuid {
        w.id
    }

    fn from_create(input: AttachmentCreate) -> Attachment {
        let now = Utc::now();
        Attachment {
            id: Uuid::new_v4(),
            owner_id: input.owner_id,
            owner_type: input.owner_type,
            source: input.source,
            path: input.path,
            label: input.label,
            mime: input.mime,
            size_bytes: input.size_bytes,
            checksum: input.checksum,
            uploader: input.uploader,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Attachment) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "owner_id", e.owner_id)?;
        write_str(m, "owner_type", &e.owner_type)?;
        write_str(m, "source", &e.source)?;
        write_str(m, "path", &e.path)?;
        write_opt_str(m, "label", e.label.as_deref())?;
        write_opt_str(m, "mime", e.mime.as_deref())?;
        write_opt_i64(m, "size_bytes", e.size_bytes)?;
        write_opt_str(m, "checksum", e.checksum.as_deref())?;
        write_opt_str(m, "uploader", e.uploader.as_deref())?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Attachment, RepoError> {
        Ok(Attachment {
            id: read_uuid(m, "id")?,
            owner_id: read_uuid(m, "owner_id")?,
            owner_type: read_str(m, "owner_type")?,
            source: read_str(m, "source")?,
            path: read_str(m, "path")?,
            label: read_opt_str(m, "label")?,
            mime: read_opt_str(m, "mime")?,
            size_bytes: read_opt_i64(m, "size_bytes")?,
            checksum: read_opt_str(m, "checksum")?,
            uploader: read_opt_str(m, "uploader")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: AttachmentUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.owner_id {
            write_uuid(m, "owner_id", v)?;
        }
        if let Some(v) = u.owner_type {
            write_str(m, "owner_type", &v)?;
        }
        if let Some(v) = u.source {
            write_str(m, "source", &v)?;
        }
        if let Some(v) = u.path {
            write_str(m, "path", &v)?;
        }
        if let Some(v) = u.label {
            write_opt_str(m, "label", v.as_deref())?;
        }
        if let Some(v) = u.mime {
            write_opt_str(m, "mime", v.as_deref())?;
        }
        if let Some(v) = u.size_bytes {
            write_opt_i64(m, "size_bytes", v)?;
        }
        if let Some(v) = u.checksum {
            write_opt_str(m, "checksum", v.as_deref())?;
        }
        if let Some(v) = u.uploader {
            write_opt_str(m, "uploader", v.as_deref())?;
        }
        if let Some(v) = u.tags {
            write_opt_string_list(m, "tags", Some(&v))?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(
        items: &mut [Attachment],
        field: &str,
        order: SortOrder,
    ) -> Result<(), RepoError> {
        match field {
            "path" => items.sort_by(|a, b| a.path.cmp(&b.path)),
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

    fn build_list(items: Vec<Attachment>, total: u32, page: Page) -> AttachmentList {
        AttachmentList { items, total, page }
    }
}

impl AttachmentRepo for AttachmentRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Attachment, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<AttachmentList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: AttachmentCreate) -> Result<Attachment, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: AttachmentUpdate) -> Result<Attachment, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}
