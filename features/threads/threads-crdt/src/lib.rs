use architect::{Page, RepoError, SortOrder};
use chrono::Utc;
use crdt::EntityCrdt;
use crdt::codec::{
    read_bool, read_dt, read_opt_bool, read_opt_dt, read_opt_i64, read_opt_str, read_opt_uuid,
    read_str, read_string_list, read_uuid, write_bool, write_dt, write_opt_dt, write_opt_i64,
    write_opt_str, write_opt_string_list, write_opt_uuid, write_str, write_string_list, write_uuid,
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
            kind: "discussion".into(),
            action_status: None,
            action_assignee: None,
            action_priority: None,
            action_due_date: None,
            spawned_task_id: None,
            edited_at: None,
            deleted: false,
            deleted_by: None,
            anchor_json: input.anchor_json,
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
        write_str(m, "kind", &e.kind)?;
        write_opt_str(m, "action_status", e.action_status.as_deref())?;
        write_opt_str(m, "action_assignee", e.action_assignee.as_deref())?;
        write_opt_str(m, "action_priority", e.action_priority.as_deref())?;
        write_opt_dt(m, "action_due_date", e.action_due_date)?;
        write_opt_uuid(m, "spawned_task_id", e.spawned_task_id)?;
        write_opt_dt(m, "edited_at", e.edited_at)?;
        write_bool(m, "deleted", e.deleted)?;
        write_opt_str(m, "deleted_by", e.deleted_by.as_deref())?;
        write_opt_str(m, "anchor_json", e.anchor_json.as_deref())?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Comment, RepoError> {
        // Tolerant decode: pre-extension snapshots lack the post-Phase-A
        // fields. Missing values fall back to the documented defaults so
        // existing on-disk docs load without migration.
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
            kind: read_opt_str(m, "kind")?.unwrap_or_else(|| "discussion".into()),
            action_status: read_opt_str(m, "action_status")?,
            action_assignee: read_opt_str(m, "action_assignee")?,
            action_priority: read_opt_str(m, "action_priority")?,
            action_due_date: read_opt_dt(m, "action_due_date")?,
            spawned_task_id: read_opt_uuid(m, "spawned_task_id")?,
            edited_at: read_opt_dt(m, "edited_at")?,
            deleted: read_opt_bool(m, "deleted")?.unwrap_or(false),
            deleted_by: read_opt_str(m, "deleted_by")?,
            anchor_json: read_opt_str(m, "anchor_json")?,
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
        if let Some(v) = u.kind {
            write_str(m, "kind", &v)?;
        }
        if let Some(v) = u.action_status {
            write_opt_str(m, "action_status", v.as_deref())?;
        }
        if let Some(v) = u.action_assignee {
            write_opt_str(m, "action_assignee", v.as_deref())?;
        }
        if let Some(v) = u.action_priority {
            write_opt_str(m, "action_priority", v.as_deref())?;
        }
        if let Some(v) = u.action_due_date {
            write_opt_dt(m, "action_due_date", v)?;
        }
        if let Some(v) = u.spawned_task_id {
            write_opt_uuid(m, "spawned_task_id", v)?;
        }
        if let Some(v) = u.edited_at {
            write_opt_dt(m, "edited_at", v)?;
        }
        if let Some(v) = u.deleted {
            write_bool(m, "deleted", v)?;
        }
        if let Some(v) = u.deleted_by {
            write_opt_str(m, "deleted_by", v.as_deref())?;
        }
        if let Some(v) = u.anchor_json {
            write_opt_str(m, "anchor_json", v.as_deref())?;
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
            kind: "file".into(),
            duration_ms: None,
            width: None,
            height: None,
            blob_url: None,
            blob_loro_key: None,
            waveform_json: None,
            transcript: None,
            title: None,
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
        write_str(m, "kind", &e.kind)?;
        write_opt_i64(m, "duration_ms", e.duration_ms)?;
        write_opt_i64(m, "width", e.width.map(i64::from))?;
        write_opt_i64(m, "height", e.height.map(i64::from))?;
        write_opt_str(m, "blob_url", e.blob_url.as_deref())?;
        write_opt_str(m, "blob_loro_key", e.blob_loro_key.as_deref())?;
        write_opt_str(m, "waveform_json", e.waveform_json.as_deref())?;
        write_opt_str(m, "transcript", e.transcript.as_deref())?;
        write_opt_str(m, "title", e.title.as_deref())?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Attachment, RepoError> {
        // Tolerant decode: pre-extension snapshots lack the post-Phase-A
        // fields. Missing values fall back to defaults.
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
            kind: read_opt_str(m, "kind")?.unwrap_or_else(|| "file".into()),
            duration_ms: read_opt_i64(m, "duration_ms")?,
            width: read_opt_i64(m, "width")?.map(|v| v as i32),
            height: read_opt_i64(m, "height")?.map(|v| v as i32),
            blob_url: read_opt_str(m, "blob_url")?,
            blob_loro_key: read_opt_str(m, "blob_loro_key")?,
            waveform_json: read_opt_str(m, "waveform_json")?,
            transcript: read_opt_str(m, "transcript")?,
            title: read_opt_str(m, "title")?,
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
        if let Some(v) = u.kind {
            write_str(m, "kind", &v)?;
        }
        if let Some(v) = u.duration_ms {
            write_opt_i64(m, "duration_ms", v)?;
        }
        if let Some(v) = u.width {
            write_opt_i64(m, "width", v.map(i64::from))?;
        }
        if let Some(v) = u.height {
            write_opt_i64(m, "height", v.map(i64::from))?;
        }
        if let Some(v) = u.blob_url {
            write_opt_str(m, "blob_url", v.as_deref())?;
        }
        if let Some(v) = u.blob_loro_key {
            write_opt_str(m, "blob_loro_key", v.as_deref())?;
        }
        if let Some(v) = u.waveform_json {
            write_opt_str(m, "waveform_json", v.as_deref())?;
        }
        if let Some(v) = u.transcript {
            write_opt_str(m, "transcript", v.as_deref())?;
        }
        if let Some(v) = u.title {
            write_opt_str(m, "title", v.as_deref())?;
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

#[cfg(test)]
mod tests {
    use super::*;
    use crdt::EntityCrdt;
    use loro::LoroDoc;

    /// Build a Comment map containing only the pre-Phase-A field shape, then
    /// verify `decode_from` reconstructs a Comment with documented defaults
    /// for the new post-Phase-A fields.
    #[test]
    fn tolerant_decode_pre_extension_comment() {
        let doc = LoroDoc::new();
        let root = doc.get_map("test_root");
        let id = Uuid::new_v4();
        let entity_id = Uuid::new_v4();
        let now = Utc::now();

        write_uuid(&root, "id", id).unwrap();
        write_uuid(&root, "entity_id", entity_id).unwrap();
        write_str(&root, "entity_type", "task").unwrap();
        write_str(&root, "author", "alice").unwrap();
        write_str(&root, "body", "looks good").unwrap();
        write_opt_i64(&root, "time_start_ms", None).unwrap();
        write_opt_i64(&root, "time_end_ms", None).unwrap();
        write_opt_uuid(&root, "reply_to", None).unwrap();
        write_bool(&root, "resolved", false).unwrap();
        write_opt_str(&root, "resolved_by", None).unwrap();
        write_string_list(&root, "mentions", &[]).unwrap();
        write_string_list(&root, "tags", &[]).unwrap();
        write_dt(&root, "created_at", now).unwrap();
        write_dt(&root, "updated_at", now).unwrap();

        let c = CommentEntity::decode_from(&root).expect("tolerant decode");

        assert_eq!(c.id, id);
        assert_eq!(c.body, "looks good");
        assert_eq!(c.kind, "discussion");
        assert!(c.action_status.is_none());
        assert!(c.action_assignee.is_none());
        assert!(c.action_priority.is_none());
        assert!(c.action_due_date.is_none());
        assert!(c.spawned_task_id.is_none());
        assert!(c.edited_at.is_none());
        assert!(!c.deleted);
        assert!(c.deleted_by.is_none());
        assert!(c.anchor_json.is_none());
    }

    #[test]
    fn tolerant_decode_pre_extension_attachment() {
        let doc = LoroDoc::new();
        let root = doc.get_map("test_root");
        let id = Uuid::new_v4();
        let owner_id = Uuid::new_v4();
        let now = Utc::now();

        write_uuid(&root, "id", id).unwrap();
        write_uuid(&root, "owner_id", owner_id).unwrap();
        write_str(&root, "owner_type", "comment").unwrap();
        write_str(&root, "source", "upload").unwrap();
        write_str(&root, "path", "attachments/1/report.pdf").unwrap();
        write_opt_str(&root, "label", None).unwrap();
        write_opt_str(&root, "mime", Some("application/pdf")).unwrap();
        write_opt_i64(&root, "size_bytes", Some(12345)).unwrap();
        write_opt_str(&root, "checksum", None).unwrap();
        write_opt_str(&root, "uploader", None).unwrap();
        write_string_list(&root, "tags", &[]).unwrap();
        write_dt(&root, "created_at", now).unwrap();
        write_dt(&root, "updated_at", now).unwrap();

        let a = AttachmentEntity::decode_from(&root).expect("tolerant decode");

        assert_eq!(a.id, id);
        assert_eq!(a.path, "attachments/1/report.pdf");
        assert_eq!(a.kind, "file");
        assert!(a.duration_ms.is_none());
        assert!(a.width.is_none());
        assert!(a.height.is_none());
        assert!(a.blob_url.is_none());
        assert!(a.blob_loro_key.is_none());
        assert!(a.waveform_json.is_none());
        assert!(a.transcript.is_none());
        assert!(a.title.is_none());
    }
}
