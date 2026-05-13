//! Loro-backed source-of-truth for the chat feature.

use architect::{Page, RepoError, SortOrder};
use chat_proto::{
    Channel, ChannelCreate, ChannelList, ChannelMember, ChannelMemberCreate, ChannelMemberList,
    ChannelMemberRepo, ChannelMemberUpdate, ChannelRepo, ChannelUpdate, Message, MessageCreate,
    MessageList, MessageRepo, MessageUpdate,
};
use chrono::Utc;
use crdt::EntityCrdt;
use crdt::codec::{
    read_bool, read_dt, read_opt_dt, read_opt_str, read_opt_uuid, read_str, read_string_list,
    read_uuid, write_bool, write_dt, write_opt_dt, write_opt_str, write_opt_string_list,
    write_opt_uuid, write_str, write_string_list, write_uuid,
};
use loro::LoroMap;
use uuid::Uuid;

pub use crdt::{CrdtDoc, LoroRepo};

// ── Channel ───────────────────────────────────────────────────────────

pub struct ChannelEntity;

#[derive(Clone)]
pub struct ChannelRepoLoro {
    inner: LoroRepo<ChannelEntity>,
}

impl ChannelRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<ChannelEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for ChannelEntity {
    type Wire = Channel;
    type Create = ChannelCreate;
    type Update = ChannelUpdate;
    type List = ChannelList;

    const ROOT: &'static str = "channels";

    fn id(w: &Channel) -> Uuid {
        w.id
    }

    fn from_create(input: ChannelCreate) -> Channel {
        let now = Utc::now();
        Channel {
            id: Uuid::new_v4(),
            name: input.name,
            kind: input.kind,
            topic: input.topic,
            project_id: input.project_id,
            archived: input.archived,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Channel) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "name", &e.name)?;
        write_str(m, "kind", &e.kind)?;
        write_opt_str(m, "topic", e.topic.as_deref())?;
        write_opt_uuid(m, "project_id", e.project_id)?;
        write_bool(m, "archived", e.archived)?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Channel, RepoError> {
        Ok(Channel {
            id: read_uuid(m, "id")?,
            name: read_str(m, "name")?,
            kind: read_str(m, "kind")?,
            topic: read_opt_str(m, "topic")?,
            project_id: read_opt_uuid(m, "project_id")?,
            archived: read_bool(m, "archived")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: ChannelUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.kind {
            write_str(m, "kind", &v)?;
        }
        if let Some(v) = u.topic {
            write_opt_str(m, "topic", v.as_deref())?;
        }
        if let Some(v) = u.project_id {
            write_opt_uuid(m, "project_id", v)?;
        }
        if let Some(v) = u.archived {
            write_bool(m, "archived", v)?;
        }
        if let Some(v) = u.tags {
            write_opt_string_list(m, "tags", Some(&v))?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [Channel], field: &str, order: SortOrder) -> Result<(), RepoError> {
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

    fn build_list(items: Vec<Channel>, total: u32, page: Page) -> ChannelList {
        ChannelList { items, total, page }
    }
}

impl ChannelRepo for ChannelRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Channel, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<ChannelList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: ChannelCreate) -> Result<Channel, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: ChannelUpdate) -> Result<Channel, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── Message ───────────────────────────────────────────────────────────

pub struct MessageEntity;

#[derive(Clone)]
pub struct MessageRepoLoro {
    inner: LoroRepo<MessageEntity>,
}

impl MessageRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<MessageEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for MessageEntity {
    type Wire = Message;
    type Create = MessageCreate;
    type Update = MessageUpdate;
    type List = MessageList;

    const ROOT: &'static str = "messages";

    fn id(w: &Message) -> Uuid {
        w.id
    }

    fn from_create(input: MessageCreate) -> Message {
        let now = Utc::now();
        Message {
            id: Uuid::new_v4(),
            channel_id: input.channel_id,
            author: input.author,
            body: input.body,
            reply_to: input.reply_to,
            edited_at: input.edited_at,
            deleted: input.deleted,
            mentions: input.mentions,
            attachment_ids: input.attachment_ids,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Message) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "channel_id", e.channel_id)?;
        write_str(m, "author", &e.author)?;
        write_str(m, "body", &e.body)?;
        write_opt_uuid(m, "reply_to", e.reply_to)?;
        write_opt_dt(m, "edited_at", e.edited_at)?;
        write_bool(m, "deleted", e.deleted)?;
        write_string_list(m, "mentions", &e.mentions)?;
        write_string_list(m, "attachment_ids", &e.attachment_ids)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Message, RepoError> {
        Ok(Message {
            id: read_uuid(m, "id")?,
            channel_id: read_uuid(m, "channel_id")?,
            author: read_str(m, "author")?,
            body: read_str(m, "body")?,
            reply_to: read_opt_uuid(m, "reply_to")?,
            edited_at: read_opt_dt(m, "edited_at")?,
            deleted: read_bool(m, "deleted")?,
            mentions: read_string_list(m, "mentions")?,
            attachment_ids: read_string_list(m, "attachment_ids")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: MessageUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.channel_id {
            write_uuid(m, "channel_id", v)?;
        }
        if let Some(v) = u.author {
            write_str(m, "author", &v)?;
        }
        if let Some(v) = u.body {
            write_str(m, "body", &v)?;
        }
        if let Some(v) = u.reply_to {
            write_opt_uuid(m, "reply_to", v)?;
        }
        if let Some(v) = u.edited_at {
            write_opt_dt(m, "edited_at", v)?;
        }
        if let Some(v) = u.deleted {
            write_bool(m, "deleted", v)?;
        }
        if let Some(v) = u.mentions {
            write_opt_string_list(m, "mentions", Some(&v))?;
        }
        if let Some(v) = u.attachment_ids {
            write_opt_string_list(m, "attachment_ids", Some(&v))?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [Message], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "channel_id" => items.sort_by(|a, b| a.channel_id.cmp(&b.channel_id)),
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

    fn build_list(items: Vec<Message>, total: u32, page: Page) -> MessageList {
        MessageList { items, total, page }
    }
}

impl MessageRepo for MessageRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Message, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<MessageList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: MessageCreate) -> Result<Message, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: MessageUpdate) -> Result<Message, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── ChannelMember ─────────────────────────────────────────────────────

pub struct ChannelMemberEntity;

#[derive(Clone)]
pub struct ChannelMemberRepoLoro {
    inner: LoroRepo<ChannelMemberEntity>,
}

impl ChannelMemberRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<ChannelMemberEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for ChannelMemberEntity {
    type Wire = ChannelMember;
    type Create = ChannelMemberCreate;
    type Update = ChannelMemberUpdate;
    type List = ChannelMemberList;

    const ROOT: &'static str = "channel_members";

    fn id(w: &ChannelMember) -> Uuid {
        w.id
    }

    fn from_create(input: ChannelMemberCreate) -> ChannelMember {
        let now = Utc::now();
        ChannelMember {
            id: Uuid::new_v4(),
            channel_id: input.channel_id,
            user: input.user,
            role: input.role,
            joined_at: input.joined_at,
            last_read_message_id: input.last_read_message_id,
            muted: input.muted,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &ChannelMember) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "channel_id", e.channel_id)?;
        write_str(m, "user", &e.user)?;
        write_str(m, "role", &e.role)?;
        write_dt(m, "joined_at", e.joined_at)?;
        write_opt_uuid(m, "last_read_message_id", e.last_read_message_id)?;
        write_bool(m, "muted", e.muted)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<ChannelMember, RepoError> {
        Ok(ChannelMember {
            id: read_uuid(m, "id")?,
            channel_id: read_uuid(m, "channel_id")?,
            user: read_str(m, "user")?,
            role: read_str(m, "role")?,
            joined_at: read_dt(m, "joined_at")?,
            last_read_message_id: read_opt_uuid(m, "last_read_message_id")?,
            muted: read_bool(m, "muted")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: ChannelMemberUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.channel_id {
            write_uuid(m, "channel_id", v)?;
        }
        if let Some(v) = u.user {
            write_str(m, "user", &v)?;
        }
        if let Some(v) = u.role {
            write_str(m, "role", &v)?;
        }
        if let Some(v) = u.joined_at {
            write_dt(m, "joined_at", v)?;
        }
        if let Some(v) = u.last_read_message_id {
            write_opt_uuid(m, "last_read_message_id", v)?;
        }
        if let Some(v) = u.muted {
            write_bool(m, "muted", v)?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(
        items: &mut [ChannelMember],
        field: &str,
        order: SortOrder,
    ) -> Result<(), RepoError> {
        match field {
            "joined_at" => items.sort_by(|a, b| a.joined_at.cmp(&b.joined_at)),
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

    fn build_list(items: Vec<ChannelMember>, total: u32, page: Page) -> ChannelMemberList {
        ChannelMemberList { items, total, page }
    }
}

impl ChannelMemberRepo for ChannelMemberRepoLoro {
    async fn get(&self, id: Uuid) -> Result<ChannelMember, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<ChannelMemberList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: ChannelMemberCreate) -> Result<ChannelMember, RepoError> {
        self.inner.create(input).await
    }
    async fn update(
        &self,
        id: Uuid,
        input: ChannelMemberUpdate,
    ) -> Result<ChannelMember, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}
