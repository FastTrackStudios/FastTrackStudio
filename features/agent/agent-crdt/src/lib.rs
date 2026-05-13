use agent_proto::{
    AgentConversation, AgentConversationCreate, AgentConversationList, AgentConversationRepo,
    AgentConversationUpdate, AgentLogLine, AgentLogLineCreate, AgentLogLineList, AgentLogLineRepo,
    AgentLogLineUpdate, AgentRun, AgentRunCreate, AgentRunList, AgentRunRepo, AgentRunUpdate,
};
use architect::{Page, RepoError, SortOrder};
use chrono::Utc;
use crdt::EntityCrdt;
use crdt::codec::{
    read_bool, read_dt, read_i64, read_opt_dt, read_opt_i64, read_opt_str, read_opt_u32,
    read_opt_uuid, read_str, read_string_list, read_uuid, write_bool, write_dt, write_i64,
    write_opt_dt, write_opt_i64, write_opt_str, write_opt_string_list, write_opt_u32,
    write_opt_uuid, write_str, write_string_list, write_uuid,
};

// `crdt::codec` has no i32 helpers — widen to i64 for storage and
// narrow on read, matching the pattern in `invoice-crdt`.
fn write_i32(m: &loro::LoroMap, k: &str, v: i32) -> Result<(), RepoError> {
    write_i64(m, k, v as i64)
}
fn read_i32(m: &loro::LoroMap, k: &str) -> Result<i32, RepoError> {
    Ok(read_i64(m, k)? as i32)
}
fn write_opt_i32(m: &loro::LoroMap, k: &str, v: Option<i32>) -> Result<(), RepoError> {
    write_opt_i64(m, k, v.map(|x| x as i64))
}
fn read_opt_i32(m: &loro::LoroMap, k: &str) -> Result<Option<i32>, RepoError> {
    Ok(read_opt_i64(m, k)?.map(|x| x as i32))
}
use loro::LoroMap;
use uuid::Uuid;

pub use crdt::{CrdtDoc, LoroRepo};

pub mod git;
pub use git::{GitRepoConnectionEntity, GitRepoConnectionRepoLoro};

pub struct AgentRunEntity;

#[derive(Clone)]
pub struct AgentRunRepoLoro {
    inner: LoroRepo<AgentRunEntity>,
}

impl AgentRunRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<AgentRunEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for AgentRunEntity {
    type Wire = AgentRun;
    type Create = AgentRunCreate;
    type Update = AgentRunUpdate;
    type List = AgentRunList;

    const ROOT: &'static str = "agent_runs";

    fn id(w: &AgentRun) -> Uuid {
        w.id
    }

    fn from_create(input: AgentRunCreate) -> AgentRun {
        let now = Utc::now();
        AgentRun {
            id: Uuid::new_v4(),
            name: input.name,
            kind: input.kind,
            prompt: input.prompt,
            status: input.status,
            task_id: input.task_id,
            started_at: input.started_at,
            completed_at: input.completed_at,
            result: input.result,
            error_message: input.error_message,
            tokens_used: input.tokens_used,
            cost_cents: input.cost_cents,
            tags: input.tags,
            integration: input.integration,
            external_id: input.external_id,
            external_url: input.external_url,
            log_cursor: input.log_cursor,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &AgentRun) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "name", &e.name)?;
        write_str(m, "kind", &e.kind)?;
        write_str(m, "prompt", &e.prompt)?;
        write_str(m, "status", &e.status)?;
        write_opt_uuid(m, "task_id", e.task_id)?;
        write_opt_dt(m, "started_at", e.started_at)?;
        write_opt_dt(m, "completed_at", e.completed_at)?;
        write_opt_str(m, "result", e.result.as_deref())?;
        write_opt_str(m, "error_message", e.error_message.as_deref())?;
        write_opt_u32(m, "tokens_used", e.tokens_used)?;
        write_opt_u32(m, "cost_cents", e.cost_cents)?;
        write_string_list(m, "tags", &e.tags)?;
        write_opt_str(m, "integration", e.integration.as_deref())?;
        write_opt_str(m, "external_id", e.external_id.as_deref())?;
        write_opt_str(m, "external_url", e.external_url.as_deref())?;
        write_opt_i64(m, "log_cursor", e.log_cursor)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<AgentRun, RepoError> {
        Ok(AgentRun {
            id: read_uuid(m, "id")?,
            name: read_str(m, "name")?,
            kind: read_str(m, "kind")?,
            prompt: read_str(m, "prompt")?,
            status: read_str(m, "status")?,
            task_id: read_opt_uuid(m, "task_id")?,
            started_at: read_opt_dt(m, "started_at")?,
            completed_at: read_opt_dt(m, "completed_at")?,
            result: read_opt_str(m, "result")?,
            error_message: read_opt_str(m, "error_message")?,
            tokens_used: read_opt_u32(m, "tokens_used")?,
            cost_cents: read_opt_u32(m, "cost_cents")?,
            tags: read_string_list(m, "tags")?,
            integration: read_opt_str(m, "integration").unwrap_or(None),
            external_id: read_opt_str(m, "external_id").unwrap_or(None),
            external_url: read_opt_str(m, "external_url").unwrap_or(None),
            log_cursor: read_opt_i64(m, "log_cursor").unwrap_or(None),
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: AgentRunUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.kind {
            write_str(m, "kind", &v)?;
        }
        if let Some(v) = u.prompt {
            write_str(m, "prompt", &v)?;
        }
        if let Some(v) = u.status {
            write_str(m, "status", &v)?;
        }
        if let Some(v) = u.task_id {
            write_opt_uuid(m, "task_id", v)?;
        }
        if let Some(v) = u.started_at {
            write_opt_dt(m, "started_at", v)?;
        }
        if let Some(v) = u.completed_at {
            write_opt_dt(m, "completed_at", v)?;
        }
        if let Some(v) = u.result {
            write_opt_str(m, "result", v.as_deref())?;
        }
        if let Some(v) = u.error_message {
            write_opt_str(m, "error_message", v.as_deref())?;
        }
        if let Some(v) = u.tokens_used {
            write_opt_u32(m, "tokens_used", v)?;
        }
        if let Some(v) = u.cost_cents {
            write_opt_u32(m, "cost_cents", v)?;
        }
        if let Some(v) = u.tags {
            write_opt_string_list(m, "tags", Some(&v))?;
        }
        if let Some(v) = u.integration {
            write_opt_str(m, "integration", v.as_deref())?;
        }
        if let Some(v) = u.external_id {
            write_opt_str(m, "external_id", v.as_deref())?;
        }
        if let Some(v) = u.external_url {
            write_opt_str(m, "external_url", v.as_deref())?;
        }
        if let Some(v) = u.log_cursor {
            write_opt_i64(m, "log_cursor", v)?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [AgentRun], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "name" => items.sort_by(|a, b| a.name.cmp(&b.name)),
            "status" => items.sort_by(|a, b| a.status.cmp(&b.status)),
            "started_at" => items.sort_by(|a, b| a.started_at.cmp(&b.started_at)),
            "completed_at" => items.sort_by(|a, b| a.completed_at.cmp(&b.completed_at)),
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

    fn build_list(items: Vec<AgentRun>, total: u32, page: Page) -> AgentRunList {
        AgentRunList { items, total, page }
    }
}

impl AgentRunRepo for AgentRunRepoLoro {
    async fn get(&self, id: Uuid) -> Result<AgentRun, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<AgentRunList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: AgentRunCreate) -> Result<AgentRun, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: AgentRunUpdate) -> Result<AgentRun, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── AgentLogLine ──────────────────────────────────────────────────────

pub struct AgentLogLineEntity;

#[derive(Clone)]
pub struct AgentLogLineRepoLoro {
    inner: LoroRepo<AgentLogLineEntity>,
}

impl AgentLogLineRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<AgentLogLineEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for AgentLogLineEntity {
    type Wire = AgentLogLine;
    type Create = AgentLogLineCreate;
    type Update = AgentLogLineUpdate;
    type List = AgentLogLineList;

    const ROOT: &'static str = "agent_log_lines";

    fn id(w: &AgentLogLine) -> Uuid {
        w.id
    }

    fn from_create(input: AgentLogLineCreate) -> AgentLogLine {
        let now = Utc::now();
        AgentLogLine {
            id: Uuid::new_v4(),
            run_id: input.run_id,
            at: input.at,
            level: input.level,
            source: input.source,
            text: input.text,
            external_event_id: input.external_event_id,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &AgentLogLine) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "run_id", e.run_id)?;
        write_dt(m, "at", e.at)?;
        write_str(m, "level", &e.level)?;
        write_str(m, "source", &e.source)?;
        write_str(m, "text", &e.text)?;
        write_opt_i64(m, "external_event_id", e.external_event_id)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<AgentLogLine, RepoError> {
        Ok(AgentLogLine {
            id: read_uuid(m, "id")?,
            run_id: read_uuid(m, "run_id")?,
            at: read_dt(m, "at")?,
            level: read_str(m, "level")?,
            source: read_str(m, "source")?,
            text: read_str(m, "text")?,
            external_event_id: read_opt_i64(m, "external_event_id")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: AgentLogLineUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.run_id {
            write_uuid(m, "run_id", v)?;
        }
        if let Some(v) = u.at {
            write_dt(m, "at", v)?;
        }
        if let Some(v) = u.level {
            write_str(m, "level", &v)?;
        }
        if let Some(v) = u.source {
            write_str(m, "source", &v)?;
        }
        if let Some(v) = u.text {
            write_str(m, "text", &v)?;
        }
        if let Some(v) = u.external_event_id {
            write_opt_i64(m, "external_event_id", v)?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(
        items: &mut [AgentLogLine],
        field: &str,
        order: SortOrder,
    ) -> Result<(), RepoError> {
        match field {
            "at" => items.sort_by(|a, b| a.at.cmp(&b.at)),
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

    fn build_list(items: Vec<AgentLogLine>, total: u32, page: Page) -> AgentLogLineList {
        AgentLogLineList { items, total, page }
    }
}

impl AgentLogLineRepo for AgentLogLineRepoLoro {
    async fn get(&self, id: Uuid) -> Result<AgentLogLine, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<AgentLogLineList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: AgentLogLineCreate) -> Result<AgentLogLine, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: AgentLogLineUpdate) -> Result<AgentLogLine, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── AgentConversation ─────────────────────────────────────────────────

pub struct AgentConversationEntity;

#[derive(Clone)]
pub struct AgentConversationRepoLoro {
    inner: LoroRepo<AgentConversationEntity>,
}

impl AgentConversationRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<AgentConversationEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for AgentConversationEntity {
    type Wire = AgentConversation;
    type Create = AgentConversationCreate;
    type Update = AgentConversationUpdate;
    type List = AgentConversationList;

    const ROOT: &'static str = "agent_conversations";

    fn id(w: &AgentConversation) -> Uuid {
        w.id
    }

    fn from_create(input: AgentConversationCreate) -> AgentConversation {
        let now = Utc::now();
        AgentConversation {
            id: Uuid::new_v4(),
            title: input.title,
            system_prompt: input.system_prompt,
            default_model: input.default_model,
            temperature_milli: input.temperature_milli,
            max_tokens: input.max_tokens,
            tool_set: input.tool_set,
            agent_run_id: input.agent_run_id,
            project_id: input.project_id,
            parent_conversation_id: input.parent_conversation_id,
            branch_from_message_id: input.branch_from_message_id,
            archived: input.archived,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &loro::LoroMap, e: &AgentConversation) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "title", &e.title)?;
        write_opt_str(m, "system_prompt", e.system_prompt.as_deref())?;
        write_str(m, "default_model", &e.default_model)?;
        write_i32(m, "temperature_milli", e.temperature_milli)?;
        write_opt_i32(m, "max_tokens", e.max_tokens)?;
        write_string_list(m, "tool_set", &e.tool_set)?;
        write_opt_uuid(m, "agent_run_id", e.agent_run_id)?;
        write_opt_uuid(m, "project_id", e.project_id)?;
        write_opt_uuid(m, "parent_conversation_id", e.parent_conversation_id)?;
        write_opt_uuid(m, "branch_from_message_id", e.branch_from_message_id)?;
        write_bool(m, "archived", e.archived)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &loro::LoroMap) -> Result<AgentConversation, RepoError> {
        Ok(AgentConversation {
            id: read_uuid(m, "id")?,
            title: read_str(m, "title")?,
            system_prompt: read_opt_str(m, "system_prompt")?,
            default_model: read_str(m, "default_model")?,
            temperature_milli: read_i32(m, "temperature_milli")?,
            max_tokens: read_opt_i32(m, "max_tokens")?,
            tool_set: read_string_list(m, "tool_set")?,
            agent_run_id: read_opt_uuid(m, "agent_run_id")?,
            project_id: read_opt_uuid(m, "project_id")?,
            parent_conversation_id: read_opt_uuid(m, "parent_conversation_id")?,
            branch_from_message_id: read_opt_uuid(m, "branch_from_message_id")?,
            archived: read_bool(m, "archived")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &loro::LoroMap, u: AgentConversationUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.title {
            write_str(m, "title", &v)?;
        }
        if let Some(v) = u.system_prompt {
            write_opt_str(m, "system_prompt", v.as_deref())?;
        }
        if let Some(v) = u.default_model {
            write_str(m, "default_model", &v)?;
        }
        if let Some(v) = u.temperature_milli {
            write_i32(m, "temperature_milli", v)?;
        }
        if let Some(v) = u.max_tokens {
            write_opt_i32(m, "max_tokens", v)?;
        }
        if let Some(v) = u.tool_set {
            write_opt_string_list(m, "tool_set", Some(&v))?;
        }
        if let Some(v) = u.agent_run_id {
            write_opt_uuid(m, "agent_run_id", v)?;
        }
        if let Some(v) = u.project_id {
            write_opt_uuid(m, "project_id", v)?;
        }
        if let Some(v) = u.parent_conversation_id {
            write_opt_uuid(m, "parent_conversation_id", v)?;
        }
        if let Some(v) = u.branch_from_message_id {
            write_opt_uuid(m, "branch_from_message_id", v)?;
        }
        if let Some(v) = u.archived {
            write_bool(m, "archived", v)?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(
        items: &mut [AgentConversation],
        field: &str,
        order: SortOrder,
    ) -> Result<(), RepoError> {
        match field {
            "title" => items.sort_by(|a, b| a.title.cmp(&b.title)),
            "created_at" => items.sort_by(|a, b| a.created_at.cmp(&b.created_at)),
            "updated_at" => items.sort_by(|a, b| a.updated_at.cmp(&b.updated_at)),
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

    fn build_list(items: Vec<AgentConversation>, total: u32, page: Page) -> AgentConversationList {
        AgentConversationList { items, total, page }
    }
}

impl AgentConversationRepo for AgentConversationRepoLoro {
    async fn get(&self, id: Uuid) -> Result<AgentConversation, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<AgentConversationList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: AgentConversationCreate) -> Result<AgentConversation, RepoError> {
        self.inner.create(input).await
    }
    async fn update(
        &self,
        id: Uuid,
        input: AgentConversationUpdate,
    ) -> Result<AgentConversation, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}
