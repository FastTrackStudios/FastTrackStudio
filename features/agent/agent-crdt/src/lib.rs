use agent_proto::{
    AgentConversation, AgentConversationCreate, AgentConversationList, AgentConversationRepo,
    AgentConversationUpdate, AgentLogLine, AgentLogLineCreate, AgentLogLineList, AgentLogLineRepo,
    AgentLogLineUpdate, AgentRun, AgentRunCreate, AgentRunList, AgentRunRepo, AgentRunUpdate,
    ConversationMessage, ConversationMessageCreate, ConversationMessageList,
    ConversationMessageRepo, ConversationMessageUpdate, ToolCall, ToolCallCreate, ToolCallList,
    ToolCallRepo, ToolCallUpdate,
};
use architect::{Page, RepoError, SortOrder};
use chrono::Utc;
use crdt::EntityCrdt;
use crdt::codec::{
    read_bool, read_dt, read_i64, read_opt_dt, read_opt_i64, read_opt_str, read_opt_u32,
    read_opt_uuid, read_str, read_string_list, read_u32, read_uuid, write_bool, write_dt,
    write_i64, write_opt_dt, write_opt_i64, write_opt_str, write_opt_string_list, write_opt_u32,
    write_opt_uuid, write_str, write_string_list, write_u32, write_uuid,
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
            parent_run_id: input.parent_run_id,
            worktree_path: input.worktree_path,
            git_repo_connection_id: input.git_repo_connection_id,
            spawned_from_message_id: input.spawned_from_message_id,
            input_tokens: input.input_tokens,
            output_tokens: input.output_tokens,
            cache_read_tokens: input.cache_read_tokens,
            cache_creation_tokens: input.cache_creation_tokens,
            cost_cents_estimate: input.cost_cents_estimate,
            tool_call_count: input.tool_call_count,
            assistant_message_count: input.assistant_message_count,
            max_tokens: input.max_tokens,
            max_tool_calls: input.max_tool_calls,
            max_wall_seconds: input.max_wall_seconds,
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
        write_opt_uuid(m, "parent_run_id", e.parent_run_id)?;
        write_opt_str(m, "worktree_path", e.worktree_path.as_deref())?;
        write_opt_uuid(m, "git_repo_connection_id", e.git_repo_connection_id)?;
        write_opt_uuid(m, "spawned_from_message_id", e.spawned_from_message_id)?;
        write_opt_u32(m, "input_tokens", e.input_tokens)?;
        write_opt_u32(m, "output_tokens", e.output_tokens)?;
        write_opt_u32(m, "cache_read_tokens", e.cache_read_tokens)?;
        write_opt_u32(m, "cache_creation_tokens", e.cache_creation_tokens)?;
        write_opt_i64(m, "cost_cents_estimate", e.cost_cents_estimate)?;
        write_u32(m, "tool_call_count", e.tool_call_count)?;
        write_u32(m, "assistant_message_count", e.assistant_message_count)?;
        write_opt_i64(m, "max_tokens", e.max_tokens.map(|v| v as i64))?;
        write_opt_u32(m, "max_tool_calls", e.max_tool_calls)?;
        write_opt_u32(m, "max_wall_seconds", e.max_wall_seconds)?;
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
            parent_run_id: read_opt_uuid(m, "parent_run_id").unwrap_or(None),
            worktree_path: read_opt_str(m, "worktree_path").unwrap_or(None),
            git_repo_connection_id: read_opt_uuid(m, "git_repo_connection_id").unwrap_or(None),
            spawned_from_message_id: read_opt_uuid(m, "spawned_from_message_id").unwrap_or(None),
            input_tokens: read_opt_u32(m, "input_tokens").unwrap_or(None),
            output_tokens: read_opt_u32(m, "output_tokens").unwrap_or(None),
            cache_read_tokens: read_opt_u32(m, "cache_read_tokens").unwrap_or(None),
            cache_creation_tokens: read_opt_u32(m, "cache_creation_tokens").unwrap_or(None),
            cost_cents_estimate: read_opt_i64(m, "cost_cents_estimate").unwrap_or(None),
            tool_call_count: read_u32(m, "tool_call_count").unwrap_or(0),
            assistant_message_count: read_u32(m, "assistant_message_count").unwrap_or(0),
            max_tokens: read_opt_i64(m, "max_tokens")
                .unwrap_or(None)
                .map(|v| v as u64),
            max_tool_calls: read_opt_u32(m, "max_tool_calls").unwrap_or(None),
            max_wall_seconds: read_opt_u32(m, "max_wall_seconds").unwrap_or(None),
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
        if let Some(v) = u.parent_run_id {
            write_opt_uuid(m, "parent_run_id", v)?;
        }
        if let Some(v) = u.worktree_path {
            write_opt_str(m, "worktree_path", v.as_deref())?;
        }
        if let Some(v) = u.git_repo_connection_id {
            write_opt_uuid(m, "git_repo_connection_id", v)?;
        }
        if let Some(v) = u.spawned_from_message_id {
            write_opt_uuid(m, "spawned_from_message_id", v)?;
        }
        if let Some(v) = u.input_tokens {
            write_opt_u32(m, "input_tokens", v)?;
        }
        if let Some(v) = u.output_tokens {
            write_opt_u32(m, "output_tokens", v)?;
        }
        if let Some(v) = u.cache_read_tokens {
            write_opt_u32(m, "cache_read_tokens", v)?;
        }
        if let Some(v) = u.cache_creation_tokens {
            write_opt_u32(m, "cache_creation_tokens", v)?;
        }
        if let Some(v) = u.cost_cents_estimate {
            write_opt_i64(m, "cost_cents_estimate", v)?;
        }
        if let Some(v) = u.tool_call_count {
            write_u32(m, "tool_call_count", v)?;
        }
        if let Some(v) = u.assistant_message_count {
            write_u32(m, "assistant_message_count", v)?;
        }
        if let Some(v) = u.max_tokens {
            write_opt_i64(m, "max_tokens", v.map(|x| x as i64))?;
        }
        if let Some(v) = u.max_tool_calls {
            write_opt_u32(m, "max_tool_calls", v)?;
        }
        if let Some(v) = u.max_wall_seconds {
            write_opt_u32(m, "max_wall_seconds", v)?;
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

// ── ToolCallEntity ────────────────────────────────────────────────────

pub struct ToolCallEntity;

#[derive(Clone)]
pub struct ToolCallRepoLoro {
    inner: LoroRepo<ToolCallEntity>,
}

impl ToolCallRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<ToolCallEntity> {
        &self.inner
    }
}

impl EntityCrdt for ToolCallEntity {
    type Wire = ToolCall;
    type Create = ToolCallCreate;
    type Update = ToolCallUpdate;
    type List = ToolCallList;

    const ROOT: &'static str = "agent_tool_calls";

    fn id(w: &ToolCall) -> Uuid {
        w.id
    }

    fn from_create(input: ToolCallCreate) -> ToolCall {
        let now = Utc::now();
        ToolCall {
            id: Uuid::new_v4(),
            run_id: input.run_id,
            seq: input.seq,
            name: input.name,
            args_json: input.args_json,
            result_json: input.result_json,
            status: input.status,
            started_at: input.started_at,
            completed_at: input.completed_at,
            approval_required: input.approval_required,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &ToolCall) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "run_id", e.run_id)?;
        write_i64(m, "seq", e.seq)?;
        write_str(m, "name", &e.name)?;
        write_str(m, "args_json", &e.args_json)?;
        write_opt_str(m, "result_json", e.result_json.as_deref())?;
        write_str(m, "status", &e.status)?;
        write_opt_dt(m, "started_at", e.started_at)?;
        write_opt_dt(m, "completed_at", e.completed_at)?;
        write_bool(m, "approval_required", e.approval_required)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<ToolCall, RepoError> {
        Ok(ToolCall {
            id: read_uuid(m, "id")?,
            run_id: read_uuid(m, "run_id")?,
            seq: read_i64(m, "seq")?,
            name: read_str(m, "name")?,
            args_json: read_str(m, "args_json")?,
            result_json: read_opt_str(m, "result_json")?,
            status: read_str(m, "status")?,
            started_at: read_opt_dt(m, "started_at")?,
            completed_at: read_opt_dt(m, "completed_at")?,
            approval_required: read_bool(m, "approval_required")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: ToolCallUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.run_id {
            write_uuid(m, "run_id", v)?;
        }
        if let Some(v) = u.seq {
            write_i64(m, "seq", v)?;
        }
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.args_json {
            write_str(m, "args_json", &v)?;
        }
        if let Some(v) = u.result_json {
            write_opt_str(m, "result_json", v.as_deref())?;
        }
        if let Some(v) = u.status {
            write_str(m, "status", &v)?;
        }
        if let Some(v) = u.started_at {
            write_opt_dt(m, "started_at", v)?;
        }
        if let Some(v) = u.completed_at {
            write_opt_dt(m, "completed_at", v)?;
        }
        if let Some(v) = u.approval_required {
            write_bool(m, "approval_required", v)?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [ToolCall], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "seq" => items.sort_by_key(|x| x.seq),
            "name" => items.sort_by(|a, b| a.name.cmp(&b.name)),
            "status" => items.sort_by(|a, b| a.status.cmp(&b.status)),
            "started_at" => items.sort_by(|a, b| a.started_at.cmp(&b.started_at)),
            "completed_at" => items.sort_by(|a, b| a.completed_at.cmp(&b.completed_at)),
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

    fn build_list(items: Vec<ToolCall>, total: u32, page: Page) -> ToolCallList {
        ToolCallList { items, total, page }
    }
}

impl ToolCallRepo for ToolCallRepoLoro {
    async fn get(&self, id: Uuid) -> Result<ToolCall, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<ToolCallList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: ToolCallCreate) -> Result<ToolCall, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: ToolCallUpdate) -> Result<ToolCall, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── ConversationMessageEntity ─────────────────────────────────────────

pub struct ConversationMessageEntity;

#[derive(Clone)]
pub struct ConversationMessageRepoLoro {
    inner: LoroRepo<ConversationMessageEntity>,
}

impl ConversationMessageRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<ConversationMessageEntity> {
        &self.inner
    }
}

impl EntityCrdt for ConversationMessageEntity {
    type Wire = ConversationMessage;
    type Create = ConversationMessageCreate;
    type Update = ConversationMessageUpdate;
    type List = ConversationMessageList;

    const ROOT: &'static str = "agent_conversation_messages";

    fn id(w: &ConversationMessage) -> Uuid {
        w.id
    }

    fn from_create(input: ConversationMessageCreate) -> ConversationMessage {
        let now = Utc::now();
        ConversationMessage {
            id: Uuid::new_v4(),
            conversation_id: input.conversation_id,
            seq: input.seq,
            role: input.role,
            body: input.body,
            tool_call_id: input.tool_call_id,
            model_id: input.model_id,
            streaming: input.streaming,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &ConversationMessage) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "conversation_id", e.conversation_id)?;
        write_i64(m, "seq", e.seq)?;
        write_str(m, "role", &e.role)?;
        // Spec r[agent.crdt.conversation-message-text]: body is a
        // `LoroText` child container — see plans/loro-text-editor-upgrade.md
        // for the codec helpers. We seed with the initial body on
        // create; per-keystroke ops will replace the full-string write
        // when the editor wires through (future PR).
        let _ = crdt::codec::text_child(m, "body")?;
        if !e.body.is_empty() {
            crdt::codec::apply_text_diff(m, "body", "", &e.body)?;
        }
        write_opt_uuid(m, "tool_call_id", e.tool_call_id)?;
        write_opt_str(m, "model_id", e.model_id.as_deref())?;
        write_bool(m, "streaming", e.streaming)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<ConversationMessage, RepoError> {
        Ok(ConversationMessage {
            id: read_uuid(m, "id")?,
            conversation_id: read_uuid(m, "conversation_id")?,
            seq: read_i64(m, "seq")?,
            role: read_str(m, "role")?,
            body: crdt::codec::read_text_with_migration(m, "body")?,
            tool_call_id: read_opt_uuid(m, "tool_call_id")?,
            model_id: read_opt_str(m, "model_id")?,
            streaming: read_bool(m, "streaming")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: ConversationMessageUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.conversation_id {
            write_uuid(m, "conversation_id", v)?;
        }
        if let Some(v) = u.seq {
            write_i64(m, "seq", v)?;
        }
        if let Some(v) = u.role {
            write_str(m, "role", &v)?;
        }
        if let Some(v) = u.body {
            // Apply minimal diff against current LoroText so concurrent
            // peer edits merge at character granularity instead of LWW.
            let old = crdt::codec::read_text(m, "body")?;
            crdt::codec::apply_text_diff(m, "body", &old, &v)?;
        }
        if let Some(v) = u.tool_call_id {
            write_opt_uuid(m, "tool_call_id", v)?;
        }
        if let Some(v) = u.model_id {
            write_opt_str(m, "model_id", v.as_deref())?;
        }
        if let Some(v) = u.streaming {
            write_bool(m, "streaming", v)?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(
        items: &mut [ConversationMessage],
        field: &str,
        order: SortOrder,
    ) -> Result<(), RepoError> {
        match field {
            "seq" => items.sort_by_key(|x| x.seq),
            "role" => items.sort_by(|a, b| a.role.cmp(&b.role)),
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

    fn build_list(
        items: Vec<ConversationMessage>,
        total: u32,
        page: Page,
    ) -> ConversationMessageList {
        ConversationMessageList { items, total, page }
    }
}

impl ConversationMessageRepo for ConversationMessageRepoLoro {
    async fn get(&self, id: Uuid) -> Result<ConversationMessage, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<ConversationMessageList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(
        &self,
        input: ConversationMessageCreate,
    ) -> Result<ConversationMessage, RepoError> {
        self.inner.create(input).await
    }
    async fn update(
        &self,
        id: Uuid,
        input: ConversationMessageUpdate,
    ) -> Result<ConversationMessage, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

#[cfg(test)]
mod state_machine_tests {
    use agent_proto::{RunStatus, validate_status_transition};

    #[test]
    fn queued_to_starting_ok() {
        assert!(validate_status_transition(RunStatus::Queued, RunStatus::Starting).is_ok());
    }

    #[test]
    fn running_to_paused_ok() {
        assert!(validate_status_transition(RunStatus::Running, RunStatus::Paused).is_ok());
    }

    #[test]
    fn running_to_awaiting_input_ok() {
        assert!(validate_status_transition(RunStatus::Running, RunStatus::AwaitingInput).is_ok());
    }

    #[test]
    fn paused_to_running_ok() {
        assert!(validate_status_transition(RunStatus::Paused, RunStatus::Running).is_ok());
    }

    #[test]
    fn terminal_is_absorbing() {
        for from in [
            RunStatus::Completed,
            RunStatus::Failed,
            RunStatus::Cancelled,
            RunStatus::TimedOut,
        ] {
            for to in [RunStatus::Running, RunStatus::Starting, RunStatus::Queued] {
                assert!(
                    validate_status_transition(from, to).is_err(),
                    "expected illegal: {} -> {}",
                    from.as_str(),
                    to.as_str()
                );
            }
        }
    }

    #[test]
    fn queued_cannot_skip_to_running() {
        assert!(validate_status_transition(RunStatus::Queued, RunStatus::Running).is_err());
    }

    #[test]
    fn idempotent_self_transition_ok() {
        assert!(validate_status_transition(RunStatus::Running, RunStatus::Running).is_ok());
    }

    #[test]
    fn parse_round_trip() {
        for s in [
            "queued",
            "starting",
            "running",
            "paused",
            "awaiting-input",
            "completed",
            "failed",
            "cancelled",
            "timed-out",
        ] {
            let st = RunStatus::parse(s).expect(s);
            assert_eq!(st.as_str(), s);
        }
        assert!(RunStatus::parse("bogus").is_none());
    }

    #[test]
    fn terminal_check() {
        assert!(RunStatus::Completed.is_terminal());
        assert!(RunStatus::Failed.is_terminal());
        assert!(RunStatus::Cancelled.is_terminal());
        assert!(RunStatus::TimedOut.is_terminal());
        assert!(!RunStatus::Running.is_terminal());
        assert!(!RunStatus::Queued.is_terminal());
    }
}
