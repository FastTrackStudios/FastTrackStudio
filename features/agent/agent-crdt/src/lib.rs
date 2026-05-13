use agent_proto::{AgentRun, AgentRunCreate, AgentRunList, AgentRunRepo, AgentRunUpdate};
use architect::{Page, RepoError, SortOrder};
use chrono::Utc;
use crdt::EntityCrdt;
use crdt::codec::{
    read_dt, read_opt_dt, read_opt_str, read_opt_u32, read_opt_uuid, read_str, read_string_list,
    read_uuid, write_dt, write_opt_dt, write_opt_str, write_opt_string_list, write_opt_u32,
    write_opt_uuid, write_str, write_string_list, write_uuid,
};
use loro::LoroMap;
use uuid::Uuid;

pub use crdt::{CrdtDoc, LoroRepo};

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
