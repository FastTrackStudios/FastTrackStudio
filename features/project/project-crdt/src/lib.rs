//! Loro-backed source-of-truth for the project feature. Four
//! entities, four `EntityCrdt` impls, four `*RepoLoro` newtypes. The
//! `ProjectService` (reorder / reparent / complete-cascade) is a
//! separate impl that owns LoroTree + LoroMovableList sub-containers;
//! it's stubbed for now and fleshed out when the UI lands.

use architect::{Page, RepoError, SortOrder};
use chrono::Utc;
use crdt::EntityCrdt;
use crdt::codec::{
    read_dt, read_i64, read_opt_dt, read_opt_i64, read_opt_str, read_opt_uuid, read_str,
    read_string_list, read_uuid, write_dt, write_i64, write_opt_dt, write_opt_i64, write_opt_str,
    write_opt_string_list, write_opt_uuid, write_str, write_string_list, write_uuid,
};
use loro::LoroMap;
use project_proto::{
    CommitRef, Cycle, CycleCreate, CycleList, CycleRepo, CycleUpdate, Milestone, MilestoneCreate,
    MilestoneList, MilestoneRepo, MilestoneUpdate, Project, ProjectCreate, ProjectList,
    ProjectRepo, ProjectUpdate, Task, TaskCreate, TaskList, TaskRepo, TaskUpdate,
};
use uuid::Uuid;

// ── CommitRef codec ───────────────────────────────────────────────────
//
// A Vec<CommitRef> is encoded as a JSON-string at the field key (one
// row per Task, replaced wholesale on update). This matches the
// codec strategy used by `write_string_list` (tab-joined → naive LWW
// on the whole vec): fine for append-mostly leaf data like commit
// pointers. Upgrade to a LoroList<LoroMap> sub-container if
// concurrent commit appends from multiple replicas start clobbering.

fn write_commit_refs(m: &LoroMap, k: &str, v: &[CommitRef]) -> Result<(), RepoError> {
    let encoded = serde_json::to_string(v)
        .map_err(|e| RepoError::Internal(format!("encode commit_refs: {e}")))?;
    crdt::codec::write_str(m, k, &encoded)
}

fn read_commit_refs(m: &LoroMap, k: &str) -> Result<Vec<CommitRef>, RepoError> {
    let raw = crdt::codec::read_str(m, k)?;
    if raw.is_empty() {
        return Ok(Vec::new());
    }
    serde_json::from_str(&raw)
        .map_err(|e| RepoError::Internal(format!("decode commit_refs at `{k}`: {e}")))
}

pub use crdt::{CrdtDoc, LoroRepo};

// ── Project ───────────────────────────────────────────────────────────

pub struct ProjectEntity;

#[derive(Clone)]
pub struct ProjectRepoLoro {
    inner: LoroRepo<ProjectEntity>,
}

impl ProjectRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<ProjectEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for ProjectEntity {
    type Wire = Project;
    type Create = ProjectCreate;
    type Update = ProjectUpdate;
    type List = ProjectList;

    const ROOT: &'static str = "projects";

    fn id(w: &Project) -> Uuid {
        w.id
    }

    fn from_create(input: ProjectCreate) -> Project {
        let now = Utc::now();
        Project {
            id: Uuid::new_v4(),
            name: input.name,
            description: input.description,
            status: input.status,
            project_type: input.project_type,
            color: input.color,
            owner: input.owner,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Project) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "name", &e.name)?;
        write_opt_str(m, "description", e.description.as_deref())?;
        write_str(m, "status", &e.status)?;
        write_opt_str(m, "project_type", e.project_type.as_deref())?;
        write_opt_str(m, "color", e.color.as_deref())?;
        write_opt_str(m, "owner", e.owner.as_deref())?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Project, RepoError> {
        Ok(Project {
            id: read_uuid(m, "id")?,
            name: read_str(m, "name")?,
            description: read_opt_str(m, "description")?,
            status: read_str(m, "status")?,
            project_type: read_opt_str(m, "project_type")?,
            color: read_opt_str(m, "color")?,
            owner: read_opt_str(m, "owner")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: ProjectUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.description {
            write_opt_str(m, "description", v.as_deref())?;
        }
        if let Some(v) = u.status {
            write_str(m, "status", &v)?;
        }
        if let Some(v) = u.project_type {
            write_opt_str(m, "project_type", v.as_deref())?;
        }
        if let Some(v) = u.color {
            write_opt_str(m, "color", v.as_deref())?;
        }
        if let Some(v) = u.owner {
            write_opt_str(m, "owner", v.as_deref())?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [Project], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "name" => items.sort_by(|a, b| a.name.cmp(&b.name)),
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

    fn build_list(items: Vec<Project>, total: u32, page: Page) -> ProjectList {
        ProjectList { items, total, page }
    }
}

impl ProjectRepo for ProjectRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Project, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<ProjectList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: ProjectCreate) -> Result<Project, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: ProjectUpdate) -> Result<Project, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── Task ──────────────────────────────────────────────────────────────

pub struct TaskEntity;

#[derive(Clone)]
pub struct TaskRepoLoro {
    inner: LoroRepo<TaskEntity>,
}

impl TaskRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<TaskEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for TaskEntity {
    type Wire = Task;
    type Create = TaskCreate;
    type Update = TaskUpdate;
    type List = TaskList;

    const ROOT: &'static str = "tasks";

    fn id(w: &Task) -> Uuid {
        w.id
    }

    fn from_create(input: TaskCreate) -> Task {
        let now = Utc::now();
        Task {
            id: Uuid::new_v4(),
            project_id: input.project_id,
            parent_id: input.parent_id,
            cycle_id: input.cycle_id,
            title: input.title,
            description: input.description,
            status: input.status,
            priority: input.priority,
            assignee: input.assignee,
            estimate_minutes: input.estimate_minutes,
            due_date: input.due_date,
            tags: input.tags,
            sort_index: input.sort_index,
            completed_at: input.completed_at,
            agent_run_id: input.agent_run_id,
            branch_name: input.branch_name,
            pr_urls: input.pr_urls,
            commit_refs: input.commit_refs,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Task) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "project_id", e.project_id)?;
        write_opt_uuid(m, "parent_id", e.parent_id)?;
        write_opt_uuid(m, "cycle_id", e.cycle_id)?;
        write_str(m, "title", &e.title)?;
        write_opt_str(m, "description", e.description.as_deref())?;
        write_str(m, "status", &e.status)?;
        write_str(m, "priority", &e.priority)?;
        write_opt_str(m, "assignee", e.assignee.as_deref())?;
        write_opt_i64(m, "estimate_minutes", e.estimate_minutes)?;
        write_opt_dt(m, "due_date", e.due_date)?;
        write_string_list(m, "tags", &e.tags)?;
        write_i64(m, "sort_index", e.sort_index)?;
        write_opt_dt(m, "completed_at", e.completed_at)?;
        write_opt_uuid(m, "agent_run_id", e.agent_run_id)?;
        write_opt_str(m, "branch_name", e.branch_name.as_deref())?;
        write_string_list(m, "pr_urls", &e.pr_urls)?;
        write_commit_refs(m, "commit_refs", &e.commit_refs)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Task, RepoError> {
        Ok(Task {
            id: read_uuid(m, "id")?,
            project_id: read_uuid(m, "project_id")?,
            parent_id: read_opt_uuid(m, "parent_id")?,
            cycle_id: read_opt_uuid(m, "cycle_id")?,
            title: read_str(m, "title")?,
            description: read_opt_str(m, "description")?,
            status: read_str(m, "status")?,
            priority: read_str(m, "priority")?,
            assignee: read_opt_str(m, "assignee")?,
            estimate_minutes: read_opt_i64(m, "estimate_minutes")?,
            due_date: read_opt_dt(m, "due_date")?,
            tags: read_string_list(m, "tags")?,
            sort_index: read_i64(m, "sort_index")?,
            completed_at: read_opt_dt(m, "completed_at")?,
            agent_run_id: read_opt_uuid(m, "agent_run_id").unwrap_or(None),
            branch_name: read_opt_str(m, "branch_name").unwrap_or(None),
            pr_urls: read_string_list(m, "pr_urls").unwrap_or_default(),
            commit_refs: read_commit_refs(m, "commit_refs").unwrap_or_default(),
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: TaskUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.project_id {
            write_uuid(m, "project_id", v)?;
        }
        if let Some(v) = u.parent_id {
            write_opt_uuid(m, "parent_id", v)?;
        }
        if let Some(v) = u.cycle_id {
            write_opt_uuid(m, "cycle_id", v)?;
        }
        if let Some(v) = u.title {
            write_str(m, "title", &v)?;
        }
        if let Some(v) = u.description {
            write_opt_str(m, "description", v.as_deref())?;
        }
        if let Some(v) = u.status {
            write_str(m, "status", &v)?;
        }
        if let Some(v) = u.priority {
            write_str(m, "priority", &v)?;
        }
        if let Some(v) = u.assignee {
            write_opt_str(m, "assignee", v.as_deref())?;
        }
        if let Some(v) = u.estimate_minutes {
            write_opt_i64(m, "estimate_minutes", v)?;
        }
        if let Some(v) = u.due_date {
            write_opt_dt(m, "due_date", v)?;
        }
        if let Some(v) = u.tags {
            write_opt_string_list(m, "tags", Some(&v))?;
        }
        if let Some(v) = u.sort_index {
            write_i64(m, "sort_index", v)?;
        }
        if let Some(v) = u.completed_at {
            write_opt_dt(m, "completed_at", v)?;
        }
        if let Some(v) = u.agent_run_id {
            write_opt_uuid(m, "agent_run_id", v)?;
        }
        if let Some(v) = u.branch_name {
            write_opt_str(m, "branch_name", v.as_deref())?;
        }
        if let Some(v) = u.pr_urls {
            write_opt_string_list(m, "pr_urls", Some(&v))?;
        }
        if let Some(v) = u.commit_refs {
            write_commit_refs(m, "commit_refs", &v)?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [Task], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "title" => items.sort_by(|a, b| a.title.cmp(&b.title)),
            "status" => items.sort_by(|a, b| a.status.cmp(&b.status)),
            "priority" => items.sort_by(|a, b| a.priority.cmp(&b.priority)),
            "due_date" => items.sort_by(|a, b| a.due_date.cmp(&b.due_date)),
            "sort_index" => items.sort_by(|a, b| a.sort_index.cmp(&b.sort_index)),
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

    fn build_list(items: Vec<Task>, total: u32, page: Page) -> TaskList {
        TaskList { items, total, page }
    }
}

impl TaskRepo for TaskRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Task, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<TaskList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: TaskCreate) -> Result<Task, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: TaskUpdate) -> Result<Task, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── Cycle ─────────────────────────────────────────────────────────────

pub struct CycleEntity;

#[derive(Clone)]
pub struct CycleRepoLoro {
    inner: LoroRepo<CycleEntity>,
}

impl CycleRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<CycleEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for CycleEntity {
    type Wire = Cycle;
    type Create = CycleCreate;
    type Update = CycleUpdate;
    type List = CycleList;

    const ROOT: &'static str = "cycles";

    fn id(w: &Cycle) -> Uuid {
        w.id
    }

    fn from_create(input: CycleCreate) -> Cycle {
        let now = Utc::now();
        Cycle {
            id: Uuid::new_v4(),
            project_id: input.project_id,
            name: input.name,
            start_date: input.start_date,
            end_date: input.end_date,
            status: input.status,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Cycle) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "project_id", e.project_id)?;
        write_str(m, "name", &e.name)?;
        write_dt(m, "start_date", e.start_date)?;
        write_dt(m, "end_date", e.end_date)?;
        write_str(m, "status", &e.status)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Cycle, RepoError> {
        Ok(Cycle {
            id: read_uuid(m, "id")?,
            project_id: read_uuid(m, "project_id")?,
            name: read_str(m, "name")?,
            start_date: read_dt(m, "start_date")?,
            end_date: read_dt(m, "end_date")?,
            status: read_str(m, "status")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: CycleUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.project_id {
            write_uuid(m, "project_id", v)?;
        }
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.start_date {
            write_dt(m, "start_date", v)?;
        }
        if let Some(v) = u.end_date {
            write_dt(m, "end_date", v)?;
        }
        if let Some(v) = u.status {
            write_str(m, "status", &v)?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [Cycle], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "name" => items.sort_by(|a, b| a.name.cmp(&b.name)),
            "start_date" => items.sort_by(|a, b| a.start_date.cmp(&b.start_date)),
            "end_date" => items.sort_by(|a, b| a.end_date.cmp(&b.end_date)),
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

    fn build_list(items: Vec<Cycle>, total: u32, page: Page) -> CycleList {
        CycleList { items, total, page }
    }
}

impl CycleRepo for CycleRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Cycle, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<CycleList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: CycleCreate) -> Result<Cycle, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: CycleUpdate) -> Result<Cycle, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── Milestone ─────────────────────────────────────────────────────────

pub struct MilestoneEntity;

#[derive(Clone)]
pub struct MilestoneRepoLoro {
    inner: LoroRepo<MilestoneEntity>,
}

impl MilestoneRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<MilestoneEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for MilestoneEntity {
    type Wire = Milestone;
    type Create = MilestoneCreate;
    type Update = MilestoneUpdate;
    type List = MilestoneList;

    const ROOT: &'static str = "milestones";

    fn id(w: &Milestone) -> Uuid {
        w.id
    }

    fn from_create(input: MilestoneCreate) -> Milestone {
        let now = Utc::now();
        Milestone {
            id: Uuid::new_v4(),
            project_id: input.project_id,
            name: input.name,
            description: input.description,
            target_date: input.target_date,
            completed_at: input.completed_at,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Milestone) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "project_id", e.project_id)?;
        write_str(m, "name", &e.name)?;
        write_opt_str(m, "description", e.description.as_deref())?;
        write_opt_dt(m, "target_date", e.target_date)?;
        write_opt_dt(m, "completed_at", e.completed_at)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Milestone, RepoError> {
        Ok(Milestone {
            id: read_uuid(m, "id")?,
            project_id: read_uuid(m, "project_id")?,
            name: read_str(m, "name")?,
            description: read_opt_str(m, "description")?,
            target_date: read_opt_dt(m, "target_date")?,
            completed_at: read_opt_dt(m, "completed_at")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: MilestoneUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.project_id {
            write_uuid(m, "project_id", v)?;
        }
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.description {
            write_opt_str(m, "description", v.as_deref())?;
        }
        if let Some(v) = u.target_date {
            write_opt_dt(m, "target_date", v)?;
        }
        if let Some(v) = u.completed_at {
            write_opt_dt(m, "completed_at", v)?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [Milestone], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "name" => items.sort_by(|a, b| a.name.cmp(&b.name)),
            "target_date" => items.sort_by(|a, b| a.target_date.cmp(&b.target_date)),
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

    fn build_list(items: Vec<Milestone>, total: u32, page: Page) -> MilestoneList {
        MilestoneList { items, total, page }
    }
}

impl MilestoneRepo for MilestoneRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Milestone, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<MilestoneList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: MilestoneCreate) -> Result<Milestone, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: MilestoneUpdate) -> Result<Milestone, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}
