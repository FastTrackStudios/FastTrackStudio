//! Loro-backed `TaskRepo` impl. One row per Task under the
//! `tasks` LoroMap.

use architect::{Page, RepoError, Sort, SortOrder};
use crdt::EntityCrdt;
use crdt::codec::{read_bool, read_str, read_uuid, write_bool, write_str, write_uuid};
use loro::LoroMap;
use project_proto::{Task, TaskCreate, TaskList, TaskRepo, TaskUpdate};
use uuid::Uuid;

use crate::{CrdtDoc, LoroRepo};

pub struct TaskEntity;

#[derive(Clone)]
pub struct TaskRepoLoro {
    inner: LoroRepo<TaskEntity>,
}

impl TaskRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
}

impl EntityCrdt for TaskEntity {
    type Wire = Task;
    type Create = TaskCreate;
    type Update = TaskUpdate;
    type List = TaskList;

    const ROOT: &'static str = "tasks";

    fn id(e: &Task) -> Uuid {
        e.id
    }

    fn from_create(c: TaskCreate) -> Task {
        Task {
            id: Uuid::new_v4(),
            project_id: c.project_id,
            title: c.title,
            done: c.done,
        }
    }

    fn encode_into(m: &LoroMap, e: &Task) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "project_id", e.project_id)?;
        write_str(m, "title", &e.title)?;
        write_bool(m, "done", e.done)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Task, RepoError> {
        Ok(Task {
            id: read_uuid(m, "id")?,
            project_id: read_uuid(m, "project_id")?,
            title: read_str(m, "title")?,
            done: read_bool(m, "done")?,
        })
    }

    fn apply_update(m: &LoroMap, u: TaskUpdate) -> Result<(), RepoError> {
        if let Some(project_id) = u.project_id {
            write_uuid(m, "project_id", project_id)?;
        }
        if let Some(title) = u.title {
            write_str(m, "title", &title)?;
        }
        if let Some(done) = u.done {
            write_bool(m, "done", done)?;
        }
        Ok(())
    }

    fn build_list(items: Vec<Task>, total: u32, page: Page) -> TaskList {
        TaskList { items, total, page }
    }

    fn sort_items(items: &mut [Task], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "title" => items.sort_by(|a, b| a.title.cmp(&b.title)),
            "done" => items.sort_by_key(|t| t.done),
            other => return Err(RepoError::Internal(format!("unknown sort field: {other}"))),
        }
        if order == SortOrder::Desc {
            items.reverse();
        }
        Ok(())
    }
}

impl TaskRepo for TaskRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Task, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: Page,
        sort: Option<Sort>,
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
