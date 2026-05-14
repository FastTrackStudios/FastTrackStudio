//! Loro-backed `ProjectRepo` impl. One row per Project under the
//! `projects` LoroMap.

use architect::{Page, RepoError, Sort, SortOrder};
use crdt::EntityCrdt;
use crdt::codec::{read_str, read_uuid, write_str, write_uuid};
use loro::LoroMap;
use project_proto::{Project, ProjectCreate, ProjectList, ProjectRepo, ProjectUpdate};
use uuid::Uuid;

use crate::{CrdtDoc, LoroRepo};

pub struct ProjectEntity;

#[derive(Clone)]
pub struct ProjectRepoLoro {
    inner: LoroRepo<ProjectEntity>,
}

impl ProjectRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
}

impl EntityCrdt for ProjectEntity {
    type Wire = Project;
    type Create = ProjectCreate;
    type Update = ProjectUpdate;
    type List = ProjectList;

    const ROOT: &'static str = "projects";

    fn id(e: &Project) -> Uuid {
        e.id
    }

    fn from_create(c: ProjectCreate) -> Project {
        Project {
            id: Uuid::new_v4(),
            name: c.name,
        }
    }

    fn encode_into(m: &LoroMap, e: &Project) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "name", &e.name)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Project, RepoError> {
        Ok(Project {
            id: read_uuid(m, "id")?,
            name: read_str(m, "name")?,
        })
    }

    fn apply_update(m: &LoroMap, u: ProjectUpdate) -> Result<(), RepoError> {
        if let Some(name) = u.name {
            write_str(m, "name", &name)?;
        }
        Ok(())
    }

    fn build_list(items: Vec<Project>, total: u32, page: Page) -> ProjectList {
        ProjectList { items, total, page }
    }

    fn sort_items(items: &mut [Project], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "name" => items.sort_by(|a, b| a.name.cmp(&b.name)),
            other => return Err(RepoError::Internal(format!("unknown sort field: {other}"))),
        }
        if order == SortOrder::Desc {
            items.reverse();
        }
        Ok(())
    }
}

impl ProjectRepo for ProjectRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Project, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: Page,
        sort: Option<Sort>,
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
