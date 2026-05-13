//! Loro source-of-truth for `GitRepoConnection`. Kept in its own
//! submodule because the data is server-only sensitive — separating
//! it from the main agent doc makes it easier to reason about
//! "never ship this to wasm" boundaries and to swap the storage
//! engine independently later if needed.

use agent_proto::{
    GitRepoConnection, GitRepoConnectionCreate, GitRepoConnectionList, GitRepoConnectionRepo,
    GitRepoConnectionUpdate,
};
use architect::{Page, RepoError, SortOrder};
use chrono::Utc;
use crdt::EntityCrdt;
use crdt::codec::{
    read_dt, read_opt_dt, read_opt_uuid, read_str, read_uuid, write_dt, write_opt_dt,
    write_opt_uuid, write_str, write_uuid,
};
use loro::LoroMap;
use uuid::Uuid;

use crate::{CrdtDoc, LoroRepo};

pub struct GitRepoConnectionEntity;

#[derive(Clone)]
pub struct GitRepoConnectionRepoLoro {
    inner: LoroRepo<GitRepoConnectionEntity>,
}

impl GitRepoConnectionRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<GitRepoConnectionEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for GitRepoConnectionEntity {
    type Wire = GitRepoConnection;
    type Create = GitRepoConnectionCreate;
    type Update = GitRepoConnectionUpdate;
    type List = GitRepoConnectionList;

    const ROOT: &'static str = "git_repo_connections";

    fn id(w: &GitRepoConnection) -> Uuid {
        w.id
    }

    fn from_create(input: GitRepoConnectionCreate) -> GitRepoConnection {
        let now = Utc::now();
        GitRepoConnection {
            id: Uuid::new_v4(),
            provider: input.provider,
            owner: input.owner,
            repo: input.repo,
            default_branch: input.default_branch,
            project_id: input.project_id,
            webhook_secret_hash: input.webhook_secret_hash,
            webhook_path: input.webhook_path,
            last_event_at: input.last_event_at,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &GitRepoConnection) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "provider", &e.provider)?;
        write_str(m, "owner", &e.owner)?;
        write_str(m, "repo", &e.repo)?;
        write_str(m, "default_branch", &e.default_branch)?;
        write_opt_uuid(m, "project_id", e.project_id)?;
        write_str(m, "webhook_secret_hash", &e.webhook_secret_hash)?;
        write_str(m, "webhook_path", &e.webhook_path)?;
        write_opt_dt(m, "last_event_at", e.last_event_at)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<GitRepoConnection, RepoError> {
        Ok(GitRepoConnection {
            id: read_uuid(m, "id")?,
            provider: read_str(m, "provider")?,
            owner: read_str(m, "owner")?,
            repo: read_str(m, "repo")?,
            default_branch: read_str(m, "default_branch")?,
            project_id: read_opt_uuid(m, "project_id")?,
            webhook_secret_hash: read_str(m, "webhook_secret_hash")?,
            webhook_path: read_str(m, "webhook_path")?,
            last_event_at: read_opt_dt(m, "last_event_at")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: GitRepoConnectionUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.provider {
            write_str(m, "provider", &v)?;
        }
        if let Some(v) = u.owner {
            write_str(m, "owner", &v)?;
        }
        if let Some(v) = u.repo {
            write_str(m, "repo", &v)?;
        }
        if let Some(v) = u.default_branch {
            write_str(m, "default_branch", &v)?;
        }
        if let Some(v) = u.project_id {
            write_opt_uuid(m, "project_id", v)?;
        }
        if let Some(v) = u.webhook_secret_hash {
            write_str(m, "webhook_secret_hash", &v)?;
        }
        if let Some(v) = u.webhook_path {
            write_str(m, "webhook_path", &v)?;
        }
        if let Some(v) = u.last_event_at {
            write_opt_dt(m, "last_event_at", v)?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(
        items: &mut [GitRepoConnection],
        field: &str,
        order: SortOrder,
    ) -> Result<(), RepoError> {
        match field {
            "provider" => items.sort_by(|a, b| a.provider.cmp(&b.provider)),
            "owner" => items.sort_by(|a, b| a.owner.cmp(&b.owner)),
            "repo" => items.sort_by(|a, b| a.repo.cmp(&b.repo)),
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

    fn build_list(items: Vec<GitRepoConnection>, total: u32, page: Page) -> GitRepoConnectionList {
        GitRepoConnectionList { items, total, page }
    }
}

impl GitRepoConnectionRepo for GitRepoConnectionRepoLoro {
    async fn get(&self, id: Uuid) -> Result<GitRepoConnection, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<GitRepoConnectionList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: GitRepoConnectionCreate) -> Result<GitRepoConnection, RepoError> {
        self.inner.create(input).await
    }
    async fn update(
        &self,
        id: Uuid,
        input: GitRepoConnectionUpdate,
    ) -> Result<GitRepoConnection, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}
