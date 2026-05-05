//! Local storage adapter hooks for generated crudcrate repositories.

use crudcrate::{ApiError, ApplyUpdate, CreateResource, ResourceIdentity};
use serde::{Serialize, de::DeserializeOwned};
use uuid::Uuid;

use crate::project::{Project, ProjectApi, ProjectApiCreate, ProjectApiUpdate};
use crate::task::{Task, TaskApi, TaskApiCreate, TaskApiUpdate};

impl ResourceIdentity<Uuid> for TaskApi {
    fn id(&self) -> Uuid {
        self.id
    }
}

impl CreateResource<TaskApiCreate> for TaskApi {
    fn create_from(data: TaskApiCreate) -> Result<Self, ApiError> {
        let mut task: Task = merge_with_default(data, "task create")?;
        if task.id == Uuid::nil() {
            task.id = Uuid::new_v4();
        }
        Ok(Self::from(task))
    }
}

impl ApplyUpdate<TaskApiUpdate> for TaskApi {
    fn apply_update(&mut self, data: TaskApiUpdate) -> Result<(), ApiError> {
        *self = merge_with_existing(self.clone(), data, "task update")?;
        Ok(())
    }
}

impl ResourceIdentity<Uuid> for ProjectApi {
    fn id(&self) -> Uuid {
        self.id
    }
}

impl CreateResource<ProjectApiCreate> for ProjectApi {
    fn create_from(data: ProjectApiCreate) -> Result<Self, ApiError> {
        let mut project: Project = merge_with_default(data, "project create")?;
        if project.id == Uuid::nil() {
            project.id = Uuid::new_v4();
        }
        Ok(Self::from(project))
    }
}

impl ApplyUpdate<ProjectApiUpdate> for ProjectApi {
    fn apply_update(&mut self, data: ProjectApiUpdate) -> Result<(), ApiError> {
        *self = merge_with_existing(self.clone(), data, "project update")?;
        Ok(())
    }
}

fn merge_with_default<T, P>(patch: P, label: &str) -> Result<T, ApiError>
where
    T: Default + DeserializeOwned + Serialize,
    P: Serialize,
{
    let base = T::default();
    merge_with_existing(base, patch, label)
}

fn merge_with_existing<T, P>(existing: T, patch: P, label: &str) -> Result<T, ApiError>
where
    T: DeserializeOwned + Serialize,
    P: Serialize,
{
    let mut base = serde_json::to_value(existing).map_err(|e| {
        ApiError::internal(
            format!("failed to serialize {label} base"),
            Some(e.to_string()),
        )
    })?;
    merge_json_object(
        &mut base,
        serde_json::to_value(patch)
            .map_err(|e| ApiError::bad_request(format!("invalid {label} payload: {e}")))?,
        label,
    )?;
    serde_json::from_value(base)
        .map_err(|e| ApiError::bad_request(format!("invalid {label} payload: {e}")))
}

fn merge_json_object(
    base: &mut serde_json::Value,
    patch: serde_json::Value,
    label: &str,
) -> Result<(), ApiError> {
    let Some(base_obj) = base.as_object_mut() else {
        return Err(ApiError::internal(
            format!("{label} base did not serialize to an object"),
            None,
        ));
    };
    let serde_json::Value::Object(patch_obj) = patch else {
        return Err(ApiError::bad_request(format!(
            "{label} payload must be an object"
        )));
    };
    for (key, value) in patch_obj {
        base_obj.insert(key, value);
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use crudcrate::InMemoryStorage;

    use super::*;
    use crate::project::ProjectRepo;
    use crate::task::{Priority, Status, TaskRepo};

    fn create_model<T, C>(model: T) -> C
    where
        T: Serialize,
        C: DeserializeOwned,
    {
        serde_json::to_value(model)
            .and_then(serde_json::from_value)
            .expect("model should convert into generated create payload")
    }

    fn update_model<T, U>(model: T) -> U
    where
        T: Serialize,
        U: DeserializeOwned,
    {
        serde_json::to_value(model)
            .and_then(serde_json::from_value)
            .expect("model should convert into generated update payload")
    }

    #[tokio::test]
    async fn task_repo_runs_against_in_memory_storage() {
        let repo = crate::task::TaskRepoStorage::new(InMemoryStorage::<TaskApi>::new());
        let created = repo
            .create_task(create_model::<_, TaskApiCreate>(Task {
                title: "In-memory generated repo".to_string(),
                status: Status::Open,
                priority: Priority::High,
                body: "created through generic storage".to_string(),
                ..Task::default()
            }))
            .await
            .expect("task should create");

        assert_ne!(created.id, Uuid::nil());
        assert_eq!(created.title, "In-memory generated repo");

        let updated = repo
            .update_task(
                created.id.to_string(),
                update_model::<_, TaskApiUpdate>(serde_json::json!({
                    "status": Status::Done,
                })),
            )
            .await
            .expect("task should update");
        assert_eq!(updated.status, Status::Done);

        let listed = repo
            .list_tasks(None, None, None, Some(50))
            .await
            .expect("tasks should list");
        assert_eq!(listed.len(), 1);
        assert_eq!(listed[0].id, created.id);
    }

    #[tokio::test]
    async fn project_repo_runs_against_in_memory_storage() {
        let repo = crate::project::ProjectRepoStorage::new(InMemoryStorage::<ProjectApi>::new());
        let created = repo
            .create_project(create_model::<_, ProjectApiCreate>(Project {
                title: "Adapter Project".to_string(),
                description: Some("created through generic storage".to_string()),
                ..Project::default()
            }))
            .await
            .expect("project should create");

        assert_ne!(created.id, Uuid::nil());
        assert_eq!(created.title, "Adapter Project");

        let listed = repo
            .list_projects(None, None, None, Some(50))
            .await
            .expect("projects should list");
        assert_eq!(listed.len(), 1);
        assert_eq!(listed[0].id, created.id);
    }

    #[cfg(feature = "server")]
    #[tokio::test]
    async fn generated_repos_run_against_markdown_vault_storage() {
        use std::sync::Arc;

        use tokio::sync::RwLock;

        use crate::vault::{Vault, VaultStorage};

        let root = std::env::temp_dir().join(format!("task-repo-adapters-{}", Uuid::new_v4()));
        std::fs::create_dir_all(&root).expect("temp vault should be created");
        let storage = VaultStorage::new(Arc::new(RwLock::new(Vault::new(&root))));

        let task_repo = crate::task::TaskRepoStorage::new(storage.clone());
        let task = task_repo
            .create_task(create_model::<_, TaskApiCreate>(Task {
                title: "Markdown generated repo task".to_string(),
                status: Status::Open,
                priority: Priority::Normal,
                body: "created through VaultStorage".to_string(),
                ..Task::default()
            }))
            .await
            .expect("task should create in markdown storage");
        assert_ne!(task.id, Uuid::nil());

        let project_repo = crate::project::ProjectRepoStorage::new(storage);
        let project = project_repo
            .create_project(create_model::<_, ProjectApiCreate>(Project {
                title: "Markdown generated repo project".to_string(),
                description: Some("created through VaultStorage".to_string()),
                ..Project::default()
            }))
            .await
            .expect("project should create in markdown storage");
        assert_ne!(project.id, Uuid::nil());

        let tasks = task_repo
            .list_tasks(None, None, None, Some(50))
            .await
            .expect("tasks should list from markdown storage");
        assert!(tasks.iter().any(|listed| listed.id == task.id));

        let projects = project_repo
            .list_projects(None, None, None, Some(50))
            .await
            .expect("projects should list from markdown storage");
        assert!(projects.iter().any(|listed| listed.id == project.id));

        std::fs::remove_dir_all(&root).expect("temp vault should be removed");
    }
}
