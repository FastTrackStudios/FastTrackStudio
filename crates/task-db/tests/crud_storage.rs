use crudcrate::{
    ApiError, ApplyUpdate, CreateResource, CrudModel, CrudService, CrudStorage, InMemoryQuery,
    InMemoryStorage, ResourceIdentity,
};
use uuid::Uuid;

#[derive(Clone, Debug, PartialEq, Eq)]
struct MemoryTask {
    id: Uuid,
    title: String,
}

struct MemoryTaskCreate {
    id: Uuid,
    title: String,
}

struct MemoryTaskUpdate {
    title: Option<String>,
}

impl CrudModel for MemoryTask {
    type Id = Uuid;
    type CreateModel = MemoryTaskCreate;
    type UpdateModel = MemoryTaskUpdate;
    type ListModel = MemoryTask;

    const RESOURCE_NAME_SINGULAR: &'static str = "memory task";
    const RESOURCE_NAME_PLURAL: &'static str = "memory tasks";
}

impl ResourceIdentity<Uuid> for MemoryTask {
    fn id(&self) -> Uuid {
        self.id
    }
}

impl CreateResource<MemoryTaskCreate> for MemoryTask {
    fn create_from(data: MemoryTaskCreate) -> Result<Self, ApiError> {
        Ok(Self {
            id: data.id,
            title: data.title,
        })
    }
}

impl ApplyUpdate<MemoryTaskUpdate> for MemoryTask {
    fn apply_update(&mut self, data: MemoryTaskUpdate) -> Result<(), ApiError> {
        if let Some(title) = data.title {
            self.title = title;
        }
        Ok(())
    }
}

#[tokio::test]
async fn crudcrate_storage_can_back_task_resources_without_seaorm() {
    let storage = InMemoryStorage::<MemoryTask>::new();
    let id = Uuid::new_v4();

    let created = CrudService::create(
        &storage,
        MemoryTaskCreate {
            id,
            title: "Draft".to_string(),
        },
    )
    .await
    .expect("create in memory task");
    assert_eq!(created.title, "Draft");

    let updated = CrudService::update(
        &storage,
        id,
        MemoryTaskUpdate {
            title: Some("Ready".to_string()),
        },
    )
    .await
    .expect("update in memory task");
    assert_eq!(updated.title, "Ready");

    let listed = CrudStorage::get_all(&storage, InMemoryQuery::all())
        .await
        .expect("list in memory tasks");
    assert_eq!(listed, vec![updated]);
}
