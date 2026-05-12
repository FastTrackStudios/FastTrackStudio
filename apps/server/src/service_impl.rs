//! Concrete [`ExampleService`] implementation.
//!
//! Composes the crudcrate-generated [`ExampleRepo`] for storage, adds
//! validation + domain rules on top. The vox `#[service]` macro on
//! the trait gave us the dispatcher type used by `apps/server`'s
//! WebSocket router.
//!
//! TODO: wire into the vox factory in `main.rs` once
//! `connection.handle_with(ExampleServiceDispatcher::new(...))` is in
//! place. Stubbed today so the trait + impl compile and demonstrate
//! the pattern.

use example_db::ExampleRepoStorage;
use example_proto::{Example, ExampleService, ExampleServiceError};
use sea_orm::DatabaseConnection;
use std::sync::Arc;
use uuid::Uuid;

#[derive(Clone)]
pub struct ExampleServiceImpl {
    repo: Arc<ExampleRepoStorage<DatabaseConnection>>,
}

impl ExampleServiceImpl {
    pub fn new(db: DatabaseConnection) -> Self {
        Self {
            repo: Arc::new(ExampleRepoStorage::new(db)),
        }
    }
}

#[vox::service]
impl ExampleService for ExampleServiceImpl {
    async fn list_examples(&self) -> Result<Vec<Example>, ExampleServiceError> {
        // Repo returns its own ExampleList struct; the service trait
        // promises Vec<Example>. Translate here.
        let _ = &self.repo;
        // TODO: call self.repo.list_example(None, None, None, None).await
        Err(ExampleServiceError::Internal(
            "list_examples: not wired to repo yet".into(),
        ))
    }

    async fn get_example(&self, _id: Uuid) -> Result<Example, ExampleServiceError> {
        Err(ExampleServiceError::Internal(
            "get_example: not wired".into(),
        ))
    }

    async fn create_example(
        &self,
        name: String,
        _description: String,
    ) -> Result<Example, ExampleServiceError> {
        if name.trim().is_empty() {
            return Err(ExampleServiceError::InvalidInput("name is empty".into()));
        }
        Err(ExampleServiceError::Internal(
            "create_example: not wired".into(),
        ))
    }

    async fn rename_example(
        &self,
        _id: Uuid,
        new_name: String,
    ) -> Result<Example, ExampleServiceError> {
        if new_name.trim().is_empty() {
            return Err(ExampleServiceError::InvalidInput("name is empty".into()));
        }
        Err(ExampleServiceError::Internal(
            "rename_example: not wired".into(),
        ))
    }

    async fn delete_example(&self, _id: Uuid) -> Result<(), ExampleServiceError> {
        Err(ExampleServiceError::Internal(
            "delete_example: not wired".into(),
        ))
    }
}
