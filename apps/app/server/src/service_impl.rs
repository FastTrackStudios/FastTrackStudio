//! Concrete [`ExampleService`] implementation.
//!
//! `ExampleService` is hand-written domain logic on top of whichever
//! `ExampleRepo` backend the build pulled in (db, memory, …). It owns
//! only operations that aren't pure CRUD: search, duplicate, etc.
//! Plain CRUD comes from `ExampleRepo` directly.

use example::{Example, ExampleRepo, ExampleService, ExampleServiceError};
use uuid::Uuid;

#[derive(Clone)]
pub struct ExampleServiceImpl<R: ExampleRepo + Clone + Send + Sync + 'static> {
    repo: R,
}

impl<R: ExampleRepo + Clone + Send + Sync + 'static> ExampleServiceImpl<R> {
    pub fn new(repo: R) -> Self {
        Self { repo }
    }
}

impl<R: ExampleRepo + Clone + Send + Sync + 'static> ExampleService for ExampleServiceImpl<R> {
    async fn search(
        &self,
        query: String,
        _limit: u32,
    ) -> Result<Vec<Example>, ExampleServiceError> {
        if query.trim().is_empty() {
            return Err(ExampleServiceError::InvalidInput("query is empty".into()));
        }
        let _ = &self.repo;
        Err(ExampleServiceError::Internal(
            "search: FTS5 wiring lands with the derive's fulltext emission".into(),
        ))
    }

    async fn duplicate(
        &self,
        _id: Uuid,
        _new_name: Option<String>,
    ) -> Result<Example, ExampleServiceError> {
        Err(ExampleServiceError::Internal(
            "duplicate: not implemented yet".into(),
        ))
    }
}
