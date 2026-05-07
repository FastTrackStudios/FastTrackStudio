//! SeaORM-backed `OperatingService` implementation.

use crate::calendar_event::{CalendarEvent, CalendarEventApiList, CalendarEventRepo};
use crate::project::{Project, ProjectApiList, ProjectRepo};
use crate::service::{OperatingModelReport, OperatingService, VaultError};
use crate::task::{Task, TaskApiList, TaskRepo};

use super::helpers::{build_operating_model_report, convert_model};

/// Typed requirements for [`OperatingServiceImpl`].
pub struct OperatingServiceDeps<T, P, E> {
    pub task_repo: T,
    pub project_repo: P,
    pub event_repo: E,
}

#[derive(Clone)]
pub struct OperatingServiceImpl<T, P, E> {
    task_repo: T,
    project_repo: P,
    event_repo: E,
}

impl<T, P, E> OperatingServiceImpl<T, P, E> {
    pub fn new(deps: OperatingServiceDeps<T, P, E>) -> Self {
        Self {
            task_repo: deps.task_repo,
            project_repo: deps.project_repo,
            event_repo: deps.event_repo,
        }
    }
}

impl<T, P, E> OperatingServiceImpl<T, P, E>
where
    T: TaskRepo,
    P: ProjectRepo,
    E: CalendarEventRepo,
{
    async fn list_task_models(&self) -> Result<Vec<Task>, VaultError> {
        self.task_repo
            .list_tasks(None, None, None, Some(10_000))
            .await
            .map_err(VaultError::ParseError)?
            .into_iter()
            .map(convert_model::<TaskApiList, Task>)
            .collect()
    }

    async fn list_project_models(&self) -> Result<Vec<Project>, VaultError> {
        self.project_repo
            .list_projects(None, None, None, Some(10_000))
            .await
            .map_err(VaultError::ParseError)?
            .into_iter()
            .map(convert_model::<ProjectApiList, Project>)
            .collect()
    }

    async fn list_event_models(&self) -> Result<Vec<CalendarEvent>, VaultError> {
        self.event_repo
            .list_calendar_events(None, None, None, Some(10_000))
            .await
            .map_err(VaultError::ParseError)?
            .into_iter()
            .map(convert_model::<CalendarEventApiList, CalendarEvent>)
            .collect()
    }
}

impl<T, P, E> OperatingService for OperatingServiceImpl<T, P, E>
where
    T: TaskRepo,
    P: ProjectRepo,
    E: CalendarEventRepo,
{
    async fn operating_model(&self) -> OperatingModelReport {
        let tasks = self.list_task_models().await.unwrap_or_default();
        let projects = self.list_project_models().await.unwrap_or_default();
        let events = self.list_event_models().await.unwrap_or_default();
        build_operating_model_report(tasks, projects, events)
    }
}
