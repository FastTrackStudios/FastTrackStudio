use serde::{Serialize, de::DeserializeOwned};

use crate::calendar_event::{CalendarEvent, CalendarEventApiList, CalendarEventRepo};
use crate::expense::{
    Expense, ExpenseApiList, ExpenseFilter, ExpenseRepo, ExpenseReport, build_expense_report,
    matches_expense_filter,
};
use crate::project::{
    Project, ProjectApiList, ProjectDashboardEntry, ProjectRepo, ProjectStats,
    next_task as find_next_task, project_dashboard as build_project_dashboard,
};
use crate::query::Query;
use crate::service::{
    CalDavDeleteObjectRequest, CalDavDiscovery, CalDavFreeBusyInterval, CalDavFreeBusyRequest,
    CalDavMultigetRequest, CalDavObject, CalDavPutObjectRequest, CalDavScheduleRequest,
    CalDavScheduleResponse, CalDavSyncCollectionRequest, CalDavSyncCollectionResponse,
    CalendarService, CardDavDeleteObjectRequest, CardDavDiscovery, CardDavMultigetRequest,
    CardDavObject, CardDavPutObjectRequest, CardDavSyncCollectionRequest,
    CardDavSyncCollectionResponse, ExpenseService, ProjectKnowledgeContext, ProjectService,
    RemoteDeckBoard, RemoteDeckStack, SyncPlan, SyncStats, TaskService, VaultError,
};
use crate::task::{Status, Task, TaskApi, TaskApiList, TaskApiUpdate, TaskRepo};

#[derive(Clone)]
pub struct TaskServiceImpl<R> {
    task_repo: R,
}

#[derive(Clone)]
pub struct ProjectServiceImpl<P, T> {
    project_repo: P,
    task_repo: T,
}

#[derive(Clone)]
pub struct ExpenseServiceImpl<R> {
    expense_repo: R,
}

#[derive(Clone)]
pub struct CalendarServiceImpl<T, E> {
    task_repo: T,
    event_repo: E,
}

impl<R> TaskServiceImpl<R> {
    pub fn new(task_repo: R) -> Self {
        Self { task_repo }
    }
}

impl<P, T> ProjectServiceImpl<P, T> {
    pub fn new(project_repo: P, task_repo: T) -> Self {
        Self {
            project_repo,
            task_repo,
        }
    }
}

impl<R> ExpenseServiceImpl<R> {
    pub fn new(expense_repo: R) -> Self {
        Self { expense_repo }
    }
}

impl<T, E> CalendarServiceImpl<T, E> {
    pub fn new(task_repo: T, event_repo: E) -> Self {
        Self {
            task_repo,
            event_repo,
        }
    }
}

impl<R> TaskServiceImpl<R>
where
    R: TaskRepo,
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

    async fn update_task_model(&self, task: &Task) -> Result<Task, VaultError> {
        let update = convert_ref::<Task, TaskApiUpdate>(task)?;
        self.task_repo
            .update_task(task.id.to_string(), update)
            .await
            .map_err(VaultError::ParseError)
            .and_then(convert_model::<TaskApi, Task>)
    }
}

impl<P, T> ProjectServiceImpl<P, T>
where
    P: ProjectRepo,
    T: TaskRepo,
{
    async fn list_project_models(&self) -> Result<Vec<Project>, VaultError> {
        self.project_repo
            .list_projects(None, None, None, Some(10_000))
            .await
            .map_err(VaultError::ParseError)?
            .into_iter()
            .map(convert_model::<ProjectApiList, Project>)
            .collect()
    }

    async fn list_task_models(&self) -> Result<Vec<Task>, VaultError> {
        self.task_repo
            .list_tasks(None, None, None, Some(10_000))
            .await
            .map_err(VaultError::ParseError)?
            .into_iter()
            .map(convert_model::<TaskApiList, Task>)
            .collect()
    }
}

impl<R> ExpenseServiceImpl<R>
where
    R: ExpenseRepo,
{
    async fn list_expense_models(&self) -> Result<Vec<Expense>, VaultError> {
        self.expense_repo
            .list_expenses(None, None, None, Some(10_000))
            .await
            .map_err(VaultError::ParseError)?
            .into_iter()
            .map(convert_model::<ExpenseApiList, Expense>)
            .collect()
    }
}

impl<T, E> CalendarServiceImpl<T, E>
where
    T: TaskRepo,
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

impl<R> TaskService for TaskServiceImpl<R>
where
    R: TaskRepo,
{
    async fn execute_query(&self, query: Query) -> Vec<Task> {
        self.list_task_models()
            .await
            .map(|tasks| query.execute(&tasks).into_iter().cloned().collect())
            .unwrap_or_default()
    }

    async fn urgency_score(&self, task: Task) -> i32 {
        task.urgency_score()
    }

    async fn complete_task(&self, title: String) -> Result<Task, VaultError> {
        let mut task = self
            .list_task_models()
            .await?
            .into_iter()
            .find(|task| task.matches_reference(&title))
            .ok_or_else(|| VaultError::NotFound(title.clone()))?;
        task.status = Status::Done;
        task.completed_date = Some(chrono::Utc::now().date_naive());
        task.date_modified = Some(chrono::Utc::now());
        self.update_task_model(&task).await
    }

    async fn search_tasks(&self, query: String) -> Vec<Task> {
        let needle = query.to_lowercase();
        self.list_task_models()
            .await
            .unwrap_or_default()
            .into_iter()
            .filter(|task| {
                task.title.to_lowercase().contains(&needle)
                    || task.body.to_lowercase().contains(&needle)
            })
            .collect()
    }

    async fn tasks_for_user(&self, username: String) -> Vec<Task> {
        self.list_task_models()
            .await
            .unwrap_or_default()
            .into_iter()
            .filter(|task| {
                task.assignee.as_deref() == Some(username.as_str())
                    || task.assignees.iter().any(|assignee| assignee == &username)
            })
            .collect()
    }
}

impl<P, T> ProjectService for ProjectServiceImpl<P, T>
where
    P: ProjectRepo,
    T: TaskRepo,
{
    async fn project_stats(&self, project_title: String) -> ProjectStats {
        let tasks = self.list_task_models().await.unwrap_or_default();
        let refs: Vec<&Task> = tasks
            .iter()
            .filter(|task| {
                task.projects
                    .iter()
                    .any(|project| project.0 == project_title)
            })
            .collect();
        ProjectStats::from_tasks(&refs)
    }

    async fn project_dashboard(&self) -> Vec<ProjectDashboardEntry> {
        let projects = self.list_project_models().await.unwrap_or_default();
        let tasks = self.list_task_models().await.unwrap_or_default();
        build_project_dashboard(&projects, &tasks)
    }

    async fn next_task(&self, project_title: String) -> Option<Task> {
        let tasks = self.list_task_models().await.unwrap_or_default();
        find_next_task(&project_title, &tasks).cloned()
    }

    async fn tasks_for_project(&self, project_title: String) -> Vec<Task> {
        self.list_task_models()
            .await
            .unwrap_or_default()
            .into_iter()
            .filter(|task| {
                task.projects
                    .iter()
                    .any(|project| project.0 == project_title)
            })
            .collect()
    }

    async fn project_context(
        &self,
        project_title: String,
        _include_files: bool,
        _depth: String,
    ) -> Result<Option<ProjectKnowledgeContext>, VaultError> {
        let project = self
            .list_project_models()
            .await?
            .into_iter()
            .find(|project| project.title.eq_ignore_ascii_case(&project_title));
        let Some(project) = project else {
            return Ok(None);
        };
        let tasks = self.tasks_for_project(project.title.clone()).await;
        let next_action = find_next_task(&project.title, &tasks).cloned();
        let project_path = project
            .dev_path
            .as_deref()
            .filter(|path| {
                !path.trim().is_empty() && !path.starts_with('~') && !path.starts_with('/')
            })
            .map(|path| path.trim_matches('/').to_string())
            .unwrap_or_else(|| project.title.trim_matches('/').to_string());

        Ok(Some(ProjectKnowledgeContext {
            references: project
                .references
                .iter()
                .map(|link| link.0.clone())
                .collect(),
            project,
            project_path,
            tasks,
            next_action,
            files: Vec::new(),
            notes: Vec::new(),
            decisions: Vec::new(),
            deliverables: Vec::new(),
        }))
    }
}

impl<R> ExpenseService for ExpenseServiceImpl<R>
where
    R: ExpenseRepo,
{
    async fn expense_report(&self, filter: ExpenseFilter) -> ExpenseReport {
        let expenses: Vec<Expense> = self
            .list_expense_models()
            .await
            .unwrap_or_default()
            .into_iter()
            .filter(|expense| matches_expense_filter(expense, &filter))
            .collect();
        build_expense_report(&expenses, chrono::Utc::now().date_naive())
    }
}

impl<T, E> CalendarService for CalendarServiceImpl<T, E>
where
    T: TaskRepo,
    E: CalendarEventRepo,
{
    async fn tasks_due_by(&self, date: String) -> Vec<Task> {
        let Ok(due_date) = date.parse::<chrono::NaiveDate>() else {
            return Vec::new();
        };
        self.list_task_models()
            .await
            .unwrap_or_default()
            .into_iter()
            .filter(|task| {
                task.due.map(|due| due <= due_date).unwrap_or(false) && !task.is_complete()
            })
            .collect()
    }

    async fn scheduled_between(&self, from: String, to: String) -> Result<Vec<Task>, VaultError> {
        let from = from.parse::<chrono::NaiveDate>().map_err(|err| {
            VaultError::ParseError(format!("invalid scheduled start date '{from}': {err}"))
        })?;
        let to = to.parse::<chrono::NaiveDate>().map_err(|err| {
            VaultError::ParseError(format!("invalid scheduled end date '{to}': {err}"))
        })?;
        Ok(self
            .list_task_models()
            .await?
            .into_iter()
            .filter(|task| {
                task.scheduled
                    .map(|scheduled| scheduled >= from && scheduled <= to)
                    .unwrap_or(false)
            })
            .collect())
    }

    async fn events_between(
        &self,
        from: String,
        to: String,
    ) -> Result<Vec<CalendarEvent>, VaultError> {
        let from = chrono::DateTime::parse_from_rfc3339(&from)
            .map_err(|err| VaultError::ParseError(format!("invalid event start '{from}': {err}")))?
            .with_timezone(&chrono::Utc);
        let to = chrono::DateTime::parse_from_rfc3339(&to)
            .map_err(|err| VaultError::ParseError(format!("invalid event end '{to}': {err}")))?
            .with_timezone(&chrono::Utc);
        Ok(self
            .list_event_models()
            .await?
            .into_iter()
            .filter(|event| {
                let event_end = event.end.unwrap_or(event.start);
                event_end >= from && event.start <= to
            })
            .collect())
    }

    async fn trigger_sync(&self) -> Result<SyncStats, VaultError> {
        Err(provider_not_configured("calendar sync"))
    }

    async fn sync_status(&self) -> Option<SyncStats> {
        None
    }

    async fn sync_plan(&self) -> SyncPlan {
        SyncPlan {
            generated_at: chrono::Utc::now().to_rfc3339(),
            safe_to_run: false,
            warnings: vec![
                "provider adapters are not configured for sqlite-only CalendarService".into(),
            ],
            ..SyncPlan::default()
        }
    }

    async fn discover_caldav(&self) -> Result<CalDavDiscovery, VaultError> {
        Err(provider_not_configured("CalDAV discovery"))
    }

    async fn discover_carddav(&self) -> Result<CardDavDiscovery, VaultError> {
        Err(provider_not_configured("CardDAV discovery"))
    }

    async fn calendar_multiget(
        &self,
        _request: CalDavMultigetRequest,
    ) -> Result<Vec<CalDavObject>, VaultError> {
        Err(provider_not_configured("CalDAV multiget"))
    }

    async fn calendar_sync_collection(
        &self,
        _request: CalDavSyncCollectionRequest,
    ) -> Result<CalDavSyncCollectionResponse, VaultError> {
        Err(provider_not_configured("CalDAV sync collection"))
    }

    async fn addressbook_multiget(
        &self,
        _request: CardDavMultigetRequest,
    ) -> Result<Vec<CardDavObject>, VaultError> {
        Err(provider_not_configured("CardDAV multiget"))
    }

    async fn addressbook_sync_collection(
        &self,
        _request: CardDavSyncCollectionRequest,
    ) -> Result<CardDavSyncCollectionResponse, VaultError> {
        Err(provider_not_configured("CardDAV sync collection"))
    }

    async fn put_calendar_object(
        &self,
        _request: CalDavPutObjectRequest,
    ) -> Result<(), VaultError> {
        Err(provider_not_configured("CalDAV put object"))
    }

    async fn delete_calendar_object(
        &self,
        _request: CalDavDeleteObjectRequest,
    ) -> Result<(), VaultError> {
        Err(provider_not_configured("CalDAV delete object"))
    }

    async fn put_addressbook_object(
        &self,
        _request: CardDavPutObjectRequest,
    ) -> Result<(), VaultError> {
        Err(provider_not_configured("CardDAV put object"))
    }

    async fn delete_addressbook_object(
        &self,
        _request: CardDavDeleteObjectRequest,
    ) -> Result<(), VaultError> {
        Err(provider_not_configured("CardDAV delete object"))
    }

    async fn send_calendar_schedule(
        &self,
        _request: CalDavScheduleRequest,
    ) -> Result<CalDavScheduleResponse, VaultError> {
        Err(provider_not_configured("CalDAV schedule"))
    }

    async fn calendar_free_busy(
        &self,
        _request: CalDavFreeBusyRequest,
    ) -> Result<Vec<CalDavFreeBusyInterval>, VaultError> {
        Err(provider_not_configured("CalDAV free busy"))
    }

    async fn list_deck_boards(&self) -> Result<Vec<RemoteDeckBoard>, VaultError> {
        Err(provider_not_configured("Deck boards"))
    }

    async fn list_deck_stacks(&self, _board_id: u64) -> Result<Vec<RemoteDeckStack>, VaultError> {
        Err(provider_not_configured("Deck stacks"))
    }
}

fn provider_not_configured(operation: &str) -> VaultError {
    VaultError::IoError(format!(
        "{operation} requires a provider adapter; sqlite repositories only provide local read/query behavior"
    ))
}

fn convert_ref<T, U>(value: &T) -> Result<U, VaultError>
where
    T: Serialize,
    U: DeserializeOwned,
{
    serde_json::from_value(
        serde_json::to_value(value).map_err(|err| {
            VaultError::ParseError(format!("failed to serialize repo model: {err}"))
        })?,
    )
    .map_err(|err| VaultError::ParseError(format!("failed to deserialize repo model: {err}")))
}

fn convert_model<T, U>(value: T) -> Result<U, VaultError>
where
    T: Serialize,
    U: DeserializeOwned,
{
    serde_json::from_value(
        serde_json::to_value(value).map_err(|err| {
            VaultError::ParseError(format!("failed to serialize repo model: {err}"))
        })?,
    )
    .map_err(|err| VaultError::ParseError(format!("failed to deserialize repo model: {err}")))
}
