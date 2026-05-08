//! SeaORM-backed [`TemplateService`] implementation.
//!
//! Materializes `ProjectTemplate` entries (declared inside `Integration`
//! rows) into real `Project` + `Task` rows. Deterministic UUIDs derived
//! from `(integration_name, template_name, project_name)` keep
//! materialization idempotent — re-running the same request returns the
//! same project + task ids without duplicating rows.

use sea_orm::{ActiveModelTrait, ActiveValue::Set, ColumnTrait, EntityTrait, QueryFilter};
use uuid::Uuid;

use crate::integration::{self, Integration, ProjectTemplate, TaskTemplate};
use crate::project::{self, ProjectStatus};
use crate::property::JsonObject;
use crate::service::{
    MaterializeRequest, MaterializeResult, TemplateService, TemplateView, VaultError,
};
use crate::task::{
    self, EmailRefList, Priority, RecurrenceAnchor, ReminderList, Status, StringList,
    TaskDependencyList, TaskRelationList, TimeEntryList, WikiLinkList,
};

/// Stable namespace for template-derived ids. Unrelated to demo seed
/// namespace — these ids belong to user-materialized projects, not
/// fixture data.
const TEMPLATE_NAMESPACE: Uuid = Uuid::from_u128(0x7e_47_70_18_7e_47_70_18_7e_47_70_18_7e_47_70_18);

fn template_id(integration: &str, template: &str, project: &str, suffix: &str) -> Uuid {
    let key = format!("{integration}/{template}/{project}/{suffix}");
    Uuid::new_v5(&TEMPLATE_NAMESPACE, key.as_bytes())
}

/// Typed dependencies for [`TemplateServiceImpl`].
pub struct TemplateServiceDeps {
    pub db: sea_orm::DatabaseConnection,
}

#[derive(Clone)]
pub struct TemplateServiceImpl {
    db: sea_orm::DatabaseConnection,
}

impl TemplateServiceImpl {
    pub fn new(deps: TemplateServiceDeps) -> Self {
        Self { db: deps.db }
    }
}

fn template_to_view(integration: &Integration, tpl: &ProjectTemplate) -> TemplateView {
    TemplateView {
        integration_name: integration.name.clone(),
        name: tpl.name.clone(),
        description: tpl.description.clone(),
        task_templates_json: serde_json::to_string(&tpl.tasks).unwrap_or_else(|_| "[]".to_string()),
    }
}

impl TemplateService for TemplateServiceImpl {
    async fn list_templates(&self) -> Result<Vec<TemplateView>, VaultError> {
        let integrations = integration::Entity::find()
            .all(&self.db)
            .await
            .map_err(|err| VaultError::ParseError(format!("integration list: {err}")))?;
        let mut out = Vec::new();
        for integ in &integrations {
            for tpl in integ.project_templates.iter() {
                out.push(template_to_view(integ, tpl));
            }
        }
        Ok(out)
    }

    async fn get_template(
        &self,
        integration_name: String,
        template_name: String,
    ) -> Result<Option<TemplateView>, VaultError> {
        let integ = integration::Entity::find()
            .filter(integration::Column::Name.eq(integration_name))
            .one(&self.db)
            .await
            .map_err(|err| VaultError::ParseError(format!("integration get: {err}")))?;
        let Some(integ) = integ else {
            return Ok(None);
        };
        Ok(integ
            .project_templates
            .iter()
            .find(|t| t.name == template_name)
            .map(|t| template_to_view(&integ, t)))
    }

    async fn materialize(
        &self,
        request: MaterializeRequest,
    ) -> Result<MaterializeResult, VaultError> {
        let integ = integration::Entity::find()
            .filter(integration::Column::Name.eq(request.integration_name.clone()))
            .one(&self.db)
            .await
            .map_err(|err| VaultError::ParseError(format!("integration lookup: {err}")))?
            .ok_or_else(|| {
                VaultError::NotFound(format!(
                    "integration '{}' not registered",
                    request.integration_name
                ))
            })?;

        let tpl = integ
            .project_templates
            .iter()
            .find(|t| t.name == request.template_name)
            .cloned()
            .ok_or_else(|| {
                VaultError::NotFound(format!(
                    "integration '{}' has no template '{}'",
                    request.integration_name, request.template_name
                ))
            })?;

        let project_id = template_id(
            &request.integration_name,
            &request.template_name,
            &request.project_name,
            "project",
        );

        let area = request
            .area
            .clone()
            .or_else(|| integ.area_conventions.iter().next().cloned());

        // Idempotent project upsert.
        let existing = project::Entity::find_by_id(project_id)
            .one(&self.db)
            .await
            .map_err(|err| VaultError::ParseError(format!("project lookup: {err}")))?;
        let created_project = existing.is_none();
        if created_project {
            let active = project_base(
                project_id,
                &request.project_name,
                Some(request.integration_name.clone()),
                area,
                request.organization.clone(),
                request.lead.clone(),
            );
            active
                .insert(&self.db)
                .await
                .map_err(|err| VaultError::ParseError(format!("project insert: {err}")))?;
        }

        // Tasks: deterministic id per template task.
        let mut task_ids = Vec::with_capacity(tpl.tasks.len());
        let mut created_task_count: u32 = 0;
        let mut unchanged_task_count: u32 = 0;
        let now = chrono::Utc::now();
        for (idx, t) in tpl.tasks.iter().enumerate() {
            let task_id = template_id(
                &request.integration_name,
                &request.template_name,
                &request.project_name,
                &format!("task:{idx:03}:{}", t.title),
            );
            task_ids.push(task_id);
            if task::Entity::find_by_id(task_id)
                .one(&self.db)
                .await
                .map_err(|err| VaultError::ParseError(format!("task lookup: {err}")))?
                .is_some()
            {
                unchanged_task_count += 1;
                continue;
            }
            let active = task_from_template(task_id, &request.project_name, t, now);
            task::Entity::insert(active)
                .exec(&self.db)
                .await
                .map_err(|err| VaultError::ParseError(format!("task insert: {err}")))?;
            created_task_count += 1;
        }

        Ok(MaterializeResult {
            project_id,
            project_title: request.project_name,
            task_ids,
            created_project,
            created_task_count,
            unchanged_task_count,
        })
    }
}

fn project_base(
    id: Uuid,
    title: &str,
    project_type: Option<String>,
    area: Option<String>,
    organization: Option<String>,
    lead: Option<String>,
) -> project::ActiveModel {
    project::ActiveModel {
        id: Set(id),
        title: Set(title.to_string()),
        status: Set(ProjectStatus::Active),
        project_type: Set(project_type),
        area: Set(area),
        organization: Set(organization),
        lead: Set(lead),
        up: Set(WikiLinkList::default()),
        tags: Set(StringList::default()),
        team: Set(StringList::default()),
        references: Set(WikiLinkList::default()),
        favorited_by: Set(StringList::default()),
        email_tags: Set(StringList::default()),
        emails: Set(EmailRefList::default()),
        properties: Set(JsonObject::default()),
        ..Default::default()
    }
}

fn task_from_template(
    id: Uuid,
    project_title: &str,
    tpl: &TaskTemplate,
    now: chrono::DateTime<chrono::Utc>,
) -> task::ActiveModel {
    let priority = tpl
        .priority
        .as_deref()
        .and_then(parse_priority)
        .unwrap_or(Priority::Normal);
    let status = tpl
        .status
        .as_deref()
        .and_then(parse_status)
        .unwrap_or(Status::Open);

    task::ActiveModel {
        id: Set(id),
        title: Set(tpl.title.clone()),
        priority: Set(priority),
        status: Set(status),
        projects: Set(WikiLinkList(vec![crate::task::WikiLink(
            project_title.to_string(),
        )])),
        contexts: Set(StringList(tpl.contexts.iter().cloned().collect())),
        tags: Set(StringList(tpl.tags.iter().cloned().collect())),
        areas: Set(WikiLinkList::default()),
        time_entries: Set(TimeEntryList::default()),
        completed_instances: Set(StringList::default()),
        skipped_instances: Set(StringList::default()),
        blocked_by: Set(TaskDependencyList::default()),
        blocking: Set(WikiLinkList::default()),
        reminders: Set(ReminderList::default()),
        assignees: Set(StringList::default()),
        relations: Set(TaskRelationList::default()),
        subscribers: Set(StringList::default()),
        reactions: Set(Default::default()),
        is_draft: Set(false),
        email_tags: Set(StringList::default()),
        emails: Set(EmailRefList::default()),
        recurrence: Set(tpl.recurrence.clone()),
        recurrence_anchor: Set(RecurrenceAnchor::Scheduled),
        time_estimate: Set(tpl.time_estimate_minutes),
        body: Set(tpl.body.clone().unwrap_or_default()),
        properties: Set(JsonObject::default()),
        date_created: Set(Some(now)),
        date_modified: Set(Some(now)),
        ..Default::default()
    }
}

fn parse_priority(s: &str) -> Option<Priority> {
    match s.to_ascii_lowercase().as_str() {
        "none" => Some(Priority::None),
        "low" => Some(Priority::Low),
        "normal" | "" => Some(Priority::Normal),
        "high" => Some(Priority::High),
        "urgent" => Some(Priority::Urgent),
        _ => None,
    }
}

fn parse_status(s: &str) -> Option<Status> {
    // Tasks that template-author-statuses don't map to; fall back to Open.
    // The integration's own status set is *advisory* — we still need a
    // canonical Task::status enum value for the row.
    match s.to_ascii_lowercase().as_str() {
        "open" | "" => Some(Status::Open),
        "in-progress" | "in_progress" | "inprogress" => Some(Status::InProgress),
        "done" | "complete" | "completed" | "approved" | "cooked" | "final" => Some(Status::Done),
        "cancelled" | "canceled" | "skipped" => Some(Status::Cancelled),
        "on-hold" | "on_hold" | "onhold" | "blocked" => Some(Status::OnHold),
        _ => Some(Status::Open),
    }
}
