//! SeaORM-backed [`ProjectTypeService`] implementation.
//!
//! Binds `Project::project_type` (free-form string) to an `Integration`
//! row whose `name` matches. Status sets, task templates, and project
//! templates live on the integration; this service is the lookup +
//! registration surface.
//!
//! Uses raw `IntegrationRepo` + `ProjectRepo` rather than a special
//! cross-entity dispatcher — both repos are crudcrate-generated and
//! Vox-RPC-friendly.

use sea_orm::{ActiveModelTrait, ActiveValue, ColumnTrait, EntityTrait, QueryFilter};
use uuid::Uuid;

use crate::integration::{self, Integration};
use crate::project;
use crate::service::{ProjectTypeService, ProjectTypeSpec, ProjectTypeView, VaultError};

/// Typed dependencies for [`ProjectTypeServiceImpl`].
pub struct ProjectTypeServiceDeps {
    pub db: sea_orm::DatabaseConnection,
}

#[derive(Clone)]
pub struct ProjectTypeServiceImpl {
    db: sea_orm::DatabaseConnection,
}

impl ProjectTypeServiceImpl {
    pub fn new(deps: ProjectTypeServiceDeps) -> Self {
        Self { db: deps.db }
    }
}

fn integration_to_view(row: Integration) -> Result<ProjectTypeView, VaultError> {
    let statuses_json = serde_json::to_string(&row.statuses)
        .map_err(|err| VaultError::ParseError(format!("encode statuses: {err}")))?;
    let task_templates_json = serde_json::to_string(&row.task_templates)
        .map_err(|err| VaultError::ParseError(format!("encode task_templates: {err}")))?;
    let project_templates_json = serde_json::to_string(&row.project_templates)
        .map_err(|err| VaultError::ParseError(format!("encode project_templates: {err}")))?;
    Ok(ProjectTypeView {
        id: row.id,
        name: row.name,
        statuses_json,
        task_templates_json,
        project_templates_json,
        area_conventions: row.area_conventions.0,
        context_conventions: row.context_conventions.0,
    })
}

impl ProjectTypeService for ProjectTypeServiceImpl {
    async fn list_types(&self) -> Result<Vec<ProjectTypeView>, VaultError> {
        let rows = integration::Entity::find()
            .all(&self.db)
            .await
            .map_err(|err| VaultError::ParseError(format!("integration list: {err}")))?;
        rows.into_iter().map(integration_to_view).collect()
    }

    async fn get_type(&self, name: String) -> Result<Option<ProjectTypeView>, VaultError> {
        let row = integration::Entity::find()
            .filter(integration::Column::Name.eq(name))
            .one(&self.db)
            .await
            .map_err(|err| VaultError::ParseError(format!("integration get: {err}")))?;
        row.map(integration_to_view).transpose()
    }

    async fn register_type(&self, spec: ProjectTypeSpec) -> Result<ProjectTypeView, VaultError> {
        let statuses: integration::StatusDefList = serde_json::from_str(&spec.statuses_json)
            .map_err(|err| VaultError::ParseError(format!("decode statuses_json: {err}")))?;
        let task_templates: integration::TaskTemplateList =
            serde_json::from_str(&spec.task_templates_json).map_err(|err| {
                VaultError::ParseError(format!("decode task_templates_json: {err}"))
            })?;
        let project_templates: integration::ProjectTemplateList =
            if spec.project_templates_json.trim().is_empty() {
                integration::ProjectTemplateList::default()
            } else {
                serde_json::from_str(&spec.project_templates_json).map_err(|err| {
                    VaultError::ParseError(format!("decode project_templates_json: {err}"))
                })?
            };

        // Upsert: match by name. Find existing first.
        let existing = integration::Entity::find()
            .filter(integration::Column::Name.eq(spec.name.clone()))
            .one(&self.db)
            .await
            .map_err(|err| VaultError::ParseError(format!("integration upsert lookup: {err}")))?;

        let saved = if let Some(row) = existing {
            let mut active: integration::ActiveModel = row.into();
            active.statuses = ActiveValue::Set(statuses);
            active.task_templates = ActiveValue::Set(task_templates);
            active.project_templates = ActiveValue::Set(project_templates);
            active.area_conventions =
                ActiveValue::Set(integration::IntegrationStringList(spec.area_conventions));
            active.context_conventions =
                ActiveValue::Set(integration::IntegrationStringList(spec.context_conventions));
            active
                .update(&self.db)
                .await
                .map_err(|err| VaultError::ParseError(format!("integration update: {err}")))?
        } else {
            let active = integration::ActiveModel {
                id: ActiveValue::Set(Uuid::new_v4()),
                name: ActiveValue::Set(spec.name.clone()),
                statuses: ActiveValue::Set(statuses),
                task_templates: ActiveValue::Set(task_templates),
                project_templates: ActiveValue::Set(project_templates),
                area_conventions: ActiveValue::Set(integration::IntegrationStringList(
                    spec.area_conventions,
                )),
                context_conventions: ActiveValue::Set(integration::IntegrationStringList(
                    spec.context_conventions,
                )),
            };
            active
                .insert(&self.db)
                .await
                .map_err(|err| VaultError::ParseError(format!("integration insert: {err}")))?
        };

        integration_to_view(saved)
    }

    async fn delete_type(&self, name: String) -> Result<(), VaultError> {
        integration::Entity::delete_many()
            .filter(integration::Column::Name.eq(name))
            .exec(&self.db)
            .await
            .map_err(|err| VaultError::ParseError(format!("integration delete: {err}")))?;
        Ok(())
    }

    async fn get_active_for_project(
        &self,
        project_id: Uuid,
    ) -> Result<Option<ProjectTypeView>, VaultError> {
        let project = project::Entity::find_by_id(project_id)
            .one(&self.db)
            .await
            .map_err(|err| VaultError::ParseError(format!("project lookup: {err}")))?;
        let Some(project) = project else {
            return Ok(None);
        };
        let Some(type_name) = project.project_type else {
            return Ok(None);
        };
        self.get_type(type_name).await
    }
}
