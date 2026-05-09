//! SeaORM-backed [`FitnessService`] implementation.
//!
//! Handles the Exercise catalog and Routine + RoutineExercise template
//! tables. Slug uniqueness is per-`(organization, slug)`; collisions
//! are resolved by appending `-2`, `-3`, …

use chrono::Utc;
use sea_orm::{
    ActiveModelTrait, ColumnTrait, DatabaseConnection, EntityTrait, QueryFilter, QueryOrder, Set,
    TransactionTrait,
};
use uuid::Uuid;

use crate::exercise::{self, ExerciseAliasList, ExerciseApi, ExerciseModality, ExerciseMuscleList};
use crate::property::JsonObject;
use crate::routine::{self, RoutineApi, RoutineTagList};
use crate::routine_exercise::{self, RoutineExerciseApi};
use crate::service::{
    AddRoutineExerciseRequest, CreateExerciseRequest, CreateRoutineRequest, ExercisePatch,
    FitnessService, RoutineWithExercisesView, VaultError,
};

use super::helpers::convert_model;

/// Typed dependencies for [`FitnessServiceImpl`].
pub struct FitnessServiceDeps {
    pub db: DatabaseConnection,
}

#[derive(Clone)]
pub struct FitnessServiceImpl {
    db: DatabaseConnection,
}

impl FitnessServiceImpl {
    pub fn new(deps: FitnessServiceDeps) -> Self {
        Self { db: deps.db }
    }
}

fn io(err: impl std::fmt::Display, ctx: &str) -> VaultError {
    VaultError::IoError(format!("{ctx}: {err}"))
}

fn parse(err: impl std::fmt::Display, ctx: &str) -> VaultError {
    VaultError::ParseError(format!("{ctx}: {err}"))
}

fn to_slug(name: &str, fallback: &str) -> String {
    let s = slug::slugify(name);
    if s.is_empty() {
        fallback.to_string()
    } else {
        s
    }
}

async fn unique_exercise_slug(
    db: &DatabaseConnection,
    base: &str,
    organization: Option<&str>,
) -> Result<String, VaultError> {
    let mut candidate = base.to_string();
    let mut suffix = 2u32;
    loop {
        let mut q = exercise::Entity::find().filter(exercise::Column::Slug.eq(candidate.clone()));
        q = match organization {
            Some(org) => q.filter(exercise::Column::Organization.eq(org)),
            None => q.filter(exercise::Column::Organization.is_null()),
        };
        let collision = q
            .one(db)
            .await
            .map_err(|e| io(e, "exercise slug uniqueness check"))?
            .is_some();
        if !collision {
            return Ok(candidate);
        }
        candidate = format!("{base}-{suffix}");
        suffix += 1;
        if suffix > 1000 {
            return Err(VaultError::IoError(format!(
                "exhausted slug suffixes for base {base}"
            )));
        }
    }
}

async fn unique_routine_slug(
    db: &DatabaseConnection,
    base: &str,
    organization: Option<&str>,
) -> Result<String, VaultError> {
    let mut candidate = base.to_string();
    let mut suffix = 2u32;
    loop {
        let mut q = routine::Entity::find().filter(routine::Column::Slug.eq(candidate.clone()));
        q = match organization {
            Some(org) => q.filter(routine::Column::Organization.eq(org)),
            None => q.filter(routine::Column::Organization.is_null()),
        };
        let collision = q
            .one(db)
            .await
            .map_err(|e| io(e, "routine slug uniqueness check"))?
            .is_some();
        if !collision {
            return Ok(candidate);
        }
        candidate = format!("{base}-{suffix}");
        suffix += 1;
        if suffix > 1000 {
            return Err(VaultError::IoError(format!(
                "exhausted slug suffixes for base {base}"
            )));
        }
    }
}

fn exercise_to_api(model: exercise::Model) -> Result<ExerciseApi, VaultError> {
    convert_model::<exercise::Model, ExerciseApi>(model)
}

fn routine_to_api(model: routine::Model) -> Result<RoutineApi, VaultError> {
    convert_model::<routine::Model, RoutineApi>(model)
}

fn routine_exercise_to_api(
    model: routine_exercise::Model,
) -> Result<RoutineExerciseApi, VaultError> {
    convert_model::<routine_exercise::Model, RoutineExerciseApi>(model)
}

fn parse_modality(s: &str) -> Result<ExerciseModality, VaultError> {
    ExerciseModality::parse(s)
        .ok_or_else(|| VaultError::ParseError(format!("unknown modality '{s}'")))
}

impl FitnessService for FitnessServiceImpl {
    // ── Exercises ─────────────────────────────────────────────

    async fn list_exercises(
        &self,
        organization: Option<String>,
        modality: Option<String>,
        primary_muscle: Option<String>,
    ) -> Result<Vec<ExerciseApi>, VaultError> {
        let mut q = exercise::Entity::find().order_by_asc(exercise::Column::Name);
        if let Some(org) = organization {
            q = q.filter(exercise::Column::Organization.eq(org));
        }
        if let Some(modality) = modality {
            let m = parse_modality(&modality)?;
            q = q.filter(exercise::Column::Modality.eq(m));
        }
        if let Some(muscle) = primary_muscle {
            q = q.filter(exercise::Column::PrimaryMuscle.eq(muscle));
        }
        let rows = q.all(&self.db).await.map_err(|e| io(e, "list_exercises"))?;
        rows.into_iter().map(exercise_to_api).collect()
    }

    async fn get_exercise(&self, id: Uuid) -> Result<Option<ExerciseApi>, VaultError> {
        let row = exercise::Entity::find_by_id(id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "get_exercise"))?;
        row.map(exercise_to_api).transpose()
    }

    async fn find_exercise_by_slug_or_alias(
        &self,
        organization: Option<String>,
        slug_or_alias: String,
    ) -> Result<Option<ExerciseApi>, VaultError> {
        let hit = crate::exercise::find_exercise_by_slug_or_alias(
            &self.db,
            organization.as_deref(),
            &slug_or_alias,
        )
        .await
        .map_err(|e| io(e, "find_exercise_by_slug_or_alias"))?;
        hit.map(exercise_to_api).transpose()
    }

    async fn create_exercise(
        &self,
        request: CreateExerciseRequest,
    ) -> Result<ExerciseApi, VaultError> {
        if request.name.trim().is_empty() {
            return Err(VaultError::ParseError("exercise name is empty".into()));
        }
        let modality = parse_modality(&request.modality)?;
        let base_slug = match request.slug.as_deref() {
            Some(s) if !s.trim().is_empty() => to_slug(s, "exercise"),
            _ => to_slug(&request.name, "exercise"),
        };
        let slug =
            unique_exercise_slug(&self.db, &base_slug, request.organization.as_deref()).await?;
        let now = Utc::now();
        let id = Uuid::new_v4();
        let active = exercise::ActiveModel {
            id: Set(id),
            name: Set(request.name.clone()),
            slug: Set(slug),
            aliases: Set(ExerciseAliasList::from(request.aliases)),
            modality: Set(modality),
            primary_muscle: Set(request.primary_muscle),
            secondary_muscles: Set(ExerciseMuscleList::from(request.secondary_muscles)),
            equipment: Set(request.equipment),
            body_markdown: Set(request.body_markdown.unwrap_or_default()),
            media_url: Set(request.media_url),
            organization: Set(request.organization),
            created_by: Set(request.created_by),
            properties: Set(JsonObject::default()),
            created_at: Set(now),
            updated_at: Set(now),
        };
        let saved = active
            .insert(&self.db)
            .await
            .map_err(|e| io(e, "insert exercise"))?;
        exercise_to_api(saved)
    }

    async fn update_exercise(
        &self,
        id: Uuid,
        patch: ExercisePatch,
    ) -> Result<ExerciseApi, VaultError> {
        let model = exercise::Entity::find_by_id(id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load exercise"))?
            .ok_or_else(|| VaultError::NotFound(format!("exercise:{id}")))?;
        let organization = model.organization.clone();
        let mut active: exercise::ActiveModel = model.into();
        if let Some(name) = patch.name {
            active.name = Set(name);
        }
        if let Some(slug) = patch.slug {
            let base = to_slug(&slug, "exercise");
            let resolved = unique_exercise_slug(&self.db, &base, organization.as_deref()).await?;
            active.slug = Set(resolved);
        }
        if let Some(aliases) = patch.aliases {
            active.aliases = Set(ExerciseAliasList::from(aliases));
        }
        if let Some(modality) = patch.modality {
            active.modality = Set(parse_modality(&modality)?);
        }
        if let Some(muscle) = patch.primary_muscle {
            active.primary_muscle = Set(if muscle.trim().is_empty() {
                None
            } else {
                Some(muscle)
            });
        }
        if let Some(secondary) = patch.secondary_muscles {
            active.secondary_muscles = Set(ExerciseMuscleList::from(secondary));
        }
        if let Some(equipment) = patch.equipment {
            active.equipment = Set(if equipment.trim().is_empty() {
                None
            } else {
                Some(equipment)
            });
        }
        if let Some(body) = patch.body_markdown {
            active.body_markdown = Set(body);
        }
        if let Some(media) = patch.media_url {
            active.media_url = Set(if media.trim().is_empty() {
                None
            } else {
                Some(media)
            });
        }
        active.updated_at = Set(Utc::now());
        let saved = active
            .update(&self.db)
            .await
            .map_err(|e| io(e, "update exercise"))?;
        exercise_to_api(saved)
    }

    async fn delete_exercise(&self, id: Uuid) -> Result<(), VaultError> {
        exercise::Entity::delete_by_id(id)
            .exec(&self.db)
            .await
            .map_err(|e| io(e, "delete exercise"))?;
        Ok(())
    }

    // ── Routines ─────────────────────────────────────────────

    async fn list_routines(
        &self,
        organization: Option<String>,
        category: Option<String>,
    ) -> Result<Vec<RoutineApi>, VaultError> {
        let mut q = routine::Entity::find().order_by_asc(routine::Column::Name);
        if let Some(org) = organization {
            q = q.filter(routine::Column::Organization.eq(org));
        }
        if let Some(cat) = category {
            q = q.filter(routine::Column::Category.eq(cat));
        }
        let rows = q.all(&self.db).await.map_err(|e| io(e, "list_routines"))?;
        rows.into_iter().map(routine_to_api).collect()
    }

    async fn get_routine_with_exercises(
        &self,
        id: Uuid,
    ) -> Result<Option<RoutineWithExercisesView>, VaultError> {
        let Some(model) = routine::Entity::find_by_id(id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "get_routine"))?
        else {
            return Ok(None);
        };
        let exercises = routine_exercise::Entity::find()
            .filter(routine_exercise::Column::RoutineId.eq(id))
            .order_by_asc(routine_exercise::Column::Position)
            .all(&self.db)
            .await
            .map_err(|e| io(e, "load routine exercises"))?;
        let exercise_apis: Vec<RoutineExerciseApi> = exercises
            .into_iter()
            .map(routine_exercise_to_api)
            .collect::<Result<Vec<_>, _>>()?;
        let exercises_json = serde_json::to_string(&exercise_apis)
            .map_err(|e| io(e, "serialize routine exercises"))?;
        Ok(Some(RoutineWithExercisesView {
            routine: routine_to_api(model)?,
            exercises_json,
        }))
    }

    async fn create_routine(
        &self,
        request: CreateRoutineRequest,
    ) -> Result<RoutineApi, VaultError> {
        if request.name.trim().is_empty() {
            return Err(VaultError::ParseError("routine name is empty".into()));
        }
        let base_slug = match request.slug.as_deref() {
            Some(s) if !s.trim().is_empty() => to_slug(s, "routine"),
            _ => to_slug(&request.name, "routine"),
        };
        let slug =
            unique_routine_slug(&self.db, &base_slug, request.organization.as_deref()).await?;
        let now = Utc::now();
        let id = Uuid::new_v4();
        let active = routine::ActiveModel {
            id: Set(id),
            name: Set(request.name.clone()),
            slug: Set(slug),
            description: Set(request.description),
            body_markdown: Set(request.body_markdown.unwrap_or_default()),
            category: Set(request.category),
            estimated_duration_minutes: Set(request.estimated_duration_minutes),
            difficulty: Set(request.difficulty),
            tags: Set(RoutineTagList::from(request.tags)),
            organization: Set(request.organization),
            created_by: Set(request.created_by),
            properties: Set(JsonObject::default()),
            created_at: Set(now),
            updated_at: Set(now),
        };
        let saved = active
            .insert(&self.db)
            .await
            .map_err(|e| io(e, "insert routine"))?;
        routine_to_api(saved)
    }

    async fn delete_routine(&self, id: Uuid) -> Result<(), VaultError> {
        let txn = self
            .db
            .begin()
            .await
            .map_err(|e| io(e, "begin delete_routine txn"))?;
        routine_exercise::Entity::delete_many()
            .filter(routine_exercise::Column::RoutineId.eq(id))
            .exec(&txn)
            .await
            .map_err(|e| io(e, "delete routine exercises"))?;
        routine::Entity::delete_by_id(id)
            .exec(&txn)
            .await
            .map_err(|e| io(e, "delete routine"))?;
        txn.commit()
            .await
            .map_err(|e| io(e, "commit delete_routine txn"))?;
        Ok(())
    }

    async fn add_routine_exercise(
        &self,
        request: AddRoutineExerciseRequest,
    ) -> Result<RoutineExerciseApi, VaultError> {
        // Resolve display_name from Exercise.name when exercise_id given.
        // Reject when neither exercise_id nor display_name is set.
        let (exercise_id, display_name) = match (request.exercise_id, request.display_name.clone())
        {
            (Some(eid), _) => {
                let ex = exercise::Entity::find_by_id(eid)
                    .one(&self.db)
                    .await
                    .map_err(|e| io(e, "load exercise for add_routine_exercise"))?
                    .ok_or_else(|| VaultError::NotFound(format!("exercise:{eid}")))?;
                (Some(eid), ex.name)
            }
            (None, Some(name)) if !name.trim().is_empty() => (None, name),
            _ => {
                return Err(parse(
                    "neither exercise_id nor display_name supplied",
                    "add_routine_exercise",
                ));
            }
        };

        // Make sure the routine exists.
        if routine::Entity::find_by_id(request.routine_id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load routine for add_routine_exercise"))?
            .is_none()
        {
            return Err(VaultError::NotFound(format!(
                "routine:{}",
                request.routine_id
            )));
        }

        let next_position = routine_exercise::Entity::find()
            .filter(routine_exercise::Column::RoutineId.eq(request.routine_id))
            .order_by_desc(routine_exercise::Column::Position)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "next position lookup"))?
            .map(|r| r.position + 1)
            .unwrap_or(0);

        let now = Utc::now();
        let id = Uuid::new_v4();
        let active = routine_exercise::ActiveModel {
            id: Set(id),
            routine_id: Set(request.routine_id),
            exercise_id: Set(exercise_id),
            display_name: Set(display_name),
            position: Set(next_position),
            group_label: Set(request.group_label),
            target_sets: Set(request.target_sets),
            target_reps: Set(request.target_reps),
            target_weight_kg: Set(request.target_weight_kg),
            target_rest_seconds: Set(request.target_rest_seconds),
            target_rpe: Set(request.target_rpe),
            tempo: Set(request.tempo),
            target_duration_seconds: Set(request.target_duration_seconds),
            target_distance_meters: Set(request.target_distance_meters),
            target_avg_hr: Set(request.target_avg_hr),
            target_pace_seconds_per_km: Set(request.target_pace_seconds_per_km),
            notes: Set(request.notes),
            properties: Set(JsonObject::default()),
            created_at: Set(now),
            updated_at: Set(now),
        };
        let saved = active
            .insert(&self.db)
            .await
            .map_err(|e| io(e, "insert routine_exercise"))?;
        routine_exercise_to_api(saved)
    }

    async fn remove_routine_exercise(&self, id: Uuid) -> Result<(), VaultError> {
        routine_exercise::Entity::delete_by_id(id)
            .exec(&self.db)
            .await
            .map_err(|e| io(e, "delete routine_exercise"))?;
        Ok(())
    }

    async fn reorder_routine_exercises(
        &self,
        routine_id: Uuid,
        ordered_ids: Vec<Uuid>,
    ) -> Result<(), VaultError> {
        // Validate: every id must belong to the routine, and the count
        // must match exactly so we don't silently lose rows.
        let existing = routine_exercise::Entity::find()
            .filter(routine_exercise::Column::RoutineId.eq(routine_id))
            .all(&self.db)
            .await
            .map_err(|e| io(e, "load routine_exercises for reorder"))?;
        if existing.len() != ordered_ids.len() {
            return Err(parse(
                format!(
                    "expected {} ids in reorder list, got {}",
                    existing.len(),
                    ordered_ids.len()
                ),
                "reorder_routine_exercises",
            ));
        }
        let owned: std::collections::HashSet<Uuid> = existing.iter().map(|r| r.id).collect();
        for id in &ordered_ids {
            if !owned.contains(id) {
                return Err(parse(
                    format!("id {id} does not belong to routine {routine_id}"),
                    "reorder_routine_exercises",
                ));
            }
        }

        let txn = self
            .db
            .begin()
            .await
            .map_err(|e| io(e, "begin reorder txn"))?;
        let now = Utc::now();
        for (idx, id) in ordered_ids.iter().enumerate() {
            let model = routine_exercise::Entity::find_by_id(*id)
                .one(&txn)
                .await
                .map_err(|e| io(e, "load row in reorder"))?
                .ok_or_else(|| VaultError::NotFound(format!("routine_exercise:{id}")))?;
            let mut active: routine_exercise::ActiveModel = model.into();
            active.position = Set(idx as i32);
            active.updated_at = Set(now);
            active
                .update(&txn)
                .await
                .map_err(|e| io(e, "update position"))?;
        }
        txn.commit()
            .await
            .map_err(|e| io(e, "commit reorder txn"))?;
        Ok(())
    }
}
