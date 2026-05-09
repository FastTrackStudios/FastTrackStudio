//! SeaORM-backed [`FitnessService`] implementation.
//!
//! Handles the Exercise catalog and Routine + RoutineExercise template
//! tables. Slug uniqueness is per-`(organization, slug)`; collisions
//! are resolved by appending `-2`, `-3`, …

use chrono::Utc;
use sea_orm::{
    ActiveModelTrait, ColumnTrait, DatabaseConnection, EntityTrait, PaginatorTrait, QueryFilter,
    QueryOrder, QuerySelect, Set, TransactionTrait,
};
use uuid::Uuid;

use crate::exercise::{self, ExerciseAliasList, ExerciseApi, ExerciseModality, ExerciseMuscleList};
use crate::property::JsonObject;
use crate::routine::{self, RoutineApi, RoutineTagList};
use crate::routine_exercise::{self, RoutineExerciseApi};
use crate::service::{
    AddRoutineExerciseRequest, CompleteWorkoutSessionRequest, CreateExerciseRequest,
    CreateRoutineRequest, ExercisePatch, FitnessService, LogSetRequest, RoutineWithExercisesView,
    StartWorkoutSessionRequest, UpdateSetRequest, VaultError, WorkoutSessionView,
};
use crate::set_log::{self, SetLogApi};
use crate::workout_session::{self, WorkoutSessionApi, WorkoutSessionStatus};

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

    // ── Workout sessions ─────────────────────────────────────

    async fn start_workout_session(
        &self,
        request: StartWorkoutSessionRequest,
    ) -> Result<WorkoutSessionView, VaultError> {
        let now = Utc::now();
        let session_id = Uuid::new_v4();

        let (routine_name_snapshot, planned_rows): (String, Vec<routine_exercise::Model>) =
            if let Some(rid) = request.routine_id {
                let routine = routine::Entity::find_by_id(rid)
                    .one(&self.db)
                    .await
                    .map_err(|e| io(e, "load routine"))?
                    .ok_or_else(|| VaultError::NotFound(format!("routine:{rid}")))?;
                let exercises = routine_exercise::Entity::find()
                    .filter(routine_exercise::Column::RoutineId.eq(rid))
                    .order_by_asc(routine_exercise::Column::Position)
                    .all(&self.db)
                    .await
                    .map_err(|e| io(e, "load routine exercises"))?;
                (routine.name, exercises)
            } else {
                let label = request
                    .label
                    .filter(|s| !s.trim().is_empty())
                    .unwrap_or_else(|| "Ad-hoc workout".to_string());
                (label, Vec::new())
            };

        let txn = self
            .db
            .begin()
            .await
            .map_err(|e| io(e, "begin start_workout_session txn"))?;

        let session_active = workout_session::ActiveModel {
            id: Set(session_id),
            routine_id: Set(request.routine_id),
            routine_name_snapshot: Set(routine_name_snapshot),
            status: Set(WorkoutSessionStatus::Active),
            started_at: Set(now),
            completed_at: Set(None),
            notes: Set(String::new()),
            overall_rpe: Set(None),
            bodyweight_kg: Set(request.bodyweight_kg),
            organization: Set(request.organization),
            created_by: Set(request.created_by),
            properties: Set(JsonObject::default()),
            created_at: Set(now),
            updated_at: Set(now),
        };
        session_active
            .insert(&txn)
            .await
            .map_err(|e| io(e, "insert workout_session"))?;

        // Pre-populate planned SetLog rows from routine targets.
        let mut position: i32 = 0;
        let mut per_exercise_index: std::collections::HashMap<String, i32> =
            std::collections::HashMap::new();
        for re_row in &planned_rows {
            // target_sets = None → seed exactly one planned set so the
            // user can still see the row in the checklist.
            let n = re_row.target_sets.unwrap_or(1).max(1);
            let display_name = re_row.display_name.clone();
            for _ in 0..n {
                let set_index = per_exercise_index.entry(display_name.clone()).or_insert(0);
                let id = Uuid::new_v4();
                let active = set_log::ActiveModel {
                    id: Set(id),
                    workout_session_id: Set(session_id),
                    exercise_id: Set(re_row.exercise_id),
                    exercise_name_snapshot: Set(display_name.clone()),
                    routine_exercise_id: Set(Some(re_row.id)),
                    position: Set(position),
                    set_index: Set(*set_index),
                    reps: Set(re_row.target_reps),
                    weight_kg: Set(re_row.target_weight_kg),
                    duration_seconds: Set(re_row.target_duration_seconds),
                    distance_meters: Set(re_row.target_distance_meters),
                    avg_hr: Set(re_row.target_avg_hr),
                    pace_seconds_per_km: Set(re_row.target_pace_seconds_per_km),
                    rpe: Set(None),
                    notes: Set(None),
                    completed_at: Set(None),
                    properties: Set(JsonObject::default()),
                    created_at: Set(now),
                    updated_at: Set(now),
                };
                active
                    .insert(&txn)
                    .await
                    .map_err(|e| io(e, "insert planned set_log"))?;
                *set_index += 1;
                position += 1;
            }
        }

        txn.commit()
            .await
            .map_err(|e| io(e, "commit start_workout_session txn"))?;

        build_session_view(&self.db, session_id).await
    }

    async fn get_workout_session(
        &self,
        id: Uuid,
    ) -> Result<Option<WorkoutSessionView>, VaultError> {
        let exists = workout_session::Entity::find_by_id(id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "get_workout_session"))?
            .is_some();
        if !exists {
            return Ok(None);
        }
        build_session_view(&self.db, id).await.map(Some)
    }

    async fn list_workout_sessions(
        &self,
        organization: Option<String>,
        status: Option<String>,
        limit: Option<u32>,
    ) -> Result<Vec<WorkoutSessionApi>, VaultError> {
        let mut q =
            workout_session::Entity::find().order_by_desc(workout_session::Column::StartedAt);
        if let Some(org) = organization {
            q = q.filter(workout_session::Column::Organization.eq(org));
        }
        if let Some(status) = status {
            let parsed = WorkoutSessionStatus::parse(&status).ok_or_else(|| {
                parse(
                    format!("unknown status '{status}'"),
                    "list_workout_sessions",
                )
            })?;
            q = q.filter(workout_session::Column::Status.eq(parsed));
        }
        let limit = limit.unwrap_or(20);
        let rows = q
            .limit(u64::from(limit))
            .all(&self.db)
            .await
            .map_err(|e| io(e, "list_workout_sessions"))?;
        rows.into_iter().map(workout_session_to_api).collect()
    }

    async fn log_set(&self, request: LogSetRequest) -> Result<SetLogApi, VaultError> {
        // Resolve exercise_id / display_name.
        let (exercise_id, display_name) = match (request.exercise_id, request.display_name.clone())
        {
            (Some(eid), _) => {
                let ex = exercise::Entity::find_by_id(eid)
                    .one(&self.db)
                    .await
                    .map_err(|e| io(e, "load exercise for log_set"))?
                    .ok_or_else(|| VaultError::NotFound(format!("exercise:{eid}")))?;
                (Some(eid), ex.name)
            }
            (None, Some(name)) if !name.trim().is_empty() => (None, name),
            _ => {
                return Err(parse(
                    "neither exercise_id nor display_name supplied",
                    "log_set",
                ));
            }
        };

        // Make sure the session exists (and we can write to it).
        if workout_session::Entity::find_by_id(request.workout_session_id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load workout_session for log_set"))?
            .is_none()
        {
            return Err(VaultError::NotFound(format!(
                "workout_session:{}",
                request.workout_session_id
            )));
        }

        let next_position = set_log::Entity::find()
            .filter(set_log::Column::WorkoutSessionId.eq(request.workout_session_id))
            .order_by_desc(set_log::Column::Position)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "next position lookup"))?
            .map(|r| r.position + 1)
            .unwrap_or(0);

        let set_index = set_log::Entity::find()
            .filter(set_log::Column::WorkoutSessionId.eq(request.workout_session_id))
            .filter(set_log::Column::ExerciseNameSnapshot.eq(display_name.clone()))
            .count(&self.db)
            .await
            .map_err(|e| io(e, "set index lookup"))? as i32;

        let now = Utc::now();
        let completed_at = if request.defer { None } else { Some(now) };
        let id = Uuid::new_v4();
        let active = set_log::ActiveModel {
            id: Set(id),
            workout_session_id: Set(request.workout_session_id),
            exercise_id: Set(exercise_id),
            exercise_name_snapshot: Set(display_name),
            routine_exercise_id: Set(request.routine_exercise_id),
            position: Set(next_position),
            set_index: Set(set_index),
            reps: Set(request.reps),
            weight_kg: Set(request.weight_kg),
            duration_seconds: Set(request.duration_seconds),
            distance_meters: Set(request.distance_meters),
            avg_hr: Set(request.avg_hr),
            pace_seconds_per_km: Set(request.pace_seconds_per_km),
            rpe: Set(request.rpe),
            notes: Set(request.notes),
            completed_at: Set(completed_at),
            properties: Set(JsonObject::default()),
            created_at: Set(now),
            updated_at: Set(now),
        };
        let saved = active
            .insert(&self.db)
            .await
            .map_err(|e| io(e, "insert set_log"))?;
        set_log_to_api(saved)
    }

    async fn mark_set_done(&self, set_log_id: Uuid, done: bool) -> Result<SetLogApi, VaultError> {
        let model = set_log::Entity::find_by_id(set_log_id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load set_log"))?
            .ok_or_else(|| VaultError::NotFound(format!("set_log:{set_log_id}")))?;
        let now = Utc::now();
        let new_completed_at = match (done, model.completed_at) {
            (true, Some(prev)) => Some(prev),
            (true, None) => Some(now),
            (false, _) => None,
        };
        let mut active: set_log::ActiveModel = model.into();
        active.completed_at = Set(new_completed_at);
        active.updated_at = Set(now);
        let saved = active
            .update(&self.db)
            .await
            .map_err(|e| io(e, "update set_log mark_done"))?;
        set_log_to_api(saved)
    }

    async fn update_set(&self, request: UpdateSetRequest) -> Result<SetLogApi, VaultError> {
        let model = set_log::Entity::find_by_id(request.set_log_id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load set_log"))?
            .ok_or_else(|| VaultError::NotFound(format!("set_log:{}", request.set_log_id)))?;
        let mut active: set_log::ActiveModel = model.into();
        if let Some(reps) = request.reps {
            active.reps = Set(Some(reps));
        }
        if let Some(w) = request.weight_kg {
            active.weight_kg = Set(Some(w));
        }
        if let Some(d) = request.duration_seconds {
            active.duration_seconds = Set(Some(d));
        }
        if let Some(d) = request.distance_meters {
            active.distance_meters = Set(Some(d));
        }
        if let Some(h) = request.avg_hr {
            active.avg_hr = Set(Some(h));
        }
        if let Some(p) = request.pace_seconds_per_km {
            active.pace_seconds_per_km = Set(Some(p));
        }
        if let Some(r) = request.rpe {
            active.rpe = Set(Some(r));
        }
        if let Some(n) = request.notes {
            active.notes = Set(Some(n));
        }
        active.updated_at = Set(Utc::now());
        let saved = active
            .update(&self.db)
            .await
            .map_err(|e| io(e, "update set_log"))?;
        set_log_to_api(saved)
    }

    async fn delete_set(&self, set_log_id: Uuid) -> Result<(), VaultError> {
        set_log::Entity::delete_by_id(set_log_id)
            .exec(&self.db)
            .await
            .map_err(|e| io(e, "delete set_log"))?;
        Ok(())
    }

    async fn complete_workout_session(
        &self,
        request: CompleteWorkoutSessionRequest,
    ) -> Result<WorkoutSessionApi, VaultError> {
        let model = workout_session::Entity::find_by_id(request.id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load workout_session"))?
            .ok_or_else(|| VaultError::NotFound(format!("workout_session:{}", request.id)))?;

        if request.require_all_sets_done {
            let pending = set_log::Entity::find()
                .filter(set_log::Column::WorkoutSessionId.eq(request.id))
                .filter(set_log::Column::CompletedAt.is_null())
                .count(&self.db)
                .await
                .map_err(|e| io(e, "count pending set_logs"))?;
            if pending > 0 {
                return Err(parse(
                    format!("{pending} set(s) still pending"),
                    "complete_workout_session",
                ));
            }
        }

        let now = Utc::now();
        let merged_notes = match request.notes {
            Some(extra) if !extra.trim().is_empty() => {
                if model.notes.trim().is_empty() {
                    extra
                } else {
                    format!("{}\n{}", model.notes, extra)
                }
            }
            _ => model.notes.clone(),
        };
        let mut active: workout_session::ActiveModel = model.into();
        active.status = Set(WorkoutSessionStatus::Completed);
        active.completed_at = Set(Some(now));
        active.overall_rpe = Set(request.overall_rpe);
        active.notes = Set(merged_notes);
        active.updated_at = Set(now);
        let saved = active
            .update(&self.db)
            .await
            .map_err(|e| io(e, "complete workout_session"))?;
        workout_session_to_api(saved)
    }

    async fn abandon_workout_session(&self, id: Uuid) -> Result<WorkoutSessionApi, VaultError> {
        let model = workout_session::Entity::find_by_id(id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load workout_session"))?
            .ok_or_else(|| VaultError::NotFound(format!("workout_session:{id}")))?;
        let now = Utc::now();
        let mut active: workout_session::ActiveModel = model.into();
        active.status = Set(WorkoutSessionStatus::Abandoned);
        active.completed_at = Set(Some(now));
        active.updated_at = Set(now);
        let saved = active
            .update(&self.db)
            .await
            .map_err(|e| io(e, "abandon workout_session"))?;
        workout_session_to_api(saved)
    }
}

fn workout_session_to_api(model: workout_session::Model) -> Result<WorkoutSessionApi, VaultError> {
    convert_model::<workout_session::Model, WorkoutSessionApi>(model)
}

fn set_log_to_api(model: set_log::Model) -> Result<SetLogApi, VaultError> {
    convert_model::<set_log::Model, SetLogApi>(model)
}

async fn build_session_view(
    db: &DatabaseConnection,
    session_id: Uuid,
) -> Result<WorkoutSessionView, VaultError> {
    let session = workout_session::Entity::find_by_id(session_id)
        .one(db)
        .await
        .map_err(|e| io(e, "load workout_session for view"))?
        .ok_or_else(|| VaultError::NotFound(format!("workout_session:{session_id}")))?;
    let sets = set_log::Entity::find()
        .filter(set_log::Column::WorkoutSessionId.eq(session_id))
        .order_by_asc(set_log::Column::Position)
        .all(db)
        .await
        .map_err(|e| io(e, "load set_logs for view"))?;

    let sets_total = sets.len() as u32;
    let mut sets_done: u32 = 0;
    let mut total_volume_kg: f64 = 0.0;
    let mut total_cardio_seconds: u32 = 0;
    for s in &sets {
        if s.completed_at.is_some() {
            sets_done += 1;
            if let (Some(reps), Some(w)) = (s.reps, s.weight_kg) {
                total_volume_kg += f64::from(reps) * w;
            }
            if let Some(dur) = s.duration_seconds {
                // Treat sets with a duration but no reps as cardio/mobility.
                if s.reps.is_none() {
                    total_cardio_seconds = total_cardio_seconds.saturating_add(dur);
                }
            }
        }
    }

    let set_apis: Vec<SetLogApi> = sets
        .into_iter()
        .map(set_log_to_api)
        .collect::<Result<Vec<_>, _>>()?;
    let sets_json = serde_json::to_string(&set_apis).map_err(|e| io(e, "serialize set_logs"))?;

    Ok(WorkoutSessionView {
        session: workout_session_to_api(session)?,
        sets_json,
        sets_done,
        sets_total,
        total_volume_kg,
        total_cardio_seconds,
    })
}
