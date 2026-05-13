//! Loro-backed source-of-truth for the fitness feature. Five
//! entities, five `EntityCrdt` impls, five `*RepoLoro` newtypes.

use architect::{Page, RepoError, SortOrder};
use chrono::Utc;
use crdt::EntityCrdt;
use crdt::codec::{
    read_dt, read_opt_dt, read_opt_i64, read_opt_str, read_opt_u32, read_opt_uuid, read_str,
    read_string_list, read_u32, read_uuid, write_dt, write_opt_dt, write_opt_i64, write_opt_str,
    write_opt_string_list, write_opt_u32, write_opt_uuid, write_str, write_string_list, write_u32,
    write_uuid,
};
use fitness_proto::{
    BodyMeasurement, BodyMeasurementCreate, BodyMeasurementList, BodyMeasurementRepo,
    BodyMeasurementUpdate, Exercise, ExerciseCreate, ExerciseList, ExerciseRepo, ExerciseUpdate,
    Routine, RoutineCreate, RoutineList, RoutineRepo, RoutineUpdate, SetLog, SetLogCreate,
    SetLogList, SetLogRepo, SetLogUpdate, WorkoutSession, WorkoutSessionCreate, WorkoutSessionList,
    WorkoutSessionRepo, WorkoutSessionUpdate,
};
use loro::LoroMap;
use uuid::Uuid;

pub use crdt::{CrdtDoc, LoroRepo};

// ── Exercise ──────────────────────────────────────────────────────────

pub struct ExerciseEntity;

#[derive(Clone)]
pub struct ExerciseRepoLoro {
    inner: LoroRepo<ExerciseEntity>,
}

impl ExerciseRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<ExerciseEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for ExerciseEntity {
    type Wire = Exercise;
    type Create = ExerciseCreate;
    type Update = ExerciseUpdate;
    type List = ExerciseList;

    const ROOT: &'static str = "exercises";

    fn id(w: &Exercise) -> Uuid {
        w.id
    }

    fn from_create(input: ExerciseCreate) -> Exercise {
        let now = Utc::now();
        Exercise {
            id: Uuid::new_v4(),
            name: input.name,
            category: input.category,
            muscle_groups: input.muscle_groups,
            equipment: input.equipment,
            instructions: input.instructions,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Exercise) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "name", &e.name)?;
        write_opt_str(m, "category", e.category.as_deref())?;
        write_string_list(m, "muscle_groups", &e.muscle_groups)?;
        write_opt_str(m, "equipment", e.equipment.as_deref())?;
        write_opt_str(m, "instructions", e.instructions.as_deref())?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Exercise, RepoError> {
        Ok(Exercise {
            id: read_uuid(m, "id")?,
            name: read_str(m, "name")?,
            category: read_opt_str(m, "category")?,
            muscle_groups: read_string_list(m, "muscle_groups")?,
            equipment: read_opt_str(m, "equipment")?,
            instructions: read_opt_str(m, "instructions")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: ExerciseUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.category {
            write_opt_str(m, "category", v.as_deref())?;
        }
        if let Some(v) = u.muscle_groups {
            write_opt_string_list(m, "muscle_groups", Some(&v))?;
        }
        if let Some(v) = u.equipment {
            write_opt_str(m, "equipment", v.as_deref())?;
        }
        if let Some(v) = u.instructions {
            write_opt_str(m, "instructions", v.as_deref())?;
        }
        if let Some(v) = u.tags {
            write_opt_string_list(m, "tags", Some(&v))?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [Exercise], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "name" => items.sort_by(|a, b| a.name.cmp(&b.name)),
            "created_at" => items.sort_by(|a, b| a.created_at.cmp(&b.created_at)),
            other => {
                return Err(RepoError::InvalidInput(format!(
                    "unsortable field: {other}"
                )));
            }
        }
        if matches!(order, SortOrder::Desc) {
            items.reverse();
        }
        Ok(())
    }

    fn build_list(items: Vec<Exercise>, total: u32, page: Page) -> ExerciseList {
        ExerciseList { items, total, page }
    }
}

impl ExerciseRepo for ExerciseRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Exercise, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<ExerciseList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: ExerciseCreate) -> Result<Exercise, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: ExerciseUpdate) -> Result<Exercise, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── Routine ───────────────────────────────────────────────────────────

pub struct RoutineEntity;

#[derive(Clone)]
pub struct RoutineRepoLoro {
    inner: LoroRepo<RoutineEntity>,
}

impl RoutineRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<RoutineEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for RoutineEntity {
    type Wire = Routine;
    type Create = RoutineCreate;
    type Update = RoutineUpdate;
    type List = RoutineList;

    const ROOT: &'static str = "routines";

    fn id(w: &Routine) -> Uuid {
        w.id
    }

    fn from_create(input: RoutineCreate) -> Routine {
        let now = Utc::now();
        Routine {
            id: Uuid::new_v4(),
            name: input.name,
            description: input.description,
            target_duration_minutes: input.target_duration_minutes,
            difficulty: input.difficulty,
            exercise_ids: input.exercise_ids,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &Routine) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_str(m, "name", &e.name)?;
        write_opt_str(m, "description", e.description.as_deref())?;
        write_opt_u32(m, "target_duration_minutes", e.target_duration_minutes)?;
        write_opt_str(m, "difficulty", e.difficulty.as_deref())?;
        write_string_list(m, "exercise_ids", &e.exercise_ids)?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<Routine, RepoError> {
        Ok(Routine {
            id: read_uuid(m, "id")?,
            name: read_str(m, "name")?,
            description: read_opt_str(m, "description")?,
            target_duration_minutes: read_opt_u32(m, "target_duration_minutes")?,
            difficulty: read_opt_str(m, "difficulty")?,
            exercise_ids: read_string_list(m, "exercise_ids")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: RoutineUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.description {
            write_opt_str(m, "description", v.as_deref())?;
        }
        if let Some(v) = u.target_duration_minutes {
            write_opt_u32(m, "target_duration_minutes", v)?;
        }
        if let Some(v) = u.difficulty {
            write_opt_str(m, "difficulty", v.as_deref())?;
        }
        if let Some(v) = u.exercise_ids {
            write_opt_string_list(m, "exercise_ids", Some(&v))?;
        }
        if let Some(v) = u.tags {
            write_opt_string_list(m, "tags", Some(&v))?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [Routine], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "name" => items.sort_by(|a, b| a.name.cmp(&b.name)),
            "created_at" => items.sort_by(|a, b| a.created_at.cmp(&b.created_at)),
            other => {
                return Err(RepoError::InvalidInput(format!(
                    "unsortable field: {other}"
                )));
            }
        }
        if matches!(order, SortOrder::Desc) {
            items.reverse();
        }
        Ok(())
    }

    fn build_list(items: Vec<Routine>, total: u32, page: Page) -> RoutineList {
        RoutineList { items, total, page }
    }
}

impl RoutineRepo for RoutineRepoLoro {
    async fn get(&self, id: Uuid) -> Result<Routine, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<RoutineList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: RoutineCreate) -> Result<Routine, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: RoutineUpdate) -> Result<Routine, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── WorkoutSession ────────────────────────────────────────────────────

pub struct WorkoutSessionEntity;

#[derive(Clone)]
pub struct WorkoutSessionRepoLoro {
    inner: LoroRepo<WorkoutSessionEntity>,
}

impl WorkoutSessionRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<WorkoutSessionEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for WorkoutSessionEntity {
    type Wire = WorkoutSession;
    type Create = WorkoutSessionCreate;
    type Update = WorkoutSessionUpdate;
    type List = WorkoutSessionList;

    const ROOT: &'static str = "workout_sessions";

    fn id(w: &WorkoutSession) -> Uuid {
        w.id
    }

    fn from_create(input: WorkoutSessionCreate) -> WorkoutSession {
        let now = Utc::now();
        WorkoutSession {
            id: Uuid::new_v4(),
            routine_id: input.routine_id,
            name: input.name,
            started_at: input.started_at,
            ended_at: input.ended_at,
            notes: input.notes,
            mood: input.mood,
            tags: input.tags,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &WorkoutSession) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_opt_uuid(m, "routine_id", e.routine_id)?;
        write_str(m, "name", &e.name)?;
        write_dt(m, "started_at", e.started_at)?;
        write_opt_dt(m, "ended_at", e.ended_at)?;
        write_opt_str(m, "notes", e.notes.as_deref())?;
        write_opt_str(m, "mood", e.mood.as_deref())?;
        write_string_list(m, "tags", &e.tags)?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<WorkoutSession, RepoError> {
        Ok(WorkoutSession {
            id: read_uuid(m, "id")?,
            routine_id: read_opt_uuid(m, "routine_id")?,
            name: read_str(m, "name")?,
            started_at: read_dt(m, "started_at")?,
            ended_at: read_opt_dt(m, "ended_at")?,
            notes: read_opt_str(m, "notes")?,
            mood: read_opt_str(m, "mood")?,
            tags: read_string_list(m, "tags")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: WorkoutSessionUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.routine_id {
            write_opt_uuid(m, "routine_id", v)?;
        }
        if let Some(v) = u.name {
            write_str(m, "name", &v)?;
        }
        if let Some(v) = u.started_at {
            write_dt(m, "started_at", v)?;
        }
        if let Some(v) = u.ended_at {
            write_opt_dt(m, "ended_at", v)?;
        }
        if let Some(v) = u.notes {
            write_opt_str(m, "notes", v.as_deref())?;
        }
        if let Some(v) = u.mood {
            write_opt_str(m, "mood", v.as_deref())?;
        }
        if let Some(v) = u.tags {
            write_opt_string_list(m, "tags", Some(&v))?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(
        items: &mut [WorkoutSession],
        field: &str,
        order: SortOrder,
    ) -> Result<(), RepoError> {
        match field {
            "name" => items.sort_by(|a, b| a.name.cmp(&b.name)),
            "started_at" => items.sort_by(|a, b| a.started_at.cmp(&b.started_at)),
            "ended_at" => items.sort_by(|a, b| a.ended_at.cmp(&b.ended_at)),
            other => {
                return Err(RepoError::InvalidInput(format!(
                    "unsortable field: {other}"
                )));
            }
        }
        if matches!(order, SortOrder::Desc) {
            items.reverse();
        }
        Ok(())
    }

    fn build_list(items: Vec<WorkoutSession>, total: u32, page: Page) -> WorkoutSessionList {
        WorkoutSessionList { items, total, page }
    }
}

impl WorkoutSessionRepo for WorkoutSessionRepoLoro {
    async fn get(&self, id: Uuid) -> Result<WorkoutSession, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<WorkoutSessionList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: WorkoutSessionCreate) -> Result<WorkoutSession, RepoError> {
        self.inner.create(input).await
    }
    async fn update(
        &self,
        id: Uuid,
        input: WorkoutSessionUpdate,
    ) -> Result<WorkoutSession, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── SetLog ────────────────────────────────────────────────────────────

pub struct SetLogEntity;

#[derive(Clone)]
pub struct SetLogRepoLoro {
    inner: LoroRepo<SetLogEntity>,
}

impl SetLogRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<SetLogEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for SetLogEntity {
    type Wire = SetLog;
    type Create = SetLogCreate;
    type Update = SetLogUpdate;
    type List = SetLogList;

    const ROOT: &'static str = "set_logs";

    fn id(w: &SetLog) -> Uuid {
        w.id
    }

    fn from_create(input: SetLogCreate) -> SetLog {
        let now = Utc::now();
        SetLog {
            id: Uuid::new_v4(),
            session_id: input.session_id,
            exercise_id: input.exercise_id,
            set_number: input.set_number,
            reps: input.reps,
            weight_grams: input.weight_grams,
            duration_seconds: input.duration_seconds,
            distance_meters: input.distance_meters,
            rpe: input.rpe,
            notes: input.notes,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &SetLog) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_uuid(m, "session_id", e.session_id)?;
        write_uuid(m, "exercise_id", e.exercise_id)?;
        write_u32(m, "set_number", e.set_number)?;
        write_opt_u32(m, "reps", e.reps)?;
        write_opt_i64(m, "weight_grams", e.weight_grams)?;
        write_opt_u32(m, "duration_seconds", e.duration_seconds)?;
        write_opt_u32(m, "distance_meters", e.distance_meters)?;
        write_opt_u32(m, "rpe", e.rpe)?;
        write_opt_str(m, "notes", e.notes.as_deref())?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<SetLog, RepoError> {
        Ok(SetLog {
            id: read_uuid(m, "id")?,
            session_id: read_uuid(m, "session_id")?,
            exercise_id: read_uuid(m, "exercise_id")?,
            set_number: read_u32(m, "set_number")?,
            reps: read_opt_u32(m, "reps")?,
            weight_grams: read_opt_i64(m, "weight_grams")?,
            duration_seconds: read_opt_u32(m, "duration_seconds")?,
            distance_meters: read_opt_u32(m, "distance_meters")?,
            rpe: read_opt_u32(m, "rpe")?,
            notes: read_opt_str(m, "notes")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: SetLogUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.session_id {
            write_uuid(m, "session_id", v)?;
        }
        if let Some(v) = u.exercise_id {
            write_uuid(m, "exercise_id", v)?;
        }
        if let Some(v) = u.set_number {
            write_u32(m, "set_number", v)?;
        }
        if let Some(v) = u.reps {
            write_opt_u32(m, "reps", v)?;
        }
        if let Some(v) = u.weight_grams {
            write_opt_i64(m, "weight_grams", v)?;
        }
        if let Some(v) = u.duration_seconds {
            write_opt_u32(m, "duration_seconds", v)?;
        }
        if let Some(v) = u.distance_meters {
            write_opt_u32(m, "distance_meters", v)?;
        }
        if let Some(v) = u.rpe {
            write_opt_u32(m, "rpe", v)?;
        }
        if let Some(v) = u.notes {
            write_opt_str(m, "notes", v.as_deref())?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(items: &mut [SetLog], field: &str, order: SortOrder) -> Result<(), RepoError> {
        match field {
            "session_id" => items.sort_by(|a, b| a.session_id.cmp(&b.session_id)),
            "set_number" => items.sort_by(|a, b| a.set_number.cmp(&b.set_number)),
            "weight_grams" => items.sort_by(|a, b| a.weight_grams.cmp(&b.weight_grams)),
            other => {
                return Err(RepoError::InvalidInput(format!(
                    "unsortable field: {other}"
                )));
            }
        }
        if matches!(order, SortOrder::Desc) {
            items.reverse();
        }
        Ok(())
    }

    fn build_list(items: Vec<SetLog>, total: u32, page: Page) -> SetLogList {
        SetLogList { items, total, page }
    }
}

impl SetLogRepo for SetLogRepoLoro {
    async fn get(&self, id: Uuid) -> Result<SetLog, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<SetLogList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: SetLogCreate) -> Result<SetLog, RepoError> {
        self.inner.create(input).await
    }
    async fn update(&self, id: Uuid, input: SetLogUpdate) -> Result<SetLog, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}

// ── BodyMeasurement ───────────────────────────────────────────────────

pub struct BodyMeasurementEntity;

#[derive(Clone)]
pub struct BodyMeasurementRepoLoro {
    inner: LoroRepo<BodyMeasurementEntity>,
}

impl BodyMeasurementRepoLoro {
    pub fn new(doc: &CrdtDoc) -> Self {
        Self { inner: doc.repo() }
    }
    pub fn inner(&self) -> &LoroRepo<BodyMeasurementEntity> {
        &self.inner
    }
    pub fn doc(&self) -> &loro::LoroDoc {
        self.inner.doc()
    }
}

impl EntityCrdt for BodyMeasurementEntity {
    type Wire = BodyMeasurement;
    type Create = BodyMeasurementCreate;
    type Update = BodyMeasurementUpdate;
    type List = BodyMeasurementList;

    const ROOT: &'static str = "body_measurements";

    fn id(w: &BodyMeasurement) -> Uuid {
        w.id
    }

    fn from_create(input: BodyMeasurementCreate) -> BodyMeasurement {
        let now = Utc::now();
        BodyMeasurement {
            id: Uuid::new_v4(),
            taken_at: input.taken_at,
            weight_grams: input.weight_grams,
            body_fat_pct_x10: input.body_fat_pct_x10,
            waist_mm: input.waist_mm,
            chest_mm: input.chest_mm,
            arm_mm: input.arm_mm,
            thigh_mm: input.thigh_mm,
            notes: input.notes,
            created_at: now,
            updated_at: now,
        }
    }

    fn encode_into(m: &LoroMap, e: &BodyMeasurement) -> Result<(), RepoError> {
        write_uuid(m, "id", e.id)?;
        write_dt(m, "taken_at", e.taken_at)?;
        write_opt_i64(m, "weight_grams", e.weight_grams)?;
        write_opt_u32(m, "body_fat_pct_x10", e.body_fat_pct_x10)?;
        write_opt_u32(m, "waist_mm", e.waist_mm)?;
        write_opt_u32(m, "chest_mm", e.chest_mm)?;
        write_opt_u32(m, "arm_mm", e.arm_mm)?;
        write_opt_u32(m, "thigh_mm", e.thigh_mm)?;
        write_opt_str(m, "notes", e.notes.as_deref())?;
        write_dt(m, "created_at", e.created_at)?;
        write_dt(m, "updated_at", e.updated_at)?;
        Ok(())
    }

    fn decode_from(m: &LoroMap) -> Result<BodyMeasurement, RepoError> {
        Ok(BodyMeasurement {
            id: read_uuid(m, "id")?,
            taken_at: read_dt(m, "taken_at")?,
            weight_grams: read_opt_i64(m, "weight_grams")?,
            body_fat_pct_x10: read_opt_u32(m, "body_fat_pct_x10")?,
            waist_mm: read_opt_u32(m, "waist_mm")?,
            chest_mm: read_opt_u32(m, "chest_mm")?,
            arm_mm: read_opt_u32(m, "arm_mm")?,
            thigh_mm: read_opt_u32(m, "thigh_mm")?,
            notes: read_opt_str(m, "notes")?,
            created_at: read_dt(m, "created_at")?,
            updated_at: read_dt(m, "updated_at")?,
        })
    }

    fn apply_update(m: &LoroMap, u: BodyMeasurementUpdate) -> Result<(), RepoError> {
        if let Some(v) = u.taken_at {
            write_dt(m, "taken_at", v)?;
        }
        if let Some(v) = u.weight_grams {
            write_opt_i64(m, "weight_grams", v)?;
        }
        if let Some(v) = u.body_fat_pct_x10 {
            write_opt_u32(m, "body_fat_pct_x10", v)?;
        }
        if let Some(v) = u.waist_mm {
            write_opt_u32(m, "waist_mm", v)?;
        }
        if let Some(v) = u.chest_mm {
            write_opt_u32(m, "chest_mm", v)?;
        }
        if let Some(v) = u.arm_mm {
            write_opt_u32(m, "arm_mm", v)?;
        }
        if let Some(v) = u.thigh_mm {
            write_opt_u32(m, "thigh_mm", v)?;
        }
        if let Some(v) = u.notes {
            write_opt_str(m, "notes", v.as_deref())?;
        }
        write_dt(m, "updated_at", Utc::now())?;
        Ok(())
    }

    fn sort_items(
        items: &mut [BodyMeasurement],
        field: &str,
        order: SortOrder,
    ) -> Result<(), RepoError> {
        match field {
            "taken_at" => items.sort_by(|a, b| a.taken_at.cmp(&b.taken_at)),
            "weight_grams" => items.sort_by(|a, b| a.weight_grams.cmp(&b.weight_grams)),
            other => {
                return Err(RepoError::InvalidInput(format!(
                    "unsortable field: {other}"
                )));
            }
        }
        if matches!(order, SortOrder::Desc) {
            items.reverse();
        }
        Ok(())
    }

    fn build_list(items: Vec<BodyMeasurement>, total: u32, page: Page) -> BodyMeasurementList {
        BodyMeasurementList { items, total, page }
    }
}

impl BodyMeasurementRepo for BodyMeasurementRepoLoro {
    async fn get(&self, id: Uuid) -> Result<BodyMeasurement, RepoError> {
        self.inner.get(id).await
    }
    async fn list(
        &self,
        page: architect::Page,
        sort: Option<architect::Sort>,
        filter: Option<architect::Filter>,
    ) -> Result<BodyMeasurementList, RepoError> {
        self.inner.list(page, sort, filter).await
    }
    async fn create(&self, input: BodyMeasurementCreate) -> Result<BodyMeasurement, RepoError> {
        self.inner.create(input).await
    }
    async fn update(
        &self,
        id: Uuid,
        input: BodyMeasurementUpdate,
    ) -> Result<BodyMeasurement, RepoError> {
        self.inner.update(id, input).await
    }
    async fn delete(&self, id: Uuid) -> Result<(), RepoError> {
        self.inner.delete(id).await
    }
}
