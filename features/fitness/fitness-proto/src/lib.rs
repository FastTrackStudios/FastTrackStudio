//! `fitness-proto` — wire contract for the fitness-tracking feature.
//!
//! Five top-level entities:
//!
//! - `Exercise`        — a reusable movement / activity definition
//! - `Routine`         — an ordered workout template
//! - `WorkoutSession`  — an actual session performed by the user
//! - `SetLog`          — one set within a session (reps / weight / time / distance)
//! - `BodyMeasurement` — periodic body check-in (weight, body fat, etc.)

pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

// ── Exercise ──────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "exercises", repo)]
pub struct Exercise {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ExerciseName"))]
    pub name: String,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::ExerciseCategory"))]
    pub category: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::MuscleGroups"))]
    pub muscle_groups: Vec<String>,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::Equipment"))]
    pub equipment: Option<String>,

    #[architect(fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Paragraph(1..3)")
    )]
    pub instructions: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::FitnessTags"))]
    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── Routine ───────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "routines", repo)]
pub struct Routine {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RoutineName"))]
    pub name: String,

    #[architect(fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Paragraph(1..3)")
    )]
    pub description: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RoutineDuration"))]
    pub target_duration_minutes: Option<u32>,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::Difficulty"))]
    pub difficulty: Option<String>,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "(fake::faker::lorem::en::Word(), 3..10)")
    )]
    pub exercise_ids: Vec<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::FitnessTags"))]
    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── WorkoutSession ────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "workout_sessions", repo)]
pub struct WorkoutSession {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub routine_id: Option<Uuid>,

    #[architect(filterable, sortable, fulltext)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::SessionName"))]
    pub name: String,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub started_at: DateTime<Utc>,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub ended_at: Option<DateTime<Utc>>,

    #[architect(fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(3..10)")
    )]
    pub notes: Option<String>,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::Mood"))]
    pub mood: Option<String>,

    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::FitnessTags"))]
    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── SetLog ────────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "set_logs", repo)]
pub struct SetLog {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable)]
    pub session_id: Uuid,

    #[architect(filterable)]
    pub exercise_id: Uuid,

    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "1u32..6"))]
    pub set_number: u32,

    #[cfg_attr(feature = "fake", dummy(faker = "1u32..30"))]
    pub reps: Option<u32>,

    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "1000i64..150_000"))]
    pub weight_grams: Option<i64>,

    #[cfg_attr(feature = "fake", dummy(faker = "10u32..600"))]
    pub duration_seconds: Option<u32>,

    #[cfg_attr(feature = "fake", dummy(faker = "100u32..10000"))]
    pub distance_meters: Option<u32>,

    #[cfg_attr(feature = "fake", dummy(faker = "1u32..11"))]
    pub rpe: Option<u32>,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(2..6)")
    )]
    pub notes: Option<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── BodyMeasurement ───────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "body_measurements", repo)]
pub struct BodyMeasurement {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RecentDateTime"))]
    pub taken_at: DateTime<Utc>,

    #[architect(sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "50_000i64..120_000"))]
    pub weight_grams: Option<i64>,

    #[cfg_attr(feature = "fake", dummy(faker = "80u32..350"))]
    pub body_fat_pct_x10: Option<u32>,

    #[cfg_attr(feature = "fake", dummy(faker = "600u32..1200"))]
    pub waist_mm: Option<u32>,

    #[cfg_attr(feature = "fake", dummy(faker = "800u32..1400"))]
    pub chest_mm: Option<u32>,

    #[cfg_attr(feature = "fake", dummy(faker = "250u32..450"))]
    pub arm_mm: Option<u32>,

    #[cfg_attr(feature = "fake", dummy(faker = "400u32..800"))]
    pub thigh_mm: Option<u32>,

    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Sentence(2..6)")
    )]
    pub notes: Option<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── FitnessService ────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum FitnessServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait FitnessService {
    /// Mark a session complete by setting `ended_at` to now.
    async fn complete_session(&self, session_id: Uuid) -> Result<(), FitnessServiceError>;
}

#[cfg(feature = "fake")]
pub mod fakers {
    use chrono::{DateTime, Duration, Utc};
    use fake::Dummy;
    use fake::rand::{Rng, seq::IndexedRandom};

    fn pick<R: Rng + ?Sized>(rng: &mut R, values: &[&str]) -> String {
        (*values.choose(rng).unwrap()).to_string()
    }

    pub struct ExerciseName;
    impl Dummy<ExerciseName> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ExerciseName, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "Bench Press",
                    "Back Squat",
                    "Deadlift",
                    "Overhead Press",
                    "Pull-up",
                    "Bent-over Row",
                    "Lunge",
                    "Bicep Curl",
                    "Tricep Dip",
                    "Plank",
                    "Push-up",
                    "Romanian Deadlift",
                    "Lat Pulldown",
                    "Leg Press",
                    "Hip Thrust",
                    "Face Pull",
                ],
            )
        }
    }

    pub struct ExerciseCategory;
    impl Dummy<ExerciseCategory> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &ExerciseCategory, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "strength",
                    "cardio",
                    "mobility",
                    "core",
                    "plyometric",
                    "stretching",
                ],
            )
        }
    }

    pub struct MuscleGroups;
    impl Dummy<MuscleGroups> for Vec<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &MuscleGroups, rng: &mut R) -> Self {
            const POOL: &[&str] = &[
                "chest",
                "back",
                "shoulders",
                "biceps",
                "triceps",
                "quads",
                "hamstrings",
                "glutes",
                "calves",
                "core",
                "forearms",
            ];
            let n = rng.random_range(1..=3usize);
            POOL.choose_multiple(rng, n)
                .map(|s| s.to_string())
                .collect()
        }
    }

    pub struct Equipment;
    impl Dummy<Equipment> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &Equipment, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "barbell",
                    "dumbbell",
                    "kettlebell",
                    "machine",
                    "cable",
                    "bodyweight",
                    "resistance-band",
                    "bench",
                ],
            )
        }
    }

    pub struct RoutineName;
    impl Dummy<RoutineName> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &RoutineName, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "Push Day",
                    "Pull Day",
                    "Leg Day",
                    "Full Body A",
                    "Full Body B",
                    "Upper / Lower",
                    "Conditioning",
                    "Mobility Flow",
                    "5x5 Strength",
                    "Hypertrophy Block",
                ],
            )
        }
    }

    pub struct RoutineDuration;
    impl Dummy<RoutineDuration> for u32 {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &RoutineDuration, rng: &mut R) -> Self {
            const VALUES: &[u32] = &[20, 30, 45, 60, 75, 90];
            *VALUES.choose(rng).unwrap()
        }
    }

    pub struct Difficulty;
    impl Dummy<Difficulty> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &Difficulty, rng: &mut R) -> Self {
            pick(rng, &["beginner", "intermediate", "advanced"])
        }
    }

    pub struct SessionName;
    impl Dummy<SessionName> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &SessionName, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "Morning Lift",
                    "Evening Cardio",
                    "Quick Conditioning",
                    "Heavy Squat Day",
                    "Push Session",
                    "Pull Session",
                    "Recovery Walk",
                    "HIIT Block",
                ],
            )
        }
    }

    pub struct Mood;
    impl Dummy<Mood> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &Mood, rng: &mut R) -> Self {
            pick(
                rng,
                &["great", "good", "okay", "tired", "drained", "energized"],
            )
        }
    }

    pub struct RecentDateTime;
    impl Dummy<RecentDateTime> for DateTime<Utc> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &RecentDateTime, rng: &mut R) -> Self {
            Utc::now() - Duration::minutes(rng.random_range(0..525_600))
        }
    }

    pub struct FitnessTags;
    impl Dummy<FitnessTags> for Vec<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &FitnessTags, rng: &mut R) -> Self {
            const POOL: &[&str] = &[
                "strength",
                "cardio",
                "mobility",
                "deload",
                "pr",
                "endurance",
                "hypertrophy",
                "warmup",
                "recovery",
            ];
            let n = rng.random_range(1..=3usize);
            POOL.choose_multiple(rng, n)
                .map(|s| s.to_string())
                .collect()
        }
    }
}
