//! Integration tests for `FitnessService::daily_calorie_balance`.

use chrono::{Duration, NaiveDate, Utc};
use sea_orm::{ActiveValue::Set, DatabaseConnection, EntityTrait};
use task_core::food_log;
use task_core::meal_plan::MealType;
use task_core::property::JsonObject;
use task_core::service::{
    DailyCalorieBalanceRequest, DayBalance, DaySessionBreakdown, FitnessService,
};
use task_core::service_impl::{FitnessServiceDeps, FitnessServiceImpl};
use task_core::set_log;
use task_core::workout_session::{self, WorkoutSessionStatus};
use uuid::Uuid;

const ORG: &str = "personal";

async fn seed_food(db: &DatabaseConnection, date: NaiveDate, name: &str, kcal: f64) {
    let id = Uuid::new_v4();
    let now = Utc::now();
    let mut active = <food_log::ActiveModel as sea_orm::ActiveModelTrait>::default();
    active.id = Set(id);
    active.date = Set(date);
    active.meal_type = Set(MealType::Lunch);
    active.food_id = Set(None);
    active.product_id = Set(None);
    active.food_name = Set(name.to_string());
    active.quantity_grams = Set(100.0);
    active.kcal = Set(Some(kcal));
    active.protein_g = Set(None);
    active.carbs_g = Set(None);
    active.sugars_g = Set(None);
    active.fiber_g = Set(None);
    active.fat_g = Set(None);
    active.saturated_fat_g = Set(None);
    active.sodium_mg = Set(None);
    active.notes = Set(None);
    active.created_by = Set(Some("test".into()));
    active.meal_plan_entry_id = Set(None);
    active.recipe_id = Set(None);
    active.organization = Set(Some(ORG.into()));
    active.properties = Set(JsonObject::default());
    active.created_at = Set(now);
    active.updated_at = Set(now);
    food_log::Entity::insert(active)
        .exec(db)
        .await
        .expect("insert food");
}

/// Build a completed strength workout session of the given duration on a
/// given date, with one bench-press set.
async fn seed_strength_session(
    db: &DatabaseConnection,
    bench_id: Option<Uuid>,
    date: NaiveDate,
    duration_minutes: i64,
    bodyweight_kg: Option<f64>,
) -> Uuid {
    let started_at = date.and_hms_opt(12, 0, 0).unwrap().and_utc();
    let completed_at = started_at + Duration::minutes(duration_minutes);
    let session_id = Uuid::new_v4();
    let mut active = <workout_session::ActiveModel as sea_orm::ActiveModelTrait>::default();
    active.id = Set(session_id);
    active.routine_id = Set(None);
    active.routine_name_snapshot = Set("test session".into());
    active.status = Set(WorkoutSessionStatus::Completed);
    active.started_at = Set(started_at);
    active.completed_at = Set(Some(completed_at));
    active.notes = Set(String::new());
    active.overall_rpe = Set(None);
    active.bodyweight_kg = Set(bodyweight_kg);
    active.organization = Set(Some(ORG.into()));
    active.created_by = Set(Some("test".into()));
    active.properties = Set(JsonObject::default());
    active.created_at = Set(started_at);
    active.updated_at = Set(completed_at);
    workout_session::Entity::insert(active)
        .exec(db)
        .await
        .expect("insert session");

    // Add a strength set so the dominant modality resolves to "strength".
    let set_id = Uuid::new_v4();
    let mut set = <set_log::ActiveModel as sea_orm::ActiveModelTrait>::default();
    set.id = Set(set_id);
    set.workout_session_id = Set(session_id);
    set.exercise_id = Set(bench_id);
    set.exercise_name_snapshot = Set("Bench Press".into());
    set.routine_exercise_id = Set(None);
    set.position = Set(0);
    set.set_index = Set(0);
    set.reps = Set(Some(8));
    set.weight_kg = Set(Some(80.0));
    set.duration_seconds = Set(None);
    set.distance_meters = Set(None);
    set.avg_hr = Set(None);
    set.pace_seconds_per_km = Set(None);
    set.rpe = Set(None);
    set.notes = Set(None);
    set.completed_at = Set(Some(completed_at));
    set.properties = Set(JsonObject::default());
    set.created_at = Set(started_at);
    set.updated_at = Set(completed_at);
    set_log::Entity::insert(set)
        .exec(db)
        .await
        .expect("insert set");

    session_id
}

#[tokio::test]
async fn daily_calorie_balance_aggregates_food_and_burn() {
    let db = task_db::init_memory().await.expect("init db");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db: db.clone() });

    let today = Utc::now().date_naive();
    let yesterday = today - Duration::days(1);

    // Today: 500 + 700 = 1200 consumed; 1h strength @ 80kg → 5.0 * 80 * 1.0 = 400.
    seed_food(&db, today, "lunch", 500.0).await;
    seed_food(&db, today, "dinner", 700.0).await;
    seed_strength_session(&db, None, today, 60, Some(80.0)).await;
    // Yesterday: 800 consumed, no workout.
    seed_food(&db, yesterday, "snack", 800.0).await;

    let view = svc
        .daily_calorie_balance(DailyCalorieBalanceRequest {
            organization: Some(ORG.into()),
            since_date: Some(yesterday),
            until_date: Some(today),
            default_bodyweight_kg: None,
        })
        .await
        .expect("balance");
    assert_eq!(view.day_count, 2);
    let days: Vec<DayBalance> = serde_json::from_str(&view.days_json).expect("decode");
    assert_eq!(days.len(), 2);
    let yday = days.iter().find(|d| d.date == yesterday).unwrap();
    let tday = days.iter().find(|d| d.date == today).unwrap();
    assert!((yday.consumed_kcal - 800.0).abs() < 0.5);
    assert!((yday.burned_kcal - 0.0).abs() < 0.5);
    assert_eq!(yday.session_count, 0);
    assert!((tday.consumed_kcal - 1200.0).abs() < 0.5);
    assert!(
        (tday.burned_kcal - 400.0).abs() < 0.5,
        "burned={}",
        tday.burned_kcal
    );
    assert_eq!(tday.session_count, 1);
    assert!((view.total_consumed_kcal - 2000.0).abs() < 0.5);
    assert!((view.total_burned_kcal - 400.0).abs() < 0.5);
    assert!((view.net_kcal - 1600.0).abs() < 0.5);
}

#[tokio::test]
async fn mixed_modality_uses_dominant_set_count() {
    let db = task_db::init_memory().await.expect("init db");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db: db.clone() });

    let today = Utc::now().date_naive();
    // Build a session with 1 strength set + 3 cardio sets at duration 1h with bw 80.
    let started_at = today.and_hms_opt(8, 0, 0).unwrap().and_utc();
    let completed_at = started_at + Duration::minutes(60);
    let session_id = Uuid::new_v4();
    let mut active = <workout_session::ActiveModel as sea_orm::ActiveModelTrait>::default();
    active.id = Set(session_id);
    active.routine_id = Set(None);
    active.routine_name_snapshot = Set("mixed".into());
    active.status = Set(WorkoutSessionStatus::Completed);
    active.started_at = Set(started_at);
    active.completed_at = Set(Some(completed_at));
    active.notes = Set(String::new());
    active.overall_rpe = Set(None);
    active.bodyweight_kg = Set(Some(80.0));
    active.organization = Set(Some(ORG.into()));
    active.created_by = Set(Some("test".into()));
    active.properties = Set(JsonObject::default());
    active.created_at = Set(started_at);
    active.updated_at = Set(completed_at);
    workout_session::Entity::insert(active)
        .exec(&db)
        .await
        .expect("insert");

    // Strength set
    let mut s = <set_log::ActiveModel as sea_orm::ActiveModelTrait>::default();
    s.id = Set(Uuid::new_v4());
    s.workout_session_id = Set(session_id);
    s.exercise_name_snapshot = Set("Bench".into());
    s.position = Set(0);
    s.set_index = Set(0);
    s.reps = Set(Some(5));
    s.weight_kg = Set(Some(80.0));
    s.completed_at = Set(Some(completed_at));
    s.properties = Set(JsonObject::default());
    s.created_at = Set(started_at);
    s.updated_at = Set(completed_at);
    set_log::Entity::insert(s)
        .exec(&db)
        .await
        .expect("strength");

    // 3 cardio sets
    for i in 0..3 {
        let mut c = <set_log::ActiveModel as sea_orm::ActiveModelTrait>::default();
        c.id = Set(Uuid::new_v4());
        c.workout_session_id = Set(session_id);
        c.exercise_name_snapshot = Set("Run".into());
        c.position = Set(1 + i);
        c.set_index = Set(i);
        c.distance_meters = Set(Some(1000.0));
        c.duration_seconds = Set(Some(300));
        c.completed_at = Set(Some(completed_at));
        c.properties = Set(JsonObject::default());
        c.created_at = Set(started_at);
        c.updated_at = Set(completed_at);
        set_log::Entity::insert(c).exec(&db).await.expect("cardio");
    }

    let view = svc
        .daily_calorie_balance(DailyCalorieBalanceRequest {
            organization: Some(ORG.into()),
            since_date: Some(today),
            until_date: Some(today),
            default_bodyweight_kg: None,
        })
        .await
        .expect("balance");
    let days: Vec<DayBalance> = serde_json::from_str(&view.days_json).expect("decode");
    let day = &days[0];
    let bd: Vec<DaySessionBreakdown> =
        serde_json::from_str(&day.session_breakdown_json).expect("decode breakdown");
    assert_eq!(bd.len(), 1);
    // 3 cardio sets > 1 strength set → MET = 7.0; mixed label still applied.
    assert_eq!(bd[0].modality_summary, "mixed");
    let expected = 7.0 * 80.0 * 1.0;
    assert!(
        (day.burned_kcal - expected).abs() < 0.5,
        "burned={}, expected={}",
        day.burned_kcal,
        expected
    );
}

#[tokio::test]
async fn active_sessions_are_excluded_from_burn() {
    let db = task_db::init_memory().await.expect("init db");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db: db.clone() });

    let today = Utc::now().date_naive();
    let started_at = today.and_hms_opt(8, 0, 0).unwrap().and_utc();
    let session_id = Uuid::new_v4();
    let mut active = <workout_session::ActiveModel as sea_orm::ActiveModelTrait>::default();
    active.id = Set(session_id);
    active.routine_id = Set(None);
    active.routine_name_snapshot = Set("active".into());
    active.status = Set(WorkoutSessionStatus::Active);
    active.started_at = Set(started_at);
    active.completed_at = Set(None);
    active.notes = Set(String::new());
    active.overall_rpe = Set(None);
    active.bodyweight_kg = Set(Some(80.0));
    active.organization = Set(Some(ORG.into()));
    active.created_by = Set(Some("test".into()));
    active.properties = Set(JsonObject::default());
    active.created_at = Set(started_at);
    active.updated_at = Set(started_at);
    workout_session::Entity::insert(active)
        .exec(&db)
        .await
        .expect("insert");

    let view = svc
        .daily_calorie_balance(DailyCalorieBalanceRequest {
            organization: Some(ORG.into()),
            since_date: Some(today),
            until_date: Some(today),
            default_bodyweight_kg: None,
        })
        .await
        .expect("balance");
    assert!(view.total_burned_kcal.abs() < 1e-9);
    let days: Vec<DayBalance> = serde_json::from_str(&view.days_json).expect("decode");
    assert_eq!(days[0].session_count, 0);
}

#[tokio::test]
async fn bodyweight_fallback_chain() {
    let db = task_db::init_memory().await.expect("init db");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db: db.clone() });
    let today = Utc::now().date_naive();

    // Session A: bodyweight_kg set on session → wins.
    seed_strength_session(&db, None, today, 60, Some(90.0)).await;

    // Session B: no bodyweight on session, request supplies default → 70.
    let started = today.and_hms_opt(15, 0, 0).unwrap().and_utc();
    let completed = started + Duration::minutes(60);
    let session_b = Uuid::new_v4();
    let mut a = <workout_session::ActiveModel as sea_orm::ActiveModelTrait>::default();
    a.id = Set(session_b);
    a.routine_id = Set(None);
    a.routine_name_snapshot = Set("B".into());
    a.status = Set(WorkoutSessionStatus::Completed);
    a.started_at = Set(started);
    a.completed_at = Set(Some(completed));
    a.notes = Set(String::new());
    a.overall_rpe = Set(None);
    a.bodyweight_kg = Set(None);
    a.organization = Set(Some(ORG.into()));
    a.created_by = Set(Some("test".into()));
    a.properties = Set(JsonObject::default());
    a.created_at = Set(started);
    a.updated_at = Set(completed);
    workout_session::Entity::insert(a)
        .exec(&db)
        .await
        .expect("insert b");

    let mut s = <set_log::ActiveModel as sea_orm::ActiveModelTrait>::default();
    s.id = Set(Uuid::new_v4());
    s.workout_session_id = Set(session_b);
    s.exercise_name_snapshot = Set("Bench".into());
    s.position = Set(0);
    s.set_index = Set(0);
    s.reps = Set(Some(5));
    s.weight_kg = Set(Some(60.0));
    s.completed_at = Set(Some(completed));
    s.properties = Set(JsonObject::default());
    s.created_at = Set(started);
    s.updated_at = Set(completed);
    set_log::Entity::insert(s).exec(&db).await.expect("set b");

    // With request-supplied default 70 kg.
    let view = svc
        .daily_calorie_balance(DailyCalorieBalanceRequest {
            organization: Some(ORG.into()),
            since_date: Some(today),
            until_date: Some(today),
            default_bodyweight_kg: Some(70.0),
        })
        .await
        .expect("balance");
    // A: 5.0 * 90 * 1 = 450; B: 5.0 * 70 * 1 = 350.
    assert!(
        (view.total_burned_kcal - 800.0).abs() < 0.5,
        "got {}",
        view.total_burned_kcal
    );

    // Without default → fallback 75 kg for session B.
    let view2 = svc
        .daily_calorie_balance(DailyCalorieBalanceRequest {
            organization: Some(ORG.into()),
            since_date: Some(today),
            until_date: Some(today),
            default_bodyweight_kg: None,
        })
        .await
        .expect("balance2");
    // A: 450; B: 5.0 * 75 * 1 = 375. Sum: 825.
    assert!(
        (view2.total_burned_kcal - 825.0).abs() < 0.5,
        "got {}",
        view2.total_burned_kcal
    );
}

#[tokio::test]
async fn empty_window_returns_zero_totals_and_day_count() {
    let db = task_db::init_memory().await.expect("init db");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db: db.clone() });
    let start = NaiveDate::from_ymd_opt(2020, 1, 1).unwrap();
    let end = NaiveDate::from_ymd_opt(2020, 1, 5).unwrap();
    let view = svc
        .daily_calorie_balance(DailyCalorieBalanceRequest {
            organization: Some(ORG.into()),
            since_date: Some(start),
            until_date: Some(end),
            default_bodyweight_kg: None,
        })
        .await
        .expect("balance");
    assert_eq!(view.day_count, 5);
    assert!(view.total_consumed_kcal.abs() < 1e-9);
    assert!(view.total_burned_kcal.abs() < 1e-9);
    let days: Vec<DayBalance> = serde_json::from_str(&view.days_json).expect("decode");
    assert_eq!(days.len(), 5);
    for d in days {
        assert_eq!(d.food_log_count, 0);
        assert_eq!(d.session_count, 0);
    }
}

#[tokio::test]
async fn exercise_progress_with_seed_runs_5k_session() {
    // Sanity check: the seeded 5K session should be findable by progress
    // for "Easy Run".
    let db = task_db::init_memory().await.expect("init db");
    task_db::seed::seed_demo_data(&db).await.expect("seed");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db });
    let view = svc
        .exercise_progress(task_core::service::ExerciseProgressRequest {
            exercise: "easy-run".into(),
            organization: Some(ORG.into()),
            limit: None,
        })
        .await
        .expect("progress");
    assert!(view.session_count >= 1);
    assert_eq!(view.modality.as_deref(), Some("cardio"));
}
