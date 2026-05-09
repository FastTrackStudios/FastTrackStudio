//! Integration tests for the workout-session surface on
//! [`FitnessService`] (actuals layer).

use sea_orm::{ColumnTrait, EntityTrait, QueryFilter};
use task_core::service::{
    CompleteWorkoutSessionRequest, FitnessService, LogSetRequest, StartWorkoutSessionRequest,
    UpdateSetRequest, VaultError,
};
use task_core::service_impl::{FitnessServiceDeps, FitnessServiceImpl};
use task_core::set_log::SetLogApi;
use task_core::workout_session::{self, WorkoutSessionStatus};
use task_db::seed::{DEMO_NAMESPACE, seed_demo_data};
use uuid::Uuid;

fn demo_id(key: &str) -> Uuid {
    Uuid::new_v5(&DEMO_NAMESPACE, key.as_bytes())
}

#[tokio::test]
async fn seed_populates_workout_sessions() {
    let db = task_db::init_memory().await.expect("init db");
    let s = seed_demo_data(&db).await.expect("seed");
    assert!(
        s.workout_sessions_created >= 2,
        "workout_sessions_created={}",
        s.workout_sessions_created
    );
    assert!(
        s.set_logs_created >= 18,
        "set_logs_created={}",
        s.set_logs_created
    );
    let active = workout_session::Entity::find()
        .filter(workout_session::Column::Status.eq(WorkoutSessionStatus::Active))
        .all(&db)
        .await
        .expect("query active");
    assert_eq!(active.len(), 1, "exactly one active session expected");
}

#[tokio::test]
async fn start_workout_session_with_routine_seeds_planned_sets() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db: db.clone() });

    let routine_id = demo_id("routine:push-day");
    let view = svc
        .start_workout_session(StartWorkoutSessionRequest {
            routine_id: Some(routine_id),
            label: None,
            bodyweight_kg: Some(80.0),
            organization: Some("personal".to_string()),
            created_by: Some("test".to_string()),
        })
        .await
        .expect("start");
    assert_eq!(view.session.status, WorkoutSessionStatus::Active);
    assert_eq!(view.session.bodyweight_kg, Some(80.0));
    assert_eq!(view.sets_done, 0);
    // Push day routine has 4+3+3+3+3+3 = 19 planned sets when target_sets are
    // honored. The seed routine has those exact target_sets.
    assert!(view.sets_total >= 18, "sets_total={}", view.sets_total);

    let sets: Vec<SetLogApi> = serde_json::from_str(&view.sets_json).expect("decode sets");
    assert_eq!(sets.len() as u32, view.sets_total);
    assert!(
        sets.iter().all(|s| s.completed_at.is_none()),
        "all planned sets should be unchecked"
    );
}

#[tokio::test]
async fn start_ad_hoc_session_is_empty() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db });
    let view = svc
        .start_workout_session(StartWorkoutSessionRequest {
            routine_id: None,
            label: Some("Ad-hoc".to_string()),
            bodyweight_kg: None,
            organization: None,
            created_by: Some("test".to_string()),
        })
        .await
        .expect("start");
    assert_eq!(view.sets_total, 0);
    assert_eq!(view.session.routine_name_snapshot, "Ad-hoc");
}

#[tokio::test]
async fn log_set_default_marks_done_and_defer_does_not() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db });
    let view = svc
        .start_workout_session(StartWorkoutSessionRequest {
            label: Some("test".to_string()),
            ..Default::default()
        })
        .await
        .expect("start");
    let session_id = view.session.id;
    let bench_id = demo_id("exercise:bench-press");

    let done = svc
        .log_set(LogSetRequest {
            workout_session_id: session_id,
            exercise_id: Some(bench_id),
            reps: Some(8),
            weight_kg: Some(60.0),
            ..Default::default()
        })
        .await
        .expect("log default");
    assert!(
        done.completed_at.is_some(),
        "default log_set should be done"
    );
    assert_eq!(done.set_index, 0);

    let pending = svc
        .log_set(LogSetRequest {
            workout_session_id: session_id,
            exercise_id: Some(bench_id),
            reps: Some(8),
            weight_kg: Some(60.0),
            defer: true,
            ..Default::default()
        })
        .await
        .expect("log defer");
    assert!(
        pending.completed_at.is_none(),
        "defer should leave it unchecked"
    );
    assert_eq!(pending.set_index, 1);
    assert_eq!(pending.position, done.position + 1);
}

#[tokio::test]
async fn mark_set_done_toggles_and_is_idempotent() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db });
    let view = svc
        .start_workout_session(StartWorkoutSessionRequest {
            routine_id: Some(demo_id("routine:push-day")),
            organization: Some("personal".to_string()),
            ..Default::default()
        })
        .await
        .expect("start");
    let sets: Vec<SetLogApi> = serde_json::from_str(&view.sets_json).unwrap();
    let first = sets.first().expect("at least one set").id;

    let done1 = svc.mark_set_done(first, true).await.expect("mark done");
    let ts = done1.completed_at.expect("completed_at set");

    // Idempotent: marking done again preserves the original timestamp.
    let done2 = svc
        .mark_set_done(first, true)
        .await
        .expect("mark done again");
    assert_eq!(done2.completed_at, Some(ts));

    // Uncheck clears it.
    let undone = svc.mark_set_done(first, false).await.expect("uncheck");
    assert!(undone.completed_at.is_none());
}

#[tokio::test]
async fn update_set_only_writes_supplied_fields() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db });
    let view = svc
        .start_workout_session(StartWorkoutSessionRequest {
            label: Some("update test".to_string()),
            ..Default::default()
        })
        .await
        .expect("start");
    let row = svc
        .log_set(LogSetRequest {
            workout_session_id: view.session.id,
            exercise_id: Some(demo_id("exercise:bench-press")),
            reps: Some(8),
            weight_kg: Some(60.0),
            rpe: Some(7.0),
            ..Default::default()
        })
        .await
        .expect("log");

    // Only update reps; weight + rpe must survive.
    let updated = svc
        .update_set(UpdateSetRequest {
            set_log_id: row.id,
            reps: Some(10),
            ..Default::default()
        })
        .await
        .expect("update");
    assert_eq!(updated.reps, Some(10));
    assert_eq!(updated.weight_kg, Some(60.0));
    assert_eq!(updated.rpe, Some(7.0));
}

#[tokio::test]
async fn complete_workout_session_requires_all_sets_done_by_default() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db });
    let view = svc
        .start_workout_session(StartWorkoutSessionRequest {
            routine_id: Some(demo_id("routine:easy-5k")),
            organization: Some("personal".to_string()),
            ..Default::default()
        })
        .await
        .expect("start");
    let session_id = view.session.id;

    // Pending → reject when require_all_sets_done.
    let err = svc
        .complete_workout_session(CompleteWorkoutSessionRequest {
            id: session_id,
            require_all_sets_done: true,
            ..Default::default()
        })
        .await
        .expect_err("pending sets should block completion");
    assert!(matches!(err, VaultError::ParseError(_)), "got {err:?}");

    // allow-incomplete bypasses the check.
    let saved = svc
        .complete_workout_session(CompleteWorkoutSessionRequest {
            id: session_id,
            require_all_sets_done: false,
            overall_rpe: Some(6.0),
            notes: Some("hopped off the treadmill early".to_string()),
        })
        .await
        .expect("complete");
    assert_eq!(saved.status, WorkoutSessionStatus::Completed);
    assert!(saved.completed_at.is_some());
    assert_eq!(saved.overall_rpe, Some(6.0));
    assert!(saved.notes.contains("hopped off"));
}

#[tokio::test]
async fn abandon_workout_session_marks_status() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db });
    let view = svc
        .start_workout_session(StartWorkoutSessionRequest {
            label: Some("abandon test".to_string()),
            ..Default::default()
        })
        .await
        .expect("start");
    let saved = svc
        .abandon_workout_session(view.session.id)
        .await
        .expect("abandon");
    assert_eq!(saved.status, WorkoutSessionStatus::Abandoned);
    assert!(saved.completed_at.is_some());
}

#[tokio::test]
async fn workout_session_view_aggregates_volume_and_cardio() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db });

    // Demo "Push Day" active session: 3 done bench (8/7/7 @ 80kg) + 2 done OHP (8/8 @ 50kg)
    // + 1 done incline DB (10 @ 25kg) + 1 done plank (45s).
    // volume = (8+7+7)*80 + (8+8)*50 + 10*25 = 1760 + 800 + 250 = 2810
    let push_id = demo_id("workout_session:push-day-active");
    let view = svc
        .get_workout_session(push_id)
        .await
        .expect("get view")
        .expect("session present");
    assert!(view.sets_done >= 7, "sets_done={}", view.sets_done);
    assert!(
        view.total_volume_kg >= 2800.0,
        "total_volume_kg={}",
        view.total_volume_kg
    );
    assert!(
        view.total_cardio_seconds >= 45,
        "total_cardio_seconds={}",
        view.total_cardio_seconds
    );

    // Completed Easy 5K session: cardio total ≥ 1830s.
    let easy_id = demo_id("workout_session:easy-5k-completed");
    let view = svc
        .get_workout_session(easy_id)
        .await
        .expect("get view")
        .expect("session present");
    assert!(
        view.total_cardio_seconds >= 1830,
        "total_cardio_seconds={}",
        view.total_cardio_seconds
    );
}
