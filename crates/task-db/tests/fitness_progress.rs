//! Integration tests for `FitnessService::exercise_progress`.

use task_core::service::{
    ExerciseProgressEntry, ExerciseProgressRequest, FitnessService, LogSetRequest,
    StartWorkoutSessionRequest,
};
use task_core::service_impl::{FitnessServiceDeps, FitnessServiceImpl};
use task_db::seed::{DEMO_NAMESPACE, seed_demo_data};
use uuid::Uuid;

fn demo_id(key: &str) -> Uuid {
    Uuid::new_v5(&DEMO_NAMESPACE, key.as_bytes())
}

#[tokio::test]
async fn exercise_progress_finds_seeded_bench_press_session() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db });

    let view = svc
        .exercise_progress(ExerciseProgressRequest {
            exercise: "Bench Press".into(),
            organization: Some("personal".into()),
            limit: None,
        })
        .await
        .expect("progress");
    assert_eq!(view.exercise_name, "Bench Press");
    assert_eq!(view.modality.as_deref(), Some("strength"));
    assert!(view.session_count >= 1);

    let entries: Vec<ExerciseProgressEntry> =
        serde_json::from_str(&view.entries_json).expect("decode entries");
    assert!(!entries.is_empty(), "expected at least one entry");
    let push = entries
        .iter()
        .find(|e| e.workout_session_id == demo_id("workout_session:push-day-active"))
        .expect("push-day session entry");
    assert_eq!(push.completed_set_count, 3);
    // 8*80 + 7*80 + 7*80 = 1760
    assert!(
        (push.session_volume_kg - 1760.0).abs() < 0.5,
        "volume={}",
        push.session_volume_kg
    );
    let top = push.top_set.as_ref().expect("top_set present");
    assert_eq!(top.weight_kg, Some(80.0));
    assert_eq!(top.reps, Some(8));
    let orm = top.estimated_one_rep_max_kg.expect("1RM computed");
    let expected = 80.0 * (1.0 + 8.0 / 30.0);
    assert!(
        (orm - expected).abs() < 1e-6,
        "orm={orm}, expected={expected}"
    );
}

#[tokio::test]
async fn exercise_progress_falls_back_to_snapshot_name_for_custom_set() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db });

    let view = svc
        .exercise_progress(ExerciseProgressRequest {
            exercise: "Tricep Pushdown".into(),
            organization: Some("personal".into()),
            limit: None,
        })
        .await
        .expect("progress");
    assert_eq!(view.exercise_name, "Tricep Pushdown");
    assert!(view.exercise_id.is_none(), "custom name has no exercise_id");
    assert!(view.session_count >= 1);
}

#[tokio::test]
async fn exercise_progress_respects_limit() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db: db.clone() });

    // Spawn an extra ad-hoc session that also logs Bench Press so we
    // have multiple sessions to limit against.
    let view = svc
        .start_workout_session(StartWorkoutSessionRequest {
            label: Some("extra bench".into()),
            organization: Some("personal".into()),
            ..Default::default()
        })
        .await
        .expect("start");
    svc.log_set(LogSetRequest {
        workout_session_id: view.session.id,
        exercise_id: Some(demo_id("exercise:bench-press")),
        reps: Some(5),
        weight_kg: Some(85.0),
        ..Default::default()
    })
    .await
    .expect("log");

    let limited = svc
        .exercise_progress(ExerciseProgressRequest {
            exercise: "Bench Press".into(),
            organization: Some("personal".into()),
            limit: Some(1),
        })
        .await
        .expect("limited");
    assert_eq!(limited.session_count, 1);
}

#[tokio::test]
async fn exercise_progress_for_unused_exercise_returns_zero() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db });

    let view = svc
        .exercise_progress(ExerciseProgressRequest {
            exercise: "Back Squat".into(),
            organization: Some("personal".into()),
            limit: None,
        })
        .await
        .expect("progress");
    assert_eq!(view.session_count, 0);
    let entries: Vec<ExerciseProgressEntry> =
        serde_json::from_str(&view.entries_json).expect("decode");
    assert!(entries.is_empty());
    assert!(view.trend_summary.is_empty());
}

#[tokio::test]
async fn exercise_progress_trend_summary_requires_two_top_sets() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db });

    // Seeded data: only 1 bench-press session has completed sets, so
    // no trend summary should be emitted.
    let view = svc
        .exercise_progress(ExerciseProgressRequest {
            exercise: "Bench Press".into(),
            organization: Some("personal".into()),
            limit: None,
        })
        .await
        .expect("progress");
    assert!(
        view.trend_summary.is_empty(),
        "got '{}'",
        view.trend_summary
    );
}

#[tokio::test]
async fn exercise_progress_trend_summary_appears_with_two_sessions() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db: db.clone() });

    // Session A — older, log a heavy bench set.
    let a = svc
        .start_workout_session(StartWorkoutSessionRequest {
            label: Some("session-a".into()),
            organization: Some("personal".into()),
            ..Default::default()
        })
        .await
        .expect("start a");
    svc.log_set(LogSetRequest {
        workout_session_id: a.session.id,
        exercise_id: Some(demo_id("exercise:bench-press")),
        reps: Some(5),
        weight_kg: Some(70.0),
        ..Default::default()
    })
    .await
    .expect("log a");

    // Sleep so started_at differs.
    tokio::time::sleep(std::time::Duration::from_millis(20)).await;

    let b = svc
        .start_workout_session(StartWorkoutSessionRequest {
            label: Some("session-b".into()),
            organization: Some("personal".into()),
            ..Default::default()
        })
        .await
        .expect("start b");
    svc.log_set(LogSetRequest {
        workout_session_id: b.session.id,
        exercise_id: Some(demo_id("exercise:bench-press")),
        reps: Some(3),
        weight_kg: Some(95.0),
        ..Default::default()
    })
    .await
    .expect("log b");

    let view = svc
        .exercise_progress(ExerciseProgressRequest {
            exercise: "bench-press".into(),
            organization: Some("personal".into()),
            limit: None,
        })
        .await
        .expect("progress");
    assert!(
        !view.trend_summary.is_empty(),
        "expected trend summary, got: {}",
        view.trend_summary
    );
    assert!(view.trend_summary.contains("→"));
}

#[tokio::test]
async fn exercise_progress_top_set_skips_pending_and_picks_heaviest() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db: db.clone() });

    let started = svc
        .start_workout_session(StartWorkoutSessionRequest {
            label: Some("topset".into()),
            organization: Some("personal".into()),
            ..Default::default()
        })
        .await
        .expect("start");

    // Done: 5 @ 90 kg
    svc.log_set(LogSetRequest {
        workout_session_id: started.session.id,
        exercise_id: Some(demo_id("exercise:bench-press")),
        reps: Some(5),
        weight_kg: Some(90.0),
        ..Default::default()
    })
    .await
    .expect("done lighter");
    // Done tied weight, more reps: 8 @ 100 kg
    svc.log_set(LogSetRequest {
        workout_session_id: started.session.id,
        exercise_id: Some(demo_id("exercise:bench-press")),
        reps: Some(8),
        weight_kg: Some(100.0),
        ..Default::default()
    })
    .await
    .expect("done heavier");
    // Done same weight, fewer reps: 3 @ 100 kg (should lose tiebreak)
    svc.log_set(LogSetRequest {
        workout_session_id: started.session.id,
        exercise_id: Some(demo_id("exercise:bench-press")),
        reps: Some(3),
        weight_kg: Some(100.0),
        ..Default::default()
    })
    .await
    .expect("done tie");
    // Pending: should be ignored despite being heaviest weight.
    svc.log_set(LogSetRequest {
        workout_session_id: started.session.id,
        exercise_id: Some(demo_id("exercise:bench-press")),
        reps: Some(1),
        weight_kg: Some(120.0),
        defer: true,
        ..Default::default()
    })
    .await
    .expect("pending");

    let view = svc
        .exercise_progress(ExerciseProgressRequest {
            exercise: "Bench Press".into(),
            organization: Some("personal".into()),
            limit: None,
        })
        .await
        .expect("progress");
    let entries: Vec<ExerciseProgressEntry> =
        serde_json::from_str(&view.entries_json).expect("decode");
    let entry = entries
        .iter()
        .find(|e| e.workout_session_id == started.session.id)
        .expect("entry");
    let top = entry.top_set.as_ref().expect("top set");
    // Heaviest done = 100 kg; tiebreak picks the 8-rep set.
    assert_eq!(top.weight_kg, Some(100.0));
    assert_eq!(top.reps, Some(8));
}

#[tokio::test]
async fn exercise_progress_one_rep_max_uses_epley() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db: db.clone() });
    let started = svc
        .start_workout_session(StartWorkoutSessionRequest {
            label: Some("epley".into()),
            organization: Some("personal".into()),
            ..Default::default()
        })
        .await
        .expect("start");
    svc.log_set(LogSetRequest {
        workout_session_id: started.session.id,
        exercise_id: Some(demo_id("exercise:bench-press")),
        reps: Some(6),
        weight_kg: Some(95.0),
        ..Default::default()
    })
    .await
    .expect("log");
    let view = svc
        .exercise_progress(ExerciseProgressRequest {
            exercise: "Bench Press".into(),
            organization: Some("personal".into()),
            limit: Some(1),
        })
        .await
        .expect("progress");
    let entries: Vec<ExerciseProgressEntry> =
        serde_json::from_str(&view.entries_json).expect("decode");
    let top = entries[0].top_set.as_ref().expect("top set");
    let expected = 95.0 * (1.0 + 6.0 / 30.0);
    let got = top.estimated_one_rep_max_kg.expect("1RM");
    assert!(
        (got - expected).abs() < 1e-9,
        "got={got}, expected={expected}"
    );
}
