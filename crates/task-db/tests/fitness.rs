//! Integration tests for `FitnessService`.

use task_core::routine_exercise::{self, RoutineExerciseApi};
use task_core::service::{
    AddRoutineExerciseRequest, CreateExerciseRequest, CreateRoutineRequest, FitnessService,
};
use task_core::service_impl::{FitnessServiceDeps, FitnessServiceImpl};
use task_db::seed::seed_demo_data;

use sea_orm::{ColumnTrait, EntityTrait, QueryFilter, QueryOrder};

#[tokio::test]
async fn seed_populates_fitness_fixtures() {
    let db = task_db::init_memory().await.expect("init db");
    let s = seed_demo_data(&db).await.expect("seed");
    assert!(
        s.exercises_created >= 25,
        "exercises={}",
        s.exercises_created
    );
    assert!(s.routines_created >= 3, "routines={}", s.routines_created);
    assert!(
        s.routine_exercises_created >= 12,
        "routine_exercises={}",
        s.routine_exercises_created
    );

    // All three modalities represented.
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db: db.clone() });
    let strength = svc
        .list_exercises(Some("personal".into()), Some("strength".into()), None)
        .await
        .expect("list strength");
    let cardio = svc
        .list_exercises(Some("personal".into()), Some("cardio".into()), None)
        .await
        .expect("list cardio");
    let mobility = svc
        .list_exercises(Some("personal".into()), Some("mobility".into()), None)
        .await
        .expect("list mobility");
    assert!(strength.len() >= 15);
    assert!(cardio.len() >= 5);
    assert!(mobility.len() >= 4);
}

#[tokio::test]
async fn find_exercise_by_slug_or_alias_resolves_each_path() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db });

    // Slug hit.
    let by_slug = svc
        .find_exercise_by_slug_or_alias(Some("personal".into()), "bench-press".into())
        .await
        .expect("find by slug")
        .expect("slug hit");
    assert_eq!(by_slug.name, "Bench Press");

    // Alias hit.
    let by_alias = svc
        .find_exercise_by_slug_or_alias(Some("personal".into()), "ohp".into())
        .await
        .expect("find by alias")
        .expect("alias hit");
    assert_eq!(by_alias.name, "Overhead Press");

    // Miss.
    let miss = svc
        .find_exercise_by_slug_or_alias(Some("personal".into()), "no-such-thing".into())
        .await
        .expect("find miss");
    assert!(miss.is_none());
}

#[tokio::test]
async fn add_routine_exercise_snapshots_display_name_from_exercise() {
    let db = task_db::init_memory().await.expect("init db");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db });

    let ex = svc
        .create_exercise(CreateExerciseRequest {
            name: "Test Bench".into(),
            modality: "strength".into(),
            organization: Some("personal".into()),
            ..Default::default()
        })
        .await
        .expect("create exercise");
    let routine = svc
        .create_routine(CreateRoutineRequest {
            name: "Test Routine".into(),
            organization: Some("personal".into()),
            ..Default::default()
        })
        .await
        .expect("create routine");

    let row = svc
        .add_routine_exercise(AddRoutineExerciseRequest {
            routine_id: routine.id,
            exercise_id: Some(ex.id),
            // Try a different display_name — the service should overwrite.
            display_name: Some("ignored".into()),
            ..Default::default()
        })
        .await
        .expect("add");
    assert_eq!(row.display_name, "Test Bench");
    assert_eq!(row.position, 0);
}

#[tokio::test]
async fn add_routine_exercise_auto_positions_to_end() {
    let db = task_db::init_memory().await.expect("init db");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db });

    let routine = svc
        .create_routine(CreateRoutineRequest {
            name: "AutoPos".into(),
            organization: Some("personal".into()),
            ..Default::default()
        })
        .await
        .expect("create routine");

    let first = svc
        .add_routine_exercise(AddRoutineExerciseRequest {
            routine_id: routine.id,
            display_name: Some("First".into()),
            ..Default::default()
        })
        .await
        .expect("add 1");
    let second = svc
        .add_routine_exercise(AddRoutineExerciseRequest {
            routine_id: routine.id,
            display_name: Some("Second".into()),
            ..Default::default()
        })
        .await
        .expect("add 2");
    assert_eq!(first.position, 0);
    assert_eq!(second.position, 1);
}

#[tokio::test]
async fn add_routine_exercise_requires_id_or_name() {
    let db = task_db::init_memory().await.expect("init db");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db });
    let routine = svc
        .create_routine(CreateRoutineRequest {
            name: "NoEx".into(),
            organization: Some("personal".into()),
            ..Default::default()
        })
        .await
        .expect("create");
    let result = svc
        .add_routine_exercise(AddRoutineExerciseRequest {
            routine_id: routine.id,
            ..Default::default()
        })
        .await;
    assert!(result.is_err());
}

#[tokio::test]
async fn reorder_routine_exercises_reassigns_positions() {
    let db = task_db::init_memory().await.expect("init db");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db: db.clone() });

    let routine = svc
        .create_routine(CreateRoutineRequest {
            name: "Reorder".into(),
            organization: Some("personal".into()),
            ..Default::default()
        })
        .await
        .expect("create");
    let mut ids = Vec::new();
    for label in &["A", "B", "C"] {
        let row = svc
            .add_routine_exercise(AddRoutineExerciseRequest {
                routine_id: routine.id,
                display_name: Some((*label).to_string()),
                ..Default::default()
            })
            .await
            .expect("add");
        ids.push(row.id);
    }
    // Reverse order.
    let reversed = vec![ids[2], ids[1], ids[0]];
    svc.reorder_routine_exercises(routine.id, reversed.clone())
        .await
        .expect("reorder");
    let rows = routine_exercise::Entity::find()
        .filter(routine_exercise::Column::RoutineId.eq(routine.id))
        .order_by_asc(routine_exercise::Column::Position)
        .all(&db)
        .await
        .expect("load");
    let names: Vec<String> = rows.iter().map(|r| r.display_name.clone()).collect();
    assert_eq!(
        names,
        vec!["C".to_string(), "B".to_string(), "A".to_string()]
    );
    let positions: Vec<i32> = rows.iter().map(|r| r.position).collect();
    assert_eq!(positions, vec![0, 1, 2]);
}

#[tokio::test]
async fn reorder_rejects_foreign_ids() {
    let db = task_db::init_memory().await.expect("init db");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db });

    let routine_a = svc
        .create_routine(CreateRoutineRequest {
            name: "A".into(),
            organization: Some("personal".into()),
            ..Default::default()
        })
        .await
        .expect("create A");
    let routine_b = svc
        .create_routine(CreateRoutineRequest {
            name: "B".into(),
            organization: Some("personal".into()),
            ..Default::default()
        })
        .await
        .expect("create B");
    let row_a = svc
        .add_routine_exercise(AddRoutineExerciseRequest {
            routine_id: routine_a.id,
            display_name: Some("A1".into()),
            ..Default::default()
        })
        .await
        .expect("add A1");
    let row_b = svc
        .add_routine_exercise(AddRoutineExerciseRequest {
            routine_id: routine_b.id,
            display_name: Some("B1".into()),
            ..Default::default()
        })
        .await
        .expect("add B1");
    // Try to reorder routine_a using row_b's id.
    let result = svc
        .reorder_routine_exercises(routine_a.id, vec![row_b.id])
        .await;
    assert!(result.is_err());
    // Sanity — reorder using the actual id works.
    svc.reorder_routine_exercises(routine_a.id, vec![row_a.id])
        .await
        .expect("normal reorder");
}

#[tokio::test]
async fn get_routine_with_exercises_returns_position_order() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = FitnessServiceImpl::new(FitnessServiceDeps { db });

    let routine = svc
        .list_routines(Some("personal".into()), Some("kettlebell".into()))
        .await
        .expect("list")
        .into_iter()
        .find(|r| r.slug == "full-body-kettlebell")
        .expect("kb routine");
    let view = svc
        .get_routine_with_exercises(routine.id)
        .await
        .expect("load")
        .expect("present");
    let exercises: Vec<RoutineExerciseApi> =
        serde_json::from_str(&view.exercises_json).expect("decode");
    for window in exercises.windows(2) {
        assert!(
            window[0].position < window[1].position,
            "out of order: {:?}",
            exercises.iter().map(|e| e.position).collect::<Vec<_>>()
        );
    }
}
