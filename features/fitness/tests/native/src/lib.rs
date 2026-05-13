//! Native integration tests for the `fitness` feature. One ephemeral
//! `CrdtDoc` hosts every entity type — proves they coexist under one
//! root and survive cross-entity sync round-trips.

#![cfg(test)]

use architect::Page;
use chrono::{Duration, Utc};
use fitness_crdt::{
    BodyMeasurementRepoLoro, CrdtDoc, ExerciseRepoLoro, RoutineRepoLoro, SetLogRepoLoro,
    WorkoutSessionRepoLoro,
};
use fitness_proto::{
    BodyMeasurementCreate, BodyMeasurementRepo, ExerciseCreate, ExerciseRepo, RoutineCreate,
    RoutineRepo, SetLogCreate, SetLogRepo, WorkoutSessionCreate, WorkoutSessionRepo,
};
use loro::ExportMode;

#[tokio::test]
async fn exercise_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let repo = ExerciseRepoLoro::new(&doc);
    let e = repo
        .create(ExerciseCreate {
            name: "Back Squat".into(),
            category: Some("strength".into()),
            muscle_groups: vec!["quads".into(), "glutes".into()],
            equipment: Some("barbell".into()),
            instructions: Some("Brace, descend, drive.".into()),
            tags: vec!["compound".into()],
        })
        .await
        .unwrap();
    let got = repo.get(e.id).await.unwrap();
    assert_eq!(got.name, "Back Squat");
    assert_eq!(got.category.as_deref(), Some("strength"));
    assert_eq!(
        got.muscle_groups,
        vec!["quads".to_string(), "glutes".into()]
    );
    assert_eq!(got.equipment.as_deref(), Some("barbell"));
    assert_eq!(got.tags, vec!["compound".to_string()]);
}

#[tokio::test]
async fn routine_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let routines = RoutineRepoLoro::new(&doc);
    let exercises = ExerciseRepoLoro::new(&doc);
    let e = exercises
        .create(ExerciseCreate {
            name: "Push-up".into(),
            category: Some("strength".into()),
            muscle_groups: vec!["chest".into()],
            equipment: None,
            instructions: None,
            tags: vec![],
        })
        .await
        .unwrap();
    let r = routines
        .create(RoutineCreate {
            name: "Morning Mobility".into(),
            description: Some("Wake the body up.".into()),
            target_duration_minutes: Some(20),
            difficulty: Some("easy".into()),
            exercise_ids: vec![e.id.to_string()],
            tags: vec!["mobility".into()],
        })
        .await
        .unwrap();
    let got = routines.get(r.id).await.unwrap();
    assert_eq!(got.name, "Morning Mobility");
    assert_eq!(got.target_duration_minutes, Some(20));
    assert_eq!(got.difficulty.as_deref(), Some("easy"));
    assert_eq!(got.exercise_ids, vec![e.id.to_string()]);
}

#[tokio::test]
async fn session_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let sessions = WorkoutSessionRepoLoro::new(&doc);
    let started = Utc::now();
    let s = sessions
        .create(WorkoutSessionCreate {
            routine_id: None,
            name: "Friday Heavy".into(),
            started_at: started,
            ended_at: None,
            notes: Some("Felt strong.".into()),
            mood: Some("great".into()),
            tags: vec!["heavy".into()],
        })
        .await
        .unwrap();
    let got = sessions.get(s.id).await.unwrap();
    assert_eq!(got.name, "Friday Heavy");
    assert_eq!(got.started_at, started);
    assert!(got.ended_at.is_none());
    assert_eq!(got.mood.as_deref(), Some("great"));
    assert_eq!(got.tags, vec!["heavy".to_string()]);
}

#[tokio::test]
async fn set_log_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let sessions = WorkoutSessionRepoLoro::new(&doc);
    let exercises = ExerciseRepoLoro::new(&doc);
    let sets = SetLogRepoLoro::new(&doc);
    let s = sessions
        .create(WorkoutSessionCreate {
            routine_id: None,
            name: "S1".into(),
            started_at: Utc::now(),
            ended_at: None,
            notes: None,
            mood: None,
            tags: vec![],
        })
        .await
        .unwrap();
    let e = exercises
        .create(ExerciseCreate {
            name: "Bench Press".into(),
            category: Some("strength".into()),
            muscle_groups: vec!["chest".into()],
            equipment: Some("barbell".into()),
            instructions: None,
            tags: vec![],
        })
        .await
        .unwrap();
    let sl = sets
        .create(SetLogCreate {
            session_id: s.id,
            exercise_id: e.id,
            set_number: 1,
            reps: Some(5),
            weight_grams: Some(80_000),
            duration_seconds: None,
            distance_meters: None,
            rpe: Some(80),
            notes: Some("Solid".into()),
        })
        .await
        .unwrap();
    let got = sets.get(sl.id).await.unwrap();
    assert_eq!(got.session_id, s.id);
    assert_eq!(got.exercise_id, e.id);
    assert_eq!(got.set_number, 1);
    assert_eq!(got.reps, Some(5));
    assert_eq!(got.weight_grams, Some(80_000));
    assert_eq!(got.rpe, Some(80));
    assert_eq!(got.notes.as_deref(), Some("Solid"));
}

#[tokio::test]
async fn body_measurement_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let measurements = BodyMeasurementRepoLoro::new(&doc);
    let taken = Utc::now();
    let m = measurements
        .create(BodyMeasurementCreate {
            taken_at: taken,
            weight_grams: Some(82_500),
            body_fat_pct_x10: Some(155),
            waist_mm: Some(820),
            chest_mm: Some(1050),
            arm_mm: Some(360),
            thigh_mm: Some(580),
            notes: Some("morning, fasted".into()),
        })
        .await
        .unwrap();
    let got = measurements.get(m.id).await.unwrap();
    assert_eq!(got.taken_at, taken);
    assert_eq!(got.weight_grams, Some(82_500));
    assert_eq!(got.body_fat_pct_x10, Some(155));
    assert_eq!(got.waist_mm, Some(820));
    assert_eq!(got.notes.as_deref(), Some("morning, fasted"));
}

#[tokio::test]
async fn all_five_coexist_in_one_doc() {
    let doc = CrdtDoc::ephemeral();
    let exercises = ExerciseRepoLoro::new(&doc);
    let routines = RoutineRepoLoro::new(&doc);
    let sessions = WorkoutSessionRepoLoro::new(&doc);
    let sets = SetLogRepoLoro::new(&doc);
    let measurements = BodyMeasurementRepoLoro::new(&doc);

    let e = exercises
        .create(ExerciseCreate {
            name: "Deadlift".into(),
            category: Some("strength".into()),
            muscle_groups: vec!["back".into()],
            equipment: Some("barbell".into()),
            instructions: None,
            tags: vec![],
        })
        .await
        .unwrap();

    routines
        .create(RoutineCreate {
            name: "PL day".into(),
            description: None,
            target_duration_minutes: Some(90),
            difficulty: Some("hard".into()),
            exercise_ids: vec![e.id.to_string()],
            tags: vec![],
        })
        .await
        .unwrap();

    let s = sessions
        .create(WorkoutSessionCreate {
            routine_id: None,
            name: "S".into(),
            started_at: Utc::now(),
            ended_at: None,
            notes: None,
            mood: None,
            tags: vec![],
        })
        .await
        .unwrap();

    sets.create(SetLogCreate {
        session_id: s.id,
        exercise_id: e.id,
        set_number: 1,
        reps: Some(3),
        weight_grams: Some(140_000),
        duration_seconds: None,
        distance_meters: None,
        rpe: Some(90),
        notes: None,
    })
    .await
    .unwrap();

    measurements
        .create(BodyMeasurementCreate {
            taken_at: Utc::now() - Duration::days(1),
            weight_grams: Some(80_000),
            body_fat_pct_x10: None,
            waist_mm: None,
            chest_mm: None,
            arm_mm: None,
            thigh_mm: None,
            notes: None,
        })
        .await
        .unwrap();

    assert_eq!(
        exercises
            .list(
                Page {
                    index: 0,
                    size: 100
                },
                None,
                None
            )
            .await
            .unwrap()
            .total,
        1
    );
    assert_eq!(
        routines
            .list(
                Page {
                    index: 0,
                    size: 100
                },
                None,
                None
            )
            .await
            .unwrap()
            .total,
        1
    );
    assert_eq!(
        sessions
            .list(
                Page {
                    index: 0,
                    size: 100
                },
                None,
                None
            )
            .await
            .unwrap()
            .total,
        1
    );
    assert_eq!(
        sets.list(
            Page {
                index: 0,
                size: 100
            },
            None,
            None
        )
        .await
        .unwrap()
        .total,
        1
    );
    assert_eq!(
        measurements
            .list(
                Page {
                    index: 0,
                    size: 100
                },
                None,
                None
            )
            .await
            .unwrap()
            .total,
        1
    );
}

#[tokio::test]
async fn replicas_converge_across_session_and_set_log() {
    let a = CrdtDoc::ephemeral();
    let b = CrdtDoc::ephemeral();
    let sa = WorkoutSessionRepoLoro::new(&a);
    let sb = WorkoutSessionRepoLoro::new(&b);
    let la = SetLogRepoLoro::new(&a);
    let lb = SetLogRepoLoro::new(&b);

    let sess_a = sa
        .create(WorkoutSessionCreate {
            routine_id: None,
            name: "A-sess".into(),
            started_at: Utc::now(),
            ended_at: None,
            notes: None,
            mood: None,
            tags: vec![],
        })
        .await
        .unwrap();
    let sess_b = sb
        .create(WorkoutSessionCreate {
            routine_id: None,
            name: "B-sess".into(),
            started_at: Utc::now(),
            ended_at: None,
            notes: None,
            mood: None,
            tags: vec![],
        })
        .await
        .unwrap();

    la.create(SetLogCreate {
        session_id: sess_a.id,
        exercise_id: uuid::Uuid::new_v4(),
        set_number: 1,
        reps: Some(5),
        weight_grams: Some(60_000),
        duration_seconds: None,
        distance_meters: None,
        rpe: None,
        notes: None,
    })
    .await
    .unwrap();
    lb.create(SetLogCreate {
        session_id: sess_b.id,
        exercise_id: uuid::Uuid::new_v4(),
        set_number: 1,
        reps: Some(8),
        weight_grams: Some(40_000),
        duration_seconds: None,
        distance_meters: None,
        rpe: None,
        notes: None,
    })
    .await
    .unwrap();

    let ab = sa.doc().export(ExportMode::all_updates()).unwrap();
    let bb = sb.doc().export(ExportMode::all_updates()).unwrap();
    sb.doc().import(&ab).unwrap();
    sa.doc().import(&bb).unwrap();

    assert_eq!(
        sa.list(
            Page {
                index: 0,
                size: 100
            },
            None,
            None
        )
        .await
        .unwrap()
        .total,
        2
    );
    assert_eq!(
        sb.list(
            Page {
                index: 0,
                size: 100
            },
            None,
            None
        )
        .await
        .unwrap()
        .total,
        2
    );
    assert_eq!(
        la.list(
            Page {
                index: 0,
                size: 100
            },
            None,
            None
        )
        .await
        .unwrap()
        .total,
        2
    );
    assert_eq!(
        lb.list(
            Page {
                index: 0,
                size: 100
            },
            None,
            None
        )
        .await
        .unwrap()
        .total,
        2
    );
}
