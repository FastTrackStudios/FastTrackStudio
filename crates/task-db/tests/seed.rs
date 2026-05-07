use task_db::seed::{reset_demo_data, seed_demo_data};

#[tokio::test]
async fn seed_demo_data_is_idempotent() {
    let db = task_db::init_memory().await.expect("init in-memory db");

    let first = seed_demo_data(&db).await.expect("first seed run");
    assert!(first.total_created() > 0, "first run created nothing");
    assert_eq!(
        first.total_unchanged(),
        0,
        "fresh db should report 0 unchanged on first run"
    );

    let second = seed_demo_data(&db).await.expect("second seed run");
    assert_eq!(
        second.total_created(),
        0,
        "second run created rows: {second:?} — seeder isn't idempotent"
    );
    assert_eq!(
        second.total_unchanged(),
        first.total_created(),
        "second run should report all first-run rows as unchanged"
    );
}

#[tokio::test]
async fn seed_then_reset_then_seed_recreates_full_set() {
    let db = task_db::init_memory().await.expect("init in-memory db");

    let first = seed_demo_data(&db).await.expect("first seed run");
    let removed = reset_demo_data(&db).await.expect("reset");
    assert_eq!(
        removed.total_created(),
        first.total_created(),
        "reset should remove every seeded row"
    );

    let second = seed_demo_data(&db).await.expect("second seed run");
    assert_eq!(
        second.total_created(),
        first.total_created(),
        "post-reset seed should recreate the same row count"
    );
}

#[tokio::test]
async fn seed_demo_data_covers_every_entity_flavor() {
    let db = task_db::init_memory().await.expect("init in-memory db");
    let s = seed_demo_data(&db).await.expect("seed");
    assert!(
        s.projects_created >= 6,
        "want >= 6 projects, got {}",
        s.projects_created
    );
    assert!(
        s.tasks_created >= 15,
        "want >= 15 tasks, got {}",
        s.tasks_created
    );
    assert!(
        s.calendar_events_created >= 4,
        "want >= 4 events, got {}",
        s.calendar_events_created
    );
    assert!(
        s.people_created >= 4,
        "want >= 4 people, got {}",
        s.people_created
    );
    assert!(
        s.comments_created >= 3,
        "want >= 3 comments, got {}",
        s.comments_created
    );
}
