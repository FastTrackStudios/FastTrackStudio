use sea_orm::{ColumnTrait, EntityTrait, QueryFilter};
use task_core::{notification, task};
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
        s.projects_created >= 8,
        "want >= 8 projects (incl. cross-org), got {}",
        s.projects_created
    );
    assert!(
        s.tasks_created >= 25,
        "want >= 25 tasks, got {}",
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
    assert!(
        s.reactions_created >= 3,
        "want >= 3 reactions, got {}",
        s.reactions_created
    );
    assert!(
        s.notifications_created >= 3,
        "want >= 3 notifications, got {}",
        s.notifications_created
    );
    assert!(
        s.saved_views_created >= 2,
        "want >= 2 saved views, got {}",
        s.saved_views_created
    );
    assert!(
        s.cycles_created >= 1,
        "want >= 1 cycle, got {}",
        s.cycles_created
    );
    assert!(
        s.activities_created >= 10,
        "want >= 10 activity rows, got {}",
        s.activities_created
    );
    assert!(
        s.expenses_created >= 5,
        "want >= 5 expenses, got {}",
        s.expenses_created
    );
    assert!(
        s.invoices_created >= 2,
        "want >= 2 invoices, got {}",
        s.invoices_created
    );
    assert!(
        s.email_refs_created >= 3,
        "want >= 3 email refs, got {}",
        s.email_refs_created
    );
    assert!(
        s.attachments_created >= 3,
        "want >= 3 attachments, got {}",
        s.attachments_created
    );
    assert!(
        s.tracks_created >= 12,
        "want >= 12 tracks, got {}",
        s.tracks_created
    );
    assert!(
        s.recipes_created >= 6,
        "want >= 6 recipes, got {}",
        s.recipes_created
    );
    assert!(
        s.cookbooks_created >= 1,
        "want >= 1 cookbook, got {}",
        s.cookbooks_created
    );
    assert!(
        s.meal_plan_entries_created >= 7,
        "want >= 7 meal plan entries, got {}",
        s.meal_plan_entries_created
    );
    assert!(
        s.shopping_list_items_created > 0,
        "want > 0 shopping list items, got {}",
        s.shopping_list_items_created
    );
    assert!(
        s.foods_created >= 30,
        "want >= 30 foods, got {}",
        s.foods_created
    );
    assert!(
        s.food_products_created >= 5,
        "want >= 5 food products, got {}",
        s.food_products_created
    );
    assert!(
        s.locations_created >= 3,
        "want >= 3 pantry-storage locations, got {}",
        s.locations_created
    );
    assert!(
        s.pantry_items_created >= 12,
        "want >= 12 pantry items, got {}",
        s.pantry_items_created
    );

    // Pantry should have at least one expiring-soon item (within 7 days).
    use chrono::Duration as ChronoDuration;
    let today = chrono::Utc::now().date_naive();
    let cutoff = today + ChronoDuration::days(7);
    let expiring = task_core::pantry::Entity::find()
        .filter(task_core::pantry::Column::ExpirationDate.is_not_null())
        .filter(task_core::pantry::Column::ExpirationDate.gte(today))
        .filter(task_core::pantry::Column::ExpirationDate.lte(cutoff))
        .all(&db)
        .await
        .expect("expiring");
    assert!(
        !expiring.is_empty(),
        "want >= 1 pantry item expiring within 7 days"
    );

    // And at least one below-min-stock row.
    let low = task_core::pantry::Entity::find()
        .filter(task_core::pantry::Column::MinStock.is_not_null())
        .all(&db)
        .await
        .expect("low")
        .into_iter()
        .filter(|r| r.min_stock.map(|m| r.quantity <= m).unwrap_or(false))
        .count();
    assert!(low >= 1, "want >= 1 below-min-stock pantry item");

    // The auto-link on recipe insert should hit > half the seeded
    // ingredients across the demo recipes.
    let total = task_core::recipe_ingredient::Entity::find()
        .all(&db)
        .await
        .expect("ingredients")
        .len();
    let linked = task_core::recipe_ingredient::Entity::find()
        .filter(task_core::recipe_ingredient::Column::FoodId.is_not_null())
        .all(&db)
        .await
        .expect("linked")
        .len();
    assert!(
        linked * 2 > total,
        "want >50% of {total} recipe ingredients auto-linked, got {linked}"
    );
}

#[tokio::test]
async fn seed_demo_data_invariants() {
    let db = task_db::init_memory().await.expect("init in-memory db");
    seed_demo_data(&db).await.expect("seed");

    // At least one Done task.
    let done_count = task::Entity::find()
        .filter(task::Column::Status.eq(task::Status::Done))
        .all(&db)
        .await
        .expect("query done tasks")
        .len();
    assert!(done_count >= 1, "want >= 1 Done task, got {done_count}");

    // At least one Cancelled task.
    let cancelled_count = task::Entity::find()
        .filter(task::Column::Status.eq(task::Status::Cancelled))
        .all(&db)
        .await
        .expect("query cancelled tasks")
        .len();
    assert!(
        cancelled_count >= 1,
        "want >= 1 Cancelled task, got {cancelled_count}"
    );

    // At least one task with a running timer (TimeEntry with end_time = None).
    let tasks = task::Entity::find()
        .all(&db)
        .await
        .expect("query all tasks");
    let running = tasks
        .iter()
        .any(|t| t.time_entries.iter().any(|e| e.end_time.is_none()));
    assert!(running, "want at least one task with a running timer");

    // Unread notification count > 0.
    let unread = notification::Entity::find()
        .filter(notification::Column::ReadAt.is_null())
        .all(&db)
        .await
        .expect("query unread notifications")
        .len();
    assert!(unread >= 1, "want >= 1 unread notification, got {unread}");
}
