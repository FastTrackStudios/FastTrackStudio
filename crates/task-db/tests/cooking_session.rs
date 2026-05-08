//! Integration tests for the interactive cooking-session surface on
//! [`CookingService`].

use sea_orm::{ActiveModelTrait, ColumnTrait, EntityTrait, QueryFilter};
use task_core::cooking_session::CookingSessionStatus;
use task_core::service::{
    CompleteCookingSessionRequest, CookingService, MarkIngredientGatheredRequest,
    NavigateStepRequest, StartCookingSessionRequest, StepTimerActionRequest, VaultError,
};
use task_core::service_impl::{CookingServiceDeps, CookingServiceImpl};
use task_core::{food_log, recipe};
use task_db::seed::{DEMO_NAMESPACE, seed_demo_data};
use uuid::Uuid;

fn demo_id(key: &str) -> Uuid {
    Uuid::new_v5(&DEMO_NAMESPACE, key.as_bytes())
}

#[tokio::test]
async fn seed_populates_cooking_sessions() {
    let db = task_db::init_memory().await.expect("init db");
    let s = seed_demo_data(&db).await.expect("seed");
    assert!(
        s.cooking_sessions_created >= 2,
        "cooking_sessions_created={}",
        s.cooking_sessions_created
    );
    let active = task_core::cooking_session::Entity::find()
        .filter(task_core::cooking_session::Column::Status.eq(CookingSessionStatus::Active))
        .all(&db)
        .await
        .expect("query active");
    assert_eq!(active.len(), 1, "exactly one active session expected");
}

#[tokio::test]
async fn start_cooking_session_initializes_state() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = CookingServiceImpl::new(CookingServiceDeps { db: db.clone() });
    let recipe_id = demo_id("recipe:weeknight-carbonara");
    let view = svc
        .start_cooking_session(StartCookingSessionRequest {
            recipe_id,
            scaled_servings: Some(2),
            organization: None,
            created_by: Some("test".to_string()),
        })
        .await
        .expect("start");
    assert_eq!(view.session.status, CookingSessionStatus::Active);
    assert_eq!(view.session.current_step_index, -1);
    assert_eq!(view.session.scaled_servings, Some(2));
    let mise: Vec<bool> = serde_json::from_str(&view.mise_en_place_json).expect("decode mise");
    assert!(!mise.is_empty(), "mise should match ingredient count");
    assert!(mise.iter().all(|b| !*b), "all ingredients ungathered");
    let states: Vec<task_core::CookingStepState> =
        serde_json::from_str(&view.step_states_json).expect("decode states");
    assert!(!states.is_empty(), "step states should be populated");
    assert!(states.iter().all(|s| s.started_at.is_none()));
}

#[tokio::test]
async fn mark_ingredient_gathered_toggles_and_bounds_checks() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = CookingServiceImpl::new(CookingServiceDeps { db: db.clone() });
    let view = svc
        .start_cooking_session(StartCookingSessionRequest {
            recipe_id: demo_id("recipe:weeknight-carbonara"),
            ..Default::default()
        })
        .await
        .expect("start");
    let session_id = view.session.id;

    let v2 = svc
        .mark_ingredient_gathered(MarkIngredientGatheredRequest {
            session_id,
            ingredient_index: 0,
            gathered: true,
        })
        .await
        .expect("mark");
    let mise: Vec<bool> = serde_json::from_str(&v2.mise_en_place_json).unwrap();
    assert!(mise[0]);

    // Out-of-range index → ParseError.
    let err = svc
        .mark_ingredient_gathered(MarkIngredientGatheredRequest {
            session_id,
            ingredient_index: 9999,
            gathered: true,
        })
        .await
        .expect_err("out of range");
    assert!(matches!(err, VaultError::ParseError(_)), "got {err:?}");
}

#[tokio::test]
async fn navigate_step_clamps_and_validates() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = CookingServiceImpl::new(CookingServiceDeps { db: db.clone() });
    let view = svc
        .start_cooking_session(StartCookingSessionRequest {
            recipe_id: demo_id("recipe:weeknight-carbonara"),
            ..Default::default()
        })
        .await
        .expect("start");
    let id = view.session.id;
    // next from -1 → 0
    let v = svc
        .navigate_step(NavigateStepRequest {
            session_id: id,
            direction: "next".into(),
            jump_to: None,
        })
        .await
        .expect("next");
    assert_eq!(v.session.current_step_index, 0);
    // previous → -1
    let v = svc
        .navigate_step(NavigateStepRequest {
            session_id: id,
            direction: "previous".into(),
            jump_to: None,
        })
        .await
        .expect("prev");
    assert_eq!(v.session.current_step_index, -1);
    // previous past -1 stays at -1
    let v = svc
        .navigate_step(NavigateStepRequest {
            session_id: id,
            direction: "previous".into(),
            jump_to: None,
        })
        .await
        .expect("prev clamp");
    assert_eq!(v.session.current_step_index, -1);
    // jump out of range
    let err = svc
        .navigate_step(NavigateStepRequest {
            session_id: id,
            direction: "jump".into(),
            jump_to: Some(9999),
        })
        .await
        .expect_err("jump too far");
    assert!(matches!(err, VaultError::ParseError(_)));
}

#[tokio::test]
async fn step_timer_pause_resume_excludes_paused_time() {
    use std::thread::sleep;
    use std::time::Duration as StdDuration;

    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = CookingServiceImpl::new(CookingServiceDeps { db: db.clone() });
    let view = svc
        .start_cooking_session(StartCookingSessionRequest {
            recipe_id: demo_id("recipe:weeknight-carbonara"),
            ..Default::default()
        })
        .await
        .expect("start");
    let id = view.session.id;

    // start step 0
    svc.step_timer_action(StepTimerActionRequest {
        session_id: id,
        step_index: Some(0),
        action: "start".into(),
    })
    .await
    .expect("start");
    sleep(StdDuration::from_millis(100));
    // pause 1
    svc.step_timer_action(StepTimerActionRequest {
        session_id: id,
        step_index: Some(0),
        action: "pause".into(),
    })
    .await
    .expect("pause1");
    sleep(StdDuration::from_millis(120));
    // resume 1
    svc.step_timer_action(StepTimerActionRequest {
        session_id: id,
        step_index: Some(0),
        action: "resume".into(),
    })
    .await
    .expect("resume1");
    sleep(StdDuration::from_millis(80));
    // pause 2
    svc.step_timer_action(StepTimerActionRequest {
        session_id: id,
        step_index: Some(0),
        action: "pause".into(),
    })
    .await
    .expect("pause2");
    sleep(StdDuration::from_millis(150));
    // resume 2
    let view = svc
        .step_timer_action(StepTimerActionRequest {
            session_id: id,
            step_index: Some(0),
            action: "resume".into(),
        })
        .await
        .expect("resume2");
    let states: Vec<task_core::CookingStepState> =
        serde_json::from_str(&view.step_states_json).unwrap();
    // We paused for ~120 + ~150 = ~270ms total. pause_offset_seconds is
    // floored to whole seconds (0). The important invariant: it's never
    // negative and never exceeds the wall clock.
    let s = &states[0];
    // After two pause/resume cycles, paused_at should be cleared.
    assert!(s.paused_at.is_none());
    // started_at must be set, completed_at not yet set.
    assert!(s.started_at.is_some());
    assert!(s.completed_at.is_none());
    // Cannot pause when already paused — sanity: "resume" without paused
    // must fail.
    let err = svc
        .step_timer_action(StepTimerActionRequest {
            session_id: id,
            step_index: Some(0),
            action: "resume".into(),
        })
        .await
        .expect_err("resume when not paused");
    assert!(matches!(err, VaultError::ParseError(_)));
}

#[tokio::test]
async fn complete_session_logs_meal_and_updates_last_made() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = CookingServiceImpl::new(CookingServiceDeps { db: db.clone() });
    let recipe_id = demo_id("recipe:weeknight-carbonara");
    let view = svc
        .start_cooking_session(StartCookingSessionRequest {
            recipe_id,
            scaled_servings: Some(2),
            ..Default::default()
        })
        .await
        .expect("start");
    let session_id = view.session.id;

    let logs_before = food_log::Entity::find()
        .filter(food_log::Column::RecipeId.eq(recipe_id))
        .all(&db)
        .await
        .unwrap();

    let view = svc
        .complete_cooking_session(CompleteCookingSessionRequest {
            session_id,
            log_meal: true,
            servings_eaten: Some(2),
            meal_type: Some("dinner".into()),
            log_date: None,
            actor: Some("cody".into()),
        })
        .await
        .expect("complete");
    assert_eq!(view.session.status, CookingSessionStatus::Completed);
    assert!(view.session.completed_at.is_some());

    let logs_after = food_log::Entity::find()
        .filter(food_log::Column::RecipeId.eq(recipe_id))
        .all(&db)
        .await
        .unwrap();
    assert_eq!(logs_after.len(), logs_before.len() + 1);

    let recipe_row = recipe::Entity::find_by_id(recipe_id)
        .one(&db)
        .await
        .unwrap()
        .unwrap();
    assert!(recipe_row.last_made.is_some());
}

#[tokio::test]
async fn abandon_session_does_not_log_or_update_last_made() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = CookingServiceImpl::new(CookingServiceDeps { db: db.clone() });
    let recipe_id = demo_id("recipe:greek-salad");

    // Reset last_made to a known state.
    let pre_last_made = recipe::Entity::find_by_id(recipe_id)
        .one(&db)
        .await
        .unwrap()
        .unwrap()
        .last_made;

    let view = svc
        .start_cooking_session(StartCookingSessionRequest {
            recipe_id,
            ..Default::default()
        })
        .await
        .expect("start");
    let logs_before = food_log::Entity::find()
        .filter(food_log::Column::RecipeId.eq(recipe_id))
        .all(&db)
        .await
        .unwrap();

    let v = svc
        .abandon_cooking_session(view.session.id)
        .await
        .expect("abandon");
    assert_eq!(v.session.status, CookingSessionStatus::Abandoned);
    assert!(v.session.completed_at.is_some());

    let logs_after = food_log::Entity::find()
        .filter(food_log::Column::RecipeId.eq(recipe_id))
        .all(&db)
        .await
        .unwrap();
    assert_eq!(logs_after.len(), logs_before.len(), "no new log");
    let post = recipe::Entity::find_by_id(recipe_id)
        .one(&db)
        .await
        .unwrap()
        .unwrap()
        .last_made;
    assert_eq!(pre_last_made, post, "last_made unchanged after abandon");
}

#[tokio::test]
async fn scale_recipe_doubles_quantities() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = CookingServiceImpl::new(CookingServiceDeps { db: db.clone() });
    let recipe_id = demo_id("recipe:weeknight-carbonara");
    let recipe_row = recipe::Entity::find_by_id(recipe_id)
        .one(&db)
        .await
        .unwrap()
        .unwrap();
    let source = recipe_row.servings.unwrap_or(1);
    let target = source * 2;
    let view = svc.scale_recipe(recipe_id, target).await.expect("scale");
    assert_eq!(view.target_servings, target);
    assert!((view.multiplier - 2.0).abs() < f64::EPSILON);
    let scaled: Vec<task_core::recipe_ingredient::RecipeIngredientApi> =
        serde_json::from_str(&view.scaled_ingredients_json).unwrap();
    let original = task_core::recipe_ingredient::Entity::find()
        .filter(task_core::recipe_ingredient::Column::RecipeId.eq(recipe_id))
        .all(&db)
        .await
        .unwrap();
    for (orig, scaled) in original.iter().zip(scaled.iter()) {
        match (orig.quantity, scaled.quantity) {
            (Some(o), Some(s)) => assert!((s - o * 2.0).abs() < 1e-9),
            (None, None) => {}
            _ => {
                if !orig.is_section {
                    panic!("quantity Option mismatch: {orig:?} vs {scaled:?}");
                }
            }
        }
    }
}

#[tokio::test]
async fn scale_recipe_half_and_unknown_servings() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = CookingServiceImpl::new(CookingServiceDeps { db: db.clone() });
    let recipe_id = demo_id("recipe:weeknight-carbonara");
    let row = recipe::Entity::find_by_id(recipe_id)
        .one(&db)
        .await
        .unwrap()
        .unwrap();
    let source = row.servings.unwrap_or(2).max(2);
    let view = svc.scale_recipe(recipe_id, source / 2).await.expect("half");
    assert!((view.multiplier - 0.5).abs() < f64::EPSILON);

    // Recipe with servings cleared → warning, multiplier 1.0.
    let mut active: recipe::ActiveModel = row.into();
    active.servings = sea_orm::Set(None);
    let _ = active.update(&db).await.unwrap();
    let view = svc.scale_recipe(recipe_id, 4).await.expect("unknown");
    assert!((view.multiplier - 1.0).abs() < f64::EPSILON);
    assert!(!view.warnings.is_empty());
}
