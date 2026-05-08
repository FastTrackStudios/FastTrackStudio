//! Integration tests for the Open Food Facts client + the
//! `lookup_food_product_by_barcode` cache+upsert path.
//!
//! These tests stand up a tiny in-process axum mock server, point a
//! [`OpenFoodFactsClient::with_http`] at it, and exercise both the
//! standalone client and the [`FoodService`] cache flow. No live
//! network calls.

use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

use axum::{
    Router,
    extract::{Path, State},
    http::StatusCode,
    response::IntoResponse,
    routing::get,
};
use sea_orm::{ActiveModelTrait, Set};
use task_core::food::{self, FoodAliasList};
use task_core::property::JsonObject;
use task_core::provider::{OpenFoodFactsClient, OpenFoodFactsConfig};
use task_core::service::{BarcodeLookupRequest, FoodService};
use task_core::service_impl::{FoodServiceDeps, FoodServiceImpl};
use uuid::Uuid;

const BERTOLLI_BARCODE: &str = "0048500201497";
const BERTOLLI_FIXTURE: &str =
    include_str!("../../task-core/tests/openfoodfacts_fixtures/bertolli-evoo.json");

const NOT_FOUND_BODY: &str =
    r#"{"code":"0000000000000","status":0,"status_verbose":"product not found","product":null}"#;

#[derive(Clone, Default)]
struct MockState {
    hits: Arc<AtomicUsize>,
}

async fn handler(State(state): State<MockState>, Path(barcode): Path<String>) -> impl IntoResponse {
    state.hits.fetch_add(1, Ordering::SeqCst);
    if barcode == BERTOLLI_BARCODE {
        (
            StatusCode::OK,
            [("content-type", "application/json")],
            BERTOLLI_FIXTURE.to_string(),
        )
    } else {
        (
            StatusCode::OK,
            [("content-type", "application/json")],
            NOT_FOUND_BODY.to_string(),
        )
    }
}

/// Spawn the mock server, returning `(base_url, hits_counter, shutdown_drop_guard)`.
async fn spawn_mock() -> (String, Arc<AtomicUsize>, tokio::task::JoinHandle<()>) {
    let state = MockState::default();
    let hits = state.hits.clone();
    let app = Router::new()
        .route("/api/v2/product/{barcode}", get(handler))
        .with_state(state);
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0")
        .await
        .expect("bind mock");
    let addr = listener.local_addr().expect("addr");
    let handle = tokio::spawn(async move {
        // The test will drop the JoinHandle when it returns, which
        // cancels the task and stops the server.
        let _ = axum::serve(listener, app).await;
    });
    (format!("http://{addr}"), hits, handle)
}

fn build_client(base_url: &str) -> OpenFoodFactsClient {
    OpenFoodFactsClient::with_http(
        reqwest::Client::new(),
        base_url.to_string(),
        "task-server-tests/0.1".to_string(),
    )
}

#[tokio::test]
async fn lookup_returns_mapped_product_for_real_shape_payload() {
    let (base_url, hits, _server) = spawn_mock().await;
    let client = build_client(&base_url);
    let product = client
        .lookup(BERTOLLI_BARCODE)
        .await
        .expect("lookup ok")
        .expect("product present");
    assert_eq!(product.barcode, BERTOLLI_BARCODE);
    assert_eq!(
        product.product_name.as_deref(),
        Some("Bertolli Extra Virgin Olive Oil")
    );
    assert_eq!(product.brands.as_deref(), Some("Bertolli"));
    assert_eq!(product.package_size_g, Some(500.0));
    assert_eq!(product.quantity_label.as_deref(), Some("500 ml"));
    assert!(product.image_url.as_deref().unwrap().contains("front_en"));
    assert!(product.categories.iter().any(|c| c == "olive oils"));
    let kcal = product.nutrition.kcal_per_100g.unwrap();
    assert!((kcal - 884.0).abs() < 1e-6, "kcal={kcal}");
    assert_eq!(product.nutrition.fat_g, Some(100.0));
    assert_eq!(product.nutrition.source.as_deref(), Some("openfoodfacts"));
    assert_eq!(hits.load(Ordering::SeqCst), 1);
}

#[tokio::test]
async fn lookup_returns_none_when_status_zero() {
    let (base_url, _hits, _server) = spawn_mock().await;
    let client = build_client(&base_url);
    let outcome = client.lookup("0000000000000").await.expect("lookup ok");
    assert!(outcome.is_none());
}

/// Pre-create the Food row that the BERTOLLI fixture's product_name will
/// resolve against, so we don't need `auto_create_food`.
async fn insert_olive_oil_food(db: &sea_orm::DatabaseConnection) -> Uuid {
    let id = Uuid::new_v4();
    let now = chrono::Utc::now();
    food::ActiveModel {
        id: Set(id),
        name: Set("olive oil".to_string()),
        aliases: Set(FoodAliasList::from(vec!["evoo".to_string()])),
        category: Set(Some("pantry".to_string())),
        default_unit: Set(Some("ml".to_string())),
        organization: Set(Some("personal".to_string())),
        nutrition_per_100g: Set(JsonObject::default()),
        notes: Set(None),
        properties: Set(JsonObject::default()),
        created_by: Set(None),
        created_at: Set(now),
        updated_at: Set(now),
    }
    .insert(db)
    .await
    .expect("insert food");
    id
}

#[tokio::test]
async fn lookup_food_product_by_barcode_writes_then_caches() {
    let db = task_db::init_memory().await.expect("init db");
    let food_id = insert_olive_oil_food(&db).await;

    let (base_url, hits, _server) = spawn_mock().await;
    let client = Arc::new(build_client(&base_url));
    let svc = FoodServiceImpl::new(FoodServiceDeps {
        db: db.clone(),
        openfoodfacts: Some(client.clone()),
    });

    // First call: cold cache → 1 mock hit, row inserted.
    let first = svc
        .lookup_food_product_by_barcode(BarcodeLookupRequest {
            barcode: BERTOLLI_BARCODE.into(),
            organization: Some("personal".into()),
            max_age_hours: 24,
            auto_create_food: false,
            user_agent_override: None,
        })
        .await
        .expect("first lookup")
        .expect("hit");
    assert_eq!(first.barcode.as_deref(), Some(BERTOLLI_BARCODE));
    assert_eq!(first.source, "openfoodfacts");
    assert!(first.last_synced_at.is_some());
    // The product_name "Bertolli Extra Virgin Olive Oil" includes
    // "olive oil" as a substring, so find_food_by_name's contains-fallback
    // resolves it to our seeded row.
    assert_eq!(first.food_id, food_id);
    assert_eq!(hits.load(Ordering::SeqCst), 1);

    // Second call within TTL: cached row returned, no new mock hits.
    let second = svc
        .lookup_food_product_by_barcode(BarcodeLookupRequest {
            barcode: BERTOLLI_BARCODE.into(),
            organization: Some("personal".into()),
            max_age_hours: 24,
            auto_create_food: false,
            user_agent_override: None,
        })
        .await
        .expect("cached lookup")
        .expect("hit");
    assert_eq!(second.id, first.id);
    assert_eq!(
        hits.load(Ordering::SeqCst),
        1,
        "cache should suppress the second fetch"
    );

    // Force-fresh: max_age_hours=0 forces a refetch.
    let third = svc
        .lookup_food_product_by_barcode(BarcodeLookupRequest {
            barcode: BERTOLLI_BARCODE.into(),
            organization: Some("personal".into()),
            max_age_hours: 0,
            auto_create_food: false,
            user_agent_override: None,
        })
        .await
        .expect("force-refresh")
        .expect("hit");
    assert_eq!(third.id, first.id, "row id should be stable");
    assert_eq!(hits.load(Ordering::SeqCst), 2, "force-refresh re-fetches");
}

#[tokio::test]
async fn lookup_returns_none_for_unknown_barcode_without_writing() {
    let db = task_db::init_memory().await.expect("init db");
    let (base_url, _hits, _server) = spawn_mock().await;
    let client = Arc::new(build_client(&base_url));
    let svc = FoodServiceImpl::new(FoodServiceDeps {
        db: db.clone(),
        openfoodfacts: Some(client),
    });
    let outcome = svc
        .lookup_food_product_by_barcode(BarcodeLookupRequest {
            barcode: "0000000000000".into(),
            organization: Some("personal".into()),
            max_age_hours: 24,
            auto_create_food: false,
            user_agent_override: None,
        })
        .await
        .expect("ok");
    assert!(outcome.is_none());
    let count = svc
        .list_food_products(Some("personal".into()))
        .await
        .expect("list")
        .len();
    assert_eq!(count, 0);
}

#[tokio::test]
async fn lookup_without_provider_surfaces_provider_not_configured() {
    let db = task_db::init_memory().await.expect("init db");
    let svc = FoodServiceImpl::new(FoodServiceDeps {
        db: db.clone(),
        openfoodfacts: None,
    });
    let err = svc
        .lookup_food_product_by_barcode(BarcodeLookupRequest {
            barcode: BERTOLLI_BARCODE.into(),
            organization: Some("personal".into()),
            max_age_hours: 24,
            auto_create_food: false,
            user_agent_override: None,
        })
        .await
        .expect_err("expected provider_not_configured");
    let msg = format!("{err:?}");
    assert!(msg.contains("openfoodfacts"), "got {msg}");
}

#[tokio::test]
async fn lookup_auto_creates_food_when_no_match() {
    let db = task_db::init_memory().await.expect("init db");
    let (base_url, _hits, _server) = spawn_mock().await;
    let client = Arc::new(build_client(&base_url));
    let svc = FoodServiceImpl::new(FoodServiceDeps {
        db: db.clone(),
        openfoodfacts: Some(client),
    });

    // No seeded olive oil food → auto-create should fire.
    let saved = svc
        .lookup_food_product_by_barcode(BarcodeLookupRequest {
            barcode: BERTOLLI_BARCODE.into(),
            organization: Some("personal".into()),
            max_age_hours: 24,
            auto_create_food: true,
            user_agent_override: None,
        })
        .await
        .expect("ok")
        .expect("hit");
    let food = svc
        .get_food(saved.food_id)
        .await
        .expect("get_food")
        .expect("food row");
    // The first category tag "olive oils" is preferred over the
    // product_name when auto-creating.
    assert!(food.name.contains("olive"), "got name={}", food.name);

    // Without auto_create, an unmatched product should ParseError.
    let db2 = task_db::init_memory().await.expect("init db2");
    let (base2, _h2, _s2) = spawn_mock().await;
    let client2 = Arc::new(build_client(&base2));
    let svc2 = FoodServiceImpl::new(FoodServiceDeps {
        db: db2,
        openfoodfacts: Some(client2),
    });
    let err = svc2
        .lookup_food_product_by_barcode(BarcodeLookupRequest {
            barcode: BERTOLLI_BARCODE.into(),
            organization: Some("personal".into()),
            max_age_hours: 24,
            auto_create_food: false,
            user_agent_override: None,
        })
        .await
        .expect_err("expected ParseError for unresolved food");
    let msg = format!("{err:?}");
    assert!(msg.contains("auto_create_food"), "got {msg}");
}

#[tokio::test]
async fn config_defaults_apply_when_unspecified() {
    // Sanity check on the default base URL + UA. We don't make a real
    // network call — just inspect the struct.
    let client = OpenFoodFactsClient::new(OpenFoodFactsConfig::default());
    assert_eq!(client.base_url(), "https://world.openfoodfacts.net");
    assert!(client.user_agent().starts_with("task-server/"));
    assert!(client.user_agent().contains("github.com"));
}
