use crudcrate::{
    ApiError, ApplyUpdate, CreateResource, CrudModel, CrudService, CrudStorage, InMemoryQuery,
    InMemoryStorage, ResourceIdentity,
};
use task_core::calendar_event::{
    CalendarEvent, CalendarEventApi, CalendarEventApiCreate, CalendarEventStatus,
};
use task_core::client::{Client, ClientApi, ClientApiCreate};
use task_core::expense::{Expense, ExpenseApi, ExpenseApiCreate};
use task_core::revenue::{Revenue, RevenueApi, RevenueApiCreate};
use task_core::task::{Task, TaskApi, TaskApiCreate};
use uuid::Uuid;

#[derive(Clone, Debug, PartialEq, Eq)]
struct MemoryTask {
    id: Uuid,
    title: String,
}

struct MemoryTaskCreate {
    id: Uuid,
    title: String,
}

struct MemoryTaskUpdate {
    title: Option<String>,
}

impl CrudModel for MemoryTask {
    type Id = Uuid;
    type CreateModel = MemoryTaskCreate;
    type UpdateModel = MemoryTaskUpdate;
    type ListModel = MemoryTask;

    const RESOURCE_NAME_SINGULAR: &'static str = "memory task";
    const RESOURCE_NAME_PLURAL: &'static str = "memory tasks";
}

impl ResourceIdentity<Uuid> for MemoryTask {
    fn id(&self) -> Uuid {
        self.id
    }
}

impl CreateResource<MemoryTaskCreate> for MemoryTask {
    fn create_from(data: MemoryTaskCreate) -> Result<Self, ApiError> {
        Ok(Self {
            id: data.id,
            title: data.title,
        })
    }
}

impl ApplyUpdate<MemoryTaskUpdate> for MemoryTask {
    fn apply_update(&mut self, data: MemoryTaskUpdate) -> Result<(), ApiError> {
        if let Some(title) = data.title {
            self.title = title;
        }
        Ok(())
    }
}

#[tokio::test]
async fn crudcrate_storage_can_back_task_resources_without_seaorm() {
    let storage = InMemoryStorage::<MemoryTask>::new();
    let id = Uuid::new_v4();

    let created = CrudService::create(
        &storage,
        MemoryTaskCreate {
            id,
            title: "Draft".to_string(),
        },
    )
    .await
    .expect("create in memory task");
    assert_eq!(created.title, "Draft");

    let updated = CrudService::update(
        &storage,
        id,
        MemoryTaskUpdate {
            title: Some("Ready".to_string()),
        },
    )
    .await
    .expect("update in memory task");
    assert_eq!(updated.title, "Ready");

    let listed = CrudStorage::get_all(&storage, InMemoryQuery::all())
        .await
        .expect("list in memory tasks");
    assert_eq!(listed, vec![updated]);
}

#[tokio::test]
async fn seaorm_storage_can_back_generated_task_repo_models() {
    let db = task_db::init_memory()
        .await
        .expect("initialize in-memory task database");
    let task = Task {
        title: "SQLite backed task".to_string(),
        ..Default::default()
    };
    let create: TaskApiCreate =
        serde_json::from_value(serde_json::to_value(task).expect("serialize task create seed"))
            .expect("decode task create model");

    let created = CrudStorage::<TaskApi>::create(&db, create)
        .await
        .expect("create task through SeaORM storage");
    let loaded = CrudStorage::<TaskApi>::get_one(&db, created.id)
        .await
        .expect("load task through SeaORM storage");

    assert_eq!(loaded.id, created.id);
    assert_eq!(loaded.title, "SQLite backed task");
}

#[tokio::test]
async fn seaorm_storage_can_back_generated_client_repo_models() {
    let db = task_db::init_memory()
        .await
        .expect("initialize in-memory task database");
    let client = Client {
        name: "Acme Records".to_string(),
        currency_code: "USD".to_string(),
        email: Some("billing@example.com".to_string()),
        ..Default::default()
    };
    let create: ClientApiCreate =
        serde_json::from_value(serde_json::to_value(client).expect("serialize client create seed"))
            .expect("decode client create model");

    let created = CrudStorage::<ClientApi>::create(&db, create)
        .await
        .expect("create client through SeaORM storage");
    let loaded = CrudStorage::<ClientApi>::get_one(&db, created.id)
        .await
        .expect("load client through SeaORM storage");

    assert_eq!(loaded.id, created.id);
    assert_eq!(loaded.name, "Acme Records");
    assert_eq!(loaded.email.as_deref(), Some("billing@example.com"));
}

#[tokio::test]
async fn seaorm_storage_can_back_generated_expense_repo_models() {
    let db = task_db::init_memory()
        .await
        .expect("initialize in-memory task database");
    let expense = Expense {
        id: "EXP-2026-0001".to_string(),
        number: 1,
        description: "Studio cables".to_string(),
        amount_cents: 12_500,
        currency_code: "USD".to_string(),
        date: chrono::NaiveDate::from_ymd_opt(2026, 5, 4).expect("valid test date"),
        ..Default::default()
    };
    let create: ExpenseApiCreate = serde_json::from_value(
        serde_json::to_value(expense).expect("serialize expense create seed"),
    )
    .expect("decode expense create model");

    let created = CrudStorage::<ExpenseApi>::create(&db, create)
        .await
        .expect("create expense through SeaORM storage");
    let loaded = CrudStorage::<ExpenseApi>::get_one(&db, created.uuid)
        .await
        .expect("load expense through SeaORM storage");

    assert_eq!(loaded.uuid, created.uuid);
    assert_eq!(loaded.id, "EXP-2026-0001");
    assert_eq!(loaded.description, "Studio cables");
    assert_eq!(loaded.amount_cents, 12_500);
}

#[tokio::test]
async fn seaorm_storage_can_back_generated_revenue_repo_models() {
    let db = task_db::init_memory()
        .await
        .expect("initialize in-memory task database");
    let revenue = Revenue {
        id: "REV-2026-0001".to_string(),
        number: 1,
        description: "Album mix payment".to_string(),
        amount_cents: 250_000,
        currency_code: "USD".to_string(),
        date: chrono::NaiveDate::from_ymd_opt(2026, 5, 4).expect("valid test date"),
        invoice_id: Some("INV-2026-0001".to_string()),
        ..Default::default()
    };
    let create: RevenueApiCreate = serde_json::from_value(
        serde_json::to_value(revenue).expect("serialize revenue create seed"),
    )
    .expect("decode revenue create model");

    let created = CrudStorage::<RevenueApi>::create(&db, create)
        .await
        .expect("create revenue through SeaORM storage");
    let loaded = CrudStorage::<RevenueApi>::get_one(&db, created.uuid)
        .await
        .expect("load revenue through SeaORM storage");

    assert_eq!(loaded.uuid, created.uuid);
    assert_eq!(loaded.id, "REV-2026-0001");
    assert_eq!(loaded.invoice_id.as_deref(), Some("INV-2026-0001"));
    assert_eq!(loaded.amount_cents, 250_000);
}

#[tokio::test]
async fn seaorm_storage_can_back_generated_calendar_event_repo_models() {
    let db = task_db::init_memory()
        .await
        .expect("initialize in-memory task database");
    let start = chrono::DateTime::parse_from_rfc3339("2026-05-04T15:00:00Z")
        .expect("valid test datetime")
        .to_utc();
    let event = CalendarEvent {
        id: Some("event-1".to_string()),
        title: "Planning session".to_string(),
        description: Some("Review delivery plan".to_string()),
        location: Some("Studio A".to_string()),
        start,
        status: CalendarEventStatus::Tentative,
        attendees: vec!["agent".to_string(), "codywright".to_string()].into(),
        ..Default::default()
    };
    let create: CalendarEventApiCreate = serde_json::from_value(
        serde_json::to_value(event).expect("serialize calendar event create seed"),
    )
    .expect("decode calendar event create model");

    let created = CrudStorage::<CalendarEventApi>::create(&db, create)
        .await
        .expect("create calendar event through SeaORM storage");
    let loaded = CrudStorage::<CalendarEventApi>::get_one(&db, created.uuid)
        .await
        .expect("load calendar event through SeaORM storage");

    assert_eq!(loaded.uuid, created.uuid);
    assert_eq!(loaded.title, "Planning session");
    assert_eq!(loaded.status, CalendarEventStatus::Tentative);
    assert_eq!(loaded.attendees.len(), 2);
}
