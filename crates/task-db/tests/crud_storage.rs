use crudcrate::{
    ApiError, ApplyUpdate, CreateResource, CrudModel, CrudService, CrudStorage, InMemoryQuery,
    InMemoryStorage, ResourceIdentity,
};
use task_core::asset::{Asset, AssetApi, AssetApiCreate, AssetStatus};
use task_core::calendar_event::{
    CalendarEvent, CalendarEventApi, CalendarEventApiCreate, CalendarEventStatus,
};
use task_core::client::{Client, ClientApi, ClientApiCreate};
use task_core::expense::{Expense, ExpenseApi, ExpenseApiCreate};
use task_core::invoice::{Invoice, InvoiceApi, InvoiceApiCreate, InvoiceLine, InvoiceStatus};
use task_core::revenue::{Revenue, RevenueApi, RevenueApiCreate};
use task_core::task::{Task, TaskApi, TaskApiCreate};
use task_core::team::{AccountStatus, TeamMember, TeamMemberApi, TeamMemberApiCreate};
use task_core::views::{SavedView, SavedViewApi, SavedViewApiCreate, ViewDisplay, ViewFilters};
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

#[tokio::test]
async fn seaorm_storage_can_back_generated_team_member_repo_models() {
    let db = task_db::init_memory()
        .await
        .expect("initialize in-memory task database");
    let member = TeamMember {
        username: "james".to_string(),
        name: "James Rodriguez".to_string(),
        role: "Session Drummer".to_string(),
        department: "music".to_string(),
        email: "james@example.com".to_string(),
        status: AccountStatus::Invited,
        aliases: vec!["james-temp".to_string()].into(),
        ..Default::default()
    };
    let create: TeamMemberApiCreate = serde_json::from_value(
        serde_json::to_value(member).expect("serialize team member create seed"),
    )
    .expect("decode team member create model");

    let created = CrudStorage::<TeamMemberApi>::create(&db, create)
        .await
        .expect("create team member through SeaORM storage");
    let loaded = CrudStorage::<TeamMemberApi>::get_one(&db, created.uuid)
        .await
        .expect("load team member through SeaORM storage");

    assert_eq!(loaded.uuid, created.uuid);
    assert_eq!(loaded.username, "james");
    assert_eq!(loaded.status, AccountStatus::Invited);
    assert_eq!(loaded.aliases.as_slice(), ["james-temp"]);
}

#[tokio::test]
async fn seaorm_storage_can_back_generated_saved_view_repo_models() {
    let db = task_db::init_memory()
        .await
        .expect("initialize in-memory task database");
    let view = SavedView {
        title: "Urgent work".to_string(),
        project: Some("Album".to_string()),
        filters: ViewFilters {
            status: vec!["open".to_string()],
            priority: vec!["urgent".to_string(), "high".to_string()],
            ..Default::default()
        },
        display: ViewDisplay {
            layout: Some("kanban".to_string()),
            group_by: Some("status".to_string()),
            visible_properties: vec!["assignee".to_string(), "due".to_string()],
            ..Default::default()
        },
        is_shared: true,
        ..Default::default()
    };
    let create: SavedViewApiCreate =
        serde_json::from_value(serde_json::to_value(view).expect("serialize saved view seed"))
            .expect("decode saved view create model");

    let created = CrudStorage::<SavedViewApi>::create(&db, create)
        .await
        .expect("create saved view through SeaORM storage");
    let loaded = CrudStorage::<SavedViewApi>::get_one(&db, created.id)
        .await
        .expect("load saved view through SeaORM storage");

    assert_eq!(loaded.id, created.id);
    assert_eq!(loaded.title, "Urgent work");
    assert_eq!(loaded.filters.priority, ["urgent", "high"]);
    assert_eq!(loaded.display.layout.as_deref(), Some("kanban"));
}

#[tokio::test]
async fn seaorm_storage_can_back_generated_asset_repo_models() {
    let db = task_db::init_memory()
        .await
        .expect("initialize in-memory task database");
    let asset = Asset {
        id: "AST-2026-0001".to_string(),
        number: 1,
        name: "Shure SM58".to_string(),
        status: AssetStatus::InUse,
        manufacturer: Some("Shure".to_string()),
        category: Some("audio".to_string()),
        organization: Some("FastTrack".to_string()),
        cost_cents: Some(9_999),
        linked_tasks: vec![task_core::task::WikiLink("Repair mic".to_string())].into(),
        ..Default::default()
    };
    let create: AssetApiCreate =
        serde_json::from_value(serde_json::to_value(asset).expect("serialize asset seed"))
            .expect("decode asset create model");

    let created = CrudStorage::<AssetApi>::create(&db, create)
        .await
        .expect("create asset through SeaORM storage");
    let loaded = CrudStorage::<AssetApi>::get_one(&db, created.uuid)
        .await
        .expect("load asset through SeaORM storage");

    assert_eq!(loaded.uuid, created.uuid);
    assert_eq!(loaded.id, "AST-2026-0001");
    assert_eq!(loaded.name, "Shure SM58");
    assert_eq!(loaded.status, AssetStatus::InUse);
    assert_eq!(loaded.linked_tasks.len(), 1);
}

#[tokio::test]
async fn seaorm_storage_can_back_generated_invoice_repo_models() {
    let db = task_db::init_memory()
        .await
        .expect("initialize in-memory task database");
    let invoice = Invoice {
        id: "INV-2026-0001".to_string(),
        number: 1,
        status: InvoiceStatus::Sent,
        client: task_core::task::WikiLink("Acme Records".to_string()),
        issue_date: chrono::NaiveDate::from_ymd_opt(2026, 5, 4).expect("valid issue date"),
        due_date: chrono::NaiveDate::from_ymd_opt(2026, 6, 3).expect("valid due date"),
        currency_code: "USD".to_string(),
        line_items: vec![InvoiceLine {
            id: "line-1".to_string(),
            task_title: "Mix".to_string(),
            description: "Album mix".to_string(),
            hours: 2.0,
            rate_cents: 10_000,
            ..Default::default()
        }]
        .into(),
        entry_ids: vec!["entry-1".to_string()].into(),
        ..Default::default()
    };
    let create: InvoiceApiCreate =
        serde_json::from_value(serde_json::to_value(invoice).expect("serialize invoice seed"))
            .expect("decode invoice create model");

    let created = CrudStorage::<InvoiceApi>::create(&db, create)
        .await
        .expect("create invoice through SeaORM storage");
    let loaded = CrudStorage::<InvoiceApi>::get_one(&db, created.uuid)
        .await
        .expect("load invoice through SeaORM storage");

    assert_eq!(loaded.uuid, created.uuid);
    assert_eq!(loaded.id, "INV-2026-0001");
    assert_eq!(loaded.status, InvoiceStatus::Sent);
    assert_eq!(loaded.line_items.len(), 1);
    assert_eq!(loaded.line_items[0].description, "Album mix");
}
