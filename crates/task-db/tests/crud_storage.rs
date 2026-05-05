use crudcrate::{
    ApiError, ApplyUpdate, CreateResource, CrudModel, CrudService, CrudStorage, InMemoryQuery,
    InMemoryStorage, ResourceIdentity,
};
use task_core::asset::{Asset, AssetApi, AssetApiCreate, AssetStatus};
use task_core::calendar_event::{
    CalendarEvent, CalendarEventApi, CalendarEventApiCreate, CalendarEventStatus,
};
use task_core::client::{Client, ClientApi, ClientApiCreate};
use task_core::cycle::{Cycle, CycleApi, CycleApiCreate, CycleStatus};
use task_core::email::{EmailRef, EmailRefApi, EmailRefApiCreate};
use task_core::expense::{Expense, ExpenseApi, ExpenseApiCreate};
use task_core::integration::{
    Integration, IntegrationApi, IntegrationApiCreate, ProjectTemplate, StatusDef, TaskTemplate,
};
use task_core::invoice::{Invoice, InvoiceApi, InvoiceApiCreate, InvoiceLine, InvoiceStatus};
use task_core::location::{Location, LocationApi, LocationApiCreate, Space, VenueDefault};
use task_core::module::{Module, ModuleApi, ModuleApiCreate, ModuleStatus};
use task_core::people::{ContactMethod, Person, PersonApi, PersonApiCreate, ProviderRef};
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

#[tokio::test]
async fn seaorm_storage_can_back_generated_cycle_repo_models() {
    let db = task_db::init_memory()
        .await
        .expect("initialize in-memory task database");
    let cycle = Cycle {
        title: "May Sprint".to_string(),
        description: Some("Core migration work".to_string()),
        start_date: chrono::NaiveDate::from_ymd_opt(2026, 5, 1),
        end_date: chrono::NaiveDate::from_ymd_opt(2026, 5, 15),
        owned_by: Some("cody".to_string()),
        tasks: vec!["TASK-d9k".to_string()].into(),
        status: CycleStatus::Active,
        total_tasks: Some(4),
        completed_tasks: Some(2),
        ..Default::default()
    };
    let create: CycleApiCreate =
        serde_json::from_value(serde_json::to_value(cycle).expect("serialize cycle seed"))
            .expect("decode cycle create model");

    let created = CrudStorage::<CycleApi>::create(&db, create)
        .await
        .expect("create cycle through SeaORM storage");
    let loaded = CrudStorage::<CycleApi>::get_one(&db, created.id)
        .await
        .expect("load cycle through SeaORM storage");

    assert_eq!(loaded.id, created.id);
    assert_eq!(loaded.title, "May Sprint");
    assert_eq!(loaded.status, CycleStatus::Active);
    assert_eq!(loaded.tasks.as_slice(), ["TASK-d9k"]);
}

#[tokio::test]
async fn seaorm_storage_can_back_generated_location_repo_models() {
    let db = task_db::init_memory()
        .await
        .expect("initialize in-memory task database");
    let location = Location {
        id: Some("venue-1".to_string()),
        name: "Studio A".to_string(),
        city: Some("Los Angeles".to_string()),
        state: Some("CA".to_string()),
        venue_type: Some("studio".to_string()),
        tags: vec!["recording".to_string()].into(),
        spaces: vec![Space {
            name: "Vocal Booth".to_string(),
            capacity: Some(2),
            default_files: vec![VenueDefault {
                kind: "input_list".to_string(),
                path: "Inputs.md".to_string(),
                ..Default::default()
            }]
            .into(),
            ..Default::default()
        }]
        .into(),
        ..Default::default()
    };
    let create: LocationApiCreate =
        serde_json::from_value(serde_json::to_value(location).expect("serialize location seed"))
            .expect("decode location create model");

    let created = CrudStorage::<LocationApi>::create(&db, create)
        .await
        .expect("create location through SeaORM storage");
    let loaded = CrudStorage::<LocationApi>::get_one(&db, created.uuid)
        .await
        .expect("load location through SeaORM storage");

    assert_eq!(loaded.uuid, created.uuid);
    assert_eq!(loaded.name, "Studio A");
    assert_eq!(loaded.spaces.len(), 1);
    assert_eq!(loaded.spaces[0].name, "Vocal Booth");
}

#[tokio::test]
async fn seaorm_storage_can_back_generated_module_repo_models() {
    let db = task_db::init_memory()
        .await
        .expect("initialize in-memory task database");
    let module = Module {
        title: "Storage migration".to_string(),
        description: Some("Move remaining models to generated repos".to_string()),
        lead: Some("cody".to_string()),
        members: vec!["agent".to_string(), "cody".to_string()].into(),
        tasks: vec!["TASK-d9k".to_string()].into(),
        status: ModuleStatus::InProgress,
        sort_order: Some(1.0),
        ..Default::default()
    };
    let create: ModuleApiCreate =
        serde_json::from_value(serde_json::to_value(module).expect("serialize module seed"))
            .expect("decode module create model");

    let created = CrudStorage::<ModuleApi>::create(&db, create)
        .await
        .expect("create module through SeaORM storage");
    let loaded = CrudStorage::<ModuleApi>::get_one(&db, created.id)
        .await
        .expect("load module through SeaORM storage");

    assert_eq!(loaded.id, created.id);
    assert_eq!(loaded.title, "Storage migration");
    assert_eq!(loaded.status, ModuleStatus::InProgress);
    assert_eq!(loaded.members.as_slice(), ["agent", "cody"]);
}

#[tokio::test]
async fn seaorm_storage_can_back_generated_email_ref_repo_models() {
    let db = task_db::init_memory()
        .await
        .expect("initialize in-memory task database");
    let email = EmailRef {
        message_id: "<msg-1@example.com>".to_string(),
        subject: "Mix notes".to_string(),
        from: "Client <client@example.com>".to_string(),
        to: vec!["studio@example.com".to_string()].into(),
        date: chrono::DateTime::parse_from_rfc3339("2026-05-04T15:00:00Z")
            .expect("valid email date")
            .to_utc(),
        has_attachments: true,
        attachment_count: 2,
        user_tags: vec!["client".to_string()].into(),
        ..Default::default()
    };
    let create: EmailRefApiCreate =
        serde_json::from_value(serde_json::to_value(email).expect("serialize email ref seed"))
            .expect("decode email ref create model");

    let created = CrudStorage::<EmailRefApi>::create(&db, create)
        .await
        .expect("create email ref through SeaORM storage");
    let loaded = CrudStorage::<EmailRefApi>::get_one(&db, created.uuid)
        .await
        .expect("load email ref through SeaORM storage");

    assert_eq!(loaded.uuid, created.uuid);
    assert_eq!(loaded.message_id, "<msg-1@example.com>");
    assert_eq!(loaded.to.as_slice(), ["studio@example.com"]);
    assert_eq!(loaded.attachment_count, 2);
}

#[tokio::test]
async fn seaorm_storage_can_back_generated_person_repo_models() {
    let db = task_db::init_memory()
        .await
        .expect("initialize in-memory task database");
    let person = Person {
        id: Some("person-1".to_string()),
        display_name: "Ada Lovelace".to_string(),
        given_name: Some("Ada".to_string()),
        family_name: Some("Lovelace".to_string()),
        organization: Some("Analytical Engines".to_string()),
        title: Some("Founder".to_string()),
        contact_methods: vec![ContactMethod {
            kind: "email".to_string(),
            value: "ada@example.com".to_string(),
            primary: true,
            ..Default::default()
        }]
        .into(),
        provider_refs: vec![ProviderRef {
            provider: "carddav".to_string(),
            uid: Some("person-1".to_string()),
            ..Default::default()
        }]
        .into(),
        ..Default::default()
    };
    let create: PersonApiCreate =
        serde_json::from_value(serde_json::to_value(person).expect("serialize person seed"))
            .expect("decode person create model");

    let created = CrudStorage::<PersonApi>::create(&db, create)
        .await
        .expect("create person through SeaORM storage");
    let loaded = CrudStorage::<PersonApi>::get_one(&db, created.uuid)
        .await
        .expect("load person through SeaORM storage");

    assert_eq!(loaded.uuid, created.uuid);
    assert_eq!(loaded.display_name, "Ada Lovelace");
    assert_eq!(loaded.contact_methods.len(), 1);
    assert_eq!(loaded.provider_refs[0].provider, "carddav");
}

#[tokio::test]
async fn seaorm_storage_can_back_generated_integration_repo_models() {
    let db = task_db::init_memory()
        .await
        .expect("initialize in-memory task database");
    let integration = Integration {
        name: "studio".to_string(),
        statuses: vec![StatusDef {
            name: "Tracking".to_string(),
            is_completion: false,
            color: Some("#0099ff".to_string()),
        }]
        .into(),
        project_templates: vec![ProjectTemplate {
            name: "Album".to_string(),
            description: Some("Album production".to_string()),
            tasks: vec![TaskTemplate {
                title: "Create session".to_string(),
                status: Some("Tracking".to_string()),
                tags: vec!["studio".to_string()].into(),
                ..Default::default()
            }]
            .into(),
        }]
        .into(),
        area_conventions: vec!["Music".to_string()].into(),
        context_conventions: vec!["studio".to_string()].into(),
        ..Default::default()
    };
    let create: IntegrationApiCreate = serde_json::from_value(
        serde_json::to_value(integration).expect("serialize integration seed"),
    )
    .expect("decode integration create model");

    let created = CrudStorage::<IntegrationApi>::create(&db, create)
        .await
        .expect("create integration through SeaORM storage");
    let loaded = CrudStorage::<IntegrationApi>::get_one(&db, created.id)
        .await
        .expect("load integration through SeaORM storage");

    assert_eq!(loaded.id, created.id);
    assert_eq!(loaded.name, "studio");
    assert_eq!(loaded.statuses[0].name, "Tracking");
    assert_eq!(loaded.project_templates[0].tasks[0].title, "Create session");
}
