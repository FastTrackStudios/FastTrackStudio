use std::net::SocketAddr;
use std::path::PathBuf;
use std::process::Stdio;
use std::time::{Duration, SystemTime, UNIX_EPOCH};
use std::{fmt::Debug, future::Future};

use chrono::{TimeZone, Utc};
use serde::{Serialize, de::DeserializeOwned};
use task_core::{
    CalendarEvent, CalendarEventStatus, Client, Filter, InboxCaptureRequest, InboxPromoteRequest,
    InvoiceCreateRequest, InvoicePaymentRequest, Priority, Project, Query, Sort, Status, Task,
    TimeEntryFilter, TimeLogRequest, TimeStartRequest, WikiLink,
};
use tokio::net::{TcpListener, TcpStream};
use tokio::process::{Child, Command};
use tokio::time::timeout;

const TEST_TOKEN: &str = "task-server-e2e-session-token";

#[tokio::test]
async fn authenticated_system_service_reports_capabilities_and_health() {
    let server_bin = env!("CARGO_BIN_EXE_task-server");

    let fixture = TestFixture::new();
    let bind_addr = free_loopback_addr().await;
    let mut server = RunningServer::spawn(server_bin, bind_addr, &fixture);

    wait_for_tcp(bind_addr).await;

    let vox_url = format!("ws://{bind_addr}/vox?token={TEST_TOKEN}&organization_id=org_fts");
    let system: task_core::service::SystemServiceClient =
        timeout(Duration::from_secs(10), vox::connect(&vox_url).establish())
            .await
            .expect("authenticated Vox connection should not time out")
            .expect("authenticated Vox connection should establish");

    let capabilities = timeout(Duration::from_secs(10), system.capabilities())
        .await
        .expect("SystemService capabilities should not time out")
        .expect("SystemService capabilities should return");
    assert_eq!(capabilities.package, "task-server");
    assert_eq!(capabilities.protocol_version, 1);
    assert!(
        capabilities
            .services
            .iter()
            .any(|svc| svc == "SystemService")
    );
    assert!(
        capabilities
            .features
            .iter()
            .any(|feature| feature == "generated-repos")
    );
    assert!(!capabilities.vault.exists);

    let health = timeout(Duration::from_secs(10), system.health(false))
        .await
        .expect("SystemService health should not time out")
        .expect("SystemService health should return");
    assert!(!health.deep);
    assert!(health.checks.iter().any(|check| check.code == "SQLITE_OK"));

    server.stop().await;
}

#[tokio::test]
#[ignore = "full Task Vox payloads need DTOs or codec support for the canonical SeaORM task model"]
async fn authenticated_core_services_smoke_over_vox() {
    let server_bin = env!("CARGO_BIN_EXE_task-server");

    let fixture = TestFixture::new();
    fixture.seed_project(Project {
        title: "E2E Project".to_string(),
        ..Default::default()
    });
    fixture.seed_task(Task {
        id: uuid::Uuid::parse_str("00000000-0000-4000-8000-000000000801").unwrap(),
        title: "E2E seeded task".to_string(),
        status: Status::Open,
        priority: Priority::High,
        projects: vec![WikiLink("E2E Project".to_string())].into(),
        assignee: Some("agent".to_string()),
        body: "Seeded before task-server starts so remote read paths have data.".to_string(),
        ..Default::default()
    });

    let bind_addr = free_loopback_addr().await;
    let mut server = RunningServer::spawn(server_bin, bind_addr, &fixture);

    wait_for_tcp(bind_addr).await;
    let vox_url = format!("ws://{bind_addr}/vox?token={TEST_TOKEN}&organization_id=org_fts");

    let task_service: task_core::service::TaskServiceClient = connect_service(&vox_url).await;
    let task_repo: task_core::task::TaskRepoClient = connect_service(&vox_url).await;
    let project_repo: task_core::project::ProjectRepoClient = connect_service(&vox_url).await;
    let client_repo: task_core::client::ClientRepoClient = connect_service(&vox_url).await;
    let invoice_repo: task_core::invoice::InvoiceRepoClient = connect_service(&vox_url).await;
    let calendar_event_repo: task_core::calendar_event::CalendarEventRepoClient =
        connect_service(&vox_url).await;
    let inbox_service: task_core::service::InboxServiceClient = connect_service(&vox_url).await;
    let project_service: task_core::service::ProjectServiceClient = connect_service(&vox_url).await;
    let time_service: task_core::service::TimeServiceClient = connect_service(&vox_url).await;
    let people_service: task_core::service::PeopleServiceClient = connect_service(&vox_url).await;
    let conversation_service: task_core::service::ConversationServiceClient =
        connect_service(&vox_url).await;
    let operating_service: task_core::service::OperatingServiceClient =
        connect_service(&vox_url).await;
    let invoice_service: task_core::service::InvoiceServiceClient = connect_service(&vox_url).await;
    let calendar_service: task_core::service::CalendarServiceClient =
        connect_service(&vox_url).await;
    let activity_service: task_core::service::ActivityServiceClient =
        connect_service(&vox_url).await;
    let mail_service: task_core::service::MailServiceClient = connect_service(&vox_url).await;

    let client = service_call(
        "create_client",
        client_repo.create_client(model_to_api(&Client {
            name: "E2E Client".to_string(),
            default_hourly_rate: Some(12_000),
            currency_code: "USD".to_string(),
            email: Some("billing@example.com".to_string()),
            ..Default::default()
        })),
    )
    .await;
    let client: Client = api_to_model(client);
    assert_eq!(client.name, "E2E Client");
    let clients: Vec<Client> = service_call(
        "list_clients",
        client_repo.list_clients(None, None, None, Some(100)),
    )
    .await
    .into_iter()
    .map(api_to_model)
    .collect();
    assert!(clients.iter().any(|client| client.name == "E2E Client"));
    assert!(service_error("list_people", people_service.list_people(None)).await);
    assert!(
        service_error(
            "list_conversations",
            conversation_service.list_conversations()
        )
        .await
    );
    let operating = service_call("operating_model", operating_service.operating_model()).await;
    assert_eq!(operating.today.len(), 10);

    let captured = service_call(
        "capture",
        inbox_service.capture(InboxCaptureRequest {
            text: "Review inbox capture flow 2026-05-01 !high #ops @desk".to_string(),
            actor: Some("agent".to_string()),
            source: Some("e2e".to_string()),
            kind: None,
        }),
    )
    .await;
    assert_eq!(captured.kind, "inbox");
    assert_eq!(captured.priority, "high");
    assert_eq!(captured.source.as_deref(), Some("e2e"));
    let inbox_items = service_call("list_inbox", inbox_service.list_inbox()).await;
    assert!(
        inbox_items
            .iter()
            .any(|item| item.id == captured.id && item.title == captured.title)
    );
    let promoted = service_call(
        "promote",
        inbox_service.promote(InboxPromoteRequest {
            reference: captured.id.clone().expect("captured item should have id"),
            kind: Some("commitment".to_string()),
            project: Some("E2E Project".to_string()),
            status: Some("planned".to_string()),
            assignee: Some("agent".to_string()),
            due: None,
            scheduled: Some("2026-05-01".to_string()),
            add_tags: vec!["review".to_string()],
            actor: Some("agent".to_string()),
        }),
    )
    .await;
    assert_eq!(promoted.kind, "commitment");
    assert_eq!(promoted.status, "planned");
    assert!(
        promoted
            .projects
            .iter()
            .any(|project| project == "E2E Project")
    );
    assert!(promoted.tags.iter().any(|tag| tag == "review"));
    assert!(!promoted.tags.iter().any(|tag| tag == "inbox"));
    assert_eq!(promoted.due.as_deref(), Some("2026-05-01"));
    assert_eq!(promoted.scheduled.as_deref(), Some("2026-05-01"));

    let created = service_call(
        "create_task",
        task_repo.create_task(model_to_api(&Task {
            title: "E2E remote task".to_string(),
            status: Status::Open,
            priority: Priority::High,
            projects: vec![WikiLink("E2E Project".to_string())].into(),
            assignee: Some("agent".to_string()),
            due: Some(chrono::NaiveDate::from_ymd_opt(2026, 5, 2).unwrap()),
            scheduled: Some(chrono::NaiveDate::from_ymd_opt(2026, 5, 1).unwrap()),
            body: "Created through authenticated Vox e2e.".to_string(),
            ..Default::default()
        })),
    )
    .await;
    let created: Task = api_to_model(created);
    assert_eq!(created.title, "E2E remote task");
    assert_ne!(created.id, uuid::Uuid::nil());
    assert!(created.date_created.is_some());
    assert!(created.date_modified.is_some());

    let daily_review = service_call("daily_review", inbox_service.daily_review()).await;
    assert!(daily_review.inbox.is_empty());
    assert!(
        daily_review
            .commitments
            .iter()
            .any(|task| task.title == promoted.title)
    );
    let weekly_review = service_call("weekly_review", inbox_service.weekly_review()).await;
    assert!(
        weekly_review
            .commitments
            .iter()
            .any(|task| task.title == promoted.title)
    );

    let tasks: Vec<Task> = service_call(
        "list_tasks",
        task_repo.list_tasks(None, None, None, Some(100)),
    )
    .await
    .into_iter()
    .map(api_to_model)
    .collect();
    let seeded = tasks
        .iter()
        .find(|task| task.title == "E2E seeded task")
        .cloned()
        .expect("seeded task should be listed");
    assert!(tasks.iter().any(|task| task.title == created.title));
    let search = service_call(
        "search_tasks",
        task_service.search_tasks("seeded task".to_string()),
    )
    .await;
    assert!(search.iter().any(|task| task.title == seeded.title));
    let assigned = service_call(
        "tasks_for_user",
        task_service.tasks_for_user("agent".to_string()),
    )
    .await;
    assert!(assigned.iter().any(|task| task.title == seeded.title));
    assert!(assigned.iter().any(|task| task.title == created.title));
    let urgency = service_call("urgency_score", task_service.urgency_score(seeded.clone())).await;
    assert!(urgency > 0);
    let query_results = service_call(
        "execute_query",
        task_service.execute_query(Query {
            filters: vec![Filter::TitleContains("seeded".to_string())],
            sort: Sort::Urgency,
            limit: Some(10),
            group: None,
        }),
    )
    .await;
    assert!(query_results.iter().any(|task| task.title == seeded.title));

    let project = service_call(
        "update_project",
        project_repo.create_project(model_to_api(&Project {
            title: "E2E Project".to_string(),
            client: Some(WikiLink("E2E Client".to_string())),
            default_rate: Some(15_000),
            tags: vec!["e2e".to_string()].into(),
            ..Default::default()
        })),
    )
    .await;
    let project: Project = api_to_model(project);
    assert_eq!(project.title, "E2E Project");
    assert_eq!(
        project.client.as_ref().map(|client| client.0.as_str()),
        Some("E2E Client")
    );
    let project_tasks = service_call(
        "tasks_for_project",
        project_service.tasks_for_project("E2E Project".to_string()),
    )
    .await;
    assert!(project_tasks.iter().any(|task| task.title == seeded.title));
    assert!(project_tasks.iter().any(|task| task.title == created.title));
    assert!(
        service_call(
            "project_stats",
            project_service.project_stats("E2E Project".to_string())
        )
        .await
        .total()
            >= 1
    );
    let projects: Vec<Project> = service_call(
        "list_projects",
        project_repo.list_projects(None, None, None, Some(100)),
    )
    .await
    .into_iter()
    .map(api_to_model)
    .collect();
    assert!(
        projects
            .iter()
            .any(|project| project.title == "E2E Project")
    );
    let next_task = service_call(
        "next_task",
        project_service.next_task("E2E Project".to_string()),
    )
    .await;
    let next_task = next_task.expect("E2E project should have a next task");
    assert!(
        next_task
            .projects
            .iter()
            .any(|project| project.0 == "E2E Project")
    );

    assert!(
        service_call("active_timer", time_service.active_timer())
            .await
            .is_none()
    );
    let timer = service_call(
        "start_timer",
        time_service.start_timer(TimeStartRequest {
            task_ref: created.title.clone(),
            description: Some("active e2e timer".to_string()),
            billable: true,
            billable_rate: Some(18_000),
            user: Some("agent".to_string()),
        }),
    )
    .await;
    assert!(timer.is_running());
    assert!(
        service_call("active_timer", time_service.active_timer())
            .await
            .is_some()
    );
    let stopped = service_call(
        "stop_timer",
        time_service.stop_timer(Some(created.title.clone())),
    )
    .await;
    assert_eq!(stopped.task_title, created.title);
    assert!(!stopped.entry.is_running());

    let start = Utc.with_ymd_and_hms(2026, 4, 29, 9, 0, 0).unwrap();
    let end = Utc.with_ymd_and_hms(2026, 4, 29, 10, 0, 0).unwrap();
    let logged = service_call(
        "log_time",
        time_service.log_time(TimeLogRequest {
            task_ref: created.title.clone(),
            start,
            end,
            description: Some("billable invoice hour".to_string()),
            billable: true,
            billable_rate: Some(20_000),
            user: Some("agent".to_string()),
        }),
    )
    .await;
    assert_eq!(logged.duration_minutes(), 60);
    let entries = service_call(
        "list_time_entries",
        time_service.list_time_entries(TimeEntryFilter {
            task_ref: Some(created.title.clone()),
            user: Some("agent".to_string()),
            ..Default::default()
        }),
    )
    .await;
    assert!(entries.iter().any(|entry| entry.entry.id == logged.id));

    let invoice = service_call(
        "create_invoice_from_entries",
        invoice_service.create_invoice_from_entries(InvoiceCreateRequest {
            client_name: "E2E Client".to_string(),
            from: Some(start),
            to: Some(end),
            fallback_rate: Some(10_000),
            tax_rate_percent: Some(0.0),
            discount_percent: Some(0.0),
            po_number: Some("PO-E2E".to_string()),
            public_notes: Some("Remote Vox invoice e2e".to_string()),
            actor: Some("agent".to_string()),
        }),
    )
    .await;
    assert_eq!(invoice.client.0, "E2E Client");
    assert!(invoice.total_cents() >= 20_000);
    let invoices: Vec<task_core::Invoice> = service_call(
        "list_invoices",
        invoice_repo.list_invoices(None, None, None, Some(100)),
    )
    .await
    .into_iter()
    .map(api_to_model)
    .collect();
    assert!(invoices.iter().any(|candidate| candidate.id == invoice.id));
    let paid = service_call(
        "record_invoice_payment",
        invoice_service.record_invoice_payment(InvoicePaymentRequest {
            invoice_id: invoice.id.clone(),
            amount_cents: invoice.total_cents(),
            method: Some("test".to_string()),
            reference: Some("remote-e2e".to_string()),
            notes: None,
            actor: Some("agent".to_string()),
        }),
    )
    .await;
    assert_eq!(paid.balance_cents(), 0);

    let event_start = Utc.with_ymd_and_hms(2026, 4, 30, 15, 0, 0).unwrap();
    let event_end = Utc.with_ymd_and_hms(2026, 4, 30, 16, 0, 0).unwrap();
    let event = service_call(
        "create_event",
        calendar_event_repo.create_calendar_event(model_to_api(&CalendarEvent {
            title: "E2E calendar event".to_string(),
            description: Some("Remote calendar event".to_string()),
            location: Some("Remote".to_string()),
            start: event_start,
            end: Some(event_end),
            status: CalendarEventStatus::Confirmed,
            ..Default::default()
        })),
    )
    .await;
    let event: CalendarEvent = api_to_model(event);
    assert!(event.id.is_some());
    let updated_event = service_call(
        "update_event",
        calendar_event_repo.update_calendar_event(
            event.uuid.to_string(),
            model_to_api(&CalendarEvent {
                title: "E2E calendar event updated".to_string(),
                status: CalendarEventStatus::Tentative,
                ..event.clone()
            }),
        ),
    )
    .await;
    let updated_event: CalendarEvent = api_to_model(updated_event);
    assert_eq!(updated_event.title, "E2E calendar event updated");
    let events = service_call(
        "events_between",
        calendar_service.events_between(event_start.to_rfc3339(), event_end.to_rfc3339()),
    )
    .await;
    assert!(
        events
            .iter()
            .any(|candidate| candidate.title == updated_event.title)
    );
    let due = service_call(
        "tasks_due_by",
        calendar_service.tasks_due_by("2026-05-02".to_string()),
    )
    .await;
    assert!(due.iter().any(|task| task.title == created.title));
    let scheduled = service_call(
        "scheduled_between",
        calendar_service.scheduled_between("2026-05-01".to_string(), "2026-05-01".to_string()),
    )
    .await;
    assert!(scheduled.iter().any(|task| task.title == created.title));
    assert!(
        service_call("sync_status", calendar_service.sync_status())
            .await
            .is_none()
    );

    let activity = service_call("recent_activity", activity_service.recent_activity(20)).await;
    assert!(activity.len() <= 20);
    let conflicts = service_call("list_conflicts", activity_service.list_conflicts(true, 20)).await;
    assert!(conflicts.len() <= 20);

    assert!(service_error("list_accounts", mail_service.list_accounts()).await);

    let completed = service_call(
        "complete_task",
        task_service.complete_task(created.title.clone()),
    )
    .await;
    assert_eq!(completed.status, Status::Done);
    service_call(
        "delete_event",
        calendar_event_repo.delete_calendar_event(updated_event.uuid.to_string()),
    )
    .await;

    server.stop().await;
}

async fn connect_service<C>(vox_url: &str) -> C
where
    C: vox::FromVoxSession,
{
    timeout(Duration::from_secs(30), vox::connect(vox_url).establish())
        .await
        .expect("Vox connection should not time out")
        .expect("Vox connection should establish")
}

fn model_to_api<T, U>(value: &T) -> U
where
    T: Serialize,
    U: DeserializeOwned,
{
    serde_json::from_value(serde_json::to_value(value).expect("serialize model"))
        .expect("deserialize api model")
}

fn api_to_model<T, U>(value: T) -> U
where
    T: Serialize,
    U: DeserializeOwned,
{
    serde_json::from_value(serde_json::to_value(value).expect("serialize api model"))
        .expect("deserialize model")
}

async fn service_call<T, E, F>(name: &str, future: F) -> T
where
    E: Debug,
    F: Future<Output = Result<T, E>>,
{
    timeout(Duration::from_secs(30), future)
        .await
        .unwrap_or_else(|_| panic!("{name} should not time out"))
        .unwrap_or_else(|err| panic!("{name} should succeed: {err:?}"))
}

async fn service_error<T, E, F>(name: &str, future: F) -> bool
where
    E: Debug,
    F: Future<Output = Result<T, E>>,
{
    timeout(Duration::from_secs(30), future)
        .await
        .unwrap_or_else(|_| panic!("{name} should not time out"))
        .is_err()
}

async fn free_loopback_addr() -> SocketAddr {
    let listener = TcpListener::bind("127.0.0.1:0")
        .await
        .expect("free loopback port should bind");
    listener
        .local_addr()
        .expect("bound listener should have local addr")
}

async fn wait_for_tcp(addr: SocketAddr) {
    let deadline = tokio::time::Instant::now() + Duration::from_secs(20);
    loop {
        if TcpStream::connect(addr).await.is_ok() {
            return;
        }
        assert!(
            tokio::time::Instant::now() < deadline,
            "task-server did not start listening on {addr}"
        );
        tokio::time::sleep(Duration::from_millis(100)).await;
    }
}

struct TestFixture {
    root: PathBuf,
}

impl TestFixture {
    fn new() -> Self {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system clock should be after unix epoch")
            .as_nanos();
        let root = std::env::temp_dir().join(format!("task-server-e2e-{nanos}"));
        std::fs::create_dir_all(root.join("vault")).expect("fixture vault should be created");
        Self { root }
    }

    fn db_path(&self) -> PathBuf {
        self.root.join("server.sqlite")
    }

    fn vault_path(&self) -> PathBuf {
        self.root.join("vault")
    }

    fn seed_task(&self, task: Task) {
        let content =
            task_core::Vault::render_task_file(&task, &task.body).expect("seed task should render");
        std::fs::write(
            self.vault_path().join(format!("{}.md", task.title)),
            content,
        )
        .expect("seed task should be written");
    }

    fn seed_project(&self, project: Project) {
        let content = task_core::Vault::render_project_file(&project, "")
            .expect("seed project should render");
        std::fs::write(
            self.vault_path().join(format!("{}.md", project.title)),
            content,
        )
        .expect("seed project should be written");
    }
}

impl Drop for TestFixture {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.root);
    }
}

struct RunningServer {
    child: Child,
}

impl RunningServer {
    fn spawn(server_bin: &str, bind_addr: SocketAddr, fixture: &TestFixture) -> Self {
        let child = Command::new(server_bin)
            .env("BIND_ADDR", bind_addr.to_string())
            .env("PUBLIC_BASE_URL", format!("http://{bind_addr}"))
            .env("TASK_DB_PATH", fixture.db_path())
            .env("TASK_VAULT", fixture.vault_path())
            .env("TASK_SEED_DEMO", "1")
            .env("TASK_TEST_SESSION_TOKEN", TEST_TOKEN)
            .env("HOME", fixture.root.as_os_str())
            .env(
                "TASK_NEXTCLOUD_CONFIG",
                fixture.root.join("missing-nextcloud.toml"),
            )
            .env_remove("NEXTCLOUD_URL")
            .env_remove("NEXTCLOUD_USERNAME")
            .env_remove("NEXTCLOUD_PASSWORD")
            .env_remove("NEXTCLOUD_APP_PASSWORD")
            .env_remove("NEXTCLOUD_PROJECTS_PATH")
            .env_remove("NEXTCLOUD_CALENDAR")
            .env_remove("NEXTCLOUD_EVENT_CALENDAR")
            .env_remove("NEXTCLOUD_DECK_ENABLED")
            .env(
                "AUTH_SECRET",
                "task-server-e2e-secret-key-must-be-at-least-32-chars",
            )
            .env("RUST_LOG", "task_server=warn")
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .expect("task-server should spawn");
        Self { child }
    }

    async fn stop(&mut self) {
        if let Ok(Some(_)) = self.child.try_wait() {
            return;
        }
        let _ = self.child.kill().await;
        let _ = self.child.wait().await;
    }
}

impl Drop for RunningServer {
    fn drop(&mut self) {
        let _ = self.child.start_kill();
    }
}
