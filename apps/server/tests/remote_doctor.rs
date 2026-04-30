use std::net::SocketAddr;
use std::path::PathBuf;
use std::process::Stdio;
use std::time::{Duration, SystemTime, UNIX_EPOCH};
use std::{fmt::Debug, future::Future};

use chrono::{TimeZone, Utc};
use task_core::{
    Client, Filter, Priority, Project, ProjectPatch, Query, Sort, Status, Task, TimeEntryFilter,
    WikiLink,
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
    assert!(capabilities
        .services
        .iter()
        .any(|svc| svc == "SystemService"));
    assert!(capabilities
        .features
        .iter()
        .any(|feature| feature == "webdav-files"));
    assert!(capabilities.vault.exists);

    let health = timeout(Duration::from_secs(10), system.health(false))
        .await
        .expect("SystemService health should not time out")
        .expect("SystemService health should return");
    assert!(!health.deep);
    assert!(health.checks.iter().any(|check| check.code == "VAULT_OK"));

    server.stop().await;
}

#[tokio::test]
async fn authenticated_core_services_smoke_over_vox() {
    let server_bin = env!("CARGO_BIN_EXE_task-server");

    let fixture = TestFixture::new();
    fixture.seed_project(Project {
        title: "E2E Project".to_string(),
        ..Default::default()
    });
    fixture.seed_task(Task {
        id: Some("task-e2e-seeded".to_string()),
        title: "E2E seeded task".to_string(),
        status: Status::Open,
        priority: Priority::High,
        projects: vec![WikiLink("E2E Project".to_string())],
        assignee: Some("agent".to_string()),
        body: "Seeded before task-server starts so remote read paths have data.".to_string(),
        ..Default::default()
    });

    let bind_addr = free_loopback_addr().await;
    let mut server = RunningServer::spawn(server_bin, bind_addr, &fixture);

    wait_for_tcp(bind_addr).await;
    let vox_url = format!("ws://{bind_addr}/vox?token={TEST_TOKEN}&organization_id=org_fts");

    let task_service: task_core::service::TaskServiceClient = connect_service(&vox_url).await;
    let project_service: task_core::service::ProjectServiceClient = connect_service(&vox_url).await;
    let time_service: task_core::service::TimeServiceClient = connect_service(&vox_url).await;
    let client_service: task_core::service::ClientServiceClient = connect_service(&vox_url).await;
    let invoice_service: task_core::service::InvoiceServiceClient = connect_service(&vox_url).await;
    let calendar_service: task_core::service::CalendarServiceClient =
        connect_service(&vox_url).await;
    let activity_service: task_core::service::ActivityServiceClient =
        connect_service(&vox_url).await;
    let file_service: task_core::service::FileServiceClient = connect_service(&vox_url).await;
    let mail_service: task_core::service::MailServiceClient = connect_service(&vox_url).await;

    let client = service_call(
        "save_client",
        client_service.save_client(Client {
            name: "E2E Client".to_string(),
            default_hourly_rate: Some(12_000),
            currency_code: "USD".to_string(),
            email: Some("billing@example.com".to_string()),
            ..Default::default()
        }),
    )
    .await;
    assert_eq!(client.name, "E2E Client");
    assert_eq!(
        service_call(
            "find_client",
            client_service.find_client("E2E Client".to_string())
        )
        .await
        .map(|client| client.name),
        Some("E2E Client".to_string())
    );

    let tasks = service_call("list_tasks", task_service.list_tasks()).await;
    let seeded = tasks
        .iter()
        .find(|task| task.title == "E2E seeded task")
        .cloned()
        .expect("seeded task should be listed");
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
        project_service.update_project(
            "E2E Project".to_string(),
            ProjectPatch {
                client: Some("E2E Client".to_string()),
                default_rate: Some(15_000),
                add_tag: vec!["e2e".to_string()],
                ..Default::default()
            },
            Some("agent".to_string()),
        ),
    )
    .await;
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
    assert!(
        service_call(
            "project_stats",
            project_service.project_stats("E2E Project".to_string())
        )
        .await
        .total()
            >= 1
    );
    let projects = service_call("list_projects", project_service.list_projects()).await;
    assert!(projects
        .iter()
        .any(|project| project.title == "E2E Project"));
    let next_task = service_call(
        "next_task",
        project_service.next_task("E2E Project".to_string()),
    )
    .await;
    assert_eq!(next_task.map(|task| task.title), Some(seeded.title.clone()));

    assert!(service_call("active_timer", time_service.active_timer())
        .await
        .is_none());
    let entries = service_call(
        "list_time_entries",
        time_service.list_time_entries(TimeEntryFilter {
            task_ref: Some(seeded.title.clone()),
            user: Some("agent".to_string()),
            ..Default::default()
        }),
    )
    .await;
    assert!(entries.is_empty());

    let invoices = service_call("list_invoices", invoice_service.list_invoices()).await;
    assert!(invoices.is_empty());

    let event_start = Utc.with_ymd_and_hms(2026, 4, 30, 15, 0, 0).unwrap();
    let event_end = Utc.with_ymd_and_hms(2026, 4, 30, 16, 0, 0).unwrap();
    let events = service_call(
        "events_between",
        calendar_service.events_between(event_start.to_rfc3339(), event_end.to_rfc3339()),
    )
    .await;
    assert!(events.is_empty());
    let due = service_call(
        "tasks_due_by",
        calendar_service.tasks_due_by("2026-04-30".to_string()),
    )
    .await;
    assert!(due.is_empty());
    let scheduled = service_call(
        "scheduled_between",
        calendar_service.scheduled_between("2026-04-29".to_string(), "2026-04-30".to_string()),
    )
    .await;
    assert!(scheduled.is_empty());
    assert!(service_call("sync_status", calendar_service.sync_status())
        .await
        .is_none());

    let activity = service_call("recent_activity", activity_service.recent_activity(20)).await;
    assert!(activity.len() <= 20);
    let conflicts = service_call("list_conflicts", activity_service.list_conflicts(true, 20)).await;
    assert!(conflicts.len() <= 20);

    assert!(
        service_error(
            "stat_file",
            file_service.stat_file("missing.txt".to_string())
        )
        .await
    );
    assert!(service_error("list_accounts", mail_service.list_accounts()).await);

    server.stop().await;
}

async fn connect_service<C>(vox_url: &str) -> C
where
    C: vox::FromVoxSession,
{
    timeout(Duration::from_secs(10), vox::connect(vox_url).establish())
        .await
        .expect("Vox connection should not time out")
        .expect("Vox connection should establish")
}

async fn service_call<T, E, F>(name: &str, future: F) -> T
where
    E: Debug,
    F: Future<Output = Result<T, E>>,
{
    timeout(Duration::from_secs(10), future)
        .await
        .unwrap_or_else(|_| panic!("{name} should not time out"))
        .unwrap_or_else(|err| panic!("{name} should succeed: {err:?}"))
}

async fn service_error<T, E, F>(name: &str, future: F) -> bool
where
    E: Debug,
    F: Future<Output = Result<T, E>>,
{
    timeout(Duration::from_secs(10), future)
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
            .env_remove("TASK_NEXTCLOUD_CONFIG")
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
