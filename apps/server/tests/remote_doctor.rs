use std::net::SocketAddr;
use std::path::PathBuf;
use std::process::Stdio;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

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
