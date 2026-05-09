//! Shared integration-test harness for `task-server`.
//!
//! Boots the compiled `task-server` binary against an isolated tempdir,
//! waits for the bind port, and tears the child down on `Drop`.
//! `CliRunner` adds CLI-subprocess invocation against the running server.

#![allow(dead_code)]

use std::net::SocketAddr;
use std::path::PathBuf;
use std::process::Stdio;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use task_core::{Project, Task};
use tokio::net::{TcpListener, TcpStream};
use tokio::process::{Child, Command};

pub(crate) const TEST_TOKEN: &str = "task-server-e2e-session-token";

// ── Test fixture (tempdir + cleanup) ────────────────────────────────────────

pub(crate) struct TestFixture {
    root: PathBuf,
}

impl TestFixture {
    pub(crate) fn new() -> Self {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system clock should be after unix epoch")
            .as_nanos();
        let root = std::env::temp_dir().join(format!("task-server-e2e-{nanos}"));
        std::fs::create_dir_all(root.join("vault")).expect("fixture vault should be created");
        Self { root }
    }

    pub(crate) fn root(&self) -> &PathBuf {
        &self.root
    }

    pub(crate) fn db_path(&self) -> PathBuf {
        self.root.join("server.sqlite")
    }

    pub(crate) fn vault_path(&self) -> PathBuf {
        self.root.join("vault")
    }

    pub(crate) fn seed_task(&self, task: Task) {
        // The markdown seed pathway has been removed alongside the Vault
        // layer. The task-server's persistence boundary is now SQLite via
        // SeaORM, so this fixture method is a no-op kept for the (currently
        // ignored) full-services smoke test until it's rewritten to seed
        // through the repo services directly.
        let _ = task;
        let _ = self.vault_path();
    }

    pub(crate) fn seed_project(&self, project: Project) {
        let _ = project;
        let _ = self.vault_path();
    }
}

impl Drop for TestFixture {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.root);
    }
}

// ── Running task-server child ───────────────────────────────────────────────

pub(crate) struct RunningServer {
    child: Child,
}

impl RunningServer {
    pub(crate) fn spawn(server_bin: &str, bind_addr: SocketAddr, fixture: &TestFixture) -> Self {
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

    pub(crate) async fn stop(&mut self) {
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

// ── Networking helpers ──────────────────────────────────────────────────────

pub(crate) async fn free_loopback_addr() -> SocketAddr {
    let listener = TcpListener::bind("127.0.0.1:0")
        .await
        .expect("free loopback port should bind");
    listener
        .local_addr()
        .expect("bound listener should have local addr")
}

pub(crate) async fn wait_for_tcp(addr: SocketAddr) {
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

// ── CLI subprocess runner ───────────────────────────────────────────────────

pub(crate) struct CliRunner {
    server_addr: SocketAddr,
    config_dir: PathBuf,
    cli_bin: PathBuf,
    organization_id: String,
    profile_name: String,
}

pub(crate) struct CliOutput {
    pub stdout: String,
    pub stderr: String,
    pub status: std::process::ExitStatus,
}

impl CliRunner {
    /// Build a runner. Writes `<config_dir>/servers.tsv` with a `test`
    /// profile pointing at the running server (with the test session
    /// token + organization id baked in) and marks it as default.
    pub(crate) fn new(server_addr: SocketAddr, organization_id: &str) -> Self {
        let cli_bin = locate_task_cli_binary();
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system clock should be after unix epoch")
            .as_nanos();
        let config_dir = std::env::temp_dir().join(format!("task-cli-e2e-{nanos}"));
        std::fs::create_dir_all(&config_dir).expect("cli config dir should be created");
        let profile_name = "test".to_string();
        let url = format!("http://{server_addr}");
        let tsv = format!(
            "default\t{profile_name}\nserver\t{profile_name}\t{url}\t{TEST_TOKEN}\t{organization_id}\n"
        );
        std::fs::write(config_dir.join("servers.tsv"), tsv)
            .expect("write servers.tsv for CLI test profile");
        Self {
            server_addr,
            config_dir,
            cli_bin,
            organization_id: organization_id.to_string(),
            profile_name,
        }
    }

    pub(crate) fn server_addr(&self) -> SocketAddr {
        self.server_addr
    }

    pub(crate) fn organization_id(&self) -> &str {
        &self.organization_id
    }

    /// Run `task <args>` synchronously with `--server <profile>` injected.
    /// Returns captured stdout/stderr and the exit status. Times out after 30s.
    pub(crate) fn run(&self, args: &[&str]) -> CliOutput {
        use std::process::Command as StdCommand;
        let mut cmd = StdCommand::new(&self.cli_bin);
        cmd.env("TASK_CONFIG_DIR", &self.config_dir)
            .env("TASK_SERVER", &self.profile_name)
            .env("TASK_USER", "agent")
            .env("RUST_LOG", "warn")
            .env_remove("TASK_VAULT");
        cmd.args(args);
        cmd.stdin(Stdio::null());
        cmd.stdout(Stdio::piped());
        cmd.stderr(Stdio::piped());

        // Spawn + wait with a 30-second timeout in a worker thread so we
        // can report a useful panic if the CLI hangs.
        let mut child = cmd
            .spawn()
            .unwrap_or_else(|e| panic!("failed to spawn `task {}`: {e}", args.join(" ")));
        let pid = child.id();

        let deadline = std::time::Instant::now() + Duration::from_secs(30);
        loop {
            match child.try_wait() {
                Ok(Some(_)) => break,
                Ok(None) => {
                    if std::time::Instant::now() > deadline {
                        let _ = child.kill();
                        let _ = child.wait();
                        panic!("`task {}` (pid {pid}) timed out after 30s", args.join(" "));
                    }
                    std::thread::sleep(Duration::from_millis(50));
                }
                Err(e) => panic!("waiting on `task {}` failed: {e}", args.join(" ")),
            }
        }
        let output = child
            .wait_with_output()
            .unwrap_or_else(|e| panic!("collect output for `task {}`: {e}", args.join(" ")));
        CliOutput {
            stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
            stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
            status: output.status,
        }
    }

    /// Same as [`run`] but panics with stdout+stderr on non-zero exit.
    pub(crate) fn run_ok(&self, args: &[&str]) -> CliOutput {
        let out = self.run(args);
        if !out.status.success() {
            panic!(
                "`task {}` exited {:?}\n--- stdout ---\n{}\n--- stderr ---\n{}",
                args.join(" "),
                out.status.code(),
                out.stdout,
                out.stderr
            );
        }
        out
    }

    /// Same as [`run_ok`] plus asserts every needle is a substring of stdout.
    pub(crate) fn run_ok_contains(&self, args: &[&str], expected: &[&str]) -> CliOutput {
        let out = self.run_ok(args);
        for needle in expected {
            assert!(
                out.stdout.contains(needle),
                "`task {}` stdout missing needle {needle:?}\n--- stdout ---\n{}\n--- stderr ---\n{}",
                args.join(" "),
                out.stdout,
                out.stderr
            );
        }
        out
    }
}

impl Drop for CliRunner {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.config_dir);
    }
}

/// Locate the compiled `task` CLI binary, building it on demand.
///
/// `env!("CARGO_BIN_EXE_<name>")` is only populated for binaries
/// declared in the same crate as the integration test, so for a
/// cross-package binary we resolve the path ourselves. The
/// integration-test binary lives at
/// `<target>/<profile>/deps/<name>-<hash>`; the `task` binary lives
/// at `<target>/<profile>/task`.
///
/// To avoid a chicken-and-egg problem where the test runs before
/// `task-cli` has been built, we shell out to `cargo build -p task-cli`
/// the first time the binary is needed. Subsequent runs are cheap
/// because cargo skips the up-to-date build.
fn locate_task_cli_binary() -> PathBuf {
    use std::sync::OnceLock;
    static BUILT: OnceLock<()> = OnceLock::new();

    let mut path = std::env::current_exe().expect("current_exe should resolve");
    path.pop(); // strip `<test>-<hash>`
    path.pop(); // strip `deps`
    let profile_dir = path.clone();
    let bin_name = if cfg!(windows) { "task.exe" } else { "task" };
    path.push(bin_name);

    if !path.exists() {
        BUILT.get_or_init(|| {
            // Cargo's profile dir name (`debug` / `release`) tells us which
            // profile the test was built under. Match it for the CLI build.
            let profile_name = profile_dir
                .file_name()
                .and_then(|s| s.to_str())
                .unwrap_or("debug");
            let mut cmd = std::process::Command::new(env!("CARGO"));
            cmd.arg("build").arg("-p").arg("task-cli");
            if profile_name == "release" {
                cmd.arg("--release");
            }
            let status = cmd.status().expect("cargo build -p task-cli should spawn");
            assert!(status.success(), "cargo build -p task-cli failed");
        });
    }

    assert!(
        path.exists(),
        "task CLI binary not found at {path:?} after build attempt",
    );
    path
}
