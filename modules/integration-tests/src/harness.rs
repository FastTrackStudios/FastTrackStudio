//! Test harness for spawning and managing test-extension.
//!
//! This module provides utilities for integration testing the full
//! FastTrackStudio stack by spawning test-extension as a subprocess
//! and connecting to it via Unix socket or WebSocket.

use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::Duration;

pub use host_client::{HostConnection, HostConnector};
use rand::Rng;
use tempfile::TempDir;
use tokio::net::TcpStream;
use tokio::time::{sleep, timeout};

/// Error type for test harness operations.
#[derive(Debug)]
pub enum HarnessError {
    /// Failed to find the test-extension binary.
    BinaryNotFound(String),
    /// Failed to spawn the test-extension process.
    SpawnFailed(std::io::Error),
    /// Timed out waiting for the extension to be ready.
    ReadyTimeout,
    /// Failed to connect to the extension.
    ConnectionFailed(String),
}

impl std::fmt::Display for HarnessError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HarnessError::BinaryNotFound(msg) => write!(f, "Binary not found: {}", msg),
            HarnessError::SpawnFailed(e) => write!(f, "Failed to spawn process: {}", e),
            HarnessError::ReadyTimeout => write!(f, "Timed out waiting for extension to be ready"),
            HarnessError::ConnectionFailed(msg) => write!(f, "Connection failed: {}", msg),
        }
    }
}

impl std::error::Error for HarnessError {}

/// Test harness for managing a test-extension instance.
///
/// Spawns test-extension with isolated socket/port configuration,
/// waits for it to be ready, and provides connection helpers.
///
/// The extension is automatically killed when the harness is dropped.
pub struct TestExtensionHarness {
    /// The spawned test-extension process.
    child: Child,
    /// Path to the Unix socket.
    socket_path: PathBuf,
    /// WebSocket port.
    ws_port: u16,
    /// Temporary directory for socket file.
    _temp_dir: TempDir,
}

impl TestExtensionHarness {
    /// Spawn a new test-extension instance with isolated configuration.
    ///
    /// Uses a random port and temp directory socket to avoid conflicts
    /// when running tests in parallel.
    pub async fn spawn() -> Result<Self, HarnessError> {
        // Find the binary
        let binary_path = find_binary("test-extension").ok_or_else(|| {
            HarnessError::BinaryNotFound(
                "Could not find test-extension binary. Run `cargo build -p test-extension` first."
                    .to_string(),
            )
        })?;

        // Create temp directory for socket
        let temp_dir = TempDir::new().map_err(|e| HarnessError::SpawnFailed(e))?;
        let socket_path = temp_dir.path().join("fts-control.sock");

        // Pick a random port in the range 20000-30000
        let ws_port = rand::thread_rng().gen_range(20000..30000);

        // Spawn the process with environment configuration
        let child = Command::new(&binary_path)
            .env("FTS_SOCKET", &socket_path)
            .env("GATEWAY_WS_ADDR", format!("127.0.0.1:{}", ws_port))
            .env("FTS_QUIET", "1") // Suppress cell output
            .env("RUST_LOG", "warn") // Reduce log noise
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .map_err(HarnessError::SpawnFailed)?;

        Ok(Self {
            child,
            socket_path,
            ws_port,
            _temp_dir: temp_dir,
        })
    }

    /// Wait for the extension to be ready (socket and port available).
    ///
    /// Times out after the specified duration (default 10 seconds).
    pub async fn wait_ready(&self) -> Result<(), HarnessError> {
        self.wait_ready_timeout(Duration::from_secs(10)).await
    }

    /// Wait for the extension to be ready with a custom timeout.
    pub async fn wait_ready_timeout(&self, max_wait: Duration) -> Result<(), HarnessError> {
        timeout(max_wait, async {
            // Wait for socket file to exist
            while !self.socket_path.exists() {
                sleep(Duration::from_millis(50)).await;
            }

            // Wait for WebSocket port to be listening
            loop {
                if TcpStream::connect(format!("127.0.0.1:{}", self.ws_port))
                    .await
                    .is_ok()
                {
                    break;
                }
                sleep(Duration::from_millis(50)).await;
            }

            // Give it a tiny bit more time for full initialization
            sleep(Duration::from_millis(100)).await;
        })
        .await
        .map_err(|_| HarnessError::ReadyTimeout)
    }

    /// Get the Unix socket path.
    pub fn socket_path(&self) -> &Path {
        &self.socket_path
    }

    /// Get the WebSocket port.
    pub fn ws_port(&self) -> u16 {
        self.ws_port
    }

    /// Get the WebSocket URL.
    pub fn ws_url(&self) -> String {
        format!("ws://127.0.0.1:{}/ws", self.ws_port)
    }

    /// Connect to the extension via Unix socket.
    pub async fn connect_unix(&self) -> Result<HostConnection, HarnessError> {
        let connector = HostConnector::unix(&self.socket_path);
        connector
            .connect()
            .await
            .map_err(|e| HarnessError::ConnectionFailed(e.to_string()))
    }

    /// Connect to the extension via WebSocket.
    pub async fn connect_websocket(&self) -> Result<HostConnection, HarnessError> {
        let connector = HostConnector::websocket(self.ws_url());
        connector
            .connect()
            .await
            .map_err(|e| HarnessError::ConnectionFailed(e.to_string()))
    }

    /// Kill the test-extension process.
    pub fn kill(&mut self) {
        let _ = self.child.kill();
    }
}

impl Drop for TestExtensionHarness {
    fn drop(&mut self) {
        self.kill();
    }
}

/// Find a binary in the target directory.
fn find_binary(name: &str) -> Option<PathBuf> {
    // Try multiple possible locations
    let possible_paths = [
        // From workspace root
        PathBuf::from("target/debug").join(name),
        // From a crate directory
        PathBuf::from("../target/debug").join(name),
        // Two levels up
        PathBuf::from("../../target/debug").join(name),
        // Three levels up
        PathBuf::from("../../../target/debug").join(name),
        // Using CARGO_MANIFEST_DIR
        PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap_or_default())
            .join("../../target/debug")
            .join(name),
    ];

    for path in &possible_paths {
        if path.exists() {
            return Some(path.clone());
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_binary() {
        // This test just verifies the function doesn't panic
        let _ = find_binary("test-extension");
    }
}
