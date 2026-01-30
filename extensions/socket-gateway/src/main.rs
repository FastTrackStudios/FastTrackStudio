//! Socket Gateway Extension
//!
//! Exposes ROAM services over Unix sockets for local applications like control-desktop.
//! Uses transparent forwarding - all RPC calls are proxied to the host.
//!
//! Architecture:
//! - Unix Socket Client → Socket Gateway → SHM → Host → DAW Services
//!
//! Socket Path:
//! - Unix: /tmp/fts-reaper-{pid}.sock
//! - Windows: \\.\pipe\fts-reaper-{pid}
//!
//! Environment:
//! - FTS_SOCKET_PATH: Override the default socket path (optional)

use extension_runtime::{run_extension, ConnectionHandle, NoDispatcher};
use roam_session::ForwardingDispatcher;
use roam_stream::{accept, HandshakeConfig};
use std::sync::{Arc, OnceLock};
use tokio::net::UnixListener;
use tracing::{error, info, warn};

/// Get the socket path for this REAPER instance.
///
/// Default: /tmp/fts-reaper-{ppid}.sock (uses parent PID since we're spawned by REAPER)
fn get_socket_path() -> std::path::PathBuf {
    if let Ok(path) = std::env::var("FTS_SOCKET_PATH") {
        return std::path::PathBuf::from(path);
    }

    // Use parent PID (REAPER's PID) for the socket name
    #[cfg(unix)]
    let ppid = unsafe { libc::getppid() };
    #[cfg(not(unix))]
    let ppid = std::process::id();

    std::env::temp_dir().join(format!("fts-reaper-{ppid}.sock"))
}

/// Start the Unix socket server.
async fn start_socket_server(
    handle_cell: Arc<OnceLock<ConnectionHandle>>,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    // Wait for the handle to be initialized
    let host_handle = loop {
        if let Some(h) = handle_cell.get() {
            break h.clone();
        }
        tokio::time::sleep(std::time::Duration::from_millis(10)).await;
    };

    let socket_path = get_socket_path();

    // Clean up any stale socket file
    if socket_path.exists() {
        warn!("Removing stale socket file: {}", socket_path.display());
        let _ = std::fs::remove_file(&socket_path);
    }

    // Bind the Unix socket listener
    let listener = UnixListener::bind(&socket_path)?;

    info!("════════════════════════════════════════════════════════════");
    info!("  🔌 Socket Gateway listening at: {}", socket_path.display());
    info!("     All ROAM calls forwarded to host");
    info!("════════════════════════════════════════════════════════════");
    info!("✅ Socket Gateway ready!");

    // Accept connections in a loop
    loop {
        match listener.accept().await {
            Ok((stream, _addr)) => {
                info!("New connection accepted");
                let host_handle = host_handle.clone();

                // Spawn a task to handle this connection
                tokio::spawn(async move {
                    if let Err(e) = handle_connection(stream, host_handle).await {
                        error!("Connection error: {:#}", e);
                    }
                });
            }
            Err(e) => {
                error!("Accept error: {}", e);
            }
        }
    }
}

/// Handle a single client connection.
async fn handle_connection(
    stream: tokio::net::UnixStream,
    host_handle: ConnectionHandle,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    // Create a forwarding dispatcher that proxies all calls to the host
    let dispatcher = ForwardingDispatcher::new(host_handle);

    // Accept the ROAM connection (performs handshake, sets up framing)
    let (client_handle, _incoming, driver) =
        accept(stream, HandshakeConfig::default(), dispatcher).await?;

    info!("ROAM handshake complete");

    // Run the driver until the connection closes
    let result = driver.run().await;

    drop(client_handle);
    info!("Connection closed");

    result?;
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    run_extension!("socket-gateway", |handle| {
        // Clone the handle cell for the socket server task
        let handle_for_server = handle.clone();

        // Start socket server in background task
        extension_runtime::tokio::spawn(async move {
            if let Err(e) = start_socket_server(handle_for_server).await {
                error!("Socket server error: {:#}", e);
            }
        });

        // The extension needs a dispatcher for the root connection
        // We don't expose any services ourselves, just run the socket server
        NoDispatcher
    })
}
