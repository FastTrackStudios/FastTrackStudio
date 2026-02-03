//! Integration tests for test-extension.
//!
//! These tests spawn the full test-extension binary and verify
//! connections via Unix socket and WebSocket work correctly.

use daw_proto::ProjectContext;
use integration_tests::harness::TestExtensionHarness;

/// Test connecting to test-extension via Unix socket.
///
/// Verifies:
/// - Extension spawns successfully
/// - Unix socket becomes available
/// - Can connect and receive host identity
/// - Transport controls work (play/stop)
#[tokio::test]
async fn test_unix_socket_connection() -> eyre::Result<()> {
    // Spawn test-extension
    let harness = TestExtensionHarness::spawn().await?;

    // Wait for it to be ready
    harness.wait_ready().await?;

    // Connect via Unix socket
    let conn = harness.connect_unix().await?;

    // Verify we got a connection (identity may or may not be present depending on host config)
    assert!(conn.is_connected());

    // Test transport controls
    conn.transport().play(ProjectContext::Current).await?;
    conn.transport().stop(ProjectContext::Current).await?;

    Ok(())
}

/// Test connecting to test-extension via WebSocket.
///
/// Verifies:
/// - Extension spawns successfully
/// - WebSocket gateway becomes available
/// - Can connect via WebSocket
/// - Transport controls work (play/stop)
#[tokio::test]
async fn test_websocket_connection() -> eyre::Result<()> {
    // Spawn test-extension
    let harness = TestExtensionHarness::spawn().await?;

    // Wait for it to be ready
    harness.wait_ready().await?;

    // Connect via WebSocket
    let conn = harness.connect_websocket().await?;

    // Verify we got a connection
    assert!(conn.is_connected());

    // Test transport controls
    conn.transport().play(ProjectContext::Current).await?;
    conn.transport().stop(ProjectContext::Current).await?;

    Ok(())
}

/// Test that both Unix socket and WebSocket connections work simultaneously.
///
/// Verifies:
/// - Both connection types can be active at once
/// - Commands from either connection work
/// - No interference between connections
#[tokio::test]
async fn test_concurrent_connections() -> eyre::Result<()> {
    // Spawn test-extension
    let harness = TestExtensionHarness::spawn().await?;

    // Wait for it to be ready
    harness.wait_ready().await?;

    // Connect via both methods
    let unix_conn = harness.connect_unix().await?;
    let ws_conn = harness.connect_websocket().await?;

    // Both should be connected
    assert!(unix_conn.is_connected());
    assert!(ws_conn.is_connected());

    // Control from Unix connection
    unix_conn.transport().play(ProjectContext::Current).await?;

    // Control from WebSocket connection
    ws_conn.transport().stop(ProjectContext::Current).await?;

    // Both connections should still work
    ws_conn.transport().play(ProjectContext::Current).await?;
    unix_conn.transport().stop(ProjectContext::Current).await?;

    Ok(())
}

/// Test reconnection after disconnecting.
///
/// Verifies:
/// - Can disconnect and reconnect
/// - Extension remains stable after client disconnects
#[tokio::test]
async fn test_reconnection() -> eyre::Result<()> {
    // Spawn test-extension
    let harness = TestExtensionHarness::spawn().await?;

    // Wait for it to be ready
    harness.wait_ready().await?;

    // First connection
    {
        let conn = harness.connect_unix().await?;
        conn.transport().play(ProjectContext::Current).await?;
        // Connection dropped here
    }

    // Small delay to let the extension handle the disconnect
    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

    // Second connection
    {
        let conn = harness.connect_unix().await?;
        conn.transport().stop(ProjectContext::Current).await?;
    }

    Ok(())
}

/// Test multiple WebSocket connections at once.
///
/// Verifies:
/// - Gateway can handle multiple browser connections
/// - All connections can send commands
#[tokio::test]
async fn test_multiple_websocket_connections() -> eyre::Result<()> {
    // Spawn test-extension
    let harness = TestExtensionHarness::spawn().await?;

    // Wait for it to be ready
    harness.wait_ready().await?;

    // Connect multiple WebSocket clients
    let ws_conn1 = harness.connect_websocket().await?;
    let ws_conn2 = harness.connect_websocket().await?;

    // Both should be connected
    assert!(ws_conn1.is_connected());
    assert!(ws_conn2.is_connected());

    // Commands from both should work
    ws_conn1.transport().play(ProjectContext::Current).await?;
    ws_conn2.transport().stop(ProjectContext::Current).await?;

    Ok(())
}
