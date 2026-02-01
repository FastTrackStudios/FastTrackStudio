//! Integration tests for test-extension.
//!
//! These tests spawn the full test-extension binary and verify
//! connections via Unix socket and WebSocket work correctly.

use integration_tests::harness::TestExtensionHarness;

/// Test connecting to test-extension via Unix socket.
///
/// Verifies:
/// - Extension spawns successfully
/// - Unix socket becomes available
/// - Can connect and receive host identity
/// - Transport controls work (play/stop)
#[tokio::test]
async fn test_unix_socket_connection() {
    // Spawn test-extension
    let harness = TestExtensionHarness::spawn()
        .await
        .expect("Failed to spawn test-extension");

    // Wait for it to be ready
    harness
        .wait_ready()
        .await
        .expect("Extension failed to become ready");

    // Connect via Unix socket
    let conn = harness
        .connect_unix()
        .await
        .expect("Failed to connect via Unix socket");

    // Verify we got a connection (identity may or may not be present depending on host config)
    assert!(conn.is_connected());

    // Test transport controls
    conn.transport()
        .play(None)
        .await
        .expect("play() should succeed");

    conn.transport()
        .stop(None)
        .await
        .expect("stop() should succeed");
}

/// Test connecting to test-extension via WebSocket.
///
/// Verifies:
/// - Extension spawns successfully
/// - WebSocket gateway becomes available
/// - Can connect via WebSocket
/// - Transport controls work (play/stop)
#[tokio::test]
async fn test_websocket_connection() {
    // Spawn test-extension
    let harness = TestExtensionHarness::spawn()
        .await
        .expect("Failed to spawn test-extension");

    // Wait for it to be ready
    harness
        .wait_ready()
        .await
        .expect("Extension failed to become ready");

    // Connect via WebSocket
    let conn = harness
        .connect_websocket()
        .await
        .expect("Failed to connect via WebSocket");

    // Verify we got a connection
    assert!(conn.is_connected());

    // Test transport controls
    conn.transport()
        .play(None)
        .await
        .expect("play() should succeed");

    conn.transport()
        .stop(None)
        .await
        .expect("stop() should succeed");
}

/// Test that both Unix socket and WebSocket connections work simultaneously.
///
/// Verifies:
/// - Both connection types can be active at once
/// - Commands from either connection work
/// - No interference between connections
#[tokio::test]
async fn test_concurrent_connections() {
    // Spawn test-extension
    let harness = TestExtensionHarness::spawn()
        .await
        .expect("Failed to spawn test-extension");

    // Wait for it to be ready
    harness
        .wait_ready()
        .await
        .expect("Extension failed to become ready");

    // Connect via both methods
    let unix_conn = harness
        .connect_unix()
        .await
        .expect("Failed to connect via Unix socket");

    let ws_conn = harness
        .connect_websocket()
        .await
        .expect("Failed to connect via WebSocket");

    // Both should be connected
    assert!(unix_conn.is_connected());
    assert!(ws_conn.is_connected());

    // Control from Unix connection
    unix_conn
        .transport()
        .play(None)
        .await
        .expect("play() from Unix should succeed");

    // Control from WebSocket connection
    ws_conn
        .transport()
        .stop(None)
        .await
        .expect("stop() from WebSocket should succeed");

    // Both connections should still work
    ws_conn
        .transport()
        .play(None)
        .await
        .expect("play() from WebSocket should succeed");

    unix_conn
        .transport()
        .stop(None)
        .await
        .expect("stop() from Unix should succeed");
}

/// Test reconnection after disconnecting.
///
/// Verifies:
/// - Can disconnect and reconnect
/// - Extension remains stable after client disconnects
#[tokio::test]
async fn test_reconnection() {
    // Spawn test-extension
    let harness = TestExtensionHarness::spawn()
        .await
        .expect("Failed to spawn test-extension");

    // Wait for it to be ready
    harness
        .wait_ready()
        .await
        .expect("Extension failed to become ready");

    // First connection
    {
        let conn = harness
            .connect_unix()
            .await
            .expect("Failed to connect via Unix socket");

        conn.transport()
            .play(None)
            .await
            .expect("play() should succeed");

        // Connection dropped here
    }

    // Small delay to let the extension handle the disconnect
    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

    // Second connection
    {
        let conn = harness
            .connect_unix()
            .await
            .expect("Failed to reconnect via Unix socket");

        conn.transport()
            .stop(None)
            .await
            .expect("stop() should succeed after reconnect");
    }
}

/// Test multiple WebSocket connections at once.
///
/// Verifies:
/// - Gateway can handle multiple browser connections
/// - All connections can send commands
#[tokio::test]
async fn test_multiple_websocket_connections() {
    // Spawn test-extension
    let harness = TestExtensionHarness::spawn()
        .await
        .expect("Failed to spawn test-extension");

    // Wait for it to be ready
    harness
        .wait_ready()
        .await
        .expect("Extension failed to become ready");

    // Connect multiple WebSocket clients
    let ws_conn1 = harness
        .connect_websocket()
        .await
        .expect("Failed to connect first WebSocket");

    let ws_conn2 = harness
        .connect_websocket()
        .await
        .expect("Failed to connect second WebSocket");

    // Both should be connected
    assert!(ws_conn1.is_connected());
    assert!(ws_conn2.is_connected());

    // Commands from both should work
    ws_conn1
        .transport()
        .play(None)
        .await
        .expect("play() from first WS should succeed");

    ws_conn2
        .transport()
        .stop(None)
        .await
        .expect("stop() from second WS should succeed");
}
