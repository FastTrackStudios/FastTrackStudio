//! Integration tests for cell hot-reload functionality.
//!
//! These tests verify that cells can be spawned and the system remains stable
//! under various connection patterns. The tests use WebSocket connections
//! which go through the gateway-ws cell.
//!
//! NOTE: Unix socket tests are skipped due to a pre-existing connection issue.

use daw_proto::ProjectContext;
use integration_tests::harness::TestExtensionHarness;
use std::time::Duration;
use tokio::time::sleep;

/// Test that we can connect to test-extension and cells are working via WebSocket.
#[tokio::test]
async fn test_cells_are_working_via_websocket() -> eyre::Result<()> {
    // Spawn test-extension (this starts DAW, Session, and Gateway cells)
    let harness = TestExtensionHarness::spawn().await?;
    harness.wait_ready().await?;

    // Connect via WebSocket (goes through gateway-ws cell)
    let conn = harness.connect_websocket().await?;
    assert!(conn.is_connected());

    // Verify transport works (DAW cell, routed through gateway)
    conn.transport().play(ProjectContext::Current).await?;
    let state = conn.transport().get_state(ProjectContext::Current).await?;
    assert!(state.is_playing(), "Transport should be playing");

    conn.transport().stop(ProjectContext::Current).await?;
    let state = conn.transport().get_state(ProjectContext::Current).await?;
    assert!(!state.is_playing(), "Transport should be stopped");

    Ok(())
}

/// Test that after killing and respawning extension, cells recover.
///
/// This is a higher-level test that verifies the hot-reload infrastructure
/// allows the system to recover when cells are restarted.
#[tokio::test]
async fn test_extension_restart_recovery_websocket() -> eyre::Result<()> {
    // First instance
    let mut harness = TestExtensionHarness::spawn().await?;
    harness.wait_ready().await?;

    let conn = harness.connect_websocket().await?;
    conn.transport().play(ProjectContext::Current).await?;

    // Kill the extension
    harness.kill();

    // Small delay for cleanup
    sleep(Duration::from_millis(500)).await;

    // Spawn a fresh extension
    let harness2 = TestExtensionHarness::spawn().await?;
    harness2.wait_ready().await?;

    // Connect to the new instance
    let conn2 = harness2.connect_websocket().await?;
    assert!(conn2.is_connected());

    // Verify it works
    conn2.transport().play(ProjectContext::Current).await?;
    let state = conn2.transport().get_state(ProjectContext::Current).await?;
    assert!(state.is_playing(), "New transport should be playing");

    Ok(())
}

/// Test multiple sequential WebSocket connections to verify stability.
#[tokio::test]
async fn test_multiple_sequential_websocket_connections() -> eyre::Result<()> {
    let harness = TestExtensionHarness::spawn().await?;
    harness.wait_ready().await?;

    // Connect, use, disconnect multiple times
    for i in 0..3 {
        let conn = harness.connect_websocket().await?;
        assert!(
            conn.is_connected(),
            "Connection {} should be established",
            i
        );

        conn.transport().play(ProjectContext::Current).await?;
        conn.transport().stop(ProjectContext::Current).await?;

        // Drop connection
        drop(conn);

        // Small delay between connections
        sleep(Duration::from_millis(100)).await;
    }

    Ok(())
}

/// Test that WebSocket gateway remains stable under reconnection.
#[tokio::test]
async fn test_websocket_gateway_stability() -> eyre::Result<()> {
    let harness = TestExtensionHarness::spawn().await?;
    harness.wait_ready().await?;

    // Multiple WebSocket connections
    for i in 0..3 {
        let conn = harness.connect_websocket().await?;
        assert!(
            conn.is_connected(),
            "WebSocket connection {} should work",
            i
        );

        conn.transport().play(ProjectContext::Current).await?;
        let state = conn.transport().get_state(ProjectContext::Current).await?;
        assert!(
            state.is_playing(),
            "Transport should be playing via WS {}",
            i
        );

        conn.transport().stop(ProjectContext::Current).await?;
        drop(conn);

        sleep(Duration::from_millis(100)).await;
    }

    Ok(())
}

/// Test multiple concurrent WebSocket connections.
///
/// This tests that the gateway-ws cell can handle multiple browser connections
/// at once, which is important for the hot-reload scenario where the control
/// app might reconnect while other connections are active.
#[tokio::test]
async fn test_concurrent_websocket_connections() -> eyre::Result<()> {
    let harness = TestExtensionHarness::spawn().await?;
    harness.wait_ready().await?;

    // Connect multiple WebSocket clients simultaneously
    let ws_conn1 = harness.connect_websocket().await?;
    let ws_conn2 = harness.connect_websocket().await?;

    assert!(ws_conn1.is_connected());
    assert!(ws_conn2.is_connected());

    // Control from first connection
    ws_conn1.transport().play(ProjectContext::Current).await?;

    // Verify from second connection
    let state = ws_conn2
        .transport()
        .get_state(ProjectContext::Current)
        .await?;
    assert!(
        state.is_playing(),
        "State should be synced across WebSocket connections"
    );

    // Control from second connection
    ws_conn2.transport().stop(ProjectContext::Current).await?;

    // Verify from first connection
    let state = ws_conn1
        .transport()
        .get_state(ProjectContext::Current)
        .await?;
    assert!(!state.is_playing(), "State should be synced back");

    Ok(())
}

/// Test rapid play/stop cycles to ensure the system remains stable.
///
/// This simulates rapid user interactions that might occur during development
/// with hot-reload enabled.
#[tokio::test]
async fn test_rapid_transport_cycles() -> eyre::Result<()> {
    let harness = TestExtensionHarness::spawn().await?;
    harness.wait_ready().await?;

    let conn = harness.connect_websocket().await?;

    // Rapid play/stop cycles
    for i in 0..5 {
        conn.transport().play(ProjectContext::Current).await?;
        let state = conn.transport().get_state(ProjectContext::Current).await?;
        assert!(state.is_playing(), "Should be playing in cycle {}", i);

        conn.transport().stop(ProjectContext::Current).await?;
        let state = conn.transport().get_state(ProjectContext::Current).await?;
        assert!(!state.is_playing(), "Should be stopped in cycle {}", i);
    }

    Ok(())
}
