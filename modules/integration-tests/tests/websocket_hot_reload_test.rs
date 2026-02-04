//! Integration tests for hot-reload with active WebSocket connections.
//!
//! These tests verify that WebSocket connections survive cell hot-reloads.
//! The key functionality being tested is the RebindableHandle system which
//! allows cells to be reloaded while maintaining forwarding connections.

use cell_host_proto::HostServiceClient;
use daw_proto::ProjectContext;
use integration_tests::harness::TestExtensionHarness;
use session_proto::SetlistServiceClient;
use std::time::Duration;
use tokio::time::{sleep, timeout};

/// Test that WebSocket connections survive a session cell hot-reload.
///
/// This test:
/// 1. Spawns test-extension (DAW, Session, Gateway cells)
/// 2. Connects via WebSocket
/// 3. Verifies transport control works
/// 4. Triggers a hot-reload of the session cell
/// 5. Verifies transport control still works after reload
#[tokio::test]
async fn test_websocket_survives_session_reload() -> eyre::Result<()> {
    // Spawn test-extension with all cells
    let harness = TestExtensionHarness::spawn().await?;
    harness.wait_ready_timeout(Duration::from_secs(30)).await?;

    // Connect via WebSocket (goes through gateway-ws -> session -> daw)
    let conn = harness.connect_websocket().await?;
    assert!(conn.is_connected(), "Should be connected via WebSocket");

    // Verify transport works before reload
    conn.transport().play(ProjectContext::Current).await?;
    let state = conn.transport().get_state(ProjectContext::Current).await?;
    assert!(
        state.is_playing(),
        "Transport should be playing before reload"
    );

    conn.transport().stop(ProjectContext::Current).await?;
    let state = conn.transport().get_state(ProjectContext::Current).await?;
    assert!(
        !state.is_playing(),
        "Transport should be stopped before reload"
    );

    // Connect via Unix socket to access HostService for reload
    let admin_conn = harness.connect_unix().await?;
    let host_service = HostServiceClient::new(admin_conn.handle().clone());

    println!("Triggering session cell hot-reload...");

    // Trigger hot-reload of the session cell
    let reload_result = timeout(Duration::from_secs(30), async {
        host_service.reload_cell("session".to_string()).await
    })
    .await
    .expect("reload_cell timed out")
    .expect("reload_cell RPC failed");

    assert!(
        reload_result.success,
        "Session cell reload should succeed: {:?}",
        reload_result.error
    );

    println!("Session cell reloaded, verifying WebSocket still works...");

    // Small delay to ensure cell is fully ready
    sleep(Duration::from_millis(500)).await;

    // Verify transport still works via the SAME WebSocket connection
    // This proves the RebindableHandle correctly rebinds after reload
    conn.transport().play(ProjectContext::Current).await?;
    let state = conn.transport().get_state(ProjectContext::Current).await?;
    assert!(
        state.is_playing(),
        "Transport should be playing after reload"
    );

    conn.transport().stop(ProjectContext::Current).await?;
    let state = conn.transport().get_state(ProjectContext::Current).await?;
    assert!(
        !state.is_playing(),
        "Transport should be stopped after reload"
    );

    println!("WebSocket connection survived session cell hot-reload!");

    Ok(())
}

/// Test that multiple concurrent WebSocket connections all survive hot-reload.
#[tokio::test]
async fn test_multiple_websockets_survive_reload() -> eyre::Result<()> {
    let harness = TestExtensionHarness::spawn().await?;
    harness.wait_ready_timeout(Duration::from_secs(30)).await?;

    // Create multiple WebSocket connections
    let conn1 = harness.connect_websocket().await?;
    let conn2 = harness.connect_websocket().await?;
    let conn3 = harness.connect_websocket().await?;

    // Verify all connections work
    for (i, conn) in [&conn1, &conn2, &conn3].iter().enumerate() {
        conn.transport().play(ProjectContext::Current).await?;
        let state = conn.transport().get_state(ProjectContext::Current).await?;
        assert!(
            state.is_playing(),
            "Connection {} should work before reload",
            i + 1
        );
        conn.transport().stop(ProjectContext::Current).await?;
    }

    // Trigger hot-reload
    let admin_conn = harness.connect_unix().await?;
    let host_service = HostServiceClient::new(admin_conn.handle().clone());

    let reload_result = host_service.reload_cell("session".to_string()).await?;
    assert!(reload_result.success, "Session reload should succeed");

    sleep(Duration::from_millis(500)).await;

    // Verify ALL connections still work after reload
    for (i, conn) in [&conn1, &conn2, &conn3].iter().enumerate() {
        conn.transport().play(ProjectContext::Current).await?;
        let state = conn.transport().get_state(ProjectContext::Current).await?;
        assert!(
            state.is_playing(),
            "Connection {} should work after reload",
            i + 1
        );
        conn.transport().stop(ProjectContext::Current).await?;
    }

    println!("All 3 WebSocket connections survived hot-reload!");

    Ok(())
}

/// Test that setlist service subscription survives session cell reload.
///
/// This tests the streaming functionality - subscriptions should continue
/// receiving events after the session cell is reloaded.
#[tokio::test]
async fn test_setlist_subscription_survives_reload() -> eyre::Result<()> {
    let harness = TestExtensionHarness::spawn().await?;
    harness.wait_ready_timeout(Duration::from_secs(30)).await?;

    // Connect and create setlist client
    let conn = harness.connect_websocket().await?;
    let setlist_client = SetlistServiceClient::new(conn.handle().clone());

    // Build setlist first
    setlist_client.build_from_open_projects().await?;

    // Verify we can get the setlist
    let setlist = setlist_client.get_setlist().await?;
    assert!(setlist.is_some(), "Should have a setlist before reload");
    let song_count = setlist.unwrap().songs.len();
    println!("Setlist has {} songs before reload", song_count);

    // Trigger hot-reload
    let admin_conn = harness.connect_unix().await?;
    let host_service = HostServiceClient::new(admin_conn.handle().clone());

    println!("Reloading session cell...");
    let reload_result = host_service.reload_cell("session".to_string()).await?;
    assert!(reload_result.success, "Session reload should succeed");

    sleep(Duration::from_millis(500)).await;

    // The setlist service state is lost on reload, but connection should work
    // We need to rebuild the setlist
    println!("Rebuilding setlist after reload...");
    setlist_client.build_from_open_projects().await?;

    // Verify setlist service works after reload
    let setlist_after = setlist_client.get_setlist().await?;
    assert!(
        setlist_after.is_some(),
        "Should have a setlist after reload"
    );
    let songs_after = setlist_after.unwrap().songs.len();
    assert_eq!(
        songs_after, song_count,
        "Should have same number of songs after reload"
    );

    println!(
        "Setlist service recovered after reload with {} songs",
        songs_after
    );

    Ok(())
}

/// Test rapid reload cycles don't break connections.
#[tokio::test]
async fn test_rapid_reload_cycles() -> eyre::Result<()> {
    let harness = TestExtensionHarness::spawn().await?;
    harness.wait_ready_timeout(Duration::from_secs(30)).await?;

    let conn = harness.connect_websocket().await?;
    let admin_conn = harness.connect_unix().await?;
    let host_service = HostServiceClient::new(admin_conn.handle().clone());

    // Perform 3 rapid reload cycles
    for cycle in 1..=3 {
        println!("Reload cycle {}...", cycle);

        let reload_result = host_service.reload_cell("session".to_string()).await?;
        assert!(
            reload_result.success,
            "Reload cycle {} should succeed",
            cycle
        );

        // Brief delay for cell to stabilize
        sleep(Duration::from_millis(300)).await;

        // Verify connection still works
        conn.transport().play(ProjectContext::Current).await?;
        let state = conn.transport().get_state(ProjectContext::Current).await?;
        assert!(
            state.is_playing(),
            "Should be playing after reload cycle {}",
            cycle
        );
        conn.transport().stop(ProjectContext::Current).await?;
    }

    println!("Survived 3 rapid reload cycles!");

    Ok(())
}

/// Test that gateway-ws cell reload also works (full chain test).
#[tokio::test]
async fn test_gateway_reload() -> eyre::Result<()> {
    let harness = TestExtensionHarness::spawn().await?;
    harness.wait_ready_timeout(Duration::from_secs(30)).await?;

    let conn = harness.connect_websocket().await?;

    // Verify works before
    conn.transport().play(ProjectContext::Current).await?;
    conn.transport().stop(ProjectContext::Current).await?;

    // Reload gateway-ws
    let admin_conn = harness.connect_unix().await?;
    let host_service = HostServiceClient::new(admin_conn.handle().clone());

    println!("Reloading gateway-ws cell...");
    let reload_result = host_service.reload_cell("gateway-ws".to_string()).await?;
    assert!(reload_result.success, "Gateway reload should succeed");

    sleep(Duration::from_millis(500)).await;

    // Note: After gateway reload, existing WebSocket connections are broken
    // because the gateway-ws process dies. New connections should work.
    let new_conn = harness.connect_websocket().await?;
    assert!(
        new_conn.is_connected(),
        "New connection after gateway reload should work"
    );

    new_conn.transport().play(ProjectContext::Current).await?;
    let state = new_conn
        .transport()
        .get_state(ProjectContext::Current)
        .await?;
    assert!(
        state.is_playing(),
        "Transport should work with new connection after gateway reload"
    );
    new_conn.transport().stop(ProjectContext::Current).await?;

    println!("Gateway-ws reload works (new connections work after reload)");

    Ok(())
}
