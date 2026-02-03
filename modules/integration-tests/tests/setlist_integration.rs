//! Integration tests for setlist building through the full RPC stack
//!
//! These tests spawn test-extension and verify that the SetlistService
//! correctly builds setlists from the daw-standalone mock data.

use integration_tests::harness::TestExtensionHarness;
use session_proto::SetlistServiceClient;
use std::time::Duration;
use tokio::time::timeout;

/// Test that SetlistService can build a setlist from open projects
#[tokio::test]
async fn test_setlist_service_builds_from_projects() {
    // Spawn test-extension
    let harness = TestExtensionHarness::spawn()
        .await
        .expect("Failed to spawn test-extension");

    // Wait for it to be ready
    harness
        .wait_ready_timeout(Duration::from_secs(30))
        .await
        .expect("Test extension not ready in time");

    println!("Test extension ready, connecting via Unix socket...");

    // Connect via Unix socket
    let conn = harness
        .connect_unix()
        .await
        .expect("Failed to connect to test-extension");

    println!("Connected! Creating SetlistServiceClient...");

    // Create setlist service client
    let setlist_client = SetlistServiceClient::new(conn.handle().clone());

    // Build setlist from open projects
    println!("Calling build_from_open_projects...");
    let result = timeout(Duration::from_secs(10), async {
        setlist_client.build_from_open_projects().await
    })
    .await;

    match result {
        Ok(Ok(())) => println!("build_from_open_projects succeeded"),
        Ok(Err(e)) => panic!("build_from_open_projects failed: {}", e),
        Err(_) => panic!("build_from_open_projects timed out"),
    }

    // Get the setlist
    println!("Getting setlist...");
    let setlist = timeout(Duration::from_secs(10), async {
        setlist_client.get_setlist().await
    })
    .await
    .expect("get_setlist timed out")
    .expect("get_setlist failed");

    println!(
        "Got setlist: {:?}",
        setlist.as_ref().map(|s| (&s.name, s.songs.len()))
    );

    // Verify we got songs
    let setlist = setlist.expect("Expected a setlist, got None");

    println!("Setlist: {} ({} songs)", setlist.name, setlist.songs.len());
    for (i, song) in setlist.songs.iter().enumerate() {
        println!(
            "  Song {}: {} ({} sections)",
            i,
            song.name,
            song.sections.len()
        );
    }

    assert_eq!(
        setlist.songs.len(),
        3,
        "Expected 3 songs from mock data, got {}",
        setlist.songs.len()
    );

    // Verify song names
    assert_eq!(setlist.songs[0].name, "How Great is Our God");
    assert_eq!(setlist.songs[1].name, "Holy, Holy, Holy");
    assert_eq!(setlist.songs[2].name, "Amazing Grace");

    // Verify each song has sections
    for song in &setlist.songs {
        assert!(
            !song.sections.is_empty(),
            "Song '{}' should have sections",
            song.name
        );
        println!(
            "  {}: {} sections, {:.0}s - {:.0}s",
            song.name,
            song.sections.len(),
            song.start_seconds,
            song.end_seconds
        );
    }
}
