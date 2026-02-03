//! Integration test harness for DAW cells
//!
//! Provides utilities for testing DAW cells with both in-process and multi-process scenarios.
//!
//! # Modules
//!
//! - [`harness`] - Test harness for spawning and managing test-extension
//!
//! # Example
//!
//! ```ignore
//! use integration_tests::harness::TestExtensionHarness;
//!
//! #[tokio::test]
//! async fn test_connection() {
//!     let harness = TestExtensionHarness::spawn().await.unwrap();
//!     harness.wait_ready().await.unwrap();
//!
//!     let conn = harness.connect_unix().await.unwrap();
//!     conn.transport().play(None).await.unwrap();
//! }
//! ```

#![deny(unsafe_code)]

pub mod harness;

use daw_proto::transport::transport::TransportServiceDispatcher;
use daw_standalone::{StandaloneProject, StandaloneTransport};
use roam_shm::driver::{establish_guest, establish_multi_peer_host};
use roam_shm::host::ShmHost;
use roam_shm::layout::SegmentConfig;
use roam_shm::spawn::AddPeerOptions;
use roam_shm::transport::ShmGuestTransport;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::time::{Duration, sleep};

/// Test implementation of Transport service - uses StandaloneTransport from daw-standalone
pub type TestTransport = StandaloneTransport;

/// Test fixture for host-guest testing
pub struct TestFixture {
    pub guest_handle: roam::session::ConnectionHandle,
    pub host_handle: roam::session::ConnectionHandle,
    pub _temp_dir: tempfile::TempDir,
}

/// Sets up an in-process test with host and guest
pub fn setup_test() -> TestFixture {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("test.shm");

    let config = SegmentConfig::default();
    let mut host = ShmHost::create(&path, config).unwrap();

    let ticket = host
        .add_peer(AddPeerOptions {
            peer_name: Some("test-guest".to_string()),
            on_death: None,
            ..Default::default()
        })
        .unwrap();

    let peer_id = ticket.peer_id;
    let spawn_args = ticket.into_spawn_args();

    // Create project first so we can share its state with transport
    let project = StandaloneProject::new();
    let dispatcher = TransportServiceDispatcher::new(TestTransport::new(project.shared_state()));

    let guest_transport = ShmGuestTransport::from_spawn_args(spawn_args).unwrap();
    let (guest_handle, _guest_incoming, guest_driver) =
        establish_guest(guest_transport, dispatcher.clone());

    let (host_driver, mut handles, _host_incoming, _driver_handle) =
        establish_multi_peer_host::<TransportServiceDispatcher<TestTransport>, _>(
            host,
            vec![(peer_id, dispatcher)],
        );
    let host_handle = handles.remove(&peer_id).unwrap();

    tokio::spawn(guest_driver.run());
    tokio::spawn(host_driver.run());

    TestFixture {
        guest_handle,
        host_handle,
        _temp_dir: dir,
    }
}

/// Sets up a test that spawns an external binary
pub async fn setup_external_test(
    binary_name: &str,
) -> (roam::session::ConnectionHandle, tempfile::TempDir) {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("daw-test.shm");

    let config = SegmentConfig::default();
    let mut host = ShmHost::create(&path, config).unwrap();

    let ticket = host
        .add_peer(AddPeerOptions {
            peer_name: Some(binary_name.to_string()),
            on_death: Some(Arc::new(|peer_id| {
                eprintln!("Cell {:?} died!", peer_id);
            })),
            ..Default::default()
        })
        .unwrap();

    let peer_id = ticket.peer_id;

    // Find the binary
    let binary = find_binary(binary_name).expect(&format!(
        "Could not find {} binary. Run `cargo build -p {}` first.",
        binary_name, binary_name
    ));

    // Spawn the binary
    let _child = ticket.spawn(std::process::Command::new(&binary)).unwrap();

    // Give it time to start
    sleep(Duration::from_millis(100)).await;

    // Set up host - create project first for shared state
    let project = StandaloneProject::new();
    let dispatcher = TransportServiceDispatcher::new(TestTransport::new(project.shared_state()));
    let (host_driver, mut handles, _incoming, _driver_handle) =
        establish_multi_peer_host::<TransportServiceDispatcher<TestTransport>, _>(
            host,
            vec![(peer_id, dispatcher)],
        );
    let host_handle = handles.remove(&peer_id).unwrap();

    tokio::spawn(host_driver.run());

    (host_handle, dir)
}

/// Find a binary in the target directory
fn find_binary(name: &str) -> Option<PathBuf> {
    // Try multiple possible locations
    let possible_paths = [
        // Current directory (when running from workspace root)
        PathBuf::from("target/debug").join(name),
        // One level up (when running from a crate directory)
        PathBuf::from("../target/debug").join(name),
        // Two levels up (when running from cells/daw/daw-standalone)
        PathBuf::from("../../target/debug").join(name),
        // Three levels up (when running from deep in the tree)
        PathBuf::from("../../../target/debug").join(name),
        // Absolute path from CARGO_MANIFEST_DIR
        PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap_or_default())
            .join("../../../target/debug")
            .join(name),
    ];

    for path in &possible_paths {
        if path.exists() {
            return Some(path.clone());
        }
    }

    None
}
