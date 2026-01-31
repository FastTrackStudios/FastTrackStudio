//! Integration test harness for DAW cells
//!
//! Provides utilities for testing DAW cells with both in-process and multi-process scenarios.

#![deny(unsafe_code)]

use daw_proto::{Transport, TransportState, TransportStateUpdate};
use roam::session::Tx;
use roam_shm::driver::{establish_guest, establish_multi_peer_host};
use roam_shm::host::ShmHost;
use roam_shm::layout::SegmentConfig;
use roam_shm::spawn::AddPeerOptions;
use roam_shm::transport::ShmGuestTransport;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::Mutex;
use tokio::time::{sleep, Duration};

/// Test implementation of Transport service
#[derive(Clone)]
pub struct TestTransport {
    state: Arc<Mutex<TransportState>>,
    update_tx: Arc<Mutex<Option<Tx<TransportStateUpdate>>>>,
}

impl TestTransport {
    pub fn new() -> Self {
        Self {
            state: Arc::new(Mutex::new(TransportState::Stopped)),
            update_tx: Arc::new(Mutex::new(None)),
        }
    }

    pub async fn broadcast_update(&self) {
        let state = *self.state.lock().await;
        let update = TransportStateUpdate {
            state,
            position: daw_proto::TimePosition::from_seconds(0.0),
            tempo: 120.0,
        };

        if let Some(tx) = self.update_tx.lock().await.as_ref() {
            let _ = tx.send(&update).await;
        }
    }
}

impl Transport for TestTransport {
    async fn play(&self, _cx: &roam::Context) {
        let mut state = self.state.lock().await;
        *state = TransportState::Playing;
        drop(state);
        self.broadcast_update().await;
    }

    async fn stop(&self, _cx: &roam::Context) {
        let mut state = self.state.lock().await;
        *state = TransportState::Stopped;
        drop(state);
        self.broadcast_update().await;
    }

    async fn subscribe_state(&self, _cx: &roam::Context, updates: Tx<TransportStateUpdate>) {
        *self.update_tx.lock().await = Some(updates);
        self.broadcast_update().await;
    }
}

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

    let dispatcher = daw_proto::TransportDispatcher::new(TestTransport::new());

    let guest_transport = ShmGuestTransport::from_spawn_args(spawn_args).unwrap();
    let (guest_handle, _guest_incoming, guest_driver) =
        establish_guest(guest_transport, dispatcher.clone());

    let (host_driver, mut handles, _host_incoming, _driver_handle) =
        establish_multi_peer_host::<daw_proto::TransportDispatcher<TestTransport>, _>(
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
pub async fn setup_external_test(binary_name: &str) -> (roam::session::ConnectionHandle, tempfile::TempDir) {
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

    // Set up host
    let dispatcher = daw_proto::TransportDispatcher::new(TestTransport::new());
    let (host_driver, mut handles, _incoming, _driver_handle) =
        establish_multi_peer_host::<daw_proto::TransportDispatcher<TestTransport>, _>(
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