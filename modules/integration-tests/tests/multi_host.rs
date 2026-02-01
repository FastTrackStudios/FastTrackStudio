//! Multi-host integration tests
//!
//! Tests the architecture where:
//! 1. Multiple hosts run independently (each with their own Unix socket)
//! 2. A "desktop app" connects to multiple hosts via Unix sockets
//! 3. Hosts identify themselves with purpose and optional instance
//!
//! This validates the host-manager architecture using daw-standalone implementations.

use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

use daw_control::Daw;
use daw_proto::project::ProjectServiceDispatcher;
use daw_proto::transport::transport::TransportServiceDispatcher;
use daw_standalone::{StandaloneProject, StandaloneTransport};
use gateway_proto::{
    GatewayCoordinator, GatewayCoordinatorClient, GatewayCoordinatorDispatcher, GatewayInfo,
    GatewayState, GatewayType, TakeOverRequest, TakeOverResponse,
};
use host_manager::{HostConnection, HostManager};
use host_manager_proto::{HOST_IDENTITY_KEY, HostIdentity};
use roam::Context;
use roam::session::{HandshakeConfig, RoutedDispatcher};
use roam_local::LocalListener;
use roam_stream::CobsFramed;
use roam_wire::MetadataValue;
use tokio::sync::RwLock;
use tracing::{info, warn};

/// Well-known purpose values for testing
pub mod purposes {
    pub const DEFAULT: &str = "default";
    pub const TRACKS: &str = "tracks";
    pub const GUITAR: &str = "guitar";
    pub const KEYBOARD: &str = "keyboard";
}

// ============================================================================
// Test Host - Simulates a DAW host with Unix socket server
// ============================================================================

/// A test host that simulates a real roam host.
/// It exposes TransportService and ProjectService (via daw-standalone) over a Unix socket.
struct TestHost {
    name: String,
    purpose: String,
    instance: Option<String>,
    socket_path: PathBuf,
    transport: StandaloneTransport,
    gateway_state: Arc<RwLock<GatewayState>>,
    _temp_dir: tempfile::TempDir,
}

impl TestHost {
    fn new(name: &str) -> Self {
        Self::new_with_purpose(name, purposes::DEFAULT)
    }

    fn new_with_purpose(name: &str, purpose: &str) -> Self {
        Self::new_with_instance(name, purpose, None)
    }

    fn new_with_instance(name: &str, purpose: &str, instance: Option<&str>) -> Self {
        let temp_dir = tempfile::tempdir().unwrap();
        let socket_path = temp_dir.path().join(format!("{}.sock", name));

        Self {
            name: name.to_string(),
            purpose: purpose.to_string(),
            instance: instance.map(String::from),
            socket_path,
            transport: StandaloneTransport::new(),
            gateway_state: Arc::new(RwLock::new(GatewayState::Active)),
            _temp_dir: temp_dir,
        }
    }

    fn socket_path(&self) -> &PathBuf {
        &self.socket_path
    }

    async fn is_playing(&self) -> bool {
        self.transport.is_playing().await
    }

    /// Start the Unix socket server for this host.
    async fn start(&self) -> eyre::Result<()> {
        let listener = LocalListener::bind(&self.socket_path)?;
        info!(
            "[{}] Unix socket listening at: {:?} (purpose: {}, instance: {:?})",
            self.name, self.socket_path, self.purpose, self.instance
        );

        let name = self.name.clone();
        let purpose = self.purpose.clone();
        let instance = self.instance.clone();
        let transport = self.transport.clone();
        let gateway_state = self.gateway_state.clone();

        tokio::spawn(async move {
            loop {
                match listener.accept().await {
                    Ok(stream) => {
                        info!("[{}] Client connected", name);
                        let transport = transport.clone();
                        let gateway_state = gateway_state.clone();
                        let name = name.clone();
                        let purpose = purpose.clone();
                        let instance = instance.clone();

                        tokio::spawn(async move {
                            let framed = CobsFramed::new(stream);

                            // Create dispatchers using daw-standalone implementations
                            let transport_dispatcher =
                                TransportServiceDispatcher::new(transport.clone());
                            let project_dispatcher =
                                ProjectServiceDispatcher::new(StandaloneProject::new());

                            let gateway_impl = TestGatewayCoordinator {
                                host_name: name.clone(),
                                state: gateway_state,
                            };
                            let gateway_dispatcher =
                                GatewayCoordinatorDispatcher::new(gateway_impl);

                            // Route: project + transport + gateway
                            let daw_dispatcher =
                                RoutedDispatcher::new(project_dispatcher, transport_dispatcher);
                            let dispatcher =
                                RoutedDispatcher::new(daw_dispatcher, gateway_dispatcher);

                            let (handle, _incoming, driver) = roam::session::accept_framed(
                                framed,
                                HandshakeConfig::default(),
                                dispatcher,
                            )
                            .await
                            .unwrap();

                            // Spawn driver FIRST so it can process messages
                            let driver_name = name.clone();
                            let driver_handle = tokio::spawn(async move {
                                if let Err(e) = driver.run().await {
                                    warn!("[{}] Driver error: {}", driver_name, e);
                                }
                            });

                            // Open virtual connection back to desktop with our identity
                            let identity = HostIdentity {
                                id: format!("{}-id", name),
                                name: name.clone(),
                                purpose: purpose.clone(),
                                instance: instance.clone(),
                                tags: vec![],
                            };

                            let identity_bytes = facet_postcard::to_vec(&identity).unwrap();
                            let metadata = vec![(
                                HOST_IDENTITY_KEY.to_string(),
                                MetadataValue::Bytes(identity_bytes),
                                0u64,
                            )];

                            // Create dispatcher for the virtual connection (services the host exposes)
                            let virtual_transport_dispatcher =
                                TransportServiceDispatcher::new(transport);
                            let virtual_project_dispatcher =
                                ProjectServiceDispatcher::new(StandaloneProject::new());
                            let virtual_dispatcher = RoutedDispatcher::new(
                                virtual_project_dispatcher,
                                virtual_transport_dispatcher,
                            );

                            match handle
                                .connect(metadata, Some(Box::new(virtual_dispatcher)))
                                .await
                            {
                                Ok(_virtual_handle) => {
                                    info!(
                                        "[{}] Opened virtual connection with purpose '{}'",
                                        name, purpose
                                    );
                                }
                                Err(e) => {
                                    warn!("[{}] Failed to open virtual connection: {}", name, e);
                                }
                            }

                            // Wait for driver to complete
                            let _ = driver_handle.await;
                            info!("[{}] Client disconnected", name);
                        });
                    }
                    Err(e) => {
                        warn!("[{}] Accept error: {}", name, e);
                        break;
                    }
                }
            }
        });

        // Give server time to start
        tokio::time::sleep(Duration::from_millis(50)).await;
        Ok(())
    }
}

/// Test implementation of GatewayCoordinator
#[derive(Clone)]
struct TestGatewayCoordinator {
    host_name: String,
    state: Arc<RwLock<GatewayState>>,
}

impl GatewayCoordinator for TestGatewayCoordinator {
    async fn take_over(&self, _cx: &Context, request: TakeOverRequest) -> TakeOverResponse {
        info!(
            "[{}] take_over({:?}) -> redirect to {}",
            self.host_name, request.gateway_type, request.redirect_info
        );
        *self.state.write().await = GatewayState::Suspended;
        TakeOverResponse {
            success: true,
            active_connections: 0,
        }
    }

    async fn release(&self, _cx: &Context, gateway_type: GatewayType) {
        info!("[{}] release({:?})", self.host_name, gateway_type);
        *self.state.write().await = GatewayState::Active;
    }

    async fn get_states(&self, _cx: &Context) -> Vec<GatewayInfo> {
        vec![GatewayInfo {
            gateway_type: GatewayType::WebSocket,
            state: *self.state.read().await,
            connection_count: 0,
        }]
    }
}

// ============================================================================
// Simple Desktop Client - Direct connection without host identity tracking
// ============================================================================

/// A simple client that connects to a host via Unix socket.
/// Uses daw-control's Daw API for transport/project access.
struct SimpleDesktopClient {
    daw: Daw,
    gateway: GatewayCoordinatorClient,
}

impl SimpleDesktopClient {
    async fn connect(socket_path: &PathBuf) -> eyre::Result<Self> {
        let stream = roam_local::connect(socket_path).await?;
        let framed = CobsFramed::new(stream);

        let (handle, _incoming, driver) = roam::session::initiate_framed(
            framed,
            HandshakeConfig::default(),
            roam::session::NoDispatcher,
        )
        .await?;

        // Spawn driver
        tokio::spawn(async move {
            let _ = driver.run().await;
        });

        Ok(Self {
            daw: Daw::new(handle.clone()),
            gateway: GatewayCoordinatorClient::new(handle),
        })
    }

    fn daw(&self) -> &Daw {
        &self.daw
    }

    async fn take_over_gateway(&self, redirect_url: &str) -> eyre::Result<TakeOverResponse> {
        Ok(self
            .gateway
            .take_over(TakeOverRequest {
                gateway_type: GatewayType::WebSocket,
                redirect_info: redirect_url.to_string(),
            })
            .await?)
    }

    async fn release_gateway(&self) -> eyre::Result<()> {
        self.gateway.release(GatewayType::WebSocket).await?;
        Ok(())
    }

    async fn get_gateway_states(&self) -> eyre::Result<Vec<GatewayInfo>> {
        Ok(self.gateway.get_states().await?)
    }
}

// ============================================================================
// Purpose-Aware Desktop Client - Tracks hosts by purpose via virtual connections
// ============================================================================

/// Desktop client that tracks hosts by their purpose using virtual connections.
///
/// When connected, hosts open virtual connections back with their identity.
/// This client tracks those connections and provides access by purpose.
struct PurposeAwareDesktopClient {
    manager: HostManager,
}

impl PurposeAwareDesktopClient {
    fn new() -> Self {
        Self {
            manager: HostManager::new(),
        }
    }

    /// Connect to a host.
    async fn connect(&mut self, socket_path: &PathBuf) -> eyre::Result<()> {
        self.manager.connect(socket_path, 0).await
    }

    /// Get a host by its purpose.
    async fn get_host_by_purpose(&self, purpose: &str) -> Option<HostConnection> {
        self.manager.get_live_host(purpose).await
    }

    /// Check if we have a host with the given purpose.
    async fn has_purpose(&self, purpose: &str) -> bool {
        self.manager.get_live_host(purpose).await.is_some()
    }

    /// Get all connected instance keys (purpose or purpose:instance).
    async fn instance_keys(&self) -> Vec<String> {
        self.manager.instance_keys().await
    }
}

// ============================================================================
// Tests
// ============================================================================

#[tokio::test]
async fn test_single_host_connection() {
    tracing_subscriber::fmt()
        .with_env_filter("info")
        .try_init()
        .ok();

    // Start a test host
    let host = TestHost::new("host-a");
    host.start().await.unwrap();

    // Connect as desktop client using daw-control
    let client = SimpleDesktopClient::connect(host.socket_path())
        .await
        .unwrap();

    // Test transport controls via daw-control API
    let project = client.daw().current_project().await.unwrap();
    let transport = project.transport();

    // Initially stopped
    assert!(!host.is_playing().await, "Host should not be playing yet");

    transport.play().await.unwrap();
    // Give time for async processing
    tokio::time::sleep(Duration::from_millis(50)).await;
    assert!(
        host.is_playing().await,
        "Host should be playing after play()"
    );

    transport.stop().await.unwrap();
    tokio::time::sleep(Duration::from_millis(50)).await;
    assert!(
        !host.is_playing().await,
        "Host should be stopped after stop()"
    );

    // Test project info
    let project_info = client
        .daw()
        .current_project()
        .await
        .map(|p| p.guid().to_string());
    assert!(project_info.is_ok(), "Should get project info");

    info!("Single host test passed!");
}

#[tokio::test]
async fn test_multi_host_connections() {
    tracing_subscriber::fmt()
        .with_env_filter("info")
        .try_init()
        .ok();

    // Start multiple test hosts
    let host_a = TestHost::new("host-a");
    let host_b = TestHost::new("host-b");
    let host_c = TestHost::new("host-c");

    host_a.start().await.unwrap();
    host_b.start().await.unwrap();
    host_c.start().await.unwrap();

    // Connect to all hosts (simulating desktop app managing multiple hosts)
    let client_a = SimpleDesktopClient::connect(host_a.socket_path())
        .await
        .unwrap();
    let client_b = SimpleDesktopClient::connect(host_b.socket_path())
        .await
        .unwrap();
    let client_c = SimpleDesktopClient::connect(host_c.socket_path())
        .await
        .unwrap();

    // Get transports
    let transport_a = client_a.daw().current_project().await.unwrap().transport();
    let transport_b = client_b.daw().current_project().await.unwrap().transport();
    let transport_c = client_c.daw().current_project().await.unwrap().transport();

    // Play on host A only
    transport_a.play().await.unwrap();
    tokio::time::sleep(Duration::from_millis(50)).await;

    // Verify only host A is playing
    assert!(host_a.is_playing().await);
    assert!(!host_b.is_playing().await);
    assert!(!host_c.is_playing().await);

    // Play on all hosts
    transport_b.play().await.unwrap();
    transport_c.play().await.unwrap();
    tokio::time::sleep(Duration::from_millis(50)).await;

    assert!(host_a.is_playing().await);
    assert!(host_b.is_playing().await);
    assert!(host_c.is_playing().await);

    // Stop all
    transport_a.stop().await.unwrap();
    transport_b.stop().await.unwrap();
    transport_c.stop().await.unwrap();
    tokio::time::sleep(Duration::from_millis(50)).await;

    assert!(!host_a.is_playing().await);
    assert!(!host_b.is_playing().await);
    assert!(!host_c.is_playing().await);

    info!("Multi-host test passed!");
}

#[tokio::test]
async fn test_gateway_takeover() {
    tracing_subscriber::fmt()
        .with_env_filter("info")
        .try_init()
        .ok();

    let host = TestHost::new("host-gateway");
    host.start().await.unwrap();

    let client = SimpleDesktopClient::connect(host.socket_path())
        .await
        .unwrap();

    // Check initial gateway state (should be Active)
    let states = client.get_gateway_states().await.unwrap();
    assert_eq!(states.len(), 1);
    assert_eq!(states[0].gateway_type, GatewayType::WebSocket);
    assert_eq!(states[0].state, GatewayState::Active);

    // Desktop takes over the gateway
    let response = client
        .take_over_gateway("ws://desktop:3030/ws")
        .await
        .unwrap();
    assert!(response.success);

    // Gateway should now be suspended
    let states = client.get_gateway_states().await.unwrap();
    assert_eq!(states[0].state, GatewayState::Suspended);

    // Desktop releases the gateway
    client.release_gateway().await.unwrap();

    // Gateway should be active again
    let states = client.get_gateway_states().await.unwrap();
    assert_eq!(states[0].state, GatewayState::Active);

    info!("Gateway takeover test passed!");
}

#[tokio::test]
async fn test_multi_host_gateway_takeover() {
    tracing_subscriber::fmt()
        .with_env_filter("info")
        .try_init()
        .ok();

    // Two hosts
    let host_a = TestHost::new("host-a");
    let host_b = TestHost::new("host-b");

    host_a.start().await.unwrap();
    host_b.start().await.unwrap();

    let client_a = SimpleDesktopClient::connect(host_a.socket_path())
        .await
        .unwrap();
    let client_b = SimpleDesktopClient::connect(host_b.socket_path())
        .await
        .unwrap();

    // Take over gateway on host A only
    client_a
        .take_over_gateway("ws://desktop:3030/ws")
        .await
        .unwrap();

    // Host A suspended, Host B still active
    let states_a = client_a.get_gateway_states().await.unwrap();
    let states_b = client_b.get_gateway_states().await.unwrap();

    assert_eq!(states_a[0].state, GatewayState::Suspended);
    assert_eq!(states_b[0].state, GatewayState::Active);

    // Now take over host B too
    client_b
        .take_over_gateway("ws://desktop:3030/ws")
        .await
        .unwrap();

    // Both suspended
    let states_a = client_a.get_gateway_states().await.unwrap();
    let states_b = client_b.get_gateway_states().await.unwrap();

    assert_eq!(states_a[0].state, GatewayState::Suspended);
    assert_eq!(states_b[0].state, GatewayState::Suspended);

    // Release both
    client_a.release_gateway().await.unwrap();
    client_b.release_gateway().await.unwrap();

    let states_a = client_a.get_gateway_states().await.unwrap();
    let states_b = client_b.get_gateway_states().await.unwrap();

    assert_eq!(states_a[0].state, GatewayState::Active);
    assert_eq!(states_b[0].state, GatewayState::Active);

    info!("Multi-host gateway takeover test passed!");
}

#[tokio::test]
async fn test_purpose_based_routing() {
    tracing_subscriber::fmt()
        .with_env_filter("info")
        .try_init()
        .ok();

    // Start hosts with different purposes
    let tracks_host = TestHost::new_with_purpose("host-tracks", purposes::TRACKS);
    let guitar_host = TestHost::new_with_purpose("host-guitar", purposes::GUITAR);
    let keyboard_host = TestHost::new_with_purpose("host-keyboard", purposes::KEYBOARD);

    tracks_host.start().await.unwrap();
    guitar_host.start().await.unwrap();
    keyboard_host.start().await.unwrap();

    // Desktop connects and receives virtual connections with purposes
    let mut desktop = PurposeAwareDesktopClient::new();
    desktop.connect(tracks_host.socket_path()).await.unwrap();
    desktop.connect(guitar_host.socket_path()).await.unwrap();
    desktop.connect(keyboard_host.socket_path()).await.unwrap();

    // Wait for virtual connections to be established
    tokio::time::sleep(Duration::from_millis(100)).await;

    // Verify we have all 3 hosts by purpose
    assert!(
        desktop.has_purpose("tracks").await,
        "Should have tracks host"
    );
    assert!(
        desktop.has_purpose("guitar").await,
        "Should have guitar host"
    );
    assert!(
        desktop.has_purpose("keyboard").await,
        "Should have keyboard host"
    );

    info!("Connected purposes: {:?}", desktop.instance_keys().await);

    // Get the tracks host and use daw-control API
    let tracks_conn = desktop.get_host_by_purpose("tracks").await.unwrap();
    let tracks_daw = Daw::new(tracks_conn.handle().clone());
    let tracks_transport = tracks_daw.current_project().await.unwrap().transport();

    // Play on tracks host only
    tracks_transport.play().await.unwrap();
    tokio::time::sleep(Duration::from_millis(50)).await;

    // Verify only tracks host is playing
    assert!(
        tracks_host.is_playing().await,
        "Tracks host should be playing"
    );
    assert!(
        !guitar_host.is_playing().await,
        "Guitar host should NOT be playing"
    );
    assert!(
        !keyboard_host.is_playing().await,
        "Keyboard host should NOT be playing"
    );

    // Can also control individual instrument hosts
    let guitar_conn = desktop.get_host_by_purpose("guitar").await.unwrap();
    let guitar_daw = Daw::new(guitar_conn.handle().clone());
    guitar_daw
        .current_project()
        .await
        .unwrap()
        .transport()
        .play()
        .await
        .unwrap();
    tokio::time::sleep(Duration::from_millis(50)).await;

    assert!(
        guitar_host.is_playing().await,
        "Guitar host should now be playing"
    );
    assert!(
        !keyboard_host.is_playing().await,
        "Keyboard host should still NOT be playing"
    );

    // Stop all
    tracks_transport.stop().await.unwrap();
    guitar_daw
        .current_project()
        .await
        .unwrap()
        .transport()
        .stop()
        .await
        .unwrap();
    tokio::time::sleep(Duration::from_millis(50)).await;

    assert!(!tracks_host.is_playing().await);
    assert!(!guitar_host.is_playing().await);

    info!("Purpose-based routing test passed!");
}

#[tokio::test]
async fn test_redundant_hosts_sync() {
    tracing_subscriber::fmt()
        .with_env_filter("info")
        .try_init()
        .ok();

    // Start two redundant hosts with the same purpose ("guitar")
    let guitar_primary = TestHost::new_with_purpose("guitar-primary", purposes::GUITAR);
    let guitar_secondary = TestHost::new_with_purpose("guitar-secondary", purposes::GUITAR);

    guitar_primary.start().await.unwrap();
    guitar_secondary.start().await.unwrap();

    // Connect with priorities (0 = primary, 1 = secondary)
    let mut manager = HostManager::new();
    manager
        .connect(guitar_primary.socket_path(), 0)
        .await
        .unwrap();
    manager
        .connect(guitar_secondary.socket_path(), 1)
        .await
        .unwrap();

    // Wait for virtual connections
    tokio::time::sleep(Duration::from_millis(100)).await;

    // Verify both hosts registered
    assert_eq!(
        manager.host_count("guitar").await,
        2,
        "Should have 2 guitar hosts"
    );

    // Get individual host connections
    let primary = manager.get_host("guitar", "guitar-primary").await.unwrap();
    let secondary = manager
        .get_host("guitar", "guitar-secondary")
        .await
        .unwrap();

    // The primary should automatically be live
    let live = manager.get_live_host("guitar").await.unwrap();
    assert_eq!(
        live.identity().name,
        "guitar-primary",
        "Primary should be live"
    );

    // Send play to BOTH hosts using daw-control API
    let primary_daw = Daw::new(primary.handle().clone());
    let secondary_daw = Daw::new(secondary.handle().clone());

    primary_daw
        .current_project()
        .await
        .unwrap()
        .transport()
        .play()
        .await
        .unwrap();
    secondary_daw
        .current_project()
        .await
        .unwrap()
        .transport()
        .play()
        .await
        .unwrap();
    tokio::time::sleep(Duration::from_millis(50)).await;

    // Both hosts should be playing (kept in sync by caller)
    assert!(
        guitar_primary.is_playing().await,
        "Primary should be playing"
    );
    assert!(
        guitar_secondary.is_playing().await,
        "Secondary should ALSO be playing"
    );

    // Stop both
    primary_daw
        .current_project()
        .await
        .unwrap()
        .transport()
        .stop()
        .await
        .unwrap();
    secondary_daw
        .current_project()
        .await
        .unwrap()
        .transport()
        .stop()
        .await
        .unwrap();
    tokio::time::sleep(Duration::from_millis(50)).await;

    assert!(
        !guitar_primary.is_playing().await,
        "Primary should be stopped"
    );
    assert!(
        !guitar_secondary.is_playing().await,
        "Secondary should be stopped"
    );

    info!("Redundant hosts sync test passed!");
}

#[tokio::test]
async fn test_redundant_hosts_failover() {
    tracing_subscriber::fmt()
        .with_env_filter("info")
        .try_init()
        .ok();

    // Start three redundant hosts for the "tracks" purpose
    let tracks_primary = TestHost::new_with_purpose("tracks-primary", purposes::TRACKS);
    let tracks_secondary = TestHost::new_with_purpose("tracks-secondary", purposes::TRACKS);
    let tracks_tertiary = TestHost::new_with_purpose("tracks-tertiary", purposes::TRACKS);

    tracks_primary.start().await.unwrap();
    tracks_secondary.start().await.unwrap();
    tracks_tertiary.start().await.unwrap();

    // Connect with priorities
    let mut manager = HostManager::new();
    manager
        .connect(tracks_primary.socket_path(), 0)
        .await
        .unwrap();
    manager
        .connect(tracks_secondary.socket_path(), 1)
        .await
        .unwrap();
    manager
        .connect(tracks_tertiary.socket_path(), 2)
        .await
        .unwrap();

    // Wait for virtual connections
    tokio::time::sleep(Duration::from_millis(100)).await;

    // Verify setup
    assert_eq!(
        manager.host_count("tracks").await,
        3,
        "Should have 3 tracks hosts"
    );

    // Primary should be live initially
    let live = manager.get_live_host("tracks").await.unwrap();
    assert_eq!(
        live.identity().name,
        "tracks-primary",
        "Primary should be live initially"
    );

    // Simulate primary failure
    info!("Simulating primary failure...");
    let primary = manager.get_host("tracks", "tracks-primary").await.unwrap();
    primary.mark_unhealthy().await;
    manager.check_failover("tracks").await;

    // The live host should have failed over to secondary
    let live = manager.get_live_host("tracks").await.unwrap();
    assert_eq!(
        live.identity().name,
        "tracks-secondary",
        "Secondary should now be live"
    );

    // Get healthy hosts and send play to them
    let secondary = manager
        .get_host("tracks", "tracks-secondary")
        .await
        .unwrap();
    let tertiary = manager.get_host("tracks", "tracks-tertiary").await.unwrap();

    let secondary_daw = Daw::new(secondary.handle().clone());
    let tertiary_daw = Daw::new(tertiary.handle().clone());

    secondary_daw
        .current_project()
        .await
        .unwrap()
        .transport()
        .play()
        .await
        .unwrap();
    tertiary_daw
        .current_project()
        .await
        .unwrap()
        .transport()
        .play()
        .await
        .unwrap();
    tokio::time::sleep(Duration::from_millis(50)).await;

    assert!(
        tracks_secondary.is_playing().await,
        "Secondary should be playing"
    );
    assert!(
        tracks_tertiary.is_playing().await,
        "Tertiary should be playing"
    );

    // Simulate secondary failure too
    info!("Simulating secondary failure...");
    secondary.mark_unhealthy().await;
    manager.check_failover("tracks").await;

    // Tertiary should now be live
    let live = manager.get_live_host("tracks").await.unwrap();
    assert_eq!(
        live.identity().name,
        "tracks-tertiary",
        "Tertiary should now be live"
    );

    info!("Redundant hosts failover test passed!");
}

#[tokio::test]
async fn test_manual_live_switch() {
    tracing_subscriber::fmt()
        .with_env_filter("info")
        .try_init()
        .ok();

    // Start two redundant hosts
    let keyboard_a = TestHost::new_with_purpose("keyboard-a", purposes::KEYBOARD);
    let keyboard_b = TestHost::new_with_purpose("keyboard-b", purposes::KEYBOARD);

    keyboard_a.start().await.unwrap();
    keyboard_b.start().await.unwrap();

    let mut manager = HostManager::new();
    manager.connect(keyboard_a.socket_path(), 0).await.unwrap();
    manager.connect(keyboard_b.socket_path(), 1).await.unwrap();

    tokio::time::sleep(Duration::from_millis(100)).await;

    // Verify initial live host
    let live = manager.get_live_host("keyboard").await.unwrap();
    assert_eq!(
        live.identity().name,
        "keyboard-a",
        "A should be live initially"
    );

    // Manually switch live to B
    manager
        .set_live_host("keyboard", "keyboard-b")
        .await
        .unwrap();

    let live = manager.get_live_host("keyboard").await.unwrap();
    assert_eq!(live.identity().name, "keyboard-b", "B should now be live");

    // Get both hosts and send commands to both (sync)
    let host_a = manager.get_host("keyboard", "keyboard-a").await.unwrap();
    let host_b = manager.get_host("keyboard", "keyboard-b").await.unwrap();

    let daw_a = Daw::new(host_a.handle().clone());
    let daw_b = Daw::new(host_b.handle().clone());

    daw_a
        .current_project()
        .await
        .unwrap()
        .transport()
        .play()
        .await
        .unwrap();
    daw_b
        .current_project()
        .await
        .unwrap()
        .transport()
        .play()
        .await
        .unwrap();
    tokio::time::sleep(Duration::from_millis(50)).await;

    assert!(
        keyboard_a.is_playing().await,
        "A should still receive commands"
    );
    assert!(
        keyboard_b.is_playing().await,
        "B should also receive commands"
    );

    // Try to set an unhealthy host as live - should fail
    host_a.mark_unhealthy().await;
    let result = manager.set_live_host("keyboard", "keyboard-a").await;
    assert!(
        result.is_err(),
        "Should not be able to set unhealthy host as live"
    );

    info!("Manual live switch test passed!");
}

#[tokio::test]
async fn test_multiple_instances_same_purpose() {
    tracing_subscriber::fmt()
        .with_env_filter("info")
        .try_init()
        .ok();

    // Create two distinct guitar instances: "Guitar 1" and "Guitar 2"
    // Each instance can have its own redundant hosts
    let guitar1_primary =
        TestHost::new_with_instance("guitar1-primary", purposes::GUITAR, Some("Guitar 1"));
    let guitar1_backup =
        TestHost::new_with_instance("guitar1-backup", purposes::GUITAR, Some("Guitar 1"));
    let guitar2_primary =
        TestHost::new_with_instance("guitar2-primary", purposes::GUITAR, Some("Guitar 2"));
    let guitar2_backup =
        TestHost::new_with_instance("guitar2-backup", purposes::GUITAR, Some("Guitar 2"));

    guitar1_primary.start().await.unwrap();
    guitar1_backup.start().await.unwrap();
    guitar2_primary.start().await.unwrap();
    guitar2_backup.start().await.unwrap();

    let mut manager = HostManager::new();

    // Connect Guitar 1 hosts (with priorities)
    manager
        .connect(guitar1_primary.socket_path(), 0)
        .await
        .unwrap();
    manager
        .connect(guitar1_backup.socket_path(), 1)
        .await
        .unwrap();

    // Connect Guitar 2 hosts (with priorities)
    manager
        .connect(guitar2_primary.socket_path(), 0)
        .await
        .unwrap();
    manager
        .connect(guitar2_backup.socket_path(), 1)
        .await
        .unwrap();

    // Wait for virtual connections
    tokio::time::sleep(Duration::from_millis(150)).await;

    // Verify we have two separate instance keys
    let keys = manager.instance_keys().await;
    info!("Instance keys: {:?}", keys);
    assert!(
        keys.contains(&"guitar:Guitar 1".to_string()),
        "Should have Guitar 1"
    );
    assert!(
        keys.contains(&"guitar:Guitar 2".to_string()),
        "Should have Guitar 2"
    );

    // Each instance should have 2 hosts
    assert_eq!(
        manager.host_count("guitar:Guitar 1").await,
        2,
        "Guitar 1 should have 2 hosts"
    );
    assert_eq!(
        manager.host_count("guitar:Guitar 2").await,
        2,
        "Guitar 2 should have 2 hosts"
    );

    // Get individual host connections for each instance
    let g1_primary = manager
        .get_host("guitar:Guitar 1", "guitar1-primary")
        .await
        .unwrap();
    let g1_backup = manager
        .get_host("guitar:Guitar 1", "guitar1-backup")
        .await
        .unwrap();
    let g2_primary = manager
        .get_host("guitar:Guitar 2", "guitar2-primary")
        .await
        .unwrap();
    let g2_backup = manager
        .get_host("guitar:Guitar 2", "guitar2-backup")
        .await
        .unwrap();

    // Each instance has its own live host
    let live1 = manager.get_live_host("guitar:Guitar 1").await.unwrap();
    let live2 = manager.get_live_host("guitar:Guitar 2").await.unwrap();
    assert_eq!(
        live1.identity().name,
        "guitar1-primary",
        "Guitar 1 primary should be live"
    );
    assert_eq!(
        live2.identity().name,
        "guitar2-primary",
        "Guitar 2 primary should be live"
    );

    // Create Daw instances for each host
    let daw_g1_primary = Daw::new(g1_primary.handle().clone());
    let daw_g1_backup = Daw::new(g1_backup.handle().clone());
    let daw_g2_primary = Daw::new(g2_primary.handle().clone());
    let daw_g2_backup = Daw::new(g2_backup.handle().clone());

    // Play on Guitar 1 only - should NOT affect Guitar 2
    daw_g1_primary
        .current_project()
        .await
        .unwrap()
        .transport()
        .play()
        .await
        .unwrap();
    daw_g1_backup
        .current_project()
        .await
        .unwrap()
        .transport()
        .play()
        .await
        .unwrap();
    tokio::time::sleep(Duration::from_millis(50)).await;

    assert!(
        guitar1_primary.is_playing().await,
        "Guitar 1 primary should be playing"
    );
    assert!(
        guitar1_backup.is_playing().await,
        "Guitar 1 backup should be playing (sync)"
    );
    assert!(
        !guitar2_primary.is_playing().await,
        "Guitar 2 primary should NOT be playing"
    );
    assert!(
        !guitar2_backup.is_playing().await,
        "Guitar 2 backup should NOT be playing"
    );

    // Play on Guitar 2
    daw_g2_primary
        .current_project()
        .await
        .unwrap()
        .transport()
        .play()
        .await
        .unwrap();
    daw_g2_backup
        .current_project()
        .await
        .unwrap()
        .transport()
        .play()
        .await
        .unwrap();
    tokio::time::sleep(Duration::from_millis(50)).await;

    assert!(
        guitar2_primary.is_playing().await,
        "Guitar 2 primary should now be playing"
    );
    assert!(
        guitar2_backup.is_playing().await,
        "Guitar 2 backup should now be playing (sync)"
    );

    // Stop Guitar 1, Guitar 2 keeps playing
    daw_g1_primary
        .current_project()
        .await
        .unwrap()
        .transport()
        .stop()
        .await
        .unwrap();
    daw_g1_backup
        .current_project()
        .await
        .unwrap()
        .transport()
        .stop()
        .await
        .unwrap();
    tokio::time::sleep(Duration::from_millis(50)).await;

    assert!(
        !guitar1_primary.is_playing().await,
        "Guitar 1 primary should be stopped"
    );
    assert!(
        !guitar1_backup.is_playing().await,
        "Guitar 1 backup should be stopped"
    );
    assert!(
        guitar2_primary.is_playing().await,
        "Guitar 2 primary should still be playing"
    );
    assert!(
        guitar2_backup.is_playing().await,
        "Guitar 2 backup should still be playing"
    );

    // Failover within an instance doesn't affect other instances
    g2_primary.mark_unhealthy().await;
    manager.check_failover("guitar:Guitar 2").await;

    let live2 = manager.get_live_host("guitar:Guitar 2").await.unwrap();
    assert_eq!(
        live2.identity().name,
        "guitar2-backup",
        "Guitar 2 backup should now be live"
    );

    // Guitar 1's live host is unaffected
    let live1 = manager.get_live_host("guitar:Guitar 1").await.unwrap();
    assert_eq!(
        live1.identity().name,
        "guitar1-primary",
        "Guitar 1 primary should still be live"
    );

    info!("Multiple instances same purpose test passed!");
}
