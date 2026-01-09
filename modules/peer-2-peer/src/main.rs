//! Iroh example using irpc + iroh-gossip for state synchronization
//!
//! This demonstrates:
//! - irpc for local RPC (Reaper extension → Desktop app)
//! - iroh-gossip for broadcasting state changes to network peers
//! - mDNS discovery for finding peers
//!
//! Architecture:
//!   Reaper Extension (local) → irpc → Desktop App → iroh-gossip → Network Peers

use anyhow::Result;
use iroh::{
    Endpoint, EndpointId,
    discovery::mdns::{DiscoveryEvent, MdnsDiscovery},
    endpoint_info::UserData,
    protocol::Router,
};
use iroh_gossip::{api::Event, net::Gossip, proto::TopicId};
use irpc::{Client, WithChannels, channel::oneshot, rpc_requests};
use irpc_iroh::IrohProtocol;
use n0_future::StreamExt;
use serde::{Deserialize, Serialize};
use std::sync::{Arc, Mutex};

const IRPC_ALPN: &[u8] = b"iroh-example-irpc/1";
const GOSSIP_ALPN: &[u8] = b"iroh-gossip/1";

// State that gets synchronized
#[derive(Debug, Clone, Serialize, Deserialize)]
struct AppState {
    transport_playing: bool,
    transport_position: f64,
    current_setlist_id: Option<u64>,
}

impl Default for AppState {
    fn default() -> Self {
        Self {
            transport_playing: false,
            transport_position: 0.0,
            current_setlist_id: None,
        }
    }
}

// irpc protocol for commands (Reaper extension → Desktop app)
#[derive(Debug, Serialize, Deserialize)]
struct SetTransport {
    playing: bool,
    position: f64,
}

#[derive(Debug, Serialize, Deserialize)]
struct SetSetlist {
    setlist_id: u64,
}

#[derive(Debug, Serialize, Deserialize)]
struct GetState;

#[rpc_requests(message = CommandMessage)]
#[derive(Debug, Serialize, Deserialize)]
enum CommandProtocol {
    /// Set transport state
    #[rpc(tx=oneshot::Sender<()>)]
    SetTransport(SetTransport),

    /// Set current setlist
    #[rpc(tx=oneshot::Sender<()>)]
    SetSetlist(SetSetlist),

    /// Get current state
    #[rpc(tx=oneshot::Sender<AppState>)]
    GetState(GetState),
}

// Gossip messages for broadcasting state changes
#[derive(Debug, Clone, Serialize, Deserialize)]
enum StateUpdate {
    TransportChanged { playing: bool, position: f64 },
    SetlistChanged { setlist_id: u64 },
}

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::fmt::init();
    println!("\nIroh State Sync Example (irpc + gossip)!\n");

    // Create an endpoint
    let endpoint = Endpoint::bind().await?;

    let endpoint_id = endpoint.id();
    println!("Created endpoint {}", endpoint_id.fmt_short());

    // Set user data for discovery filtering
    let user_data = UserData::try_from(String::from("iroh-example"))?;
    endpoint.set_user_data_for_discovery(Some(user_data.clone()));

    // Add mDNS discovery
    let mdns = MdnsDiscovery::builder().build(endpoint_id)?;
    endpoint.discovery().add(mdns.clone());

    // Wait for endpoint to be online
    endpoint.online().await;
    println!("Endpoint is online");
    println!("Endpoint address: {:?}", endpoint.addr());
    println!();

    // Shared state (simulating desktop app state)
    let state = Arc::new(Mutex::new(AppState::default()));

    // Create gossip protocol for broadcasting state changes
    let gossip = Gossip::builder().spawn(endpoint.clone());

    // Create a topic for state synchronization
    let topic_id = TopicId::from_bytes([42u8; 32]); // Fixed topic for this example
    println!("State sync topic: {}", hex::encode(topic_id.as_bytes()));

    // Subscribe to gossip topic (will receive updates from other peers)
    let (gossip_sender, mut gossip_receiver) = gossip
        .subscribe(topic_id, vec![]) // Start with no bootstrap peers
        .await?
        .split();

    // Spawn task to handle gossip messages (state updates from peers)
    let state_clone = state.clone();
    tokio::spawn(async move {
        while let Some(event) = gossip_receiver.next().await {
            match event {
                Ok(Event::Received(msg)) => {
                    if let Ok(update) = postcard::from_bytes::<StateUpdate>(&msg.content) {
                        let mut state = state_clone.lock().unwrap();
                        match update {
                            StateUpdate::TransportChanged { playing, position } => {
                                println!(
                                    "📡 Received transport update: playing={}, position={}",
                                    playing, position
                                );
                                state.transport_playing = playing;
                                state.transport_position = position;
                            }
                            StateUpdate::SetlistChanged { setlist_id } => {
                                println!("📡 Received setlist update: id={}", setlist_id);
                                state.current_setlist_id = Some(setlist_id);
                            }
                        }
                    }
                }
                Ok(Event::NeighborUp(peer_id)) => {
                    println!("✅ Neighbor {} joined gossip swarm", peer_id.fmt_short());
                }
                Ok(Event::NeighborDown(peer_id)) => {
                    println!("❌ Neighbor {} left gossip swarm", peer_id.fmt_short());
                }
                Ok(Event::Lagged) => {
                    println!("⚠️ Lagged behind gossip messages");
                }
                Err(e) => {
                    eprintln!("Gossip error: {}", e);
                }
            }
        }
    });

    // Create irpc handler for commands (simulating Reaper extension → Desktop app)
    let (tx, rx) = tokio::sync::mpsc::channel(16);
    let command_client = Client::<CommandProtocol>::local(tx);

    // Spawn actor to handle irpc commands
    let state_for_commands = state.clone();
    let gossip_sender_for_commands = gossip_sender.clone();
    tokio::spawn(command_actor(
        rx,
        state_for_commands,
        gossip_sender_for_commands,
    ));

    // Create irpc protocol handler for Router
    let irpc_protocol = IrohProtocol::with_sender(command_client.as_local().unwrap());

    // Build router with both irpc and gossip protocols
    let router = Router::builder(endpoint.clone())
        .accept(IRPC_ALPN, irpc_protocol)
        .accept(GOSSIP_ALPN, gossip.clone())
        .spawn();

    // Spawn discovery task
    let ud = user_data.clone();
    let endpoint_clone = endpoint.clone();

    tokio::spawn(async move {
        let mut discovery_stream = mdns.subscribe().await;
        let mut discovered_endpoints: Vec<EndpointId> = vec![];

        while let Some(event) = discovery_stream.next().await {
            match event {
                DiscoveryEvent::Discovered { endpoint_info, .. } => {
                    match endpoint_info.data.user_data() {
                        Some(user_data) if user_data == &ud => {
                            let peer_id = endpoint_info.endpoint_id;

                            if peer_id == endpoint_clone.id() {
                                continue;
                            }

                            if discovered_endpoints.contains(&peer_id) {
                                continue;
                            }

                            discovered_endpoints.push(peer_id);
                            println!("🔍 Found peer {}!", peer_id.fmt_short());

                            // Try to join their gossip swarm
                            // In a real app, you'd exchange topic info via irpc first
                            // For this example, we use a fixed topic
                        }
                        _ => {}
                    }
                }
                DiscoveryEvent::Expired { .. } => {}
            }
        }
    });

    // Simulate Reaper extension sending commands via irpc
    // In real app, this would be a separate process connecting via irpc
    println!("\nSimulating Reaper extension commands...\n");

    // Create a local irpc client (simulating Reaper extension)
    let reaper_client = command_client.clone();

    tokio::spawn(async move {
        tokio::time::sleep(tokio::time::Duration::from_secs(2)).await;

        // Send transport command
        println!("🎹 Reaper: Setting transport to playing=true, position=10.5");
        let cmd = SetTransport {
            playing: true,
            position: 10.5,
        };
        if let Err(e) = reaper_client.rpc(cmd).await {
            eprintln!("Failed to send command: {}", e);
        }

        tokio::time::sleep(tokio::time::Duration::from_secs(3)).await;

        // Send setlist command
        println!("🎹 Reaper: Setting setlist to id=42");
        let cmd = SetSetlist { setlist_id: 42 };
        if let Err(e) = reaper_client.rpc(cmd).await {
            eprintln!("Failed to send command: {}", e);
        }
    });

    println!("Router started. Listening for connections...");
    println!("- irpc: Accepting commands (simulating Reaper extension)");
    println!("- gossip: Broadcasting state changes to network peers");
    println!("- mDNS: Discovering peers on local network");
    println!("Press Ctrl+C to exit.\n");

    tokio::signal::ctrl_c().await?;

    println!("Shutting down.");
    router.shutdown().await?;

    Ok(())
}

/// Actor that handles incoming irpc commands (like Reaper extension → Desktop app)
async fn command_actor(
    mut rx: tokio::sync::mpsc::Receiver<CommandMessage>,
    state: Arc<Mutex<AppState>>,
    gossip_sender: iroh_gossip::api::GossipSender,
) {
    while let Some(msg) = rx.recv().await {
        match msg {
            CommandMessage::SetTransport(msg) => {
                let WithChannels { inner, tx, .. } = msg;
                let SetTransport { playing, position } = inner;

                println!(
                    "📥 Received SetTransport command: playing={}, position={}",
                    playing, position
                );

                // Update local state
                {
                    let mut s = state.lock().unwrap();
                    s.transport_playing = playing;
                    s.transport_position = position;
                }

                // Broadcast state change via gossip
                let update = StateUpdate::TransportChanged { playing, position };
                let bytes = postcard::to_stdvec(&update).unwrap();
                if let Err(e) = gossip_sender.broadcast(bytes.into()).await {
                    eprintln!("Failed to broadcast: {}", e);
                } else {
                    println!("📢 Broadcasted transport update to network");
                }

                tx.send(()).await.ok();
            }
            CommandMessage::SetSetlist(msg) => {
                let WithChannels { inner, tx, .. } = msg;
                let SetSetlist { setlist_id } = inner;

                println!("📥 Received SetSetlist command: id={}", setlist_id);

                // Update local state
                {
                    let mut s = state.lock().unwrap();
                    s.current_setlist_id = Some(setlist_id);
                }

                // Broadcast state change via gossip
                let update = StateUpdate::SetlistChanged { setlist_id };
                let bytes = postcard::to_stdvec(&update).unwrap();
                if let Err(e) = gossip_sender.broadcast(bytes.into()).await {
                    eprintln!("Failed to broadcast: {}", e);
                } else {
                    println!("📢 Broadcasted setlist update to network");
                }

                tx.send(()).await.ok();
            }
            CommandMessage::GetState(msg) => {
                let WithChannels { tx, .. } = msg;

                let state_copy = state.lock().unwrap().clone();
                println!("📥 Received GetState command");

                tx.send(state_copy).await.ok();
            }
        }
    }
}
