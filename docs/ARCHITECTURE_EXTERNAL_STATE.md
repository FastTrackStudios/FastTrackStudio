# Architecture Analysis: Stateless Client Architecture with Message Router

## Core Principle: Stateless Clients

**Key Insight**: Clients operate in two modes:
1. **Standalone Mode**: Client manages its own state (current CLI behavior)
2. **Slave Mode**: Client is completely stateless, only renders received state updates

When in slave mode, clients:
- Hold **NO state** - they are pure view/control layers
- Receive state updates via message bus
- Send commands via message bus
- Render whatever state they receive
- If no updates arrive, nothing changes (no local state to advance)

## Current State Analysis

### Current Architecture
- **CLI App**: Manages state locally, advances playhead via `on_tick()` polling
- **Desktop App**: Uses Tauri IPC with polling (250-1000ms intervals)
- **Transport Layer**: `TransportActions` trait abstracts operations
- **HTTP Infrastructure**: Already exists (`transport/src/infra/http.rs`) but not actively used
- **OSC Support**: `rosc` crate already in dependencies (`project/Cargo.toml`)
- **State Management**: Each app maintains its own copy of state

### Problems with Current Approach
1. **State Duplication**: Each client maintains its own state copy
2. **Polling Overhead**: Constant polling wastes resources
3. **No Single Source of Truth**: State can diverge between clients
4. **No Real-time Updates**: Changes aren't broadcast immediately
5. **Tight Coupling**: CLI directly manipulates transport state

## Proposed Architecture

### High-Level Design: Message Router Pattern

```
┌─────────────────────────────────────────────────────────────┐
│              Message Router/Bus (Multi-Protocol)              │
│  ┌──────────────────────────────────────────────────────┐   │
│  │         Unified Message Router                        │   │
│  │  - tokio::sync::broadcast for pub/sub               │   │
│  │  - Protocol adapters: UDP, OSC, HTTP, WebSocket      │   │
│  │  - Message routing and transformation                │   │
│  └──────────────────────────────────────────────────────┘   │
│                          │                                   │
│        ┌─────────────────┼─────────────────┐               │
│        │                 │                 │               │
│  ┌─────▼─────┐   ┌──────▼──────┐   ┌──────▼──────┐        │
│  │ UDP       │   │ OSC         │   │ WebSocket   │        │
│  │ Adapter   │   │ Adapter      │   │ Adapter     │        │
│  └───────────┘   └──────────────┘   └─────────────┘        │
│        │                 │                 │               │
│  ┌─────┴─────────────────┼─────────────────┘               │
│  │                       │                                 │
│  ┌───────────────────────▼───────────────────────┐          │
│  │         State Manager (Single Source)         │          │
│  │  - Manages Transport state                   │          │
│  │  - Manages Setlist state                     │          │
│  │  - Manages Project state                     │          │
│  │  - Broadcasts state changes                   │          │
│  └───────────────────────┬───────────────────────┘          │
│                          │                                   │
│  ┌───────────────────────┼───────────────────────┐          │
│  │                       │                       │          │
│  ┌───────▼────────┐  ┌───▼──────┐  ┌────────▼────┐         │
│  │ Transport      │  │ Setlist   │  │ Projects    │         │
│  │ Backends       │  │ Manager   │  │ Manager     │         │
│  └───────┬────────┘  └──────────┘  └──────────────┘         │
│          │                                                   │
│  ┌───────┴──────────────────────────────────────┐          │
│  │  Backend Adapters                              │          │
│  │  - LocalTransportBackend                       │          │
│  │  - ReaperTransportBackend                      │          │
│  │  - NetworkTransportBackend                     │          │
│  └───────────────────────────────────────────────┘          │
└──────────────────────────────────────────────────────────────┘
           │                                   │
    ┌──────┴──────┐                    ┌──────┴──────┐
    │             │                    │             │
┌───▼────┐  ┌────▼─────┐        ┌─────▼────┐  ┌────▼─────┐
│  CLI   │  │ Desktop  │        │  WebApp  │  │ Reaper   │
│(Slave) │  │(Slave)   │        │(Slave)   │  │ Extension│
│        │  │          │        │          │  │          │
│ NO     │  │ NO       │        │ NO       │  │          │
│ STATE  │  │ STATE    │        │ STATE    │  │          │
└────────┘  └──────────┘        └──────────┘  └──────────┘
```

### Client Modes

```
┌─────────────────────────────────────────────────────────┐
│                    Client Application                   │
│                                                         │
│  ┌──────────────────────────────────────────────────┐  │
│  │  Mode: Standalone | Slave                        │  │
│  └──────────────────────────────────────────────────┘  │
│                          │                             │
│        ┌──────────────────┼──────────────────┐        │
│        │                  │                  │        │
│  ┌─────▼─────┐    ┌───────▼────────┐         │        │
│  │Standalone │    │  Slave Mode     │         │        │
│  │           │    │                 │         │        │
│  │ - Has     │    │ - NO STATE      │         │        │
│  │   state   │    │ - Pure view     │         │        │
│  │ - Local   │    │ - Receives      │         │        │
│  │   control │    │   updates only  │         │        │
│  │ - Can     │    │ - Sends commands│         │        │
│  │   work    │    │   only          │         │        │
│  │   offline │    │ - Requires      │         │        │
│  └───────────┘    │   connection    │         │        │
│                   └──────────────────┘         │        │
└─────────────────────────────────────────────────────────┘
```

## Implementation Strategy

### Phase 1: Message Router Foundation

#### 1.1 Create Message Router Module
**Location**: `modules/daw/message-router/` or `libs/message-router/`

```
message-router/
├── src/
│   ├── lib.rs
│   ├── router.rs          # Core message router
│   ├── message.rs         # Message types
│   ├── transports/
│   │   ├── mod.rs
│   │   ├── udp.rs         # UDP transport
│   │   ├── osc.rs         # OSC transport (using rosc)
│   │   ├── websocket.rs   # WebSocket transport
│   │   ├── http.rs        # HTTP/REST transport
│   │   └── shared_mem.rs  # Shared memory (local)
│   └── client.rs          # Client library for apps
```

#### 1.2 Message Router Design

```rust
// libs/message-router/src/router.rs

use tokio::sync::broadcast;
use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Message {
    // State updates (server → clients)
    StateUpdate {
        transport: Transport,
        setlist: Setlist,
        active_project_id: Option<uuid::Uuid>,
    },
    TransportChanged {
        project_id: uuid::Uuid,
        transport: Transport,
    },
    SetlistChanged {
        setlist: Setlist,
    },
    
    // Commands (clients → server)
    Command {
        id: uuid::Uuid,
        command: TransportCommand,
    },
}

pub struct MessageRouter {
    // Broadcast channel for pub/sub
    tx: broadcast::Sender<Message>,
    
    // Protocol adapters
    udp_adapter: Option<UdpAdapter>,
    osc_adapter: Option<OscAdapter>,
    websocket_adapter: Option<WebSocketAdapter>,
    http_adapter: Option<HttpAdapter>,
}

impl MessageRouter {
    pub fn new() -> Self {
        let (tx, _) = broadcast::channel(1000);
        Self {
            tx,
            udp_adapter: None,
            osc_adapter: None,
            websocket_adapter: None,
            http_adapter: None,
        }
    }
    
    pub fn subscribe(&self) -> broadcast::Receiver<Message> {
        self.tx.subscribe()
    }
    
    pub fn broadcast(&self, message: Message) -> Result<(), Error> {
        self.tx.send(message)?;
        Ok(())
    }
    
    pub async fn start_udp(&mut self, port: u16) -> Result<(), Error> {
        // Start UDP listener
        // Convert UDP packets to Messages
        // Broadcast via tx
    }
    
    pub async fn start_osc(&mut self, port: u16) -> Result<(), Error> {
        // Start OSC listener (using rosc crate)
        // Convert OSC messages to Messages
        // Broadcast via tx
    }
    
    pub async fn start_websocket(&mut self, port: u16) -> Result<(), Error> {
        // Start WebSocket server
        // Convert WebSocket messages to Messages
        // Broadcast via tx
    }
}
```

### Phase 2: State Server Foundation

#### 2.1 Create State Server Module
**Location**: `modules/daw/state-server/`

```
state-server/
├── src/
│   ├── lib.rs
│   ├── server.rs          # Main server loop
│   ├── state.rs           # Centralized state management
│   ├── backends/
│   │   ├── mod.rs
│   │   ├── local.rs       # Local transport backend
│   │   ├── reaper.rs      # Reaper API backend
│   │   └── network.rs    # Network transport backend
│   └── infra/
│       ├── mod.rs
│       └── http.rs        # HTTP REST API (optional)
```

#### 2.2 State Manager Design

```rust
// modules/daw/state-server/src/state.rs

use std::sync::Arc;
use tokio::sync::RwLock;
use transport::{Transport, TransportActions};
use setlist::Setlist;
use project::TransportProject;
use message_router::{MessageRouter, Message};

pub struct StateManager {
    // Transport state (one per active project)
    transport: Arc<RwLock<Transport>>,
    
    // Setlist state
    setlist: Arc<RwLock<Setlist>>,
    
    // Projects registry
    projects: Arc<RwLock<HashMap<uuid::Uuid, TransportProject>>>,
    
    // Active project ID
    active_project_id: Arc<RwLock<Option<uuid::Uuid>>>,
    
    // Message router for broadcasting
    router: Arc<MessageRouter>,
}

impl StateManager {
    pub async fn apply_transport_action(
        &self,
        action: TransportCommand,
    ) -> Result<String, StateError> {
        // Apply action to transport
        let result = match action {
            TransportCommand::Play => {
                let mut transport = self.transport.write().await;
                transport.play()
            }
            // ... other actions
        };
        
        // Broadcast state update via message router
        if result.is_ok() {
            let transport = self.transport.read().await.clone();
            let project_id = *self.active_project_id.read().await
                .ok_or(StateError::NoActiveProject)?;
            
            // Broadcast to all connected clients
            self.router.broadcast(Message::TransportChanged {
                project_id,
                transport,
            })?;
        }
        
        result
    }
    
    pub async fn handle_message(&self, message: Message) -> Result<(), StateError> {
        match message {
            Message::Command { command, .. } => {
                self.apply_transport_action(command).await?;
            }
            _ => {
                // Ignore state updates (we're the source)
            }
        }
        Ok(())
    }
}
```

### Phase 3: Stateless Client Implementation

#### 3.1 Stateless Client Design

```rust
// libs/message-router/src/client.rs

use tokio::sync::broadcast;
use serde::{Serialize, Deserialize};

pub enum ClientMode {
    Standalone,  // Client manages its own state
    Slave,       // Client is stateless, receives all state
}

pub struct StateClient {
    mode: ClientMode,
    router: MessageRouter,
    state_rx: broadcast::Receiver<Message>,
    
    // Only used in Standalone mode
    local_state: Option<AppState>,
}

impl StateClient {
    pub fn new_standalone() -> Self {
        Self {
            mode: ClientMode::Standalone,
            router: MessageRouter::new(),
            state_rx: router.subscribe(),
            local_state: Some(AppState::default()),
        }
    }
    
    pub fn new_slave(router_url: &str) -> Self {
        // Connect to remote message router
        let router = MessageRouter::connect(router_url).await?;
        let state_rx = router.subscribe();
        
        Self {
            mode: ClientMode::Slave,
            router,
            state_rx,
            local_state: None,  // NO STATE in slave mode
        }
    }
    
    pub async fn send_command(&self, command: TransportCommand) -> Result<(), Error> {
        // In slave mode, send to router
        // In standalone mode, apply locally
        match self.mode {
            ClientMode::Slave => {
                self.router.send(Message::Command {
                    id: uuid::Uuid::new_v4(),
                    command,
                }).await
            }
            ClientMode::Standalone => {
                // Apply locally
                if let Some(ref mut state) = self.local_state {
                    state.apply_command(command).await?;
                }
                Ok(())
            }
        }
    }
    
    pub async fn receive_update(&mut self) -> Option<Message> {
        // Receive state update from router
        self.state_rx.recv().await.ok()
    }
    
    pub fn current_state(&self) -> Option<&AppState> {
        // In slave mode, return None (no state)
        // In standalone mode, return local state
        match self.mode {
            ClientMode::Slave => None,
            ClientMode::Standalone => self.local_state.as_ref(),
        }
    }
}
```

#### 3.2 Refactored CLI App (Stateless Mode)

```rust
// apps/cli/src/app.rs

pub struct CliSetlistApp {
    // State client (stateless in slave mode)
    state_client: StateClient,
    
    // UI state only (not transport state)
    selected_song_index: usize,
    selected_section_index: Option<usize>,
    expanded_songs: HashSet<usize>,
    
    // NO transport state stored here!
    // NO playhead advancement!
    // State comes from state_client.receive_update()
}

impl CliSetlistApp {
    pub fn new_slave(router_url: &str) -> Result<Self, Error> {
        let state_client = StateClient::new_slave(router_url).await?;
        
        Ok(Self {
            state_client,
            selected_song_index: 0,
            selected_section_index: None,
            expanded_songs: HashSet::new(),
        })
    }
    
    pub fn new_standalone() -> Self {
        // Current implementation - manages own state
        let state_client = StateClient::new_standalone();
        // ... rest of initialization
    }
    
    pub fn on_tick(&mut self) {
        // In slave mode: NO playhead advancement
        // Just check for state updates
        while let Some(message) = self.state_client.receive_update().now_or_never() {
            if let Some(message) = message {
                self.handle_state_update(message);
            }
        }
        
        // In standalone mode: advance playhead locally
        if matches!(self.state_client.mode(), ClientMode::Standalone) {
            // Old on_tick logic here
        }
    }
    
    fn handle_state_update(&mut self, message: Message) {
        match message {
            Message::TransportChanged { transport, .. } => {
                // Update UI based on received transport state
                // NO local state to update - we're stateless!
                self.request_redraw();
            }
            Message::SetlistChanged { setlist } => {
                // Update UI based on received setlist
                self.request_redraw();
            }
            _ => {}
        }
    }
    
    pub fn handle_key(&mut self, key: KeyEvent) {
        match key.code {
            KeyCode::Char(' ') => {
                // Send command, don't apply locally
                self.state_client.send_command(TransportCommand::PlayPause).await?;
            }
            // ... other commands
        }
    }
}
```

### Phase 4: Transport Backends

```rust
// modules/daw/state-server/src/backends/mod.rs

pub trait TransportBackend: Send + Sync {
    fn name(&self) -> &'static str;
    
    // Push state changes TO the backend (e.g., Reaper)
    async fn push_state(&mut self, transport: &Transport) -> Result<(), BackendError>;
    
    // Pull state changes FROM the backend
    async fn pull_state(&mut self) -> Result<Transport, BackendError>;
    
    // Check if backend is ready
    async fn is_ready(&self) -> bool;
    
    // Start monitoring backend for changes
    fn start_monitoring(&mut self, callback: Box<dyn Fn(Transport) + Send>) -> Result<(), BackendError>;
}
```

#### 4.2 Reaper Backend Implementation

```rust
// modules/daw/state-server/src/backends/reaper.rs

use message_router::{MessageRouter, Message};

pub struct ReaperTransportBackend {
    reaper_api: Option<ReaperApi>,
    monitoring_task: Option<tokio::task::JoinHandle<()>>,
}

impl TransportBackend for ReaperTransportBackend {
    async fn push_state(&mut self, transport: &Transport) -> Result<(), BackendError> {
        if let Some(api) = &self.reaper_api {
            // Push play state to Reaper
            if transport.is_playing() {
                api.play()?;
            } else if transport.is_paused() {
                api.pause()?;
            } else {
                api.stop()?;
            }
            
            api.set_playhead_position(transport.playhead_position.time.to_seconds())?;
            api.set_tempo(transport.tempo.bpm)?;
            Ok(())
        } else {
            Err(BackendError::NotConnected)
        }
    }
    
    async fn pull_state(&mut self) -> Result<Transport, BackendError> {
        // Pull current state from Reaper
        // ...
    }
    
    fn start_monitoring(
        &mut self,
        router: Arc<MessageRouter>,
    ) -> Result<tokio::task::JoinHandle<()>, BackendError> {
        let api = self.reaper_api.clone();
        let handle = tokio::spawn(async move {
            let mut last_state = None;
            loop {
                // Poll Reaper API
                if let Some(transport) = pull_from_reaper(&api).await {
                    // Only broadcast if state changed
                    if Some(&transport) != last_state.as_ref() {
                        router.broadcast(Message::TransportChanged {
                            project_id: uuid::Uuid::new_v4(), // Or get from context
                            transport: transport.clone(),
                        }).ok();
                        last_state = Some(transport);
                    }
                }
                tokio::time::sleep(Duration::from_millis(50)).await;
            }
        });
        Ok(handle)
    }
}
```

#### 2.3 Local Backend (Current Implementation)

```rust
// modules/daw/state-server/src/backends/local.rs

pub struct LocalTransportBackend {
    transport: Arc<RwLock<Transport>>,
}

impl TransportBackend for LocalTransportBackend {
    // Local backend just wraps the in-memory transport
    // No external system to sync with
}
```

### Phase 5: Protocol Adapters (Message Router Transports)

#### 5.1 UDP Adapter

```rust
// libs/message-router/src/transports/udp.rs

use tokio::net::UdpSocket;
use message_router::{MessageRouter, Message};

pub struct UdpAdapter {
    socket: UdpSocket,
    router: Arc<MessageRouter>,
}

impl UdpAdapter {
    pub async fn new(port: u16, router: Arc<MessageRouter>) -> Result<Self, Error> {
        let socket = UdpSocket::bind(format!("0.0.0.0:{}", port)).await?;
        
        let adapter = Self { socket, router };
        adapter.start_listening().await?;
        Ok(adapter)
    }
    
    async fn start_listening(&self) -> Result<(), Error> {
        let mut buf = [0; 1024];
        loop {
            let (len, addr) = self.socket.recv_from(&mut buf).await?;
            let message: Message = bincode::deserialize(&buf[..len])?;
            
            // Broadcast received message
            self.router.broadcast(message)?;
        }
    }
    
    pub async fn send(&self, message: Message, addr: SocketAddr) -> Result<(), Error> {
        let data = bincode::serialize(&message)?;
        self.socket.send_to(&data, addr).await?;
        Ok(())
    }
}
```

#### 5.2 OSC Adapter (Using `rosc` crate)

```rust
// libs/message-router/src/transports/osc.rs

use rosc::{OscPacket, OscMessage, OscType};
use tokio::net::UdpSocket;
use message_router::{MessageRouter, Message};

pub struct OscAdapter {
    socket: UdpSocket,
    router: Arc<MessageRouter>,
}

impl OscAdapter {
    pub async fn new(port: u16, router: Arc<MessageRouter>) -> Result<Self, Error> {
        let socket = UdpSocket::bind(format!("0.0.0.0:{}", port)).await?;
        
        let adapter = Self { socket, router };
        adapter.start_listening().await?;
        Ok(adapter)
    }
    
    async fn start_listening(&self) -> Result<(), Error> {
        let mut buf = [0; 1024];
        loop {
            let (len, _) = self.socket.recv_from(&mut buf).await?;
            let packet = rosc::decoder::decode(&buf[..len])?;
            
            // Convert OSC to Message
            let message = self.osc_to_message(packet)?;
            self.router.broadcast(message)?;
        }
    }
    
    fn osc_to_message(&self, packet: OscPacket) -> Result<Message, Error> {
        match packet {
            OscPacket::Message(msg) => {
                match msg.addr.as_str() {
                    "/transport/play" => Ok(Message::Command {
                        id: uuid::Uuid::new_v4(),
                        command: TransportCommand::Play,
                    }),
                    "/transport/pause" => Ok(Message::Command {
                        id: uuid::Uuid::new_v4(),
                        command: TransportCommand::Pause,
                    }),
                    "/transport/position" => {
                        // Extract position from OSC args
                        // ...
                    }
                    _ => Err(Error::UnknownOscAddress(msg.addr)),
                }
            }
            OscPacket::Bundle(_) => Err(Error::BundleNotSupported),
        }
    }
    
    pub async fn send_osc(&self, message: Message, addr: SocketAddr) -> Result<(), Error> {
        let osc_packet = self.message_to_osc(message)?;
        let buf = rosc::encoder::encode(&osc_packet)?;
        self.socket.send_to(&buf, addr).await?;
        Ok(())
    }
}
```

#### 5.3 WebSocket Adapter

```rust
// libs/message-router/src/transports/websocket.rs

use axum::extract::ws::WebSocket;
use message_router::{MessageRouter, Message};

pub struct WebSocketAdapter {
    router: Arc<MessageRouter>,
}

impl WebSocketAdapter {
    pub async fn handle_connection(
        ws: WebSocket,
        router: Arc<MessageRouter>,
    ) -> Result<(), Error> {
        let (mut sender, mut receiver) = ws.split();
        let mut rx = router.subscribe();
        
        // Task to send messages to client
        let send_task = tokio::spawn(async move {
            while let Ok(message) = rx.recv().await {
                let json = serde_json::to_string(&message)?;
                sender.send(axum::extract::ws::Message::Text(json)).await?;
            }
        });
        
        // Task to receive messages from client
        let recv_task = tokio::spawn(async move {
            while let Some(Ok(msg)) = receiver.next().await {
                if let axum::extract::ws::Message::Text(text) = msg {
                    if let Ok(message) = serde_json::from_str::<Message>(&text) {
                        router.broadcast(message)?;
                    }
                }
            }
        });
        
        tokio::select! {
            _ = send_task => {}
            _ = recv_task => {}
        }
        
        Ok(())
    }
}
```

### Phase 6: Complete Example - Stateless CLI

#### 6.1 Stateless CLI Implementation

```rust
// apps/cli/src/app.rs

pub struct CliSetlistApp {
    // State client - NO local state in slave mode
    state_client: StateClient,
    
    // UI-only state (not transport state)
    selected_song_index: usize,
    selected_section_index: Option<usize>,
    expanded_songs: HashSet<usize>,
    
    // Current received state (for rendering only)
    current_transport: Option<Transport>,
    current_setlist: Option<Setlist>,
}

impl CliSetlistApp {
    pub async fn new_slave(router_url: &str) -> Result<Self, Error> {
        let mut state_client = StateClient::new_slave(router_url).await?;
        
        // Request initial state
        state_client.request_state().await?;
        
        Ok(Self {
            state_client,
            selected_song_index: 0,
            selected_section_index: None,
            expanded_songs: HashSet::new(),
            current_transport: None,  // Will be populated by updates
            current_setlist: None,    // Will be populated by updates
        })
    }
    
    pub fn on_tick(&mut self) {
        // In slave mode: NO playhead advancement!
        // Just process any received state updates
        while let Ok(message) = self.state_client.receive_update().try_recv() {
            match message {
                Message::TransportChanged { transport, .. } => {
                    // Update UI state only - NO local transport state
                    self.current_transport = Some(transport);
                    self.request_redraw();
                }
                Message::SetlistChanged { setlist } => {
                    self.current_setlist = Some(setlist);
                    self.request_redraw();
                }
                Message::StateUpdate { transport, setlist, .. } => {
                    // Full state update
                    self.current_transport = Some(transport);
                    self.current_setlist = Some(setlist);
                    self.request_redraw();
                }
                _ => {}
            }
        }
    }
    
    pub fn handle_key(&mut self, key: KeyEvent) {
        // Send commands - don't apply locally
        match key.code {
            KeyCode::Char(' ') => {
                self.state_client.send_command(TransportCommand::PlayPause).await?;
            }
            KeyCode::Char('b') => {
                self.state_client.send_command(TransportCommand::Back).await?;
            }
            // ... other commands
        }
    }
    
    pub fn draw(&mut self, frame: &mut Frame) {
        // Render based on current_transport and current_setlist
        // If None, show "Connecting..." or "No state"
        if let (Some(transport), Some(setlist)) = (&self.current_transport, &self.current_setlist) {
            crate::ui::draw_ui(frame, self, transport, setlist);
        } else {
            // Show connection status
        }
    }
}
```

#### 6.2 Web Client (TypeScript)

```typescript
// apps/web/src/state-client.ts

export class StateClient {
  private ws: WebSocket | null = null;
  private routerUrl: string;
  
  // NO STATE STORED - just for rendering
  private currentTransport: Transport | null = null;
  private currentSetlist: Setlist | null = null;
  private listeners: Set<(transport: Transport, setlist: Setlist) => void> = new Set();

  constructor(routerUrl: string) {
    this.routerUrl = routerUrl;
  }

  async connect() {
    this.ws = new WebSocket(this.routerUrl);
    
    this.ws.onmessage = (event) => {
      const message: Message = JSON.parse(event.data);
      this.handleMessage(message);
    };
    
    // Request initial state
    this.requestState();
  }

  private handleMessage(message: Message) {
    switch (message.type) {
      case 'TransportChanged':
        this.currentTransport = message.transport;
        this.notifyListeners();
        break;
      case 'SetlistChanged':
        this.currentSetlist = message.setlist;
        this.notifyListeners();
        break;
      case 'StateUpdate':
        this.currentTransport = message.transport;
        this.currentSetlist = message.setlist;
        this.notifyListeners();
        break;
    }
  }

  async sendCommand(command: TransportCommand) {
    if (this.ws?.readyState === WebSocket.OPEN) {
      this.ws.send(JSON.stringify({
        type: 'Command',
        command,
        id: crypto.randomUUID(),
      }));
    }
  }

  subscribe(callback: (transport: Transport, setlist: Setlist) => void) {
    this.listeners.add(callback);
    return () => this.listeners.delete(callback);
  }

  private notifyListeners() {
    if (this.currentTransport && this.currentSetlist) {
      this.listeners.forEach(cb => cb(this.currentTransport!, this.currentSetlist!));
    }
  }
}
```

## Communication Patterns via Message Router

The message router supports multiple protocols simultaneously. Clients can connect via any supported protocol:

### Protocol 1: WebSocket (Recommended)

**Pros**:
- Real-time bidirectional communication
- Low latency
- Efficient for frequent updates
- Works over network
- JSON serialization (human-readable)

**Cons**:
- Requires persistent connection
- More complex than HTTP

**Use Case**: CLI, WebApp, Desktop App

**Implementation**: `WebSocketAdapter` in message-router

### Protocol 2: OSC (Open Sound Control)

**Pros**:
- Standard in music/audio applications
- Low overhead
- Works with existing OSC tools
- UDP-based (fast, connectionless)

**Cons**:
- Less human-readable
- Requires OSC message mapping

**Use Case**: Reaper extension, MIDI controllers, audio hardware

**Implementation**: `OscAdapter` using `rosc` crate (already in dependencies)

### Protocol 3: UDP (Raw)

**Pros**:
- Fastest protocol
- Low overhead
- Connectionless
- Good for LAN

**Cons**:
- No delivery guarantee
- Requires custom serialization

**Use Case**: High-performance local networks

**Implementation**: `UdpAdapter` with bincode serialization

### Protocol 4: HTTP REST (Fallback)

**Pros**:
- Simple, well-understood
- Works everywhere
- Easy to debug
- Firewall-friendly

**Cons**:
- Requires polling for state (not real-time)
- Higher latency

**Use Case**: Fallback, simple integrations, web browsers without WebSocket

**Implementation**: `HttpAdapter` with REST endpoints

### Protocol 5: Shared Memory (Local Only)

**Pros**:
- Extremely fast (zero-copy)
- No serialization overhead
- Perfect for same-machine communication

**Cons**:
- Only works locally
- Platform-specific
- Requires careful synchronization

**Use Case**: CLI ↔ Desktop App on same machine (optional optimization)

**Implementation**: `SharedMemoryAdapter` using platform-specific APIs

## Recommended Implementation Order

### Step 1: Create Message Router Library
1. Create `libs/message-router/` crate
2. Implement core `MessageRouter` with `tokio::sync::broadcast`
3. Define `Message` enum (commands + state updates)
4. Test with simple in-memory pub/sub

### Step 2: Add Protocol Adapters
1. Implement `WebSocketAdapter` (highest priority)
2. Implement `OscAdapter` using `rosc` crate
3. Implement `UdpAdapter` for raw UDP
4. Implement `HttpAdapter` for REST fallback
5. All adapters convert to/from unified `Message` type

### Step 3: Create State Server
1. Create `modules/daw/state-server/` crate
2. Implement `StateManager` that uses `MessageRouter`
3. Connect state changes → router broadcasts
4. Connect router commands → state manager actions

### Step 4: Implement Stateless Client Library
1. Create `libs/state-client/` crate
2. Implement `StateClient` with `ClientMode` (Standalone/Slave)
3. In slave mode: NO state, only receives updates
4. In standalone mode: maintains local state

### Step 5: Refactor CLI to Support Both Modes
1. Add `--slave` flag to CLI
2. In slave mode: use `StateClient::new_slave()`
3. Remove `on_tick()` playhead advancement in slave mode
4. Render based on received state only
5. Keep standalone mode working (backward compatible)

### Step 6: Implement Reaper Backend
1. Create `ReaperTransportBackend`
2. Implement Reaper API integration
3. Monitor Reaper state changes → broadcast via router
4. Receive commands from router → push to Reaper

### Step 7: Create Web Client
1. Implement TypeScript `StateClient`
2. Create React hooks for stateless state management
3. Build web UI similar to CLI
4. Connect via WebSocket to message router

## Code Structure Recommendations

### New Modules to Create

```
modules/daw/
├── state-server/          # NEW: Centralized state server
│   ├── src/
│   │   ├── lib.rs
│   │   ├── server.rs
│   │   ├── state.rs
│   │   ├── events.rs
│   │   ├── backends/
│   │   ├── adapters/
│   │   └── infra/
│   └── Cargo.toml
│
└── transport/
    └── src/
        └── infra/
            └── websocket.rs  # Move WebSocket here or to state-server
```

### Client Libraries

```
libs/
├── state-client/          # NEW: Shared client library
│   ├── src/
│   │   ├── lib.rs
│   │   ├── client.rs      # Generic state client
│   │   ├── websocket.rs   # WebSocket implementation
│   │   ├── http.rs        # HTTP fallback
│   │   └── shared_mem.rs  # Shared memory (local)
│   └── Cargo.toml
```

## Migration Path

### Phase 1: Add State Server (Non-Breaking)
- Keep CLI working as-is
- Add state server as optional component
- CLI can work standalone or connected

### Phase 2: Refactor CLI (Breaking)
- Update CLI to use `StateClient`
- Remove local playhead advancement
- State comes from server

### Phase 3: Add External Backends
- Implement Reaper backend
- Add network transport backend
- Support multiple backends simultaneously

### Phase 4: Add Web Client
- Create web application
- Use same `StateClient` pattern
- Share UI components where possible

## Benefits of This Architecture

1. **Stateless Clients**: Clients are pure view/control layers in slave mode
2. **Single Source of Truth**: State lives in one place (state server)
3. **Real-time Updates**: All clients see changes immediately via message router
4. **Protocol Agnostic**: Clients can connect via UDP, OSC, WebSocket, HTTP, etc.
5. **Flexible Backends**: Can swap between Reaper, local, network
6. **Scalable**: Easy to add new clients (just connect to router)
7. **Testable**: Can test state server and clients independently
8. **Decoupled**: Clients don't need to know about transport internals
9. **Backward Compatible**: Standalone mode still works for offline use

## Key Advantages of Stateless Clients

1. **No State Drift**: Clients can't have stale or incorrect state
2. **Simple Logic**: Clients just render what they receive
3. **Easy Recovery**: Reconnect and get full state immediately
4. **No Playhead Advancement**: Server handles all timing
5. **Consistent UI**: All clients show identical state

## Implementation Feasibility

**✅ Highly Achievable** because:

1. **tokio::sync::broadcast** already provides pub/sub infrastructure
2. **rosc** crate already in dependencies for OSC support
3. **axum** already in dependencies for HTTP/WebSocket
4. **Message Router Pattern** is well-established
5. **Stateless clients** simplify implementation (less code!)

## Open Questions

1. **State Persistence**: Should state be persisted? (SQLite, file, etc.)
2. **Conflict Resolution**: What if multiple clients send conflicting commands?
   - **Answer**: First-wins or timestamp-based ordering
3. **Authentication**: Do we need auth for network access?
   - **Answer**: Start without, add later if needed
4. **Performance**: How many clients can one server handle?
   - **Answer**: `tokio::broadcast` handles thousands efficiently
5. **Backend Priority**: What if Reaper and local backend conflict?
   - **Answer**: Configurable backend priority/fallback order
6. **Message Router Location**: Standalone server or embedded?
   - **Answer**: Can be both - embedded in Reaper extension, standalone for network

## Next Steps

1. **Decide on communication pattern**: WebSocket vs SSE vs both
2. **Create state-server crate**: Start with basic structure
3. **Implement StateManager**: Central state management
4. **Add WebSocket server**: Real-time updates
5. **Refactor CLI**: Use StateClient instead of direct manipulation
6. **Test end-to-end**: Verify state synchronization works

