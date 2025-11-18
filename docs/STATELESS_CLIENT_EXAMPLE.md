# Stateless Client Implementation Example

## Core Concept

Clients operate in **two modes**:

1. **Standalone Mode**: Client manages its own state (current behavior)
2. **Slave Mode**: Client is **completely stateless** - only renders received updates

## Stateless Client Flow

```
┌─────────────────────────────────────────────────────────┐
│              Stateless Client (Slave Mode)              │
│                                                         │
│  ┌──────────────────────────────────────────────────┐  │
│  │  StateClient (NO LOCAL STATE)                    │  │
│  │  - Receives: Message::TransportChanged           │  │
│  │  - Receives: Message::SetlistChanged             │  │
│  │  - Sends: Message::Command                       │  │
│  └──────────────────────────────────────────────────┘  │
│                          │                             │
│  ┌───────────────────────▼──────────────────────────┐  │
│  │  UI Rendering (Based on Received State Only)     │  │
│  │  - current_transport: Option<Transport>         │  │
│  │  - current_setlist: Option<Setlist>              │  │
│  │  - If None → Show "Connecting..."               │  │
│  └──────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────┘
```

## Code Example: Stateless CLI

```rust
// apps/cli/src/app.rs

use message_router::{StateClient, Message, TransportCommand};
use transport::Transport;
use setlist::Setlist;

pub struct CliSetlistApp {
    // State client - NO local transport state
    state_client: StateClient,
    
    // UI-only state (selection, expansion, etc.)
    selected_song_index: usize,
    selected_section_index: Option<usize>,
    expanded_songs: HashSet<usize>,
    
    // Current received state (for rendering ONLY)
    // These are Option<> because we might not have received state yet
    current_transport: Option<Transport>,
    current_setlist: Option<Setlist>,
}

impl CliSetlistApp {
    pub async fn new_slave(router_url: &str) -> Result<Self, Error> {
        // Connect to message router
        let mut state_client = StateClient::new_slave(router_url).await?;
        
        // Request initial state (optional - server may send it automatically)
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
        
        // Try to receive updates (non-blocking)
        while let Ok(message) = self.state_client.receive_update().try_recv() {
            match message {
                Message::TransportChanged { transport, .. } => {
                    // Update rendering state only
                    self.current_transport = Some(transport);
                    // UI will redraw on next frame
                }
                Message::SetlistChanged { setlist } => {
                    self.current_setlist = Some(setlist);
                }
                Message::StateUpdate { transport, setlist, .. } => {
                    // Full state update (initial connection)
                    self.current_transport = Some(transport);
                    self.current_setlist = Some(setlist);
                }
                Message::PositionUpdated { position, .. } => {
                    // Update position in current transport
                    if let Some(ref mut transport) = self.current_transport {
                        transport.playhead_position.time = TimePosition::from_seconds(position);
                    }
                }
                _ => {}
            }
        }
        
        // NO playhead advancement here!
        // Server handles all timing
    }
    
    pub fn handle_key(&mut self, key: KeyEvent) {
        // Send commands to server - don't apply locally
        match key.code {
            KeyCode::Char(' ') => {
                // Send command, don't change local state
                self.state_client.send_command(TransportCommand::PlayPause).await?;
                // State will be updated via Message::TransportChanged
            }
            KeyCode::Char('b') => {
                self.state_client.send_command(TransportCommand::Back).await?;
            }
            KeyCode::Char('L') => {
                self.state_client.send_command(TransportCommand::ToggleLoop).await?;
            }
            // Navigation commands can be local (UI-only)
            KeyCode::Up => {
                // This is UI state, not transport state - OK to handle locally
                if self.selected_song_index > 0 {
                    self.selected_song_index -= 1;
                }
            }
            // ...
        }
    }
    
    pub fn draw(&mut self, frame: &mut Frame) {
        // Render based on received state
        match (&self.current_transport, &self.current_setlist) {
            (Some(transport), Some(setlist)) => {
                // We have state - render normally
                crate::ui::draw_ui(frame, self, transport, setlist);
            }
            _ => {
                // No state yet - show connection status
                self.draw_connecting(frame);
            }
        }
    }
    
    fn draw_connecting(&self, frame: &mut Frame) {
        // Show "Connecting to server..." message
    }
}
```

## Message Router Setup

```rust
// Server side: modules/daw/state-server/src/server.rs

use message_router::MessageRouter;
use tokio::sync::broadcast;

pub async fn start_server() -> Result<(), Error> {
    // Create message router
    let router = Arc::new(MessageRouter::new());
    
    // Start protocol adapters
    router.start_websocket(8080).await?;  // WebSocket on port 8080
    router.start_osc(9000).await?;      // OSC on port 9000
    router.start_udp(8081).await?;      // UDP on port 8081
    
    // Create state manager
    let state_manager = Arc::new(StateManager::new(router.clone()));
    
    // Subscribe router to state manager
    let mut state_rx = state_manager.subscribe();
    tokio::spawn(async move {
        while let Ok(event) = state_rx.recv().await {
            // Convert state event to message and broadcast
            let message = Message::from_state_event(event);
            router.broadcast(message).ok();
        }
    });
    
    // Subscribe state manager to router commands
    let mut router_rx = router.subscribe();
    tokio::spawn(async move {
        while let Ok(message) = router_rx.recv().await {
            if let Message::Command { command, .. } = message {
                state_manager.handle_command(command).await.ok();
            }
        }
    });
    
    // Start Reaper backend monitoring
    let mut reaper_backend = ReaperTransportBackend::new()?;
    reaper_backend.start_monitoring(router.clone()).await?;
    
    // Keep server running
    tokio::signal::ctrl_c().await?;
    Ok(())
}
```

## Client Connection Examples

### CLI Client (Rust)

```rust
// Connect via WebSocket
let app = CliSetlistApp::new_slave("ws://localhost:8080").await?;

// Or connect via OSC
let app = CliSetlistApp::new_slave("osc://localhost:9000").await?;

// Or connect via UDP
let app = CliSetlistApp::new_slave("udp://localhost:8081").await?;
```

### Web Client (TypeScript)

```typescript
// Connect via WebSocket
const client = new StateClient("ws://localhost:8080");
await client.connect();

// Subscribe to state updates
client.subscribe((transport, setlist) => {
  // Update React state
  setTransport(transport);
  setSetlist(setlist);
});

// Send commands
await client.sendCommand(TransportCommand.PlayPause);
```

### Reaper Extension (C++/Lua)

```lua
-- Connect via OSC (Reaper has built-in OSC support)
local osc = reaper.GetOSCAddress()

-- Send commands
reaper.OSC_Send(osc, "/transport/play", {})

-- Receive state updates
reaper.OSC_Listen(osc, function(message)
    if message.addr == "/transport/state" then
        -- Update Reaper transport state
    end
end)
```

## Benefits

1. **Zero State Management**: Clients don't manage transport state
2. **Always Consistent**: All clients show identical state
3. **Easy Recovery**: Reconnect = get full state immediately
4. **Protocol Flexible**: Use best protocol for each client
5. **Simple Logic**: Clients just render what they receive

