# State Management Architecture

This document explains how the state management system works, supporting both local (client-managed) and server-side (stateless client) modes.

## Overview

The application can run in two modes:

1. **Local Mode**: All state is managed client-side. The app works standalone without any external server.
2. **Server Mode**: State is managed server-side. The client is stateless and receives updates via WebSocket.

## Architecture

### State Management Structure

```
src/state/
├── mod.rs           # StateManager struct and StateManagerMode enum
├── local.rs         # Local state operations (direct signal updates)
├── server.rs        # Server WebSocket connection using Dioxus API
├── messages.rs      # ClientEvent and ServerEvent message types
└── integration.rs  # Helper functions for component integration
```

### Key Components

#### 1. StateManagerMode

```rust
pub enum StateManagerMode {
    Local,
    Server { url: String },
}
```

Determines whether state is managed locally or server-side.

#### 2. Message Types

**ClientEvent** (sent from client to server):
- `RequestSetlist` - Request current setlist
- `SwitchToProject { project_name }` - Switch active project
- `SeekToSection { project_name, song_name, section_name }` - Seek to section
- `NavigateNextSection`, `NavigatePreviousSection` - Section navigation
- `NavigateNextSong`, `NavigatePreviousSong` - Song navigation
- `TogglePlayPause`, `ToggleLoop` - Transport controls
- `Ping` - Connection health check

**ServerEvent** (sent from server to client):
- `SetlistUpdate { setlist, active_song_index }` - Full setlist update
- `TransportUpdate { project_name, playing, position, ... }` - Transport state
- `Pong` - Response to ping

#### 3. WebSocket Server Function

The `setlist_ws` server function (in `server.rs`) acts as a bridge:

- **On Server**: Connects to external WebSocket servers (like reaper_extension) and forwards messages
- **On Client**: Called via `use_websocket` hook, which handles the HTTP/WebSocket connection

```rust
#[get("/api/setlist_ws?server_url")]
pub async fn setlist_ws(
    server_url: String,
    options: WebSocketOptions,
) -> Result<Websocket<ClientEvent, ServerEvent, JsonEncoding>, ServerFnError>
```

## Usage Example

### Basic Setup

```rust
use crate::state::{StateManagerMode, integration::initialize_state_manager};

#[component]
fn App() -> Element {
    // Determine mode (could be based on config, environment, or user choice)
    let mode = StateManagerMode::Local; // or Server { url: "localhost:8080".to_string() }
    
    // Initialize state
    let initial_setlist = create_sample_setlist();
    let (
        setlist,
        transport_positions,
        song_positions,
        current_song_index,
        current_section_index,
        is_playing,
        is_looping,
        connection_status,
    ) = initialize_state_manager(initial_setlist, mode);
    
    // Use signals as normal - they work the same regardless of mode
    rsx! {
        // Your UI components
    }
}
```

### Navigation Actions

```rust
use crate::state::local::*;

// In local mode, call functions directly:
navigate_to_next_section_local(
    setlist,
    transport_positions,
    song_positions,
    current_song_index,
    current_section_index,
);

// In server mode, send events via WebSocket:
socket.send(ClientEvent::NavigateNextSection).await;
```

### Server Mode Setup

When running in server mode:

1. **Dioxus Fullstack Server**: The `setlist_ws` server function bridges to external servers
2. **External Server** (e.g., reaper_extension): Must implement the SetlistMessage protocol
3. **Client**: Uses `use_websocket` hook which automatically connects via the server function

## Benefits

1. **Unified Interface**: Same signals and components work in both modes
2. **Stateless Client**: In server mode, client doesn't manage state - just displays it
3. **Fallback Support**: Can fall back to local mode if server unavailable
4. **Type Safety**: Strongly typed messages via Dioxus WebSocket API
5. **Reactive**: Signals automatically update UI when server sends updates

## Future Enhancements

- Auto-detection of server availability with fallback to local mode
- Connection retry logic
- State synchronization on reconnect
- Support for multiple server backends (reaper_extension, Dioxus server, etc.)

