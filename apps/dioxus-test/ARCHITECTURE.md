# Multi-Client Architecture

This document explains the multi-client architecture where the desktop app serves the web UI and manages WebSocket connections.

## Overview

The application supports multiple clients (web browsers, desktop app itself) where:

1. **Desktop App** serves the web UI via Axum (accessible on local network)
2. **Desktop App** manages WebSocket connections to external servers (like reaper_extension)
3. **Each Client** (web or desktop) can toggle between Local and Server modes independently
4. **Desktop App** can also toggle between Local and Server modes

## Architecture Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                    Desktop App (Dioxus)                    │
│  ┌──────────────────────────────────────────────────────┐ │
│  │  Web UI Server (Axum) - serves on 0.0.0.0:8080     │ │
│  │  - Serves Dioxus app to web browsers                 │ │
│  │  - WebSocket endpoint: /api/setlist_ws               │ │
│  └──────────────────────────────────────────────────────┘ │
│                                                             │
│  ┌──────────────────────────────────────────────────────┐ │
│  │  State Management                                     │ │
│  │  - Local Mode: Manages state itself                  │ │
│  │  - Server Mode: Bridges to external server           │ │
│  └──────────────────────────────────────────────────────┘ │
│                                                             │
│  ┌──────────────────────────────────────────────────────┐ │
│  │  External Server Bridge (reaper_extension)          │ │
│  │  - Unix sockets / threads for fast communication     │ │
│  └──────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
         │                    │                    │
         │                    │                    │
    ┌────▼────┐         ┌────▼────┐         ┌────▼────┐
    │ Browser │         │ Browser │         │ Browser │
    │ Client  │         │ Client  │         │ Client  │
    │         │         │         │         │         │
    │ Local/  │         │ Local/  │         │ Local/  │
    │ Server  │         │ Server  │         │ Server  │
    │ Toggle  │         │ Toggle  │         │ Toggle  │
    └─────────┘         └─────────┘         └─────────┘
```

## Modes

### Desktop App Modes

1. **Local Mode**: Desktop app manages all state itself (like current implementation)
2. **Server Mode**: Desktop app gets state from external server (reaper_extension)

### Web Client Modes

1. **Local Mode**: Web client manages its own state independently
2. **Server Mode**: Web client connects to desktop app's WebSocket, receives state updates

## Implementation Details

### Desktop App Setup

```rust
// main.rs
#[cfg(feature = "server")]
fn main() {
    dioxus::serve(|| async move {
        use dioxus::server::router;
        Ok(router(App))
    });
}
```

The desktop app:
- Serves the Dioxus app via Axum on `0.0.0.0:8080` (accessible on local network)
- Provides WebSocket endpoint at `/api/setlist_ws`
- Can bridge to external servers (reaper_extension) when in server mode

### WebSocket Server Function

```rust
#[get("/api/setlist_ws?external_server_url")]
pub async fn setlist_ws(
    external_server_url: Option<String>,
    options: WebSocketOptions,
) -> Result<Websocket<ClientEvent, ServerEvent, JsonEncoding>, ServerFnError>
```

- When `external_server_url` is `None`: Desktop app manages state itself
- When `external_server_url` is `Some(url)`: Desktop app bridges to external server

### Client Connection

Web clients connect to desktop app:

```rust
// Connect to desktop app (localhost means desktop app manages state)
let socket = use_websocket(|| setlist_ws(None, WebSocketOptions::new()));

// Or connect with external server URL (desktop app bridges)
let socket = use_websocket(|| setlist_ws(Some("reaper:8080".to_string()), WebSocketOptions::new()));
```

### Mode Toggle

Each client (web or desktop) has a toggle component:

```rust
<ModeToggle 
    mode={mode} 
    connection_status={connection_status} 
/>
```

- **Local Mode**: State managed independently, no WebSocket connection
- **Server Mode**: Connects to desktop app's WebSocket, receives state updates

## Message Flow

### Web Client → Desktop App → External Server

1. Web client sends `ClientEvent` via WebSocket
2. Desktop app receives event
3. Desktop app forwards to external server (if in server mode)
4. External server processes and sends `ServerEvent`
5. Desktop app receives `ServerEvent` and broadcasts to all connected clients
6. Web clients receive update and update their UI

### Desktop App Local Mode

1. Desktop app manages state itself
2. User interactions update local signals directly
3. No external communication needed

### Web Client Local Mode

1. Web client manages state itself
2. User interactions update local signals directly
3. No WebSocket connection

## Benefits

1. **Flexible**: Each client can operate independently
2. **Scalable**: Multiple web clients can connect simultaneously
3. **Fast**: Desktop app uses Unix sockets/threads for external communication
4. **Network Accessible**: Web UI accessible from any device on local network
5. **Fallback**: Clients can work locally if server unavailable

## Future Enhancements

- Server-side state management for desktop app (when not bridging)
- Connection retry logic
- State synchronization on reconnect
- Multiple external server support
- Authentication/authorization for web clients

