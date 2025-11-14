# Transport Module

A comprehensive transport control system for DAW applications, built with Rust and following clean architecture principles.

## Features

- **Multi-Protocol Support**: HTTP REST API, WebSocket real-time, and OSC (Open Sound Control)
- **Clean Architecture**: Separation between core domain logic and infrastructure adapters
- **Async/Await Support**: Built on Tokio for high-performance async operations
- **Type Safety**: Full Rust type safety with comprehensive error handling
- **Extensible Design**: Easy to add new transport implementations and protocols

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    INFRASTRUCTURE                           │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────┐ │
│  │    HTTP     │  │    OSC      │  │    WebSocket        │ │
│  │  (REST API) │  │ (Real-time) │  │  (Real-time)        │ │
│  └─────────────┘  └─────────────┘  └─────────────────────┘ │
└─────────────────────────┬───────────────────────────────────┘
                          │ Adapters
┌─────────────────────────▼───────────────────────────────────┐
│                      CORE DOMAIN                            │
│  ┌─────────────────────────────────────────────────────────┐ │
│  │  TransportActions trait (Port)                          │ │
│  │  Transport, Tempo, PlayState, RecordMode (Types)       │ │
│  │  TransportError (Error Handling)                       │ │
│  └─────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

## Quick Start

### HTTP Server

```bash
cargo run --example core_and_infra
```

Then test with curl:
```bash
# Start playback
curl -X POST http://localhost:3000/play

# Set tempo
curl -X POST http://localhost:3000/set_tempo \
     -H "Content-Type: application/json" \
     -d '{"bpm": 140.0}'

# Get status
curl http://localhost:3000/status
```

### OSC Server

```bash
cargo run --example osc_server
```

Then send OSC messages (using `oscsend` from liblo):
```bash
# Start playback
oscsend localhost 9000 /transport/play

# Set tempo to 140 BPM
oscsend localhost 9000 /transport/tempo f 140.0

# Set time signature to 3/4
oscsend localhost 9000 /transport/time_signature i 3 i 4

# Get complete status
oscsend localhost 9000 /transport/status
```

### Multi-Protocol Server

Run all protocols simultaneously:
```bash
cargo run --example multi_protocol_server
```

This starts:
- HTTP API on port 3000
- WebSocket server on port 3001
- OSC server on UDP port 9000

## OSC (Open Sound Control) Support

The transport module provides comprehensive OSC support for real-time control from any OSC-capable application like TouchOSC, Max/MSP, Pure Data, Ableton Live, etc. The OSC server uses high-performance `matchit` routing for extremely fast message dispatch.

### OSC Address Patterns

#### Transport Control
- `/transport/play` - Start playback
- `/transport/pause` - Pause playback
- `/transport/stop` - Stop playback
- `/transport/play_pause` - Toggle play/pause
- `/transport/play_stop` - Toggle play/stop

#### Recording Control
- `/transport/record/start` - Start recording
- `/transport/record/stop` - Stop recording
- `/transport/record/toggle` - Toggle recording

#### Configuration
- `/transport/tempo` [float bpm] - Set tempo
- `/transport/time_signature` [int numerator] [int denominator] - Set time signature
- `/transport/position` [float seconds] - Set position
- `/transport/record_mode` [string mode] - Set record mode ("normal", "time_selection", "item")

#### Queries (with responses)
- `/transport/status` - Get complete transport status
- `/transport/is_playing` - Get playing state (bool)
- `/transport/is_recording` - Get recording state (bool)
- `/transport/get_tempo` - Get current tempo (float)
- `/transport/get_position` - Get current position (float)
- `/transport/get_time_signature` - Get time signature (int, int)
- `/transport/get_record_mode` - Get record mode (string)
- `/transport/is_ready` - Check if transport is ready (bool)

### OSC Response Patterns

Successful commands return:
- Address: `/transport/response`
- Args: [string "success", string message]

Errors return:
- Address: `/transport/error`
- Args: [string error_message]

Query responses use specific addresses:
- `/transport/status/response` - Complete status with all fields
- `/transport/tempo/response` - Tempo value
- `/transport/is_playing/response` - Boolean playing state
- etc.

### OSC Examples

Using `oscsend` from liblo:

```bash
# Basic transport control
oscsend localhost 9000 /transport/play
oscsend localhost 9000 /transport/pause
oscsend localhost 9000 /transport/stop

# Set tempo to 128 BPM
oscsend localhost 9000 /transport/tempo f 128.0

# Set time signature to 4/4
oscsend localhost 9000 /transport/time_signature i 4 i 4

# Jump to 30.5 seconds
oscsend localhost 9000 /transport/position f 30.5

# Set record mode to time selection
oscsend localhost 9000 /transport/record_mode s time_selection

# Recording control
oscsend localhost 9000 /transport/record/start
oscsend localhost 9000 /transport/record/stop

# Query current status
oscsend localhost 9000 /transport/status
oscsend localhost 9000 /transport/get_tempo
oscsend localhost 9000 /transport/is_playing
```

## HTTP API Endpoints

### Transport Control
- `POST /play` - Start playback
- `POST /pause` - Pause playback  
- `POST /stop` - Stop playback
- `POST /play_pause` - Toggle play/pause
- `POST /play_stop` - Toggle play/stop

### Recording Control
- `POST /start_recording` - Start recording
- `POST /stop_recording` - Stop recording
- `POST /toggle_recording` - Toggle recording

### Configuration
- `POST /set_tempo` - Set tempo (JSON: `{"bpm": 140.0}`)
- `POST /set_time_signature` - Set time signature (JSON: `{"numerator": 4, "denominator": 4}`)
- `POST /set_position` - Set position (JSON: `{"seconds": 30.5}`)
- `POST /set_record_mode` - Set record mode (JSON: `{"mode": "normal"}`)

### Queries
- `GET /status` - Get complete transport status
- `GET /is_playing` - Check if playing
- `GET /is_recording` - Check if recording
- `GET /tempo` - Get current tempo
- `GET /position` - Get current position
- `GET /time_signature` - Get time signature
- `GET /record_mode` - Get record mode
- `GET /is_ready` - Check if ready

## WebSocket Support

Real-time bidirectional communication for live transport updates.

Connect to: `ws://localhost:3001/ws`

Send JSON commands:
```json
{"type": "Command", "data": {"action": "Play"}}
{"type": "Command", "data": {"action": "SetTempo", "bpm": 140.0}}
```

Receive real-time status updates:
```json
{"type": "StatusUpdate", "data": {"is_playing": true, "tempo_bpm": 140.0, ...}}
```

## Core Types

### TransportActions Trait

The main interface for all transport operations:

```rust
pub trait TransportActions: Send + Sync {
    // Transport control
    fn play(&mut self) -> Result<String, TransportError>;
    fn pause(&mut self) -> Result<String, TransportError>;
    fn stop(&mut self) -> Result<String, TransportError>;
    
    // Configuration
    fn set_tempo(&mut self, tempo: Tempo) -> Result<String, TransportError>;
    fn set_time_signature(&mut self, time_signature: TimeSignature) -> Result<String, TransportError>;
    
    // Queries
    fn get_tempo(&self) -> Result<Tempo, TransportError>;
    fn is_playing(&self) -> Result<bool, TransportError>;
    // ... and more
}
```

### Transport State

```rust
pub struct Transport {
    pub play_state: PlayState,
    pub tempo: Tempo,
    pub time_signature: TimeSignature,
    pub playhead_position: Position,
    pub record_mode: RecordMode,
    // ... and more fields
}
```

## Examples

Run the examples to see the transport system in action:

- `cargo run --example osc_server` - OSC server only
- `cargo run --example core_and_infra` - HTTP server only
- `cargo run --example websocket_server` - WebSocket server only
- `cargo run --example multi_protocol_server` - All protocols together
- `cargo run --example test_osc_client` - OSC client test

## Testing

### Unit Tests
```bash
cargo test
```

### Integration Testing with OSC

1. Start the OSC server:
   ```bash
   cargo run --example osc_server
   ```

2. In another terminal, run the client test:
   ```bash
   cargo run --example test_osc_client
   ```

3. Or test manually with `oscsend`:
   ```bash
   oscsend localhost 9000 /transport/play
   oscsend localhost 9000 /transport/status
   ```

## Dependencies

- **rosc** - OSC protocol implementation
- **matchit** - High-performance URL router for OSC routing
- **axum** - HTTP web framework
- **tokio** - Async runtime
- **tower** - Service abstractions
- **tower-http** - HTTP middleware
- **serde** - Serialization
- **thiserror** - Error handling

## License

MIT OR Apache-2.0