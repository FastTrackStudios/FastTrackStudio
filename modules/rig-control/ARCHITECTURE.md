# RigControl ROAM Architecture

## Overview

The rig control system uses ROAM services with multiple transport backends, allowing the same service interface to work in:
- Audio plugins (local, minimal latency)
- Desktop apps (SHM, crash isolation)
- Web/mobile apps (network, remote control)

## Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│ RigControlService Trait (Service Definition)                   │
│ ┌─────────────────────────────────────────────────────────────┐ │
│ │ async fn get_current_preset(&self) -> Option<PresetInfo>   │ │
│ │ async fn load_preset(&self, preset_id: Uuid)               │ │
│ │ async fn execute(&self, command: RigControlCommand)        │ │
│ │ ... (all rig control methods)                              │ │
│ └─────────────────────────────────────────────────────────────┘ │
└───────────┬─────────────────────────────────────────────────────┘
            │ Implemented by ↓
    ┌───────┴────────┬────────────────┐
    │                │                │
    ▼                ▼                ▼
┌──────────┐  ┌──────────┐  ┌──────────────┐
│   Mock   │  │   Live   │  │ ROAM Client  │
│ Service  │  │ Service  │  │  (generated) │
└──────────┘  └──────────┘  └──────────────┘
    │              │              │
    │              │              │ Uses ↓
    │              │         ┌────┴─────┬────────┐
    │              │         │          │        │
    │              │         ▼          ▼        ▼
    │              │      ┌────┐   ┌─────┐  ┌─────────┐
    │              │      │SHM │   │ TCP │  │WebSocket│
    │              │      └────┘   └─────┘  └─────────┘
    │              │
    └──────┬───────┘
           │ Wrapped by ↓
           ▼
    ┌─────────────┐
    │ RigService  │ ← Enum with transport variants
    └─────────────┘
           │
           │ Used by ↓
    ┌──────┴───────────────────────────────┐
    │                                      │
    ▼                                      ▼
┌────────────┐                      ┌──────────┐
│ UI Layer   │                      │ Hooks    │
│ Components │                      │ Actions  │
└────────────┘                      └──────────┘
```

## Transport Selection by Use Case

### Audio Plugins (VST/CLAP)

```rust
// In plugin initialization
let service = RigService::local(LiveRigControlService::new());

// Performance: ~0.1-1ns overhead (essentially free)
// Why: DSP needs minimal latency, no IPC overhead
// Trade-off: Plugin crash = whole process crashes
```

### Desktop App (Development Mode)

```rust
// In development
let service = RigService::local(MockRigControlService::with_guitar_defaults());

// Performance: Same as audio plugins
// Why: Fast iteration, no setup required
// Trade-off: No crash isolation
```

### Desktop App (Production with REAPER)

```rust
// Connect to REAPER extension's SHM hub
let connection = shm_connect("/dev/shm/fts-rig", peer_id)?;
let service = RigService::shm(connection);

// Performance: ~1-2µs (cache-line copy)
// Why: Crash isolation, hot reload, modularity
// Trade-off: Slightly more complex setup
```

### Web/Mobile Apps

```rust
// Connect via WebSocket or TCP
let client = tcp_connect("localhost:8080")?;
let service = RigService::network(client);

// Performance: ~50-200µs (localhost)
// Why: Works over network, no platform dependencies
// Trade-off: Higher latency (but acceptable for rig control)
```

## Key Benefits

### 1. **Uniform Interface**

```rust
// Same code works everywhere
pub async fn load_preset_action(service: &RigService, preset_id: Uuid) {
    service.load_preset(preset_id).await;
}

// Works in audio plugin (local)
load_preset_action(&plugin_service, preset_id).await;

// Works in desktop app (SHM)
load_preset_action(&desktop_service, preset_id).await;

// Works in web app (network)
load_preset_action(&web_service, preset_id).await;
```

### 2. **Swappable at Runtime**

```rust
let service = match config.mode {
    Mode::Plugin => RigService::local(LiveRigControlService::new()),
    Mode::Desktop => RigService::shm(shm_connect()?),
    Mode::Remote => RigService::network(tcp_connect()?),
};

// All UI code works with any transport
```

### 3. **Performance Where It Matters**

- Audio plugins use local (no overhead)
- UI apps can use SHM (acceptable latency)
- Remote apps use network (still fine for control)

### 4. **Gradual Migration**

Start with local everywhere, add SHM later:

```rust
// Phase 1: Everything local
let service = RigService::local(mock);

// Phase 2: Add SHM for desktop
let service = if cfg!(feature = "shm") {
    RigService::shm(connect()?)
} else {
    RigService::local(mock)
};

// Phase 3: Add network for remote
let service = match transport {
    Transport::Local => RigService::local(mock),
    Transport::Shm => RigService::shm(connect()?),
    Transport::Network => RigService::network(connect()?),
};
```

## Implementation Phases

### ✅ Phase 1: Foundation (Current)
- [x] Define `RigControlService` trait with ROAM
- [x] Implement `MockRigControlService`
- [x] Create `RigService::Local` variant
- [x] Use in UI components and hooks
- [x] Pattern proven in `roam_test.rs`

### 🚧 Phase 2: SHM Transport (Next)
- [ ] Add SHM dependency
- [ ] Create host in REAPER extension
- [ ] Add `RigService::Shm` variant
- [ ] Update desktop app to connect via SHM
- [ ] Test crash isolation

### 🔮 Phase 3: Network Transport (Future)
- [ ] Add network dependency
- [ ] Implement WebSocket/TCP bridge
- [ ] Add `RigService::Network` variant
- [ ] Support remote control from web/mobile

### 🔮 Phase 4: Live Service (Future)
- [ ] Implement `LiveRigControlService` (real DSP)
- [ ] Integrate with rig engine
- [ ] Use in audio plugins

## Code Organization

```
modules/rig-control/
├── src/
│   ├── service.rs              # RigControlService trait definition
│   ├── roam_test.rs            # ROAM pattern examples
│   ├── mock.rs                 # MockRigControlService impl
│   └── ui/
│       ├── context/
│       │   ├── rig.rs          # RigService enum + context
│       │   └── rig_service_pattern.rs  # Architecture examples
│       └── hooks/
│           ├── rig_actions.rs  # Actions using RigService
│           └── rig_state.rs    # State subscription
```

## Testing Strategy

```rust
#[cfg(test)]
mod tests {
    // Unit tests: Use Local with Mock
    let service = RigService::local(MockRigControlService::new());

    // Integration tests: Use Local with Live
    let service = RigService::local(LiveRigControlService::new());

    // E2E tests: Use SHM
    let service = RigService::shm(test_connection);
}
```

## Performance Characteristics

| Transport | Latency    | Throughput | Use Case                |
|-----------|------------|------------|-------------------------|
| Local     | ~0.1-1ns   | Unlimited  | Audio plugins, tests    |
| SHM       | ~1-2µs     | ~1GB/s     | Desktop ↔ REAPER        |
| Network   | ~50-200µs  | ~100MB/s   | Remote control          |

All transports are suitable for rig control operations (preset switching, parameter updates). Only audio DSP requires Local transport.

## Error Handling

```rust
impl RigService {
    pub async fn get_current_preset(&self) -> Result<Option<PresetInfo>, RigError> {
        match self {
            Self::Local(s) => {
                let cx = dummy_context();
                Ok(s.get_current_preset(&cx).await)
            }
            Self::Shm(h) => {
                RigControlClient::new(h.clone())
                    .get_current_preset()
                    .await
                    .map_err(|e| RigError::Transport(e))
            }
            Self::Network(c) => {
                c.call(method_id::get_current_preset(), &())
                    .await
                    .map_err(|e| RigError::Network(e))
            }
        }
    }
}
```

## Conclusion

Using ROAM services everywhere gives you:
- ✅ Same interface for all transports
- ✅ Minimal overhead for local (audio plugins)
- ✅ Crash isolation for SHM (desktop apps)
- ✅ Network support for remote (web/mobile)
- ✅ Easy testing (swap mock for live)
- ✅ Gradual migration (add transports incrementally)

The key insight: **Define once, use everywhere, swap transports at will.**
