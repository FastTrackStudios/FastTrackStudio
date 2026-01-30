# SHM Host/Plugin Architecture Evaluation

## ROAM SHM Capabilities

From the spec, ROAM SHM provides:

### Hub Topology (1:N)
- **One Host** creates/owns the shared memory segment
- **Multiple Guests** (up to 255) attach to communicate with the host
- Guests only communicate with the host, NOT with each other
- Either host or guest can initiate calls

### Key Features
1. **Crash Isolation** - Guest crashes don't kill the host
2. **Death Detection** - Host detects crashed guests via:
   - Doorbell POLLHUP (immediate, Unix)
   - Heartbeat timeout (2× interval)
   - Process handles (immediate, all platforms)
3. **Hot Reload** - Restart guests without affecting the host
4. **Performance** - ~1-2µs per call (acceptable for control, not audio DSP)
5. **Resource Cleanup** - Host automatically cleans up crashed guest resources

## Proposed Architecture

### Option A: Fine-Grained Services (Your Suggestion)

```
┌─────────────────────────────────────────────────────────────┐
│ Host: ReaperHost or FTSHost                                 │
└──────┬──────────────────────────────────────────────────────┘
       │
   ┌───┴────┬─────────┬──────────┬──────────┬──────────┬──────┐
   │        │         │          │          │          │      │
┌──┴───┐ ┌─┴──┐ ┌────┴────┐ ┌───┴────┐ ┌───┴────┐ ┌───┴────┐
│Trans │ │Set │ │Module   │ │Preset  │ │Profile │ │Song    │
│port  │ │list│ │Service  │ │Service │ │Service │ │Service │
└──────┘ └────┘ └─────────┘ └────────┘ └────────┘ └────────┘
```

**Pros:**
- ✅ Maximum isolation - Each service crashes independently
- ✅ Maximum hot-reload granularity
- ✅ Clear service boundaries
- ✅ Can scale services independently

**Cons:**
- ❌ **Over-engineered** - Do we really need preset/profile/song as separate processes?
- ❌ Complex deployment - 6+ processes to spawn/monitor
- ❌ Harder debugging - Multi-process debugging
- ❌ Overhead accumulation - 6+ SHM round-trips for a preset load
- ❌ **Artificial boundaries** - PresetService likely needs ProfileService data anyway

### Option B: Coarse-Grained Plugins (Recommended)

```
┌─────────────────────────────────────────────────────────────┐
│ Host: ReaperHost or FTSHost                                 │
└──────┬──────────────────────────────────────────────────────┘
       │
   ┌───┴────────┬────────────┬──────────┐
   │            │            │          │
┌──┴──────┐ ┌──┴────┐ ┌─────┴──────┐  │
│Transport│ │Setlist│ │RigControl  │  │...
│Plugin   │ │Plugin │ │Plugin      │  │
└─────────┘ └───────┘ └────┬───────┘  │
                            │
                   ┌────────┴─────────┐
                   │ Internal:        │
                   │ - ModuleService  │
                   │ - PresetService  │
                   │ - ProfileService │
                   │ - SongService    │
                   └──────────────────┘
```

**Pros:**
- ✅ Meaningful isolation boundaries (Transport ≠ Rig ≠ Setlist)
- ✅ Manageable process count (3-5 processes)
- ✅ Internal services can share data efficiently
- ✅ Simpler deployment and debugging
- ✅ Plugin can choose internal architecture (services, modules, etc.)

**Cons:**
- ❌ Less granular hot-reload (but is that needed?)
- ❌ Larger crash blast radius (but Rust crashes are rare)

## The Killer Use Case: REAPER Integration

**This is where SHM shines:**

```
┌──────────────────────────────────────────────────────────┐
│ REAPER Process (ReaperHost)                              │
│ ┌──────────────────────────────────────────────────────┐ │
│ │ FTS REAPER Extension (Host)                          │ │
│ │ - Creates SHM segment                                │ │
│ │ - Spawns/monitors guests                             │ │
│ │ - Provides DAW integration services                  │ │
│ └──────────────────────────────────────────────────────┘ │
└──────┬───────────────────────────────────────────────────┘
       │
   ┌───┴────────┬────────────┬──────────────┐
   │            │            │              │
┌──┴──────┐ ┌──┴────────┐ ┌─┴──────────┐ ┌─┴───────┐
│Desktop  │ │Web Client │ │Mobile App │ │Audio    │
│App      │ │(via WS    │ │(via WS    │ │Plugin   │
│(Guest)  │ │ bridge)   │ │ bridge)   │ │(Guest)  │
└─────────┘ └───────────┘ └───────────┘ └─────────┘
```

**Benefits:**
1. **Crash Isolation** - Desktop app crash doesn't kill REAPER
2. **Hot Reload** - Update desktop app without restarting REAPER session
3. **Multi-Client** - Desktop + web + mobile all connected to same REAPER
4. **Transport Flexibility** - Local SHM, remote WebSocket bridge
5. **Development** - Test UI changes without restarting REAPER

## Performance Analysis

### SHM Overhead Comparison

| Operation | Direct Call | In-Process Client | SHM Client | Impact |
|-----------|-------------|-------------------|------------|--------|
| Load preset | ~1-10ms | ~1ms + 100ns | ~1ms + 2µs | 0.0002% |
| Update param | ~100-500µs | ~100µs + 100ns | ~100µs + 2µs | 0.002% |
| Scene change | ~5-20ms | ~5ms + 100ns | ~5ms + 2µs | 0.0004% |

**Verdict:** SHM overhead is **completely negligible** for rig control operations.

## Recommendation

### ✅ Use SHM Host/Plugin Architecture IF:

1. **You want REAPER integration** - Desktop app ↔ REAPER extension
2. **You want crash isolation** - Desktop app crashes don't kill DAW session
3. **You want hot reload** - Update UI without restarting REAPER
4. **You want multi-client** - Desktop + web + mobile simultaneously

### The Correct Pattern: Plugins = Processes, Services = ROAM APIs

**KEY INSIGHT:** Each plugin is a **process** (SHM guest) that exposes **multiple ROAM services**:

```
┌──────────────────────────────────────────────────────────┐
│ ReaperHost (SHM Host in REAPER Extension)                │
└──────┬───────────────────────────────────────────────────┘
       │
   ┌───┴──────────┬─────────────┬──────────────┐
   │              │             │              │
┌──┴─────────────┐│             │              │
│ RigControl     ││ Setlist     │ Transport    │
│ Plugin         ││ Plugin      │ Plugin       │
│ (Process)      ││ (Process)   │ (Process)    │
│                ││             │              │
│ Services:      ││ Services:   │ Services:    │
│ ├─ModuleService││ ├─Setlist   │ ├─Transport  │
│ ├─PresetService││ │  Service  │ │  Service   │
│ ├─ProfileService│ └─SongService └─Metronome  │
│ └─RigSongService│                Service     │
└────────────────┘└─────────────┴──────────────┘
       ↓                 ↓              ↓
   All services accessible via ROAM Client from ANY client!
```

**Then from desktop/web/mobile:**

```rust
// All clients connect to the host
let handle = connect_to_host(...).await?;

// Create clients for ANY service from ANY plugin
let presets = PresetClient::new(handle.clone());
let modules = ModuleClient::new(handle.clone());
let transport = TransportClient::new(handle.clone());
let setlists = SetlistClient::new(handle.clone());

// Call methods - host routes to correct plugin process
presets.load_preset(preset_id).await?;
modules.add_module(module_type).await?;
transport.play().await?;
```

### Use **Coarse-Grained Plugins**, NOT fine-grained services:

```rust
// ✅ Good: Coarse-grained plugins
pub enum FTSPlugin {
    Transport,    // Transport control, metronome, recording
    Setlist,      // Setlists, songs, scenes
    RigControl,   // Modules, presets, profiles, parameters
    Lyrics,       // Lyrics display and sync
    Charts,       // Chart parsing and display
}

// ❌ Bad: Fine-grained services (over-engineered)
pub enum FTSService {
    Transport,
    Setlist,
    Song,
    Scene,
    Module,
    Preset,
    Profile,
    Parameter,
    Block,
    Routing,
    // ... 20+ services (too granular!)
}
```

### Proposed Implementation Strategy

#### Phase 1: ReaperHost + Desktop Guest

```rust
// In REAPER extension
let mut host = ReaperHost::new("/tmp/fts-reaper")?;
host.register_service::<TransportService>();
host.register_service::<SetlistService>();
host.register_service::<RigControlService>();
host.spawn_desktop_app()?;

// In desktop app
let guest = FTSGuest::attach("/tmp/fts-reaper", peer_id)?;
let transport = TransportClient::new(guest.handle());
let rig = RigControlClient::new(guest.handle());
```

#### Phase 2: Add Web Bridge

```rust
// Web/mobile connects via WebSocket bridge
let bridge = WebSocketBridge::new(host.handle())?;
bridge.listen("0.0.0.0:8080").await?;

// Web client connects remotely
let client = await fts.connect("ws://localhost:8080");
await client.rig.loadPreset(presetId);
```

#### Phase 3: Plugin Internal Architecture with Multiple Services

```rust
// RigControlPlugin is a process that exposes MULTIPLE ROAM services
pub struct RigControlPlugin {
    // Each plugin runs multiple service dispatchers
    module_service: Arc<ModuleServiceImpl>,
    preset_service: Arc<PresetServiceImpl>,
    profile_service: Arc<ProfileServiceImpl>,
    rig_song_service: Arc<RigSongServiceImpl>,
}

impl RigControlPlugin {
    pub fn start(hub_path: &str, peer_id: u8) -> Result<()> {
        // Attach to SHM as a guest
        let guest = SHMGuest::attach(hub_path, peer_id)?;

        // Register ALL services this plugin provides
        guest.register_service(ModuleDispatcher::new(module_service))?;
        guest.register_service(PresetDispatcher::new(preset_service))?;
        guest.register_service(ProfileDispatcher::new(profile_service))?;
        guest.register_service(RigSongDispatcher::new(rig_song_service))?;

        // The host will route calls to the correct dispatcher
        guest.run().await
    }
}

// From ANY client (desktop, web, mobile):
let handle = connect_to_host(...).await?;

// Create clients for services from different plugins
let presets = PresetClient::new(handle.clone());   // → RigControlPlugin
let modules = ModuleClient::new(handle.clone());   // → RigControlPlugin
let setlists = SetlistClient::new(handle.clone()); // → SetlistPlugin
let transport = TransportClient::new(handle.clone()); // → TransportPlugin

// All calls work the same - host routes to correct plugin/service
presets.load_preset(id).await?;  // Routed to RigControlPlugin's PresetService
```

**Key Benefits:**

1. **Process isolation** - RigControl crash doesn't affect Transport
2. **Service granularity** - Clean API boundaries (Module vs Preset vs Profile)
3. **Uniform client code** - Same Client pattern for all services
4. **Implementation flexibility** - Swap mock/live/cloud implementations
5. **Multi-client access** - Desktop, web, mobile all use same services

**The Pattern:**
- **Plugin** = Process boundary (crash isolation, hot reload)
- **Service** = API boundary (clean interfaces, swappable implementations)
- **Client** = Uniform access (same code for local/remote/mock)
```

## Decision Matrix

| Factor | In-Process | Coarse Plugins | Fine Services |
|--------|------------|----------------|---------------|
| **Crash isolation** | ❌ None | ✅ Good | ✅ Maximum |
| **Hot reload** | ❌ None | ✅ Plugin-level | ✅ Service-level |
| **Performance** | ✅ Best (~100ns) | ✅ Great (~2µs) | ⚠️ Okay (~10µs) |
| **Complexity** | ✅ Simple | ⚠️ Moderate | ❌ High |
| **REAPER integration** | ❌ Tight coupling | ✅ Clean | ✅ Clean |
| **Multi-client** | ❌ Hard | ✅ Easy | ✅ Easy |
| **Debugging** | ✅ Easy | ⚠️ Moderate | ❌ Hard |
| **Deployment** | ✅ 1 process | ⚠️ 3-5 processes | ❌ 10+ processes |

## Final Recommendation

**YES** - Use SHM host/plugin architecture, but with **coarse-grained plugins**:

1. **ReaperHost** or **FTSHost** (depending on deployment)
2. **3-5 coarse plugins**: Transport, Setlist, RigControl, Lyrics, Charts
3. Each plugin internally organizes as it sees fit (services, modules, etc.)
4. Desktop app = guest process (crash isolated from REAPER)
5. Web/mobile via WebSocket bridge (network transparency)

**Avoid** fine-grained service separation (Module/Preset/Profile as separate processes) - it's over-engineered for this domain.

## Complete Architecture Example

### Plugins and Their Services

```rust
// ── RigControlPlugin (Process) ───────────────────────────────────
#[roam::service]
trait ModuleService {
    async fn get_modules(&self) -> Vec<ModuleInfo>;
    async fn add_module(&self, module_type: ModuleType) -> Result<ModuleId>;
}

#[roam::service]
trait PresetService {
    async fn load_preset(&self, preset_id: Uuid);
    async fn get_available_presets(&self) -> Vec<PresetInfo>;
}

#[roam::service]
trait ProfileService {
    async fn load_profile(&self, profile_id: Uuid);
    async fn get_current_profile(&self) -> Option<ProfileInfo>;
}

// ── SetlistPlugin (Process) ──────────────────────────────────────
#[roam::service]
trait SetlistService {
    async fn load_setlist(&self, setlist_id: Uuid);
    async fn get_available_setlists(&self) -> Vec<SetlistInfo>;
}

#[roam::service]
trait SongService {
    async fn go_to_song(&self, song_index: usize);
    async fn get_current_song(&self) -> Option<SongInfo>;
}

// ── TransportPlugin (Process) ────────────────────────────────────
#[roam::service]
trait TransportService {
    async fn play(&self);
    async fn stop(&self);
    async fn get_transport_state(&self) -> TransportState;
}

// ── KeyflowPlugin (Process) ───────────────────────────────────────
#[roam::service]
trait ChordDetectionService {
    async fn detect_chords(&self, audio: Vec<f32>) -> Vec<Chord>;
    async fn start_realtime_detection(&self) -> Rx<Chord>;
}
```

### Client Code (Desktop/Web/Mobile)

```rust
// Desktop App
async fn main() {
    // Connect to REAPER host
    let handle = shm_connect("/tmp/fts-reaper", peer_id).await?;

    // Create clients for ALL services
    let presets = PresetClient::new(handle.clone());
    let modules = ModuleClient::new(handle.clone());
    let transport = TransportClient::new(handle.clone());
    let setlists = SetlistClient::new(handle.clone());
    let chords = ChordDetectionClient::new(handle.clone());

    // Use them - host routes to correct plugin
    presets.load_preset(preset_id).await?;
    transport.play().await?;

    // Services can interact across plugin boundaries
    let current_song = SongClient::new(handle.clone()).get_current_song().await?;
    if let Some(song) = current_song {
        chords.detect_chords(song.audio_data).await?;
    }
}

// Web App (via WebSocket bridge)
async function loadPreset(presetId) {
    // Same API, different transport!
    const handle = await connectWebSocket("ws://localhost:8080");
    const presets = new PresetClient(handle);
    await presets.loadPreset(presetId);
}
```

### Plugin Spawning

```rust
// In REAPER extension (ReaperHost)
impl ReaperHost {
    pub fn spawn_plugins(&mut self) -> Result<()> {
        // Spawn each plugin as a separate process
        self.spawn_plugin("RigControl", &[
            "--hub-path=/tmp/fts-reaper",
            "--peer-id=1",
        ])?;

        self.spawn_plugin("Setlist", &[
            "--hub-path=/tmp/fts-reaper",
            "--peer-id=2",
        ])?;

        self.spawn_plugin("Transport", &[
            "--hub-path=/tmp/fts-reaper",
            "--peer-id=3",
        ])?;

        self.spawn_plugin("Keyflow", &[
            "--hub-path=/tmp/fts-reaper",
            "--peer-id=4",
        ])?;

        Ok(())
    }

    fn spawn_plugin(&mut self, name: &str, args: &[&str]) -> Result<PeerId> {
        // Allocate peer slot
        let peer_id = self.allocate_peer()?;

        // Spawn process
        let child = Command::new(&format!("fts-{}-plugin", name.to_lowercase()))
            .args(args)
            .spawn()?;

        // Monitor for crashes
        self.monitor_peer(peer_id, child);

        Ok(peer_id)
    }
}
```

### Crash Isolation Example

```
User loads a preset in desktop app
→ PresetClient.load_preset()
→ SHM to ReaperHost
→ Routes to RigControlPlugin (peer_id=1)
→ PresetService.load_preset() executes

RigControlPlugin crashes!
→ ReaperHost detects death (POLLHUP on doorbell)
→ Cleans up peer_id=1 resources
→ Restarts RigControlPlugin
→ Desktop app reconnects to new instance

Transport/Setlist/Keyflow plugins unaffected!
REAPER session continues playing!
```

## Implementation Checklist

### Phase 1: Core Infrastructure
- [ ] Create `ReaperHost` (SHM host in REAPER extension)
- [ ] Create `FTSPlugin` base (SHM guest with multi-service registration)
- [ ] Implement service routing (method_id → plugin → service → dispatcher)
- [ ] Add death detection and auto-restart logic

### Phase 2: Plugin Services
- [ ] Define all service traits (Module, Preset, Profile, Setlist, Song, Transport, etc.)
- [ ] Implement `RigControlPlugin` with 4 services (Module, Preset, Profile, RigSong)
- [ ] Implement `SetlistPlugin` with 2 services (Setlist, Song)
- [ ] Implement `TransportPlugin` with 1 service (Transport)
- [ ] Implement `KeyflowPlugin` with 1 service (ChordDetection)

### Phase 3: Client Integration
- [ ] Desktop app connects as SHM guest or uses Client over SHM
- [ ] Create WebSocket bridge for web/mobile clients
- [ ] Generate TypeScript client bindings
- [ ] Unified Client API works across all transports

### Phase 4: Testing
- [ ] Test crash isolation (kill RigControl, Transport keeps running)
- [ ] Test hot reload (restart plugin mid-session)
- [ ] Test multi-client (desktop + web simultaneously)
- [ ] Test cross-plugin communication (Song → ChordDetection)
