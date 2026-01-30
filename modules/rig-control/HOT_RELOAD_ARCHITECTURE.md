# Hot-Reload Architecture with SHM + Dynamic Libraries

## The Problem

We want to update plugin code without:
- ❌ Restarting REAPER
- ❌ Killing the plugin process (loses SHM connection)
- ❌ Disconnecting clients

## Recommended Crates

### 1. hot-lib-reloader ✅ (RECOMMENDED)
- **Crates.io**: [hot-lib-reloader](https://crates.io/crates/hot-lib-reloader)
- **Features**: Built-in file watching, automatic reloading, simple macro API
- **Status**: Actively maintained, widely used in gamedev
- **API**: Clean `#[hot_module]` macro

### 2. code_reload
- **Crates.io**: [code_reload](https://crates.io/crates/code_reload)
- **Features**: Simple `#[hotreload]` attribute, runtime mode
- **API**: Per-function annotation

### 3. dynamic_reload
- **Crates.io**: [dynamic_reload](https://crates.io/crates/dynamic_reload)
- **Features**: Lower-level control over reload process

**We'll use `hot-lib-reloader`** - it's the most mature and has the best API.

## The Solution: Plugin as Loader + Dynamic Implementation (using hot-lib-reloader)

```
┌─────────────────────────────────────────────────────────────┐
│ RigControlPlugin Process (stays alive)                      │
│ ┌─────────────────────────────────────────────────────────┐ │
│ │ Plugin Loader (stable, rarely changes)                  │ │
│ │ - Attaches to SHM                                       │ │
│ │ - Watches for .so file changes                          │ │
│ │ - Loads/reloads implementation                          │ │
│ │ - Routes ROAM calls to current implementation           │ │
│ └─────────────────────────────────────────────────────────┘ │
│                           │                                  │
│                           ↓ dlopen/dlsym                     │
│ ┌─────────────────────────────────────────────────────────┐ │
│ │ librig_control_impl.so (hot-reloadable)                 │ │
│ │ ┌─────────────────────────────────────────────────────┐ │ │
│ │ │ ModuleServiceImpl                                   │ │ │
│ │ │ PresetServiceImpl                                   │ │ │
│ │ │ ProfileServiceImpl                                  │ │ │
│ │ │ RigSongServiceImpl                                  │ │ │
│ │ └─────────────────────────────────────────────────────┘ │ │
│ └─────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
```

## Architecture

### 1. Plugin Loader (Stable Process) - Using hot-lib-reloader

```toml
# apps/plugins/rig-control/Cargo.toml
[package]
name = "rig-control-plugin"
version = "0.1.0"

[dependencies]
hot-lib-reloader = "0.7"
roam = { git = "https://github.com/bearcove/roam.git" }
rig-control = { path = "../../../modules/rig-control" }
```

```rust
// apps/plugins/rig-control/src/main.rs
// This binary NEVER changes (or changes rarely)

use hot_lib_reloader::{LibReloader, Symbol};

// This macro watches and auto-reloads the dylib!
#[hot_lib_reloader::hot_module(dylib = "rig_control_impl")]
mod hot_lib {
    // Re-export functions from the hot-reloadable library
    hot_functions_from_file!("rig-control-impl/src/lib.rs");

    // You can also manually declare functions:
    // #[lib_change_subscription]
    // pub fn subscribe() -> hot_lib_reloader::LibReloadObserver {}
}

struct PluginLoader {
    handle: ConnectionHandle,  // SHM connection (persists across reloads)
    state: Arc<RwLock<PluginState>>,  // Shared state
}

impl PluginLoader {
    async fn run(&mut self) -> Result<()> {
        // hot-lib-reloader handles watching and reloading automatically!

        // Optional: Get notifications when library reloads
        let reload_observer = hot_lib::subscribe();

        loop {
            select! {
                // Handle ROAM calls from host
                msg = self.handle.recv() => {
                    // Call into hot-reloadable functions
                    let modules = hot_lib::get_modules();
                    self.dispatch(msg, modules)?;
                }

                // Optional: React to reload events
                _ = reload_observer.wait_for_reload() => {
                    info!("Library reloaded! Migrating state...");
                    self.migrate_state()?;
                }
            }
        }
    }

    fn migrate_state(&mut self) -> Result<()> {
        // Optional state migration logic
        // hot-lib-reloader handles the actual reload

        // You can save/restore state if needed
        let state = self.state.read().unwrap();
        let serialized = serde_json::to_string(&*state)?;

        // After reload, restore state
        hot_lib::restore_state(&serialized);

        Ok(())
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    // Parse args
    let hub_path = std::env::args().nth(1).expect("--hub-path required");
    let peer_id: u8 = std::env::args().nth(2).expect("--peer-id required").parse()?;

    // Attach to SHM
    let guest = SHMGuest::attach(&hub_path, peer_id)?;

    // Create loader
    let mut loader = PluginLoader {
        handle: guest.handle(),
        state: Arc::new(RwLock::new(PluginState::default())),
    };

    // Run forever - hot-lib-reloader watches and reloads automatically!
    loader.run().await
}
```

### 2. Service Implementation (Hot-Reloadable Library)

```toml
# rig-control-impl/Cargo.toml
[package]
name = "rig-control-impl"
version = "0.1.0"

[lib]
# CRITICAL: Must be dylib for hot-reloading!
crate-type = ["rlib", "dylib"]

[dependencies]
rig-control = { path = "../modules/rig-control" }
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

```rust
// rig-control-impl/src/lib.rs
// This gets recompiled and reloaded frequently

use rig_control::{ModuleInfo, ModuleType, ModuleId, PresetInfo};

// ═══════════════════════════════════════════════════════════════════════════
// Hot-Reloadable Service Functions
// ═══════════════════════════════════════════════════════════════════════════

// Mark functions with #[no_mangle] and pub for hot-reloading
// hot-lib-reloader will watch and reload these automatically!

#[no_mangle]
pub fn get_modules() -> Vec<ModuleInfo> {
    // Implementation here - can be changed and reloaded!
    vec![
        ModuleInfo {
            id: ModuleId::new(),
            name: "Drive".to_string(),
            module_type: ModuleType::Drive,
        },
        ModuleInfo {
            id: ModuleId::new(),
            name: "Amp".to_string(),
            module_type: ModuleType::Amp,
        },
    ]
}

#[no_mangle]
pub fn add_module(module_type: ModuleType) -> Result<ModuleId, String> {
    // Implementation here
    println!("Adding module: {:?}", module_type);
    Ok(ModuleId::new())
}

#[no_mangle]
pub fn get_available_presets() -> Vec<PresetInfo> {
    // Implementation here
    vec![]
}

#[no_mangle]
pub fn load_preset(preset_id: uuid::Uuid) {
    // Implementation here
    println!("Loading preset: {}", preset_id);
}

// ═══════════════════════════════════════════════════════════════════════════
// State Management (Optional)
// ═══════════════════════════════════════════════════════════════════════════

use std::sync::OnceLock;
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Default)]
struct PluginState {
    current_preset: Option<uuid::Uuid>,
    current_profile: Option<uuid::Uuid>,
    modules: Vec<ModuleInfo>,
}

static STATE: OnceLock<std::sync::RwLock<PluginState>> = OnceLock::new();

fn get_state() -> &'static std::sync::RwLock<PluginState> {
    STATE.get_or_init(|| std::sync::RwLock::new(PluginState::default()))
}

#[no_mangle]
pub fn save_state() -> String {
    let state = get_state().read().unwrap();
    serde_json::to_string(&*state).unwrap()
}

#[no_mangle]
pub fn restore_state(json: &str) {
    if let Ok(new_state) = serde_json::from_str(json) {
        let mut state = get_state().write().unwrap();
        *state = new_state;
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Hot-Reload Event Handlers
// ═══════════════════════════════════════════════════════════════════════════

// Called before library is unloaded
#[no_mangle]
pub extern "C" fn on_before_reload() {
    println!("About to reload, saving state...");
    // Save state to disk or return it
}

// Called after library is loaded
#[no_mangle]
pub extern "C" fn on_after_reload() {
    println!("Reloaded! Restoring state...");
    // Restore state from disk or parameter
}
```

### 3. State Management

```rust
// Serializable state that persists across reloads
#[derive(Serialize, Deserialize)]
pub struct PluginState {
    // Current loaded data
    pub current_preset: Option<Uuid>,
    pub current_profile: Option<Uuid>,
    pub modules: Vec<ModuleInfo>,

    // In-flight requests (need to be retried or cancelled)
    pub pending_requests: Vec<PendingRequest>,

    // Cached data
    pub available_presets: Vec<PresetInfo>,
    pub available_profiles: Vec<ProfileInfo>,
}
```

## Hot-Reload Workflow with hot-lib-reloader

### Developer Workflow:

1. **Edit code** in `rig-control-impl/src/lib.rs`
2. **Save file** (Ctrl+S)
3. **Compiler watches and rebuilds** `librig_control_impl.dylib`
4. **hot-lib-reloader automatically detects and reloads!**
5. **State is preserved** via `save_state()` / `restore_state()`
6. **Client code continues working** - no reconnection needed!

### Build Setup (Simple!):

```bash
# Terminal 1: Watch and rebuild implementation (hot-lib-reloader watches this!)
cargo watch -x 'build --lib -p rig-control-impl'

# Terminal 2: Plugin loader running (includes hot-lib-reloader)
./rig-control-plugin --hub-path=/tmp/fts-reaper --peer-id=1

# That's it! hot-lib-reloader handles the rest.
# Edit code, save, see reload happen automatically in <1 second!
```

### What hot-lib-reloader Does For You:

✅ **Watches dylib file** automatically
✅ **Detects changes** via file system events
✅ **Unloads old library** safely
✅ **Loads new library** with proper cleanup
✅ **Notifies your code** via observers
✅ **Handles platform differences** (macOS codesigning, etc.)

### What You Still Need to Do:

⚠️ **State management** - Call `save_state()` before reload, `restore_state()` after
⚠️ **Design for reloadability** - Keep function signatures stable
⚠️ **Handle in-flight requests** - Gracefully handle requests during reload

## Comparison Table

| Approach | Reload Time | Works in Release | State Preservation | Client Impact | Complexity |
|----------|-------------|------------------|-------------------|---------------|------------|
| **Process Restart** | 100ms - 1s | ✅ Yes | ❌ Lost | Reconnect needed | Low |
| **subsecond** | <100ms | ❌ Debug only | ⚠️ Limited | None | Medium |
| **Manual .so Reload** | 10-100ms | ✅ Yes | ✅ Full control | None | High |
| **hot-lib-reloader** | 10-100ms | ✅ Yes | ✅ Full control | None | **Low** ✅ |

## Why hot-lib-reloader > Manual Implementation

### Manual (libloading + notify):
```rust
// You have to write:
- File watching logic (notify crate)
- Load/unload sequencing
- Error handling for invalid .so files
- Platform-specific path handling
- Symbol lookup boilerplate
- State serialization plumbing
= ~200-300 lines of complex code
```

### hot-lib-reloader:
```rust
// You just write:
#[hot_lib_reloader::hot_module(dylib = "rig_control_impl")]
mod hot_lib {
    hot_functions_from_file!("rig-control-impl/src/lib.rs");
}

// hot-lib-reloader does the rest!
= ~5 lines of simple code
```

### Additional Benefits:

✅ **Handles platform quirks** - macOS codesigning, Windows DLL locks, Linux .so paths
✅ **Safe reload sequencing** - Won't crash on mid-compilation reloads
✅ **Observer pattern** - Get notified before/after reloads
✅ **Battle-tested** - Used in production gamedev with frequent reloads
✅ **Active maintenance** - Regular updates and bug fixes

## Benefits of Dynamic .so Reload

✅ **Works in release mode** - Can use in production for live updates
✅ **Full control over state** - Design your own migration strategy
✅ **No external tooling** - Just cargo watch + notify crate
✅ **SHM connection persists** - No client reconnection needed
✅ **Clean separation** - Loader is stable, implementation is fluid
✅ **Cross-platform** - Works on Linux, macOS, Windows

## Limitations

⚠️ **Need to design for reloadability** - State must be serializable
⚠️ **Compilation time** - Still need to wait for cargo build
⚠️ **ABI stability** - Interface between loader and .so must be stable
⚠️ **In-flight requests** - Need to handle gracefully during reload

## Implementation Checklist

### Phase 1: Setup hot-lib-reloader
- [ ] Add `hot-lib-reloader = "0.7"` to plugin binary dependencies
- [ ] Create hot-reloadable library crate with `crate-type = ["rlib", "dylib"]`
- [ ] Add `#[hot_module]` macro to plugin main
- [ ] Mark service functions with `#[no_mangle]`
- [ ] Test basic reload without state preservation

### Phase 2: Integrate with SHM
- [ ] Attach plugin to SHM as guest
- [ ] Register ROAM service dispatchers
- [ ] Route ROAM calls to hot-reloadable functions
- [ ] Test calling service methods from desktop/web clients
- [ ] Verify SHM connection persists across reloads

### Phase 3: State Preservation
- [ ] Define `PluginState` serialization format (serde)
- [ ] Implement `save_state()` / `restore_state()` functions
- [ ] Use `LibReloadObserver` to save state before reload
- [ ] Test state preservation across reloads
- [ ] Handle in-flight requests during reload

### Phase 4: Developer Experience
- [ ] Set up `cargo watch -x 'build --lib -p rig-control-impl'`
- [ ] Add reload notifications (log messages, UI toast?)
- [ ] Measure reload time (should be <100ms)
- [ ] Add error handling for failed compiles (keep old .so)
- [ ] Document hot-reload workflow for team

### Phase 5: Production Readiness
- [ ] Add feature flag for hot-reload (maybe disable in release?)
- [ ] Test with multiple rapid reloads (memory leaks?)
- [ ] Add versioning to .so files (rollback support)
- [ ] Monitor reload metrics (frequency, duration, errors)
- [ ] Document state migration strategies for breaking changes

## Alternative: subsecond

If you REALLY want to use subsecond:

```rust
// Wrap all service methods with subsecond::call()
impl ModuleService for RigServicesImpl {
    async fn get_modules(&self, _cx: &roam::Context) -> Vec<ModuleInfo> {
        subsecond::call(|| {
            self.module_service.get_modules_impl()
        }).await
    }
}
```

**But** you'll hit the limitations:
- ❌ Only works in debug mode
- ❌ Struct state doesn't reload automatically
- ❌ Need Dioxus CLI tooling

## Final Recommendation

**Use `hot-lib-reloader` + SHM Architecture**

### Why hot-lib-reloader:

1. ✅ **Simple API** - `#[hot_module]` macro does everything
2. ✅ **Works in release** - Can use for production live updates
3. ✅ **Full state control** - You design migration strategy
4. ✅ **Battle-tested** - Proven in gamedev with rapid iteration
5. ✅ **Integrates perfectly** with SHM architecture
6. ✅ **Platform-aware** - Handles macOS/Windows/Linux quirks
7. ✅ **Active maintenance** - Regular updates and community

### The Complete Stack:

```
SHM (Process Isolation)
  ↓
hot-lib-reloader (Code Hot-Reload)
  ↓
ROAM Services (Uniform API)
  ↓
Multiple Transports (SHM/Network/In-Process)
```

### What You Get:

- **Process isolation** - Plugins crash independently ✅
- **Hot-reload** - Update code without restart ✅
- **State preservation** - No data loss across reloads ✅
- **Multi-client** - Desktop + web + mobile ✅
- **Transport flexibility** - SHM, network, in-process ✅
- **Simple implementation** - 5 lines vs 200+ lines ✅

**This is the holy grail of plugin architecture!**
