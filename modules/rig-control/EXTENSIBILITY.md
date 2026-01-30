# RigControl Service Extensibility Patterns

## The Question

How do we support unknown service implementations at compile time while still using ROAM?

## Three Approaches

### Approach 1: Static Enum (Current)

**When to use:** You know all implementations at compile time

```rust
pub enum RigService {
    Local(Arc<MockRigControlService>),
    // Future: Add new variants as needed
    // Shm(ConnectionHandle),
    // Network(ReconnectingClient),
}
```

**Pros:**
- ✅ Zero overhead (static dispatch)
- ✅ Exhaustive match checking (compiler catches missing cases)
- ✅ Simple and explicit

**Cons:**
- ❌ Must modify this crate to add new implementations
- ❌ All implementations must be known at compile time
- ❌ Can't load plugins dynamically

**Performance:** Best (0 overhead)

---

### Approach 2: Trait Objects with Wrapper (Extensible)

**When to use:** Other crates need to add implementations

```rust
// In rig-control crate
pub trait RigControlServiceExt: Send + Sync {
    fn get_current_preset<'a>(&'a self, cx: &'a roam::Context)
        -> Pin<Box<dyn Future<Output = Option<PresetInfo>> + Send + 'a>>;
}

impl<T: RigControlService> RigControlServiceExt for T { ... }

pub struct DynRigService {
    inner: Box<dyn RigControlServiceExt>,
}

// In third-party crate
struct MyCustomService;
impl RigControlService for MyCustomService { ... }

// Works automatically!
let service = DynRigService::new(MyCustomService);
```

**Pros:**
- ✅ Third-party crates can add implementations
- ✅ Can select implementation at runtime
- ✅ Can load plugins dynamically
- ✅ Blanket impl makes it work with any ROAM service

**Cons:**
- ❌ Small overhead (boxing futures + vtable)
- ❌ More complex type signatures
- ❌ No exhaustive checking

**Performance:** ~10-20ns overhead per call

---

### Approach 3: Generic (Zero-overhead extensibility)

**When to use:** Known at compile time but want flexibility

```rust
pub struct RigService<S: RigControlService> {
    inner: Arc<S>,
}

impl<S: RigControlService> RigService<S> {
    pub async fn get_current_preset(&self) -> Option<PresetInfo> {
        let cx = dummy_context();
        self.inner.get_current_preset(&cx).await
    }
}

// Usage
let service = RigService::new(MockRigControlService::new());
let service = RigService::new(LiveRigControlService::new());
let service = RigService::new(CustomService::new()); // From other crate!
```

**Pros:**
- ✅ Zero overhead (monomorphization)
- ✅ Third-party crates can add implementations
- ✅ Type-safe

**Cons:**
- ❌ Can't change implementation at runtime
- ❌ Can't store different implementations in same collection
- ❌ Type complexity propagates everywhere

**Performance:** Best (0 overhead)

---

## Recommendation for FastTrackStudio

### Use **Approach 2 (Trait Objects)** because:

1. **Plugin Architecture**: You want other crates to add service implementations
2. **Runtime Selection**: Desktop app might connect to REAPER via SHM OR run locally
3. **Future-Proof**: Can add WebAssembly plugins, remote services, etc.
4. **Acceptable Overhead**: 10-20ns is nothing for rig control (not audio DSP)

### Implementation Strategy

```rust
// In rig-control/src/service.rs (already exists)
#[roam::service]
pub trait RigControlService { ... }

// In rig-control/src/extensible_service.rs (created)
pub trait RigControlServiceExt { ... }  // Object-safe wrapper
impl<T: RigControlService> RigControlServiceExt for T { ... }  // Blanket impl
pub struct DynRigService { ... }  // Uses Box<dyn RigControlServiceExt>

// In rig-control/src/ui/context/rig.rs (update)
pub type RigService = DynRigService;  // Use dynamic version
```

### Usage Examples

```rust
// Built-in implementations (in rig-control crate)
let service = RigService::new(MockRigControlService::with_guitar_defaults());
let service = RigService::new(LiveRigControlService::new());

// Third-party implementations (in other crates)
// Example: A community plugin for controlling hardware
use my_hardware_plugin::HardwareRigService;
let service = RigService::new(HardwareRigService::connect("192.168.1.100")?);

// Example: A cloud sync service
use my_cloud_plugin::CloudRigService;
let service = RigService::new(CloudRigService::new(api_key));

// Example: A preset marketplace service
use preset_market::MarketplaceRigService;
let service = RigService::new(MarketplaceRigService::new(user_id));
```

### The Power of This Pattern

Other developers can create crates that:
- Implement `RigControlService` trait
- Automatically work with all UI components
- No modification to rig-control crate needed!

```rust
// In third-party crate: my-custom-rig-plugin
pub struct MyRigService;

impl RigControlService for MyRigService {
    async fn get_current_preset(&self, _cx: &roam::Context) -> Option<PresetInfo> {
        // Custom implementation
    }
    // ... implement all methods
}

// Users can just drop it in:
let service = RigService::new(MyRigService);
// It just works!™
```

## Performance Comparison

| Approach | Call Overhead | Compile Time | Binary Size | Runtime Flexibility |
|----------|---------------|--------------|-------------|---------------------|
| Enum | 0ns | Fast | Small | Medium |
| Trait Object | ~10-20ns | Fast | Small | High |
| Generic | 0ns | Slow | Large | Low |

**For rig control operations:**
- Preset switching: ~1ms total (10ns overhead = 0.001%)
- Parameter updates: ~100µs total (10ns overhead = 0.01%)
- Even for 1000 calls/sec: 10µs total overhead

The overhead is **completely negligible**.

## Migration Path

### Phase 1: Add extensibility without breaking changes
```rust
// Keep existing RigService enum
pub enum RigService { ... }

// Add new DynRigService for extensibility
pub struct DynRigService { ... }
```

### Phase 2: Deprecate enum, use DynRigService everywhere
```rust
#[deprecated(note = "Use DynRigService for better extensibility")]
pub enum RigService { ... }

pub type RigService = DynRigService;  // Type alias for migration
```

### Phase 3: Remove old enum
```rust
// Only DynRigService remains
pub struct DynRigService { ... }
```

## Real-World Plugin Examples

### Audio Plugin (Zero-overhead needed)
```rust
// Still use static dispatch for audio thread
impl RigControlService for AudioPluginService {
    async fn get_current_preset(&self, _cx: &roam::Context) -> Option<PresetInfo> {
        // Direct memory access, no allocation
    }
}

// In audio thread: Use concrete type (no boxing)
let service = AudioPluginService::new();
service.get_current_preset(&cx).await;

// In UI thread: Use dynamic dispatch
let dyn_service = DynRigService::new(service.clone());
```

### Cloud Preset Service (Community Plugin)
```rust
// Third-party crate: fts-cloud-presets
pub struct CloudPresetService {
    client: reqwest::Client,
    api_key: String,
}

impl RigControlService for CloudPresetService {
    async fn get_available_presets(&self, _cx: &roam::Context) -> Vec<PresetInfo> {
        // Fetch from cloud
        self.client.get("https://api.presets.com/user/presets")
            .bearer_auth(&self.api_key)
            .send().await?
            .json().await?
    }
}

// Users install the crate and it just works:
let service = RigService::new(CloudPresetService::new(api_key));
```

### Hardware Controller (Community Plugin)
```rust
// Third-party crate: fts-helix-controller
pub struct HelixControllerService {
    midi_port: MidiOutputPort,
}

impl RigControlService for HelixControllerService {
    async fn execute(&self, _cx: &roam::Context, command: RigControlCommand) {
        // Send MIDI CC to Line 6 Helix
        match command {
            RigControlCommand::LoadPreset { preset_id } => {
                self.send_program_change(preset_id);
            }
            // ...
        }
    }
}

// Users connect their Helix and it works:
let service = RigService::new(HelixControllerService::connect("MIDI 1")?);
```

## Conclusion

**Use Approach 2 (Trait Objects with Wrapper)** for RigControl because:

1. ✅ **Extensibility**: Other crates can add implementations
2. ✅ **Runtime Flexibility**: Choose implementation at startup
3. ✅ **Performance**: 10-20ns overhead is negligible for control operations
4. ✅ **ROAM Compatible**: Blanket impl makes any ROAM service work
5. ✅ **Community**: Opens ecosystem for plugins and extensions

The small performance cost (which is truly tiny) is worth the massive increase in flexibility and extensibility.
