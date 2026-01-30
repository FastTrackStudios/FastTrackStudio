# Rig-Control Anti-Patterns Analysis

## 1. ❌ LocalClient Pattern is Redundant

**Location:** `src/local_client.rs`, `define_local_client!` macro

**Issue:** ROAM already generates optimized clients. The `LocalClient` wrapper adds unnecessary abstraction.

**ROAM's built-in pattern:**
```rust
// ROAM generates this automatically:
let connection = roam::local::LocalConnection::new(service);
let client = RigControlServiceClient::new(connection);
```

**Current anti-pattern:**
```rust
// Redundant wrapper that just calls service with dummy context:
pub struct LocalRigControlClient<S> {
    service: Arc<S>,
}

impl LocalRigControlClient<S> {
    pub async fn execute(&self, cmd: RigControlCommand) {
        self.service.execute(&dummy_context(), cmd).await
    }
}
```

**Recommendation:** Delete `local_client.rs` and `define_local_client!` macro. Use ROAM's generated clients directly:
```rust
// Good:
let service = Arc::new(MockRigControlService::new());
let handle = roam::local::spawn(service);
let client = RigControlServiceClient::new(handle);
```

---

## 2. ❌ Service Layer Mixing Concerns

**Location:** `src/service.rs` - The `RigControlService` trait

**Issues:**
1. **Too many responsibilities** - Single service handles:
   - Engine state queries
   - Profile/preset/song/setlist management
   - Commands
   - Event subscriptions

2. **Violation of Interface Segregation Principle** - UI components that only care about presets must pull in the entire service.

**Current:**
```rust
#[roam::service]
pub trait RigControlService {
    async fn get_engine_state(&self) -> EngineStateInfo;
    async fn get_available_profiles(&self) -> Vec<ProfileInfo>;
    async fn get_available_presets(&self) -> Vec<PresetInfo>;
    async fn get_available_setlists(&self) -> Vec<SetlistInfo>;
    async fn get_setlist_songs(&self) -> Vec<SongInfo>;
    async fn execute(&self, cmd: RigControlCommand);
    async fn subscribe(&self, events: Tx<RigControlEvent>);
    async fn subscribe_slots(&self, ...);
}
```

**Recommendation:** Split into focused services:
```rust
#[roam::service]
pub trait RigEngineService {
    async fn get_engine_state(&self) -> EngineStateInfo;
    async fn get_slot_state(&self, module_type: ModuleType) -> Option<SlotStateInfo>;
    async fn execute_engine_command(&self, cmd: EngineCommand);
    async fn subscribe_engine_events(&self, events: Tx<EngineEvent>);
}

#[roam::service]
pub trait PresetLibraryService {
    async fn get_available_presets(&self) -> Vec<PresetInfo>;
    async fn get_preset(&self, id: Uuid) -> Option<PresetInfo>;
    async fn load_preset(&self, id: Uuid, scene_index: usize);
}

#[roam::service]
pub trait PerformanceService {
    async fn get_available_setlists(&self) -> Vec<SetlistInfo>;
    async fn get_setlist_songs(&self) -> Vec<SongInfo>;
    async fn get_current_song(&self) -> Option<SongInfo>;
    async fn go_to_scene(&self, scene_index: usize);
}
```

---

## 3. ⚠️ Director May Be a God Object

**Location:** `src/director.rs`

**Issue:** The `RigDirector` coordinates multiple rigs, assigns roles, manages views, handles sends, and resolves priorities. This could become a god object.

**Current scope:**
- Multi-rig management
- Role assignment (Lead/Background/Custom)
- View management across rigs
- Send/return routing
- Priority resolution
- Preset/scene coordination

**Recommendation:**
- If this is purely for multi-guitar setups, consider renaming to `MultiRigCoordinator`
- Consider splitting role management from routing concerns
- Ensure it doesn't handle engine lifecycle - that should stay in `RigEngine`

---

## 4. ❌ Global Signals in UI Module

**Location:** `src/ui/signals.rs`

**Issue:** Using global signals creates hidden dependencies and makes components hard to test.

**Current:**
```rust
pub static RIG_PROFILE: GlobalSignal<Option<ProfileInfo>> = Signal::global(|| None);
pub static RIG_CURRENT_PRESET: GlobalSignal<Option<PresetInfo>> = Signal::global(|| None);
// ... 13 global signals
```

**Problems:**
1. Hidden state - components don't declare dependencies
2. Hard to test - global mutable state
3. Race conditions - multiple subscribers can conflict
4. Unclear ownership - who writes? who reads?

**Recommendation:** Use context + local signals:
```rust
#[derive(Clone)]
pub struct RigState {
    pub profile: Signal<Option<ProfileInfo>>,
    pub current_preset: Signal<Option<PresetInfo>>,
    // ...
}

#[component]
pub fn RigStateProvider(children: Element) -> Element {
    let state = use_signal(RigState::default);
    use_context_provider(|| state);

    // Subscribe to service and update local signals
    use_rig_subscription(state);

    children
}

#[component]
pub fn PresetSelector() -> Element {
    let state = use_context::<Signal<RigState>>();
    let current_preset = state.read().current_preset();
    // ...
}
```

---

## 5. ⚠️ Commands as Enum May Not Scale

**Location:** `src/service.rs` - `RigControlCommand` enum

**Issue:** All commands in one enum forces recompilation when adding new commands.

**Current:**
```rust
pub enum RigControlCommand {
    Initialize { rig_id: Uuid },
    LoadPreset { preset_id: Uuid },
    LoadPresetWithScene { preset_id: Uuid, scene_index: usize },
    LoadProfile { profile_id: Uuid },
    // ... 20+ variants
}
```

**Recommendation:** If service is split (see #2), commands stay focused. Otherwise, consider command pattern with traits:
```rust
#[roam::service]
pub trait RigControlService {
    async fn execute<C: Command>(&self, cmd: C) -> C::Output;
}

pub trait Command {
    type Output;
}

pub struct LoadPreset {
    pub preset_id: Uuid,
    pub scene_index: usize,
}

impl Command for LoadPreset {
    type Output = Result<(), EngineError>;
}
```

---

## 6. ✅ Good Patterns Found

1. **Typed IDs** - `PresetId`, `RigId`, etc. prevent UUID mix-ups
2. **Typestate Pattern** - `ActivePreset<Unresolved|Resolved>` enforces scene selection
3. **Non-empty collections** - `NonEmptyVec<T>` makes invalid states unrepresentable
4. **Facet for serialization** - Clean, derive-based approach
5. **Domain-driven modules** - Clear separation of block, module, preset, profile
6. **Builder pattern for presets** - Enforces required fields at compile time
7. **ROAM for RPC** - Modern, efficient RPC framework

---

## Summary

### High Priority Fixes:
1. ❌ **Remove `LocalClient` pattern** - Use ROAM's built-in clients
2. ❌ **Split `RigControlService`** - Too many responsibilities
3. ❌ **Replace global signals** - Use context + local state

### Medium Priority:
4. ⚠️ **Review Director scope** - Ensure it doesn't become a god object
5. ⚠️ **Consider command pattern** - If command enum becomes unwieldy

### Low Priority:
6. Consider extracting performance/setlist into separate crate if it grows
