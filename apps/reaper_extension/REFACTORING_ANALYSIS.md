# REAPER Extension Refactoring Analysis

## Current Architecture Issues

### 1. **Massive Monolithic Files**
- `setlist_stream.rs` (1110 lines) - Does too much:
  - State management
  - Command handling  
  - Stream service creation
  - Seek/command request processing
  - Smooth seek queue management
  - Setlist state updates

### 2. **Excessive Static State**
Heavy use of `OnceLock` and static variables creates:
- Hidden dependencies
- Testing difficulties
- Thread-safety concerns
- No dependency injection

**Static variables found:**
- `LATEST_SETLIST_API`
- `SEEK_REQUEST_CHANNEL` / `SEEK_REQUEST_RECEIVER`
- `COMMAND_REQUEST_CHANNEL` / `COMMAND_REQUEST_RECEIVER`
- `REGISTERED_ACTIONS`
- `COMMAND_IDS`
- `MIDI_EDITOR_HANDLERS`
- `CHANGE_DETECTION_MIDDLEWARE`
- `PREV_ACTIVE_SLIDE_INDEX`
- `BUILD_STATS`
- `TRACK_STATE_MANAGER`

### 3. **Tight Coupling**
- Modules directly access static state from other modules
- No clear boundaries between layers
- Hard to understand data flow

### 4. **Mixed Concerns in `lib.rs`**
The `plugin_main` function does:
- Initialization
- Action registration
- Menu registration
- Change detection setup
- IROH server setup
- Timer callback registration

### 5. **No Dependency Injection**
- Impossible to mock REAPER APIs for testing
- Hard to test individual components
- No way to swap implementations

### 6. **Unclear Module Boundaries**
- `reaper_*` modules mix abstraction levels
- No clear separation between:
  - REAPER API adapters
  - Business logic
  - Infrastructure code

## Refactoring Goals

1. **Modular Architecture** - Clear separation of concerns
2. **Dependency Injection** - Make components testable
3. **Reduce Static State** - Use dependency injection instead
4. **Smaller Files** - Single responsibility per module
5. **Clear Boundaries** - Well-defined interfaces between layers
6. **Easier Testing** - Mockable dependencies

## Proposed Architecture

```
reaper_extension/
├── src/
│   ├── lib.rs                    # Plugin entry point (minimal)
│   ├── config.rs                 # Configuration
│   │
│   ├── core/                     # Core business logic
│   │   ├── mod.rs
│   │   ├── setlist.rs            # Setlist domain logic
│   │   ├── transport.rs          # Transport domain logic
│   │   └── lyrics.rs             # Lyrics domain logic
│   │
│   ├── adapters/                 # REAPER API adapters
│   │   ├── mod.rs
│   │   ├── reaper_setlist.rs     # REAPER → Setlist adapter
│   │   ├── reaper_transport.rs   # REAPER → Transport adapter
│   │   ├── reaper_markers.rs     # REAPER markers adapter
│   │   ├── reaper_tracks.rs      # REAPER tracks adapter
│   │   └── reaper_project.rs     # REAPER project adapter
│   │
│   ├── services/                 # Application services
│   │   ├── mod.rs
│   │   ├── setlist_service.rs    # Setlist state management
│   │   ├── command_service.rs    # Command execution
│   │   ├── seek_service.rs       # Seek operations
│   │   └── stream_service.rs     # Stream service creation
│   │
│   ├── infrastructure/           # Infrastructure code
│   │   ├── mod.rs
│   │   ├── action_registry.rs    # Action registration
│   │   ├── change_detection.rs    # Change detection middleware
│   │   ├── menu.rs                # Menu registration
│   │   ├── timer.rs               # Timer callbacks
│   │   └── iroh_server.rs        # IROH server setup
│   │
│   ├── features/                 # Feature modules
│   │   ├── live/                 # Live tracks feature
│   │   │   ├── mod.rs
│   │   │   ├── navigation.rs
│   │   │   ├── smooth_seek.rs
│   │   │   └── actions.rs
│   │   └── lyrics/               # Lyrics feature
│   │       ├── mod.rs
│   │       ├── read.rs
│   │       ├── write.rs
│   │       └── stream.rs
│   │
│   ├── app.rs                    # Application container
│   └── utils/                    # Utilities
│       ├── mod.rs
│       ├── color_utils.rs
│       └── tracing_config.rs
```

## Key Refactoring Steps

### Phase 1: Extract Application Container
- Create `app.rs` with `App` struct
- Move initialization logic from `lib.rs`
- Use dependency injection pattern

### Phase 2: Break Down `setlist_stream.rs`
- Extract `SetlistService` for state management
- Extract `CommandService` for command execution
- Extract `SeekService` for seek operations
- Extract `StreamService` for stream creation

### Phase 3: Reduce Static State
- Create service structs that hold state
- Pass services via dependency injection
- Keep only truly global state as static

### Phase 4: Improve Module Organization
- Move `reaper_*` modules to `adapters/`
- Create clear interfaces (traits) for adapters
- Separate business logic from REAPER API calls

### Phase 5: Add Dependency Injection
- Create `AppContext` struct
- Pass context to services
- Make services testable with trait-based design

## Benefits

1. **Testability** - Can mock REAPER APIs
2. **Maintainability** - Clear module boundaries
3. **Readability** - Smaller, focused files
4. **Extensibility** - Easy to add new features
5. **Debugging** - Clear data flow

