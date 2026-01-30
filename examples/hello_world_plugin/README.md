# Hello World Plugin - ROAM SHM + Hot-Reload Example

This example demonstrates the complete SHM + hot-reload architecture:

1. **REAPER Extension** - SHM Host
2. **hello_world_plugin** - SHM Guest with hot-reload support
3. **hello_world_impl** - Hot-reloadable service implementations

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│ REAPER Extension (SHM Host)                                 │
│ - Creates SHM segment at /tmp/fts-hello                     │
│ - Spawns hello_world_plugin as guest                        │
│ - Routes ROAM calls to correct service                      │
└──────┬──────────────────────────────────────────────────────┘
       │ SHM
   ┌───┴──────────────────────────────────────────────────────┐
   │ hello_world_plugin (Process)                             │
   │ ┌──────────────────────────────────────────────────────┐ │
   │ │ Plugin Loader (stable, uses hot-lib-reloader)        │ │
   │ │ - Attaches to SHM as guest                           │ │
   │ │ - Watches for .dylib changes                         │ │
   │ │ - Auto-reloads on changes                            │ │
   │ └──────────────────────────────────────────────────────┘ │
   │                      ↓ dlopen/dlsym                      │
   │ ┌──────────────────────────────────────────────────────┐ │
   │ │ libhello_world_impl.dylib (HOT-RELOADABLE)           │ │
   │ │ ┌──────────────────────────────────────────────────┐ │ │
   │ │ │ GreeterService - greet(), set_greeting()         │ │ │
   │ │ │ CounterService - increment(), get_count()        │ │ │
   │ │ └──────────────────────────────────────────────────┘ │ │
   │ └──────────────────────────────────────────────────────┘ │
   └──────────────────────────────────────────────────────────┘
```

## Running the Example

### Terminal 1: REAPER Extension (SHM Host)

```bash
# Build and run REAPER with the extension
just test-reaper
```

### Terminal 2: Auto-rebuild on changes

```bash
# Watch and rebuild the hot-reloadable implementation
cd examples/hello_world_plugin
cargo watch -x 'build --lib -p hello-world-impl'
```

### Terminal 3: Test the services

```bash
# Use REAPER's console or a test client
reaper> hello_world.greet("World")
# Output: "Hello, World!"

# Edit hello_world_impl/src/lib.rs to change the greeting
# Save the file
# Watch it hot-reload in <1 second!

reaper> hello_world.greet("World")
# Output: "Hola, World!" (or whatever you changed it to)
```

## File Structure

```
examples/hello_world_plugin/
├── README.md
├── Cargo.toml (workspace)
│
├── services/               # ROAM service definitions
│   ├── Cargo.toml
│   └── src/
│       └── lib.rs          # GreeterService, CounterService traits
│
├── hello-world-impl/       # Hot-reloadable implementation
│   ├── Cargo.toml          # crate-type = ["rlib", "dylib"]
│   └── src/
│       └── lib.rs          # Service implementations with #[no_mangle]
│
└── hello-world-plugin/     # Plugin binary (SHM guest)
    ├── Cargo.toml
    └── src/
        └── main.rs         # Uses hot-lib-reloader + connects to SHM
```

## Hot-Reload Demo

1. **Start everything** (REAPER + cargo watch)
2. **Edit** `hello-world-impl/src/lib.rs`
3. **Change** the greeting message
4. **Save** the file
5. **See** the change instantly in REAPER!

**Reload time:** ~50-100ms

## Key Benefits

✅ **Multiple REAPER instances** can share one plugin process
✅ **Hot-reload** updates ALL connected instances simultaneously
✅ **State preserved** across reloads (counter value persists)
✅ **No reconnection** needed - SHM stays connected
✅ **Simple development** - just edit and save!
