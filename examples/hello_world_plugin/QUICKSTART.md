# Quick Start - Hot-Reload Demo

This guide shows you how to see hot-reloading in action in ~2 minutes!

## Step 1: Build Everything

```bash
cd examples/hello_world_plugin
cargo build
```

## Step 2: Start Auto-Rebuild (Terminal 1)

```bash
cargo watch -x 'build --lib -p hello-world-impl'
```

This watches for changes to `hello-world-impl/src/lib.rs` and rebuilds automatically.

## Step 3: Run the Plugin (Terminal 2)

```bash
cd examples/hello_world_plugin
cargo run --bin hello-world-plugin -- --standalone
```

You should see output like:

```
🚀 Hello World Plugin starting...
🔥 Running in standalone mode (hot-reload demo)
💡 Edit hello-world-impl/src/lib.rs and save to see hot-reload!

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Tick #1
📞 greeter.greet('World') = 'Hello, World! 👋'
📞 counter.increment() = 1
📞 get_version() = 'v1.0.0 - Initial version'
📞 get_emoji() = '🦀'
```

## Step 4: Edit and See Hot-Reload!

### Edit 1: Change the greeting

Open `hello-world-impl/src/lib.rs` and find this function:

```rust
#[no_mangle]
pub fn greeter_greet(name: String) -> String {
    let state = state().read();
    let greeting = &state.greeting;

    // 🔥 HOT-RELOAD ME! Change this message and save to see instant updates!
    format!("{}, {}! 👋", greeting, name)
}
```

Change it to:

```rust
    format!("{}, {}! 🎉🔥", greeting, name)  // Changed emojis!
}
```

**Save the file** and watch Terminal 1 rebuild (~1-2 seconds).

Terminal 2 will show:

```
♻️  🔥 HOT-RELOAD #1 DETECTED! 🔥
   Implementation reloaded without restarting!

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Tick #2
📞 greeter.greet('World') = 'Hello, World! 🎉🔥'  ← CHANGED!
📞 counter.increment() = 2
```

Notice:
- ✅ Greeting changed to new emojis
- ✅ Counter kept incrementing (state preserved!)
- ✅ No restart needed

### Edit 2: Change the version

Find:

```rust
#[no_mangle]
pub fn get_version() -> String {
    // 🔥 CHANGE THIS AND SAVE TO SEE HOT-RELOAD!
    "v1.0.0 - Initial version".to_string()
}
```

Change to:

```rust
    "v2.0.0 - Hot-reloaded! 🚀".to_string()
```

Save and see it update instantly!

### Edit 3: Change the emoji

Find:

```rust
#[no_mangle]
pub fn get_emoji() -> String {
    // 🔥 CHANGE THE EMOJI AND SAVE!
    "🦀".to_string()
}
```

Try different emojis: 🎸 🎹 🎵 🔥 ⚡ 🌟

Each save triggers a hot-reload in <1 second!

## What's Happening?

1. **Terminal 1** (cargo watch):
   - Watches `hello-world-impl/src/lib.rs` for changes
   - Rebuilds `libhello_world_impl.dylib` when you save
   - Takes ~1-2 seconds to compile

2. **Terminal 2** (plugin):
   - Runs the plugin with hot-lib-reloader
   - Automatically detects when `.dylib` is rebuilt
   - Saves state → Unloads old → Loads new → Restores state
   - All in ~50-100ms!

3. **Result**:
   - Code changes instantly without restart
   - State preserved (counter keeps incrementing)
   - SHM connection would stay alive (when implemented)

## Key Benefits

✅ **Sub-second reload** - See changes in ~1 second total
✅ **State preserved** - Counter value persists across reloads
✅ **No restart needed** - Plugin keeps running
✅ **Multiple REAPER instances** - All would update simultaneously
✅ **Simple workflow** - Just edit and save!

## Next Steps

Once the SHM host is implemented in REAPER extension:
- Remove `--standalone` flag
- Plugin connects to REAPER via SHM
- Hot-reload works across REAPER ↔ Plugin boundary
- Multiple REAPER instances can share one plugin

## Troubleshooting

### "error: could not compile `hello-world-impl`"

Check the error message. If it's a syntax error, fix it and save again.
The old `.dylib` is still loaded, so the plugin keeps running!

### "Library not found"

Make sure you ran `cargo build` first to create the initial `.dylib`.

### Hot-reload not detected

Make sure cargo watch is running in Terminal 1 and actually rebuilding.
You should see "Finished" messages in Terminal 1 when you save.
