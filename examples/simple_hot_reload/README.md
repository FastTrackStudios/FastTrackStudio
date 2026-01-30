# Simple Hot-Reload Demo

A minimal example demonstrating hot code reloading with `hot-lib-reloader`.

## Quick Start

### Step 1: Build
```bash
cd examples/simple_hot_reload
cargo build
```

### Step 2: Start Auto-Rebuild (Terminal 1)
```bash
cargo watch -x 'build --lib -p hot-reload-impl'
```

### Step 3: Run Demo (Terminal 2)
```bash
cargo run --bin hot-reload-demo
```

You should see output every 3 seconds like:
```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Tick #1

📞 greet('World') = "Hello, World! 👋🦀"
📞 increment() = 1
📞 get_version() = "v1.0.0 - Initial version"
📞 get_emoji() = "🦀"
```

### Step 4: Edit and See Hot-Reload!

#### Change the greeting emojis:
Open `hot-reload-impl/src/lib.rs`, find:
```rust
let result = format!("{}, {}! 👋🦀", greeting, name_str);
```

Change to:
```rust
let result = format!("{}, {}! 🎉🔥", greeting, name_str);
```

**Save** and watch Terminal 2:
```
♻️  ═══════════════════════════════════════════════════
🔥  HOT-RELOAD #1 DETECTED! 🔥
    Implementation reloaded without restarting!
    ═══════════════════════════════════════════════════

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Tick #2

📞 greet('World') = "Hello, World! 🎉🔥"  ← CHANGED!
📞 increment() = 2  ← Counter preserved!
```

## Other Changes to Try

### Change increment amount:
```rust
state.counter += 5; // Was: += 1
```

### Change version:
```rust
let version = "v2.0.0 - Hot-reloaded! 🚀";
```

### Change emoji:
```rust
let emoji = "🎸"; // Try: 🎹 🎵 🔥 ⚡ 🌟 ✨
```

## Key Benefits

✅ **Sub-second reload** - ~50-100ms reload time
✅ **State preserved** - Counter keeps incrementing
✅ **No restart needed** - Application keeps running
✅ **Simple workflow** - Just edit and save!

## How It Works

1. `hot-lib-reloader` watches `libhot_reload_impl.dylib`
2. When you save changes, cargo watch rebuilds the `.dylib`
3. `hot-lib-reloader` detects the change and reloads
4. State is saved before reload, restored after
5. Your changes are live in <1 second!

## Next Steps

This same pattern can be applied to:
- REAPER extension plugins
- Audio DSP processors
- UI components
- Game logic
- Any code you want to iterate on quickly!
