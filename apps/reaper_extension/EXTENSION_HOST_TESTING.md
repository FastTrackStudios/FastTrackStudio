# Testing SHM Plugin Host in REAPER Extension

The REAPER extension now includes a Plugin Host that spawns and manages SHM guest plugins.

## How It Works

1. **On Extension Load** (`plugin_main()`):
   - Creates `PluginHost` with SHM segment at `/tmp/fts-reaper-shm`
   - Looks for plugin directory at `target/debug`
   - Attempts to load `hello-world-plugin` binary

2. **Plugin Spawning**:
   - Host spawns plugin process with spawn ticket args:
     ```bash
     ./hello-world-plugin \
         --hub-path /tmp/fts-reaper-shm \
         --peer-id 1 \
         --doorbell-fd 3
     ```

3. **Guest Attachment**:
   - Plugin validates spawn ticket
   - Plugin attaches to SHM segment
   - Plugin runs event loop

## Testing

### Step 1: Build the hello-world plugin

```bash
cargo build --bin hello-world-plugin
```

This creates `target/debug/hello-world-plugin`.

### Step 2: Build the REAPER extension

```bash
cargo build -p reaper_extension
```

### Step 3: Install the extension

On macOS:
```bash
# The extension is built to target/debug/libreaper_extension.dylib
# Symlink it to REAPER's UserPlugins directory (without 'lib' prefix)
ln -sf "$(pwd)/target/debug/libreaper_extension.dylib" \
    "$HOME/Library/Application Support/REAPER/UserPlugins/reaper_extension.dylib"
```

On Linux:
```bash
ln -sf "$(pwd)/target/debug/libreaper_extension.so" \
    "$HOME/.config/REAPER/UserPlugins/reaper_extension.so"
```

### Step 4: Run REAPER with logging

```bash
# Set RUST_LOG to see plugin host logs
RUST_LOG=reaper_extension=debug,info reaper
```

### Step 5: Check the logs

You should see in the console/terminal:

```
INFO reaper_extension::app: Initializing SHM Plugin Host
INFO reaper_extension::app:   Plugin directory: target/debug
INFO reaper_extension::app:   SHM path: /tmp/fts-reaper-shm
INFO reaper_extension::plugin_host::shm_hub: Creating SHM hub at: /tmp/fts-reaper-shm
INFO reaper_extension::app: ✅ Plugin Host initialized successfully
INFO reaper_extension::app: Loading SHM plugins...
INFO reaper_extension::app: Found hello-world plugin: target/debug/hello-world-plugin
INFO reaper_extension::plugin_host: Loading plugin: target/debug/hello-world-plugin
INFO reaper_extension::plugin_host:   Assigned peer ID: 1
INFO reaper_extension::plugin_host:   Plugin attached successfully
INFO reaper_extension::plugin_host: Plugin loaded successfully: hello-world-plugin
INFO reaper_extension::app: ✅ hello-world plugin loaded successfully
```

### Step 6: Verify the plugin is running

Check running processes:

```bash
ps aux | grep hello-world-plugin
```

You should see:
```
./target/debug/hello-world-plugin --hub-path /tmp/fts-reaper-shm --peer-id 1 --doorbell-fd 3
```

## What You Should See

### In REAPER Terminal/Console:

```
🏠 REAPER Extension loading...
INFO reaper_extension::app: Initializing SHM Plugin Host
INFO reaper_extension::app:   Plugin directory: target/debug
INFO reaper_extension::app:   SHM path: /tmp/fts-reaper-shm
INFO reaper_extension::plugin_host::shm_hub: Creating SHM hub
INFO reaper_extension::app: ✅ Plugin Host initialized successfully
INFO reaper_extension::app: Loading SHM plugins...
INFO reaper_extension::app: Found hello-world plugin: target/debug/hello-world-plugin
INFO reaper_extension::plugin_host: Loading plugin: target/debug/hello-world-plugin
INFO reaper_extension::plugin_host::shm_hub: Spawning guest: target/debug/hello-world-plugin (peer_id=1)
INFO reaper_extension::plugin_host: Guest spawned: PID 12345
INFO reaper_extension::plugin_host:   Plugin attached successfully
INFO reaper_extension::app: ✅ hello-world plugin loaded successfully
```

### In Plugin Output (if you tail its logs):

```
🚀 Hello World Plugin starting
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
📋 Spawn Ticket:
   Hub path: /tmp/fts-reaper-shm
   Peer ID: 1
   Doorbell FD: 3
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

✅ Validating spawn ticket...
   ✓ Peer ID valid: 1
   ✓ Hub path exists: /tmp/fts-reaper-shm
   ✓ Doorbell FD valid: 3

📡 Attaching to SHM segment...
   Hub: /tmp/fts-reaper-shm
   1. Opening shared memory segment
   2. Validating magic and version
   3. Finding peer table entry for peer 1
   4. Atomically claiming peer slot (CAS)
   5. Incrementing epoch

✅ Attached successfully as peer 1

💚 Plugin ready - peer 1 operational
   Press Ctrl+C to shutdown

[Tick 1] Heartbeat from peer 1
[Tick 2] Heartbeat from peer 1
...
```

## Troubleshooting

### Plugin not found
```
ℹ hello-world plugin not found at: target/debug/hello-world-plugin
  Build it with: cargo build --bin hello-world-plugin
```

**Solution**: Build the plugin first.

### Failed to spawn guest
```
❌ Failed to load hello-world plugin: Failed to spawn plugin: target/debug/hello-world-plugin
```

**Solution**:
- Check file permissions: `chmod +x target/debug/hello-world-plugin`
- Check the binary exists: `ls -la target/debug/hello-world-plugin`

### SHM segment creation failed
```
❌ Failed to initialize Plugin Host: Failed to create SHM segment
```

**Solution**:
- Check `/tmp` is writable
- Remove stale SHM file: `rm /tmp/fts-reaper-shm`

## Next Steps

Once this is working:

1. **Add ROAM Services**: Replace simulated attachment with real ROAM SHM transport
2. **Implement Hot-Reload**: Watch plugin binary for changes and respawn
3. **Create Rig Control Plugin**: Real plugin with ROAM services for rig control
4. **State Preservation**: Save/restore state across hot-reloads
5. **Plugin Discovery**: Auto-discover plugins from `~/.fts/plugins/`

## Architecture

```
┌──────────────────────────────────────────────────────────────┐
│ REAPER Process                                               │
│ ┌──────────────────────────────────────────────────────────┐ │
│ │ REAPER Extension (SHM Host)                              │ │
│ │ ├─ PluginHost::new("/tmp/fts-reaper-shm", "target/debug")│ │
│ │ ├─ ShmHub::create()                                      │ │
│ │ └─ load_plugin("hello-world-plugin")                     │ │
│ └──────────────────────────────────────────────────────────┘ │
└──────────────────────────────────────────────────────────────┘
                         │
                         │ spawn with ticket
                         ▼
┌──────────────────────────────────────────────────────────────┐
│ Plugin Process (PID 12345)                                   │
│ ┌──────────────────────────────────────────────────────────┐ │
│ │ hello-world-plugin                                       │ │
│ │ ├─ Args: --hub-path /tmp/fts-reaper-shm                 │ │
│ │ ├─       --peer-id 1                                     │ │
│ │ ├─       --doorbell-fd 3                                 │ │
│ │ ├─ validate_spawn_ticket()                               │ │
│ │ ├─ attach_to_shm()                                       │ │
│ │ └─ run_event_loop()                                      │ │
│ └──────────────────────────────────────────────────────────┘ │
└──────────────────────────────────────────────────────────────┘
```
