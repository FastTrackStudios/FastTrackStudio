# TODO: Add SHM Host to REAPER Extension

Once the hello-world-plugin standalone mode is working, we need to add SHM host support to the REAPER extension.

## Changes Needed in `apps/reaper_extension/`

### 1. Add dependencies

```toml
# apps/reaper_extension/Cargo.toml
[dependencies]
roam-shm = { git = "https://github.com/bearcove/roam.git", rev = "1407c929" }
```

### 2. Create SHM host module

```rust
// apps/reaper_extension/src/shm_host.rs

use roam_shm::Host;
use std::path::PathBuf;

pub struct ReaperShmHost {
    host: Host,
    hub_path: PathBuf,
}

impl ReaperShmHost {
    pub fn new(hub_path: PathBuf) -> Result<Self> {
        // Create SHM segment
        let config = HostConfig {
            max_guests: 10,
            max_payload_size: 1024 * 1024, // 1MB
            initial_credit: 64 * 1024,     // 64KB
            ring_size: 256,
            // ...
        };

        let host = Host::create(&hub_path, config)?;

        Ok(Self { host, hub_path })
    }

    pub fn spawn_plugin(&mut self, plugin_name: &str) -> Result<PeerId> {
        // Allocate peer slot
        let peer_id = self.host.allocate_peer()?;

        // Spawn plugin process
        let child = std::process::Command::new(plugin_name)
            .arg("--hub-path").arg(&self.hub_path)
            .arg("--peer-id").arg(peer_id.to_string())
            .spawn()?;

        // Monitor for crashes
        self.host.monitor_peer(peer_id, child);

        Ok(peer_id)
    }
}
```

### 3. Initialize in extension startup

```rust
// apps/reaper_extension/src/lib.rs

static SHM_HOST: OnceLock<ReaperShmHost> = OnceLock::new();

#[no_mangle]
pub extern "C" fn ReaperPluginEntry(h_instance: HINSTANCE, rec: *mut reaper_plugin_info_t) -> c_int {
    // ... existing code ...

    // Create SHM host
    let hub_path = std::env::temp_dir().join("fts-reaper");
    let host = ReaperShmHost::new(hub_path).expect("Failed to create SHM host");
    SHM_HOST.set(host).expect("SHM host already initialized");

    // Spawn plugins
    let host = SHM_HOST.get().unwrap();
    host.spawn_plugin("hello-world-plugin").expect("Failed to spawn plugin");

    // ... existing code ...
}
```

### 4. Handle plugin lifecycle

```rust
// Clean up on extension unload
#[no_mangle]
pub extern "C" fn ReaperPluginExit() {
    if let Some(host) = SHM_HOST.get() {
        host.shutdown();
    }
}
```

## Plugin Binary Location

The REAPER extension needs to find the plugin binary. Options:

1. **Bundle with extension** - Copy to `UserPlugins/FTS/plugins/`
2. **System PATH** - Install to `/usr/local/bin` or similar
3. **Relative to extension** - Look in `../plugins/` directory

Recommended: Option 1 (bundle)

```rust
fn get_plugin_path(plugin_name: &str) -> PathBuf {
    let extension_dir = std::env::current_exe()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf();

    extension_dir.join("plugins").join(plugin_name)
}
```

## Testing

1. Build extension with SHM support
2. Build hello-world-plugin
3. Copy plugin to `UserPlugins/FTS/plugins/`
4. Launch REAPER
5. Plugin should auto-start and connect via SHM
6. Edit plugin code and save
7. See hot-reload without restarting REAPER!

## Next Steps

1. ✅ Get standalone hot-reload working first
2. ⏳ Add roam-shm dependency to extension
3. ⏳ Implement SHM host in extension
4. ⏳ Update plugin to connect to SHM (remove `--standalone`)
5. ⏳ Test full hot-reload across SHM boundary
6. ⏳ Document plugin installation and deployment
