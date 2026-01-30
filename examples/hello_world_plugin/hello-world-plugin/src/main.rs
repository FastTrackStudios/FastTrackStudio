//! Hello World Plugin - SHM Guest with Hot-Reload Support
//!
//! This plugin:
//! 1. Connects to REAPER extension via SHM as a guest
//! 2. Exposes ROAM services (Greeter, Counter, Session)
//! 3. Hot-reloads service implementations without reconnecting

use clap::Parser;
use hot_lib_reloader::{LibReloader, Symbol};
use std::sync::Arc;
use tracing::{info, warn};

// ═══════════════════════════════════════════════════════════════════════════
// Hot Module - Auto-reloads implementation library
// ═══════════════════════════════════════════════════════════════════════════

#[hot_lib_reloader::hot_module(dylib = "hello_world_impl")]
mod hot_impl {
    // These function signatures must stay stable!
    // The implementations can change and will be hot-reloaded.

    hot_functions_from_file!("hello-world-impl/src/lib.rs");

    // Alternative: Manually declare functions
    // #[lib_change_subscription]
    // pub fn subscribe() -> hot_lib_reloader::LibReloadObserver {}
}

// ═══════════════════════════════════════════════════════════════════════════
// CLI Args
// ═══════════════════════════════════════════════════════════════════════════

#[derive(Parser, Debug)]
#[command(name = "hello-world-plugin")]
#[command(about = "Hello World Plugin with SHM + Hot-Reload")]
struct Args {
    /// Path to SHM hub segment
    #[arg(long, default_value = "/tmp/fts-hello")]
    hub_path: String,

    /// Peer ID assigned by host
    #[arg(long, default_value = "1")]
    peer_id: u8,

    /// Run in standalone mode (no SHM, for testing)
    #[arg(long)]
    standalone: bool,
}

// ═══════════════════════════════════════════════════════════════════════════
// Plugin Main Loop
// ═══════════════════════════════════════════════════════════════════════════

struct PluginRunner {
    args: Args,
    reload_count: usize,
}

impl PluginRunner {
    fn new(args: Args) -> Self {
        Self {
            args,
            reload_count: 0,
        }
    }

    async fn run(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        info!("🚀 Hello World Plugin starting...");
        info!("   Hub path: {}", self.args.hub_path);
        info!("   Peer ID: {}", self.args.peer_id);
        info!("   Standalone mode: {}", self.args.standalone);

        if self.args.standalone {
            self.run_standalone().await
        } else {
            self.run_with_shm().await
        }
    }

    /// Standalone mode - just demonstrate hot-reload
    async fn run_standalone(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        info!("🔥 Running in standalone mode (hot-reload demo)");
        info!("💡 Edit hello-world-impl/src/lib.rs and save to see hot-reload!");
        info!("");

        // Subscribe to reload events
        let reload_observer = hot_impl::subscribe();

        // Demo loop
        let mut tick = 0;
        loop {
            tokio::time::sleep(tokio::time::Duration::from_secs(3)).await;

            tick += 1;
            info!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
            info!("Tick #{}", tick);

            // Call hot-reloadable functions
            let greeting = hot_impl::greeter_greet("World".to_string());
            let count = hot_impl::counter_increment();
            let version = hot_impl::get_version();
            let emoji = hot_impl::get_emoji();

            info!("📞 greeter.greet('World') = '{}'", greeting);
            info!("📞 counter.increment() = {}", count);
            info!("📞 get_version() = '{}'", version);
            info!("📞 get_emoji() = '{}'", emoji);

            // Check for reloads
            if reload_observer.check_for_reload() {
                self.reload_count += 1;
                info!("");
                info!("♻️  🔥 HOT-RELOAD #{} DETECTED! 🔥", self.reload_count);
                info!("   Implementation reloaded without restarting!");
                info!("");

                // Save state before reload
                let state_json = hot_impl::save_state();

                // Wait for reload to complete
                reload_observer.wait_for_reload().await;

                // Restore state after reload
                hot_impl::restore_state(&state_json);
            }

            info!("");
        }
    }

    /// SHM mode - connect to REAPER extension
    async fn run_with_shm(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        warn!("🚧 SHM mode not yet implemented - use --standalone for now");
        warn!("   (Will implement after REAPER extension SHM host is ready)");

        // TODO: Implement SHM guest connection
        // let guest = roam_shm::Guest::attach(&self.args.hub_path, self.args.peer_id)?;
        // let handle = guest.handle();
        //
        // // Register service dispatchers
        // guest.register_service(GreeterDispatcher::new(/* hot impl */));
        // guest.register_service(CounterDispatcher::new(/* hot impl */));
        // guest.register_service(SessionDispatcher::new(/* hot impl */));
        //
        // // Run event loop
        // loop {
        //     select! {
        //         msg = handle.recv() => {
        //             // Route to hot-reloadable functions
        //         }
        //         _ = reload_observer.wait_for_reload() => {
        //             // Handle hot-reload
        //         }
        //     }
        // }

        Ok(())
    }
}

// ═══════════════════════════════════════════════════════════════════════════
// Main Entry Point
// ═══════════════════════════════════════════════════════════════════════════

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Setup logging
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "hello_world_plugin=info".into()),
        )
        .init();

    // Parse args
    let args = Args::parse();

    // Create and run plugin
    let mut runner = PluginRunner::new(args);
    runner.run().await?;

    Ok(())
}
