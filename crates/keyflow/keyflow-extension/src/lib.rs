//! Keyflow integrated REAPER extension.
//!
//! Loaded directly by REAPER from `UserPlugins/`. Listens for action triggers
//! and (eventually) drives chart parsing / guide track generation / MIDI export.

use std::cell::OnceCell;
use std::error::Error;

use daw::Daw;
use daw_extension_runtime::ExtensionRuntime;
use eyre::Result;
use fragile::Fragile;
use reaper_low::PluginContext;
use reaper_macros::reaper_extension_plugin;
use tracing::{info, warn};

thread_local! {
    static APP: OnceCell<Fragile<KeyflowExtension>> = const { OnceCell::new() };
}

struct KeyflowExtension {
    runtime: ExtensionRuntime,
}

impl KeyflowExtension {
    fn new(context: PluginContext) -> Result<Self> {
        let runtime = ExtensionRuntime::new(context)?;
        let daw = runtime.build_daw()?;

        runtime.spawn(async move {
            if let Err(e) = run(daw).await {
                warn!("[keyflow] event loop ended: {e}");
            }
        });

        Ok(Self { runtime })
    }

    fn timer(&self) {
        self.runtime.process_tasks();
    }
}

extern "C" fn timer_callback() {
    APP.with(|cell| {
        if let Some(app) = cell.get() {
            app.get().timer();
        }
    });
}

#[reaper_extension_plugin]
fn plugin_main(context: PluginContext) -> std::result::Result<(), Box<dyn Error>> {
    init_tracing();
    info!("keyflow-extension starting");

    let app = KeyflowExtension::new(context).map_err(|e| -> Box<dyn Error> { e.into() })?;
    app.runtime.add_timer(timer_callback).map_err(|e| -> Box<dyn Error> { e.into() })?;

    let stored = APP.with(|cell| cell.set(Fragile::new(app)).is_ok());
    if !stored {
        return Err("keyflow-extension already initialized".into());
    }

    info!("keyflow-extension loaded");
    Ok(())
}

fn init_tracing() {
    let Ok(log_file) = std::fs::File::create("/tmp/keyflow-extension.log") else {
        return;
    };
    let subscriber = tracing_subscriber::fmt()
        .with_writer(std::sync::Mutex::new(log_file))
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::INFO.into()),
        )
        .finish();
    let _ = tracing::subscriber::set_global_default(subscriber);
}

async fn run(daw: Daw) -> Result<()> {
    let pid = std::process::id();
    info!("[keyflow:{pid}] runtime started");

    // Subscribe to action trigger events.
    // Keyflow doesn't register its own actions yet — guide_track and
    // keyflow_actions will be wired in later.
    let registry = daw.action_registry();
    let mut rx = registry
        .subscribe_actions()
        .await
        .map_err(|e| eyre::eyre!("subscribe_actions: {e}"))?;
    info!("[keyflow:{pid}] subscribed to action events");

    while let Ok(Some(event)) = rx.recv().await {
        match event.get() {
            daw::service::ActionEvent::Triggered { command_name } => {
                handle_action(command_name);
            }
        }
    }

    info!("[keyflow:{pid}] action event stream ended");
    Ok(())
}

fn handle_action(command_name: &str) {
    // TODO: wire to guide track generation (keyflow::midi::guide::GuideGenerator)
    // TODO: wire to MIDI export pipeline
    // TODO: handle keyflow-specific actions once action definitions are added
    info!("[keyflow] action triggered: {command_name}");
}
