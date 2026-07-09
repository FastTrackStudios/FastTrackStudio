//! Example integrated REAPER extension.
//!
//! Loaded directly by REAPER from `UserPlugins/`. It registers one normal
//! action and one host-managed fake-toggle action without going through
//! `daw-bridge`, sockets, or shared memory.

use std::cell::OnceCell;
use std::error::Error;

use daw_extension_runtime::{ActionDef, ExtensionRuntime};
use fragile::Fragile;
use reaper_low::PluginContext;
use reaper_macros::reaper_extension_plugin;
use tracing::{info, warn};

thread_local! {
    static APP: OnceCell<Fragile<ExampleExtension>> = const { OnceCell::new() };
}

#[derive(Debug)]
struct ExampleExtension {
    runtime: ExtensionRuntime,
}

impl ExampleExtension {
    fn new(context: PluginContext) -> eyre::Result<Self> {
        let runtime = ExtensionRuntime::new(context)?;
        let daw = runtime.build_daw()?;

        runtime.spawn(async move {
            let reg = daw_extension_runtime::register_actions(
                &daw,
                &[
                    ActionDef {
                        command_name: "FTS_GUEST_HELLO",
                        description: "FTS: Integrated Hello World",
                        toggleable: false,
                    },
                    ActionDef {
                        command_name: "FTS_GUEST_TOGGLE_EXAMPLE",
                        description: "FTS: Integrated Toggle Example",
                        toggleable: true,
                    },
                ],
            )
            .await;

            match reg {
                Ok(reg) => {
                    info!(
                        "example-extension registered actions: {} ok, {} failed",
                        reg.registered, reg.failed
                    );
                }
                Err(e) => {
                    warn!("example-extension action registration failed: {e}");
                }
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
fn plugin_main(context: PluginContext) -> Result<(), Box<dyn Error>> {
    init_tracing();
    info!("example-extension starting");

    let app = ExampleExtension::new(context)?;
    app.runtime.add_timer(timer_callback)?;

    let stored = APP.with(|cell| cell.set(Fragile::new(app)).is_ok());
    if !stored {
        return Err("example-extension already initialized".into());
    }

    info!("example-extension loaded");
    Ok(())
}

fn init_tracing() {
    let Ok(log_file) = std::fs::File::create("/tmp/example-extension.log") else {
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
