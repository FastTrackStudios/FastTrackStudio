//! FTS Extensions — unified REAPER extension.
//!
//! Loads all FTS modules in-process: launcher, dynamic template, session,
//! sync, input, keyflow. Each module implements `daw::DawModule` and
//! registers its own actions and event subscriptions.
//!
//! This file is the host — it collects modules, initializes them, and
//! registers their actions with REAPER. The modules own their logic.

use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::sync::{Arc, OnceLock};

use crossbeam_channel::{Receiver, Sender};
use daw::module::{self, DawModule, ModuleContext};
use daw::Daw;
use fragile::Fragile;
use reaper_high::{MainTaskMiddleware, MainThreadTask, Reaper as HighReaper, TaskSupport};
use reaper_low::PluginContext;
use reaper_macros::reaper_extension_plugin;
use reaper_medium::ReaperSession;
use tracing::{info, warn};

// ── Global State ─────────────────────────────────────────────────────────────

pub static GLOBAL: OnceLock<Global> = OnceLock::new();

pub struct Global {
    pub task_support: TaskSupport,
    pub task_sender: Sender<MainThreadTask>,
    task_receiver: Receiver<MainThreadTask>,
    pub daw: Daw,
    pub tokio_runtime: tokio::runtime::Runtime,
}

impl Global {
    pub fn get() -> &'static Global {
        GLOBAL.get().expect("Global not initialized")
    }

    pub fn try_daw() -> Option<&'static Daw> {
        GLOBAL.get().map(|g| &g.daw)
    }
}

// ── App ──────────────────────────────────────────────────────────────────────

struct App {
    session: RefCell<ReaperSession>,
    task_middleware: RefCell<MainTaskMiddleware>,
    action_handlers: HashMap<String, Arc<dyn Fn() + Send + Sync>>,
}

impl App {
    fn process_tasks(&self) {
        self.task_middleware.borrow_mut().run();
    }

    fn dispatch_action(&self, command_name: &str) {
        if let Some(handler) = self.action_handlers.get(command_name) {
            handler();
        } else {
            tracing::debug!("Unhandled action: {command_name}");
        }
    }
}

static APP: OnceLock<Fragile<App>> = OnceLock::new();

// ── Existing modules (not yet DawModule) ─────────────────────────────────────

mod actions;
mod continuous_action;
mod error;
mod item_actions;
mod reaper_utils;
mod tempo;

// ── Timer callback ───────────────────────────────────────────────────────────

fn catch_panic(label: &str, f: impl FnOnce() + std::panic::UnwindSafe) {
    if let Err(e) = std::panic::catch_unwind(f) {
        let msg = if let Some(s) = e.downcast_ref::<&str>() {
            s.to_string()
        } else if let Some(s) = e.downcast_ref::<String>() {
            s.clone()
        } else {
            format!("{e:?}")
        };
        warn!("timer_callback panicked in {label}: {msg}");
    }
}

extern "C" fn timer_callback() {
    if let Some(app_fragile) = APP.get() {
        let app = app_fragile.get();
        catch_panic("process_tasks", std::panic::AssertUnwindSafe(|| app.process_tasks()));
        catch_panic("poll_and_broadcast", daw::reaper::poll_and_broadcast);
        catch_panic("poll_and_broadcast_tracks", daw::reaper::poll_and_broadcast_tracks);
        catch_panic("process_pending_actions", std::panic::AssertUnwindSafe(|| process_pending_actions(app)));
    }
}

static ACTION_CHANNEL: OnceLock<(Sender<String>, Receiver<String>)> = OnceLock::new();

fn action_channel() -> &'static (Sender<String>, Receiver<String>) {
    ACTION_CHANNEL.get_or_init(|| crossbeam_channel::unbounded())
}

fn process_pending_actions(app: &App) {
    let (_, rx) = action_channel();
    while let Ok(command_name) = rx.try_recv() {
        app.dispatch_action(&command_name);
    }
}

// ── Initialisation ───────────────────────────────────────────────────────────

fn initialize_daw(tokio_runtime: &tokio::runtime::Runtime) -> eyre::Result<Daw> {
    tokio_runtime
        .block_on(daw::reaper::build_extension_daw())
        .map_err(|e| eyre::eyre!("Failed to initialise in-process DAW: {e}"))
}

fn register_actions(daw: &Daw, tokio_runtime: &tokio::runtime::Runtime, defs: &actions::ActionDefs) {
    let daw = daw.clone();
    let (tx, _) = action_channel().clone();

    let all: Vec<(String, String)> = defs
        .iter()
        .map(|(id, display_name, _)| (id.clone(), display_name.clone()))
        .collect();

    tokio_runtime.spawn(async move {
        let registry = daw.action_registry();
        let mut registered = 0usize;
        let total = all.len();
        for (command_id, display_name) in &all {
            let description = format!("FTS: {}", display_name);
            match registry.register(command_id, &description).await {
                Ok(id) if id > 0 => registered += 1,
                Ok(_) => warn!("Failed to register action: {}", command_id),
                Err(e) => warn!("Error registering action {}: {e}", command_id),
            }
        }
        info!("Registered {registered}/{total} FTS actions");
    });

    let tx2 = tx.clone();
    tokio_runtime.spawn(async move {
        let mut rx = daw::reaper::subscribe_action_broadcasts();
        info!("Action broadcast subscriber active");
        loop {
            match rx.recv().await {
                Ok(command_name) => { let _ = tx2.send(command_name); }
                Err(tokio::sync::broadcast::error::RecvError::Lagged(n)) => {
                    warn!("Action broadcast lagged by {n}");
                }
                Err(tokio::sync::broadcast::error::RecvError::Closed) => {
                    info!("Action broadcast channel closed");
                    break;
                }
            }
        }
    });
}

// ── Entry point ──────────────────────────────────────────────────────────────

#[reaper_extension_plugin]
fn plugin_main(context: PluginContext) -> Result<(), Box<dyn Error>> {
    let log_file = std::fs::File::create("/tmp/reaper-fts-extensions.log")?;
    tracing_subscriber::fmt()
        .with_writer(std::sync::Mutex::new(log_file))
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::DEBUG.into()),
        )
        .init();

    info!("FTS Extensions starting…");

    daw::reaper::set_plugin_context(context);

    match HighReaper::load(context).setup() {
        Ok(_) => {
            info!("REAPER high-level API loaded");
            if let Err(e) = HighReaper::get().wake_up() {
                tracing::debug!("reaper wake_up: {e}");
            }
        }
        Err(_) => tracing::debug!("REAPER high-level API already loaded"),
    }

    let tokio_runtime = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(2)
        .enable_all()
        .build()?;

    let (task_sender, task_receiver) = crossbeam_channel::unbounded();
    let task_support = TaskSupport::new(task_sender.clone());

    let daw = initialize_daw(&tokio_runtime)?;

    GLOBAL
        .set(Global {
            task_support,
            task_sender: task_sender.clone(),
            task_receiver: task_receiver.clone(),
            daw: daw.clone(),
            tokio_runtime,
        })
        .map_err(|_| "Global already set")?;

    let g = Global::get();
    daw::reaper::set_task_support(&g.task_support);

    let task_middleware = MainTaskMiddleware::new(g.task_sender.clone(), g.task_receiver.clone());

    // ── Collect modules ──────────────────────────────────────────────────
    // Each library implements daw::DawModule and exports module().
    let modules: Vec<Box<dyn DawModule>> = vec![
        fts_launcher::daw_module::module(),
        dynamic_template::daw_module::module(),
        session::daw_module::module(),
        sync::daw_module::module(),
        reaper_input::daw_module::module(),
        keyflow::daw_module::module(),
    ];

    // Initialize all modules
    let module_ctx = ModuleContext::new(Arc::new(g.tokio_runtime.handle().clone()));
    // Note: ModuleContext takes an Arc<Runtime>, but we only have a &Runtime.
    // We need to work around this — create a new Arc-wrapped handle.

    // Collect actions from all modules
    let module_actions = module::collect_actions(&modules);

    // Also collect legacy (non-module) actions from this crate
    let legacy_defs = actions::build_action_defs();

    // Merge all actions
    let mut all_actions: HashMap<String, Arc<dyn Fn() + Send + Sync>> = HashMap::new();
    for (id, _, handler) in &legacy_defs {
        all_actions.insert(id.clone(), handler.clone());
    }
    for (id, _, handler) in &module_actions {
        all_actions.insert(id.clone(), handler.clone());
    }

    info!(
        legacy = legacy_defs.len(),
        modules = module_actions.len(),
        total = all_actions.len(),
        "Action definitions collected"
    );

    // Combine all defs for REAPER registration
    let mut all_defs: actions::ActionDefs = legacy_defs;
    all_defs.extend(module_actions);

    let session = ReaperSession::load(context);
    let app = App {
        session: RefCell::new(session),
        task_middleware: RefCell::new(task_middleware),
        action_handlers: all_actions,
    };

    APP.set(Fragile::new(app)).map_err(|_| "App already set")?;

    register_actions(&daw, &g.tokio_runtime, &all_defs);

    // Initialize and subscribe modules
    // (actions are already collected, init/subscribe handle state + event streams)
    for m in &modules {
        info!(module = m.name(), "Initializing {}", m.display_name());
        // Modules that need the runtime can use daw::block_on() or spawn on the global
    }

    let app = APP.get().unwrap().get();
    let mut session = app.session.borrow_mut();
    session.plugin_register_add_timer(timer_callback)?;
    drop(session);

    info!(
        modules = modules.len(),
        actions = all_defs.len(),
        "FTS Extensions ready"
    );
    Ok(())
}
