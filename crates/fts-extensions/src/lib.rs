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
    pub tokio_runtime: Arc<tokio::runtime::Runtime>,
    _log_guard: tracing_appender::non_blocking::WorkerGuard,
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
mod menu;
mod reaper_utils;
mod tempo;
mod ui_test_panel;

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
        catch_panic("update_panels", reaper_dioxus::update_panels);
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

/// Register all actions synchronously on the main thread (like helgobox).
///
/// This ensures command IDs are available immediately for the menu hook
/// and action list. Actions are registered via `reaper_high::Reaper::register_action`,
/// and gaccels are added manually so they appear in REAPER's action list even
/// after `wake_up()` has already run.
fn register_actions_sync(defs: &actions::ActionDefs) {
    let reaper = HighReaper::get();
    let (tx, _) = action_channel();

    for (command_id, display_name, handler, _show_in_menu) in defs {
        let handler = handler.clone();
        let tx = tx.clone();
        let cmd_name_for_broadcast = command_id.clone();

        // Leak strings for 'static lifetime — actions live for the process lifetime.
        let cmd_name: &'static str = Box::leak(command_id.clone().into_boxed_str());
        let desc: &'static str = Box::leak(display_name.clone().into_boxed_str());

        let action = reaper.register_action(
            cmd_name,
            desc,
            None,
            move || {
                handler();
                // Also forward to the action channel so App::dispatch_action
                // can handle it (for modules that use the broadcast path).
                let _ = tx.send(cmd_name_for_broadcast.clone());
            },
            reaper_high::ActionKind::NotToggleable,
        );

        // Register gaccel so the action appears in REAPER's action list
        // (register_action doesn't do this after wake_up has already run).
        let cmd_id = action.command_id();
        {
            let gaccel = reaper_medium::OwnedGaccelRegister::without_key_binding(cmd_id, desc);
            let mut session = reaper.medium_session();
            if let Err(e) = session.plugin_register_add_gaccel(gaccel) {
                warn!("Failed to register gaccel for '{}': {:?}", command_id, e);
            }
        }

        // Leak the RegisteredAction so it stays alive (action stays registered).
        std::mem::forget(action);
    }

    info!("Registered {} FTS actions (synchronous)", defs.len());
}

// ── Entry point ──────────────────────────────────────────────────────────────

#[reaper_extension_plugin]
fn plugin_main(context: PluginContext) -> Result<(), Box<dyn Error>> {
    // Tracing -> rolling daily log file in $XDG_STATE_HOME/fasttrackstudio/
    let log_dir = std::env::var("XDG_STATE_HOME")
        .map(std::path::PathBuf::from)
        .unwrap_or_else(|_| {
            std::path::PathBuf::from(std::env::var("HOME").unwrap_or_else(|_| "/tmp".into()))
                .join(".local/state")
        })
        .join("fasttrackstudio");
    std::fs::create_dir_all(&log_dir).ok();
    let file_appender = tracing_appender::rolling::daily(&log_dir, "reaper-fts-extensions.log");
    let (non_blocking, log_guard) = tracing_appender::non_blocking(file_appender);
    tracing_subscriber::fmt()
        .with_writer(non_blocking)
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::INFO.into()),
        )
        .init();

    info!("FTS Extensions starting…");

    // Low-level REAPER and SWELL APIs must be available globally before
    // any Reaper::get() / Swell::get() calls (needed for menus, panels, etc.)
    let _ = reaper_low::Reaper::make_available_globally(reaper_low::Reaper::load(context));
    let _ = reaper_low::Swell::make_available_globally(reaper_low::Swell::load(context));

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

    let tokio_runtime = Arc::new(
        tokio::runtime::Builder::new_multi_thread()
            .worker_threads(2)
            .enable_all()
            .build()?,
    );

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
            _log_guard: log_guard,
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
    let module_ctx = ModuleContext::new(g.tokio_runtime.clone());

    // Collect actions from all modules
    let module_actions = module::collect_actions(&modules);

    // Also collect legacy (non-module) actions from this crate
    let legacy_defs = actions::build_action_defs();

    // Merge all actions
    let mut all_actions: HashMap<String, Arc<dyn Fn() + Send + Sync>> = HashMap::new();
    for (id, _, handler, _) in &legacy_defs {
        all_actions.insert(id.clone(), handler.clone());
    }
    for (id, _, handler, _) in &module_actions {
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
    // Add the UI test panel toggle action
    let ui_test_action = ui_test_panel::action_def();
    all_defs.push(ui_test_action.into_tuple());

    let session = ReaperSession::load(context);
    let app = App {
        session: RefCell::new(session),
        task_middleware: RefCell::new(task_middleware),
        action_handlers: all_actions,
    };

    APP.set(Fragile::new(app)).map_err(|_| "App already set")?;

    register_actions_sync(&all_defs);

    // Initialize all modules and subscribe to events
    module::init_all(&modules, &module_ctx);

    let app = APP.get().unwrap().get();
    let mut session = app.session.borrow_mut();
    session.plugin_register_add_timer(timer_callback)?;

    drop(session);

    // ── Extensions → FastTrackStudio menu ────────────────────────────────
    // Collect menu entries from all action defs that have show_in_menu=true.
    let menu_entries: Vec<(String, String)> = all_defs
        .iter()
        .filter(|(_, _, _, show_in_menu)| *show_in_menu)
        .map(|(id, display_name, _, _)| (id.clone(), display_name.clone()))
        .collect();
    info!(menu_entries = menu_entries.len(), "Menu entries collected");
    menu::set_menu_entries(menu_entries);

    // Register the menu hook using the high-level Reaper session (like helgobox).
    info!("Registering Extensions menu hook...");
    HighReaper::get().medium_reaper().add_extensions_main_menu();
    match HighReaper::get()
        .medium_session()
        .plugin_register_add_hook_custom_menu::<menu::FtsMenuHook>()
    {
        Ok(()) => info!("Extensions menu hook registered successfully"),
        Err(e) => warn!("Extensions menu hook registration FAILED: {:?}", e),
    }

    // ── Dioxus panel rendering ────────────────────────────────────────
    reaper_dioxus::service::init();
    reaper_dioxus::dock::init(reaper_low::Reaper::get(), reaper_low::Swell::get());
    let mut panels = module::collect_panels(&modules);
    panels.push(ui_test_panel::panel_def());
    info!(panels = panels.len(), "Panel definitions collected");
    for panel in &panels {
        reaper_dioxus::dock::register_panel_from_service(panel);
    }
    reaper_dioxus::restore_dock_state();

    info!(
        modules = modules.len(),
        actions = all_defs.len(),
        panels = panels.len(),
        "FTS Extensions ready"
    );
    Ok(())
}
