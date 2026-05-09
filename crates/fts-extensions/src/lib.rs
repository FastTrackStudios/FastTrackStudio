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
use daw::Daw;
use daw::module::{self, DawModule, ModuleContext};
use daw::service::ActionEvent;
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
        catch_panic(
            "process_tasks",
            std::panic::AssertUnwindSafe(|| app.process_tasks()),
        );
        catch_panic("poll_and_broadcast", daw::reaper::poll_and_broadcast);
        catch_panic(
            "poll_and_broadcast_tracks",
            daw::reaper::poll_and_broadcast_tracks,
        );
        catch_panic(
            "process_pending_actions",
            std::panic::AssertUnwindSafe(|| process_pending_actions(app)),
        );
        catch_panic("update_panels", daw::ui::dock::update_panels);
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
    use daw::reaper::{RoutedHandler, build_extension_daw_with, create_daw_handler};
    use keyflow_daw_analysis::{
        KeyflowMidiAnalysis, MidiChartServiceDispatcher, midi_chart_service_service_descriptor,
    };

    tokio_runtime
        .block_on(async {
            // The keyflow chart service needs a `Daw` handle of its own to
            // call back into the daw client API while serving requests.
            // Build a temporary dual: first construct a "naked" daw on the
            // stock handler so KeyflowMidiAnalysis has a Daw to read from,
            // then re-build the real `Daw` on a handler that ALSO carries
            // the keyflow dispatcher. The naked-daw's caller is leaked
            // alongside the LocalCaller in build_extension_daw_with.
            let inner_daw = daw::reaper::build_extension_daw().await?;
            let keyflow = KeyflowMidiAnalysis::new(inner_daw);

            let handler: RoutedHandler = create_daw_handler().with(
                midi_chart_service_service_descriptor(),
                MidiChartServiceDispatcher::new(keyflow),
            );
            build_extension_daw_with(handler).await
        })
        .map_err(|e| eyre::eyre!("Failed to initialise in-process DAW: {e}"))
}

/// Register all actions synchronously on the main thread.
///
/// The registry service in `daw-reaper` owns the REAPER command IDs and
/// toggle state. We register through that service here, then forward action
/// trigger events into the existing dispatch channel.
fn register_actions_sync(
    defs: &actions::ActionDefs,
    modules: Vec<Box<dyn DawModule>>,
    panels: Vec<module::PanelDef>,
) {
    let g = Global::get();
    let daw = g.daw.clone();
    let runtime = g.tokio_runtime.clone();
    let defs = defs.clone();
    let action_count = defs.len();
    let panel_count = panels.len();
    let daw_for_subscription = daw.clone();
    let runtime_for_module_subscriptions = runtime.clone();
    let task_support = &g.task_support;

    runtime.spawn(async move {
        let registry = daw.action_registry();

        for (command_id, display_name, _handler, show_in_menu, toggleable) in defs {
            let description = display_name.as_str();
            let result = match (show_in_menu, toggleable) {
                (true, true) => {
                    registry
                        .register_toggle_in_menu(&command_id, description)
                        .await
                }
                (true, false) => registry.register_in_menu(&command_id, description).await,
                (false, true) => registry.register_toggle(&command_id, description).await,
                (false, false) => registry.register(&command_id, description).await,
            };

            match result {
                Ok(cmd_id) if cmd_id > 0 => {
                    info!(command_id = %command_id, cmd_id, "Registered action");

                    if matches!(
                        command_id.as_str(),
                        "FTS_INPUT_TOGGLE"
                            | "FTS_INPUT_TOGGLE_PASSTHROUGH"
                            | "FTS_INPUT_TOGGLE_DEBUG_LOGGING"
                            | "FTS_INPUT_PROFILE_SELECTOR"
                            | "FTS_INPUT_WORKFLOW_SELECTOR"
                            | "FTS_INPUT_TOGGLE_ACTIONS_PANEL"
                            | "FTS_INPUT_TOGGLE_KEYBOARD_PANEL"
                            | "FTS_INPUT_TOGGLE_STATUS_PANEL"
                    ) {
                        match registry.is_in_action_list(&command_id).await {
                            Ok(true) => info!(
                                command_id = %command_id,
                                "Input action list probe: present immediately after registration"
                            ),
                            Ok(false) => warn!(
                                command_id = %command_id,
                                "Input action list probe: missing immediately after registration"
                            ),
                            Err(e) => warn!(
                                command_id = %command_id,
                                "Input action list probe failed after registration: {e}"
                            ),
                        }
                    }
                }
                Ok(_) => warn!("Failed to register action: {command_id}"),
                Err(e) => warn!("Error registering action {command_id}: {e}"),
            }
        }
        info!(actions = action_count, "Action registration completed");

        if let Err(err) = task_support.do_later_in_main_thread_asap(move || {
            daw::ui::dock::init_service();
            daw::ui::dock::init_dock(reaper_low::Reaper::get(), reaper_low::Swell::get());
            info!(panels = panels.len(), "Panel definitions collected");
            for panel in &panels {
                daw::ui::dock::register_panel_from_service(panel);
            }
            daw::ui::dock::restore_dock_state();
            info!(panels = panels.len(), "Panel registration completed");
        }) {
            warn!("Failed to schedule panel registration: {err}");
        }

        let module_ctx = ModuleContext::new(runtime_for_module_subscriptions);
        for module in &modules {
            tracing::info!(
                module = module.name(),
                "Subscribing {}",
                module.display_name()
            );
            module.subscribe(&module_ctx);
        }
        info!(modules = modules.len(), "All modules subscribed");
        info!(
            modules = modules.len(),
            actions = action_count,
            panels = panel_count,
            "FTS Extensions ready"
        );
    });

    let (tx, _) = action_channel();
    let tx = tx.clone();
    runtime.spawn(async move {
        let registry = daw_for_subscription.action_registry();
        let Ok(mut rx) = registry.subscribe_actions().await else {
            warn!("Failed to subscribe to action trigger events");
            return;
        };

        info!("Subscribed to action trigger events");
        loop {
            match rx.recv().await {
                Ok(Some(event_ref)) => {
                    let mut event = None;
                    let _ = event_ref.map(|value| {
                        event = Some(value);
                    });
                    let event = event.expect("SelfRef::map ran");
                    match event {
                        ActionEvent::Triggered { ref command_name } => {
                            let _ = tx.send(command_name.clone());
                        }
                    }
                }
                Ok(None) => {
                    info!("Action trigger stream closed");
                    break;
                }
                Err(e) => {
                    warn!("Action trigger stream error: {e:?}");
                    break;
                }
            }
        }
    });
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
                .add_directive(tracing::Level::INFO.into())
                .add_directive("cranelift_jit=warn".parse()?)
                .add_directive("cranelift_codegen=warn".parse()?)
                .add_directive("wasmtime=warn".parse()?),
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
    let _ = daw::init_from_parts(g.daw.clone(), g.tokio_runtime.clone());
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
    let module_count = modules.len();

    // Initialize all modules before collecting actions.
    //
    // Several module actions derive toggle state or dynamic bindings from
    // configuration loaded during init, so registering them before init can
    // produce incomplete or stale action metadata.
    let module_ctx = ModuleContext::new(g.tokio_runtime.clone());
    for module in &modules {
        info!(module = module.name(), "Initializing {}", module.display_name());
        module.init(&module_ctx);
    }
    info!(modules = module_count, "All modules initialized");

    // Collect actions from all modules after init has populated runtime state.
    let module_actions = module::collect_actions(&modules);

    // Also collect legacy (non-module) actions from this crate
    let legacy_defs = actions::build_action_defs();

    // Merge all actions
    let mut all_actions: HashMap<String, Arc<dyn Fn() + Send + Sync>> = HashMap::new();
    for (id, _, handler, _, _) in &legacy_defs {
        all_actions.insert(id.clone(), handler.clone());
    }
    for (id, _, handler, _, _) in &module_actions {
        all_actions.insert(id.clone(), handler.clone());
    }
    for action in ui_test_panel::action_defs() {
        let (id, _, handler, _, _) = action.into_tuple();
        all_actions.insert(id, handler);
    }

    info!(
        legacy = legacy_defs.len(),
        modules = module_actions.len(),
        total = all_actions.len(),
        "Action definitions collected"
    );

    // Combine all defs for REAPER registration
    let mut all_defs: actions::ActionDefs = legacy_defs;
    all_defs.extend(module_actions.into_iter().map(
        |(id, display_name, handler, show_in_menu, toggleable)| {
            (id, display_name, handler, show_in_menu, toggleable)
        },
    ));
    all_defs.extend(
        ui_test_panel::action_defs()
            .into_iter()
            .map(|a| a.into_tuple()),
    );

    let mut panels = module::collect_panels(&modules);
    panels.extend(ui_test_panel::panel_defs());

    let session = ReaperSession::load(context);
    let app = App {
        session: RefCell::new(session),
        task_middleware: RefCell::new(task_middleware),
        action_handlers: all_actions,
    };

    APP.set(Fragile::new(app)).map_err(|_| "App already set")?;

    register_actions_sync(&all_defs, modules, panels);

    let app = APP.get().unwrap().get();
    let mut session = app.session.borrow_mut();
    session.plugin_register_add_timer(timer_callback)?;
    daw::reaper::register_project_importer(&mut session)?;

    drop(session);

    // ── Extensions → FastTrackStudio menu ────────────────────────────────
    // Collect menu entries from all action defs that have show_in_menu=true.
    let menu_entries: Vec<(String, String)> = all_defs
        .iter()
        .filter(|(_, _, _, show_in_menu, _)| *show_in_menu)
        .map(|(id, display_name, _, _, _)| (id.clone(), display_name.clone()))
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

    info!(modules = module_count, "FTS Extensions startup scheduled");
    Ok(())
}
