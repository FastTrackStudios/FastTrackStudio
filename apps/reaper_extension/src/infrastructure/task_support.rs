//! TaskSupport for dispatching work to the main thread
//!
//! Provides a static TaskSupport instance that can be used to dispatch
//! work to REAPER's main thread from async handlers.

use crossbeam_channel::unbounded;
use reaper_high::{MainTaskMiddleware, MainThreadTask, TaskSupport};
use std::sync::{Arc, Mutex, OnceLock};

static TASK_SUPPORT: OnceLock<Arc<TaskSupport>> = OnceLock::new();
static TASK_MIDDLEWARE: OnceLock<Mutex<MainTaskMiddleware>> = OnceLock::new();

/// Initialize TaskSupport and set up middleware
/// This must be called from the main thread during plugin initialization
pub fn init_task_support() -> Result<(), Box<dyn std::error::Error>> {
    let (tx, rx) = unbounded::<MainThreadTask>();

    // Clone the sender for TaskSupport (Sender is Clone)
    let tx_for_support = tx.clone();
    let task_support = Arc::new(TaskSupport::new(tx_for_support));
    TASK_SUPPORT
        .set(task_support.clone())
        .map_err(|_| "TaskSupport already initialized")?;

    // Set up middleware to process tasks
    // The middleware will be run in the timer callback
    let middleware = MainTaskMiddleware::new(tx, rx);
    TASK_MIDDLEWARE
        .set(Mutex::new(middleware))
        .map_err(|_| "TaskMiddleware already initialized")?;

    Ok(())
}

/// Get the TaskSupport instance
pub fn get_task_support() -> Option<Arc<TaskSupport>> {
    TASK_SUPPORT.get().cloned()
}

/// Run the middleware to process pending tasks
/// This should be called from the main thread (e.g., in a timer callback)
pub fn run_middleware() {
    if let Some(middleware) = TASK_MIDDLEWARE.get() {
        if let Ok(mut mw) = middleware.lock() {
            mw.run();
        }
    }
}
