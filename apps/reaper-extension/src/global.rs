//! Global State for REAPER Extension
//!
//! Provides access to TaskSupport for scheduling work on the main thread.
//! This follows the pattern from helgobox.
//!
//! # Pattern
//!
//! Instead of defining individual command enums for each REAPER API call,
//! we use `TaskSupport` which accepts arbitrary closures. This allows:
//!
//! 1. Sending any operation to the main thread without extra boilerplate
//! 2. Getting return values via `main_thread_future()` async function
//! 3. No need for per-API queue functions or callback registrations
//!
//! # Usage
//!
//! ```rust,ignore
//! // Fire-and-forget operation
//! Global::task_support()
//!     .do_later_in_main_thread_asap(|| {
//!         let reaper = Reaper::get();
//!         reaper.medium_reaper().main_on_command_ex(...);
//!     });
//!
//! // Operation that returns a value
//! let tempo = Global::task_support()
//!     .main_thread_future(|| {
//!         let reaper = Reaper::get();
//!         reaper.current_project().tempo().bpm().get()
//!     })
//!     .await
//!     .unwrap_or(120.0);
//! ```

use crossbeam_channel::{Receiver, Sender};
use reaper_high::{MainTaskMiddleware, MainThreadTask, TaskSupport};
use std::sync::OnceLock;
use tracing::info;

/// Global state singleton
static INSTANCE: OnceLock<Global> = OnceLock::new();

/// Global state providing TaskSupport for main thread dispatch
pub struct Global {
    task_support: TaskSupport,
    task_sender: Sender<MainThreadTask>,
    task_receiver: Receiver<MainThreadTask>,
}

impl Global {
    /// Initialize the global state. Call this once during extension startup.
    pub fn init() {
        INSTANCE.get_or_init(|| {
            // Create unbounded channel for main thread tasks
            // Unbounded is important to avoid deadlocks when sending from main thread
            let (task_sender, task_receiver) = crossbeam_channel::unbounded();

            info!("Global TaskSupport initialized");

            Global {
                task_support: TaskSupport::new(task_sender.clone()),
                task_sender,
                task_receiver,
            }
        });
    }

    /// Get the global instance. Panics if not initialized.
    pub fn get() -> &'static Global {
        INSTANCE
            .get()
            .expect("Global not initialized - call Global::init() first")
    }

    /// Get the TaskSupport for scheduling work on the main thread.
    ///
    /// # Thread Safety
    ///
    /// TaskSupport is thread-safe and can be called from any thread.
    /// Operations are queued and executed on the main thread during
    /// the timer callback.
    pub fn task_support() -> &'static TaskSupport {
        &Global::get().task_support
    }

    /// Create the middleware that processes queued tasks.
    /// Call `middleware.run()` in the timer callback.
    pub fn create_task_middleware(&self) -> MainTaskMiddleware {
        MainTaskMiddleware::new(self.task_sender.clone(), self.task_receiver.clone())
    }
}
