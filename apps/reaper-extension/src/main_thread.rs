//! Main Thread Dispatcher for REAPER API Calls
//!
//! REAPER APIs can only be called from the main thread. This module provides
//! a mechanism to queue commands from async contexts (like RPC handlers) and
//! execute them on the main thread via REAPER's timer callback.
//!
//! # Architecture
//!
//! ```text
//! Async Context (RPC handlers)
//!     ↓
//!     MainThreadDispatcher::queue_command(cmd)
//!     ↓
//!     mpsc channel (thread-safe queue)
//!     ↓
//! Timer Callback (main thread, ~30Hz)
//!     ↓
//!     process_pending() → executes REAPER API calls
//! ```

use reaper_high::Reaper;
use reaper_medium::{CommandId, ProjectContext};
use std::sync::{Mutex, OnceLock};
use tokio::sync::mpsc;
use tracing::{debug, error, info};

use daw_proto::ProjectInfo;

/// Commands that can be executed on the main thread
#[derive(Debug)]
pub enum MainThreadCommand {
    /// Transport: Play
    Play,
    /// Transport: Stop
    Stop,
    /// Get current project info (result sent via oneshot channel)
    GetCurrentProject {
        response_tx: tokio::sync::oneshot::Sender<Option<ProjectInfo>>,
    },
}

/// Global dispatcher instance
static DISPATCHER: OnceLock<MainThreadDispatcher> = OnceLock::new();

/// Main thread dispatcher for REAPER API calls
pub struct MainThreadDispatcher {
    /// Sender for queueing commands (can be cloned for async contexts)
    tx: mpsc::UnboundedSender<MainThreadCommand>,
    /// Receiver for processing commands (only used on main thread)
    rx: Mutex<mpsc::UnboundedReceiver<MainThreadCommand>>,
}

impl MainThreadDispatcher {
    /// Initialize the global dispatcher
    pub fn init() {
        let (tx, rx) = mpsc::unbounded_channel();
        let dispatcher = MainThreadDispatcher {
            tx,
            rx: Mutex::new(rx),
        };
        if DISPATCHER.set(dispatcher).is_err() {
            error!("MainThreadDispatcher already initialized");
        } else {
            info!("MainThreadDispatcher initialized");
        }
    }

    /// Get the global dispatcher instance
    pub fn get() -> Option<&'static MainThreadDispatcher> {
        DISPATCHER.get()
    }

    /// Queue a command for execution on the main thread
    pub fn queue(&self, cmd: MainThreadCommand) {
        if let Err(e) = self.tx.send(cmd) {
            error!("Failed to queue main thread command: {}", e);
        }
    }

    /// Process all pending commands (call from timer callback on main thread)
    pub fn process_pending(&self) {
        let mut rx = self.rx.lock().expect("Failed to lock receiver");
        while let Ok(cmd) = rx.try_recv() {
            self.execute(cmd);
        }
    }

    /// Execute a command on the main thread
    fn execute(&self, cmd: MainThreadCommand) {
        match cmd {
            MainThreadCommand::Play => {
                debug!("Executing: Transport Play");
                let reaper = Reaper::get();
                let medium = reaper.medium_reaper();
                // REAPER command 1007: Transport: Play
                medium.main_on_command_ex(CommandId::new(1007), 0, ProjectContext::CurrentProject);
                info!("REAPER: Playback started");
            }
            MainThreadCommand::Stop => {
                debug!("Executing: Transport Stop");
                let reaper = Reaper::get();
                let medium = reaper.medium_reaper();
                // REAPER command 1016: Transport: Stop
                medium.main_on_command_ex(CommandId::new(1016), 0, ProjectContext::CurrentProject);
                info!("REAPER: Playback stopped");
            }
            MainThreadCommand::GetCurrentProject { response_tx } => {
                debug!("Executing: Get Current Project");
                let reaper = Reaper::get();
                let project = reaper.current_project();

                // Get project file path (Utf8PathBuf from camino)
                let path = project.file().map(|p| p.to_string()).unwrap_or_default();

                // Extract name from path or use "Untitled"
                let name = if path.is_empty() {
                    "Untitled".to_string()
                } else {
                    std::path::Path::new(&path)
                        .file_stem()
                        .map(|s| s.to_string_lossy().to_string())
                        .unwrap_or_else(|| "Untitled".to_string())
                };

                // Generate a GUID-like identifier from the path
                let guid = format!("{:x}", hash_string(&path));

                let project_info = ProjectInfo { guid, name, path };
                let _ = response_tx.send(Some(project_info));
            }
        }
    }
}

/// Simple hash function for generating project GUIDs
fn hash_string(input: &str) -> u64 {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};
    let mut hasher = DefaultHasher::new();
    input.hash(&mut hasher);
    hasher.finish()
}

// ============================================================================
// Async-friendly wrappers for ReaperTransport/ReaperProject
// ============================================================================

/// Queue a play command (fire-and-forget)
pub fn queue_play() {
    if let Some(dispatcher) = MainThreadDispatcher::get() {
        dispatcher.queue(MainThreadCommand::Play);
    }
}

/// Queue a stop command (fire-and-forget)
pub fn queue_stop() {
    if let Some(dispatcher) = MainThreadDispatcher::get() {
        dispatcher.queue(MainThreadCommand::Stop);
    }
}

/// Queue a get current project command and wait for the result
pub async fn get_current_project() -> Option<ProjectInfo> {
    let dispatcher = MainThreadDispatcher::get()?;
    let (tx, rx) = tokio::sync::oneshot::channel();
    dispatcher.queue(MainThreadCommand::GetCurrentProject { response_tx: tx });
    rx.await.ok().flatten()
}
